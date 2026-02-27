# =============================================================================
# Stage 5b: Loop Recovery Analysis (CFG-tier SSA)
# =============================================================================

.mojor_ssa_cfg_successor_map <- function(ssa) {
  ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA cfg_successor_map")
  block_names <- names(ssa$blocks)
  if (is.null(block_names) || length(block_names) == 0) {
    return(list())
  }

  succ <- setNames(vector("list", length(block_names)), block_names)
  for (bn in block_names) {
    succ[[bn]] <- .mojor_ssa_successors(ssa$blocks[[bn]]$term)
  }
  succ
}

.mojor_ssa_cfg_reachable_blocks <- function(ssa) {
  ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA cfg_reachable_blocks")
  if (is.null(ssa$entry) || !is.character(ssa$entry) || length(ssa$entry) != 1) {
    return(character(0))
  }
  if (is.null(ssa$blocks) || !is.list(ssa$blocks) || length(ssa$blocks) == 0) {
    return(character(0))
  }
  if (!ssa$entry %in% names(ssa$blocks)) {
    return(character(0))
  }

  succ_map <- .mojor_ssa_cfg_successor_map(ssa)
  seen <- character(0)
  work <- ssa$entry
  while (length(work) > 0) {
    bn <- work[[1]]
    work <- work[-1]
    if (bn %in% seen) next
    if (!bn %in% names(ssa$blocks)) next
    seen <- c(seen, bn)
    succ <- succ_map[[bn]]
    if (length(succ) > 0) work <- c(work, succ[!succ %in% seen])
  }
  unique(seen)
}

.mojor_ssa_cfg_has_cycle <- function(succ_map, nodes) {
  if (length(nodes) == 0) {
    return(FALSE)
  }
  visited <- setNames(rep(FALSE, length(nodes)), nodes)
  in_stack <- setNames(rep(FALSE, length(nodes)), nodes)

  dfs <- function(n) {
    visited[[n]] <<- TRUE
    in_stack[[n]] <<- TRUE
    succ <- succ_map[[n]]
    if (!is.null(succ) && length(succ) > 0) {
      for (sn in succ) {
        if (!sn %in% nodes) next
        if (!isTRUE(visited[[sn]]) && isTRUE(dfs(sn))) {
          return(TRUE)
        }
        if (isTRUE(in_stack[[sn]])) {
          return(TRUE)
        }
      }
    }
    in_stack[[n]] <<- FALSE
    FALSE
  }

  for (n in nodes) {
    if (!isTRUE(visited[[n]]) && isTRUE(dfs(n))) {
      return(TRUE)
    }
  }
  FALSE
}

.mojor_loop_recovery_guess_kind <- function(loop_id, header, attrs = NULL, meta = NULL) {
  if (!is.null(attrs) && is.list(attrs) && !is.null(attrs$loop_kind)) {
    k <- as.character(attrs$loop_kind)
    if (k %in% c("for", "while", "repeat")) {
      return(k)
    }
  }
  if (!is.null(meta) && is.list(meta) && !is.null(meta$kind)) {
    k <- as.character(meta$kind)
    if (k %in% c("for", "while", "repeat")) {
      return(k)
    }
  }
  lid <- as.character(loop_id)
  if (grepl("^while_", lid)) {
    return("while")
  }
  if (grepl("^repeat_", lid)) {
    return("repeat")
  }
  if (grepl("^for_", lid)) {
    return("for")
  }
  if (grepl("^while_header", header)) {
    return("while")
  }
  if (grepl("^repeat_header", header)) {
    return("repeat")
  }
  "for"
}

.mojor_loop_recovery_collect_intent_attrs <- function(attrs) {
  if (is.null(attrs) || !is.list(attrs) || length(attrs) == 0) {
    return(list())
  }
  out <- list()
  for (nm in names(attrs)) {
    if (is.null(nm) || !nzchar(nm)) next
    if (startsWith(nm, "schedule.") || startsWith(nm, "intent.")) {
      out[[nm]] <- attrs[[nm]]
    }
  }
  out
}

.mojor_loop_recovery_unknown <- function(code, message, hint = NULL, block = "") {
  list(
    status = "Unknown",
    cfg_reducible = FALSE,
    boundary_verifier_enabled = FALSE,
    reason = as.character(code),
    diagnostics = list(list(
      code = as.character(code),
      severity = "warning",
      block = as.character(block),
      message = as.character(message),
      hint = if (is.null(hint)) "" else as.character(hint)
    )),
    loops = list(),
    loop_metadata = list()
  )
}

.mojor_loop_recovery_analysis <- function(
    ssa,
    require_reducible = TRUE,
    require_single_exit = TRUE) {
  ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "loop_recovery_analysis")
  block_names <- names(ssa$blocks)
  if (is.null(block_names) || length(block_names) == 0) {
    return(.mojor_loop_recovery_unknown(
      code = "LOOP_RECOVERY_UNKNOWN_EMPTY_CFG",
      message = "Loop recovery requires a non-empty CFG",
      hint = "Run SSA lowering before loop recovery"
    ))
  }

  built <- .mojor_ssa_build_preds_dom(ssa, .mojor_ssa_successors)
  preds <- built$preds
  dom <- built$dom
  succ_map <- .mojor_ssa_cfg_successor_map(ssa)
  reachable <- .mojor_ssa_cfg_reachable_blocks(ssa)

  if (length(reachable) == 0) {
    return(.mojor_loop_recovery_unknown(
      code = "LOOP_RECOVERY_UNKNOWN_UNREACHABLE_ENTRY",
      message = "Loop recovery could not find reachable blocks from entry",
      hint = "Check SSA entry block and CFG edges"
    ))
  }

 # Backedge: successor dominates predecessor.
  backedges <- list()
  for (bn in reachable) {
    succ <- succ_map[[bn]]
    if (length(succ) == 0) next
    for (sn in succ) {
      if (!sn %in% reachable) next
      if (sn %in% dom[[bn]]) {
        backedges[[length(backedges) + 1L]] <- list(latch = bn, header = sn)
      }
    }
  }

  if (length(backedges) == 0) {
    if (isTRUE(require_reducible) && isTRUE(.mojor_ssa_cfg_has_cycle(succ_map, reachable))) {
      return(.mojor_loop_recovery_unknown(
        code = "LOOP_RECOVERY_UNKNOWN_NOT_REDUCIBLE",
        message = "CFG contains cycles but no reducible natural-loop backedges were found",
        hint = "Irreducible CFG normalization is out of scope for Stage 5b MVP"
      ))
    }
    return(list(
      status = "Recovered",
      cfg_reducible = TRUE,
      boundary_verifier_enabled = TRUE,
      diagnostics = list(),
      loops = list(),
      loop_metadata = list(),
      backedges = list(),
      reachable = reachable
    ))
  }

 # Group backedges by header.
  loops_by_header <- list()
  for (be in backedges) {
    hdr <- as.character(be$header)
    lat <- as.character(be$latch)
    existing <- loops_by_header[[hdr]]
    if (is.null(existing)) existing <- character(0)
    loops_by_header[[hdr]] <- unique(c(existing, lat))
  }

  loops <- list()
  loop_metadata <- list()
  used_loop_ids <- character(0)

  for (hdr in names(loops_by_header)) {
    latches <- loops_by_header[[hdr]]
    loop_nodes <- unique(c(hdr, latches))
    work <- unique(latches)

 # Recover natural loop body by reverse walk through preds until header.
    while (length(work) > 0) {
      n <- work[[1]]
      work <- work[-1]
      pset <- preds[[n]]
      if (is.null(pset) || length(pset) == 0) next
      for (p in pset) {
        if (!p %in% reachable) next
 # Natural-loop membership is restricted to nodes dominated by header.
        if (!hdr %in% dom[[p]]) next
        if (p %in% loop_nodes) next
        loop_nodes <- c(loop_nodes, p)
        if (!identical(p, hdr)) work <- c(work, p)
      }
    }
    loop_nodes <- unique(loop_nodes)

 # Reducibility check: loop entry edges may only target header.
    external_entries <- list()
    for (n in loop_nodes) {
      pset <- preds[[n]]
      if (is.null(pset) || length(pset) == 0) next
      for (p in pset) {
        if (p %in% loop_nodes) next
        external_entries[[length(external_entries) + 1L]] <- list(from = p, to = n)
      }
    }
    entry_targets <- unique(vapply(external_entries, function(e) as.character(e$to), character(1)))
    if (isTRUE(require_reducible) && any(entry_targets != hdr)) {
      return(.mojor_loop_recovery_unknown(
        code = "LOOP_RECOVERY_UNKNOWN_NOT_REDUCIBLE",
        message = sprintf("Loop header '%s' has non-header entry edges", hdr),
        hint = "Irreducible CFG is out of scope for Stage 5b MVP",
        block = hdr
      ))
    }

    exits <- list()
    exit_targets <- character(0)
    for (n in loop_nodes) {
      succ <- succ_map[[n]]
      if (is.null(succ) || length(succ) == 0) next
      for (sn in succ) {
        if (sn %in% loop_nodes) next
        exits[[length(exits) + 1L]] <- list(from = n, to = sn)
        exit_targets <- c(exit_targets, sn)
      }
    }
    exit_targets <- unique(exit_targets)
    if (isTRUE(require_single_exit) && length(exit_targets) != 1L) {
      return(.mojor_loop_recovery_unknown(
        code = "LOOP_RECOVERY_UNKNOWN_EXIT_FORM",
        message = sprintf(
          "Loop header '%s' has %d exit targets (Stage 5b MVP expects exactly one)",
          hdr,
          length(exit_targets)
        ),
        hint = "Use recanonicalization prerequisites that preserve single-exit loop form",
        block = hdr
      ))
    }

    hdr_attrs <- ssa$blocks[[hdr]]$attrs
    existing_meta <- NULL
    existing_loop_id <- NULL
    if (!is.null(ssa$loop_metadata) && is.list(ssa$loop_metadata)) {
      for (k in names(ssa$loop_metadata)) {
        m <- ssa$loop_metadata[[k]]
        cont <- if (!is.null(m$continue_dest$block)) as.character(m$continue_dest$block) else ""
        if (identical(cont, hdr)) {
          existing_meta <- m
          existing_loop_id <- as.character(k)
          break
        }
      }
    }

    loop_id <- if (!is.null(hdr_attrs) && is.list(hdr_attrs) && !is.null(hdr_attrs$loop_id)) {
      as.character(hdr_attrs$loop_id)
    } else if (!is.null(existing_loop_id) && nzchar(existing_loop_id)) {
      existing_loop_id
    } else {
      paste0("recovered_", hdr)
    }
    if (!nzchar(loop_id)) loop_id <- paste0("recovered_", hdr)

    if (loop_id %in% used_loop_ids) {
      return(.mojor_loop_recovery_unknown(
        code = "LOOP_RECOVERY_UNKNOWN_DUPLICATE_LOOP_ID",
        message = sprintf("Recovered loop_id '%s' is duplicated across headers", loop_id),
        hint = "Ensure distinct loop anchors carry distinct loop_id attributes",
        block = hdr
      ))
    }
    used_loop_ids <- c(used_loop_ids, loop_id)

    kind <- .mojor_loop_recovery_guess_kind(loop_id, hdr, attrs = hdr_attrs, meta = existing_meta)
    continue_block <- hdr
    break_block <- if (length(exit_targets) == 1L) exit_targets[[1L]] else NULL
    intent_attrs <- .mojor_loop_recovery_collect_intent_attrs(hdr_attrs)

    loop_info <- list(
      loop_id = loop_id,
      kind = kind,
      header = hdr,
      latches = latches,
      nodes = sort(unique(loop_nodes)),
      exits = exits,
      continue_dest = list(loop_id = loop_id, role = "Continue", block = continue_block),
      break_dest = list(loop_id = loop_id, role = "Break", block = break_block),
      anchor_block = hdr,
      intent_attrs = intent_attrs
    )
    loops[[loop_id]] <- loop_info
    loop_metadata[[loop_id]] <- list(
      loop_id = loop_id,
      kind = kind,
      anchor_block = hdr,
      continue_dest = loop_info$continue_dest,
      break_dest = loop_info$break_dest
    )
  }

  list(
    status = "Recovered",
    cfg_reducible = TRUE,
    boundary_verifier_enabled = TRUE,
    diagnostics = list(),
    loops = loops,
    loop_metadata = loop_metadata,
    backedges = backedges,
    reachable = reachable
  )
}
