# =============================================================================
# Stage 5b: Loop CFG Recanonicalization (MVP)
# =============================================================================

.mojor_loop_recovery_boundary_enabled <- function(recovery) {
    if (is.null(recovery) ||
        !is.list(recovery) ||
        is.null(recovery$status)) {
        return(TRUE)
    }
    !identical(
        as.character(recovery$status),
        "Unknown"
    )
}

.mojor_loop_recanonicalize_merge_attrs <- function(existing, extra) {
    out <- if (is.null(existing) ||
        !is.list(existing))
        list() else existing
    if (is.null(extra) ||
        !is.list(extra) ||
        length(extra) ==
            0) {
        return(out)
    }
    out[names(extra)] <- extra
    out
}

.mojor_loop_recanonicalize_apply_block_attrs <- function(ssa, block, attrs) {
    if (is.null(block) ||
        !is.character(block) ||
        length(block) !=
            1 || !nzchar(block)) {
        return(ssa)
    }
    if (!block %in% names(ssa$blocks)) {
        return(ssa)
    }
    blk <- ssa$blocks[[block]]
    blk$attrs <- .mojor_loop_recanonicalize_merge_attrs(blk$attrs, attrs)
    ssa$blocks[[block]] <- blk
    ssa
}

.mojor_recanonicalize_loop_cfg <- function(ssa, recovery = NULL, verify = TRUE, strict_schema = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "recanonicalize_loop_cfg")
    if (isTRUE(verify))
        .mojor_ir_verify_ssa(ssa, strict_schema = strict_schema)

    rec <- recovery
    if (is.null(rec)) {
        rec <- .mojor_loop_recovery_analysis(ssa)
    }

    if (!.mojor_loop_recovery_boundary_enabled(rec)) {
        return(
            list(
                status = "Unknown", changed = FALSE, boundary_verifier_enabled = FALSE,
                diagnostics = if (!is.null(rec$diagnostics)) rec$diagnostics else list(),
                recovery = rec, ssa = ssa
            )
        )
    }

    out <- ssa
    changed <- FALSE
    recovered_loops <- if (is.null(rec$loops) ||
        !is.list(rec$loops))
        list() else rec$loops
    rec_meta <- if (is.null(rec$loop_metadata) ||
        !is.list(rec$loop_metadata))
        list() else rec$loop_metadata

    for (loop_id in names(recovered_loops)) {
        li <- recovered_loops[[loop_id]]
        header <- if (!is.null(li$header))
            as.character(li$header) else if (!is.null(li$anchor_block))
            as.character(li$anchor_block) else ""
        if (!nzchar(header) ||
            !header %in% names(out$blocks))
            next

        kind <- if (!is.null(li$kind))
            as.character(li$kind) else "for"
        cont_block <- if (!is.null(li$continue_dest$block))
            as.character(li$continue_dest$block) else header
        break_block <- if (!is.null(li$break_dest$block))
            as.character(li$break_dest$block) else ""
        intent_attrs <- if (!is.null(li$intent_attrs) &&
            is.list(li$intent_attrs))
            li$intent_attrs else list()

        header_attrs <- c(
            intent_attrs, list(
                loop_id = as.character(loop_id),
                loop_kind = kind, loop_role = "header", loop_continue_dest = cont_block,
                loop_break_dest = if (nzchar(break_block)) break_block else NULL,
                loop_anchor = TRUE
            )
        )
        out <- .mojor_loop_recanonicalize_apply_block_attrs(out, header, header_attrs)
        changed <- TRUE

        if (nzchar(break_block) &&
            break_block %in% names(out$blocks)) {
            out <- .mojor_loop_recanonicalize_apply_block_attrs(
                out, break_block, list(
                  loop_id = as.character(loop_id),
                  loop_kind = kind, loop_role = "exit"
              )
            )
        }

        loop_nodes <- if (!is.null(li$nodes) &&
            length(li$nodes) >
                0)
            as.character(li$nodes) else character(0)
        body_nodes <- setdiff(loop_nodes, c(header, break_block))
        if (length(body_nodes) >
            0) {
            for (bn in body_nodes) {
                if (!bn %in% names(out$blocks))
                  next
                battrs <- out$blocks[[bn]]$attrs
                cur_role <- if (is.null(battrs$loop_role))
                  "" else as.character(battrs$loop_role)
                if (identical(cur_role, "header") ||
                  identical(cur_role, "exit"))
                  next
                out <- .mojor_loop_recanonicalize_apply_block_attrs(
                  out, bn, list(
                    loop_id = as.character(loop_id),
                    loop_kind = kind, loop_role = "body"
                )
              )
            }
        }
    }

    merged_meta <- if (!is.null(out$loop_metadata) &&
        is.list(out$loop_metadata))
        out$loop_metadata else list()
    if (length(rec_meta) >
        0) {
        merged_meta[names(rec_meta)] <- rec_meta
    } else if (length(recovered_loops) >
        0) {
        for (loop_id in names(recovered_loops)) {
            li <- recovered_loops[[loop_id]]
            merged_meta[[loop_id]] <- list(
                loop_id = as.character(loop_id),
                kind = if (!is.null(li$kind)) as.character(li$kind) else "for",
                anchor_block = if (!is.null(li$header)) as.character(li$header) else NULL,
                continue_dest = if (!is.null(li$continue_dest)) li$continue_dest else list(
                  loop_id = loop_id, role = "Continue", block = if (!is.null(li$header)) as.character(li$header) else NULL
              ),
                break_dest = if (!is.null(li$break_dest)) li$break_dest else list(loop_id = loop_id, role = "Break", block = NULL)
            )
        }
    }
    out$loop_metadata <- merged_meta

    if (isTRUE(verify))
        .mojor_ir_verify_ssa(out, strict_schema = strict_schema)

    list(
        status = "Recanonicalized", changed = isTRUE(changed),
        boundary_verifier_enabled = TRUE, diagnostics = list(), recovery = rec,
        ssa = out
    )
}

.mojor_ssa_collect_recanonicalize_diagnostics <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA recanonicalize diagnostics")
    diagnostics <- list()

    loops_present <- .mojor_ssa_loop_skeleton_present(ssa)
    loop_meta <- if (!is.null(ssa$loop_metadata) &&
        is.list(ssa$loop_metadata)) {
        ssa$loop_metadata
    } else if (exists(".mojor_ssa_loop_destination_metadata", mode = "function")) {
        .mojor_ssa_loop_destination_metadata(ssa)
    } else {
        list()
    }

    if (isTRUE(loops_present) &&
        length(loop_meta) ==
            0) {
        diagnostics[[length(diagnostics) +
            1L]] <- list(
            code = "LOOP_RECANONICALIZE_METADATA_MISSING", severity = "error",
            block = "", message = "Recanonicalize boundary requires loop destination metadata for loop skeleton blocks",
            hint = "Run .mojor_recanonicalize_loop_cfg() before post_recanonicalize boundary checks"
        )
        return(diagnostics)
    }

    block_names <- names(ssa$blocks)
    for (loop_id in names(loop_meta)) {
        meta <- loop_meta[[loop_id]]
        cont_block <- if (!is.null(meta$continue_dest$block))
            as.character(meta$continue_dest$block) else ""
        break_block <- if (!is.null(meta$break_dest$block))
            as.character(meta$break_dest$block) else ""
        anchor_block <- if (!is.null(meta$anchor_block))
            as.character(meta$anchor_block) else cont_block

        if (!nzchar(cont_block) ||
            !nzchar(break_block) ||
            !cont_block %in% block_names || !break_block %in% block_names) {
            diagnostics[[length(diagnostics) +
                1L]] <- list(
                code = "LOOP_RECANONICALIZE_METADATA_INCOMPLETE", severity = "error",
                block = if (nzchar(cont_block)) cont_block else "",
                message = sprintf(
                  "Loop metadata for '%s' is incomplete at post_recanonicalize boundary",
                  loop_id
              ),
                hint = "Ensure recanonicalization recovers valid continue/break destination blocks"
            )
            next
        }

        if (!nzchar(anchor_block) ||
            !anchor_block %in% block_names) {
            diagnostics[[length(diagnostics) +
                1L]] <- list(
                code = "LOOP_RECANONICALIZE_ANCHOR_MISSING", severity = "error",
                block = "", message = sprintf("Loop '%s' has no valid anchor block metadata", loop_id),
                hint = "Anchor blocks should carry loop_id/role attrs after recanonicalization"
            )
            next
        }

        aattrs <- ssa$blocks[[anchor_block]]$attrs
        aid <- if (is.null(aattrs$loop_id))
            "" else as.character(aattrs$loop_id)
        arole <- if (is.null(aattrs$loop_role))
            "" else as.character(aattrs$loop_role)
        if (!identical(aid, as.character(loop_id)) ||
            !identical(arole, "header")) {
            diagnostics[[length(diagnostics) +
                1L]] <- list(
                code = "LOOP_RECANONICALIZE_ANCHOR_ATTRS", severity = "error",
                block = anchor_block, message = sprintf(
                  "Loop anchor '%s' does not carry canonical header attrs for '%s'",
                  anchor_block, loop_id
              ),
                hint = "Set loop_role='header' and matching loop_id on the loop anchor block"
            )
        }
    }

    diagnostics
}
