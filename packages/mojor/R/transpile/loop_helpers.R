# Transpile loop-shape helper bundle (consolidated).
# Contract: shared iter/AST/body/tiling helper utilities for loop transpile.

# Transpile loop iteration helper utilities.
# Contract: shared helpers for loop sequence length lowering and iter map setup.

.mojor_transpile_iter_len_expr <- function(info) {
  if (is.null(info)) {
    return(NULL)
  }
  len_expr <- NULL
  if (!is.null(info$end_expr_ast)) {
    len_expr <- info$end_expr_ast
  } else if (!is.null(info$kind) && info$kind %in% c("array", "iter") && !is.null(info$name)) {
    len_expr <- call("length", as.name(info$name))
  } else if (!is.null(info$kind) && info$kind == "scalar" && !is.null(info$name)) {
    len_expr <- as.name(info$name)
  }
  if (!is.null(len_expr) && !is.null(info$end_delta) && info$end_delta != 0L) {
    delta <- info$end_delta
    len_expr <- if (delta > 0L) call("+", len_expr, delta) else call("-", len_expr, abs(delta))
  }
  len_expr
}

.mojor_prepare_transpile_iter_map <- function(ctx) {
  if (!is.environment(ctx)) {
    stop(".mojor_prepare_transpile_iter_map: ctx must be an environment")
  }
  evalq(
    {
      iter_map <- list()
      if (length(loop_infos) > 0) {
        for (li in loop_infos) {
          if (is.null(li$iter_value)) next
          if (!is.null(li$iter_expr_ast)) {
            iter_map[[li$iter_value]] <- build_iter_expr_entry(li, li$var)
          } else if (!is.null(li$iter_source)) {
            iter_map[[li$iter_value]] <- list(source = li$iter_source, index = li$var)
          }
        }
      }
      if (length(iter_map) > 0) {
        .mojor_state$current_iter_map <- iter_map
      }
    },
    envir = ctx
  )
}

# Transpile loop AST helpers.
# Contract: shared construction of for/while/repeat loop AST with seq_parallel lowering.

.mojor_transpile_loop_seq_ir <- function(li, iter_len_expr = NULL) {
  loop_seq_ir <- li$seq
 # IR does not carry parallel helper calls directly; normalize to seq forms first.
  if (is.call(loop_seq_ir) && length(loop_seq_ir) >= 2) {
    op <- as.character(loop_seq_ir[[1]])
    if (identical(op, "seq_parallel_along")) {
      loop_seq_ir <- call("seq_along", loop_seq_ir[[2]])
    } else if (op %in% c("seq_parallel_len", "mojor_prange")) {
      loop_seq_ir <- call("seq_len", loop_seq_ir[[2]])
    }
  }
 # IR does not have seq_parallel_* nodes; lower them to canonical seq forms.
  if (isTRUE(li$seq_info$parallel)) {
    if (li$seq_info$kind %in% c("array", "iter")) {
      loop_seq_ir <- call("seq_along", as.name(li$seq_info$name))
    } else if (identical(li$seq_info$kind, "scalar")) {
      loop_seq_ir <- call("seq_len", as.name(li$seq_info$name))
    }
  }
  if (!is.null(li$iter_expr_ast) && is.function(iter_len_expr)) {
    len_expr_ast <- iter_len_expr(li$seq_info)
    if (!is.null(len_expr_ast)) {
      loop_seq_ir <- call("mojor_seq_len", len_expr_ast)
    }
  }
  loop_seq_ir
}

.mojor_build_transpile_loop_ast <- function(li, loop_body_expr, iter_len_expr = NULL) {
  if (!is.null(li$kind) && li$kind == "while") {
    return(as.call(list(as.name("while"), li$cond, loop_body_expr)))
  }
  if (!is.null(li$kind) && li$kind == "repeat") {
    return(as.call(list(as.name("repeat"), loop_body_expr)))
  }
  loop_seq_ir <- .mojor_transpile_loop_seq_ir(li, iter_len_expr = iter_len_expr)
  as.call(list(as.name("for"), as.name(li$var), loop_seq_ir, loop_body_expr))
}

# Transpile loop body-expression helper.
# Contract: shared construction of loop body expression (renaming + optional short-circuit wrapper).

.mojor_build_transpile_loop_body_expr <- function(li, short_ir_fallback = NULL, scalar_init = NULL) {
  loop_blocks <- li$blocks
  if (!is.null(li$iter_value) &&
    !identical(li$iter_value, li$var) &&
    is.null(li$iter_expr_ast)) {
    loop_blocks <- .mojor_rename_block(loop_blocks, li$iter_value, li$var)
  }
  loop_body_expr <- .mojor_wrap_block_expr(loop_blocks)

  if (!is.null(short_ir_fallback)) {
    loop_body_expr <- as.call(list(
      as.name("{"),
      as.call(list(
        as.name("if"),
        short_ir_fallback$cond,
        as.call(list(
          as.name("{"),
          as.call(list(as.name("<-"), as.name(scalar_init), short_ir_fallback$set)),
          as.name("break")
        ))
      ))
    ))
  }
  loop_body_expr
}

# Transpile loop tiling utility helpers.
# Contract: shared utilities for nested-for detection and range bound parsing.

.mojor_find_transpile_inner_for_index <- function(blocks) {
  if (length(blocks) == 0) {
    return(NULL)
  }
  for (idx in seq_along(blocks)) {
    b <- blocks[[idx]]
    if (is.call(b) && as.character(b[[1]]) == "for") {
      return(idx)
    }
  }
  NULL
}

.mojor_parse_transpile_range_bounds <- function(range_expr, default_start = "1") {
  split_top_level_args <- function(expr) {
    expr <- trimws(expr)
    if (!grepl("^range\\(", expr)) {
      return(character(0))
    }
    open_pos <- regexpr("\\(", expr, fixed = TRUE)[1]
    close_pos <- regexpr("\\)\\s*$", expr)[1]
    if (open_pos < 0L || close_pos < 0L || close_pos <= open_pos) {
      return(character(0))
    }
    inner <- substr(expr, open_pos + 1L, close_pos - 1L)
    chars <- strsplit(inner, "", fixed = TRUE)[[1]]
    depth <- 0L
    cur <- character(0)
    out <- character(0)
    for (ch in chars) {
      if (identical(ch, "(") || identical(ch, "[") || identical(ch, "{")) {
        depth <- depth + 1L
        cur <- c(cur, ch)
        next
      }
      if (identical(ch, ")") || identical(ch, "]") || identical(ch, "}")) {
        depth <- max(0L, depth - 1L)
        cur <- c(cur, ch)
        next
      }
      if (identical(ch, ",") && depth == 0L) {
        out <- c(out, paste(cur, collapse = ""))
        cur <- character(0)
        next
      }
      cur <- c(cur, ch)
    }
    out <- c(out, paste(cur, collapse = ""))
    trimws(out)
  }
  canonicalize_bound <- function(expr) {
    expr <- trimws(expr)
    expr <- sub(
      "^Int\\(\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\+\\s*1\\s*\\)\\)$",
      "Int(\\1) + 1",
      expr,
      perl = TRUE
    )
    expr
  }
  args <- split_top_level_args(range_expr)
  if (length(args) >= 2L) {
    return(list(start = canonicalize_bound(args[[1L]]), end = canonicalize_bound(args[[2L]])))
  }
  if (length(args) == 1L) {
    return(list(start = canonicalize_bound(default_start), end = canonicalize_bound(args[[1L]])))
  }
  list(start = canonicalize_bound(default_start), end = canonicalize_bound(default_start))
}

# Transpile loop tiling emission helpers.
# Contract: shared emission utilities for tiled loop scaffolding.

.mojor_transpile_tiled_loop_header_lines <- function(
    tile_i,
    tile_j,
    start_i,
    end_i,
    start_j,
    end_j,
    var_i,
    var_j) {
  c(
    paste0("    # Tiled loops with tile size (", tile_i, ", ", tile_j, ")"),
    paste0("    for _mojor_tile_i in range(", start_i, ", ", end_i, ", ", tile_i, "):"),
    paste0("        for _mojor_tile_j in range(", start_j, ", ", end_j, ", ", tile_j, "):"),
    paste0("            var _mojor_tile_end_i = min(_mojor_tile_i + ", tile_i, ", ", end_i, ")"),
    paste0("            var _mojor_tile_end_j = min(_mojor_tile_j + ", tile_j, ", ", end_j, ")"),
    paste0("            for ", var_i, " in range(_mojor_tile_i, _mojor_tile_end_i):"),
    paste0("                for ", var_j, " in range(_mojor_tile_j, _mojor_tile_end_j):")
  )
}
