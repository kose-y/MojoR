.mojor_ir_dump <- function(expr_or_fn) {
  body_expr <- expr_or_fn
  if (is.function(expr_or_fn)) {
    body_expr <- body(expr_or_fn)
  }
  .mojor_ir_build_block(body_expr)
}

.mojor_diag_index_nested_missing_selector <- function() {
  paste0(
    "mojor_transpile: direct nested selector with missing index is not supported; ",
    "assign the intermediate slice to a local first"
  )
}

.mojor_diag_index_negative_write_non_output <- function() {
  "negative indexed writes are supported only on the output base variable"
}

.mojor_diag_index_slice_assign_lowering_failed <- function() {
  "IR slice assignment lowering failed for this indexing pattern"
}

# =============================================================================
# .mojor_ir_normalize() <U+2014> Canonical-form pass
# =============================================================================
# Step N.1: scalar_index unwrapping
# Wherever a scalar_index wrapper node appears inside an index or subscript
# indices list, replace it with its inner expr. After this pass every
# consumer (index emitter, subscript emitter, bounds guard) can assume that
# index positions are either:
# <U+2022> slice_index <U+2014> a range dimension
# <U+2022> missing_index <U+2014> an omitted dimension
# <U+2022> a plain IR expression <U+2014> a scalar dimension
#
# The scalar_index node type is part of Sugar (see IR_SPEC_V0.md <U+00A7>1b). It is
# only needed during AST-to-IR construction to carry provenance. After
# normalization it is an error for scalar_index to appear in indices lists.
.mojor_ir_normalize <- function(node) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(node)
  }
  kind <- node$kind

 # Helper: recursively normalize a list of child nodes
  norm_list <- function(lst) lapply(lst, .mojor_ir_normalize)

 # scalar_index itself: unwrap to inner expression (normalizes children too)
  if (kind == "scalar_index") {
    return(.mojor_ir_normalize(node$expr))
  }
  if (kind == "vec_index") {
    # Canonicalize vector selector sugar to its expression node so downstream
    # lowering/emission uses one selector vocabulary and no marker symbols.
    return(.mojor_ir_normalize(node$expr))
  }

 # --- Expression nodes ---
  if (kind == "unop") {
    node$expr <- .mojor_ir_normalize(node$expr)
  } else if (kind == "binop") {
    node$lhs <- .mojor_ir_normalize(node$lhs)
    node$rhs <- .mojor_ir_normalize(node$rhs)
  } else if (kind == "cast") {
    node$expr <- .mojor_ir_normalize(node$expr)
  } else if (kind == "call") {
    node$args <- norm_list(node$args)
  } else if (kind == "index") {
    node$base <- .mojor_ir_normalize(node$base)
    node$indices <- norm_list(node$indices)
  } else if (kind == "ifelse") {
    node$cond <- .mojor_ir_normalize(node$cond)
    node$yes <- .mojor_ir_normalize(node$yes)
    node$no <- .mojor_ir_normalize(node$no)
 # --- Indexing helper nodes ---
  } else if (kind == "slice_index") {
    node$start <- .mojor_ir_normalize(node$start)
    node$end <- .mojor_ir_normalize(node$end)
  } else if (kind == "subscript") {
    node$indices <- norm_list(node$indices)
 # --- Constructor nodes ---
  } else if (kind == "c") {
    node$parts <- norm_list(node$parts)
  } else if (kind %in% c("rep", "rep_len")) {
    node$x <- .mojor_ir_normalize(node$x)
    if (!is.null(node$times)) node$times <- .mojor_ir_normalize(node$times)
    if (!is.null(node$each)) node$each <- .mojor_ir_normalize(node$each)
    if (!is.null(node$length_out)) node$length_out <- .mojor_ir_normalize(node$length_out)
 # --- Statement nodes ---
  } else if (kind == "assign") {
    node$lhs <- .mojor_ir_normalize(node$lhs)
    node$rhs <- .mojor_ir_normalize(node$rhs)
  } else if (kind == "if") {
    node$cond <- .mojor_ir_normalize(node$cond)
    node$then <- .mojor_ir_normalize(node$then)
    node$else_block <- .mojor_ir_normalize(node$else_block)
  } else if (kind == "loop") {
    node$range <- .mojor_ir_normalize(node$range)
    node$body <- .mojor_ir_normalize(node$body)
  } else if (kind == "while") {
    node$cond <- .mojor_ir_normalize(node$cond)
    node$body <- .mojor_ir_normalize(node$body)
  } else if (kind == "repeat") {
    node$body <- .mojor_ir_normalize(node$body)
  } else if (kind == "return") {
    if (!is.null(node$value)) node$value <- .mojor_ir_normalize(node$value)
  } else if (kind == "scalar_reduce") {
 # No IR child nodes <U+2014> acc and arg are plain strings
  } else if (kind == "scheduled_reduce") {
 # No IR child nodes <U+2014> all fields are scalar metadata
  } else if (kind == "block") {
    node$stmts <- norm_list(node$stmts)
  } else if (kind == "range") {
    node$start <- .mojor_ir_normalize(node$start)
    node$end <- .mojor_ir_normalize(node$end)
    if (!is.null(node$step)) node$step <- .mojor_ir_normalize(node$step)
  }
 # const, var, missing_index, break, next, range_expr, raw: no IR children

  node
}

.mojor_ir_format <- function(node, indent = "", as_lines = FALSE) {
  if (is.null(node)) {
    line <- paste0(indent, "<null>")
    return(line)
  }
  if (!is.list(node)) {
    line <- paste0(indent, "<atom> ", as.character(node))
    return(line)
  }
  kind <- node$kind
  if (is.null(kind)) {
    line <- paste0(indent, "<unknown>")
    return(line)
  }
  line <- switch(kind,
    "const" = paste0(indent, "const ", node$value),
    "var" = paste0(indent, "var ", node$name),
    "unop" = paste0(indent, "unop ", node$op),
    "binop" = paste0(indent, "binop ", node$op),
    "cast" = paste0(indent, "cast ", node$to),
    "call" = paste0(indent, "call ", node$fn),
    "index" = paste0(indent, "index ", if (!is.null(node$index_base)) node$index_base else ""),
    "assign" = paste0(indent, "assign"),
    "if" = paste0(indent, "if"),
    "loop" = paste0(indent, "loop ", node$var),
    "while" = paste0(indent, "while"),
    "break" = paste0(indent, "break"),
    "next" = paste0(indent, "next (continue)"),
    "repeat" = paste0(indent, "repeat (while True)"),
    "return" = paste0(indent, "return"),
    "scalar_reduce" = paste0(indent, "scalar_reduce ", node$op, " ", node$acc, " <- ", node$arg),
    "scheduled_reduce" = paste0(indent, "scheduled_reduce ", node$mode, " ", node$op, " ", node$acc, " <- ", node$arg),
    "slice_index" = paste0(indent, "slice_index"),
    "scalar_index" = paste0(indent, "scalar_index"),
    "vec_index" = paste0(indent, "vec_index"),
    "missing_index" = paste0(indent, "missing_index"),
    "subscript" = paste0(indent, "subscript ", node$var),
    "range" = paste0(indent, "range"),
    "range_expr" = paste0(indent, "range_expr"),
    "block" = paste0(indent, "block"),
    "raw" = paste0(indent, "raw"),
    paste0(indent, kind)
  )
  lines <- c(line)
  add_child <- function(label, child) {
    if (is.null(child)) {
      return(invisible(NULL))
    }
    lines <<- c(lines, paste0(indent, "  ", label, ":"))
    lines <<- c(lines, .mojor_ir_format(child, paste0(indent, "    "), as_lines = TRUE))
    invisible(NULL)
  }
  add_children <- function(label, children) {
    if (is.null(children)) {
      return(invisible(NULL))
    }
    lines <<- c(lines, paste0(indent, "  ", label, ":"))
    for (idx in seq_along(children)) {
      lines <<- c(lines, paste0(indent, "    [", idx, "]"))
      lines <<- c(lines, .mojor_ir_format(children[[idx]], paste0(indent, "      "), as_lines = TRUE))
    }
    invisible(NULL)
  }
  # Keep scalar_index as an IR node, but print index/subscript children as their
  # wrapped scalar expression for stable, concise golden output.
  format_index_children <- function(children) {
    if (is.null(children)) {
      return(children)
    }
    lapply(children, function(ch) {
      if (is.list(ch) && identical(ch$kind, "scalar_index") && !is.null(ch$expr)) {
        return(ch$expr)
      }
      ch
    })
  }
  if (kind == "unop") add_child("expr", node$expr)
  if (kind == "binop") {
    add_child("lhs", node$lhs)
    add_child("rhs", node$rhs)
  }
  if (kind == "cast") add_child("expr", node$expr)
  if (kind == "call") add_children("args", node$args)
  if (kind == "index") {
    add_child("base", node$base)
    add_children("indices", format_index_children(node$indices))
  }
  if (kind == "assign") {
    add_child("lhs", node$lhs)
    add_child("rhs", node$rhs)
  }
  if (kind == "if") {
    add_child("cond", node$cond)
    add_child("then", node$then)
    add_child("else", node$else_block)
  }
  if (kind == "loop") {
    add_child("range", node$range)
    add_child("body", node$body)
  }
  if (kind == "return") {
    add_child("value", node$value)
  }
 # Step 8.2: While loop formatting
  if (kind == "while") {
    add_child("cond", node$cond)
    add_child("body", node$body)
  }
 # Step 8.7: Repeat loop formatting
  if (kind == "repeat") {
    add_child("body", node$body)
  }
 # Step 8.2: Break and next have no children
  if (kind == "range") {
    add_child("start", node$start)
    add_child("end", node$end)
    add_child("step", node$step)
  }
  if (kind == "range_expr") add_child("expr", node$expr)
  if (kind == "block") add_children("stmts", node$stmts)
  if (kind == "raw") add_child("expr", node$expr)
 # Step 8.5: Slice node formatting
  if (kind == "slice_index") {
    add_child("start", node$start)
    add_child("end", node$end)
  }
  if (kind == "scalar_index") add_child("expr", node$expr)
  if (kind == "vec_index") add_child("expr", node$expr)
  if (kind == "subscript") add_children("indices", format_index_children(node$indices))
 # missing_index has no children
  if (isTRUE(as_lines)) {
    return(lines)
  }
  paste(lines, collapse = "\n")
}

# Step 8.5: Slice detection helper
.mojor_ir_strip_index_wrappers <- function(idx_expr) {
  out <- idx_expr
  for (iter in seq_len(64L)) {
    if (!is.call(out) || length(out) != 2) {
      break
    }
    op <- as.character(out[[1]])
    if (identical(op, "(") || identical(op, "+")) {
      out <- out[[2]]
      next
    }
    break
  }
  out
}

.mojor_ir_rebuild_call <- function(fn_name, args = list(), arg_names = NULL) {
  call_items <- c(list(as.name(fn_name)), args)
  if (!is.null(arg_names)) {
    names(call_items) <- c("", arg_names)
  }
  as.call(call_items)
}

.mojor_ir_read_call <- function(helper_name, ptr_expr, idx_expr, upper_expr) {
  call <- paste0(
    helper_name, "(", ptr_expr, ", ", idx_expr, ", ", upper_expr
  )
  if (isTRUE(.mojor_state$options$index_bounds)) {
    call <- paste0(call, ", __mojor_na_flag")
  }
  paste0(call, ")")
}

.mojor_ir_normalize_seq_selector_call <- function(seq_expr, normalize_child) {
  if (!is.call(seq_expr)) {
    return(seq_expr)
  }
  op <- as.character(seq_expr[[1]])
  args <- as.list(seq_expr)[-1]
  arg_names <- names(args)

  pick_arg <- function(pos, nm = NULL) {
    if (!is.null(arg_names) && !is.null(nm) && nm %in% arg_names) {
      return(args[[which(arg_names == nm)[1]]])
    }
    if (length(args) >= pos) {
      return(args[[pos]])
    }
    NULL
  }

  if (identical(op, "seq_len")) {
    if (length(args) == 1L) {
      end_expr <- normalize_child(args[[1]])
      return(.mojor_ir_rebuild_call("seq.int", list(1L, end_expr)))
    }
    norm_args <- lapply(args, normalize_child)
    return(.mojor_ir_rebuild_call(op, norm_args, arg_names))
  }

  if (!(op %in% c("seq", "seq.int"))) {
    norm_args <- lapply(args, normalize_child)
    return(.mojor_ir_rebuild_call(op, norm_args, arg_names))
  }

  from <- pick_arg(1, "from")
  to <- pick_arg(2, "to")
  by <- pick_arg(3, "by")
  length_out <- pick_arg(3, "length.out")
  along_with <- pick_arg(2, "along.with")

  if (!is.null(length_out) || !is.null(along_with)) {
    norm_args <- lapply(args, normalize_child)
    return(.mojor_ir_rebuild_call(op, norm_args, arg_names))
  }

  if (length(args) == 1L && is.null(to)) {
    to <- from
    from <- 1L
  }
  if (is.null(from) || is.null(to)) {
    norm_args <- lapply(args, normalize_child)
    return(.mojor_ir_rebuild_call(op, norm_args, arg_names))
  }

  from <- normalize_child(from)
  to <- normalize_child(to)
  if (is.null(by)) {
    return(.mojor_ir_rebuild_call("seq.int", list(from, to)))
  }

  by <- normalize_child(by)
  if ((is.numeric(by) || is.integer(by)) && length(by) == 1L && !is.na(by) && as.integer(by) == 1L) {
    return(.mojor_ir_rebuild_call("seq.int", list(from, to)))
  }

  .mojor_ir_rebuild_call("seq.int", list(from, to, by), c("", "", "by"))
}

.mojor_ir_normalize_selector_ast <- function(expr, mode = c("selector", "loop_seq")) {
  mode <- match.arg(mode)
  normalize <- NULL
  normalize <- function(node) {
    node <- .mojor_ir_strip_index_wrappers(node)
    if (!is.call(node)) {
      return(node)
    }

    op <- as.character(node[[1]])
    if (identical(op, ":") && length(node) == 3L) {
      return(.mojor_ir_rebuild_call(":", list(normalize(node[[2]]), normalize(node[[3]]))))
    }
    if (identical(op, "-") && length(node) == 2L) {
      return(.mojor_ir_rebuild_call("-", list(normalize(node[[2]]))))
    }
    if (identical(mode, "loop_seq") && identical(op, "seq_len")) {
      args <- as.list(node)[-1]
      arg_names <- names(args)
      norm_args <- lapply(args, normalize)
      return(.mojor_ir_rebuild_call("seq_len", norm_args, arg_names))
    }
    if (op %in% c("seq", "seq.int", "seq_len")) {
      return(.mojor_ir_normalize_seq_selector_call(node, normalize))
    }
    if (identical(op, "c")) {
      args <- as.list(node)[-1]
      arg_names <- names(args)
      norm_args <- lapply(args, normalize)
      return(.mojor_ir_rebuild_call("c", norm_args, arg_names))
    }

    if (identical(mode, "loop_seq")) {
      args <- as.list(node)[-1]
      arg_names <- names(args)
      if (length(args) > 0L) {
        norm_args <- lapply(args, normalize)
        return(.mojor_ir_rebuild_call(op, norm_args, arg_names))
      }
    }

    node
  }
  normalize(expr)
}

.mojor_ir_is_missing_call_arg_ast <- function(arg) {
  is.symbol(arg) && identical(as.character(arg), "")
}

.mojor_ir_is_index_control_arg_ast <- function(arg_name, is_double_bracket = FALSE) {
  if (is.null(arg_name) || length(arg_name) == 0L) {
    return(FALSE)
  }
  name <- tolower(as.character(arg_name[[1L]]))
  if (!nzchar(name)) {
    return(FALSE)
  }
  if (identical(name, "drop")) {
    return(TRUE)
  }
  if (is_double_bracket && identical(name, "exact")) {
    return(TRUE)
  }
  FALSE
}

.mojor_ir_canonicalize_loop_index_ast <- function(node) {
  walk <- NULL
  walk <- function(cur) {
    if (is.null(cur) || !is.call(cur)) {
      return(cur)
    }

    op <- as.character(cur[[1]])
    parts <- as.list(cur)
    n_parts <- length(parts)
    nms <- names(parts)
    if (is.null(nms)) {
      nms <- rep.int("", n_parts)
    }

    # for (i in seq_expr) { ... }: canonicalize the range expression once.
    if (identical(op, "for") && n_parts >= 4L) {
      if (!.mojor_ir_is_missing_call_arg_ast(parts[[3L]])) {
        seq_expr <- walk(parts[[3L]])
        parts[[3L]] <- .mojor_ir_normalize_selector_ast(seq_expr, mode = "loop_seq")
      }
      parts[[4L]] <- walk(parts[[4L]])
      if (n_parts > 4L) {
        for (i in 5:n_parts) {
          if (!.mojor_ir_is_missing_call_arg_ast(parts[[i]])) {
            parts[[i]] <- walk(parts[[i]])
          }
        }
      }
      names(parts) <- nms
      return(as.call(parts))
    }

    # x[idx] / x[[idx]]: canonicalize selector arguments (skip drop/exact controls).
    if (op %in% c("[", "[[")) {
      is_double_bracket <- identical(op, "[[")
      if (n_parts >= 2L && !.mojor_ir_is_missing_call_arg_ast(parts[[2L]])) {
        parts[[2L]] <- walk(parts[[2L]])
      }
      if (n_parts >= 3L) {
        for (i in 3:n_parts) {
          if (.mojor_ir_is_missing_call_arg_ast(parts[[i]])) {
            next
          }
          arg_name <- nms[[i]]
          arg_walked <- walk(parts[[i]])
          if (.mojor_ir_is_index_control_arg_ast(arg_name, is_double_bracket = is_double_bracket)) {
            parts[[i]] <- arg_walked
            next
          }
          parts[[i]] <- .mojor_ir_normalize_selector_ast(arg_walked, mode = "selector")
        }
      }
      names(parts) <- nms
      return(as.call(parts))
    }

    if (n_parts >= 2L) {
      for (i in 2:n_parts) {
        if (!.mojor_ir_is_missing_call_arg_ast(parts[[i]])) {
          parts[[i]] <- walk(parts[[i]])
        }
      }
    }
    names(parts) <- nms
    as.call(parts)
  }
  walk(node)
}

.mojor_ir_selector_unwrap_node <- function(idx_node) {
  if (is.null(idx_node) || !is.list(idx_node) || is.null(idx_node$kind)) {
    return(idx_node)
  }
  if (identical(idx_node$kind, "scalar_index") && !is.null(idx_node$expr)) {
    return(idx_node$expr)
  }
  if (identical(idx_node$kind, "vec_index") && !is.null(idx_node$expr)) {
    return(idx_node$expr)
  }
  idx_node
}

.mojor_ir_selector_literal_value <- function(node, depth = 0L) {
  if (is.null(node) || depth > 16L) {
    return(NULL)
  }
  node <- .mojor_ir_strip_index_wrappers(node)
  if ((is.numeric(node) || is.integer(node)) && length(node) == 1L && !is.na(node)) {
    return(as.numeric(node))
  }
  if (is.call(node) && length(node) == 2L) {
    op <- as.character(node[[1]])
    inner <- .mojor_ir_selector_literal_value(node[[2]], depth + 1L)
    if (is.null(inner)) {
      return(NULL)
    }
    if (identical(op, "+")) {
      return(inner)
    }
    if (identical(op, "-")) {
      return(-inner)
    }
  }
  NULL
}

.mojor_ir_selector_literal_scalar <- function(node, depth = 0L) {
  if (is.null(node) || depth > 32L) {
    return(NULL)
  }
  node <- .mojor_ir_strip_index_wrappers(node)

  if (is.logical(node) && length(node) == 1L) {
    return(list(kind = "logical", value = if (is.na(node)) NA else isTRUE(node)))
  }
  if ((is.numeric(node) || is.integer(node)) && length(node) == 1L) {
    return(list(kind = "numeric", value = as.numeric(node)))
  }
  if (is.name(node)) {
    nm <- as.character(node)
    if (identical(nm, "TRUE")) {
      return(list(kind = "logical", value = TRUE))
    }
    if (identical(nm, "FALSE")) {
      return(list(kind = "logical", value = FALSE))
    }
    if (nm %in% c("NA", "NA_real_", "NA_integer_", "NA_logical_", "NA_character_", "NA_complex_")) {
      return(list(kind = "numeric", value = NA_real_))
    }
    if (identical(nm, "NaN")) {
      return(list(kind = "numeric", value = NaN))
    }
    if (identical(nm, "Inf")) {
      return(list(kind = "numeric", value = Inf))
    }
    return(NULL)
  }
  if (is.call(node) && length(node) == 2L) {
    op <- as.character(node[[1]])
    if (op %in% c("+", "-")) {
      inner <- .mojor_ir_selector_literal_scalar(node[[2]], depth + 1L)
      if (is.null(inner) || !identical(inner$kind, "numeric")) {
        return(NULL)
      }
      val <- inner$value
      if (identical(op, "-")) {
        val <- -val
      }
      return(list(kind = "numeric", value = val))
    }
  }
  NULL
}

.mojor_ir_selector_collect_issues <- function(node, depth = 0L) {
  out <- list(has_na = FALSE, has_nonfinite = FALSE)
  walk <- function(expr, level = 0L) {
    if (isTRUE(out$has_na) && isTRUE(out$has_nonfinite)) {
      return(invisible(NULL))
    }
    if (is.null(expr) || level > 64L) {
      return(invisible(NULL))
    }
    lit <- .mojor_ir_selector_literal_scalar(expr, depth = level)
    if (!is.null(lit)) {
      val <- lit$value
      if (isTRUE(is.na(val))) {
        out$has_na <<- TRUE
      } else if (identical(lit$kind, "numeric") && isTRUE(!is.finite(val))) {
        out$has_nonfinite <<- TRUE
      }
      return(invisible(NULL))
    }
    expr <- .mojor_ir_strip_index_wrappers(expr)
    if (!is.call(expr)) {
      return(invisible(NULL))
    }
    if (length(expr) >= 2L) {
      for (i in 2:length(expr)) {
        arg <- expr[[i]]
        if (.mojor_ir_is_missing_call_arg_ast(arg)) {
          next
        }
        walk(arg, level + 1L)
      }
    }
    invisible(NULL)
  }
  walk(node, depth)
  out
}

.mojor_ir_selector_logical_literal_length <- function(node, depth = 0L) {
  if (is.null(node) || depth > 32L) {
    return(NULL)
  }
  node <- .mojor_ir_strip_index_wrappers(node)
  lit <- .mojor_ir_selector_literal_scalar(node, depth = depth)
  if (!is.null(lit) && identical(lit$kind, "logical")) {
    return(1L)
  }
  if (!is.call(node) || !identical(as.character(node[[1]]), "c")) {
    return(NULL)
  }
  elems <- as.list(node)[-1]
  if (length(elems) == 0L) {
    return(0L)
  }
  for (elem in elems) {
    lit_elem <- .mojor_ir_selector_literal_scalar(elem, depth = depth + 1L)
    if (is.null(lit_elem) || !identical(lit_elem$kind, "logical")) {
      return(NULL)
    }
  }
  as.integer(length(elems))
}

.mojor_ir_selector_infer_target_extent_ast <- function(base_expr, axis = 1L, depth = 0L) {
  if (is.null(base_expr) || depth > 16L) {
    return(NULL)
  }
  if (!isTRUE(axis == 1L)) {
    return(NULL)
  }
  base_expr <- .mojor_ir_strip_index_wrappers(base_expr)
  if (!is.call(base_expr)) {
    return(NULL)
  }
  op <- as.character(base_expr[[1]])
  scalar_int <- function(expr) {
    lit <- .mojor_ir_selector_literal_scalar(expr, depth = depth + 1L)
    if (is.null(lit) || !identical(lit$kind, "numeric")) {
      return(NA_integer_)
    }
    val <- lit$value
    if (is.na(val) || !is.finite(val)) {
      return(NA_integer_)
    }
    ival <- suppressWarnings(as.integer(val))
    if (is.na(ival)) {
      return(NA_integer_)
    }
    ival
  }

  if (identical(op, "c")) {
    return(length(as.list(base_expr)) - 1L)
  }
  if (op %in% c("integer", "numeric", "logical") && length(base_expr) == 2L) {
    n <- scalar_int(base_expr[[2]])
    if (!is.na(n) && n >= 0L) {
      return(n)
    }
    return(NULL)
  }
  if (identical(op, "seq_len") && length(base_expr) == 2L) {
    n <- scalar_int(base_expr[[2]])
    if (!is.na(n) && n >= 0L) {
      return(n)
    }
    return(NULL)
  }
  if (op %in% c("seq", "seq.int") && length(base_expr) >= 2L) {
    args <- as.list(base_expr)[-1]
    arg_names <- names(args)
    pick_arg <- function(pos, nm = NULL) {
      if (!is.null(arg_names) && !is.null(nm) && nm %in% arg_names) {
        return(args[[which(arg_names == nm)[1L]]])
      }
      if (length(args) >= pos) {
        return(args[[pos]])
      }
      NULL
    }
    from <- pick_arg(1L, "from")
    to <- pick_arg(2L, "to")
    by <- pick_arg(3L, "by")
    if (length(args) == 1L && is.null(to)) {
      to <- from
      from <- 1L
    }
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    by_val <- if (is.null(by)) 1L else scalar_int(by)
    if (is.na(by_val) || by_val == 0L) {
      return(NULL)
    }
    from_val <- scalar_int(from)
    to_val <- scalar_int(to)
    if (is.na(from_val) || is.na(to_val)) {
      return(NULL)
    }
    n <- floor((to_val - from_val) / by_val) + 1L
    if (is.na(n) || n < 0L) {
      return(NULL)
    }
    return(as.integer(n))
  }
  if (identical(op, "rep_len") && length(base_expr) >= 3L) {
    n <- scalar_int(base_expr[[3]])
    if (!is.na(n) && n >= 0L) {
      return(n)
    }
    return(NULL)
  }
  if (identical(op, "rep") && length(base_expr) >= 3L) {
    base_len <- .mojor_ir_selector_infer_target_extent_ast(base_expr[[2]], axis = 1L, depth = depth + 1L)
    times <- scalar_int(base_expr[[3]])
    if (!is.null(base_len) && !is.na(times) && times >= 0L) {
      return(as.integer(base_len * times))
    }
    return(NULL)
  }
  NULL
}

.mojor_ir_validate_selector_ast <- function(
    idx_expr, idx_src = idx_expr, context = c("index", "assignment"),
    target_extent = NULL
) {
  context <- match.arg(context)
  ix <- .mojor_ir_normalize_selector_ast(idx_expr, mode = "selector")
  issues <- .mojor_ir_selector_collect_issues(ix)

  if (isTRUE(issues$has_na)) {
    .mojor_err(
      "NA selector values are not supported",
      idx_src,
      "use explicit non-NA selector values"
    )
  }
  if (isTRUE(issues$has_nonfinite)) {
    .mojor_err(
      "non-finite selector values (Inf/NaN) are not supported",
      idx_src,
      "use finite selector bounds/values"
    )
  }

  scalar_val <- .mojor_ir_selector_literal_value(ix)
  if (!is.null(scalar_val) && isTRUE(all.equal(scalar_val, 0.0))) {
    .mojor_err(
      "index 0 is unsupported in scalar selector",
      idx_src,
      "use positive/negative scalar selectors or an explicit empty-vector path"
    )
  }

  if (is.call(ix) && identical(as.character(ix[[1]]), "c")) {
    elems <- as.list(ix)[-1]
    has_pos <- FALSE
    has_neg <- FALSE
    for (elem in elems) {
      val <- .mojor_ir_selector_literal_value(elem)
      if (is.null(val) || isTRUE(all.equal(val, 0.0))) {
        next
      }
      if (val > 0) {
        has_pos <- TRUE
      } else if (val < 0) {
        has_neg <- TRUE
      }
      if (has_pos && has_neg) {
        .mojor_err(
          "mixed positive and negative selector values are not supported",
          idx_src,
          "split selector into all-positive selection or all-negative exclusion"
        )
      }
    }
  }

  target_extent_i <- suppressWarnings(as.integer(target_extent))
  if (length(target_extent_i) == 1L &&
      !is.na(target_extent_i) &&
      target_extent_i > 0L) {
    logical_len <- .mojor_ir_selector_logical_literal_length(ix)
    if (!is.null(logical_len) && logical_len > 0L &&
        (target_extent_i %% logical_len) != 0L) {
      .mojor_err(
        "logical selector length mismatch for statically known extent",
        idx_src,
        paste0(
          "logical selector length ", logical_len,
          " does not divide extent ", target_extent_i
        )
      )
    }
  }

  invisible(ix)
}

.mojor_ir_validate_selector_node <- function(
    idx_node, context = c("index", "assignment"),
    target_extent = NULL
) {
  context <- match.arg(context)
  idx_node <- .mojor_ir_selector_unwrap_node(idx_node)
  if (is.null(idx_node) || !is.list(idx_node) || is.null(idx_node$kind)) {
    return(invisible(NULL))
  }
  if (identical(idx_node$kind, "missing_index")) {
    return(invisible(NULL))
  }
  if (identical(idx_node$kind, "raw")) {
    stop("mojor_transpile: IR selector lowering failed: unresolved raw selector index after canonicalization")
  }
  if (!is.null(idx_node$src)) {
    .mojor_ir_validate_selector_ast(
      idx_node$src,
      idx_node$src,
      context = context,
      target_extent = target_extent
    )
    return(invisible(NULL))
  }

  if (identical(idx_node$kind, "const")) {
    scalar_num <- suppressWarnings(as.numeric(idx_node$value))
    if (!is.na(scalar_num) && !is.finite(scalar_num)) {
      .mojor_err(
        "non-finite selector values (Inf/NaN) are not supported",
        idx_node$value,
        "use finite selector bounds/values"
      )
    }
    if (!is.na(scalar_num) && isTRUE(all.equal(scalar_num, 0.0))) {
      .mojor_err(
        "index 0 is unsupported in scalar selector",
        idx_node$value,
        "use positive/negative scalar selectors or an explicit empty-vector path"
      )
    }
  }
  invisible(NULL)
}

.mojor_ir_map_rhs_slices_to_loop_indices <- function(node, lhs_indices,
                                                     lhs_axis_loop_var_fn,
                                                     lhs_axis_start_fn,
                                                     rhs_zero_based_vars = character(0)) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(node)
  }
  kind <- node$kind

  is_rhs_selector_idx <- function(idx_node) {
    is.list(idx_node) &&
      !is.null(idx_node$kind) &&
      idx_node$kind %in% c("slice_index", "missing_index")
  }
  is_lhs_selector_axis <- function(idx_node) {
    is.list(idx_node) &&
      !is.null(idx_node$kind) &&
      idx_node$kind %in% c("slice_index", "missing_index", "vec_index", "c")
  }

  if (identical(kind, "index") && is.list(node$indices) && length(node$indices) > 0) {
    has_selector <- any(vapply(node$indices, function(idx) {
      is_rhs_selector_idx(.mojor_ir_selector_unwrap_node(idx))
    }, logical(1)))
    if (has_selector) {
      mapped <- vector("list", length(node$indices))
      resolve_rhs_source_base_name <- function(base_name) {
        if (is.null(base_name) || !nzchar(base_name)) {
          return(base_name)
        }
        if (startsWith(base_name, "__mojor_tensor_")) {
          return(sub("^__mojor_tensor_", "", base_name))
        }
        tensor_map <- .mojor_state$current_tensor_map
        if (!is.null(tensor_map) && !is.list(tensor_map)) {
          tensor_map <- as.list(tensor_map)
        }
        if (is.null(tensor_map) || length(tensor_map) == 0L) {
          return(base_name)
        }
        if (!is.null(names(tensor_map)) && base_name %in% names(tensor_map)) {
          return(base_name)
        }
        tensor_vals <- unlist(tensor_map, use.names = TRUE)
        if (length(tensor_vals) == 0L) {
          return(base_name)
        }
        matches <- names(tensor_vals)[!is.na(tensor_vals) & tensor_vals == base_name]
        if (length(matches) > 0L && nzchar(matches[[1]])) {
          return(matches[[1]])
        }
        base_name
      }
      resolve_rhs_axis_extent_node <- function(base_name, rhs_axis, rhs_idx_node, rhs_rank) {
        if (is.null(rhs_idx_node) || is.null(rhs_idx_node$kind)) {
          return(NULL)
        }
        if (identical(rhs_idx_node$kind, "slice_index")) {
          if (is.null(rhs_idx_node$start) || !is.list(rhs_idx_node$start) ||
              !identical(rhs_idx_node$start$kind, "const") || is.null(rhs_idx_node$end)) {
            return(NULL)
          }
          rhs_start_num <- suppressWarnings(as.numeric(rhs_idx_node$start$value))
          if (is.na(rhs_start_num) || !is.finite(rhs_start_num)) {
            return(NULL)
          }
          rhs_start0 <- as.integer(rhs_start_num) - 1L
          end_node <- .mojor_ir_call("Int", list(rhs_idx_node$end))
          if (rhs_start0 <= 0L) {
            return(end_node)
          }
          return(.mojor_ir_binop("-", end_node, .mojor_ir_const(as.character(rhs_start0))))
        }
        if (!identical(rhs_idx_node$kind, "missing_index")) {
          return(NULL)
        }
        source_base_name <- resolve_rhs_source_base_name(base_name)
        if (rhs_rank == 1L) {
          n_source_name <- .mojor_state$current_n_source_name
          len_var_map <- .mojor_state$current_len_var_map
          if (!is.null(n_source_name) && identical(source_base_name, n_source_name)) {
            return(.mojor_ir_var("n_i"))
          }
          if (!is.null(len_var_map) && !is.null(names(len_var_map)) &&
              source_base_name %in% names(len_var_map)) {
            return(.mojor_ir_var(len_var_map[[source_base_name]]))
          }
          return(.mojor_ir_var("n_i"))
        }
        if (rhs_rank == 2L) {
          if (rhs_axis == 1L) {
            nrow_map <- .mojor_state$current_nrow_var_map
            if (!is.null(nrow_map) && !is.null(nrow_map[[source_base_name]])) {
              return(.mojor_ir_var(nrow_map[[source_base_name]]))
            }
          } else if (rhs_axis == 2L) {
            ncol_map <- .mojor_state$current_ncol_var_map
            if (!is.null(ncol_map) && !is.null(ncol_map[[source_base_name]])) {
              return(.mojor_ir_var(ncol_map[[source_base_name]]))
            }
          }
        }
        dim_map <- .mojor_state$current_dim_var_map
        if (!is.null(dim_map) && !is.null(dim_map[[source_base_name]])) {
          return(.mojor_ir_index(
            .mojor_ir_var(dim_map[[source_base_name]]),
            list(.mojor_ir_const(as.character(rhs_axis - 1L))),
            index_base = "zero_based",
            src = NULL
          ))
        }
        NULL
      }
      lhs_selector_axes <- which(vapply(lhs_indices, is_lhs_selector_axis, logical(1)))
      if (length(lhs_selector_axes) == 0L) {
        return(node)
      }
      used_lhs_axes <- integer(0)
      next_lhs_ptr <- 1L
      pick_lhs_axis <- function(rhs_axis) {
        if (rhs_axis <= length(lhs_indices)) {
          if (rhs_axis %in% lhs_selector_axes && !(rhs_axis %in% used_lhs_axes)) {
            used_lhs_axes <<- c(used_lhs_axes, rhs_axis)
            return(rhs_axis)
          }
        }
        while (next_lhs_ptr <= length(lhs_selector_axes) &&
               lhs_selector_axes[[next_lhs_ptr]] %in% used_lhs_axes) {
          next_lhs_ptr <<- next_lhs_ptr + 1L
        }
        if (next_lhs_ptr > length(lhs_selector_axes)) {
          return(NA_integer_)
        }
        axis <- lhs_selector_axes[[next_lhs_ptr]]
        used_lhs_axes <<- c(used_lhs_axes, axis)
        next_lhs_ptr <<- next_lhs_ptr + 1L
        axis
      }
      for (ai in seq_along(node$indices)) {
        rhs_idx <- node$indices[[ai]]
        rhs_idx <- .mojor_ir_selector_unwrap_node(rhs_idx)
        if (is_rhs_selector_idx(rhs_idx)) {
          lhs_axis <- pick_lhs_axis(ai)
          if (is.na(lhs_axis)) return(node)
          loop_var <- lhs_axis_loop_var_fn(lhs_axis)
          if (is.null(loop_var)) return(node)
          idx_node <- .mojor_ir_var(loop_var)
          rhs_start_num <- NA_real_
          if (identical(rhs_idx$kind, "slice_index")) {
            if (!is.null(rhs_idx$start) && is.list(rhs_idx$start) &&
                identical(rhs_idx$start$kind, "const")) {
              rhs_start_num <- suppressWarnings(as.numeric(rhs_idx$start$value))
            }
          } else if (identical(rhs_idx$kind, "missing_index")) {
            rhs_start_num <- 1
          }
          lhs_start <- lhs_axis_start_fn(lhs_axis)
          lhs_start0 <- if (is.na(lhs_start)) 0L else as.integer(lhs_start) - 1L
          rhs_start0 <- if (is.na(rhs_start_num)) 0L else as.integer(rhs_start_num) - 1L
          if (lhs_start0 != 0L) {
            idx_node <- .mojor_ir_binop("-", idx_node, .mojor_ir_const(as.character(lhs_start0)))
          }
          rhs_extent_node <- resolve_rhs_axis_extent_node(node$base$name, ai, rhs_idx, length(node$indices))
          if (!is.null(rhs_extent_node)) {
            idx_node <- .mojor_ir_binop(
              "%%",
              idx_node,
              .mojor_ir_call("Int", list(rhs_extent_node))
            )
          }
          if (rhs_start0 != 0L) {
            idx_node <- .mojor_ir_binop("+", idx_node, .mojor_ir_const(as.character(rhs_start0)))
          }
          idx_node$`__mojor_index_normalized` <- TRUE
          mapped[[ai]] <- idx_node
        } else {
          idx_node <- rhs_idx
          if (!identical(node$index_base, "zero_based")) {
            idx_vars <- character(0)
            if (!is.null(idx_node$kind) && identical(idx_node$kind, "var") && !is.null(idx_node$name)) {
              idx_vars <- idx_node$name
            }
            idx_node <- .mojor_ir_normalize_index_expr(
              idx_node,
              idx_vars,
              rhs_zero_based_vars
            )
          }
          idx_node$`__mojor_index_normalized` <- TRUE
          mapped[[ai]] <- idx_node
        }
      }
      return(.mojor_ir_index(
        node$base,
        mapped,
        index_base = "zero_based",
        src = node$src
      ))
    }
  }

  if (identical(kind, "binop")) {
    if (!is.null(node$lhs)) {
      node$lhs <- .mojor_ir_map_rhs_slices_to_loop_indices(
        node$lhs, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    }
    if (!is.null(node$rhs)) {
      node$rhs <- .mojor_ir_map_rhs_slices_to_loop_indices(
        node$rhs, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    }
    return(node)
  }
  if (identical(kind, "unop") && !is.null(node$expr)) {
    node$expr <- .mojor_ir_map_rhs_slices_to_loop_indices(
      node$expr, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
      rhs_zero_based_vars = rhs_zero_based_vars
    )
    return(node)
  }
  if (identical(kind, "cast") && !is.null(node$expr)) {
    node$expr <- .mojor_ir_map_rhs_slices_to_loop_indices(
      node$expr, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
      rhs_zero_based_vars = rhs_zero_based_vars
    )
    return(node)
  }
  if (identical(kind, "ifelse")) {
    if (!is.null(node$cond)) {
      node$cond <- .mojor_ir_map_rhs_slices_to_loop_indices(
        node$cond, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    }
    if (!is.null(node$yes)) {
      node$yes <- .mojor_ir_map_rhs_slices_to_loop_indices(
        node$yes, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    }
    if (!is.null(node$no)) {
      node$no <- .mojor_ir_map_rhs_slices_to_loop_indices(
        node$no, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    }
    return(node)
  }
  if (identical(kind, "call") && is.list(node$args)) {
    node$args <- lapply(node$args, function(arg) {
      .mojor_ir_map_rhs_slices_to_loop_indices(
        arg, lhs_indices, lhs_axis_loop_var_fn, lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
      )
    })
    return(node)
  }
  node
}

.mojor_ir_build_slice_linear_pos_expr <- function(slice_loop_vars, slice_loop_extent_nodes) {
  if (length(slice_loop_vars) == 0L) {
    return(NULL)
  }
  if (length(slice_loop_extent_nodes) != length(slice_loop_vars)) {
    return(NULL)
  }
  pos_expr <- .mojor_ir_var(slice_loop_vars[[1L]])
  if (length(slice_loop_vars) == 1L) {
    return(pos_expr)
  }
  stride <- slice_loop_extent_nodes[[1L]]
  for (j in seq.int(2L, length(slice_loop_vars))) {
    pos_expr <- .mojor_ir_binop(
      "+",
      pos_expr,
      .mojor_ir_binop("*", .mojor_ir_var(slice_loop_vars[[j]]), stride)
    )
    stride <- .mojor_ir_binop("*", stride, slice_loop_extent_nodes[[j]])
  }
  pos_expr
}

.mojor_ir_build_slice_cycle_len_expr <- function(slice_loop_extent_nodes) {
  if (length(slice_loop_extent_nodes) == 0L) {
    return(.mojor_ir_const("1"))
  }
  len_expr <- slice_loop_extent_nodes[[1L]]
  if (length(slice_loop_extent_nodes) == 1L) {
    return(len_expr)
  }
  for (j in seq.int(2L, length(slice_loop_extent_nodes))) {
    len_expr <- .mojor_ir_binop("*", len_expr, slice_loop_extent_nodes[[j]])
  }
  len_expr
}

.mojor_ir_scalarize_array_rhs_expr <- function(
    expr_node,
    rhs_pos_expr,
    rhs_cycle_len_expr = NULL,
    type_env = NULL,
    resolve_len_fn = NULL) {
  if (is.null(expr_node) || !is.list(expr_node) || is.null(expr_node$kind)) {
    return(expr_node)
  }
  if (is.null(rhs_pos_expr) || is.null(type_env)) {
    return(expr_node)
  }

  expr_type <- .mojor_ir_infer_type(expr_node, type_env)
  expr_is_array <- !is.null(expr_type) && .mojor_type_is_array(expr_type)
  kind <- expr_node$kind

  if (expr_is_array && identical(kind, "var") && !is.null(expr_node$name)) {
    if (is.null(resolve_len_fn) || !is.function(resolve_len_fn)) {
      return(expr_node)
    }
    rhs_len_info <- resolve_len_fn(
      expr_node$name,
      fallback_node = rhs_cycle_len_expr,
      fallback_label = "rhs_slice_len",
      allow_symbol_fallback = FALSE
    )
    if (is.null(rhs_len_info) || is.null(rhs_len_info$node)) {
      return(expr_node)
    }
    rhs_len <- .mojor_ir_call("Int", list(rhs_len_info$node))
    rhs_idx <- .mojor_ir_binop("%%", rhs_pos_expr, rhs_len)
    rhs_idx$`__mojor_index_normalized` <- TRUE
    return(.mojor_ir_index(
      .mojor_ir_var(expr_node$name),
      list(rhs_idx),
      index_base = "zero_based",
      src = expr_node$src
    ))
  }

  if (identical(kind, "cast") && !is.null(expr_node$expr)) {
    expr_node$expr <- .mojor_ir_scalarize_array_rhs_expr(
      expr_node$expr,
      rhs_pos_expr,
      rhs_cycle_len_expr,
      type_env = type_env,
      resolve_len_fn = resolve_len_fn
    )
    return(expr_node)
  }
  if (identical(kind, "unop") && !is.null(expr_node$expr)) {
    expr_node$expr <- .mojor_ir_scalarize_array_rhs_expr(
      expr_node$expr,
      rhs_pos_expr,
      rhs_cycle_len_expr,
      type_env = type_env,
      resolve_len_fn = resolve_len_fn
    )
    return(expr_node)
  }
  if (identical(kind, "binop")) {
    if (!is.null(expr_node$lhs)) {
      expr_node$lhs <- .mojor_ir_scalarize_array_rhs_expr(
        expr_node$lhs,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    }
    if (!is.null(expr_node$rhs)) {
      expr_node$rhs <- .mojor_ir_scalarize_array_rhs_expr(
        expr_node$rhs,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    }
    return(expr_node)
  }
  if (identical(kind, "ifelse")) {
    if (!is.null(expr_node$cond)) {
      expr_node$cond <- .mojor_ir_scalarize_array_rhs_expr(
        expr_node$cond,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    }
    if (!is.null(expr_node$yes)) {
      expr_node$yes <- .mojor_ir_scalarize_array_rhs_expr(
        expr_node$yes,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    }
    if (!is.null(expr_node$no)) {
      expr_node$no <- .mojor_ir_scalarize_array_rhs_expr(
        expr_node$no,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    }
    return(expr_node)
  }
  if (identical(kind, "call") && is.list(expr_node$args)) {
    expr_node$args <- lapply(expr_node$args, function(arg) {
      .mojor_ir_scalarize_array_rhs_expr(
        arg,
        rhs_pos_expr,
        rhs_cycle_len_expr,
        type_env = type_env,
        resolve_len_fn = resolve_len_fn
      )
    })
    return(expr_node)
  }
  if (identical(kind, "index")) {
    if (is.list(expr_node$indices)) {
      expr_node$indices <- lapply(expr_node$indices, function(idx_expr) {
        .mojor_ir_scalarize_array_rhs_expr(
          idx_expr,
          rhs_pos_expr,
          rhs_cycle_len_expr,
          type_env = type_env,
          resolve_len_fn = resolve_len_fn
        )
      })
    }
    return(expr_node)
  }
  if (identical(kind, "raw") && !is.null(expr_node$expr)) {
    expr_node$expr <- .mojor_ir_scalarize_array_rhs_expr(
      expr_node$expr,
      rhs_pos_expr,
      rhs_cycle_len_expr,
      type_env = type_env,
      resolve_len_fn = resolve_len_fn
    )
    return(expr_node)
  }

  expr_node
}

.mojor_ir_detect_slice <- function(idx_expr) {
 # Detects slice patterns: 1:n, seq_len(n), seq.int(start, end), seq_along(x)
 # Returns: list(start = <ast>, end = <ast>) or NULL

  idx_expr <- .mojor_ir_normalize_selector_ast(idx_expr, mode = "selector")
  if (!is.call(idx_expr)) {
    return(NULL)
  }
  op <- as.character(idx_expr[[1]])

  is_scalar_slice_bound <- function(expr) {
    if (is.numeric(expr) || is.integer(expr) || is.name(expr)) {
      return(TRUE)
    }
    if (is.call(expr)) {
      return(TRUE)
    }
    FALSE
  }

 # Colon: 1:n or start:end
  if (op == ":" && length(idx_expr) == 3) {
    start <- idx_expr[[2]]
    end <- idx_expr[[3]]
 # Validate start is scalar-like; keep literal >= 1 guard for static starts.
    if (!isTRUE(is_scalar_slice_bound(start))) {
      return(NULL)
    }
    if ((is.numeric(start) || is.integer(start)) && length(start) == 1 && !is.na(start) && start < 1) {
      return(NULL)
    }
    return(list(start = start, end = end))
  }

 # seq_len(n) - implicit start = 1
  if (op == "seq_len" && length(idx_expr) == 2) {
    return(list(start = 1, end = idx_expr[[2]]))
  }

 # seq.int(n) or seq.int(start, end)
  if (op == "seq.int") {
    if (length(idx_expr) == 2) {
 # seq.int(n) - implicit start = 1
      return(list(start = 1, end = idx_expr[[2]]))
    }
    if (length(idx_expr) >= 3) {
 # seq.int(start, end)
      start <- idx_expr[[2]]
      end <- idx_expr[[3]]
      if (!isTRUE(is_scalar_slice_bound(start))) {
        return(NULL)
      }
      if ((is.numeric(start) || is.integer(start)) && length(start) == 1 && !is.na(start) && start < 1) {
        return(NULL)
      }
      return(list(start = start, end = end))
    }
  }

 # seq_along(x) - implicit start = 1, end = length(x)
  if (op == "seq_along" && length(idx_expr) == 2) {
    x <- idx_expr[[2]]
 # Build length(x) call as the end expression
    return(list(start = 1, end = call("length", x)))
  }

  NULL
}

# =============================================================================
# Section 4.5: Reduction Pattern Detection (Step 8.9)
# =============================================================================
# Detects inline reduction patterns in loop bodies for future optimization.
# A reduction is a loop with a single assignment of the form:
# acc <- acc OP expr (where OP is +, *, min, or max)
#
# Returns NULL if not a reduction pattern, otherwise returns:
# list(type = "sum"|"prod"|"min"|"max", acc_var = "acc", expr = <IR node>, op = OP)

.mojor_ir_detect_reduction_pattern <- function(loop_node) {
 # Input: loop IR node
 # Output: NULL or list(type, acc_var, expr, op)

  if (is.null(loop_node) || loop_node$kind != "loop") {
    return(NULL)
  }

  body <- loop_node$body
  if (is.null(body) || body$kind != "block") {
    return(NULL)
  }

 # Reduction pattern requires single-statement body
  if (length(body$stmts) != 1) {
    return(NULL)
  }

  stmt <- body$stmts[[1]]
  if (stmt$kind != "assign") {
    return(NULL)
  }

  lhs <- stmt$lhs
  if (lhs$kind != "var") {
    return(NULL)
  }

  acc_var <- lhs$name
  rhs <- stmt$rhs

 # Pattern 1: acc <- acc + expr OR acc <- expr + acc
 # Use .mojor_ir_is_acc_var which strips cast wrappers.
  if (rhs$kind == "binop" && rhs$op %in% c("+", "*")) {
 # Check if accumulator appears on left side
    if (.mojor_ir_is_acc_var(rhs$lhs, acc_var)) {
      return(list(
        kind = if (rhs$op == "+") "sum" else "product",
        acc = acc_var,
        rhs = rhs$rhs,
        op = rhs$op
      ))
    }
 # Check if accumulator appears on right side (commutative)
    if (.mojor_ir_is_acc_var(rhs$rhs, acc_var)) {
      return(list(
        kind = if (rhs$op == "+") "sum" else "product",
        acc = acc_var,
        rhs = rhs$lhs,
        op = rhs$op
      ))
    }
  }

 # Pattern 2: acc <- min(acc, expr) OR acc <- min(expr, acc)
 # Pattern 3: acc <- max(acc, expr) OR acc <- max(expr, acc)
  if (rhs$kind == "call" && rhs$fn %in% c("min", "max")) {
    if (length(rhs$args) == 2) {
 # Check if accumulator is first argument
      if (.mojor_ir_is_acc_var(rhs$args[[1]], acc_var)) {
        return(list(
          kind = rhs$fn,
          acc = acc_var,
          rhs = rhs$args[[2]],
          op = rhs$fn
        ))
      }
 # Check if accumulator is second argument
      if (.mojor_ir_is_acc_var(rhs$args[[2]], acc_var)) {
        return(list(
          kind = rhs$fn,
          acc = acc_var,
          rhs = rhs$args[[1]],
          op = rhs$fn
        ))
      }
    }
  }

  NULL
}

# Helper: Check if an IR node is a variable reference with given name
.mojor_ir_is_var_node <- function(node, var_name) {
  !is.null(node) && is.list(node) && node$kind == "var" && identical(node$name, var_name)
}

.mojor_ir_is_acc_var <- function(node, var_name) {
  .mojor_ir_is_var_node(.mojor_ir_strip_casts(node), var_name)
}

.mojor_ir_strip_casts <- function(node) {
  cur <- node
  while (!is.null(cur) && is.list(cur) &&
    identical(cur$kind, "cast") &&
    !is.null(cur$expr)) {
    cur <- cur$expr
  }
  cur
}

.mojor_ir_reduction_rhs_array_var <- function(rhs, loop_var) {
  expr <- .mojor_ir_strip_casts(rhs)
  if (is.null(expr) || !is.list(expr) || !identical(expr$kind, "index")) {
    return(NULL)
  }
  base <- expr$base
  if (is.null(base) || !is.list(base) || !identical(base$kind, "var") ||
    !is.character(base$name) || length(base$name) != 1) {
    return(NULL)
  }
  indices <- expr$indices
  if (is.null(indices) || length(indices) != 1) {
    return(NULL)
  }
  idx <- indices[[1]]
  if (!is.null(idx) && is.list(idx) && identical(idx$kind, "scalar_index") && !is.null(idx$expr)) {
    idx <- idx$expr
  }
  idx <- .mojor_ir_strip_casts(idx)
  if (is.null(idx) || !is.list(idx) || !identical(idx$kind, "var") ||
    !identical(idx$name, loop_var)) {
    return(NULL)
  }
  base$name
}

# =============================================================================
# Section 5: Expression IR (AST -> IR -> Mojo)
# =============================================================================
