# IR Build Helpers

# Helper: Detect if an index expression is a negative literal
# Returns list(is_negative=TRUE, value=abs(literal)) or list(is_negative=FALSE)
.mojor_ir_detect_negative_index <- function(expr) {
  expr <- .mojor_ir_strip_index_wrappers(expr)
  # Direct negative numeric literal
  if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr < 0) {
    return(list(is_negative = TRUE, value = as.integer(abs(expr)), is_vector = FALSE,
                is_dynamic = FALSE))
  }
  # Negative unary expression: -k where k is a positive literal, or -var (dynamic)
  if (is.call(expr) && length(expr) == 2 && as.character(expr[[1]]) == "-") {
    inner <- .mojor_ir_strip_index_wrappers(expr[[2]])
    if (is.numeric(inner) && length(inner) == 1 && !is.na(inner) && inner > 0) {
      return(list(is_negative = TRUE, value = as.integer(inner), is_vector = FALSE,
                  is_dynamic = FALSE))
    }
    # Dynamic variable: -idx where idx is a name
    if (is.name(inner)) {
      return(list(is_negative = TRUE, value = inner, is_vector = FALSE,
                  is_dynamic = TRUE))
    }
    # Vector case: -c(1,2) or -(1:2)
    if (is.call(inner)) {
      # Strip wrapper forms inside unary negative: -(1:3), -(+(1:3))
      unwrapped <- inner
      unwrapped <- .mojor_ir_strip_index_wrappers(unwrapped)
      if (is.call(unwrapped)) {
        inner_op <- as.character(unwrapped[[1]])
        if (inner_op %in% c("c", ":", "seq", "seq.int", "seq_len")) {
          return(list(is_negative = TRUE, value = unwrapped, is_vector = TRUE,
                      is_dynamic = FALSE))
        }
        if (inner_op %in% c("integer", "numeric", "logical") &&
            length(unwrapped) == 2) {
          arg <- unwrapped[[2]]
          if ((is.numeric(arg) || is.integer(arg)) &&
              length(arg) == 1 &&
              !is.na(arg) &&
              as.integer(arg) == 0L) {
            return(list(
              is_negative = TRUE,
              value = call("c"),
              is_vector = TRUE,
              is_dynamic = FALSE
            ))
          }
        }
      }
      # Dynamic scalar exclusion expression, e.g. -length(x), -(i + 1)
      return(list(is_negative = TRUE, value = unwrapped, is_vector = FALSE,
                  is_dynamic = TRUE))
    }
  }
  list(is_negative = FALSE)
}

# Helper: Build IR exclusion index marker for R-style negative indexing
# R-style: x[-k] means exclude element k, returning a shorter vector
# This creates an index node with a neg_exclusion field for the emission layer
.mojor_ir_build_exclusion_index <- function(neg_info) {
  if (isTRUE(neg_info$is_dynamic)) {
    # Dynamic: -idx / -expr → exclude element at position Int(expr) - 1 (0-based)
    if (is.name(neg_info$value)) {
      excl_expr <- paste0("Int(", as.character(neg_info$value), ") - 1")
      return(list(kind = "var", name = "__neg_excl__", neg_exclusion = excl_expr))
    }
    return(list(
      kind = "var",
      name = "__neg_excl__",
      neg_exclusion = NULL,
      neg_exclusion_expr_ast = neg_info$value
    ))
  } else {
    # Literal: -k → exclude element at position k-1 (0-based)
    excl_expr <- as.character(as.integer(neg_info$value) - 1L)
    return(list(kind = "var", name = "__neg_excl__", neg_exclusion = excl_expr))
  }
}

# Build IR vector exclusion index marker for -c(a,b,...) or -(a:b)
.mojor_ir_build_vector_exclusion_index <- function(neg_info) {
  val <- .mojor_ir_strip_index_wrappers(neg_info$value)
  op <- as.character(val[[1]])
  if (op == "c") {
    # -c(a, b, c, ...) — exclusion set (literal or dynamic elements)
    elems <- as.list(val)[-1]
    has_dynamic <- FALSE
    excl_indices <- vapply(elems, function(e) {
      e <- .mojor_ir_strip_index_wrappers(e)
      if (is.numeric(e)) {
        as.character(as.integer(e) - 1L)
      } else if (is.name(e)) {
        has_dynamic <<- TRUE
        paste0("Int(", as.character(e), ") - 1")
      } else {
        has_dynamic <<- TRUE
        NULL
      }
    }, character(1))
    if (any(vapply(excl_indices, is.null, logical(1)))) return(NULL)
    excl_indices <- unique(excl_indices)
    return(list(kind = "var", name = "__neg_vec_excl__",
                neg_vec_exclusion = list(type = "c", indices = excl_indices,
                                         count = length(excl_indices),
                                         is_dynamic = has_dynamic)))
  }
  if (op == ":") {
    # -(a:b) — range exclusion
    a <- .mojor_ir_strip_index_wrappers(val[[2]])
    b <- .mojor_ir_strip_index_wrappers(val[[3]])
    if (is.numeric(a) && is.numeric(b)) {
      a_val <- as.integer(a)
      b_val <- as.integer(b)
      return(list(kind = "var", name = "__neg_vec_excl__",
                  neg_vec_exclusion = list(type = "range",
                                           start_0 = as.character(a_val - 1L),
                                           end_0 = as.character(b_val),
                                           count = as.character(b_val - a_val + 1L))))
    }
    # Dynamic range: preserve AST bounds for emit-time expression lowering.
    return(list(
      kind = "var",
      name = "__neg_vec_excl__",
      neg_vec_exclusion = list(
        type = "range",
        start_expr_ast = a,
        end_expr_ast = b
      )
    ))
  }
  if (op %in% c("seq", "seq.int", "seq_len")) {
    # -seq_len(k) is -(1:k)
    if (op == "seq_len" && length(val) == 2) {
      k <- .mojor_ir_strip_index_wrappers(val[[2]])
      k_str <- if (is.name(k)) paste0("Int(", as.character(k), ")") else as.character(as.integer(k))
      return(list(kind = "var", name = "__neg_vec_excl__",
                  neg_vec_exclusion = list(type = "range",
                                           start_0 = "0",
                                           end_0 = k_str,
                                           count = k_str)))
    }
    if (op %in% c("seq", "seq.int")) {
      seq_args <- as.list(val)[-1]
      seq_arg_names <- names(seq_args)
      pick_arg <- function(pos, nm = NULL) {
        if (!is.null(seq_arg_names) && !is.null(nm) && nm %in% seq_arg_names) {
          return(seq_args[[which(seq_arg_names == nm)[1]]])
        }
        if (length(seq_args) >= pos) {
          return(seq_args[[pos]])
        }
        NULL
      }
      from <- pick_arg(1, "from")
      to <- pick_arg(2, "to")
      by <- pick_arg(3, "by")
      if (length(seq_args) == 1 && is.null(to)) {
        to <- from
        from <- 1L
      }
      if (is.null(from) || is.null(to)) {
        return(NULL)
      }
      from <- .mojor_ir_strip_index_wrappers(from)
      to <- .mojor_ir_strip_index_wrappers(to)
      if (!is.null(by)) {
        by <- .mojor_ir_strip_index_wrappers(by)
        if (!(is.numeric(by) && length(by) == 1 && !is.na(by) && as.integer(by) == 1L)) {
          return(NULL)
        }
      }
      if (is.numeric(from) && is.numeric(to)) {
        from_i <- as.integer(from)
        to_i <- as.integer(to)
        if (is.na(from_i) || is.na(to_i) || from_i > to_i) {
          return(NULL)
        }
        return(list(kind = "var", name = "__neg_vec_excl__",
                    neg_vec_exclusion = list(type = "range",
                                             start_0 = as.character(from_i - 1L),
                                             end_0 = as.character(to_i),
                                             count = as.character(to_i - from_i + 1L))))
      }
      return(list(
        kind = "var",
        name = "__neg_vec_excl__",
        neg_vec_exclusion = list(
          type = "range",
          start_expr_ast = from,
          end_expr_ast = to
        )
      ))
    }
  }
  NULL
}

# Build IR positive vector selection marker for c(1,3) or 1:3 as index
.mojor_ir_build_positive_vector_selection <- function(ix) {
  ix <- .mojor_ir_strip_index_wrappers(ix)
  ix_op <- as.character(ix[[1]])
  if (ix_op == "c") {
    elems <- as.list(ix)[-1]
    if (length(elems) == 0L) return(NULL)
    pos_indices <- character(0)
    has_dynamic <- FALSE
    for (e in elems) {
      e <- .mojor_ir_strip_index_wrappers(e)
      if (is.numeric(e) && length(e) == 1 && !is.na(e) && e > 0) {
        pos_indices <- c(pos_indices, as.character(as.integer(e) - 1L))
      } else if (is.numeric(e) && length(e) == 1 && !is.na(e) && e == 0) {
        # R semantics: 0 in positive index vectors selects nothing.
        next
      } else if (is.name(e)) {
        pos_indices <- c(pos_indices, paste0("(Int(", as.character(e), ") - 1)"))
        has_dynamic <- TRUE
      } else {
        return(NULL)
      }
    }
    expr_ir <- .mojor_ir_expr_build(ix)
    return(.mojor_ir_vec_index(
      expr_ir,
      src = ix,
      selection = list(
        type = "c",
        indices_0 = pos_indices,
        count = length(pos_indices),
        is_dynamic = has_dynamic
      )
    ))
  }
  # Keep ":" range selectors on canonical slice detection paths.
  # Emitting pos_vec_selection markers for ranges can bypass slice lowering and
  # leak unresolved selector symbols into backend codegen.
  NULL
}

.mojor_ir_is_index_control_arg <- function(arg_name, is_double_bracket = FALSE) {
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

.mojor_ir_collect_index_positions <- function(expr, is_double_bracket = FALSE) {
  n_elem <- length(expr)
  if (n_elem < 3L) {
    return(integer(0))
  }
  idx_positions <- 3:n_elem
  expr_names <- names(expr)
  if (is.null(expr_names)) {
    return(idx_positions)
  }
  idx_names <- expr_names[idx_positions]
  keep <- vapply(
    idx_names,
    function(name) !.mojor_ir_is_index_control_arg(name, is_double_bracket = is_double_bracket),
    logical(1)
  )
  idx_positions[keep]
}

.mojor_ir_build_index <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!(op %in% c("[", "[["))) {
    return(NULL)
  }
  is_double_bracket <- identical(op, "[[")
  base <- expr[[2]]

 # Extract variable name early for character index resolution
  var <- NULL
  if (is.name(base)) {
    var <- as.character(base)
  }

 # Keep named index arguments (e.g., rows=...) but skip control arguments.
  idx_positions <- .mojor_ir_collect_index_positions(expr, is_double_bracket = is_double_bracket)
  if (length(idx_positions) == 0) {
    return(NULL)
  }
  if (is_double_bracket && length(idx_positions) != 1) {
    return(NULL)
  }

  selector_extent_hint <- if (!is_double_bracket && length(idx_positions) == 1L) {
    .mojor_ir_selector_infer_target_extent_ast(base, axis = 1L)
  } else {
    NULL
  }

  indices <- list()
  selector_ir_is_raw <- function(node) {
    is.list(node) && !is.null(node$kind) && identical(node$kind, "raw")
  }
  for (k in seq_along(idx_positions)) {
    pos <- idx_positions[[k]]
    is_missing <- tryCatch(
      {
        val <- expr[[pos]]
        is.symbol(val) && identical(as.character(val), "")
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)) {
          TRUE
        } else {
          stop(e)
        }
      }
    )
    if (is_missing) {
      if (is_double_bracket) {
        return(NULL)
      }
      indices[[k]] <- .mojor_ir_missing_index(src = NULL)
      next
    }
    ix <- expr[[pos]]
    if (is.character(ix) && length(ix) == 1) {
      ix <- .mojor_ir_resolve_dimname_index(var, ix, k)
    }
    ix_src <- ix
    ix <- .mojor_ir_normalize_selector_ast(ix, mode = "selector")
    .mojor_ir_validate_selector_ast(
      ix,
      ix_src,
      context = "index",
      target_extent = if (isTRUE(k == 1L)) selector_extent_hint else NULL
    )
    # Check for negative index — R-style exclusion
    # Build exclusion marker; emission layer decides whether context is valid
    neg_info <- .mojor_ir_detect_negative_index(ix)
    if (neg_info$is_negative && is.name(base) && !isTRUE(neg_info$is_vector)) {
      excl_node <- .mojor_ir_build_exclusion_index(neg_info)
      indices[[k]] <- excl_node
      next
    }
    if (neg_info$is_negative && isTRUE(neg_info$is_vector) && is.name(base)) {
      vec_excl_node <- .mojor_ir_build_vector_exclusion_index(neg_info)
      if (!is.null(vec_excl_node)) {
        indices[[k]] <- vec_excl_node
        next
      }
    }
    # Check for positive vector selection: c(1,3) or 1:3 as index
    if (is.call(ix) && !neg_info$is_negative && is.name(base)) {
      pos_sel_node <- .mojor_ir_build_positive_vector_selection(ix)
      if (!is.null(pos_sel_node)) {
        indices[[k]] <- pos_sel_node
        next
      }
    }
    slice <- .mojor_ir_detect_slice(ix)
    if (!is_double_bracket && !is.null(slice)) {
      start_ir <- .mojor_ir_expr_build(slice$start)
      end_ir <- .mojor_ir_expr_build(slice$end)
      if (!is.null(start_ir) && !is.null(end_ir)) {
        indices[[k]] <- list(
          kind = "slice_index",
          start = start_ir,
          end = end_ir,
          end_expr_ast = slice$end,
          src = ix_src
        )
        next
      }
    }
    built <- .mojor_ir_expr_build(ix)
    if (is.null(built) || isTRUE(selector_ir_is_raw(built))) {
      .mojor_err(
        "unsupported selector expression in index",
        ix_src,
        "use covered scalar/slice/vector selector forms"
      )
    }
    indices[[k]] <- .mojor_ir_scalar_index(built, src = ix_src)
  }
  index_base <- .mojor_ir_effective_index_base()

 # PR-B3: Support constructor bases (seq, c, rep, rep_len)
  if (is.name(base)) {
    var <- as.character(base)
    return(.mojor_ir_index(.mojor_ir_var(var), indices, index_base = index_base, src = expr))
  } else if (is.call(base)) {
    base_op <- as.character(base[[1]])
    if (base_op %in% c("[", "[[")) {
      base_ir <- .mojor_ir_build_index(base)
      if (is.null(base_ir)) {
        return(NULL)
      }
      can_compose_slice_scalar <- is.list(base_ir) &&
        identical(base_ir$kind, "index") &&
        is.list(base_ir$indices) &&
        length(base_ir$indices) == 1L &&
        is.list(base_ir$indices[[1]]) &&
        identical(base_ir$indices[[1]]$kind, "slice_index") &&
        length(indices) == 1L &&
        is.list(indices[[1]])
      if (isTRUE(can_compose_slice_scalar)) {
        slice_idx <- base_ir$indices[[1]]
        outer_idx <- .mojor_ir_selector_unwrap_node(indices[[1]])
        if (is.null(outer_idx) || !is.list(outer_idx) || !identical(outer_idx$kind, "var")) {
          can_compose_slice_scalar <- FALSE
        }
      }
      if (isTRUE(can_compose_slice_scalar)) {
        slice_idx <- base_ir$indices[[1]]
        outer_idx <- .mojor_ir_selector_unwrap_node(indices[[1]])
        start_node <- slice_idx$start
        start_is_one <- is.list(start_node) &&
          identical(start_node$kind, "const") &&
          suppressWarnings(as.numeric(start_node$value)) == 1
        composed_idx <- if (isTRUE(start_is_one)) {
          outer_idx
        } else {
          .mojor_ir_binop(
            "+",
            .mojor_ir_binop("-", outer_idx, .mojor_ir_const("1")),
            start_node
          )
        }
        return(.mojor_ir_index(base_ir$base, list(composed_idx), index_base = index_base, src = expr))
      }
      return(.mojor_ir_index(base_ir, indices, index_base = index_base, src = expr))
    }
    if (base_op == "dim") {
      base_ir <- .mojor_ir_expr_build(base)
      if (!is.null(base_ir)) {
        return(.mojor_ir_index(base_ir, indices, index_base = index_base, src = expr))
      }
    }
 # PR-B5 Step 4: Extended constructor bases including cumulative operations
    if (base_op %in% c(
      "c", "rep", "rep_len", "rep.int", "seq", "seq.int", "t", "cbind", "rbind", "diag",
      "cumsum", "cumprod", "cummax", "cummin"
    )) {
 # Build constructor IR as the base
      base_ir <- .mojor_ir_expr_build(base)
      if (is.null(base_ir)) {
        return(NULL)
      }
      return(.mojor_ir_index(base_ir, indices, index_base = index_base, src = expr))
    }
 # Allow indexing over sampling expressions, e.g. sample(...)[1].
 # Emission enforces strict scalar subset where supported.
    if (base_op %in% c("sample.int", "sample")) {
      base_ir <- .mojor_ir_expr_build(base)
      if (is.null(base_ir)) {
        return(NULL)
      }
      return(.mojor_ir_index(base_ir, indices, index_base = index_base, src = expr))
    }
  }

  return(NULL)
}

# Step 8.5: Build subscript for LHS of assignments (supports slices)
.mojor_ir_build_subscript <- function(expr) {
 # Builds subscript LHS for assignments like out[1:n, j] <- rhs
 # Handles: slice indices (1:n), scalar indices (j), missing indices (mat[i, ])
  if (!is.call(expr)) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!(op %in% c("[", "[["))) {
    return(NULL)
  }
  is_double_bracket <- identical(op, "[[")
  base <- expr[[2]]
  if (!is.name(base)) {
    return(NULL)
  }
  var <- as.character(base)

 # Keep named index arguments (e.g., rows=...) but skip control arguments.
  idx_positions <- .mojor_ir_collect_index_positions(expr, is_double_bracket = is_double_bracket)

  n_idx <- length(idx_positions)
  if (n_idx == 0) {
    return(NULL)
  }
  if (is_double_bracket && n_idx != 1) {
    return(NULL)
  }

  indices <- list()
  selector_ir_is_raw <- function(node) {
    is.list(node) && !is.null(node$kind) && identical(node$kind, "raw")
  }
  for (k in seq_along(idx_positions)) {
    pos <- idx_positions[k]

 # Check if index is missing (mat[i, ]).
 # In R call objects, a missing argument in `[` is represented as an empty
 # symbol. Detect it by inspecting the call's pairlist directly.
 # Only treat genuine "missing argument" access errors as missing;
 # other errors re-raise to avoid masking real bugs.
    is_missing <- tryCatch(
      {
        val <- expr[[pos]]
 # Empty symbol: nchar("") == 0 and is.symbol(quote(expr=)) == TRUE
        is.symbol(val) && identical(as.character(val), "")
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)) {
          TRUE
        } else {
          stop(e)
        }
      }
    )

    if (is_missing) {
      if (is_double_bracket) {
        return(NULL)
      }
      indices[[k]] <- .mojor_ir_missing_index(src = NULL)
      next
    }

 # Safe to assign now - we know it's not missing
    idx_expr <- expr[[pos]]
    idx_expr <- .mojor_ir_try_resolve_dimname_index_expr(var, idx_expr, k)
    idx_expr_src <- idx_expr
    idx_expr <- .mojor_ir_normalize_selector_ast(idx_expr, mode = "selector")
    .mojor_ir_validate_selector_ast(
      idx_expr,
      idx_expr_src,
      context = "assignment",
      target_extent = NULL
    )

    # R-style negative index on write-side: out[-k] <- val
    # Build exclusion marker; emission layer emits skip-write loop
    neg_info <- .mojor_ir_detect_negative_index(idx_expr)
    if (neg_info$is_negative && !isTRUE(neg_info$is_vector)) {
      excl_node <- .mojor_ir_build_exclusion_index(neg_info)
      indices[[k]] <- excl_node
      next
    }
    if (neg_info$is_negative && isTRUE(neg_info$is_vector)) {
      vec_excl_node <- .mojor_ir_build_vector_exclusion_index(neg_info)
      if (!is.null(vec_excl_node)) {
        indices[[k]] <- vec_excl_node
        next
      }
      .mojor_err(
        "unsupported vector negative indexing form",
        idx_expr_src,
        "use covered -c(...) / -(a:b) / -seq.int(...) exclusion forms"
      )
    }
    if (is.call(idx_expr) && !neg_info$is_negative) {
      pos_sel_node <- .mojor_ir_build_positive_vector_selection(idx_expr)
      if (!is.null(pos_sel_node)) {
        if (is_double_bracket) {
          .mojor_err(
            "vector selector is not supported for [[ assignment",
            idx_expr_src,
            "use scalar selector for [["
          )
        }
        indices[[k]] <- pos_sel_node
        next
      }
    }
 # Try to detect slice pattern
    slice <- .mojor_ir_detect_slice(idx_expr)
    if (!is.null(slice)) {
      if (is_double_bracket) {
        return(NULL)
      }
 # Build IR for start and end
      start_ir <- .mojor_ir_expr_build(slice$start)
      end_ir <- .mojor_ir_expr_build(slice$end)
      if (is.null(start_ir) || is.null(end_ir)) {
 # Fallback: treat as scalar if can't build slice
        idx_ir <- .mojor_ir_expr_build(idx_expr)
        if (is.null(idx_ir) || isTRUE(selector_ir_is_raw(idx_ir))) {
          .mojor_err(
            "unsupported selector expression in assignment",
            idx_expr_src,
            "use covered scalar/slice/vector selector forms"
          )
        }
        indices[[k]] <- .mojor_ir_scalar_index(idx_ir, src = idx_expr_src)
      } else {
 # Preserve end AST for recycle warnings in IR emitter
        indices[[k]] <- list(
          kind = "slice_index",
          start = start_ir,
          end = end_ir,
          end_expr_ast = slice$end,
          src = idx_expr_src
        )
      }
      next
    }

 # Scalar index (default)
    idx_ir <- .mojor_ir_expr_build(idx_expr)
    if (is.null(idx_ir) || isTRUE(selector_ir_is_raw(idx_ir))) {
      .mojor_err(
        "unsupported selector expression in assignment",
        idx_expr_src,
        "use covered scalar/slice/vector selector forms"
      )
    }
    indices[[k]] <- .mojor_ir_scalar_index(idx_ir, src = idx_expr_src)
  }

 # Step 8.6: Backward compatibility for simple scalar indexing
 # If there's only 1 index and it's scalar, return an "index" node instead of "subscript"
 # This maintains compatibility with pre-step-8.5 assignment emission code
  if (n_idx == 1 && indices[[1]]$kind == "scalar_index") {
    base_ir <- .mojor_ir_var(var)
    index_base <- .mojor_ir_effective_index_base()
    return(.mojor_ir_index(base_ir, list(indices[[1]]$expr), index_base = index_base, src = expr))
  }

  .mojor_ir_subscript(var, indices, src = expr)
}

.mojor_ir_collect_var_refs_build <- function(node, refs = character(0)) {
  if (is.null(node)) {
    return(refs)
  }

  if (is.symbol(node)) {
    nm <- as.character(node)
    if (is.character(nm) && nzchar(nm)) {
      refs <- c(refs, nm)
    }
    return(unique(refs))
  }

  if (is.language(node)) {
    syms <- all.names(node, functions = FALSE, unique = TRUE)
    if (length(syms) > 0) {
      syms <- setdiff(syms, c(
        "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf",
        "NA_real_", "NA_integer_", "NA_character_", "NA_complex_"
      ))
      refs <- c(refs, syms)
    }
    return(unique(refs))
  }

  if (is.list(node)) {
    if (!is.null(node$kind) && identical(node$kind, "var") &&
      !is.null(node$name) && is.character(node$name) && nzchar(node$name)) {
      refs <- c(refs, node$name)
    }
    nms <- names(node)
    if (is.null(nms)) nms <- rep.int("", length(node))
    for (i in seq_along(node)) {
      nm <- nms[[i]]
      if (identical(nm, "") || !(nm %in% c("kind", "name", "value", "op", "fn", "src", "metadata"))) {
        refs <- .mojor_ir_collect_var_refs_build(node[[i]], refs)
      }
    }
    return(unique(refs))
  }

  refs
}

.mojor_ir_build_stmt <- function(stmt) {
  stmt <- .mojor_ir_canonicalize_loop_index_ast(stmt)
  if (is.call(stmt)) {
    op <- as.character(stmt[[1]])
    if (op %in% c("<-", "=") && length(stmt) >= 3) {
      lhs <- stmt[[2]]
      rhs <- stmt[[3]]
      lhs_ir <- NULL
      if (is.name(lhs)) {
        lhs_ir <- .mojor_ir_var(as.character(lhs))
      } else if (is.call(lhs) && as.character(lhs[[1]]) %in% c("[", "[[")) {
 # Step 8.5: Try subscript (with slice support) first, fall back to simple index
        lhs_ir <- .mojor_ir_build_subscript(lhs)
        if (is.null(lhs_ir)) lhs_ir <- .mojor_ir_build_index(lhs)
      }
      if (is.null(lhs_ir)) lhs_ir <- .mojor_ir_raw(lhs)
      rhs_ir <- .mojor_ir_expr_build(rhs)
      if (is.null(rhs_ir)) rhs_ir <- .mojor_ir_raw(rhs)
      return(.mojor_ir_assign(lhs_ir, rhs_ir))
    }
    if (op == "if" && length(stmt) >= 3) {
      cond <- stmt[[2]]
      then_block <- stmt[[3]]
      else_block <- if (length(stmt) >= 4) stmt[[4]] else NULL
      cond_ir <- .mojor_ir_expr_build(cond)
      if (is.null(cond_ir)) cond_ir <- .mojor_ir_raw(cond)
      then_ir <- .mojor_ir_build_block(then_block)
      else_ir <- if (!is.null(else_block)) .mojor_ir_build_block(else_block) else NULL
      return(.mojor_ir_if(cond_ir, then_ir, else_ir))
    }
    if (op == "for" && length(stmt) >= 4) {
      var <- as.character(stmt[[2]])
      range_expr <- stmt[[3]]
      body <- stmt[[4]]
 # Try to canonicalize common range patterns into structured range nodes;
 # fall back to range_expr (raw AST) for complex cases.
      range_ir <- .mojor_ir_try_build_range(range_expr)
      if (is.null(range_ir)) range_ir <- .mojor_ir_range_expr(range_expr)
      body_ir <- .mojor_ir_build_block(body)
      loop_node <- .mojor_ir_loop(var, range_ir, body_ir)

 # Detect inline reduction patterns at build time (explicit, verifiable)
      red <- .mojor_ir_detect_reduction_pattern(loop_node)
      if (!is.null(red)) loop_node$reduce <- red

 # Step broadcast_nd: Detect broadcast_nd mode and add metadata
      broadcast_nd <- isTRUE(.mojor_state$current_broadcast_nd)
      if (broadcast_nd) {
        if (is.null(loop_node$metadata)) loop_node$metadata <- list()
        loop_node$metadata$broadcast_mode <- "broadcast_nd"
        broadcast_vars <- unique(setdiff(.mojor_ir_collect_var_refs_build(body_ir), var))
        loop_node$metadata$broadcast_vars <- broadcast_vars
      }

      return(loop_node)
    }
 # Step 8.2: While loop support
    if (op == "while" && length(stmt) >= 3) {
      cond <- stmt[[2]]
      body <- stmt[[3]]
      cond_ir <- .mojor_ir_expr_build(cond)
      if (is.null(cond_ir)) {
        return(NULL)
      }
      body_ir <- .mojor_ir_build_block(body)
      if (is.null(body_ir)) {
        return(NULL)
      }
      return(.mojor_ir_while(cond_ir, body_ir, src = stmt))
    }
 # Step 8.7: Repeat loop support
    if (op == "repeat" && length(stmt) >= 2) {
      body <- stmt[[2]]
      body_ir <- .mojor_ir_build_block(body)
      if (is.null(body_ir)) {
        return(NULL)
      }
      return(.mojor_ir_repeat(body_ir, src = stmt))
    }
 # Step 8.2: Break statement support
    if (op == "break" && length(stmt) == 1) {
      return(.mojor_ir_break(src = stmt))
    }
 # Step 8.2: Next statement support
    if (op == "next" && length(stmt) == 1) {
      return(.mojor_ir_next(src = stmt))
    }
 # Step 8.8: Return statement support
    if (op == "return") {
      if (length(stmt) < 2) {
        stop("mojor_transpile: return() must include a value")
      }
      value_ir <- .mojor_ir_expr_build(stmt[[2]])
      if (is.null(value_ir)) value_ir <- .mojor_ir_raw(stmt[[2]])
      return(.mojor_ir_return(value_ir, src = stmt))
    }
  }
 # Step 8.2: Handle break/next as bare names (not calls)
  if (is.name(stmt)) {
    name_str <- as.character(stmt)
    if (name_str == "break") {
      return(.mojor_ir_break(src = stmt))
    }
    if (name_str == "next") {
      return(.mojor_ir_next(src = stmt))
    }
  }
  .mojor_ir_raw(stmt)
}

.mojor_ir_build_block <- function(block) {
  stmts <- .mojor_extract_block(block)
  ir_stmts <- lapply(stmts, .mojor_ir_build_stmt)
  .mojor_ir_block(ir_stmts)
}
