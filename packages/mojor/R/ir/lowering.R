.mojor_ir_lower_subscript <- function(node, ctx) {
 # Lower subscript with slices to explicit loop structure
 # Handles: 1D/2D/3D+ slices, missing indices, mask extraction RHS

  if (is.null(node) || node$kind != "assign") {
    return(node)
  }
  if (is.null(node$lhs)) {
    return(node)
  }
  if (!identical(node$lhs$kind, "subscript")) {
    if (identical(node$lhs$kind, "index") &&
        !is.null(node$lhs$base) &&
        is.list(node$lhs$base) &&
        identical(node$lhs$base$kind, "var") &&
        !is.null(node$lhs$indices) &&
        is.list(node$lhs$indices) &&
        # Avoid re-lowering internally generated zero-based writes.
        !(identical(node$lhs$index_base, "zero_based") &&
          is.null(node$lhs$src))) {
      node$lhs <- .mojor_ir_subscript(
        node$lhs$base$name,
        node$lhs$indices,
        src = node$lhs$src
      )
    } else {
      return(node)
    }
  }

  lhs <- node$lhs
  indices <- lhs$indices
  n_idx <- length(indices)
  index_base <- if (!is.null(ctx$index_base)) ctx$index_base else "one_based"
  walk_and_validate_selector_nodes <- function(expr_node) {
    if (is.null(expr_node) || !is.list(expr_node) || is.null(expr_node$kind)) {
      return(invisible(NULL))
    }
    kind <- expr_node$kind
    if (kind %in% c("index", "subscript") &&
      is.list(expr_node$indices) &&
      length(expr_node$indices) > 0L) {
      for (idx in expr_node$indices) {
        .mojor_ir_validate_selector_node(idx, context = "index")
      }
    }
    if (identical(kind, "assign")) {
      walk_and_validate_selector_nodes(expr_node$lhs)
      walk_and_validate_selector_nodes(expr_node$rhs)
      return(invisible(NULL))
    }
    if (identical(kind, "index")) {
      walk_and_validate_selector_nodes(expr_node$base)
      return(invisible(NULL))
    }
    if (identical(kind, "subscript")) {
      return(invisible(NULL))
    }
    if (identical(kind, "binop")) {
      walk_and_validate_selector_nodes(expr_node$lhs)
      walk_and_validate_selector_nodes(expr_node$rhs)
      return(invisible(NULL))
    }
    if (kind %in% c("unop", "cast") && !is.null(expr_node$expr)) {
      walk_and_validate_selector_nodes(expr_node$expr)
      return(invisible(NULL))
    }
    if (identical(kind, "ifelse")) {
      walk_and_validate_selector_nodes(expr_node$cond)
      walk_and_validate_selector_nodes(expr_node$yes)
      walk_and_validate_selector_nodes(expr_node$no)
      return(invisible(NULL))
    }
    if (identical(kind, "call") && is.list(expr_node$args)) {
      for (arg in expr_node$args) {
        walk_and_validate_selector_nodes(arg)
      }
      return(invisible(NULL))
    }
    if (identical(kind, "c") && is.list(expr_node$parts)) {
      for (part in expr_node$parts) {
        walk_and_validate_selector_nodes(part)
      }
      return(invisible(NULL))
    }
    invisible(NULL)
  }

  for (idx in indices) {
    .mojor_ir_validate_selector_node(idx, context = "assignment")
  }
  walk_and_validate_selector_nodes(node$rhs)

  has_unresolved_raw_selector <- any(vapply(indices, function(idx) {
    if (is.null(idx) || !is.list(idx)) {
      return(FALSE)
    }
    if (!is.null(idx$kind) && identical(idx$kind, "raw")) {
      return(TRUE)
    }
    if (!is.null(idx$kind) &&
      identical(idx$kind, "scalar_index") &&
      is.list(idx$expr) &&
      !is.null(idx$expr$kind) &&
      identical(idx$expr$kind, "raw")) {
      return(TRUE)
    }
    FALSE
  }, logical(1)))
  if (isTRUE(has_unresolved_raw_selector)) {
    stop(
      "mojor_transpile: IR selector lowering failed: unresolved raw selector index after canonicalization"
    )
  }

 # Check for N-d special subscript patterns — pass through only negative
  # exclusion markers. Positive selectors must proceed through regular
  # slice/vector lowering paths.
  is_nd_negative_exclusion <- function(idx) {
    !is.null(idx$neg_exclusion) || !is.null(idx$neg_vec_exclusion)
  }
  has_nd_special <- any(vapply(indices, function(idx) {
    is_nd_negative_exclusion(idx)
  }, logical(1)))
  all_nd_compatible <- all(vapply(indices, function(idx) {
    is_nd_negative_exclusion(idx) ||
      identical(idx$kind, "missing_index") ||
      idx$kind %in% c("const", "var", "scalar_index")
  }, logical(1)))
  rhs_scalar_only <- TRUE
  if (!is.null(ctx$type_env) && !is.null(node$rhs)) {
    rhs_type <- tryCatch(.mojor_ir_infer_type(node$rhs, ctx$type_env), error = function(e) NULL)
    if (!is.null(rhs_type) && .mojor_type_is_array(rhs_type)) {
      rhs_scalar_only <- FALSE
    }
  }
  if (has_nd_special && all_nd_compatible && n_idx >= 2L && isTRUE(rhs_scalar_only)) {
    return(node)
  }

 # Check if any index is a slice or missing
  has_slice <- FALSE
  for (idx in indices) {
    if (idx$kind %in% c("slice_index", "missing_index")) {
      has_slice <- TRUE
      break
    }
  }
  resolve_len_expr <- function(var_name,
                               fallback_node = NULL,
                               fallback_label = NULL,
                               allow_symbol_fallback = TRUE) {
    len_value <- NULL
    if (!is.null(ctx$len_var_map) &&
      !is.null(names(ctx$len_var_map)) &&
      var_name %in% names(ctx$len_var_map)) {
      len_value <- ctx$len_var_map[[var_name]]
    } else if (!is.null(.mojor_state$current_len_var_map) &&
      !is.null(names(.mojor_state$current_len_var_map)) &&
      var_name %in% names(.mojor_state$current_len_var_map)) {
      len_value <- .mojor_state$current_len_var_map[[var_name]]
    } else if (!is.null(ctx$n_source_name) && identical(var_name, ctx$n_source_name)) {
      len_value <- "n_i"
    }

    if (is.null(len_value)) {
      if (!is.null(fallback_node)) {
        label <- if (!is.null(fallback_label)) fallback_label else "n_i"
        return(list(
          node = fallback_node,
          label = label
        ))
      }
      if (!allow_symbol_fallback) {
        return(list(
          node = .mojor_ir_var("n_i"),
          label = "n_i"
        ))
      }
      fallback_len <- paste0("n_", var_name, "_i")
      return(list(
        node = .mojor_ir_var(fallback_len),
        label = fallback_len
      ))
    }
    if (is.list(len_value) && !is.null(len_value$kind)) {
      return(list(node = len_value, label = "n_i"))
    }
    if ((is.numeric(len_value) || is.integer(len_value)) &&
      length(len_value) == 1 && !is.na(len_value)) {
      val <- as.character(as.integer(len_value))
      return(list(node = .mojor_ir_const(val), label = val))
    }
    if (is.character(len_value) && length(len_value) == 1) {
      if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", len_value)) {
        return(list(node = .mojor_ir_var(len_value), label = len_value))
      }
      expr_ast <- tryCatch(parse(text = len_value)[[1]], error = function(e) NULL)
      if (!is.null(expr_ast)) {
        expr_node <- tryCatch(.mojor_ir_expr_build(expr_ast), error = function(e) NULL)
        if (!is.null(expr_node)) {
          return(list(node = expr_node, label = len_value))
        }
      }
    }
    if (!is.null(fallback_node)) {
      label <- if (!is.null(fallback_label)) fallback_label else "n_i"
      return(list(
        node = fallback_node,
        label = label
      ))
    }
    if (!allow_symbol_fallback) {
      return(list(
        node = .mojor_ir_var("n_i"),
        label = "n_i"
      ))
    }
    fallback_len <- paste0("n_", var_name, "_i")
    list(
      node = .mojor_ir_var(fallback_len),
      label = fallback_len
    )
  }

  build_literal_i32_read <- function(values, loop_var) {
    n_values <- length(values)
    if (n_values == 0L) {
      return(NULL)
    }
    read_expr <- .mojor_ir_const(as.character(values[[n_values]]))
    if (n_values == 1L) {
      return(read_expr)
    }
    for (i in seq(from = n_values - 1L, to = 1L, by = -1L)) {
      cond <- .mojor_ir_binop(
        "==",
        .mojor_ir_var(loop_var),
        .mojor_ir_const(as.character(i - 1L))
      )
      read_expr <- .mojor_ir_ifelse(
        cond,
        .mojor_ir_const(as.character(values[[i]])),
        read_expr
      )
    }
    read_expr
  }

  parse_len_expr_node <- function(len_expr) {
    if (is.null(len_expr)) {
      return(NULL)
    }
    if (is.list(len_expr) && !is.null(len_expr$kind)) {
      return(list(node = len_expr, label = "n_i"))
    }
    if ((is.numeric(len_expr) || is.integer(len_expr)) &&
      length(len_expr) == 1 && !is.na(len_expr)) {
      val <- as.character(as.integer(len_expr))
      return(list(node = .mojor_ir_const(val), label = val))
    }
    if (is.character(len_expr) && length(len_expr) == 1) {
      if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", len_expr)) {
        return(list(node = .mojor_ir_var(len_expr), label = len_expr))
      }
      expr_ast <- tryCatch(parse(text = len_expr)[[1]], error = function(e) NULL)
      if (!is.null(expr_ast)) {
        expr_node <- tryCatch(.mojor_ir_expr_build(expr_ast), error = function(e) NULL)
        if (!is.null(expr_node)) {
          return(list(node = expr_node, label = len_expr))
        }
      }
    }
    NULL
  }

  resolve_ctor_len_expr <- function(idx_node) {
    if (is.null(idx_node) || is.null(idx_node$kind)) {
      return(NULL)
    }
    if (idx_node$kind %in% c("seq", "rep_len") && !is.null(idx_node$length_out)) {
      return(list(node = idx_node$length_out, label = "length_out"))
    }
    if (idx_node$kind == "rep" && !is.null(idx_node$length_out)) {
      return(list(node = idx_node$length_out, label = "length_out"))
    }
    if (!is.null(idx_node$src)) {
      len_expr <- .mojor_ir_ctor_len_expr(
        idx_node$src,
        ctx$type_env,
        ctx$len_var_map,
        ctx$n_source_name
      )
      return(parse_len_expr_node(len_expr))
    }
    NULL
  }

  quote_mojo_string <- function(value) {
    esc <- gsub("\\\\", "\\\\\\\\", as.character(value))
    esc <- gsub("\"", "\\\\\"", esc)
    paste0("\"", esc, "\"")
  }

  fnv1a32_signed <- .mojor_fnv1a32_signed

  sanitize_name_fragment <- function(value) {
    sanitized <- gsub("[^A-Za-z0-9_]", "_", as.character(value))
    sanitized <- gsub("_+", "_", sanitized)
    sanitized <- gsub("^_+", "", sanitized)
    sanitized <- gsub("_+$", "", sanitized)
    if (!nzchar(sanitized)) {
      sanitized <- "idx"
    }
    if (!grepl("^[A-Za-z_]", sanitized)) {
      sanitized <- paste0("v_", sanitized)
    }
    sanitized
  }

  dimname_axis_names <- function(var_name, idx_pos) {
    dn_map <- .mojor_state$matrix_dimnames
    if (is.null(dn_map) || is.null(dn_map[[var_name]])) {
      return(NULL)
    }
    dn <- dn_map[[var_name]]
    dim_names <- dn$dim_names
    if (is.null(dim_names)) {
      dim_names <- list(dn$row_names, dn$col_names)
    }
    if (idx_pos < 1L || idx_pos > length(dim_names)) {
      return(NULL)
    }
    dim_names[[idx_pos]]
  }

  dimname_lookup_map_name <- function(var_name, idx_pos) {
    paste0(
      "__mojor_dimname_lookup_",
      sanitize_name_fragment(var_name),
      "_",
      as.integer(idx_pos)
    )
  }

  dimname_lookup_hash_map_name <- function(var_name, idx_pos) {
    paste0(
      "__mojor_dimname_hash_lookup_",
      sanitize_name_fragment(var_name),
      "_",
      as.integer(idx_pos)
    )
  }

  build_dimname_lookup_map_stmts <- function(map_name, names_vec) {
    if (is.null(names_vec) || length(names_vec) == 0) {
      return(list())
    }
    stmts <- list(
      .mojor_ir_assign(
        .mojor_ir_var(map_name),
        .mojor_ir_call("Dict[String, Int]", list())
      )
    )
    for (i in seq(from = length(names_vec), to = 1L, by = -1L)) {
      map_lhs <- .mojor_ir_index(
        .mojor_ir_var(map_name),
        list(.mojor_ir_const(quote_mojo_string(names_vec[[i]]))),
        index_base = "zero_based"
      )
      stmts <- c(stmts, list(.mojor_ir_assign(map_lhs, .mojor_ir_const(as.character(i)))))
    }
    stmts
  }

  build_dimname_lookup_hash_map_stmts <- function(map_name, names_vec) {
    if (is.null(names_vec) || length(names_vec) == 0) {
      return(list())
    }
    stmts <- list(
      .mojor_ir_assign(
        .mojor_ir_var(map_name),
        .mojor_ir_call("Dict[Int, Int]", list())
      )
    )
    for (i in seq(from = length(names_vec), to = 1L, by = -1L)) {
      key_hash <- fnv1a32_signed(names_vec[[i]])
      map_lhs <- .mojor_ir_index(
        .mojor_ir_var(map_name),
        list(.mojor_ir_call("Int", list(.mojor_ir_const(as.character(key_hash))))),
        index_base = "zero_based"
      )
      stmts <- c(stmts, list(.mojor_ir_assign(map_lhs, .mojor_ir_const(as.character(i)))))
    }
    stmts
  }

  build_var_chr_dimname_read <- function(var_name, loop_var, map_name) {
    if (is.null(map_name) || !nzchar(map_name)) {
      return(NULL)
    }
    idx_read <- .mojor_ir_index(
      .mojor_ir_var(var_name),
      list(.mojor_ir_var(loop_var)),
      index_base = "zero_based"
    )
    .mojor_ir_call(
      paste0(map_name, ".get"),
      list(idx_read, .mojor_ir_const("1"))
    )
  }

  build_var_chr_hash_dimname_read <- function(var_name, loop_var, len_int, map_name) {
    if (is.null(map_name) || !nzchar(map_name)) {
      return(NULL)
    }
    idx_hash <- .mojor_ir_call(
      "_mojor_read_i32",
      list(
        .mojor_ir_var(var_name),
        .mojor_ir_var(loop_var),
        len_int
      )
    )
    .mojor_ir_call(
      paste0(map_name, ".get"),
      list(.mojor_ir_call("Int", list(idx_hash)), .mojor_ir_const("1"))
    )
  }

  resolve_i32_expr_len <- function(expr_node) {
    if (is.null(expr_node) || is.null(expr_node$kind)) {
      return(NULL)
    }
    kind <- expr_node$kind
    if (identical(kind, "var")) {
      expr_type <- .mojor_ir_infer_type(expr_node, ctx$type_env)
      if (identical(expr_type, "i32[]")) {
        return(resolve_len_expr(expr_node$name))
      }
      return(NULL)
    }
    if (kind %in% c("c", "rep", "rep_len", "seq")) {
      return(resolve_ctor_len_expr(expr_node))
    }
    if (identical(kind, "cast") && !is.null(expr_node$expr)) {
      return(resolve_i32_expr_len(expr_node$expr))
    }
    if (identical(kind, "unop") && !is.null(expr_node$expr)) {
      return(resolve_i32_expr_len(expr_node$expr))
    }
    if (identical(kind, "binop") && !is.null(expr_node$lhs) && !is.null(expr_node$rhs)) {
      lhs_type <- .mojor_ir_infer_type(expr_node$lhs, ctx$type_env)
      rhs_type <- .mojor_ir_infer_type(expr_node$rhs, ctx$type_env)
      lhs_arr <- identical(lhs_type, "i32[]")
      rhs_arr <- identical(rhs_type, "i32[]")
      if (lhs_arr && rhs_arr) {
        lhs_len <- resolve_i32_expr_len(expr_node$lhs)
        rhs_len <- resolve_i32_expr_len(expr_node$rhs)
        if (is.null(lhs_len) || is.null(rhs_len)) {
          return(NULL)
        }
        lhs_label <- as.character(lhs_len$label)
        rhs_label <- as.character(rhs_len$label)
        if (!identical(lhs_label, rhs_label)) {
          return(NULL)
        }
        return(lhs_len)
      }
      if (lhs_arr) {
        return(resolve_i32_expr_len(expr_node$lhs))
      }
      if (rhs_arr) {
        return(resolve_i32_expr_len(expr_node$rhs))
      }
      return(NULL)
    }
    if (identical(kind, "call") &&
        !is.null(expr_node$fn) &&
        expr_node$fn %in% c("min", "max") &&
        is.list(expr_node$args) &&
        length(expr_node$args) == 2) {
      lhs_node <- expr_node$args[[1]]
      rhs_node <- expr_node$args[[2]]
      lhs_type <- .mojor_ir_infer_type(lhs_node, ctx$type_env)
      rhs_type <- .mojor_ir_infer_type(rhs_node, ctx$type_env)
      lhs_arr <- identical(lhs_type, "i32[]")
      rhs_arr <- identical(rhs_type, "i32[]")
      if (lhs_arr && rhs_arr) {
        lhs_len <- resolve_i32_expr_len(lhs_node)
        rhs_len <- resolve_i32_expr_len(rhs_node)
        if (is.null(lhs_len) || is.null(rhs_len)) {
          return(NULL)
        }
        lhs_label <- as.character(lhs_len$label)
        rhs_label <- as.character(rhs_len$label)
        if (!identical(lhs_label, rhs_label)) {
          return(NULL)
        }
        return(lhs_len)
      }
      if (lhs_arr) {
        return(resolve_i32_expr_len(lhs_node))
      }
      if (rhs_arr) {
        return(resolve_i32_expr_len(rhs_node))
      }
      return(NULL)
    }
    if (identical(kind, "ifelse") && !is.null(expr_node$cond) && !is.null(expr_node$yes) && !is.null(expr_node$no)) {
      branch_len <- NULL
      yes_type <- .mojor_ir_infer_type(expr_node$yes, ctx$type_env)
      no_type <- .mojor_ir_infer_type(expr_node$no, ctx$type_env)
      yes_arr <- identical(yes_type, "i32[]")
      no_arr <- identical(no_type, "i32[]")
      if (yes_arr && no_arr) {
        yes_len <- resolve_i32_expr_len(expr_node$yes)
        no_len <- resolve_i32_expr_len(expr_node$no)
        if (is.null(yes_len) && is.null(no_len)) {
          return(NULL)
        }
        branch_len <- if (!is.null(yes_len)) yes_len else no_len
      } else if (yes_arr) {
        branch_len <- resolve_i32_expr_len(expr_node$yes)
      } else if (no_arr) {
        branch_len <- resolve_i32_expr_len(expr_node$no)
      }
      if (!is.null(branch_len)) {
        return(branch_len)
      }
      cond_type <- .mojor_ir_infer_type(expr_node$cond, ctx$type_env)
      if (cond_type %in% c("lgl[]", "bool[]")) {
        cond_len <- resolve_ctor_len_expr(expr_node$cond)
        if (is.null(cond_len) && identical(expr_node$cond$kind, "var")) {
          cond_len <- resolve_len_expr(expr_node$cond$name)
        }
        return(cond_len)
      }
      return(NULL)
    }
    NULL
  }

  resolve_vector_index_info <- function(idx_node, idx_pos) {
    hashed_arg_names <- if (!is.null(ctx$chr_index_hashed_args)) {
      as.character(ctx$chr_index_hashed_args)
    } else {
      character(0)
    }
    if (is.null(idx_node) || is.null(idx_node$kind)) {
      return(NULL)
    }
    idx_type <- .mojor_ir_infer_type(idx_node, ctx$type_env)
    if (!is.null(idx_type) && .mojor_type_is_array(idx_type)) {
      if (identical(idx_node$kind, "var") && identical(idx_type, "i32[]")) {
        if (idx_node$name %in% hashed_arg_names) {
          dimnames_axis <- dimname_axis_names(lhs$var, idx_pos)
          if (is.null(dimnames_axis) || length(dimnames_axis) == 0) {
            return(list(unsupported = TRUE))
          }
          len_info <- resolve_len_expr(idx_node$name)
          return(list(
            kind = "var_chr_hashed",
            name = idx_node$name,
            dimnames_axis = as.character(dimnames_axis),
            map_name = dimname_lookup_hash_map_name(lhs$var, idx_pos),
            len_node = .mojor_ir_call("Int", list(len_info$node)),
            len_label = paste0("Int(", len_info$label, ")")
          ))
        }
        len_info <- resolve_len_expr(idx_node$name)
        return(list(
          kind = "var_i32",
          name = idx_node$name,
          len_node = .mojor_ir_call("Int", list(len_info$node)),
          len_label = paste0("Int(", len_info$label, ")")
        ))
      }
      if (identical(idx_node$kind, "var") && identical(idx_type, "chr[]")) {
        dimnames_axis <- dimname_axis_names(lhs$var, idx_pos)
        if (is.null(dimnames_axis) || length(dimnames_axis) == 0) {
          return(list(unsupported = TRUE))
        }
        len_info <- resolve_len_expr(idx_node$name)
        return(list(
          kind = "var_chr",
          name = idx_node$name,
          dimnames_axis = as.character(dimnames_axis),
          map_name = dimname_lookup_map_name(lhs$var, idx_pos),
          len_node = .mojor_ir_call("Int", list(len_info$node)),
          len_label = paste0("Int(", len_info$label, ")")
        ))
      }
      if (idx_type == "i32[]") {
        if (identical(idx_node$kind, "c")) {
          parts <- idx_node$parts
          if (is.list(parts) && length(parts) > 0L) {
            values <- integer(length(parts))
            all_i32_consts <- TRUE
            for (i in seq_along(parts)) {
              part <- parts[[i]]
              if (is.null(part$kind) || !identical(part$kind, "const")) {
                all_i32_consts <- FALSE
                break
              }
              part_num <- suppressWarnings(as.numeric(part$value))
              if (!is.finite(part_num) || part_num != floor(part_num)) {
                all_i32_consts <- FALSE
                break
              }
              values[[i]] <- as.integer(part_num)
            }
            if (all_i32_consts) {
              return(list(
                kind = "literal_i32_c",
                values = values,
                len_node = .mojor_ir_const(as.character(length(values))),
                len_label = as.character(length(values))
              ))
            }
          }
        }
        len_info <- resolve_i32_expr_len(idx_node)
        if (is.null(len_info)) {
          return(list(unsupported = TRUE))
        }
        return(list(
          kind = "expr_i32",
          expr = idx_node,
          len_node = .mojor_ir_call("Int", list(len_info$node)),
          len_label = paste0("Int(", len_info$label, ")")
        ))
      }
      return(list(unsupported = TRUE))
    }
    NULL
  }

  if (!has_slice && !is.null(ctx$type_env)) {
    vector_idx_pos <- integer(0)
    vector_idx_info <- list()
    for (k in seq_along(indices)) {
      idx <- indices[[k]]
      idx_node <- .mojor_ir_selector_unwrap_node(idx)
      info <- resolve_vector_index_info(idx_node, k)
      if (!is.null(info) && isTRUE(info$unsupported)) {
        return(node)
      }
      if (!is.null(info)) {
        vector_idx_pos <- c(vector_idx_pos, k)
        vector_idx_info[[as.character(k)]] <- info
        next
      }
      idx_type <- .mojor_ir_infer_type(idx_node, ctx$type_env)
      if (!is.null(idx_type) && .mojor_type_is_array(idx_type)) {
        return(node)
      }
    }

    if (length(vector_idx_pos) > 0) {
      loop_vars <- character(0)
      ranges <- list()
      lhs_indices <- list()
      vector_loop_vars <- character(0)
      vector_loop_len_nodes <- list()
      vector_loop_len_labels <- character(0)
      dimname_lookup_inits <- list()
      map_names_emitted <- character(0)

      for (k in seq_along(indices)) {
        idx <- indices[[k]]
        idx_node <- .mojor_ir_selector_unwrap_node(idx)
        if (k %in% vector_idx_pos) {
          info <- vector_idx_info[[as.character(k)]]
          loop_var <- paste0("__mojor_i", k)
          len_int <- info$len_node
          loop_vars <- c(loop_vars, loop_var)
          ranges <- c(ranges, list(.mojor_ir_range(
            .mojor_ir_const("0"),
            len_int,
            end_exclusive = TRUE
          )))
          vector_loop_vars <- c(vector_loop_vars, loop_var)
          vector_loop_len_nodes <- c(vector_loop_len_nodes, list(len_int))
          vector_loop_len_labels <- c(vector_loop_len_labels, info$len_label)

          idx_read <- NULL
          if (identical(info$kind, "var_i32")) {
            idx_read <- .mojor_ir_call(
              "_mojor_read_i32",
              list(
                .mojor_ir_var(info$name),
                .mojor_ir_var(loop_var),
                len_int
              )
            )
          } else if (identical(info$kind, "literal_i32_c")) {
            idx_read <- build_literal_i32_read(info$values, loop_var)
          } else if (identical(info$kind, "expr_i32")) {
            idx_read <- info$expr
            idx_read[["__mojor_index_loop_var"]] <- loop_var
            idx_read[["__mojor_slice_loop_vars"]] <- loop_var
            idx_read <- .mojor_ir_call("Int", list(idx_read))
            idx_read[["__mojor_index_loop_var"]] <- loop_var
            idx_read[["__mojor_slice_loop_vars"]] <- loop_var
          } else if (identical(info$kind, "var_chr")) {
            map_name <- info$map_name
            if (!(map_name %in% map_names_emitted)) {
              dimname_lookup_inits <- c(
                dimname_lookup_inits,
                build_dimname_lookup_map_stmts(map_name, info$dimnames_axis)
              )
              map_names_emitted <- c(map_names_emitted, map_name)
            }
            idx_read <- build_var_chr_dimname_read(info$name, loop_var, map_name)
          } else if (identical(info$kind, "var_chr_hashed")) {
            map_name <- info$map_name
            if (!(map_name %in% map_names_emitted)) {
              dimname_lookup_inits <- c(
                dimname_lookup_inits,
                build_dimname_lookup_hash_map_stmts(map_name, info$dimnames_axis)
              )
              map_names_emitted <- c(map_names_emitted, map_name)
            }
            idx_read <- build_var_chr_hash_dimname_read(info$name, loop_var, len_int, map_name)
          }
          if (is.null(idx_read)) {
            return(node)
          }
          if (identical(index_base, "one_based")) {
            idx_norm <- .mojor_ir_binop("-", idx_read, .mojor_ir_const("1"))
            idx_norm$`__mojor_index_normalized` <- TRUE
            lhs_indices[[k]] <- idx_norm
          } else {
            lhs_indices[[k]] <- idx_read
          }
        } else {
          if (identical(index_base, "one_based")) {
            idx_norm <- .mojor_ir_binop("-", idx_node, .mojor_ir_const("1"))
            idx_norm$`__mojor_index_normalized` <- TRUE
            lhs_indices[[k]] <- idx_norm
          } else {
            lhs_indices[[k]] <- idx_node
          }
        }
      }

      # Lowered selector writes are internal zero-based nodes; keep src unset
      # so index emission does not re-apply user-space one-based normalization.
      inner_lhs <- .mojor_ir_index(
        .mojor_ir_var(lhs$var),
        lhs_indices,
        index_base = "zero_based",
        src = NULL
      )

      rhs_node <- node$rhs
      lhs_axis_loop_var <- function(axis_idx) {
        lhs_idx <- indices[[axis_idx]]
        if (is.null(lhs_idx) || is.null(lhs_idx$kind)) {
          return(NULL)
        }
        if (lhs_idx$kind %in% c("vec_index", "c")) {
          return(paste0("__mojor_i", axis_idx))
        }
        if (lhs_idx$kind %in% c("slice_index", "missing_index")) {
          return(paste0("__mojor_i", axis_idx))
        }
        NULL
      }
      lhs_axis_start <- function(axis_idx) {
        lhs_idx <- indices[[axis_idx]]
        if (is.null(lhs_idx) || is.null(lhs_idx$kind)) {
          return(NA_integer_)
        }
        if (lhs_idx$kind %in% c("vec_index", "c", "missing_index")) {
          return(1L)
        }
        if (identical(lhs_idx$kind, "slice_index") &&
            !is.null(lhs_idx$start) &&
            is.list(lhs_idx$start) &&
            identical(lhs_idx$start$kind, "const")) {
          start_num <- suppressWarnings(as.numeric(lhs_idx$start$value))
          if (!is.na(start_num) && is.finite(start_num)) {
            return(as.integer(start_num))
          }
        }
        NA_integer_
      }
      rhs_node <- .mojor_ir_map_rhs_slices_to_loop_indices(
        rhs_node,
        indices,
        lhs_axis_loop_var,
        lhs_axis_start,
        rhs_zero_based_vars = loop_vars
      )
      rhs_pos <- .mojor_ir_build_slice_linear_pos_expr(vector_loop_vars, vector_loop_len_nodes)
      rhs_cycle_len <- .mojor_ir_build_slice_cycle_len_expr(vector_loop_len_nodes)
      rhs_node <- .mojor_ir_scalarize_array_rhs_expr(
        rhs_node,
        rhs_pos,
        rhs_cycle_len_expr = rhs_cycle_len,
        type_env = ctx$type_env,
        resolve_len_fn = resolve_len_expr
      )

      inner_assign <- .mojor_ir_assign(inner_lhs, rhs_node)
      if (!is.null(inner_assign$rhs) && is.list(inner_assign$rhs) && !is.null(inner_assign$rhs$kind)) {
        inner_assign$rhs[["__mojor_slice_loop_vars"]] <- loop_vars
        inner_assign$rhs[["__mojor_slice_dim_exprs"]] <- vector_loop_len_labels
      }

      body <- .mojor_ir_block(list(inner_assign))
      for (k in seq_along(loop_vars)) {
        body <- .mojor_ir_loop(
          loop_vars[k],
          ranges[[k]],
          .mojor_ir_block(list(body))
        )
      }
      if (length(dimname_lookup_inits) > 0) {
        return(.mojor_ir_block(c(dimname_lookup_inits, list(body))))
      }
      return(body)
    }
  }
  if (!has_slice) {
    return(node)
  } # No slices to lower

 # Check for mask extraction RHS: out[1:n] <- x[mask]
 # Pattern: RHS is x[mask] where mask is lgl[]/bool[]
  mask_var <- NULL
  mask_base <- NULL
  if (!is.null(node$rhs) && identical(node$rhs$kind, "index")) {
    if (!is.null(node$rhs$base) && identical(node$rhs$base$kind, "var") &&
      !is.null(node$rhs$indices) && length(node$rhs$indices) == 1) {
      rhs_idx <- node$rhs$indices[[1]]
      if (!is.null(rhs_idx$kind) && identical(rhs_idx$kind, "var")) {
        idx_type <- if (!is.null(ctx$type_env)) ctx$type_env[[rhs_idx$name]] else NULL
        if (!is.null(idx_type) && idx_type %in% c("lgl[]", "bool[]")) {
          mask_var <- rhs_idx$name
          mask_base <- node$rhs$base$name
        }
      }
    }
  }

 # Mask extraction lowering: out[1:n] <- x[mask]
  if (!is.null(mask_var)) {
 # Only lower if LHS is simple 1D slice starting at 1
    if (n_idx != 1 || indices[[1]]$kind != "slice_index") {
      return(node)
    }
    slice_idx <- indices[[1]]
    if (slice_idx$start$kind != "const" || slice_idx$start$value != "1") {
      return(node)
    }

 # Get mask length
    mask_len_var <- if (!is.null(ctx$len_var_map) && mask_var %in% names(ctx$len_var_map)) {
      ctx$len_var_map[[mask_var]]
    } else {
      "n_i" # Fallback
    }

 # Build compressed selection:
 # var __mojor_j = 0
 # for __mojor_scan in range(Int(mask_len)):
 # if _mojor_read_lgl(mask, __mojor_scan, Int(mask_len)) == 1:
 # out[__mojor_j] = x[__mojor_scan]
 # __mojor_j += 1

    counter_var <- "__mojor_j"
    scan_var <- "__mojor_scan"

 # Counter init: var __mojor_j = 0
    counter_init <- .mojor_ir_assign(
      .mojor_ir_var(counter_var),
      .mojor_ir_const("0")
    )

 # Mask read: _mojor_read_lgl(mask, __mojor_scan, Int(mask_len)) == 1
    mask_check <- .mojor_ir_binop(
      "==",
      .mojor_ir_call("_mojor_read_lgl", list(
        .mojor_ir_var(mask_var),
        .mojor_ir_var(scan_var),
        .mojor_ir_call("Int", list(.mojor_ir_var(mask_len_var)))
      )),
      .mojor_ir_const("1")
    )

 # Inner assignment: out[__mojor_j] = x[__mojor_scan]
    inner_assign <- .mojor_ir_assign(
      .mojor_ir_index(
        .mojor_ir_var(lhs$var),
        list(.mojor_ir_var(counter_var)),
        index_base = "zero_based"
      ),
      .mojor_ir_index(
        .mojor_ir_var(mask_base),
        list(.mojor_ir_var(scan_var)),
        index_base = "zero_based"
      )
    )

 # Counter increment: __mojor_j += 1
    counter_incr <- .mojor_ir_assign(
      .mojor_ir_var(counter_var),
      .mojor_ir_binop("+", .mojor_ir_var(counter_var), .mojor_ir_const("1"))
    )

 # If block: if mask_check then assign and increment
    if_block <- .mojor_ir_if(
      mask_check,
      .mojor_ir_block(list(inner_assign, counter_incr))
    )

 # Scan loop: for __mojor_scan in range(Int(mask_len))
    scan_loop <- .mojor_ir_loop(
      scan_var,
      .mojor_ir_range(
        .mojor_ir_const("0"),
        .mojor_ir_call("Int", list(.mojor_ir_var(mask_len_var))),
        end_exclusive = TRUE
      ),
      .mojor_ir_block(list(if_block))
    )

 # Return block with counter init + scan loop
    return(.mojor_ir_block(list(counter_init, scan_loop)))
  }

 # Build loop structure
  loop_vars <- character(0)
  ranges <- list()
  lhs_indices <- list()

  for (k in seq_along(indices)) {
    idx <- indices[[k]]
    idx_node <- .mojor_ir_selector_unwrap_node(idx)

    vec_info <- NULL
    if (!is.null(ctx$type_env)) {
      vec_info <- resolve_vector_index_info(idx_node, k)
      if (!is.null(vec_info) && isTRUE(vec_info$unsupported)) {
        return(node)
      }
    }
    if (!is.null(vec_info)) {
      if (!(vec_info$kind %in% c("var_i32", "literal_i32_c", "expr_i32"))) {
        return(node)
      }
      loop_var <- paste0("__mojor_i", k)
      loop_vars <- c(loop_vars, loop_var)
      len_int <- vec_info$len_node
      ranges <- c(ranges, list(.mojor_ir_range(
        .mojor_ir_const("0"),
        len_int,
        end_exclusive = TRUE
      )))

      idx_read <- NULL
      if (identical(vec_info$kind, "var_i32")) {
        idx_read <- .mojor_ir_call(
          "_mojor_read_i32",
          list(
            .mojor_ir_var(vec_info$name),
            .mojor_ir_var(loop_var),
            len_int
          )
        )
      } else if (identical(vec_info$kind, "literal_i32_c")) {
        idx_read <- build_literal_i32_read(vec_info$values, loop_var)
      } else if (identical(vec_info$kind, "expr_i32")) {
        idx_read <- vec_info$expr
        idx_read[["__mojor_index_loop_var"]] <- loop_var
        idx_read[["__mojor_slice_loop_vars"]] <- loop_var
        idx_read <- .mojor_ir_call("Int", list(idx_read))
        idx_read[["__mojor_index_loop_var"]] <- loop_var
        idx_read[["__mojor_slice_loop_vars"]] <- loop_var
      }
      if (is.null(idx_read)) {
        return(node)
      }
      if (identical(index_base, "one_based")) {
        idx_norm <- .mojor_ir_binop("-", idx_read, .mojor_ir_const("1"))
        idx_norm$`__mojor_index_normalized` <- TRUE
        lhs_indices[[k]] <- idx_norm
      } else {
        lhs_indices[[k]] <- idx_read
      }
      next
    }

    if (idx$kind == "slice_index") {
 # Slice: start:end
 # Only handle simple case: start = 1
      if (idx$start$kind != "const" || idx$start$value != "1") {
        return(node)
      }

      loop_var <- paste0("__mojor_i", k)
      loop_vars <- c(loop_vars, loop_var)

 # Range: range(0, Int(end_expr))
      ranges <- c(ranges, list(.mojor_ir_range(
        .mojor_ir_const("0"),
        .mojor_ir_call("Int", list(idx$end)),
        end_exclusive = TRUE
      )))

 # LHS index is loop_var (0-based)
      lhs_indices[[k]] <- .mojor_ir_var(loop_var)
    } else if (idx$kind == "missing_index") {
 # Missing: entire dimension (mat[i, ], arr[i, , k])
      loop_var <- paste0("__mojor_i", k)
      loop_vars <- c(loop_vars, loop_var)

 # Get dimension from layout_ctx
      dim_expr <- NULL

      if (n_idx == 2) {
 # 2D matrix
        if (k == 1) {
 # First dim: nrow
          if (!is.null(ctx$nrow_var) && lhs$var == "out") {
            dim_expr <- .mojor_ir_var(ctx$nrow_var)
          } else if (!is.null(ctx$nrow_var_map) && lhs$var %in% names(ctx$nrow_var_map)) {
            dim_expr <- .mojor_ir_var(ctx$nrow_var_map[[lhs$var]])
          } else {
 # Step 5.1: Check local matrix dims
            local_dims <- .mojor_state$current_local_matrix_dims
            if (!is.null(local_dims) && !is.null(local_dims[[lhs$var]])) {
              dim_expr <- .mojor_ir_var(paste0("nrow_", lhs$var))
            }
          }
        } else if (k == 2) {
 # Second dim: ncol
          if (!is.null(ctx$ncol_var) && lhs$var == "out") {
            dim_expr <- .mojor_ir_var(ctx$ncol_var)
          } else if (!is.null(ctx$ncol_var_map) && lhs$var %in% names(ctx$ncol_var_map)) {
            dim_expr <- .mojor_ir_var(ctx$ncol_var_map[[lhs$var]])
          } else {
 # Step 5.1: Check local matrix dims
            local_dims <- .mojor_state$current_local_matrix_dims
            if (!is.null(local_dims) && !is.null(local_dims[[lhs$var]])) {
              dim_expr <- .mojor_ir_var(paste0("ncol_", lhs$var))
            }
          }
        }
      } else if (n_idx >= 3) {
 # 3D+ array - use dim_var_map
        if (lhs$var == "out") {
 # Output array - use out_dim_var
          if (!is.null(ctx$dim_var_map) && "out" %in% names(ctx$dim_var_map)) {
            dim_var <- ctx$dim_var_map[["out"]]
 # Emit: dim_var[k-1]
            dim_expr <- .mojor_ir_index(
              .mojor_ir_var(dim_var),
              list(.mojor_ir_const(as.character(k - 1))),
              index_base = "zero_based"
            )
          }
        } else {
 # Input array - look up in dim_var_map
          if (!is.null(ctx$dim_var_map) && lhs$var %in% names(ctx$dim_var_map)) {
            dim_var <- ctx$dim_var_map[[lhs$var]]
            dim_expr <- .mojor_ir_index(
              .mojor_ir_var(dim_var),
              list(.mojor_ir_const(as.character(k - 1))),
              index_base = "zero_based"
            )
          } else {
 # Fallback: local array allocations track concrete dimension expressions
            local_dims <- .mojor_state$current_local_array_dims
            if (!is.null(local_dims) && !is.null(local_dims[[lhs$var]]) && !is.null(local_dims[[lhs$var]]$dim)) {
              local_dim_exprs <- local_dims[[lhs$var]]$dim
              if (length(local_dim_exprs) >= k) {
                dim_expr <- local_dim_exprs[[k]]
                if (!is.null(dim_expr)) {
                  dim_expr <- .mojor_ir_expr_build(dim_expr)
                }
              }
            }
          }
        }
      }

      if (is.null(dim_expr)) {
        return(node)
      } # Can't determine dimension

 # Range: range(0, Int(dim_expr))
      ranges <- c(ranges, list(.mojor_ir_range(
        .mojor_ir_const("0"),
        .mojor_ir_call("Int", list(dim_expr)),
        end_exclusive = TRUE
      )))

 # LHS index is loop_var (0-based)
      lhs_indices[[k]] <- .mojor_ir_var(loop_var)
    } else {
 # Scalar index (canonical selector node, wrapper sugar already unwrapped)
      idx <- idx_node
 # Keep scalar index as-is (will be converted to 0-based during emit)
 # Subtract 1 only for one-based index semantics
      if (identical(index_base, "one_based")) {
        idx_norm <- .mojor_ir_binop("-", idx, .mojor_ir_const("1"))
        idx_norm$`__mojor_index_normalized` <- TRUE
        lhs_indices[[k]] <- idx_norm
      } else {
        lhs_indices[[k]] <- idx
      }
    }
  }

 # Build inner assignment
  # Lowered selector writes are internal zero-based nodes; keep src unset
  # so index emission does not re-apply user-space one-based normalization.
  inner_lhs <- .mojor_ir_index(
    .mojor_ir_var(lhs$var),
    lhs_indices,
    index_base = "zero_based",
    src = NULL
  )

  rhs_node <- node$rhs
  lhs_axis_loop_var <- function(axis_idx) {
    lhs_idx <- indices[[axis_idx]]
    if (is.null(lhs_idx) || is.null(lhs_idx$kind)) {
      return(NULL)
    }
    if (lhs_idx$kind %in% c("vec_index", "c")) {
      return(paste0("__mojor_i", axis_idx))
    }
    if (!(lhs_idx$kind %in% c("slice_index", "missing_index"))) {
      return(NULL)
    }
    paste0("__mojor_i", axis_idx)
  }
  lhs_axis_start <- function(axis_idx) {
    lhs_idx <- indices[[axis_idx]]
    if (is.null(lhs_idx) || is.null(lhs_idx$kind)) {
      return(NA_integer_)
    }
    if (identical(lhs_idx$kind, "missing_index")) {
      return(1L)
    }
    if (lhs_idx$kind %in% c("vec_index", "c")) {
      return(1L)
    }
    if (identical(lhs_idx$kind, "slice_index") &&
        !is.null(lhs_idx$start) &&
        is.list(lhs_idx$start) &&
        identical(lhs_idx$start$kind, "const")) {
      start_num <- suppressWarnings(as.numeric(lhs_idx$start$value))
      if (!is.na(start_num) && is.finite(start_num)) {
        return(as.integer(start_num))
      }
    }
    NA_integer_
  }
  rhs_node <- .mojor_ir_map_rhs_slices_to_loop_indices(
    rhs_node,
    indices,
    lhs_axis_loop_var,
    lhs_axis_start,
    rhs_zero_based_vars = loop_vars
  )
  loop_extent_nodes <- lapply(ranges, function(rng) rng$end)
  rhs_pos <- .mojor_ir_build_slice_linear_pos_expr(loop_vars, loop_extent_nodes)
  rhs_cycle_len <- .mojor_ir_build_slice_cycle_len_expr(loop_extent_nodes)
  rhs_node <- .mojor_ir_scalarize_array_rhs_expr(
    rhs_node,
    rhs_pos,
    rhs_cycle_len_expr = rhs_cycle_len,
    type_env = ctx$type_env,
    resolve_len_fn = resolve_len_expr
  )

  inner_assign <- .mojor_ir_assign(inner_lhs, rhs_node)

 # Preserve the slice-generated loop variables on the RHS so constructor emissions
 # (e.g. c(...)) are indexed only by slice dimensions, not enclosing loops.
 # This keeps runtime selection aligned when RHS reads are loop-invariant and
 # should only advance with the slice assignment dimensions.
  if (!is.null(inner_assign$rhs) && is.list(inner_assign$rhs)) {
    attach_slice_loop_vars <- function(expr, loop_vars) {
      if (is.null(expr) || !is.list(expr)) {
        return(expr)
      }
      if (!is.null(expr$kind)) {
        expr[["__mojor_slice_loop_vars"]] <- loop_vars
      }
      if (expr$kind == "cast" && !is.null(expr$expr)) {
        expr$expr <- attach_slice_loop_vars(expr$expr, loop_vars)
      } else if (expr$kind == "raw" && !is.null(expr$expr)) {
        expr$expr <- attach_slice_loop_vars(expr$expr, loop_vars)
      }
      expr
    }
    inner_assign$rhs <- attach_slice_loop_vars(inner_assign$rhs, loop_vars)
  }

 # Wrap in nested loops so dim-1 varies fastest (R/column-major order).
  body <- .mojor_ir_block(list(inner_assign))
  for (k in seq_along(loop_vars)) {
    body <- .mojor_ir_loop(
      loop_vars[k],
      ranges[[k]],
      .mojor_ir_block(list(body))
    )
  }

 # Return the outermost loop (or block if no loops)
  if (length(loop_vars) == 0) {
    inner_assign
  } else {
    body
  }
}

.mojor_ir_lower_scalar_reduce <- function(node, ctx) {
 # Lower scalar_reduce node to explicit if/else + loop structure
 # ctx must contain: n_var (array length variable name), type_env (optional)
 # Step 23: scalar_reduce now carries init, axis, associative, commutative metadata
 # These fields enable future optimization (SIMD, tree reduction, parallel reordering)
 # Current lowering infers init values from op+type; future passes can use node$init directly
 # Step 7.1: Added na_rm support for NA-aware reductions
  if (is.null(node) || node$kind != "scalar_reduce") {
    return(node)
  }

  op <- node$op
  acc <- node$acc
  arg <- node$arg
  n_var <- if (!is.null(ctx$n_var)) ctx$n_var else "n_i"
  na_rm <- if (!is.null(node$na_rm)) node$na_rm else FALSE

 # Step 23 metadata (available but not yet used in lowering):
 # node$init - IR expression for initial value (NULL = infer from op+type)
 # node$axis - reduction axis (0 for 1D)
 # node$associative - enables tree reduction
 # node$commutative - enables parallel reordering
 # node$na_rm - skip NA values in reduction (Step 7.1)

 # Determine type for NaN/Inf sentinels
  arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[arg]] else NULL
  is_f32 <- identical(arg_type, "f32[]")

  if (op %in% c("sum", "product")) {
    combine <- if (op == "sum") "+" else "*"
    if (na_rm) {
 # NA-aware reduction: skip NA values
 # acc = 0 (or 1 for product)
 # count = 0
 # for __mojor_ri in range(1, n_i + 1):
 # val = arg[__mojor_ri - 1]
 # if val == val: # not NaN
 # acc = acc (+|*) val
 # count = count + 1
 # For product, need to handle empty case (return 1)
      init_val <- if (op == "sum") "0" else "1"
      init_val_f32 <- if (is_f32) "Float32(0.0)" else "0.0"
      init_val_f32 <- if (op == "product") "Float32(1.0)" else init_val_f32
      init_val <- if (is_f32) init_val_f32 else init_val

      .mojor_ir_block(list(
        .mojor_ir_assign(.mojor_ir_var(acc), .mojor_ir_const(init_val)),
        .mojor_ir_loop(
          "__mojor_ri",
          .mojor_ir_range(
            .mojor_ir_const("1"),
            .mojor_ir_binop("+", .mojor_ir_var(n_var), .mojor_ir_const("1")),
            end_exclusive = TRUE
          ),
          .mojor_ir_block(list(
            .mojor_ir_assign(
              .mojor_ir_var(acc),
              .mojor_ir_ifelse(
                .mojor_ir_call("is.na", list(
                  .mojor_ir_index(
                    .mojor_ir_var(arg),
                    list(.mojor_ir_binop("-", .mojor_ir_var("__mojor_ri"), .mojor_ir_const("1"))),
                    index_base = "zero_based"
                  )
                )),
                .mojor_ir_var(acc),
                .mojor_ir_binop(
                  combine,
                  .mojor_ir_var(acc),
                  .mojor_ir_index(
                    .mojor_ir_var(arg),
                    list(.mojor_ir_binop("-", .mojor_ir_var("__mojor_ri"), .mojor_ir_const("1"))),
                    index_base = "zero_based"
                  )
                )
              )
            )
          ))
        )
      ))
    } else {
 # Standard reduction: include all values
      if (!is.null(node$init)) {
        init_expr <- node$init
      } else if (op == "sum") {
        init_expr <- if (is_f32) .mojor_ir_const("0.0") else .mojor_ir_const("0")
      } else {
        init_expr <- if (is_f32) .mojor_ir_const("1.0") else .mojor_ir_const("1")
      }
 # Lower to:
 # acc = init_expr
 # for __mojor_ri in range(1, n_i + 1):
 # acc = acc (+|*) arg[__mojor_ri - 1]
      .mojor_ir_block(list(
        .mojor_ir_assign(.mojor_ir_var(acc), init_expr),
        .mojor_ir_loop(
          "__mojor_ri",
          .mojor_ir_range(
            .mojor_ir_const("1"),
            .mojor_ir_binop("+", .mojor_ir_var(n_var), .mojor_ir_const("1")),
            end_exclusive = TRUE
          ),
          .mojor_ir_block(list(
            .mojor_ir_assign(
              .mojor_ir_var(acc),
              .mojor_ir_binop(
                combine,
                .mojor_ir_var(acc),
                .mojor_ir_index(
                  .mojor_ir_var(arg),
                  list(.mojor_ir_binop("-", .mojor_ir_var("__mojor_ri"), .mojor_ir_const("1"))),
                  index_base = "zero_based"
                )
              )
            )
          ))
        )
      ))
    }
  } else if (op %in% c("min", "max")) {
 # Sentinel values for empty array
    nan_const <- if (is_f32) "_MOJOR_NAN_F32" else "_MOJOR_NAN"

 # Lower to:
 # if n_i == 0:
 # acc = nan_const
 # else:
 # acc = arg[0]
 # for __mojor_ri in range(2, n_i + 1):
 # acc = min/max(acc, arg[__mojor_ri - 1])

    .mojor_ir_if(
      cond = .mojor_ir_binop("==", .mojor_ir_var(n_var), .mojor_ir_const("0")),
      then = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var(acc),
          .mojor_ir_var(nan_const)
        )
      )),
      else_block = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var(acc),
          .mojor_ir_index(
            .mojor_ir_var(arg),
            list(.mojor_ir_const("0")),
            index_base = "zero_based"
          )
        ),
        .mojor_ir_loop(
          "__mojor_ri",
          .mojor_ir_range(
            .mojor_ir_const("2"),
            .mojor_ir_binop("+", .mojor_ir_var(n_var), .mojor_ir_const("1")),
            end_exclusive = TRUE
          ),
          .mojor_ir_block(list(
            .mojor_ir_assign(
              .mojor_ir_var(acc),
              .mojor_ir_call(op, list(
                .mojor_ir_var(acc),
                .mojor_ir_index(
                  .mojor_ir_var(arg),
                  list(.mojor_ir_binop("-", .mojor_ir_var("__mojor_ri"), .mojor_ir_const("1"))),
                  index_base = "zero_based"
                )
              ))
            )
          ))
        )
      ))
    )
  } else if (op %in% c("which.min", "which.max")) {
 # Determine Mojo type for comparison
    mojo_ty <- if (is_f32) "Float32" else "Float64"
    if (!is.null(arg_type) && grepl("^i32", arg_type)) mojo_ty <- "Int32"
    zero_v <- if (mojo_ty %in% c("Float64", "Float32")) "0.0" else "0"
    want_min <- (op == "which.min")
    cmp_op <- if (want_min) "<" else ">"

 # Lower to:
 # if n_i == 0:
 # acc = Int32(0)
 # else:
 # var best: mojo_ty = zero_v
 # var has_value = False
 # acc = Int32(0)
 # for __mojor_ri in range(1, n_i + 1):
 # var v: mojo_ty = arg[__mojor_ri - 1]
 # if not has_value:
 # best = v
 # acc = Int32(__mojor_ri)
 # has_value = True
 # elif v < best: # or v > best for which.max
 # best = v
 # acc = Int32(__mojor_ri)

 # Note: which.min/which.max lowering produces raw Mojo var declarations
 # These can't be represented purely in Core IR without extending the IR.
 # For now, we'll create a "raw_block" wrapper that preserves the emit logic.
 # This is a limitation - Step 20 focuses on min/max, which.min/which.max
 # will be improved in Step 23 (reduction first-class semantics).

 # For now, keep which.min/which.max as-is (no lowering)
 # They will still be emitted by the old emit path
    return(node)
  } else {
 # Unknown op - return as-is
    return(node)
  }
}

.mojor_ir_lower_gpu_reduce <- function(node, ctx) {
 # Lower gpu_reduce HIR sugar to a core call node.
 # Canonical call shape:
 # _mojor_gpu_reduce(arg, "op", dims_or_empty, keepdims_bool, dims_default_bool, length(arg))
  if (is.null(node) || node$kind != "gpu_reduce") {
    return(node)
  }

  arg_low <- .mojor_ir_lower(node$arg, ctx)
  if (is.null(arg_low)) {
    return(NULL)
  }

  dims_low <- NULL
  if (!is.null(node$dims)) {
    dims_low <- .mojor_ir_lower(node$dims, ctx)
    if (is.null(dims_low)) {
      return(NULL)
    }
  } else {
 # Use a typed scalar sentinel for dims=NULL to keep helper calls monomorphic.
    dims_low <- .mojor_ir_const("Int(0)")
  }

  op <- if (!is.null(node$op)) as.character(node$op) else ""
  op_low <- .mojor_ir_const(paste0("\"", op, "\""))
  keepdims_low <- .mojor_ir_const(if (isTRUE(node$keepdims)) "True" else "False")
  dims_default_low <- .mojor_ir_const(if (is.null(node$dims)) "True" else "False")
  n_low <- .mojor_ir_call("length", list(arg_low))

  .mojor_ir_call(
    "_mojor_gpu_reduce",
    list(arg_low, op_low, dims_low, keepdims_low, dims_default_low, n_low),
    src = node$src
  )
}

.mojor_ir_gpu_reduce_preview_op <- function(op) {
  if (is.null(op)) {
    return(NULL)
  }
  switch(as.character(op),
    "sum" = "sum",
    "prod" = "product",
    "min" = "min",
    "max" = "max",
    "argmin" = "which.min",
    "argmax" = "which.max",
    NULL
  )
}

.mojor_ir_gpu_jit_mode <- function(ctx) {
  mode <- if (!is.null(ctx$gpu_jit_mode)) as.character(ctx$gpu_jit_mode) else "auto"
  if (!mode %in% c("auto", "unified_preview")) {
    return("auto")
  }
  mode
}

.mojor_ir_gpu_jit_unified_enabled <- function(ctx) {
  .mojor_ir_gpu_jit_mode(ctx) %in% c("auto", "unified_preview")
}

.mojor_ir_lower_gpu_reduce_preview_expr <- function(node, ctx, tmp_name) {
  if (is.null(node) || node$kind != "gpu_reduce") {
    return(NULL)
  }
  if (!is.null(node$dims) || isTRUE(node$keepdims)) {
    return(NULL)
  }
  if (is.null(tmp_name) || !nzchar(tmp_name)) {
    return(NULL)
  }

  arg_low <- .mojor_ir_lower(node$arg, ctx)
  if (is.null(arg_low) || !identical(arg_low$kind, "var") || is.null(arg_low$name)) {
    return(NULL)
  }

  mapped_op <- .mojor_ir_gpu_reduce_preview_op(node$op)
  if (!is.null(mapped_op)) {
    arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[arg_low$name]] else NULL
    init_expr <- NULL
    if (identical(mapped_op, "sum")) {
      if (!is.null(arg_type) && grepl("^f32", arg_type)) {
        init_expr <- .mojor_ir_const("Float32(0.0)")
      } else if (!is.null(arg_type) && grepl("^f64", arg_type)) {
        init_expr <- .mojor_ir_const("0.0")
      }
    }
    red_stmt <- .mojor_ir_scalar_reduce(
      op = mapped_op,
      acc = tmp_name,
      arg = arg_low$name,
      init = init_expr,
      src = node$src
    )
    return(list(expr = .mojor_ir_var(tmp_name), prep_stmts = list(red_stmt)))
  }

  if (identical(as.character(node$op), "mean")) {
    arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[arg_low$name]] else NULL
    sum_init <- if (!is.null(arg_type) && grepl("^f32", arg_type)) {
      .mojor_ir_const("Float32(0.0)")
    } else {
      .mojor_ir_const("0.0")
    }
    red_stmt <- .mojor_ir_scalar_reduce(
      op = "sum",
      acc = tmp_name,
      arg = arg_low$name,
      init = sum_init,
      src = node$src
    )
    n_expr <- .mojor_ir_call("length", list(.mojor_ir_var(arg_low$name)))
    denom_cast <- if (!is.null(arg_type) && grepl("^f32", arg_type)) "Float32" else "Float64"
    mean_expr <- .mojor_ir_binop(
      "/",
      .mojor_ir_var(tmp_name),
      .mojor_ir_call(denom_cast, list(n_expr))
    )
    return(list(expr = mean_expr, prep_stmts = list(red_stmt)))
  }

  NULL
}

.mojor_ir_lower_gpu_reduce_assign <- function(node, ctx) {
 # Unified-lowering lane: lower assign(lhs_var, gpu_reduce(...)) into scalar_reduce
 # core IR for ops that map cleanly to existing scalar reduction semantics.
  if (is.null(node) || node$kind != "assign") {
    return(NULL)
  }
  if (is.null(node$lhs) || node$lhs$kind != "var") {
    return(NULL)
  }
  if (is.null(node$rhs) || node$rhs$kind != "gpu_reduce") {
    return(NULL)
  }

  if (!.mojor_ir_gpu_jit_unified_enabled(ctx)) {
    return(NULL)
  }

  rhs <- node$rhs
  if (!is.null(rhs$dims) || isTRUE(rhs$keepdims)) {
    return(NULL)
  }

  mapped_op <- .mojor_ir_gpu_reduce_preview_op(rhs$op)
  if (is.null(mapped_op)) {
    return(NULL)
  }

  arg_low <- .mojor_ir_lower(rhs$arg, ctx)
  if (is.null(arg_low) || !identical(arg_low$kind, "var") || is.null(arg_low$name)) {
    return(NULL)
  }

  .mojor_ir_scalar_reduce(
    op = mapped_op,
    acc = node$lhs$name,
    arg = arg_low$name,
    src = rhs$src
  )
}

.mojor_ir_lower_gpu_reduce_expr <- function(node, ctx, tmp_prefix = "__mojor_gpu_reduce_tmp_") {
  if (!.mojor_ir_gpu_jit_unified_enabled(ctx)) {
    return(NULL)
  }

  tmp_counter <- 0L
  next_tmp <- function() {
    tmp_counter <<- tmp_counter + 1L
    paste0(tmp_prefix, tmp_counter)
  }

  rewrite_list <- function(items, rewrite_fn) {
    out <- vector("list", length(items))
    preps <- list()
    for (i in seq_along(items)) {
      item_res <- rewrite_fn(items[[i]])
      out[[i]] <- item_res$expr
      if (length(item_res$prep_stmts) > 0) {
        preps <- c(preps, item_res$prep_stmts)
      }
    }
    if (!is.null(names(items))) names(out) <- names(items)
    list(items = out, prep_stmts = preps)
  }

  rewrite <- function(expr) {
    if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
      return(list(expr = expr, prep_stmts = list()))
    }

    kind <- if (exists(".mojor_ir_base_kind", mode = "function")) .mojor_ir_base_kind(expr$kind) else expr$kind

    if (identical(kind, "gpu_reduce")) {
      lowered_reduce <- .mojor_ir_lower_gpu_reduce_preview_expr(
        expr,
        ctx,
        tmp_name = next_tmp()
      )
      if (is.null(lowered_reduce)) {
        return(list(expr = expr, prep_stmts = list()))
      }
      return(lowered_reduce)
    }

    if (identical(kind, "binop")) {
      lhs_res <- rewrite(expr$lhs)
      rhs_res <- rewrite(expr$rhs)
      expr$lhs <- lhs_res$expr
      expr$rhs <- rhs_res$expr
      return(list(expr = expr, prep_stmts = c(lhs_res$prep_stmts, rhs_res$prep_stmts)))
    }
    if (identical(kind, "unop")) {
      inner_res <- rewrite(expr$expr)
      expr$expr <- inner_res$expr
      return(list(expr = expr, prep_stmts = inner_res$prep_stmts))
    }
    if (identical(kind, "call")) {
      args_res <- rewrite_list(expr$args, rewrite)
      expr$args <- args_res$items
      return(list(expr = expr, prep_stmts = args_res$prep_stmts))
    }
    if (identical(kind, "index")) {
      base_res <- rewrite(expr$base)
      idx_res <- rewrite_list(expr$indices, rewrite)
      expr$base <- base_res$expr
      expr$indices <- idx_res$items
      return(list(expr = expr, prep_stmts = c(base_res$prep_stmts, idx_res$prep_stmts)))
    }
    if (identical(kind, "ifelse")) {
      cond_res <- rewrite(expr$cond)
      yes_res <- rewrite(expr$yes)
      no_res <- rewrite(expr$no)
      expr$cond <- cond_res$expr
      expr$yes <- yes_res$expr
      expr$no <- no_res$expr
      return(list(expr = expr, prep_stmts = c(cond_res$prep_stmts, yes_res$prep_stmts, no_res$prep_stmts)))
    }
    if (identical(kind, "cast")) {
      inner_res <- rewrite(expr$expr)
      expr$expr <- inner_res$expr
      return(list(expr = expr, prep_stmts = inner_res$prep_stmts))
    }
    if (identical(kind, "range")) {
      start_res <- rewrite(expr$start)
      end_res <- rewrite(expr$end)
      preps <- c(start_res$prep_stmts, end_res$prep_stmts)
      expr$start <- start_res$expr
      expr$end <- end_res$expr
      if (!is.null(expr$step)) {
        step_res <- rewrite(expr$step)
        expr$step <- step_res$expr
        preps <- c(preps, step_res$prep_stmts)
      }
      return(list(expr = expr, prep_stmts = preps))
    }

    list(expr = expr, prep_stmts = list())
  }

  rewrite(node)
}

.mojor_ir_lower_gpu_matmul <- function(node, ctx) {
 # Lower gpu_matmul HIR sugar to a core call node.
 # Canonical call shape:
 # _mojor_gpu_matmul(a, b, transpose_a_bool, transpose_b_bool)
  if (is.null(node) || node$kind != "gpu_matmul") {
    return(node)
  }

  a_low <- .mojor_ir_lower(node$a, ctx)
  b_low <- .mojor_ir_lower(node$b, ctx)
  if (is.null(a_low) || is.null(b_low)) {
    return(NULL)
  }

  transpose_a_low <- .mojor_ir_const(if (isTRUE(node$transpose_a)) "True" else "False")
  transpose_b_low <- .mojor_ir_const(if (isTRUE(node$transpose_b)) "True" else "False")

  .mojor_ir_call(
    "_mojor_gpu_matmul",
    list(a_low, b_low, transpose_a_low, transpose_b_low),
    src = node$src
  )
}

.mojor_ir_lower_gpu_matmul_preview_assign <- function(node, ctx) {
 # Unified-lowering lane helper-free lowering for assign(lhs, gpu_matmul(...)).
 # Rewrites to explicit nested loops over out[row, col] with an inner dot-product loop.
  if (is.null(node) || node$kind != "assign") {
    return(NULL)
  }
  if (is.null(node$lhs) || node$lhs$kind != "var") {
    return(NULL)
  }
  if (is.null(node$rhs) || node$rhs$kind != "gpu_matmul") {
    return(NULL)
  }

  if (!.mojor_ir_gpu_jit_unified_enabled(ctx)) {
    return(NULL)
  }

  lhs_low <- .mojor_ir_lower(node$lhs, ctx)
  rhs <- node$rhs
  a_low <- .mojor_ir_lower(rhs$a, ctx)
  b_low <- .mojor_ir_lower(rhs$b, ctx)
  if (is.null(lhs_low) || is.null(a_low) || is.null(b_low)) {
    return(NULL)
  }
  if (!identical(lhs_low$kind, "var") || !identical(a_low$kind, "var") || !identical(b_low$kind, "var")) {
    return(NULL)
  }

  ta <- isTRUE(rhs$transpose_a)
  tb <- isTRUE(rhs$transpose_b)

  a_nrow_var <- "__mojor_gpu_mm_a_nrow"
  a_ncol_var <- "__mojor_gpu_mm_a_ncol"
  b_nrow_var <- "__mojor_gpu_mm_b_nrow"
  b_ncol_var <- "__mojor_gpu_mm_b_ncol"
  out_nrow_var <- "__mojor_gpu_mm_out_nrow"
  out_ncol_var <- "__mojor_gpu_mm_out_ncol"
  lhs_nrow_var <- "__mojor_gpu_mm_lhs_nrow"
  lhs_ncol_var <- "__mojor_gpu_mm_lhs_ncol"
  inner_n_var <- "__mojor_gpu_mm_inner_n"
  b_inner_n_var <- "__mojor_gpu_mm_b_inner_n"
  row_var <- "__mojor_gpu_mm_row"
  col_var <- "__mojor_gpu_mm_col"
  k_var <- "__mojor_gpu_mm_k"
  acc_var <- "__mojor_gpu_mm_acc"

  a_row <- if (ta) .mojor_ir_var(k_var) else .mojor_ir_var(row_var)
  a_col <- if (ta) .mojor_ir_var(row_var) else .mojor_ir_var(k_var)
  b_row <- if (tb) .mojor_ir_var(col_var) else .mojor_ir_var(k_var)
  b_col <- if (tb) .mojor_ir_var(k_var) else .mojor_ir_var(col_var)
  idx0 <- function(expr) .mojor_ir_binop("-", expr, .mojor_ir_const("1"))
  linear_idx <- function(row_expr, col_expr, nrow_name) {
    .mojor_ir_binop(
      "+",
      idx0(row_expr),
      .mojor_ir_binop("*", idx0(col_expr), .mojor_ir_var(nrow_name))
    )
  }
  a_linear <- linear_idx(a_row, a_col, a_nrow_var)
  b_linear <- linear_idx(b_row, b_col, b_nrow_var)
 # Always linearize writes using the destination matrix stride.
  out_linear <- linear_idx(.mojor_ir_var(row_var), .mojor_ir_var(col_var), lhs_nrow_var)

  lhs_type <- if (!is.null(ctx$type_env)) ctx$type_env[[lhs_low$name]] else NULL
  acc_zero <- if (!is.null(lhs_type) && grepl("^i32", lhs_type)) {
    "0"
  } else if (!is.null(lhs_type) && grepl("^f32", lhs_type)) {
    "Float32(0.0)"
  } else {
    "0.0"
  }

  out_nrow_src <- if (ta) .mojor_ir_var(a_ncol_var) else .mojor_ir_var(a_nrow_var)
  out_ncol_src <- if (tb) .mojor_ir_var(b_nrow_var) else .mojor_ir_var(b_ncol_var)
  inner_n_src <- if (ta) .mojor_ir_var(a_nrow_var) else .mojor_ir_var(a_ncol_var)
  b_inner_n_src <- if (tb) .mojor_ir_var(b_ncol_var) else .mojor_ir_var(b_nrow_var)

  k_loop <- .mojor_ir_loop(
    k_var,
    .mojor_ir_range(
      .mojor_ir_const("1"),
      .mojor_ir_binop("+", .mojor_ir_var(inner_n_var), .mojor_ir_const("1")),
      end_exclusive = TRUE
    ),
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var(acc_var),
        .mojor_ir_binop(
          "+",
          .mojor_ir_var(acc_var),
          .mojor_ir_binop(
            "*",
            .mojor_ir_index(.mojor_ir_var(a_low$name), list(a_linear), index_base = "zero_based"),
            .mojor_ir_index(.mojor_ir_var(b_low$name), list(b_linear), index_base = "zero_based")
          )
        )
      )
    ))
  )

  row_loop <- .mojor_ir_loop(
    row_var,
    .mojor_ir_range(
      .mojor_ir_const("1"),
      .mojor_ir_binop("+", .mojor_ir_var(lhs_nrow_var), .mojor_ir_const("1")),
      end_exclusive = TRUE
    ),
    .mojor_ir_block(list(
      .mojor_ir_assign(.mojor_ir_var(acc_var), .mojor_ir_const(acc_zero)),
      k_loop,
      .mojor_ir_assign(
        .mojor_ir_index(
          .mojor_ir_var(lhs_low$name),
          list(out_linear),
          index_base = "zero_based"
        ),
        .mojor_ir_var(acc_var)
      )
    ))
  )

  col_loop <- .mojor_ir_loop(
    col_var,
    .mojor_ir_range(
      .mojor_ir_const("1"),
      .mojor_ir_binop("+", .mojor_ir_var(lhs_ncol_var), .mojor_ir_const("1")),
      end_exclusive = TRUE
    ),
    .mojor_ir_block(list(row_loop))
  )
  dims_ok_cond <- .mojor_ir_binop(
    "&&",
    .mojor_ir_binop(
      "&&",
      .mojor_ir_binop("==", .mojor_ir_var(inner_n_var), .mojor_ir_var(b_inner_n_var)),
      .mojor_ir_binop("==", .mojor_ir_var(lhs_nrow_var), .mojor_ir_var(out_nrow_var))
    ),
    .mojor_ir_binop("==", .mojor_ir_var(lhs_ncol_var), .mojor_ir_var(out_ncol_var))
  )
  guarded_matmul <- .mojor_ir_if(
    cond = dims_ok_cond,
    then = .mojor_ir_block(list(col_loop)),
    else_block = .mojor_ir_block(list())
  )

  .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var(a_nrow_var), .mojor_ir_call("nrow", list(.mojor_ir_var(a_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(a_ncol_var), .mojor_ir_call("ncol", list(.mojor_ir_var(a_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(b_nrow_var), .mojor_ir_call("nrow", list(.mojor_ir_var(b_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(b_ncol_var), .mojor_ir_call("ncol", list(.mojor_ir_var(b_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(lhs_nrow_var), .mojor_ir_call("nrow", list(.mojor_ir_var(lhs_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(lhs_ncol_var), .mojor_ir_call("ncol", list(.mojor_ir_var(lhs_low$name)))),
    .mojor_ir_assign(.mojor_ir_var(out_nrow_var), out_nrow_src),
    .mojor_ir_assign(.mojor_ir_var(out_ncol_var), out_ncol_src),
    .mojor_ir_assign(.mojor_ir_var(inner_n_var), inner_n_src),
    .mojor_ir_assign(.mojor_ir_var(b_inner_n_var), b_inner_n_src),
    guarded_matmul
  ))
}

.mojor_ir_lower_gpu_matmul_preview_return <- function(node, ctx) {
 # Unified-lowering lane: rewrite return(gpu_matmul(...)) using output matrix contract.
  if (is.null(node) || node$kind != "return") {
    return(NULL)
  }
  if (is.null(node$value) || node$value$kind != "gpu_matmul") {
    return(NULL)
  }

  if (!.mojor_ir_gpu_jit_unified_enabled(ctx)) {
    return(NULL)
  }

  out_name <- if (!is.null(ctx$out_name)) as.character(ctx$out_name) else NULL
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }

  assign_node <- .mojor_ir_assign(
    .mojor_ir_var(out_name),
    node$value,
    src = node$src
  )
  lowered_assign <- .mojor_ir_lower_gpu_matmul_preview_assign(assign_node, ctx)
  if (is.null(lowered_assign)) {
    return(NULL)
  }

  .mojor_ir_block(list(
    lowered_assign,
    .mojor_ir_return(.mojor_ir_var(out_name), src = node$src)
  ))
}

.mojor_ir_lower_gpu_matmul_assign <- function(node, ctx) {
 # Lower assign(lhs, gpu_matmul(...)) to an explicit in-place helper call:
 # lhs = _mojor_gpu_matmul_into(lhs, a, b, ta, tb,
 # nrow(a), ncol(a), nrow(b), ncol(b),
 # out_nrow, out_ncol)
  if (is.null(node) || node$kind != "assign") {
    return(NULL)
  }
  if (is.null(node$lhs) || node$lhs$kind != "var") {
    return(NULL)
  }
  if (is.null(node$rhs) || node$rhs$kind != "gpu_matmul") {
    return(NULL)
  }

  lhs_low <- .mojor_ir_lower(node$lhs, ctx)
  if (is.null(lhs_low)) {
    return(NULL)
  }

  rhs <- node$rhs
  a_low <- .mojor_ir_lower(rhs$a, ctx)
  b_low <- .mojor_ir_lower(rhs$b, ctx)
  if (is.null(a_low) || is.null(b_low)) {
    return(NULL)
  }

  ta <- isTRUE(rhs$transpose_a)
  tb <- isTRUE(rhs$transpose_b)
  ta_low <- .mojor_ir_const(if (ta) "True" else "False")
  tb_low <- .mojor_ir_const(if (tb) "True" else "False")

  a_nrow <- .mojor_ir_call("nrow", list(a_low))
  a_ncol <- .mojor_ir_call("ncol", list(a_low))
  b_nrow <- .mojor_ir_call("nrow", list(b_low))
  b_ncol <- .mojor_ir_call("ncol", list(b_low))

 # Effective output shape after optional transpose flags.
  out_nrow <- if (ta) a_ncol else a_nrow
  out_ncol <- if (tb) b_nrow else b_ncol

  into_call <- .mojor_ir_call(
    "_mojor_gpu_matmul_into",
    list(
      lhs_low,
      a_low, b_low,
      ta_low, tb_low,
      a_nrow, a_ncol,
      b_nrow, b_ncol,
      out_nrow, out_ncol
    ),
    src = rhs$src
  )

  .mojor_ir_assign(lhs_low, into_call, src = node$src)
}

.mojor_ir_lower <- function(node, ctx = list(n_var = "n_i", type_env = NULL)) {
 # Top-level dispatcher for HIR <U+2192> LIR lowering
 # Recursively lowers all HIR nodes in the tree
 # ctx: lowering context (n_var, type_env, layout metadata - Step 21)

  if (is.null(node)) {
    return(NULL)
  }
  if (!is.list(node) || is.null(node$kind)) {
    return(node)
  }

  k <- if (exists(".mojor_ir_base_kind", mode = "function")) .mojor_ir_base_kind(node$kind) else node$kind

 # Dispatch HIR nodes to lowering functions
  if (k == "scalar_reduce") {
 # Conditionally defer scalar_reduce lowering to schedule pass when supported.
    reduction_mode <- ctx$reduction_mode
    if (!is.null(reduction_mode) && reduction_mode %in% c("tree", "simd")) {
      op <- node$op
      arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[node$arg]] else NULL
      is_na_rm <- isTRUE(node$na_rm)
      is_tree_sched_op <- !is_na_rm && op %in% c("sum", "product", "min", "max", "which.min", "which.max")
      is_simd_sched_op <- !is_na_rm && op %in% c("sum", "product", "min", "max", "which.min", "which.max")
      if (reduction_mode == "tree" && is_tree_sched_op) {
 # Tree schedule supports associative scalar reductions without na.rm semantics.
        return(node)
      }
      is_simd_dtype_ok <- FALSE
      if (!is.null(arg_type) && is_simd_sched_op) {
        if (op %in% c("sum", "product")) {
          is_simd_dtype_ok <- arg_type %in% c("f64[]", "f32[]", "i32[]")
        } else if (op %in% c("min", "max")) {
          is_simd_dtype_ok <- arg_type %in% c("f64[]", "f32[]")
        } else if (op %in% c("which.min", "which.max")) {
          is_simd_dtype_ok <- arg_type %in% c("f64[]", "f32[]", "i32[]", "lgl[]", "bool[]")
        }
      }
      if (reduction_mode == "simd" && isTRUE(is_simd_dtype_ok)) {
        return(node)
      }
    }
 # Otherwise, lower to linear loop now.
    return(.mojor_ir_lower_scalar_reduce(node, ctx))
  }

 # Step 22: Try lowering subscript slices in assignments
  if (k == "assign") {
    lowered <- .mojor_ir_lower_subscript(node, ctx)
    if (!identical(lowered, node)) {
 # Subscript was lowered - recursively lower the result
      return(.mojor_ir_lower(lowered, ctx))
    }
 # Not a lowerable subscript - fall through to normal recursion
  }

 # Recursively lower structured nodes
  if (k == "block") {
    node$stmts <- lapply(node$stmts, function(s) .mojor_ir_lower(s, ctx))
    return(node)
  } else if (k == "loop") {
    node$body <- .mojor_ir_lower(node$body, ctx)
 # Also lower the range expression if it contains HIR
    node$range <- .mojor_ir_lower(node$range, ctx)
    return(node)
  } else if (k == "if") {
    node$cond <- .mojor_ir_lower(node$cond, ctx)
    node$then <- .mojor_ir_lower(node$then, ctx)
    if (!is.null(node$else_block)) {
      node$else_block <- .mojor_ir_lower(node$else_block, ctx)
    }
    return(node)
  } else if (k == "while") {
    node$cond <- .mojor_ir_lower(node$cond, ctx)
    node$body <- .mojor_ir_lower(node$body, ctx)
    return(node)
  } else if (k == "repeat") {
    node$body <- .mojor_ir_lower(node$body, ctx)
    return(node)
  } else if (k == "assign") {
 # Preview-mode: rewrite gpu_reduce expressions embedded in RHS.
    gpu_reduce_expr <- .mojor_ir_lower_gpu_reduce_expr(
      node$rhs,
      ctx,
      tmp_prefix = "__mojor_gpu_reduce_assign_"
    )
    if (!is.null(gpu_reduce_expr) && length(gpu_reduce_expr$prep_stmts) > 0) {
      lhs_low <- .mojor_ir_lower(node$lhs, ctx)
      if (is.null(lhs_low)) {
        return(NULL)
      }
      rewritten_assign <- .mojor_ir_assign(lhs_low, gpu_reduce_expr$expr, src = node$src)
      return(.mojor_ir_lower(.mojor_ir_block(c(gpu_reduce_expr$prep_stmts, list(rewritten_assign))), ctx))
    }

 # Preview-mode GPU reduce rewrite for helper-free lowering.
    gpu_reduce_assign <- .mojor_ir_lower_gpu_reduce_assign(node, ctx)
    if (!is.null(gpu_reduce_assign)) {
      return(.mojor_ir_lower(gpu_reduce_assign, ctx))
    }
    gpu_matmul_preview_assign <- .mojor_ir_lower_gpu_matmul_preview_assign(node, ctx)
    if (!is.null(gpu_matmul_preview_assign)) {
      return(.mojor_ir_lower(gpu_matmul_preview_assign, ctx))
    }
 # Specialize gpu_matmul assignment to in-place helper call with explicit shapes.
    gpu_matmul_assign <- .mojor_ir_lower_gpu_matmul_assign(node, ctx)
    if (!is.null(gpu_matmul_assign)) {
      return(gpu_matmul_assign)
    }
    node$lhs <- .mojor_ir_lower(node$lhs, ctx)
    node$rhs <- .mojor_ir_lower(node$rhs, ctx)
    return(node)
  }

 # Expression nodes - check for nested HIR
  if (k == "binop") {
    node$lhs <- .mojor_ir_lower(node$lhs, ctx)
    node$rhs <- .mojor_ir_lower(node$rhs, ctx)
    return(node)
  } else if (k == "unop") {
    node$expr <- .mojor_ir_lower(node$expr, ctx)
    return(node)
  } else if (k == "call") {
    node$args <- lapply(node$args, function(a) .mojor_ir_lower(a, ctx))
    return(node)
  } else if (k == "index") {
    node$base <- .mojor_ir_lower(node$base, ctx)
    node$indices <- lapply(node$indices, function(i) .mojor_ir_lower(i, ctx))
    return(node)
  } else if (k == "ifelse") {
    node$cond <- .mojor_ir_lower(node$cond, ctx)
    node$yes <- .mojor_ir_lower(node$yes, ctx)
    node$no <- .mojor_ir_lower(node$no, ctx)
    return(node)
  } else if (k == "cast") {
    node$expr <- .mojor_ir_lower(node$expr, ctx)
    return(node)
  } else if (k == "return") {
    if (!is.null(node$value)) {
      gpu_matmul_return <- .mojor_ir_lower_gpu_matmul_preview_return(node, ctx)
      if (!is.null(gpu_matmul_return)) {
        return(.mojor_ir_lower(gpu_matmul_return, ctx))
      }
      gpu_reduce_expr <- .mojor_ir_lower_gpu_reduce_expr(
        node$value,
        ctx,
        tmp_prefix = "__mojor_gpu_reduce_ret_"
      )
      if (!is.null(gpu_reduce_expr) && length(gpu_reduce_expr$prep_stmts) > 0) {
        rewritten_return <- .mojor_ir_return(gpu_reduce_expr$expr, src = node$src)
        return(.mojor_ir_lower(.mojor_ir_block(c(gpu_reduce_expr$prep_stmts, list(rewritten_return))), ctx))
      }
      node$value <- .mojor_ir_lower(node$value, ctx)
    }
    return(node)
  } else if (k == "range") {
    node$start <- .mojor_ir_lower(node$start, ctx)
    node$end <- .mojor_ir_lower(node$end, ctx)
    if (!is.null(node$step)) {
      node$step <- .mojor_ir_lower(node$step, ctx)
    }
    return(node)
  } else if (k == "scalar_index") {
    node$expr <- .mojor_ir_lower(node$expr, ctx)
    return(node)
  } else if (k == "slice_index") {
    node$start <- .mojor_ir_lower(node$start, ctx)
    node$end <- .mojor_ir_lower(node$end, ctx)
    return(node)
  } else if (k == "subscript") {
    node$indices <- lapply(node$indices, function(i) .mojor_ir_lower(i, ctx))
    return(node)
  } else if (k == "rep") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    if (!is.null(node$times)) node$times <- .mojor_ir_lower(node$times, ctx)
    if (!is.null(node$each)) node$each <- .mojor_ir_lower(node$each, ctx)
    if (!is.null(node$length_out)) node$length_out <- .mojor_ir_lower(node$length_out, ctx)
    return(node)
  } else if (k == "rep_len") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    node$length_out <- .mojor_ir_lower(node$length_out, ctx)
    return(node)
  } else if (k == "c") {
    node$parts <- lapply(node$parts, function(p) .mojor_ir_lower(p, ctx))
    return(node)
  }

 # rng_vec and alloc - pass through unchanged (they are HIR sugar)
 # rng_vec will be lowered by the emit pass when assigned to a subscript
 # alloc will be lowered when used in expression mode
  if (k == "rng_vec") {
 # rng_vec is HIR sugar - pass through for emit pass to handle
 # Lower any nested expressions in params
    if (!is.null(node$params)) {
      node$params <- lapply(node$params, function(p) .mojor_ir_lower(p, ctx))
    }
    return(node)
  }
  if (k == "alloc") {
 # alloc is HIR sugar - pass through for emit pass to handle
    node$len <- .mojor_ir_lower(node$len, ctx)
    return(node)
  }

 # apply() - HIR sugar, pass through for emit pass to handle
  if (k == "apply") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    return(node)
  }

 # Higher-Order Functions.
 # Keep HIR nodes, but lower child expressions so emit can enforce strict v1 subset.
  if (k == "vapply") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    if (is.null(node$fun)) stop("IR lower [vapply]: missing FUN")
    return(node)
  }
  if (k == "sapply") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    if (is.null(node$fun)) stop("IR lower [sapply]: missing FUN")
    return(node)
  }
  if (k == "lapply") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    if (is.null(node$fun)) stop("IR lower [lapply]: missing FUN")
    return(node)
  }
  if (k == "mapply") {
    if (is.null(node$args) || !is.list(node$args)) {
      stop("IR lower [mapply]: args must be a list")
    }
    if (length(node$args) < 2) {
      stop("IR lower [mapply]: requires at least two vector arguments in the compiled subset")
    }
    node$args <- lapply(node$args, function(a) .mojor_ir_lower(a, ctx))
    return(node)
  }

 # String Basics - HIR sugar, pass through for emit pass to handle
  if (k == "nchar") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    return(node)
  }
  if (k == "nzchar") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    return(node)
  }
  if (k == "substr") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    node$start <- .mojor_ir_lower(node$start, ctx)
    node$stop <- .mojor_ir_lower(node$stop, ctx)
    return(node)
  }
  if (k == "paste") {
    node$args <- lapply(node$args, function(a) .mojor_ir_lower(a, ctx))
    return(node)
  }

 # Sampling & Permutation - HIR sugar, pass through for emit pass to handle
  if (k == "sample_int") {
    node$n <- .mojor_ir_lower(node$n, ctx)
    node$size <- .mojor_ir_lower(node$size, ctx)
    if (!is.null(node$replace)) node$replace <- .mojor_ir_lower(node$replace, ctx)
    if (!is.null(node$prob)) node$prob <- .mojor_ir_lower(node$prob, ctx)
    return(node)
  }
  if (k == "sample") {
    node$x <- .mojor_ir_lower(node$x, ctx)
    node$size <- .mojor_ir_lower(node$size, ctx)
    if (!is.null(node$replace)) node$replace <- .mojor_ir_lower(node$replace, ctx)
    if (!is.null(node$prob)) node$prob <- .mojor_ir_lower(node$prob, ctx)
    return(node)
  }

 # vexpr - HIR sugar, lower to alloc + loop + return
  if (k == "vexpr") {
    return(.mojor_ir_lower_vexpr(node, ctx))
  }

 # Step 24: GPU Array nodes
  if (k == "gpu_reduce") {
 # gpu_reduce is HIR sugar - lower to a core call node.
    return(.mojor_ir_lower_gpu_reduce(node, ctx))
  }
  if (k == "gpu_matmul") {
 # gpu_matmul is HIR sugar - lower to a core call node.
    return(.mojor_ir_lower_gpu_matmul(node, ctx))
  }

 # LIR nodes / leaf nodes - pass through unchanged
 # (const, var, break, next, missing_index, range_expr, raw)
  node
}

# =============================================================================
# vexpr Lowering
# =============================================================================

.mojor_ir_lower_vexpr <- function(node, ctx) {
 # node: vexpr HIR node
 # ctx: lowering context

  if (is.null(node) || node$kind != "vexpr") {
    return(node)
  }

  len_expr <- .mojor_ir_lower(node$len, ctx)
  body_expr <- .mojor_ir_lower(node$body, ctx)

 # Get output length - use n_i as default
  len_var <- "n_i"
  if (!is.null(ctx$len_var_map)) {
 # Look up length variable from context
    for (nm in names(ctx$len_var_map)) {
      if (identical(ctx$len_var_map[[nm]], "n_i")) {
        len_var <- nm
        break
      }
    }
  }

 # Determine output type from body expression
  out_dtype <- .mojor_ir_infer_type(body_expr, ctx$type_env)
  if (is.null(out_dtype)) out_dtype <- "f64"

 # Create output array allocation
  out_var <- "__mojor_out"
  alloc_stmt <- .mojor_ir_assign(
    .mojor_ir_var(out_var),
    .mojor_ir_call("alloc", list(.mojor_ir_const(out_dtype), len_expr))
  )

 # Create loop variable
  loop_var <- "i"

 # Build loop body: out[i] <- body
  loop_body <- .mojor_ir_assign(
    .mojor_ir_index(
      .mojor_ir_var(out_var),
      list(.mojor_ir_var(loop_var)),
      index_base = "one_based"
    ),
    body_expr
  )

 # Create loop: for i in range(1, n + 1)
  loop <- .mojor_ir_loop(
    loop_var,
    .mojor_ir_range(
      .mojor_ir_const("1"),
      .mojor_ir_binop("+", len_expr, .mojor_ir_const("1")),
      end_exclusive = TRUE
    ),
    .mojor_ir_block(list(loop_body))
  )

 # Return allocation + loop + return out
  return(.mojor_ir_block(list(alloc_stmt, loop, .mojor_ir_return(.mojor_ir_var(out_var)))))
}

# =============================================================================
# Loop Fusion Integration
# =============================================================================
#
# The fusion pass is called from the transpiler pipeline after lowering.
# It's not integrated into .mojor_ir_lower() directly because:
# - Fusion operates on statement lists, not individual nodes
# - Fusion should be applied after all lowering is complete
# - Fusion is an optimization pass, not a lowering pass
#
# Integration point: In the transpiler pipeline, after .mojor_ir_lower_stmt()
# and before .mojor_ir_stmt_emit().

# =============================================================================
# Step 21: Layout Context
# =============================================================================
#
# Formalizes the dimension/tensor metadata that IR lowering and emission need.
# Replaces scattered .mojor_state global access with explicit parameter threading.
