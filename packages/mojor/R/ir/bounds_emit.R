.mojor_ir_emit_bounds_guards <- function(
    node, indent = "    ", zero_based_vars = NULL, bounds_check = TRUE,
    loop_var = NULL, guard_cache = NULL
) {
 # Emit bounds guard checks for array accesses in an expression
 # Returns list(lines = guard_lines, guard_cache = updated_cache)
 # Step 4.4 Optimization: - If loop_var is provided, skip bounds
 # checks for that variable - This is safe when the loop range
 # matches the array length (common case) Step 16: Added
 # guard_cache for CSE - avoids redundant bounds checks

    if (!bounds_check) {
        return(
            list(
                lines = character(0),
                guard_cache = guard_cache
            )
        )
    }

 # Collect all index accesses
    accesses <- .mojor_ir_collect_index_accesses(node, zero_based_vars)
    if (length(accesses) ==
        0) {
        return(
            list(
                lines = character(0),
                guard_cache = guard_cache
            )
        )
    }

    guard_lines <- character(0)
    len_var_map <- .mojor_state$current_len_var_map
    new_cache <- guard_cache

 # For each index access, emit a bounds check
    for (access in accesses) {
        if (is.null(access$base) ||
            is.null(access$indices))
            next

 # Get the base array name
        base_name <- if (access$base$kind == "var")
            access$base$name else NULL
        if (is.null(base_name))
            next  # Skip complex base expressions

 # Handle simple 1D index (single index)
        if (length(access$indices) ==
            1) {
 # Look up length variable for this array (can't use len() on
 # raw pointers)
            base_len_var <- NULL
            if (!is.null(len_var_map) &&
                !is.null(names(len_var_map)) &&
                base_name %in% names(len_var_map)) {
                base_len_var <- len_var_map[[base_name]]
            }
            if (is.null(base_len_var)) {
                if (!is.null(len_var_map))
                    next  # len_var_map present but no entry: raw pointer, skip
                base_len_var <- paste0("len(", base_name, ")")  # No map: fall back to len()
            }

            idx_raw <- access$indices[[1]]
            idx_expr_node <- .mojor_ir_selector_unwrap_node(idx_raw)

 # Skip synthetic marker nodes emitted for exclusion/selection lowering.
 # They are not scalar subscripts and emitting scalar guards for them
 # leaks internal marker names into generated Mojo.
            if (is.null(idx_expr_node) ||
                identical(idx_raw$kind, "missing_index") ||
                !is.null(idx_raw$neg_exclusion) ||
                !is.null(idx_raw$neg_vec_exclusion) ||
                !is.null(idx_raw$pos_vec_selection))
                next

 # Emit the index expression
            idx_str <- .mojor_ir_expr_emit(
                idx_expr_node, zero_based_vars, type_env = NULL, loop_vars = if (!is.null(loop_var))
                  loop_var else character(0),
                index_context = TRUE
            )
            if (is.null(idx_str) ||
                idx_str == "")
                next

 # For simple variables, use the variable name directly
 # For complex expressions, we'll emit them inline (Mojo
 # will optimize)
            is_simple_var <- (idx_expr_node$kind == "var")

            if (is_simple_var) {
                idx_name <- idx_expr_node$name
                arg_specs <- .mojor_state$current_arg_specs

 # If the index symbol is itself an array/mask
 # argument, this is not a scalar subscript and
 # scalar-style bounds checks are invalid.
                if (!is.null(len_var_map) &&
                  !is.null(names(len_var_map)) &&
                  idx_name %in% names(len_var_map)) {
                  next
                }
                if (!is.null(arg_specs) &&
                  !is.null(arg_specs[[idx_name]]) &&
                  .mojor_is_array(arg_specs[[idx_name]])) {
                  next
                }

 # Step 16: Skip if this exact check is already in
 # cache
                check_key <- paste0(idx_name, ":", base_name)
                if (!is.null(new_cache) &&
                  check_key %in% new_cache)
                  next

 # Step 4.4: Skip bounds check for loop variable
 # (known safe)
                if (!is.null(loop_var) &&
                  idx_name %in% loop_var)
                  next

 # Emit bounds check: if idx < 1 || idx >
 # base_len_var:
                check_line <- paste0(
                  indent, "if ", idx_name, " < 1 or ", idx_name, " > Int(",
                  base_len_var, "):"
              )
                error_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                    paste0(indent, "    __mojor_na_flag[0] = Int32(2)")
                } else {
                    paste0(indent, "    _mojor_oob()")
                }
                guard_lines <- c(guard_lines, check_line, error_line)

 # Step 16: Add to cache
                new_cache <- if (is.null(new_cache))
                  check_key else c(new_cache, check_key)
            } else {
 # Complex index expression - emit inline check Build
 # a unique key for caching based on the emitted
 # expression
                check_key <- paste0(idx_str, ":", base_name)
                if (!is.null(new_cache) &&
                  check_key %in% new_cache)
                  next

 # Emit bounds check with the expression inline
                check_line <- paste0(
                  indent, "if ", idx_str, " < 1 or ", idx_str, " > Int(",
                  base_len_var, "):"
              )
                error_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                    paste0(indent, "    __mojor_na_flag[0] = Int32(2)")
                } else {
                    paste0(indent, "    _mojor_oob()")
                }
                guard_lines <- c(guard_lines, check_line, error_line)

 # Add to cache
                new_cache <- if (is.null(new_cache))
                  check_key else c(new_cache, check_key)
            }

            next
        }

 # Handle N-d index using dim pointers when available
        if (length(access$indices) >=
            2) {
            dim_var_map <- .mojor_state$current_dim_var_map
            out_name <- .mojor_state$current_out_name
            dim_var <- NULL
            if (!is.null(out_name) &&
                identical(base_name, out_name) &&
                !is.null(.mojor_state$current_out_dim_var)) {
                dim_var <- .mojor_state$current_out_dim_var
            }
            if (is.null(dim_var) && !is.null(dim_var_map)) {
                dim_var <- dim_var_map[[base_name]]
            }

            nrow_var <- NULL
            ncol_var <- NULL
            if (length(access$indices) == 2) {
                if (!is.null(out_name) &&
                    identical(base_name, out_name)) {
                    nrow_var <- .mojor_state$current_out_nrow_var
                    ncol_var <- .mojor_state$current_out_ncol_var
                }
                if (is.null(nrow_var)) {
                    nrow_map <- .mojor_state$current_nrow_var_map
                    if (!is.null(nrow_map)) {
                        nrow_var <- nrow_map[[base_name]]
                    }
                }
                if (is.null(ncol_var)) {
                    ncol_map <- .mojor_state$current_ncol_var_map
                    if (!is.null(ncol_map)) {
                        ncol_var <- ncol_map[[base_name]]
                    }
                }
            }

 # Only scalar-like dimensions participate in scalar bounds guards.
 # Special index markers (missing dims, exclusion/selectors) are handled by
 # their dedicated lowering paths and must not be emitted as scalar checks.
            conds <- character(0)
            for (axis in seq_along(access$indices)) {
                idx_raw <- access$indices[[axis]]
                idx_expr_node <- .mojor_ir_selector_unwrap_node(idx_raw)
                if (is.null(idx_expr_node) ||
                    identical(idx_raw$kind, "missing_index") ||
                    !is.null(idx_raw$neg_exclusion) ||
                    !is.null(idx_raw$neg_vec_exclusion) ||
                    !is.null(idx_raw$pos_vec_selection)) {
                    next
                }
                idx_expr <- .mojor_ir_expr_emit(
                    idx_expr_node, zero_based_vars, type_env = NULL, loop_vars = loop_var,
                    index_context = TRUE
                )
                if (is.null(idx_expr) || !nzchar(idx_expr)) {
                    next
                }
                upper_expr <- NULL
                if (!is.null(dim_var)) {
                    upper_expr <- paste0("Int(", dim_var, "[", axis - 1L, "])")
                } else if (length(access$indices) == 2) {
                    if (axis == 1 && !is.null(nrow_var)) {
                        upper_expr <- paste0("Int(", nrow_var, ")")
                    } else if (axis == 2 && !is.null(ncol_var)) {
                        upper_expr <- paste0("Int(", ncol_var, ")")
                    }
                }
                if (is.null(upper_expr) || !nzchar(upper_expr)) {
                    next
                }
                conds <- c(
                    conds,
                    paste0(
                        idx_expr, " < 1 or ", idx_expr, " > ", upper_expr
                    )
                )
            }
            if (length(conds) == 0)
                next

 # Step 16: Build check key for N-d and skip if cached
            check_key <- paste0(
                paste(conds, collapse = " || "),
                ":", base_name
            )
            if (!is.null(new_cache) &&
                check_key %in% new_cache)
                next

            check_line <- paste0(
                indent, "if ", paste(conds, collapse = " or "),
                ":"
            )
            error_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                paste0(indent, "    __mojor_na_flag[0] = Int32(2)")
            } else {
                paste0(indent, "    _mojor_oob()")
            }
            guard_lines <- c(guard_lines, check_line, error_line)

 # Step 16: Add to cache
            new_cache <- if (is.null(new_cache))
                check_key else c(new_cache, check_key)
        }
    }

    return(list(lines = guard_lines, guard_cache = new_cache))
}
