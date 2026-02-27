.mojor_ir_index_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, out_name = NULL, write_context = FALSE) {
 # Step 5.1: Added type_env parameter for logical array support Step 8.11: Added
 # out_name parameter for matrix output detection PR-B5 Step 4: Handle cumulative
 # operations in indexed expressions
    if (is.null(node) ||
        node$kind != "index") {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

    should_normalize_index <- function(idx_node) {
        if (identical(node$index_base, "one_based")) {
            return(TRUE)
        }
        if (!identical(node$index_base, "zero_based")) {
            return(FALSE)
        }
        if (is.list(idx_node) &&
            isTRUE(idx_node$`__mojor_index_normalized`)) {
            return(FALSE)
        }
        user_origin <- !is.null(node$src)
        if (!exists(".mojor_ir_collect_var_refs", mode = "function", inherits = TRUE)) {
            return(FALSE)
        }
        refs <- unique(.mojor_ir_collect_var_refs(idx_node))
        if (length(refs) >
            0) {
            return(user_origin && !all(refs %in% zero_based_vars))
        }
 # For zero-based mode, literal/no-ref indices from user AST still follow
 # one-based R semantics and must be normalized. Internal lowered zero-based
 # indices are emitted without src metadata and should remain untouched.
        user_origin
    }

 # PR-B5 Step 4: For cumulative operations, the accumulator IS the indexed value
 # cumsum(x)[i] should just return the accumulator variable, not accumulator[i]
    if (!is.null(node$base) &&
        node$base$kind %in% c("cumsum", "cumprod", "cummax", "cummin")) {
 # Just emit the cumulative operation expression - it returns the accumulator
        return(.mojor_ir_expr_emit(node$base, zero_based_vars, type_env, loop_vars))
    }

 # PR-B6: For rep/rep_len with literal c() constructors, the base expression already
 # handles indexing via an if/else chain, so we don't subscript it
    if (!is.null(node$base) &&
        node$base$kind %in% c("rep", "rep_len")) {
 # Check if the base has a literal c() constructor
        has_literal_c <- FALSE
        if (!is.null(node$base$x) &&
            node$base$x$kind %in% c("c", "rep", "rep_len") &&
            !is.null(node$base$x$src)) {
            if (is.call(node$base$x$src) &&
                as.character(node$base$x$src[[1]]) ==
                  "c") {
                parts <- as.list(node$base$x$src)[-1]
 # Filter out named args
                nms <- names(parts)
                if (!is.null(nms)) {
                  named_idx <- which(nms != "")
                  if (length(named_idx) >
                    0)
                    parts <- parts[-named_idx]
                }
                if (length(parts) >
                  0 && length(parts) <=
                  8) {
                  all_literals <- all(
                    vapply(
                      parts, function(p) {
                        is.numeric(p) ||
                          is.integer(p) ||
                          is.logical(p)
                      }, logical(1)
                  )
                )
                  if (all_literals) {
                    has_literal_c <- TRUE
                  }
                }
            }
        }

        if (has_literal_c) {
 # The base expression already handles indexing, don't add subscript
            return(.mojor_ir_expr_emit(node$base, zero_based_vars, type_env, loop_vars))
        }
    }

    base_str <- .mojor_ir_expr_emit(node$base, zero_based_vars, type_env, loop_vars)
    if (is.null(base_str)) {
        return(NULL)
    }

 # PR-B3 Step 3: Handle transpose base for 2D indexing t(x)[i, j] should map to x[j,
 # i] (swap indices) This only works when x has a tensor wrapper (matrix type)
    if (!is.null(node$base) &&
        node$base$kind == "transpose") {
 # Get the underlying expression
        x_node <- node$base$x
        if (x_node$kind == "var") {
 # For 2D indexing on transpose, swap the indices
            if (length(node$indices) ==
                2) {
 # Check if the underlying variable is a matrix (has tensor wrapper)
                x_name <- x_node$name
                tensor_map <- .mojor_state$current_tensor_map
                if (!is.null(tensor_map) &&
                    !is.list(tensor_map)) {
                    tensor_map <- as.list(tensor_map)
                }
                has_tensor <- !is.null(tensor_map) &&
                  !is.null(tensor_map[[x_name]])

                if (has_tensor) {
 # Swap indices: t(x)[i, j] -> x[j, i]
                  swapped_indices <- rev(node$indices)
                  idx_strs <- character(2)
                  for (i in seq_along(swapped_indices)) {
                    idx_node <- swapped_indices[[i]]
                    if (should_normalize_index(idx_node)) {
                      idx_vars <- character(0)
                      if (idx_node$kind == "var") {
                        idx_vars <- idx_node$name
                      }
                      idx_node <- .mojor_ir_normalize_index_expr(idx_node, idx_vars, zero_based_vars)
                    }
                    idx_str <- .mojor_ir_expr_emit(idx_node, zero_based_vars, type_env, loop_vars, index_context = TRUE)
                    if (is.null(idx_str)) {
                      return(NULL)
                    }
                    idx_strs[i] <- idx_str
                  }
 # Emit with swapped indices
                  x_str <- .mojor_ir_expr_emit(x_node, zero_based_vars, type_env, loop_vars)
                  if (is.null(x_str)) {
                    return(NULL)
                  }
                  return(
                    paste0(
                      x_str, "[", paste(idx_strs, collapse = ", "),
                      "]"
                  )
                )
                }
 # For vectors without tensor wrapper, fall through to default handling
 # This will likely fail at code generation since vectors don't support 2D
 # indexing
            }
        }
    }

 # Build index strings first
    idx_strs <- character(length(node$indices))
    for (i in seq_along(node$indices)) {
        idx_raw <- node$indices[[i]]
        idx_node <- .mojor_ir_selector_unwrap_node(idx_raw)

        # R-style negative exclusion in scalar context: x[-k] → first element of exclusion
        # x[-k][1] = x[1] (R 1-based) when k > 1, or x[2] when k == 1
        # In 0-based: x[0] when k_0based != 0, x[1] when k_0based == 0
        if (!is.null(idx_raw$neg_exclusion)) {
            excl_idx <- idx_raw$neg_exclusion  # 0-based exclusion index string
            idx_strs[i] <- paste0("1 if (", excl_idx, ") == 0 else 0")
            next
        }

        # Vector exclusion in scalar context: x[-c(a,b)] → first non-excluded element
        if (!is.null(idx_raw$neg_vec_exclusion)) {
            excl <- idx_raw$neg_vec_exclusion
            if (excl$type == "c" && !isTRUE(excl$is_dynamic)) {
                # First element not in exclusion set (literal indices only)
                # For c(1,2) (1-based) → 0-based {0,1} → first non-excluded is 2
                excl_set <- as.integer(excl$indices)
                first_ok <- 0L
                while (first_ok %in% excl_set) first_ok <- first_ok + 1L
                idx_strs[i] <- as.character(first_ok)
            } else if (excl$type == "c" && isTRUE(excl$is_dynamic)) {
                # Dynamic vector exclusion in scalar context not supported
                stop("Dynamic vector exclusion (e.g., x[-c(a,b)]) in scalar read context is not supported")
            } else if (excl$type == "range") {
                # For -(a:b), first non-excluded element after the range
                end_0 <- excl$end_0
                if (is.null(end_0) && !is.null(excl$end_expr_ast)) {
                    end_ir <- .mojor_ir_expr_build(excl$end_expr_ast)
                    end_expr <- if (!is.null(end_ir)) {
                        .mojor_ir_expr_emit(end_ir, zero_based_vars, type_env, loop_vars, index_context = TRUE)
                    } else {
                        NULL
                    }
                    if (!is.null(end_expr)) {
                        end_0 <- paste0("Int(", end_expr, ")")
                    }
                }
                if (is.null(end_0) || !nzchar(end_0)) {
                    return(NULL)
                }
                idx_strs[i] <- end_0
            } else {
                return(NULL)
            }
            next
        }

        idx_node <- .mojor_ir_selector_unwrap_node(idx_raw)
        if (should_normalize_index(idx_node)) {
            idx_vars <- character(0)
            if (idx_node$kind == "var") {
                idx_vars <- idx_node$name
            }
            idx_node <- .mojor_ir_normalize_index_expr(idx_node, idx_vars, zero_based_vars)
        }
        idx_str <- .mojor_ir_expr_emit(idx_node, zero_based_vars, type_env, loop_vars, index_context = TRUE)
        if (is.null(idx_str)) {
            return(NULL)
        }
        idx_strs[i] <- idx_str
    }

 # Step 5.1/IR-read-helpers: Determine if we should use read helpers for 1D indexing
    use_read_helper <- FALSE
    helper_type <- NULL

    if (!is.null(type_env) &&
        node$base$kind == "var" && length(node$indices) ==
        1) {
        base_type <- type_env[[node$base$name]]
        if (!is.null(base_type)) {
            if (base_type %in% c("lgl[]", "bool[]")) {
                use_read_helper <- TRUE
                helper_type <- "lgl"
            } else if (base_type == "f64[]") {
                use_read_helper <- TRUE
                helper_type <- "f64"
            } else if (base_type == "f32[]") {
                use_read_helper <- TRUE
                helper_type <- "f32"
            } else if (base_type == "i32[]") {
                use_read_helper <- TRUE
                helper_type <- "i32"
            }
        }
    }

    if (isTRUE(write_context)) {
        use_read_helper <- FALSE
    }

    if (use_read_helper && !isTRUE(.mojor_state$current_suppress_len_checks)) {
        if (isTRUE(.mojor_state$current_suppress_read_helpers)) {
            use_read_helper <- FALSE
        }
    }

    if (use_read_helper) {
 # Get the length variable for the array
        base_name <- if (node$base$kind == "var")
            node$base$name else NULL
        len_var <- NULL

        if (!is.null(base_name)) {
            len_var_map <- .mojor_state$current_len_var_map
            n_source_name <- .mojor_state$current_n_source_name

 # Step broadcast_nd: Check len_var_map first, even for n_source_name In
 # broadcast_nd mode, all arrays have explicit length vars for correct bounds
 # checks
            if (!is.null(len_var_map) &&
                !is.null(names(len_var_map)) &&
                base_name %in% names(len_var_map)) {
                len_var <- len_var_map[[base_name]]
            } else if (!is.null(n_source_name) &&
                identical(base_name, n_source_name)) {
                len_var <- "n_i"
            }
        }

 # Fallback to n_i if we can't find the length variable
        if (is.null(len_var))
            len_var <- "n_i"

 # Step broadcast_nd: apply broadcast index before bounds-checked read
        actual_idx <- idx_strs[1]
        broadcast_nd <- isTRUE(.mojor_state$current_broadcast_nd)
        if (broadcast_nd && !is.null(base_name)) {
            dim_var_map <- .mojor_state$current_dim_var_map
            ndim_var_map <- .mojor_state$current_ndim_var_map
            out_dim_var <- .mojor_state$current_out_dim_var
            out_ndim_var <- .mojor_state$current_out_ndim_var
            has_dim_info <- !is.null(dim_var_map) &&
                base_name %in% names(dim_var_map) &&
                !is.null(ndim_var_map) &&
                base_name %in% names(ndim_var_map)
            if (has_dim_info && !is.null(out_dim_var) &&
                !is.null(out_ndim_var)) {
                dim_var <- dim_var_map[base_name]
                ndim_var <- ndim_var_map[base_name]
                actual_idx <- paste0(
                  "_mojor_bcast_index(", idx_strs[1], ", ", out_dim_var, ", ", out_ndim_var,
                  ", ", dim_var, ", ", ndim_var, ")"
              )
            }
        }

        actual_idx_int <- if (grepl("^Int\\(", actual_idx)) {
            actual_idx
        } else {
            paste0("Int(", actual_idx, ")")
        }
        helper_name <- paste0("_mojor_read_", helper_type)
        upper_expr <- paste0("Int(", len_var, ")")

        if (isTRUE(.mojor_state$options$index_bounds)) {
            return(.mojor_ir_read_call(helper_name, base_str, actual_idx_int, upper_expr))
        }

 # In debug mode, pass source location info to read helpers
        if (isTRUE(.mojor_state$debug_mode)) {
 # Get source file name (default to 'kernel.R')
            source_file <- if (!is.null(.mojor_state$source_file))
                .mojor_state$source_file else "kernel.R"

 # Get source line - try node$line, then state estimate, then fallback to 0
            source_line <- 0
            if (!is.null(node$line)) {
                source_line <- node$line
            } else if (!is.null(.mojor_state$current_line_estimate)) {
                source_line <- .mojor_state$current_line_estimate
            }

 # Get variable name for error reporting
            var_name <- if (!is.null(base_name))
                base_name else "array"

            return(
                paste0(
                  helper_name, "(", base_str, ", ", actual_idx_int, ", ", upper_expr,
                  ", \"", var_name, "\", \"", source_file, "\", ", source_line, ")"
              )
            )
        } else {
            return(.mojor_ir_read_call(helper_name, base_str, actual_idx_int, upper_expr))
        }
    }

 # Step 8.11: Use LayoutTensor wrapper names for 2D+ indexing when available
    if (length(idx_strs) >=
        2 && node$base$kind == "var") {
        base_name <- node$base$name
        tensor_map <- .mojor_state$current_tensor_map
        if (!is.null(tensor_map) &&
            !is.list(tensor_map)) {
            tensor_map <- as.list(tensor_map)
        }
        if (!is.null(tensor_map) &&
            !is.null(tensor_map[[base_name]])) {
            tensor_name <- tensor_map[[base_name]]
            idx_expr <- paste0(
                tensor_name, "[", paste(idx_strs, collapse = ", "),
                "]"
            )
 # For immutable tensors, extract scalar using [0] indexing
            tensor_mut_map <- .mojor_state$current_tensor_mut_map
            if (!is.null(tensor_mut_map) &&
                !is.null(tensor_mut_map[[base_name]])) {
                if (identical(tensor_mut_map[[base_name]], "False")) {
                  idx_expr <- paste0(idx_expr, "[0]")
                }
            }
            return(idx_expr)
        }
 # If output matrix/array lacks a tensor wrapper, force fallback
        if (!is.null(out_name) &&
            identical(base_name, out_name) &&
            (isTRUE(.mojor_state$current_out_is_matrix) ||
                isTRUE(.mojor_state$current_out_is_array))) {
            return(NULL)
        }
 # Fallback: linearize multi-dimensional indices for raw pointer-backed arrays.
        nrow_map <- .mojor_state$current_nrow_var_map
        dim_map <- .mojor_state$current_dim_var_map
        nrow_var <- if (!is.null(nrow_map) &&
            !is.null(nrow_map[[base_name]]))
            nrow_map[[base_name]] else NULL
        dim_var <- if (!is.null(dim_map) &&
            !is.null(dim_map[[base_name]]))
            dim_map[[base_name]] else NULL
        linear_idx <- NULL
        if (length(idx_strs) ==
            2 && !is.null(nrow_var)) {
            linear_idx <- paste0("Int((", idx_strs[1], " + (", idx_strs[2], ") * ", nrow_var, "))")
        } else if (!is.null(dim_var)) {
            terms <- character(length(idx_strs))
            terms[1] <- paste0("(", idx_strs[1], ")")
            if (length(idx_strs) >=
                2) {
                for (axis in 2:length(idx_strs)) {
                  stride_parts <- vapply(
                    seq_len(axis - 1L),
                    function(k) {
                      paste0("Int(", dim_var, "[", k - 1L, "])")
                    }, character(1)
                )
                  stride_expr <- if (length(stride_parts) ==
                    1)
                    stride_parts[[1]] else paste0(
                    "(", paste(stride_parts, collapse = " * "),
                    ")"
                )
                  terms[axis] <- paste0("((", idx_strs[axis], ") * ", stride_expr, ")")
                }
            }
            linear_idx <- paste0(
                "Int(", paste(terms, collapse = " + "),
                ")"
            )
        }
        if (!is.null(linear_idx)) {
            return(paste0(base_str, "[", linear_idx, "]"))
        }
    }

 # Step broadcast_nd: Use _mojor_bcast_index for 1D indexing in broadcast_nd mode
    broadcast_nd <- isTRUE(.mojor_state$current_broadcast_nd)
    if (broadcast_nd && length(idx_strs) ==
        1 && node$base$kind == "var") {
        base_name <- node$base$name
        dim_var_map <- .mojor_state$current_dim_var_map
        ndim_var_map <- .mojor_state$current_ndim_var_map
        out_dim_var <- .mojor_state$current_out_dim_var
        out_ndim_var <- .mojor_state$current_out_ndim_var

 # Check if this variable has dimension info (needs broadcasting) Note:
 # dim_var_map is a named vector, not a list, so use single bracket
        has_dim_info <- !is.null(dim_var_map) &&
            base_name %in% names(dim_var_map) &&
            !is.null(ndim_var_map) &&
            base_name %in% names(ndim_var_map)

        if (has_dim_info && !is.null(out_dim_var) &&
            !is.null(out_ndim_var)) {
 # Emit broadcast index call
            dim_var <- dim_var_map[base_name]
            ndim_var <- ndim_var_map[base_name]
            bcast_idx <- paste0(
                "_mojor_bcast_index(", idx_strs[1], ", ", out_dim_var, ", ", out_ndim_var,
                ", ", dim_var, ", ", ndim_var, ")"
            )
            return(paste0(base_str, "[", bcast_idx, "]"))
        }
    }

    paste0(
        base_str, "[", paste(idx_strs, collapse = ", "),
        "]"
    )
}

# Emit a structured range node (kind == 'range') into a Mojo range() string. Handles
# ascending 1-based ranges produced by .mojor_ir_try_build_range(). Returns the same
# list shape as .mojor_ir_simple_range_emit().
