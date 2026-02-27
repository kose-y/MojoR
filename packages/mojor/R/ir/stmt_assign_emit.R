# =============================================================================
# IR Statement Assignment Emit
# =============================================================================

.mojor_ir_slice_assign_emit <- function(node, indent, zero_based_vars, out_name, type_env, bounds_check = FALSE) {
 # Emits slice assignment: out[1:n] <- rhs, mat[i, ] <- rhs
 # Supports: vectors (1D), matrix rows (2D with missing index)
 # Limitations: no array RHS cycling Returns NULL
 # <U+2192> legacy fallback for unsupported cases

 # Step 22: Simple lowerable cases should have been handled by
 # .mojor_ir_lower_subscript() If we reach here, it's either: -
 # Complex case not yet lowered (non-1 start, complex RHS) -
 # Fallback from failed lowering - Legacy path before lowering was
 # added

    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) ||
        lhs$kind != "subscript") {
        return(NULL)
    }

    var <- lhs$var
    indices <- lhs$indices
    n_idx <- length(indices)

 # Check if any indices are slices or missing
    has_slice <- FALSE
    for (idx in indices) {
        if (idx$kind %in% c("slice_index", "missing_index")) {
            has_slice <- TRUE
            break
        }
    }
    if (!has_slice) {
        return(NULL)
    }

 # Handle 1D (vector), 2D (matrix), and 3D+ (array) slice
 # assignments
    if (n_idx < 1) {
        return(NULL)
    }

 # Generate loop variables and ranges
    used_names <- c(zero_based_vars, var, out_name)
    loop_vars <- character(0)
    loop_ranges <- character(0)
    loop_dim_exprs <- character(0)
    idx_exprs <- character(0)
    literal_selector_zero_based <- function(idx_node) {
        if (is.null(idx_node) || !is.list(idx_node) || is.null(idx_node$kind)) {
            return(NULL)
        }
        if (identical(idx_node$kind, "vec_index")) {
            sel <- idx_node$pos_vec_selection
            if (!is.null(sel) && identical(sel$type, "c") && !isTRUE(sel$is_dynamic) &&
                !is.null(sel$indices_0) && length(sel$indices_0) > 0L) {
                return(as.character(sel$indices_0))
            }
            return(literal_selector_zero_based(idx_node$expr))
        }
        if (!identical(idx_node$kind, "c") || is.null(idx_node$parts) || !is.list(idx_node$parts) ||
            length(idx_node$parts) == 0L) {
            return(NULL)
        }
        parts <- idx_node$parts
        vals <- character(length(parts))
        for (i in seq_along(parts)) {
            part <- parts[[i]]
            if (is.null(part) || !is.list(part) || !identical(part$kind, "const")) {
                return(NULL)
            }
            num <- suppressWarnings(as.numeric(part$value))
            if (!is.finite(num) || num != floor(num) || num < 1) {
                return(NULL)
            }
            vals[[i]] <- as.character(as.integer(num - 1L))
        }
        vals
    }

    for (k in seq_along(indices)) {
        idx <- indices[[k]]

        if (idx$kind %in% c("vec_index", "c")) {
 # Positive vector selector (literal c(...)) for slice-assignment paths
 # such as out[c(1,3), ] <- val.
            sel_vals <- literal_selector_zero_based(idx)
            if (is.null(sel_vals) || length(sel_vals) == 0L) {
                return(NULL)
            }
            sel_count <- length(sel_vals)
            loop_var <- paste0("__mojor_sel_", k)
            loop_vars <- c(loop_vars, loop_var)
            used_names <- c(used_names, loop_var)
            loop_ranges <- c(loop_ranges, paste0("range(0, ", sel_count, ")"))
            loop_dim_exprs <- c(loop_dim_exprs, as.character(sel_count))
            sel_expr <- sel_vals[[sel_count]]
            if (sel_count > 1L) {
                for (j in seq(sel_count - 1L, 1L, by = -1L)) {
                    sel_expr <- paste0(sel_vals[[j]], " if ", loop_var, " == ", (j - 1L), " else ", sel_expr)
                }
            }
            idx_exprs <- c(idx_exprs, paste0("(", sel_expr, ")"))
        } else if (idx$kind == "slice_index") {
 # Slice: start:end
            if (idx$start$kind != "const") {
                return(NULL)
            }
            start_val <- as.integer(idx$start$value)
            if (is.na(start_val) ||
                start_val < 1) {
                return(NULL)
            }

            end_expr <- .mojor_ir_expr_emit(
                idx$end, zero_based_vars, type_env, loop_vars = loop_vars,
                index_context = TRUE
            )
            if (is.null(end_expr)) {
                return(NULL)
            }

            loop_var <- paste0("__mojor_i", k)
            loop_vars <- c(loop_vars, loop_var)
            used_names <- c(used_names, loop_var)

            start_idx <- start_val - 1
            loop_ranges <- c(loop_ranges, paste0("range(", start_idx, ", Int(", end_expr, "))"))
            extent_expr <- if (start_idx == 0L) {
                paste0("Int(", end_expr, ")")
            } else {
                paste0("(Int(", end_expr, ") - ", start_idx, ")")
            }
            loop_dim_exprs <- c(loop_dim_exprs, extent_expr)
            idx_exprs <- c(idx_exprs, loop_var)
        } else if (idx$kind == "missing_index") {
 # Missing: entire dimension (e.g., mat[i, ] or mat[, j])
            loop_var <- paste0("__mojor_i", k)
            loop_vars <- c(loop_vars, loop_var)
            used_names <- c(used_names, loop_var)

 # Get dimension from .mojor_state
            dim_expr <- NULL
            if (n_idx == 1) {
 # Full vector replacement: out[] <- rhs
                len_var_map <- .mojor_state$current_len_var_map
                n_source_name <- .mojor_state$current_n_source_name
                if (!is.null(out_name) && identical(var, out_name)) {
                    dim_expr <- "n_i"
                } else if (!is.null(len_var_map) && !is.null(names(len_var_map)) && var %in% names(len_var_map)) {
                    dim_expr <- len_var_map[[var]]
                } else if (!is.null(n_source_name) && identical(var, n_source_name)) {
                    dim_expr <- "n_i"
                }
            } else if (k == 1 && n_idx == 2) {
 # First dim of matrix: nrow (mat[, j]) Check if this
 # is the output matrix
                if (!is.null(out_name) &&
                  var == out_name) {
 # Output matrix - use hardcoded nrow variable
                  dim_expr <- "nrow_out_i"
                } else {
 # Input matrix - look up in nrow map
                  nrow_map <- .mojor_state$current_nrow_var_map
                  if (!is.null(nrow_map))
                    dim_expr <- nrow_map[[var]]
                }
            } else if (k == 2 && n_idx == 2) {
 # Second dim of matrix: ncol (mat[i, ]) Check if this
 # is the output matrix
                if (!is.null(out_name) &&
                  var == out_name) {
 # Output matrix - use hardcoded ncol variable
                  dim_expr <- "ncol_out_i"
                } else {
 # Input matrix - look up in ncol map
                  ncol_map <- .mojor_state$current_ncol_var_map
                  if (!is.null(ncol_map))
                    dim_expr <- ncol_map[[var]]
                }
            } else if (n_idx >= 3) {
 # Array: use dim ptr for this axis
                if (!is.null(out_name) &&
                  var == out_name) {
                  out_dim_var <- .mojor_state$current_out_dim_var
                  if (!is.null(out_dim_var)) {
                    dim_expr <- paste0(out_dim_var, "[", k - 1L, "]")
                  }
                }
                if (is.null(dim_expr)) {
                  dim_map <- .mojor_state$current_dim_var_map
                  if (!is.null(dim_map)) {
                    dim_var <- dim_map[[var]]
                    if (!is.null(dim_var)) {
                      dim_expr <- paste0(dim_var, "[", k - 1L, "]")
                    }
                  }
                }
            }
            if (is.null(dim_expr)) {
                return(NULL)
            }

            loop_ranges <- c(loop_ranges, paste0("range(0, Int(", dim_expr, "))"))
            loop_dim_exprs <- c(loop_dim_exprs, paste0("Int(", dim_expr, ")"))
            idx_exprs <- c(idx_exprs, loop_var)
        } else {
 # Scalar index <U+2014> plain IR expression after
 # normalization pass unwraps scalar_index sugar. idx is
 # now a direct IR expression node.
            idx_node <- .mojor_ir_selector_unwrap_node(idx)
            idx_vars <- character(0)
            if (!is.null(idx_node$kind) &&
                identical(idx_node$kind, "var") &&
                !is.null(idx_node$name)) {
                idx_vars <- idx_node$name
            }
            idx_node <- .mojor_ir_normalize_index_expr(
                idx_node,
                idx_vars,
                c(zero_based_vars, loop_vars)
            )
            scalar_expr <- .mojor_ir_expr_emit(
                idx_node, c(zero_based_vars, loop_vars),
                type_env, loop_vars = loop_vars, index_context = TRUE
            )
            if (is.null(scalar_expr)) {
                return(NULL)
            }
            idx_exprs <- c(idx_exprs, scalar_expr)
        }
    }

 # Emit RHS with loop vars available
    rhs_mask_var <- NULL
    rhs_mask_name <- NULL
    slice_starts <- rep(NA_integer_, n_idx)
    for (k in seq_along(indices)) {
        if (indices[[k]]$kind == "slice_index" && !is.null(indices[[k]]$start) &&
            indices[[k]]$start$kind == "const") {
            slice_starts[[k]] <- as.integer(indices[[k]]$start$value)
        }
    }
    lhs_axis_loop_var <- function(axis_idx) {
        lhs_idx <- indices[[axis_idx]]
        if (is.null(lhs_idx) || is.null(lhs_idx$kind)) {
            return(NULL)
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
    build_rhs_index_scalar_expr <- function(rhs_index_node) {
        if (is.null(rhs_index_node) || !is.list(rhs_index_node) || !identical(rhs_index_node$kind, "index")) {
            return(NULL)
        }
        if (is.null(rhs_index_node$base) || !is.list(rhs_index_node$base) || !identical(rhs_index_node$base$kind, "var")) {
            return(NULL)
        }
        if (is.null(rhs_index_node$indices) || !is.list(rhs_index_node$indices) || length(rhs_index_node$indices) == 0L) {
            return(NULL)
        }
        rhs_n_idx <- length(rhs_index_node$indices)
        mapped_indices <- vector("list", rhs_n_idx)
        mapped_index_exprs <- rep(NA_character_, rhs_n_idx)
        lhs_selector_axes <- which(vapply(indices, function(lhs_idx) {
            !is.null(lhs_idx) && !is.null(lhs_idx$kind) &&
                lhs_idx$kind %in% c("slice_index", "missing_index")
        }, logical(1)))
        used_lhs_axes <- integer(0)
        next_lhs_ptr <- 1L
        pick_lhs_axis <- function(rhs_axis) {
            if (rhs_axis <= length(indices)) {
                lhs_idx <- indices[[rhs_axis]]
                if (!is.null(lhs_idx) && !is.null(lhs_idx$kind) &&
                    lhs_idx$kind %in% c("slice_index", "missing_index") &&
                    !(rhs_axis %in% used_lhs_axes)) {
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
        resolve_rhs_axis_extent_expr <- function(base_name, rhs_axis, rhs_idx_node, rhs_rank) {
            if (is.null(rhs_idx_node) || is.null(rhs_idx_node$kind)) {
                return(NULL)
            }
            if (identical(rhs_idx_node$kind, "slice_index")) {
                if (is.null(rhs_idx_node$start) ||
                    !is.list(rhs_idx_node$start) ||
                    !identical(rhs_idx_node$start$kind, "const")) {
                    return(NULL)
                }
                rhs_start_num <- suppressWarnings(as.numeric(rhs_idx_node$start$value))
                if (is.na(rhs_start_num) || !is.finite(rhs_start_num)) {
                    return(NULL)
                }
                rhs_start <- as.integer(rhs_start_num)
                end_expr <- .mojor_ir_expr_emit(
                    rhs_idx_node$end,
                    c(zero_based_vars, loop_vars),
                    type_env,
                    loop_vars = loop_vars,
                    index_context = TRUE
                )
                if (is.null(end_expr) || !nzchar(end_expr)) {
                    return(NULL)
                }
                rhs_start0 <- rhs_start - 1L
                if (rhs_start0 <= 0L) {
                    return(paste0("Int(", end_expr, ")"))
                }
                return(paste0("(Int(", end_expr, ") - ", rhs_start0, ")"))
            }
            if (!identical(rhs_idx_node$kind, "missing_index")) {
                return(NULL)
            }
            if (rhs_rank == 1L) {
                rhs_len_var <- "n_i"
                n_source_name <- .mojor_state$current_n_source_name
                len_var_map <- .mojor_state$current_len_var_map
                if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
                    rhs_len_var <- "n_i"
                } else if (!is.null(len_var_map) &&
                           !is.null(names(len_var_map)) &&
                           base_name %in% names(len_var_map)) {
                    rhs_len_var <- len_var_map[[base_name]]
                }
                return(rhs_len_var)
            }
            if (rhs_rank == 2L) {
                if (rhs_axis == 1L) {
                    nrow_map <- .mojor_state$current_nrow_var_map
                    if (!is.null(nrow_map) && !is.null(nrow_map[[base_name]])) {
                        return(nrow_map[[base_name]])
                    }
                } else if (rhs_axis == 2L) {
                    ncol_map <- .mojor_state$current_ncol_var_map
                    if (!is.null(ncol_map) && !is.null(ncol_map[[base_name]])) {
                        return(ncol_map[[base_name]])
                    }
                }
                dim_map <- .mojor_state$current_dim_var_map
                if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
                    return(paste0(dim_map[[base_name]], "[", rhs_axis - 1L, "]"))
                }
                return(NULL)
            }
            dim_map <- .mojor_state$current_dim_var_map
            if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
                return(paste0(dim_map[[base_name]], "[", rhs_axis - 1L, "]"))
            }
            NULL
        }
        for (axis_idx in seq_len(rhs_n_idx)) {
            rhs_idx <- rhs_index_node$indices[[axis_idx]]
            if (is.null(rhs_idx) || !is.list(rhs_idx) || is.null(rhs_idx$kind)) {
                return(NULL)
            }
            rhs_idx <- .mojor_ir_selector_unwrap_node(rhs_idx)
            if (rhs_idx$kind %in% c("slice_index", "missing_index")) {
                lhs_axis <- pick_lhs_axis(axis_idx)
                if (is.na(lhs_axis)) {
                    return(NULL)
                }
                loop_var <- lhs_axis_loop_var(lhs_axis)
                if (is.null(loop_var)) {
                    return(NULL)
                }
                idx_node <- .mojor_ir_var(loop_var)
                rhs_start <- NA_integer_
                if (identical(rhs_idx$kind, "slice_index")) {
                    if (is.null(rhs_idx$start) || !is.list(rhs_idx$start) || !identical(rhs_idx$start$kind, "const")) {
                        return(NULL)
                    }
                    rhs_start_num <- suppressWarnings(as.numeric(rhs_idx$start$value))
                    if (is.na(rhs_start_num) || !is.finite(rhs_start_num)) {
                        return(NULL)
                    }
                    rhs_start <- as.integer(rhs_start_num)
                } else if (identical(rhs_idx$kind, "missing_index")) {
                    rhs_start <- 1L
                }
                lhs_start <- lhs_axis_start(lhs_axis)
                lhs_start0 <- if (is.na(lhs_start)) 0L else lhs_start - 1L
                rhs_start0 <- if (is.na(rhs_start)) 0L else rhs_start - 1L
                rel_expr <- if (lhs_start0 == 0L) {
                    loop_var
                } else {
                    paste0("(", loop_var, " - ", lhs_start0, ")")
                }
                rhs_extent_expr <- resolve_rhs_axis_extent_expr(
                    rhs_index_node$base$name,
                    axis_idx,
                    rhs_idx,
                    rhs_n_idx
                )
                if (!is.null(rhs_extent_expr) && nzchar(rhs_extent_expr)) {
                    cyc_expr <- paste0("((", rel_expr, ") % Int(", rhs_extent_expr, "))")
                } else {
                    cyc_expr <- rel_expr
                }
                mapped_index_exprs[[axis_idx]] <- if (rhs_start0 == 0L) {
                    cyc_expr
                } else {
                    paste0("(", cyc_expr, " + ", rhs_start0, ")")
                }
                if (!is.na(rhs_start) && !is.na(lhs_start)) {
                    offset <- rhs_start - lhs_start
                    if (!is.na(offset) && offset != 0L) {
                        idx_node <- .mojor_ir_binop("+", idx_node, .mojor_ir_const(as.character(offset)))
                    }
                }
                idx_node$`__mojor_index_normalized` <- TRUE
                mapped_indices[[axis_idx]] <- idx_node
                next
            }
            idx_node <- rhs_idx
            if (!identical(rhs_index_node$index_base, "zero_based")) {
                idx_vars <- character(0)
                if (!is.null(idx_node$kind) && identical(idx_node$kind, "var") && !is.null(idx_node$name)) {
                    idx_vars <- idx_node$name
                }
                idx_node <- .mojor_ir_normalize_index_expr(
                    idx_node,
                    idx_vars,
                    c(zero_based_vars, loop_vars)
                )
            }
            idx_node$`__mojor_index_normalized` <- TRUE
            mapped_indices[[axis_idx]] <- idx_node
        }
        emit_manual_rhs_index_expr <- function(base_name, idx_nodes, idx_expr_overrides = NULL) {
            dims_count <- length(idx_nodes)
            if (is.null(base_name) || !nzchar(base_name) || dims_count == 0L) {
                return(NULL)
            }

            idx_exprs_local <- character(dims_count)
            for (k in seq_len(dims_count)) {
                override_expr <- if (!is.null(idx_expr_overrides) && length(idx_expr_overrides) >= k) {
                    idx_expr_overrides[[k]]
                } else {
                    NA_character_
                }
                if (!is.na(override_expr) && nzchar(override_expr)) {
                    idx_exprs_local[[k]] <- override_expr
                    next
                }
                idx_exprs_local[[k]] <- .mojor_ir_expr_emit(
                    idx_nodes[[k]],
                    c(zero_based_vars, loop_vars),
                    type_env,
                    loop_vars = loop_vars,
                    index_context = TRUE
                )
            }
            if (any(!nzchar(idx_exprs_local))) {
                return(NULL)
            }

            layout_kind_local <- .mojor_state$`%||%`(.mojor_state$current_array_layout, .mojor_state$options$array_layout)
            if (is.null(layout_kind_local)) {
                layout_kind_local <- "col_major"
            }

            if (dims_count == 1) {
                return(paste0(base_name, "[", idx_exprs_local[[1]], "]"))
            }

            if (dims_count == 2) {
                nrow_var <- NULL
                nrow_map <- .mojor_state$current_nrow_var_map
                if (!is.null(nrow_map)) {
                    nrow_var <- nrow_map[[base_name]]
                }
                ncol_var <- NULL
                ncol_map <- .mojor_state$current_ncol_var_map
                if (!is.null(ncol_map)) {
                    ncol_var <- ncol_map[[base_name]]
                }
                if (is.null(nrow_var) || is.null(ncol_var)) {
                    dim_map <- .mojor_state$current_dim_var_map
                    if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
                        if (is.null(nrow_var)) {
                            nrow_var <- paste0(dim_map[[base_name]], "[0]")
                        }
                        if (is.null(ncol_var)) {
                            ncol_var <- paste0(dim_map[[base_name]], "[1]")
                        }
                    }
                }
                if (identical(layout_kind_local, "row_major")) {
                    if (is.null(ncol_var)) {
                        return(NULL)
                    }
                    return(
                        paste0(
                            base_name,
                            "[(",
                            idx_exprs_local[[1]],
                            ") * ",
                            ncol_var,
                            " + ",
                            idx_exprs_local[[2]],
                            "]"
                        )
                    )
                }
                if (is.null(nrow_var)) {
                    return(NULL)
                }
                return(
                    paste0(
                        base_name,
                        "[",
                        idx_exprs_local[[1]],
                        " + (",
                        idx_exprs_local[[2]],
                        ") * ",
                        nrow_var,
                        "]"
                    )
                )
            }

            dim_var <- NULL
            dim_map <- .mojor_state$current_dim_var_map
            if (!is.null(dim_map)) {
                dim_var <- dim_map[[base_name]]
            }
            if (is.null(dim_var)) {
                return(NULL)
            }

            if (identical(layout_kind_local, "row_major")) {
                dim_expr <- function(pos) paste0("Int(", dim_var, "[", pos, "])")
                idx_lin <- NULL
                for (k in seq_len(dims_count)) {
                    if (k < dims_count) {
                        stride_terms <- vapply(k:(dims_count - 1L), dim_expr, character(1))
                        stride_expr <- paste(stride_terms, collapse = " * ")
                        term <- paste0("(", idx_exprs_local[[k]], ") * ", stride_expr)
                    } else {
                        term <- idx_exprs_local[[k]]
                    }
                    if (is.null(idx_lin)) {
                        idx_lin <- term
                    } else {
                        idx_lin <- paste0(idx_lin, " + ", term)
                    }
                }
            } else {
                prod <- paste0("Int(", dim_var, "[0])")
                idx_lin <- idx_exprs_local[[1]]
                if (length(idx_exprs_local) >= 2) {
                    idx_lin <- paste0(idx_lin, " + (", idx_exprs_local[[2]], ") * ", prod)
                }
                if (length(idx_exprs_local) >= 3) {
                    for (k in 3:length(idx_exprs_local)) {
                        prod <- paste0(prod, " * Int(", dim_var, "[", k - 2L, "])")
                        idx_lin <- paste0(idx_lin, " + (", idx_exprs_local[[k]], ") * ", prod)
                    }
                }
            }

            paste0(base_name, "[", idx_lin, "]")
        }

        manual_rhs_expr <- emit_manual_rhs_index_expr(
            rhs_index_node$base$name,
            mapped_indices,
            idx_expr_overrides = mapped_index_exprs
        )
        if (!is.null(manual_rhs_expr)) {
            return(manual_rhs_expr)
        }

        rhs_scalar_node <- .mojor_ir_index(
            rhs_index_node$base,
            mapped_indices,
            index_base = "zero_based",
            src = rhs_index_node$src
        )
        .mojor_ir_index_emit(
            rhs_scalar_node,
            zero_based_vars = c(zero_based_vars, loop_vars),
            type_env = type_env,
            loop_vars = loop_vars,
            out_name = out_name,
            write_context = FALSE
        )
    }
    rhs_index_node <- rhs
    if (!is.null(rhs_index_node) &&
        identical(rhs_index_node$kind, "cast") &&
        !is.null(rhs_index_node$expr) &&
        is.list(rhs_index_node$expr)) {
        rhs_index_node <- rhs_index_node$expr
    }
    if (!is.null(type_env) &&
        !is.null(rhs_index_node) &&
        identical(rhs_index_node$kind, "index")) {
        if (!is.null(rhs_index_node$base) &&
            identical(rhs_index_node$base$kind, "var") &&
            length(rhs_index_node$indices) ==
                1 && !is.null(rhs_index_node$indices[[1]]) &&
            identical(rhs_index_node$indices[[1]]$kind, "var")) {
            idx_name <- rhs_index_node$indices[[1]]$name
            idx_type <- type_env[[idx_name]]
            if (!is.null(idx_type) &&
                .mojor_is_logical_mask_type(idx_type)) {
                rhs_mask_var <- rhs_index_node$base$name
                rhs_mask_name <- idx_name
            }
        }
    }

    prev_slice_extent_map <- .mojor_state$current_slice_loop_extent_map
    on.exit({
        .mojor_state$current_slice_loop_extent_map <- prev_slice_extent_map
    }, add = TRUE)
    if (length(loop_vars) > 0 &&
        length(loop_dim_exprs) == length(loop_vars)) {
        .mojor_state$current_slice_loop_extent_map <- as.list(stats::setNames(loop_dim_exprs, loop_vars))
    } else {
        .mojor_state$current_slice_loop_extent_map <- NULL
    }

 # Special case: RHS is mask extraction (x[mask]) for vector slice
 # assignment. NOTE (Issue 7): Mask extraction only detected when
 # the RHS index is a bare var node typed as lgl[]/bool[].
 # Expressions like x[mask & other], x[which(mask)], or x[mask[i]]
 # are NOT detected here and fall back to legacy. A dedicated
 # .mojor_ir_is_logical_mask_expr() predicate would be needed for
 # broader coverage.
    if (!is.null(rhs_mask_var) &&
        !is.null(rhs_mask_name)) {
        if (n_idx != 1 || length(indices) !=
            1 || indices[[1]]$kind != "slice_index") {
            return(NULL)
        }
        slice_idx <- indices[[1]]
        if (slice_idx$start$kind != "const") {
            return(NULL)
        }
        start_val <- as.integer(slice_idx$start$value)
        if (is.na(start_val) ||
            start_val < 1) {
            return(NULL)
        }
        end_expr <- .mojor_ir_expr_emit(
            slice_idx$end, zero_based_vars, type_env, loop_vars = loop_vars,
            index_context = TRUE
        )
        if (is.null(end_expr)) {
            return(NULL)
        }

 # Add recycle warning for compressed RHS
        end_expr_ast <- slice_idx$end_expr_ast
        end_expr_c <- NULL
        if (!is.null(end_expr_ast) &&
            !is.null(.mojor_state$current_args) &&
            !is.null(.mojor_state$current_arg_specs)) {
            end_expr_c <- tryCatch(
                .mojor_dim_expr_to_c(
                  end_expr_ast, .mojor_state$current_args, .mojor_state$current_arg_specs,
                  len_var_map = .mojor_state$current_len_var_map, n_source_name = .mojor_state$current_n_source_name
              )$c_expr,
                error = function(e) NULL
            )
        }
        if (is.null(end_expr_c))
            end_expr_c <- end_expr
        .mojor_add_recycle_warning(
            list(
                kind = "slice_mask", mask = rhs_mask_name, start = start_val,
                end_expr = end_expr, end_expr_c = end_expr_c, out = var
            )
        )

        used_names <- unique(c(zero_based_vars, var, out_name, rhs_mask_var, rhs_mask_name))
        count_j <- .mojor_unique_loop_var("__mojor_mask_j", used_names)
        assign_j <- .mojor_unique_loop_var("__mojor_j", used_names)
        rhs_scan <- .mojor_unique_loop_var("__mojor_rhs_scan", used_names)
        rhs_count <- .mojor_unique_loop_var("__mojor_rhs_count", used_names)
        slice_len_var <- .mojor_unique_loop_var("__mojor_len", used_names)
        end_var <- .mojor_unique_loop_var("__mojor_end", used_names)

        upper_mask <- "n_i"
        if (!is.null(.mojor_state$current_len_var_map) &&
            rhs_mask_name %in% names(.mojor_state$current_len_var_map)) {
            upper_mask <- .mojor_state$current_len_var_map[[rhs_mask_name]]
        } else if (!is.null(.mojor_state$current_len_hint[[rhs_mask_name]])) {
            upper_mask <- .mojor_state$current_len_hint[[rhs_mask_name]]
        }

        start_idx <- start_val - 1L
        pre_lines <- c(
            paste0(indent, "var ", rhs_count, ": Int = 0"),
            paste0(
                indent, "for ", count_j, " in range(Int(", upper_mask,
                ")):"
            ),
            paste0(
                indent, "    if ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    count_j,
                    paste0("Int(", upper_mask, ")")
                ),
                " == 1:"
            ),
            paste0(indent, "        ", rhs_count, " += 1"),
            paste0(indent, "var ", end_var, " = Int(", end_expr, ")"),
            paste0(
                indent, "var ", slice_len_var, " = (", end_var, " - ",
                start_idx, ")"
            ),
            paste0(indent, "if ", slice_len_var, " < 0:"),
            paste0(indent, "    ", slice_len_var, " = 0")
        )

        loop_prefix <- paste0(indent, "if ", rhs_count, " > 0 and ", slice_len_var, " > 0:")
        loop_indent <- paste0(indent, "    ")
        inner_indent <- paste0(loop_indent, "    ")
        assign_lines <- c(
            paste0(
                inner_indent, "while ", rhs_scan, " < ", upper_mask, " and ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    rhs_scan,
                    paste0("Int(", upper_mask, ")")
                ),
                " != 1:"
            ),
            paste0(inner_indent, "    ", rhs_scan, " += 1"),
            paste0(inner_indent, "if ", rhs_scan, " >= ", upper_mask, ":"),
            paste0(inner_indent, "    ", rhs_scan, " = 0"),
            paste0(
                inner_indent, "    while ", rhs_scan, " < ", upper_mask,
                " and ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    rhs_scan,
                    paste0("Int(", upper_mask, ")")
                ),
                " != 1:"
            ),
            paste0(inner_indent, "        ", rhs_scan, " += 1"),
            paste0(
                inner_indent, "    if ", rhs_scan, " >= ", upper_mask,
                ":"
            ),
            paste0(inner_indent, "        break"),
            paste0(
                inner_indent, var, "[", assign_j, " + ", start_idx, "] = ",
                rhs_mask_var, "[", rhs_scan, "]"
            ),
            paste0(inner_indent, rhs_scan, " += 1")
        )

        loop_lines <- c(
            loop_prefix, paste0(loop_indent, "var ", rhs_scan, ": Int = 0"),
            paste0(
                loop_indent, "for ", assign_j, " in range(", slice_len_var,
                "):"
            ),
            assign_lines
        )
        return(c(pre_lines, loop_lines))
    }

    rhs_expr <- NULL
    if (!is.null(rhs) && identical(rhs$kind, "index")) {
        rhs_expr <- build_rhs_index_scalar_expr(rhs)
    }
    if (!is.null(rhs) &&
        identical(rhs$kind, "index") &&
        is.null(rhs_expr) &&
        n_idx == 1 && length(rhs$indices) ==
        1 && indices[[1]]$kind == "slice_index" && !is.null(slice_starts[[1]]) &&
        !is.null(rhs$base) &&
        rhs$base$kind == "var") {
        rhs_idx <- rhs$indices[[1]]
        rhs_start_val <- NA_integer_
        if (identical(rhs_idx$kind, "slice_index") &&
            !is.null(rhs_idx$start) &&
            is.list(rhs_idx$start) &&
            identical(rhs_idx$start$kind, "const")) {
            rhs_start_num <- suppressWarnings(as.numeric(rhs_idx$start$value))
            if (!is.na(rhs_start_num))
                rhs_start_val <- as.integer(rhs_start_num)
        } else if (identical(rhs_idx$kind, "raw") &&
            is.call(rhs_idx$expr) &&
            as.character(rhs_idx$expr[[1]]) ==
                ":" && length(rhs_idx$expr) ==
            3) {
            rhs_start <- rhs_idx$expr[[2]]
            if ((is.numeric(rhs_start) ||
                is.integer(rhs_start)) &&
                length(rhs_start) ==
                  1) {
                rhs_start_val <- as.integer(rhs_start)
            }
        }
        lhs_start_val <- as.integer(slice_starts[[1]])
        if (!is.na(rhs_start_val) &&
            !is.na(lhs_start_val)) {
            offset <- rhs_start_val - lhs_start_val
            idx_expr <- idx_exprs[[1]]
            if (!is.na(offset) &&
                offset != 0L) {
                idx_expr <- paste0("(", idx_expr, " + ", offset, ")")
            }
            base_str <- .mojor_ir_expr_emit(rhs$base, zero_based_vars, type_env, loop_vars = loop_vars)
            if (!is.null(base_str)) {
                rhs_expr <- paste0(base_str, "[", idx_expr, "]")
            }
        }
    }
    if (is.null(rhs_expr)) {
        rhs_emit <- rhs
 # Rewrite any RHS slice_index nodes to scalar reads using LHS loop vars
        rhs_emit <- .mojor_ir_rewrite_rhs_slices(rhs_emit, indices, n_idx,
            lhs_axis_loop_var, lhs_axis_start,
            rhs_zero_based_vars = c(zero_based_vars, loop_vars))
        if (!is.null(rhs_emit) &&
            !is.null(rhs_emit$kind) &&
            identical(rhs_emit$kind, "c")) {
            rhs_emit[["__mojor_slice_loop_vars"]] <- loop_vars
            rhs_emit[["__mojor_slice_dim_exprs"]] <- loop_dim_exprs
        }
        rhs_expr <- .mojor_ir_expr_emit(
            rhs_emit, c(zero_based_vars, loop_vars),
            type_env, loop_vars = loop_vars
        )
    }
    if (is.null(rhs_expr)) {
        return(NULL)
    }

    rhs_array_var_name <- NULL
    rhs_array_cast_to <- NULL
    rhs_array_type <- NULL
    if (!is.null(rhs) &&
        !is.null(rhs$kind) &&
        !is.null(type_env)) {
        if (identical(rhs$kind, "var")) {
            rhs_array_var_name <- rhs$name
            rhs_array_type <- type_env[[rhs_array_var_name]]
        } else if (identical(rhs$kind, "cast") &&
                   !is.null(rhs$expr) &&
                   is.list(rhs$expr) &&
                   identical(rhs$expr$kind, "var")) {
            rhs_array_var_name <- rhs$expr$name
            rhs_array_cast_to <- rhs$to
            rhs_array_type <- type_env[[rhs_array_var_name]]
        }
    }

 # If RHS is an array variable and we have slice loops, compute a
 # linear slice position (column-major) and recycle RHS with
 # modulo.
    if (!is.null(rhs_array_var_name) && length(loop_vars) > 0 &&
        .mojor_type_is_array(rhs_array_type)) {
            rhs_len_var <- "n_i"
            len_var_map <- .mojor_state$current_len_var_map
            n_source_name <- .mojor_state$current_n_source_name
            if (!is.null(n_source_name) &&
                identical(rhs_array_var_name, n_source_name)) {
                rhs_len_var <- "n_i"
            } else if (!is.null(len_var_map) &&
                !is.null(names(len_var_map)) &&
                rhs_array_var_name %in% names(len_var_map)) {
                rhs_len_var <- len_var_map[[rhs_array_var_name]]
            }

            loop_starts <- integer(0)
            loop_extents <- character(0)
            loop_pos <- 0L
            for (k in seq_along(indices)) {
                idx <- indices[[k]]
                if (idx$kind == "slice_index") {
                  loop_pos <- loop_pos + 1L
                  start_val <- if (!is.null(idx$start) &&
                    idx$start$kind == "const")
                    as.integer(idx$start$value) else 1L
                  if (is.na(start_val) ||
                    start_val < 1L)
                    start_val <- 1L
                  end_expr <- .mojor_ir_expr_emit(
                    idx$end, zero_based_vars, type_env, loop_vars = loop_vars,
                    index_context = TRUE
                )
                  if (is.null(end_expr)) {
                    return(NULL)
                  }
                  start_idx <- start_val - 1L
                  extent <- if (start_idx == 0L) {
                    paste0("Int(", end_expr, ")")
                  } else {
                    paste0("(Int(", end_expr, ") - ", start_idx, ")")
                  }
                  loop_starts <- c(loop_starts, start_idx)
                  loop_extents <- c(loop_extents, extent)
                } else if (idx$kind == "missing_index") {
                  loop_pos <- loop_pos + 1L
                  loop_starts <- c(loop_starts, 0L)
                  dim_expr <- sub("^range\\(0,\\s*Int\\((.*)\\)\\)$", "\\1", loop_ranges[[loop_pos]])
                  loop_extents <- c(loop_extents, paste0("Int(", dim_expr, ")"))
                }
            }
            if (length(loop_vars) !=
                length(loop_extents)) {
                return(NULL)
            }

            lin_idx <- NULL
            stride <- NULL
            for (j in seq_along(loop_vars)) {
                rel <- if (loop_starts[[j]] == 0L) {
                  loop_vars[[j]]
                } else {
                  paste0("(", loop_vars[[j]], " - ", loop_starts[[j]], ")")
                }
                if (is.null(lin_idx)) {
                  lin_idx <- rel
                  stride <- loop_extents[[j]]
                } else {
                  lin_idx <- paste0(lin_idx, " + (", rel, ") * ", stride)
                  stride <- paste0(stride, " * ", loop_extents[[j]])
                }
            }
            rhs_elem_expr <- paste0(rhs_array_var_name, "[(", lin_idx, ") % ", rhs_len_var, "]")
            if (!is.null(rhs_array_cast_to) &&
                is.character(rhs_array_cast_to) &&
                nzchar(rhs_array_cast_to)) {
                mojo_cast <- .mojor_r_to_mojo_type(rhs_array_cast_to)
                rhs_expr <- paste0(mojo_cast, "(", rhs_elem_expr, ")")
            } else {
                rhs_expr <- rhs_elem_expr
            }
    }

    # Defensive scalarization: never emit a bare array pointer as scalar RHS.
    if (!is.null(rhs_array_var_name) &&
        .mojor_type_is_array(rhs_array_type) &&
        (identical(rhs_expr, rhs_array_var_name) ||
         (!is.null(rhs_array_cast_to) &&
          identical(rhs_expr, paste0(.mojor_r_to_mojo_type(rhs_array_cast_to), "(", rhs_array_var_name, ")"))))) {
        if (length(loop_vars) == 0) {
            return(NULL)
        }
        elem_type <- .mojor_type_elem(rhs_array_type)
        read_helper <- switch(elem_type,
            "f64" = "_mojor_read_f64",
            "f32" = "_mojor_read_f32",
            "i32" = "_mojor_read_i32",
            "lgl" = "_mojor_read_lgl",
            "bool" = "_mojor_read_lgl",
            NULL
        )
        if (is.null(read_helper)) {
            return(NULL)
        }
        rhs_len_var <- "n_i"
        len_var_map <- .mojor_state$current_len_var_map
        n_source_name <- .mojor_state$current_n_source_name
        if (!is.null(n_source_name) && identical(rhs_array_var_name, n_source_name)) {
            rhs_len_var <- "n_i"
        } else if (!is.null(len_var_map) &&
                   !is.null(names(len_var_map)) &&
                   rhs_array_var_name %in% names(len_var_map)) {
            rhs_len_var <- len_var_map[[rhs_array_var_name]]
        }
        rhs_fallback <- .mojor_ir_read_call(
            read_helper,
            rhs_array_var_name,
            loop_vars[[1]],
            paste0("Int(", rhs_len_var, ")")
        )
        if (!is.null(rhs_array_cast_to) &&
            is.character(rhs_array_cast_to) &&
            nzchar(rhs_array_cast_to)) {
            rhs_fallback <- paste0(.mojor_r_to_mojo_type(rhs_array_cast_to), "(", rhs_fallback, ")")
        }
        rhs_expr <- rhs_fallback
    }

    if (any(grepl("__pos_vec_sel__", c(idx_exprs, rhs_expr), fixed = TRUE))) {
        stop("IR emit [slice_assign]: unresolved __pos_vec_sel__ marker after lowering")
    }

 # Build LHS index
    layout_kind <- .mojor_state$`%||%`(.mojor_state$current_array_layout, .mojor_state$options$array_layout)
    if (is.null(layout_kind))
        layout_kind <- "col_major"
    lhs_target <- NULL
    lhs_axis_uppers <- rep(NA_character_, n_idx)
    resolve_len_var <- function(base_name) {
        len_var_map <- .mojor_state$current_len_var_map
        n_source_name <- .mojor_state$current_n_source_name
        if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
            return("n_i")
        }
        if (!is.null(len_var_map) &&
            !is.null(names(len_var_map)) &&
            base_name %in% names(len_var_map)) {
            return(len_var_map[[base_name]])
        }
        "n_i"
    }
    if (n_idx == 1) {
 # Vector
        lhs_target <- paste0(var, "[", idx_exprs[[1]], "]")
        lhs_axis_uppers[[1]] <- resolve_len_var(var)
    } else if (n_idx == 2) {
        nrow_var <- .mojor_state$current_out_nrow_var
        if (is.null(nrow_var)) {
            nrow_map <- .mojor_state$current_nrow_var_map
            if (!is.null(nrow_map))
                nrow_var <- nrow_map[[var]]
        }
        ncol_var <- .mojor_state$current_out_ncol_var
        if (is.null(ncol_var)) {
            ncol_map <- .mojor_state$current_ncol_var_map
            if (!is.null(ncol_map))
                ncol_var <- ncol_map[[var]]
        }
        if (identical(layout_kind, "row_major")) {
            if (is.null(ncol_var)) {
                return(NULL)
            }
            lhs_target <- paste0(
                var, "[(", idx_exprs[[1]], ") * ", ncol_var, " + ", idx_exprs[[2]],
                "]"
            )
        } else {
            if (is.null(nrow_var)) {
                return(NULL)
            }
            lhs_target <- paste0(
                var, "[", idx_exprs[[1]], " + (", idx_exprs[[2]], ") * ",
                nrow_var, "]"
            )
        }
        lhs_axis_uppers[[1]] <- nrow_var
        lhs_axis_uppers[[2]] <- ncol_var
    } else if (n_idx >= 3) {
 # Array: prefer LayoutTensor indexing, fall back to linear
 # indexing if needed
        tensor_map <- .mojor_state$current_tensor_map
        if (!is.null(tensor_map) &&
            !is.list(tensor_map)) {
            tensor_map <- as.list(tensor_map)
        }
        tensor_name <- NULL
        if (!is.null(tensor_map))
            tensor_name <- tensor_map[[var]]
        dim_var <- NULL
        if (!is.null(out_name) &&
            var == out_name) {
            dim_var <- .mojor_state$current_out_dim_var
        }
        if (is.null(dim_var)) {
            dim_map <- .mojor_state$current_dim_var_map
            if (!is.null(dim_map)) {
                dim_var <- dim_map[[var]]
            }
        }
        if (!is.null(tensor_name)) {
            lhs_target <- paste0(
                tensor_name, "[", paste(idx_exprs, collapse = ", "),
                "]"
            )
        } else {
            if (is.null(dim_var)) {
                return(NULL)
            }
            if (identical(layout_kind, "row_major")) {
                dims_count <- length(idx_exprs)
                dim_expr <- function(pos) paste0("Int(", dim_var, "[", pos, "])")
                idx_lin <- NULL
                for (k in seq_len(dims_count)) {
                  if (k < dims_count) {
                    stride_terms <- vapply(k:(dims_count - 1L), dim_expr, character(1))
                    stride_expr <- paste(stride_terms, collapse = " * ")
                    term <- paste0("(", idx_exprs[[k]], ") * ", stride_expr)
                  } else {
                    term <- idx_exprs[[k]]
                  }
                  if (is.null(idx_lin)) {
                    idx_lin <- term
                  } else {
                    idx_lin <- paste0(idx_lin, " + ", term)
                  }
                }
            } else {
                prod <- paste0("Int(", dim_var, "[0])")
                idx_lin <- idx_exprs[[1]]
                if (length(idx_exprs) >=
                  2) {
                  idx_lin <- paste0(idx_lin, " + (", idx_exprs[[2]], ") * ", prod)
                }
                if (length(idx_exprs) >=
                  3) {
                  for (k in 3:length(idx_exprs)) {
                    prod <- paste0(prod, " * Int(", dim_var, "[", k - 2L, "])")
                    idx_lin <- paste0(idx_lin, " + (", idx_exprs[[k]], ") * ", prod)
                  }
                }
            }
            lhs_target <- paste0(var, "[", idx_lin, "]")
        }
        if (!is.null(dim_var)) {
            for (k in seq_len(n_idx)) {
                lhs_axis_uppers[[k]] <- paste0(dim_var, "[", k - 1L, "]")
            }
        }
    }
    if (is.null(lhs_target)) {
        return(NULL)
    }

    needs_rhs_snapshot <- FALSE
    if (length(loop_vars) > 0L &&
        !is.null(rhs) &&
        isTRUE(.mojor_ir_references_var(rhs, var))) {
        needs_rhs_snapshot <- TRUE
    }

    snapshot_lines <- character(0)
    snapshot_read_var <- NULL
    if (isTRUE(needs_rhs_snapshot) && length(loop_vars) > 0L) {
        used_names <- unique(c(
            var,
            loop_vars,
            if (!is.null(type_env)) names(type_env) else character(0),
            if (!is.null(zero_based_vars)) zero_based_vars else character(0)
        ))
        snapshot_len_var <- .mojor_unique_loop_var("__mojor_rhs_snapshot_len", used_names)
        used_names <- c(used_names, snapshot_len_var)
        snapshot_var <- .mojor_unique_loop_var("__mojor_rhs_snapshot", used_names)
        used_names <- c(used_names, snapshot_var)
        snapshot_write_var <- .mojor_unique_loop_var("__mojor_rhs_snapshot_w", used_names)
        used_names <- c(used_names, snapshot_write_var)
        snapshot_read_var <- .mojor_unique_loop_var("__mojor_rhs_snapshot_r", used_names)

        snap_len_expr <- if (length(loop_dim_exprs) == 0L) {
            "Int(0)"
        } else if (length(loop_dim_exprs) == 1L) {
            loop_dim_exprs[[1L]]
        } else {
            paste0("(", paste(loop_dim_exprs, collapse = " * "), ")")
        }
        snap_mojo_type <- .mojor_type_env_to_mojo_type(var, type_env)
        if (is.null(snap_mojo_type) &&
            !is.null(rhs_array_type)) {
            snap_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(rhs_array_type))
        }
        if (is.null(snap_mojo_type) || !nzchar(snap_mojo_type)) {
            snap_mojo_type <- "Float64"
        }

        snapshot_lines <- c(
            paste0(indent, "var ", snapshot_len_var, " = Int(", snap_len_expr, ")"),
            paste0(indent, "if ", snapshot_len_var, " < 0:"),
            paste0(indent, "    ", snapshot_len_var, " = 0"),
            paste0(indent, "var ", snapshot_var, " = alloc[", snap_mojo_type, "](", snapshot_len_var, ")"),
            paste0(indent, "var ", snapshot_write_var, ": Int = 0")
        )
        snapshot_loop_indent <- indent
        for (k in seq_along(loop_vars)) {
            snapshot_lines <- c(
                snapshot_lines,
                paste0(
                    snapshot_loop_indent, "for ", loop_vars[[k]], " in ", loop_ranges[[k]], ":"
                )
            )
            snapshot_loop_indent <- paste0(snapshot_loop_indent, "    ")
        }
        snapshot_lines <- c(
            snapshot_lines,
            paste0(snapshot_loop_indent, "if ", snapshot_write_var, " < ", snapshot_len_var, ":"),
            paste0(snapshot_loop_indent, "    ", snapshot_var, "[", snapshot_write_var, "] = ", rhs_expr),
            paste0(snapshot_loop_indent, "    ", snapshot_write_var, " += 1"),
            paste0(indent, "var ", snapshot_read_var, ": Int = 0")
        )

        rhs_expr <- paste0(snapshot_var, "[", snapshot_read_var, "]")
    }

 # Assemble loops
    lines <- snapshot_lines
    loop_indent <- indent
    for (k in seq_along(loop_vars)) {
        lines <- c(
            lines, paste0(
                loop_indent, "for ", loop_vars[[k]], " in ", loop_ranges[[k]],
                ":"
            )
        )
        loop_indent <- paste0(loop_indent, "    ")
    }
    write_line <- paste0(loop_indent, lhs_target, " = ", rhs_expr)
    if (isTRUE(bounds_check)) {
        lhs_oob_conds <- character(0)
        for (k in seq_len(n_idx)) {
            upper <- lhs_axis_uppers[[k]]
            if (is.null(upper) || is.na(upper) || !nzchar(upper)) {
                next
            }
            lhs_oob_conds <- c(
                lhs_oob_conds,
                paste0(
                    "(", idx_exprs[[k]], ") < 0 or (", idx_exprs[[k]], ") >= Int(",
                    upper, ")"
                )
            )
        }
        lhs_oob_conds <- unique(lhs_oob_conds)
        if (length(lhs_oob_conds) > 0) {
            oob_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                paste0(loop_indent, "    __mojor_na_flag[0] = Int32(2)")
            } else {
                paste0(loop_indent, "    _mojor_oob()")
            }
            lines <- c(
                lines,
                paste0(loop_indent, "if ", paste(lhs_oob_conds, collapse = " or "), ":"),
                oob_line,
                paste0(loop_indent, "else:"),
                paste0(loop_indent, "    ", lhs_target, " = ", rhs_expr)
            )
        } else {
            lines <- c(lines, write_line)
        }
    } else {
        lines <- c(lines, write_line)
    }
    if (!is.null(snapshot_read_var) && nzchar(snapshot_read_var)) {
        lines <- c(lines, paste0(loop_indent, snapshot_read_var, " += 1"))
    }

    lines
}

.mojor_ir_mask_assign_emit <- function(node, indent, zero_based_vars, type_env, loop_var) {
 # Step 8.10: Emit mask assignment: out[mask] <- rhs Lowers to:
 # for j in range(out_len): if mask[j]: out[j] = rhs Uses
 # _mojor_read_lgl for safe mask reads and skips NA/false entries

    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) ||
        lhs$kind != "index") {
        return(NULL)
    }
    if (length(lhs$indices) !=
        1)
        {
            return(NULL)
        }  # Only 1D for now

    mask_idx <- lhs$indices[[1]]
    if (mask_idx$kind != "var") {
        return(NULL)
    }

    mask_name <- mask_idx$name
    base_name <- NULL
    if (lhs$base$kind == "var") {
        base_name <- lhs$base$name
    } else {
        return(NULL)
    }

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

 # Mask length (used for safe read bounds)
    mask_len_var <- NULL
    if (!is.null(n_source_name) &&
        identical(mask_name, n_source_name)) {
        mask_len_var <- "n_i"
    } else if (!is.null(len_var_map) &&
        !is.null(names(len_var_map)) &&
        mask_name %in% names(len_var_map)) {
        mask_len_var <- len_var_map[[mask_name]]
    } else {
        mask_len_var <- "n_i"
    }

 # Base length (used for write loop bounds)
    base_len_var <- NULL
    if (!is.null(n_source_name) &&
        identical(base_name, n_source_name)) {
        base_len_var <- "n_i"
    } else if (!is.null(len_var_map) &&
        !is.null(names(len_var_map)) &&
        base_name %in% names(len_var_map)) {
        base_len_var <- len_var_map[[base_name]]
    } else {
        base_len_var <- "n_i"
    }

 # Emit RHS (evaluated once per outer loop iteration) If RHS is
 # mask extraction (x[mask2]), fall back to legacy path
    rhs_mask_var <- NULL
    rhs_mask_name <- NULL
    extract_mask_name <- function(idx_node) {
        if (is.null(idx_node) || is.null(idx_node$kind)) {
            return(NULL)
        }
        if (identical(idx_node$kind, "scalar_index") &&
            !is.null(idx_node$expr) &&
            is.list(idx_node$expr)) {
            idx_node <- idx_node$expr
        }
        if (identical(idx_node$kind, "var")) {
            return(idx_node$name)
        }
        if (identical(idx_node$kind, "binop") &&
            identical(idx_node$op, "-") &&
            !is.null(idx_node$lhs) &&
            !is.null(idx_node$rhs) &&
            identical(idx_node$lhs$kind, "var") &&
            identical(idx_node$rhs$kind, "const")) {
            rhs_val <- suppressWarnings(as.integer(as.numeric(idx_node$rhs$value)))
            if (!is.na(rhs_val) && rhs_val == 1L) {
                return(idx_node$lhs$name)
            }
        }
        NULL
    }
    rhs_index_node <- rhs
    if (!is.null(rhs_index_node) &&
        identical(rhs_index_node$kind, "cast") &&
        !is.null(rhs_index_node$expr) &&
        is.list(rhs_index_node$expr)) {
        rhs_index_node <- rhs_index_node$expr
    }
    if (!is.null(type_env) &&
        !is.null(rhs_index_node) &&
        identical(rhs_index_node$kind, "index")) {
        if (!is.null(rhs_index_node$base) &&
            identical(rhs_index_node$base$kind, "var") &&
            length(rhs_index_node$indices) ==
                1 && !is.null(rhs_index_node$indices[[1]]) &&
            !is.null(extract_mask_name(rhs_index_node$indices[[1]]))) {
            idx_name <- extract_mask_name(rhs_index_node$indices[[1]])
            idx_type <- type_env[[idx_name]]
            if (is.null(idx_type)) {
                arg_specs <- tryCatch(.mojor_state$current_arg_specs, error = function(e) NULL)
                if (!is.null(arg_specs) && !is.null(arg_specs[[idx_name]])) {
                    idx_type <- arg_specs[[idx_name]]
                }
            }
            if (!is.null(idx_type) &&
                .mojor_is_logical_mask_type(idx_type)) {
                rhs_mask_var <- rhs_index_node$base$name
                rhs_mask_name <- idx_name
            }
        }
    }

 # Handle RHS mask extraction for mask assignment
    if (!is.null(rhs_mask_var) &&
        !is.null(rhs_mask_name)) {
        used_names <- unique(
            c(
                base_name, mask_name, rhs_mask_var, rhs_mask_name, if (!is.null(type_env)) names(type_env) else character(0),
                if (!is.null(loop_var)) loop_var else character(0)
            )
        )
        loop_j <- .mojor_unique_loop_var("__mojor_mask_j", used_names)
        mask_val <- .mojor_unique_loop_var("__mojor_mask_val", used_names)

        if (!identical(rhs_mask_name, mask_name)) {
            .mojor_add_recycle_warning(
                list(kind = "mask_mask", lhs_mask = mask_name, rhs_mask = rhs_mask_name)
            )
        }

        upper_rhs <- "n_i"
        if (!is.null(len_var_map) &&
            rhs_mask_name %in% names(len_var_map)) {
            upper_rhs <- len_var_map[[rhs_mask_name]]
        } else if (!is.null(.mojor_state$current_len_hint[[rhs_mask_name]])) {
            upper_rhs <- .mojor_state$current_len_hint[[rhs_mask_name]]
        }

 # Case 1: same mask on RHS and LHS
        if (identical(rhs_mask_name, mask_name)) {
            lines <- c(
                paste0(
                  indent, "for ", loop_j, " in range(Int(", base_len_var,
                  ")):"
              ),
                paste0(
                  indent, "    var ", mask_val, ": Int32 = ",
                  .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    mask_name,
                    loop_j,
                    paste0("Int(", mask_len_var, ")")
                  )
              ),
                paste0(indent, "    if ", mask_val, " == -2147483648:"),
                paste0(indent, "        continue"),
                paste0(indent, "    if ", mask_val, " == 0:"),
                paste0(indent, "        continue"),
                paste0(
                  indent, "    ", base_name, "[", loop_j, "] = ", rhs_mask_var,
                  "[", loop_j, "]"
              )
            )
            return(lines)
        }

 # Case 2: different RHS mask (compressed RHS with recycling)
        rhs_scan <- .mojor_unique_loop_var("__mojor_rhs_scan", used_names)
        rhs_count <- .mojor_unique_loop_var("__mojor_rhs_count", used_names)
        inner_indent <- paste0(indent, "    ")
        lines <- c(
            paste0(indent, "var ", rhs_count, ": Int = 0"),
            paste0(indent, "for ", loop_j, " in range(Int(", upper_rhs, ")):"),
            paste0(
                inner_indent, "if ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    loop_j,
                    paste0("Int(", upper_rhs, ")")
                ),
                " == 1:"
            ),
            paste0(inner_indent, "    ", rhs_count, " += 1"),
            paste0(indent, "if ", rhs_count, " > 0:"),
            paste0(inner_indent, "var ", rhs_scan, ": Int = 0"),
            paste0(
                inner_indent, "for ", loop_j, " in range(Int(", base_len_var,
                ")):"
            ),
            paste0(
                inner_indent, "    var ", mask_val, ": Int32 = ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    mask_name,
                    loop_j,
                    paste0("Int(", mask_len_var, ")")
                )
            ),
            paste0(inner_indent, "    if ", mask_val, " == -2147483648:"),
            paste0(inner_indent, "        continue"),
            paste0(inner_indent, "    if ", mask_val, " == 0:"),
            paste0(inner_indent, "        continue"),
            paste0(
                inner_indent, "    while ", rhs_scan, " < ", upper_rhs,
                " and ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    rhs_scan,
                    paste0("Int(", upper_rhs, ")")
                ),
                " != 1:"
            ),
            paste0(inner_indent, "        ", rhs_scan, " += 1"),
            paste0(inner_indent, "    if ", rhs_scan, " >= ", upper_rhs, ":"),
            paste0(inner_indent, "        ", rhs_scan, " = 0"),
            paste0(
                inner_indent, "        while ", rhs_scan, " < ", upper_rhs,
                " and ",
                .mojor_ir_read_call(
                    "_mojor_read_lgl",
                    rhs_mask_name,
                    rhs_scan,
                    paste0("Int(", upper_rhs, ")")
                ),
                " != 1:"
            ),
            paste0(inner_indent, "            ", rhs_scan, " += 1"),
            paste0(
                inner_indent, "        if ", rhs_scan, " >= ", upper_rhs,
                ":"
            ),
            paste0(inner_indent, "            break"),
            paste0(
                inner_indent, "    ", base_name, "[", loop_j, "] = ", rhs_mask_var,
                "[", rhs_scan, "]"
            ),
            paste0(inner_indent, "    ", rhs_scan, " += 1")
        )
        return(lines)
    }
    rhs_str <- .mojor_ir_expr_emit(rhs, zero_based_vars, type_env, loop_vars = loop_var)
    if (is.null(rhs_str)) {
        return(NULL)
    }

 # Generate unique loop variable
    used_names <- unique(
        c(
            base_name, mask_name, if (!is.null(type_env)) names(type_env) else character(0),
            if (!is.null(loop_var)) loop_var else character(0)
        )
    )
    loop_j <- .mojor_unique_loop_var("__mojor_mask_j", used_names)
    mask_val <- .mojor_unique_loop_var("__mojor_mask_val", used_names)

 # Lower to explicit loop
    lines <- character(0)
    lines <- c(
        lines, paste0(indent, "for ", loop_j, " in range(Int(", base_len_var, ")):")
    )
    loop_indent <- paste0(indent, "    ")

    lines <- c(
        lines, paste0(
            loop_indent, "var ", mask_val, ": Int32 = ",
            .mojor_ir_read_call(
                "_mojor_read_lgl",
                mask_name,
                loop_j,
                paste0("Int(", mask_len_var, ")")
            )
        )
    )

 # Check for NA in mask (Int32 NA is -2147483648)
    lines <- c(lines, paste0(loop_indent, "if ", mask_val, " == -2147483648:"))
    lines <- c(lines, paste0(loop_indent, "    continue"))

 # Check if mask is false (0)
    lines <- c(lines, paste0(loop_indent, "if ", mask_val, " == 0:"))
    lines <- c(lines, paste0(loop_indent, "    continue"))

 # Assign to output
    lines <- c(lines, paste0(loop_indent, base_name, "[", loop_j, "] = ", rhs_str))

    lines
}

# Recursively rewrite RHS index nodes that contain slice_index indices
# into scalar reads using the LHS slice loop variables.
.mojor_ir_rewrite_rhs_slices <- function(node, lhs_indices, n_idx,
                                          lhs_axis_loop_var_fn,
                                          lhs_axis_start_fn,
                                          rhs_zero_based_vars = character(0)) {
    .mojor_ir_map_rhs_slices_to_loop_indices(
        node,
        lhs_indices,
        lhs_axis_loop_var_fn,
        lhs_axis_start_fn,
        rhs_zero_based_vars = rhs_zero_based_vars
    )
}
