# =============================================================================
# IR Statement Range Emit
# =============================================================================

.mojor_ir_simple_range_emit <- function(range_expr_node, type_env = NULL, zero_based_vars = NULL) {
 # Emit simple range expressions for loops Only handles common
 # patterns: seq_len(n), 1:n, seq.int(...) Returns NULL for
 # complex cases (fallback to existing transpiler)
    if (is.null(range_expr_node) ||
        is.null(range_expr_node$kind)) {
        return(NULL)
    }
    # Dispatch: structured range nodes from canonicalization pass
    if (range_expr_node$kind == "range") {
        return(.mojor_ir_structured_range_emit(
            range_expr_node,
            type_env,
            zero_based_vars = zero_based_vars
        ))
    }
    if (range_expr_node$kind != "range_expr") {
        return(NULL)
    }

    expr_src <- range_expr_node$expr
    expr <- .mojor_ir_normalize_selector_ast(expr_src, mode = "loop_seq")
    if (is.call(expr_src) &&
        length(expr_src) >= 1L &&
        as.character(expr_src[[1]]) %in% c("seq_len", "seq_along", "mojor_seq_len")) {
        expr <- expr_src
    }
    emit_int <- function(e) {
        if (is.numeric(e) &&
            length(e) ==
                1 && (is.na(e) ||
            (e%%1) != 0)) {
            return(NULL)
        }
        node <- .mojor_ir_expr_build(e)
        if (is.null(node)) {
            return(NULL)
        }
        out <- .mojor_ir_expr_emit(node, loop_vars = NULL, type_env = type_env)
        if (is.null(out) ||
            out == "") {
            return(NULL)
        }
        if (!grepl("^Int\\(", out))
            out <- paste0("Int(", out, ")")
        out
    }
    emit_along_with_range <- function(arr_arg) {
        if (!is.name(arr_arg)) {
            return(NULL)
        }
        arr_str <- as.character(arr_arg)
        broadcast_nd <- isTRUE(.mojor_state$current_broadcast_nd)
        if (broadcast_nd) {
            return(
                list(
                  range = "range(1, n_i + 1)", loop_var_is_zero_based = FALSE,
                  is_simple_1_based = TRUE, start = "1", end = "n_i"
              )
            )
        }
        len_var_map <- .mojor_state$current_len_var_map
        n_source_name <- .mojor_state$current_n_source_name
        len_var <- NULL
        if (!is.null(n_source_name) &&
            identical(arr_str, n_source_name)) {
            len_var <- "n_i"
        } else if (!is.null(len_var_map) &&
            !is.null(names(len_var_map)) &&
            arr_str %in% names(len_var_map)) {
            len_var <- len_var_map[[arr_str]]
        }
        if (is.null(len_var)) {
            return(NULL)
        }
        list(
            range = paste0("range(1, (", len_var, " + 1))"),
            loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE, start = "1",
            end = len_var
        )
    }

    # Handle bare name (e.g., for (x in vec)) by treating it as seq_along(vec)
    if (is.name(expr)) {
        return(emit_along_with_range(expr))
    }

 # Handle seq_len(n) -> range(1, n + 1)
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "seq_len" && length(expr) ==
        2) {
        len_arg <- expr[[2]]
        if (is.name(len_arg)) {
            len_str <- as.character(len_arg)
            return(
                list(
                  range = paste0("range(1, Int(", len_str, ") + 1)"),
                  loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE,
                  start = "1", end = paste0("Int(", len_str, ")")
              )
            )
        }
        if (is.numeric(len_arg) &&
            length(len_arg) ==
                1) {
            return(
                list(
                  range = paste0(
                    "range(1, ", as.integer(len_arg) +
                      1, ")"
                ),
                  loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE,
                  start = "1", end = as.character(as.integer(len_arg))
              )
            )
        }
        len_str <- emit_int(len_arg)
        if (!is.null(len_str)) {
            return(
                list(
                  range = paste0("range(1, ", len_str, " + 1)"),
                  loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE,
                  start = "1", end = len_str
              )
            )
        }
    }

 # Handle seq_along(x) -> range(1, len(x) + 1)
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "seq_along" && length(expr) ==
        2) {
        arr_arg <- expr[[2]]
        out <- emit_along_with_range(arr_arg)
        if (!is.null(out)) {
            return(out)
        }
    }

 # Step 8.12: Handle mojor_seq_len(expr) -> range(1, expr + 1)
 # This is used by the subset flow for top-level constructor
 # loops
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "mojor_seq_len" && length(expr) ==
        2) {
        len_arg <- expr[[2]]
 # Try to emit the length expression
        len_ir <- .mojor_ir_expr_build(len_arg)
        if (!is.null(len_ir)) {
            len_str <- .mojor_ir_expr_emit(len_ir, loop_vars = NULL, type_env = type_env)
            if (!is.null(len_str) &&
                nzchar(len_str)) {
                return(
                  list(
                    range = paste0("range(1, Int(", len_str, ") + 1)"),
                    loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE,
                    start = "1", end = paste0("Int(", len_str, ")")
                )
              )
            }
        }
    }

 # Handle affine transforms of seq_len(): seq_len(n) + k, k +
 # seq_len(n), k * seq_len(n), rev(seq_len(n))
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        is_seq_len <- function(node) {
            is.call(node) &&
                as.character(node[[1]]) ==
                  "seq_len" && length(node) ==
                2
        }
        emit_seq_len <- function(node) {
            if (!is_seq_len(node)) {
                return(NULL)
            }
            emit_int(node[[2]])
        }
        if (op == "rev" && length(expr) ==
            2 && is_seq_len(expr[[2]])) {
            len_str <- emit_seq_len(expr[[2]])
            if (!is.null(len_str)) {
                return(
                  list(
                    range = paste0("range(", len_str, ", 0, -1)"),
                    loop_var_is_zero_based = FALSE
                )
              )
            }
        }
        if (op %in% c("+", "-", "*") &&
            length(expr) ==
                3) {
            lhs <- expr[[2]]
            rhs <- expr[[3]]
            seq_node <- NULL
            scalar_node <- NULL
            side <- NULL
            if (is_seq_len(lhs)) {
                seq_node <- lhs
                scalar_node <- rhs
                side <- "rhs"
            } else if (is_seq_len(rhs)) {
                seq_node <- rhs
                scalar_node <- lhs
                side <- "lhs"
            }
            if (!is.null(seq_node)) {
                len_str <- emit_seq_len(seq_node)
                scalar_str <- emit_int(scalar_node)
                if (!is.null(len_str) &&
                  !is.null(scalar_str)) {
                  if (op == "+") {
                    start_expr <- paste0("(", scalar_str, " + 1)")
                    end_expr <- paste0("(", len_str, " + ", scalar_str, ")")
                    return(
                      list(
                        range = paste0("range(", start_expr, ", ", end_expr, " + 1)"),
                        loop_var_is_zero_based = FALSE
                    )
                  )
                  }
                  if (op == "-" && side == "rhs") {
                    start_expr <- paste0("(1 - ", scalar_str, ")")
                    end_expr <- paste0("(", len_str, " - ", scalar_str, ")")
                    return(
                      list(
                        range = paste0("range(", start_expr, ", ", end_expr, " + 1)"),
                        loop_var_is_zero_based = FALSE
                    )
                  )
                  }
                  if (op == "-" && side == "lhs") {
                    start_expr <- paste0("(", scalar_str, " - 1)")
                    end_expr <- paste0("(", scalar_str, " - ", len_str, ")")
                    return(
                      list(
                        range = paste0("range(", start_expr, ", ", end_expr, " - 1, -1)"),
                        loop_var_is_zero_based = FALSE
                    )
                  )
                  }
                  if (op == "*") {
                    end_expr <- paste0("(", len_str, " * ", scalar_str, ")")
                    scalar_val <- NA_integer_
                    if (is.numeric(scalar_node) &&
                      length(scalar_node) ==
                        1 && !is.na(scalar_node) &&
                      (scalar_node%%1) == 0) {
                      scalar_val <- as.integer(scalar_node)
                    }
                    if (!is.na(scalar_val) &&
                      scalar_val != 0L) {
                      end_excl <- if (scalar_val < 0L) {
                        paste0("(", end_expr, " - 1)")
                      } else {
                        paste0("(", end_expr, " + 1)")
                      }
                      return(
                        list(
                          range = paste0(
                            "range(", scalar_str, ", ", end_excl, ", ",
                            scalar_val, ")"
                        ),
                          loop_var_is_zero_based = FALSE
                      )
                    )
                    }
                    neg_cond <- paste0("(", scalar_str, " < 0)")
                    end_excl <- paste0(
                      "(", end_expr, " - 1 if ", neg_cond, " else ", end_expr,
                      " + 1)"
                  )
                    return(
                      list(
                        range = paste0(
                          "range(", scalar_str, ", ", end_excl, ", ", scalar_str,
                          ")"
                      ),
                        loop_var_is_zero_based = FALSE
                    )
                  )
                  }
                }
            }
        }
    }

 # Handle seq.int(from, to, by=...) with explicit by or length.out
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "seq.int") {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        get_arg <- function(nm, pos) {
            if (!is.null(nms) &&
                nm %in% nms) {
                return(parts[[which(nms == nm)[1]]])
            }
            if (length(parts) >=
                pos && (is.null(nms) ||
                is.null(nms[[pos]]) ||
                nms[[pos]] == "")) {
                return(parts[[pos]])
            }
            NULL
        }
        length_out <- get_arg("length.out", 4)
        if (!is.null(length_out)) {
            len_str <- emit_int(length_out)
            if (is.null(len_str)) {
                return(NULL)
            }
            return(
                list(
                  range = paste0("range(1, (", len_str, " + 1))"),
                  loop_var_is_zero_based = FALSE, is_simple_1_based = TRUE,
                  start = "1", end = len_str
              )
            )
        }
        along_with <- get_arg("along.with", 4)
        if (!is.null(along_with)) {
            out <- emit_along_with_range(along_with)
            if (!is.null(out)) {
                return(out)
            }
            return(NULL)
        }
        by_expr <- get_arg("by", 3)
        from_expr <- get_arg("from", 1)
        to_expr <- get_arg("to", 2)
        if (is.null(from_expr))
            from_expr <- 1
        if (is.null(to_expr)) {
            return(NULL)
        }
        if (is.null(by_expr)) {
            from_str <- emit_int(from_expr)
            to_str <- emit_int(to_expr)
            if (is.null(from_str) ||
                is.null(to_str)) {
                return(NULL)
            }
            by_val <- NA_integer_
            if (is.numeric(from_expr) &&
                length(from_expr) ==
                  1 && is.numeric(to_expr) &&
                length(to_expr) ==
                  1) {
                by_val <- if (from_expr <= to_expr)
                  1L else -1L
            }
            if (!is.na(by_val)) {
                end_expr <- if (by_val < 0L) {
                  paste0("(", to_str, " - 1)")
                } else {
                  paste0("(", to_str, " + 1)")
                }
                return(
                  list(
                    range = paste0(
                      "range(", from_str, ", ", end_expr, ", ", by_val,
                      ")"
                  ),
                    loop_var_is_zero_based = FALSE
                )
              )
            }
            as_int <- function(x) {
                if (grepl("^Int\\(", x)) {
                  return(x)
                }
                paste0("Int(", x, ")")
            }
            start_cmp <- as_int(from_str)
            end_cmp <- as_int(to_str)
            neg_cond <- paste0("(", start_cmp, " > ", end_cmp, ")")
            by_str <- paste0("(-1 if ", neg_cond, " else 1)")
            end_expr <- paste0(
                "(", end_cmp, " - 1 if ", neg_cond, " else ", end_cmp,
                " + 1)"
            )
            return(
                list(
                  range = paste0("range(", from_str, ", ", end_expr, ", ", by_str, ")"),
                  loop_var_is_zero_based = FALSE
              )
            )
        }
        from_str <- emit_int(from_expr)
        to_str <- emit_int(to_expr)
        by_str <- emit_int(by_expr)
        if (is.null(from_str) ||
            is.null(to_str) ||
            is.null(by_str)) {
            return(NULL)
        }
        by_val <- NA_integer_
        if (is.integer(by_expr) &&
            length(by_expr) ==
                1)
            by_val <- as.integer(by_expr)
        if (is.numeric(by_expr) &&
            length(by_expr) ==
                1 && !is.na(by_expr) &&
            (by_expr%%1) == 0) {
            by_val <- as.integer(by_expr)
        }
        if (!is.na(by_val) &&
            by_val == 0L) {
            return(NULL)
        }
        if (is.na(by_val)) {
            end_expr <- paste0(
                "(", to_str, " - 1 if (", by_str, " < 0) else ", to_str,
                " + 1)"
            )
        } else if (by_val < 0L) {
            end_expr <- paste0("(", to_str, " - 1)")
        } else {
            end_expr <- paste0("(", to_str, " + 1)")
        }
        return(
            list(
                range = paste0("range(", from_str, ", ", end_expr, ", ", by_str, ")"),
                loop_var_is_zero_based = FALSE
            )
        )
    }

 # Handle seq(from, to, by=...) with explicit by (integer-ish
 # only)
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "seq") {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        get_arg <- function(nm, pos) {
            if (!is.null(nms) &&
                nm %in% nms) {
                return(parts[[which(nms == nm)[1]]])
            }
            if (length(parts) >=
                pos && (is.null(nms) ||
                is.null(nms[[pos]]) ||
                nms[[pos]] == "")) {
                return(parts[[pos]])
            }
            NULL
        }
        from_expr <- get_arg("from", 1)
        to_expr <- get_arg("to", 2)
        if (length(parts) ==
            1 && (is.null(nms) ||
            nms[[1]] == "")) {
            to_expr <- from_expr
            from_expr <- NULL
        }
        along_with <- get_arg("along.with", 4)
        if (!is.null(along_with)) {
            out <- emit_along_with_range(along_with)
            if (!is.null(out)) {
                return(out)
            }
            return(NULL)
        }
        # Handle seq(length.out=n) - defaults to from=1, equivalent to seq_len(n)
        length_out_expr <- get_arg("length.out", 4)
        if (!is.null(length_out_expr)) {
            # When from is not provided, it defaults to 1
            if (is.null(from_expr)) {
                from_expr <- 1
            }
            # Only handle the case where from=1 (equivalent to seq_len)
            if (is.numeric(from_expr) && length(from_expr) == 1 && from_expr == 1) {
                len_str <- emit_int(length_out_expr)
                if (!is.null(len_str)) {
                    return(list(
                        range = paste0("range(", len_str, ")"),
                        loop_var_is_zero_based = FALSE
                    ))
                }
            }
            return(NULL)
        }
        by_expr <- get_arg("by", 3)
        if (is.null(from_expr))
            from_expr <- 1
        if (is.null(to_expr)) {
            return(NULL)
        }
        if (is.null(by_expr)) {
            from_str <- emit_int(from_expr)
            to_str <- emit_int(to_expr)
            if (is.null(from_str) ||
                is.null(to_str)) {
                return(NULL)
            }
            by_val <- NA_integer_
            if (is.numeric(from_expr) &&
                length(from_expr) ==
                  1 && is.numeric(to_expr) &&
                length(to_expr) ==
                  1) {
                by_val <- if (from_expr <= to_expr)
                  1L else -1L
            }
            if (!is.na(by_val)) {
                end_expr <- if (by_val < 0L) {
                  paste0("(", to_str, " - 1)")
                } else {
                  paste0("(", to_str, " + 1)")
                }
                return(
                  list(
                    range = paste0(
                      "range(", from_str, ", ", end_expr, ", ", by_val,
                      ")"
                  ),
                    loop_var_is_zero_based = FALSE
                )
              )
            }
            as_int <- function(x) {
                if (grepl("^Int\\(", x)) {
                  return(x)
                }
                paste0("Int(", x, ")")
            }
            start_cmp <- as_int(from_str)
            end_cmp <- as_int(to_str)
            neg_cond <- paste0("(", start_cmp, " > ", end_cmp, ")")
            by_str <- paste0("(-1 if ", neg_cond, " else 1)")
            end_expr <- paste0(
                "(", end_cmp, " - 1 if ", neg_cond, " else ", end_cmp,
                " + 1)"
            )
            return(
                list(
                  range = paste0("range(", from_str, ", ", end_expr, ", ", by_str, ")"),
                  loop_var_is_zero_based = FALSE
              )
            )
        }
        from_str <- emit_int(from_expr)
        to_str <- emit_int(to_expr)
        by_str <- emit_int(by_expr)
        if (is.null(from_str) ||
            is.null(to_str) ||
            is.null(by_str)) {
            return(NULL)
        }
        by_val <- NA_integer_
        if (is.integer(by_expr) &&
            length(by_expr) ==
                1)
            by_val <- as.integer(by_expr)
        if (is.numeric(by_expr) &&
            length(by_expr) ==
                1 && !is.na(by_expr) &&
            (by_expr%%1) == 0) {
            by_val <- as.integer(by_expr)
        }
        if (!is.na(by_val) &&
            by_val == 0L) {
            return(NULL)
        }
        if (is.na(by_val)) {
            end_expr <- paste0(
                "(", to_str, " - 1 if (", by_str, " < 0) else ", to_str,
                " + 1)"
            )
        } else if (by_val < 0L) {
            end_expr <- paste0("(", to_str, " - 1)")
        } else {
            end_expr <- paste0("(", to_str, " + 1)")
        }
        return(
            list(
                range = paste0("range(", from_str, ", ", end_expr, ", ", by_str, ")"),
                loop_var_is_zero_based = FALSE
            )
        )
    }

 # Handle simple colon range a:b where both are names or constants
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            ":" && length(expr) ==
        3) {
        start <- expr[[2]]
        end <- expr[[3]]

        emit_len_call <- function(node) {
            if (!is.call(node) ||
                length(node) !=
                  2 || as.character(node[[1]]) !=
                "length") {
                return(NULL)
            }
            arg <- node[[2]]
            if (!is.name(arg)) {
                return(NULL)
            }
            arr_str <- as.character(arg)
            len_var_map <- .mojor_state$current_len_var_map
            n_source_name <- .mojor_state$current_n_source_name
            if (!is.null(n_source_name) &&
                identical(arr_str, n_source_name)) {
                return("n_i")
            }
            if (!is.null(len_var_map) &&
                !is.null(names(len_var_map)) &&
                arr_str %in% names(len_var_map)) {
                return(len_var_map[[arr_str]])
            }
            NULL
        }

        emit_colon_bound <- function(node) {
            if (is.numeric(node) &&
                length(node) ==
                  1) {
                return(as.character(as.integer(node)))
            }
            if (is.name(node)) {
                return(as.character(node))
            }
            len_str <- emit_len_call(node)
            if (!is.null(len_str)) {
                return(len_str)
            }
            ir_node <- .mojor_ir_expr_build(node)
            if (is.null(ir_node)) {
                return(NULL)
            }
            expr_str <- .mojor_ir_expr_emit(ir_node, loop_vars = NULL, type_env = type_env, index_context = TRUE)
            if (is.null(expr_str) ||
                !nzchar(expr_str)) {
                return(NULL)
            }
            expr_str
        }

        start_str <- emit_colon_bound(start)
        end_str <- emit_colon_bound(end)

        if (!is.null(start_str) &&
            !is.null(end_str)) {
 # For constant ranges, check if ascending or descending
            if (is.numeric(start) &&
                is.numeric(end)) {
                if (start <= end) {
 # Step 8.12: Add metadata for unrolling
                  is_simple <- (start == 1) && is.numeric(end)
                  return(
                    list(
                      range = paste0("range(", start_str, ", ", end_str, " + 1)"),
                      loop_var_is_zero_based = FALSE, is_simple_1_based = is_simple,
                      start = start_str, end = end_str
                  )
                )
                } else {
                  return(
                    list(
                      range = paste0("range(", start_str, ", ", end_str, " - 1, -1)"),
                      loop_var_is_zero_based = FALSE
                  )
                )
                }
            }
 # For dynamic ranges, emit with conditional step (like
 # existing transpiler) Step 8.12: Mark as simple if
 # starts at 1
            is_simple <- (is.numeric(start) &&
                length(start) ==
                  1 && start == 1) || (is.character(start_str) &&
                start_str == "1")
            as_int <- function(x) {
                if (grepl("^Int\\(", x)) {
                  return(x)
                }
                paste0("Int(", x, ")")
            }
            start_cmp <- as_int(start_str)
            end_cmp <- as_int(end_str)
            neg_cond <- paste0("(", start_cmp, " > ", end_cmp, ")")
            range_str <- paste0(
                "range(", start_str, ", ", "(", end_cmp, " - 1 if ", neg_cond,
                " else ", end_cmp, " + 1), ", "(-1 if ", neg_cond, " else 1))"
            )
            return(
                list(
                  range = range_str, loop_var_is_zero_based = FALSE, is_simple_1_based = is_simple,
                  start = start_str, end = end_str
              )
            )
        }
    }

 # Unable to emit - return NULL for fallback
    NULL
}

# =============================================================================
# Section 6: Statement IR Emitters
# =============================================================================
