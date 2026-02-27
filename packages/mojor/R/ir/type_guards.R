.mojor_ir_annotate_type <- function(node, type_env = list()) {
 # Add type field to an expression node
    if (is.null(node)) {
        return(NULL)
    }

 # Infer and attach type
    node$type <- .mojor_ir_infer_type(node, type_env)

 # Recursively annotate children
    if (node$kind == "binop") {
        node$lhs <- .mojor_ir_annotate_type(node$lhs, type_env)
        node$rhs <- .mojor_ir_annotate_type(node$rhs, type_env)
    } else if (node$kind == "unop") {
        node$expr <- .mojor_ir_annotate_type(node$expr, type_env)
    } else if (node$kind == "cast") {
        node$expr <- .mojor_ir_annotate_type(node$expr, type_env)
    } else if (node$kind == "index") {
        node$base <- .mojor_ir_annotate_type(node$base, type_env)
        for (i in seq_along(node$indices)) {
            node$indices[[i]] <- .mojor_ir_annotate_type(node$indices[[i]], type_env)
        }
    } else if (node$kind == "call") {
        for (i in seq_along(node$args)) {
            node$args[[i]] <- .mojor_ir_annotate_type(node$args[[i]], type_env)
        }
    }

    node
}

# =============================================================================
# .mojor_ir_annotate_typed() <U+2014> full-tree type + layout
# annotation pass
# =============================================================================
# Walks the entire IR tree (statements and expressions). <U+2022>
# Expression nodes get $type = .mojor_ir_infer_type(node, type_env)
# <U+2022> index nodes whose base var has ndim >= 2 get $layout =
# list(kind, ndim) <U+2022> Statement nodes are recursed but not
# themselves typed <U+2022> loop handler extends type_env with loop
# variable <U+2192> 'i32' This pass is inserted after
# .mojor_ir_type_check_stmt in the pipeline so that downstream
# consumers (emitter, verifier) can read pre-computed types from the
# tree rather than re-inferring them.
.mojor_ir_annotate_typed <- function(node, type_env = list()) {
    if (is.null(node) ||
        !is.list(node) ||
        is.null(node$kind)) {
        return(node)
    }
    kind <- node$kind

 # Expression kinds: annotate $type and recurse into children
    if (kind %in% c(
        "const", "var", "unop", "binop", "cast", "call", "index", "ifelse",
        "type_query", "type_predicate", "df_col_read", "df_col_exists_guard",
        "df_make", "list_make", "regex_grepl", "regex_grep", "regex_sub",
        "row_matrix", "col_matrix", "expand_grid"
    )) {
        node$type <- .mojor_ir_infer_type(node, type_env)
        if (kind == "binop") {
            node$lhs <- .mojor_ir_annotate_typed(node$lhs, type_env)
            node$rhs <- .mojor_ir_annotate_typed(node$rhs, type_env)
        } else if (kind == "unop") {
            node$expr <- .mojor_ir_annotate_typed(node$expr, type_env)
        } else if (kind == "cast") {
            node$expr <- .mojor_ir_annotate_typed(node$expr, type_env)
        } else if (kind == "index") {
            node$base <- .mojor_ir_annotate_typed(node$base, type_env)
            node$indices <- lapply(node$indices, .mojor_ir_annotate_typed, type_env)
 # Attach $layout for multi-dimensional array accesses
            if (node$base$kind == "var") {
                vt <- type_env[[node$base$name]]
                nd <- .mojor_type_ndim(vt)
                if (!is.null(nd) &&
                  nd >= 2L) {
                  layout_kind <- .mojor_state$`%||%`(.mojor_state$current_array_layout, .mojor_state$options$array_layout)
                  if (is.null(layout_kind))
                    layout_kind <- "col_major"
                  node$layout <- list(kind = layout_kind, ndim = nd)
                }
            }
        } else if (kind == "call") {
            node$args <- lapply(node$args, .mojor_ir_annotate_typed, type_env)
        } else if (kind == "ifelse") {
            node$cond <- .mojor_ir_annotate_typed(node$cond, type_env)
            node$yes <- .mojor_ir_annotate_typed(node$yes, type_env)
            node$no <- .mojor_ir_annotate_typed(node$no, type_env)
        } else if (kind %in% c("type_query", "type_predicate")) {
            node$x <- .mojor_ir_annotate_typed(node$x, type_env)
        } else if (kind %in% c("df_col_read", "df_col_exists_guard")) {
            node$df <- .mojor_ir_annotate_typed(node$df, type_env)
            node$col <- .mojor_ir_annotate_typed(node$col, type_env)
        } else if (kind == "df_make") {
            node$cols <- lapply(node$cols, .mojor_ir_annotate_typed, type_env)
        } else if (kind == "list_make") {
            node$items <- lapply(node$items, .mojor_ir_annotate_typed, type_env)
        } else if (kind == "regex_grepl") {
            node$pattern <- .mojor_ir_annotate_typed(node$pattern, type_env)
            node$x <- .mojor_ir_annotate_typed(node$x, type_env)
        } else if (kind == "regex_grep") {
            node$pattern <- .mojor_ir_annotate_typed(node$pattern, type_env)
            node$x <- .mojor_ir_annotate_typed(node$x, type_env)
            if (is.list(node$value) &&
                !is.null(node$value$kind)) {
                node$value <- .mojor_ir_annotate_typed(node$value, type_env)
            }
        } else if (kind == "regex_sub") {
            node$pattern <- .mojor_ir_annotate_typed(node$pattern, type_env)
            node$replacement <- .mojor_ir_annotate_typed(node$replacement, type_env)
            node$x <- .mojor_ir_annotate_typed(node$x, type_env)
        } else if (kind %in% c("row_matrix", "col_matrix")) {
            node$x <- .mojor_ir_annotate_typed(node$x, type_env)
        } else if (kind == "expand_grid") {
            node$args <- lapply(node$args, .mojor_ir_annotate_typed, type_env)
        }
        return(node)
    }

 # Statement / structural kinds: recurse only (no $type on
 # statements)
    if (kind == "block") {
        node$stmts <- lapply(node$stmts, .mojor_ir_annotate_typed, type_env)
    } else if (kind == "assign") {
        node$lhs <- .mojor_ir_annotate_typed(node$lhs, type_env)
        node$rhs <- .mojor_ir_annotate_typed(node$rhs, type_env)
    } else if (kind == "if") {
        node$cond <- .mojor_ir_annotate_typed(node$cond, type_env)
        node$then <- .mojor_ir_annotate_typed(node$then, type_env)
        if (!is.null(node$else_block)) {
            node$else_block <- .mojor_ir_annotate_typed(node$else_block, type_env)
        }
    } else if (kind == "loop") {
        new_env <- type_env
        new_env[[node$var]] <- "i32"
        node$body <- .mojor_ir_annotate_typed(node$body, new_env)
    } else if (kind == "while") {
        node$cond <- .mojor_ir_annotate_typed(node$cond, type_env)
        node$body <- .mojor_ir_annotate_typed(node$body, type_env)
    } else if (kind == "repeat") {
        node$body <- .mojor_ir_annotate_typed(node$body, type_env)
    } else if (kind == "return") {
        if (!is.null(node$value)) {
            node$value <- .mojor_ir_annotate_typed(node$value, type_env)
        }
    } else if (kind == "scalar_reduce") {
 # No IR sub-nodes; acc/arg are plain strings
    } else if (kind == "scheduled_reduce") {
 # No IR sub-nodes; emitted directly by statement emitter
    } else if (kind == "slice_index") {
        node$start <- .mojor_ir_annotate_typed(node$start, type_env)
        node$end <- .mojor_ir_annotate_typed(node$end, type_env)
    } else if (kind == "range") {
        node$start <- .mojor_ir_annotate_typed(node$start, type_env)
        node$end <- .mojor_ir_annotate_typed(node$end, type_env)
        if (!is.null(node$step)) {
            node$step <- .mojor_ir_annotate_typed(node$step, type_env)
        }
    } else if (kind %in% c("sample_int", "sample")) {
 # Sampling & Permutation - annotate all children
        node$n <- .mojor_ir_annotate_typed(node$n, type_env)
        node$size <- .mojor_ir_annotate_typed(node$size, type_env)
        if (!is.null(node$replace))
            node$replace <- .mojor_ir_annotate_typed(node$replace, type_env)
        if (!is.null(node$prob))
            node$prob <- .mojor_ir_annotate_typed(node$prob, type_env)
    }
 # const, var (already handled above), missing_index, scalar_index
 # (sugar), break, next, raw, range_expr: no sub-expressions to
 # annotate

    node
}

# =============================================================================
# Step 3: Typed IR - Cast Insertion
# =============================================================================

.mojor_ir_bool_coerce <- function(expr, expr_type = NULL) {
 # Coerce expression to bool using != 0 comparison Only needed for
 # numeric types in boolean contexts

    if (is.null(expr_type)) {
        expr_type <- expr$type %||% "unknown"
    }

    if (expr_type %in% c("i32", "f64", "f32")) {
 # Create != 0 comparison
        zero_val <- if (expr_type == "i32")
            "0" else "0.0"
        zero_const <- .mojor_ir_const(zero_val)
        return(.mojor_ir_binop("!=", expr, zero_const))
    }

 # Already bool or unknown - no coercion needed
    return(expr)
}

.mojor_ir_reduction_acc_type <- function(stmt, type_env = list()) {
    if (is.null(stmt) ||
        is.null(stmt$kind)) {
        return(NULL)
    }
    if (!stmt$kind %in% c("scalar_reduce", "scheduled_reduce")) {
        return(NULL)
    }

    op <- if (!is.null(stmt$op))
        as.character(stmt$op) else ""
    if (op %in% c("which.min", "which.max")) {
        return("i32")
    }

    dtype <- if (!is.null(stmt$dtype))
        as.character(stmt$dtype) else NULL
    if (!is.null(dtype) &&
        length(dtype) ==
            1L && nzchar(dtype)) {
        if (dtype %in% c("Float64", "DType.float64", "f64")) {
            return("f64")
        }
        if (dtype %in% c("Float32", "DType.float32", "f32")) {
            return("f32")
        }
        if (dtype %in% c("Int32", "DType.int32", "i32")) {
            return("i32")
        }
    }

    arg_name <- if (!is.null(stmt$arg))
        as.character(stmt$arg) else NULL
    if (!is.null(arg_name) &&
        length(arg_name) ==
            1L && nzchar(arg_name)) {
        arg_type <- type_env[[arg_name]]
        if (!is.null(arg_type) &&
            .mojor_type_is_array(arg_type)) {
            elem_type <- .mojor_type_elem(arg_type)
            if (elem_type %in% c("lgl", "bool")) {
                return("i32")
            }
            if (elem_type %in% c("i32", "f32", "f64")) {
                return(elem_type)
            }
        }
    }

    NULL
}

.mojor_ir_type_check_block <- function(block, type_env = list()) {
 # Type check all statements in a block
    if (is.null(block) ||
        block$kind != "block") {
        return(block)
    }

    env <- type_env

    for (i in seq_along(block$stmts)) {
        stmt <- .mojor_ir_type_check_stmt(block$stmts[[i]], env)

        if (!is.null(stmt) &&
            stmt$kind == "assign" && !is.null(stmt$lhs) &&
            is.list(stmt$lhs) &&
            stmt$lhs$kind == "var") {
            lhs_name <- stmt$lhs$name
            rhs_type <- .mojor_ir_infer_type(stmt$rhs, env)
            if (!is.null(lhs_name) &&
                nzchar(lhs_name) &&
                !is.null(rhs_type) &&
                rhs_type != "unknown") {
                env[[lhs_name]] <- rhs_type
            }
        } else if (!is.null(stmt) &&
            stmt$kind %in% c("scalar_reduce", "scheduled_reduce") &&
            !is.null(stmt$acc) &&
            is.character(stmt$acc) &&
            length(stmt$acc) ==
                1L && nzchar(stmt$acc)) {
            acc_type <- .mojor_ir_reduction_acc_type(stmt, env)
            if (!is.null(acc_type) &&
                nzchar(acc_type)) {
                env[[stmt$acc]] <- acc_type
            }
        }

        block$stmts[[i]] <- stmt
    }

    return(block)
}

# =============================================================================
# Step 4: Guard Pass - NA Guards
# =============================================================================

.mojor_ir_needs_na_guard <- function(node, na_guard = "forbid") {
 # Determine if an expression needs NA guarding Returns TRUE if
 # the operation could produce/propagate NA and guards are needed

    if (na_guard == "unsafe")
        {
            return(FALSE)
        }  # No guards in unsafe mode
    if (is.null(node) ||
        is.null(node$kind)) {
        return(FALSE)
    }

 # Arithmetic operations need guards
    if (node$kind == "binop" && node$op %in% c("+", "-", "*", "/", "%%", "%/%")) {
        return(TRUE)
    }

 # Math functions would need guards (not yet implemented in IR)
 # Comparisons don't need guards (they return bool, not NA)
 # Logical operators handle NA explicitly (don't need guards)

    FALSE
}

.mojor_ir_na_checks <- function(sources, type_env = NULL) {
    if (length(sources) ==
        0) {
        return(character(0))
    }
    checks <- character(0)
    for (src in sources) {
        src_type <- NULL
        if (!is.null(type_env)) {
            if (grepl("^[A-Za-z_.][A-Za-z0-9_.]*$", src)) {
                src_type <- type_env[[src]]
            } else if (grepl("\\[", src)) {
                base <- sub("\\[.*$", "", src)
                base_type <- type_env[[base]]
                if (.mojor_type_is_array(base_type)) {
                  src_type <- .mojor_type_elem(base_type)
                } else {
                  src_type <- base_type
                }
                if (identical(src_type, "lgl"))
                  src_type <- "bool"
            }
        }
        if (is.null(src_type) ||
            src_type == "unknown") {
            if (grepl("^_mojor_read_(i32|lgl)\\(", src)) {
                checks <- c(checks, paste0("(Int32(", src, ") == Int32(-2147483648))"))
            } else {
 # Best-effort guard for local scalars with unknown
 # type
                checks <- c(checks, paste0("(", src, " != ", src, ")"))
            }
            next
        }
        if (.mojor_type_is_array(src_type))
            next
        if (src_type %in% c("f32", "f64")) {
 # Avoid isnan() to sidestep Mojo compiler crashes; NaN is
 # the only value where x != x
            checks <- c(checks, paste0("(", src, " != ", src, ")"))
        } else if (src_type %in% c("i32", "lgl", "bool")) {
 # Cast to Int32 for stable comparisons when source is
 # emitted as Mojo Int.
            checks <- c(checks, paste0("(Int32(", src, ") == Int32(-2147483648))"))
        }
    }
    checks
}

.mojor_ir_collect_na_sources <- function(node, zero_based_vars = NULL, type_env = NULL) {
 # Step 5.1: Added type_env parameter for logical array support
 # Collect all sub-expressions that could be NA (variables and
 # index expressions) Returns a list of expression strings to
 # check

    if (is.null(node) ||
        is.null(node$kind)) {
        return(character(0))
    }

    sources <- character(0)

    if (node$kind == "var") {
        iter_map <- .mojor_state$current_iter_map
        if (!is.null(iter_map) &&
            !is.null(iter_map[[node$name]])) {
            iter_entry <- iter_map[[node$name]]
            if (!is.null(iter_entry$expr)) {
                return(iter_entry$expr)
            }
            if (!is.null(iter_entry$source) &&
                !is.null(iter_entry$index)) {
                idx_name <- iter_entry$index
                if (!is.null(zero_based_vars) &&
                  idx_name %in% zero_based_vars) {
                  return(paste0(iter_entry$source, "[", idx_name, "]"))
                }
                return(paste0(iter_entry$source, "[(", idx_name, " - 1)]"))
            }
        }
 # Variables could be NA
        return(node$name)
    }

    if (node$kind == "index") {
 # sampling index expressions are emitted through
 # dedicated sampling paths and are not plain array reads for
 # NA guard collection.
        if (!is.null(node$base) &&
            !is.null(node$base$kind) &&
            node$base$kind %in% c("sample_int", "sample")) {
            return(character(0))
        }
 # Array accesses could be NA
        idx_str <- .mojor_ir_index_emit(node, zero_based_vars, type_env, NULL, NULL)
        if (!is.null(idx_str)) {
            return(idx_str)
        }
    }

    if (node$kind == "binop") {
 # Recursively collect from operands
        lhs_sources <- .mojor_ir_collect_na_sources(node$lhs, zero_based_vars, type_env)
        rhs_sources <- .mojor_ir_collect_na_sources(node$rhs, zero_based_vars, type_env)
        return(c(lhs_sources, rhs_sources))
    }

    if (node$kind == "unop") {
 # Recursively collect from operand
        return(.mojor_ir_collect_na_sources(node$expr, zero_based_vars, type_env))
    }

    if (node$kind == "cast") {
 # Recursively collect from inner expression
        return(.mojor_ir_collect_na_sources(node$expr, zero_based_vars, type_env))
    }

 # Constants, calls, etc. - no NA sources to check
    character(0)
}

.mojor_ir_emit_na_guard <- function(
    node, indent = "    ", na_guard = "forbid", zero_based_vars = NULL,
    type_env = NULL, guard_cache = NULL
) {
 # Step 5.1: Added type_env parameter for logical array support
 # Step 16: Added guard_cache for CSE - avoids redundant NA
 # checks Emit NA guard checks for an expression Returns vector of
 # guard lines (empty if no guards needed)

    if (!.mojor_ir_needs_na_guard(node, na_guard)) {
        return(
            list(
                lines = character(0),
                guard_cache = guard_cache
            )
        )
    }

 # Collect expressions that need checking
    sources <- .mojor_ir_collect_na_sources(node, zero_based_vars, type_env)
    if (length(sources) ==
        0) {
        return(
            list(
                lines = character(0),
                guard_cache = guard_cache
            )
        )
    }

 # Step 16: Filter out sources that have already been checked
    if (!is.null(guard_cache)) {
        sources <- setdiff(sources, guard_cache)
        if (length(sources) ==
            0) {
            return(
                list(
                  lines = character(0),
                  guard_cache = guard_cache
              )
            )
        }
    }

 # Step 16: Remove duplicates within sources (CSE within single
 # expression)
    sources <- unique(sources)
    if (length(sources) ==
        0) {
        return(
            list(
                lines = character(0),
                guard_cache = guard_cache
            )
        )
    }

 # For forbid mode: emit error check
    if (na_guard == "forbid") {
 # Build condition: is_nan(x) || is_nan(y) || ...
        checks <- .mojor_ir_na_checks(sources, type_env)
        if (length(checks) ==
            0) {
            return(
                list(
                  lines = character(0),
                  guard_cache = guard_cache
              )
            )
        }

 # Step 16: Update guard cache with new checks
        new_cache <- if (is.null(guard_cache))
            sources else c(guard_cache, sources)

 # Emit guard lines
        lines <- c(
            paste0(
                indent, "if ", paste(checks, collapse = " or "),
                ":"
            ),
            paste0(indent, "    __mojor_na_flag[0] = 1")
        )
        return(list(lines = lines, guard_cache = new_cache))
    }

 # Other modes (assign, unsafe) - to be implemented in Step 4.2
    return(
        list(
            lines = character(0),
            guard_cache = guard_cache
        )
    )
}


# =============================================================================
# Step 4.3: Bounds Guards
# =============================================================================

.mojor_ir_collect_index_accesses <- function(node, zero_based_vars = NULL) {
 # Collect all array index accesses in an expression Returns a
 # list of index nodes that need bounds checking

    if (is.null(node) ||
        is.null(node$kind)) {
        return(list())
    }

    accesses <- list()

    if (node$kind == "index") {
 # This is an index access - add it to the list
        accesses <- list(node)
 # ALSO recursively collect from the index expressions
 # themselves For x[idx[i]], we need to check both idx[i] and
 # x[...]
        if (!is.null(node$indices)) {
            for (idx_node in node$indices) {
                if (!is.null(idx_node)) {
                  idx_accesses <- .mojor_ir_collect_index_accesses(idx_node, zero_based_vars)
                  accesses <- c(accesses, idx_accesses)
                }
            }
        }
    }

    if (node$kind == "binop") {
 # Recursively collect from operands
        lhs_accesses <- .mojor_ir_collect_index_accesses(node$lhs, zero_based_vars)
        rhs_accesses <- .mojor_ir_collect_index_accesses(node$rhs, zero_based_vars)
        accesses <- c(accesses, lhs_accesses, rhs_accesses)
    }

    if (node$kind == "unop") {
 # Recursively collect from operand
        accesses <- .mojor_ir_collect_index_accesses(node$expr, zero_based_vars)
    }

    if (node$kind == "cast") {
 # Recursively collect from inner expression
        accesses <- .mojor_ir_collect_index_accesses(node$expr, zero_based_vars)
    }

    if (node$kind == "call") {
 # Recursively collect from all arguments
        if (!is.null(node$args)) {
            for (arg in node$args) {
                if (!is.null(arg)) {
                  arg_accesses <- .mojor_ir_collect_index_accesses(arg, zero_based_vars)
                  accesses <- c(accesses, arg_accesses)
                }
            }
        }
    }

    if (node$kind == "ifelse") {
 # Recursively collect from condition, yes, and no branches
        cond_accesses <- .mojor_ir_collect_index_accesses(node$cond, zero_based_vars)
        yes_accesses <- .mojor_ir_collect_index_accesses(node$yes, zero_based_vars)
        no_accesses <- .mojor_ir_collect_index_accesses(node$no, zero_based_vars)
        accesses <- c(accesses, cond_accesses, yes_accesses, no_accesses)
    }

    if (node$kind == "rep") {
 # Recursively collect from rep() arguments
        x_accesses <- .mojor_ir_collect_index_accesses(node$x, zero_based_vars)
        accesses <- c(accesses, x_accesses)
        if (!is.null(node$times)) {
            times_accesses <- .mojor_ir_collect_index_accesses(node$times, zero_based_vars)
            accesses <- c(accesses, times_accesses)
        }
        if (!is.null(node$each)) {
            each_accesses <- .mojor_ir_collect_index_accesses(node$each, zero_based_vars)
            accesses <- c(accesses, each_accesses)
        }
        if (!is.null(node$length_out)) {
            len_accesses <- .mojor_ir_collect_index_accesses(node$length_out, zero_based_vars)
            accesses <- c(accesses, len_accesses)
        }
    }

    if (node$kind == "rep_len") {
 # Recursively collect from rep_len() arguments
        x_accesses <- .mojor_ir_collect_index_accesses(node$x, zero_based_vars)
        len_accesses <- .mojor_ir_collect_index_accesses(node$length_out, zero_based_vars)
        accesses <- c(accesses, x_accesses, len_accesses)
    }

    if (node$kind == "c") {
 # Recursively collect from c() parts
        if (!is.null(node$parts)) {
            for (part in node$parts) {
                if (!is.null(part)) {
                  part_accesses <- .mojor_ir_collect_index_accesses(part, zero_based_vars)
                  accesses <- c(accesses, part_accesses)
                }
            }
        }
    }

    accesses
}
