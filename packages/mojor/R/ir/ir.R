# Map R type spec (or Mojo shorthand) to Mojo scalar type name.
# Covers: i32, lgl, f64, f32, Int, Float64, Float32, and pass-through.
.mojor_r_to_mojo_type <- function(spec) {
    switch(spec,
        "i32" = "Int32",
        "lgl" = "Int32",
        "f64" = "Float64",
        "f32" = "Float32",
        "Int" = "Int",
        "Float64" = "Float64",
        "Float32" = "Float32",
        spec
    )
}

# Infer Mojo scalar type from a type_env entry (e.g. "f64[]" -> "Float64").
# Returns NULL if var not found or type unrecognized.
.mojor_type_env_to_mojo_type <- function(var_name, type_env) {
    if (is.null(type_env) || is.null(type_env[[var_name]]))
        return(NULL)
    spec <- .mojor_type_normalize(type_env[[var_name]])
    if (spec %in% c("f64", "f64[]", "f64[,]")) "Float64"
    else if (spec %in% c("f32", "f32[]", "f32[,]")) "Float32"
    else if (spec %in% c("i32", "i32[]", "i32[,]", "lgl", "lgl[]", "lgl[,]",
                          "bool", "bool[]", "bool[,]")) "Int32"
    else NULL
}

.mojor_ir_expr_emit_vec_chunks <- function(node, loop_var, type_env) {
    build <- function(n) {
        if (is.null(n) ||
            is.null(n$kind)) {
            return(NULL)
        }
        if (n$kind == "const") {
            return(list(expr = n$value, arrays = character(0)))
        }
        if (n$kind == "var") {
            if (!is.null(type_env)) {
                t <- type_env[[n$name]]
                if (!is.null(t) &&
                  .mojor_type_is_array(t)) {
                  return(NULL)
                }
            }
            return(list(expr = n$name, arrays = character(0)))
        }
        if (n$kind == "index") {
            if (is.null(n$base) ||
                is.null(n$base$kind) ||
                n$base$kind != "var") {
                return(NULL)
            }
            if (is.null(n$indices) ||
                length(n$indices) !=
                  1) {
                return(NULL)
            }
            idx <- n$indices[[1]]
            if (!is.null(idx$kind) &&
                idx$kind == "scalar_index")
                idx <- idx$expr
            if (is.null(idx) ||
                is.null(idx$kind) ||
                idx$kind != "var" || idx$name != loop_var) {
                return(NULL)
            }
            base_name <- n$base$name
            base_type <- if (!is.null(type_env))
                type_env[[base_name]] else NULL
            if (is.null(base_type) ||
                !(base_type %in% c("f64[]", "f32[]"))) {
                return(NULL)
            }
            return(
                list(
                  expr = paste0(base_name, "_chunk"),
                  arrays = base_name
              )
            )
        }
        if (n$kind == "unop") {
            if (n$op != "-") {
                return(NULL)
            }
            inner <- build(n$expr)
            if (is.null(inner)) {
                return(NULL)
            }
            return(
                list(
                  expr = paste0("(-", inner$expr, ")"),
                  arrays = inner$arrays
              )
            )
        }
        if (n$kind == "binop") {
            if (!(n$op %in% c("+", "-", "*", "/"))) {
                return(NULL)
            }
            lhs <- build(n$lhs)
            rhs <- build(n$rhs)
            if (is.null(lhs) ||
                is.null(rhs)) {
                return(NULL)
            }
            expr <- paste0("(", lhs$expr, " ", n$op, " ", rhs$expr, ")")
            return(list(expr = expr, arrays = unique(c(lhs$arrays, rhs$arrays))))
        }
        if (n$kind == "call") {
            unary <- c(
                "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh",
                "tanh", "log", "log1p", "log2", "log10", "exp", "expm1",
                "sqrt", "floor", "ceiling", "trunc", "round", "sign", "abs",
                "abs2", "cbrt", "lgamma", "erf", "gamma"
            )
            binary <- c("min", "max", "pmin", "pmax", "hypot", "atan2")
            if (n$fn %in% unary) {
                if (length(n$args) !=
                  1) {
                  return(NULL)
                }
                arg <- build(n$args[[1]])
                if (is.null(arg)) {
                  return(NULL)
                }
                if (n$fn == "abs2") {
                  return(
                    list(
                      expr = paste0("(", arg$expr, " * ", arg$expr, ")"),
                      arrays = arg$arrays
                  )
                )
                }
                mop <- if (n$fn == "ceiling")
                  "ceil" else n$fn
                return(
                  list(
                    expr = paste0(mop, "(", arg$expr, ")"),
                    arrays = arg$arrays
                )
              )
            }
            if (n$fn %in% binary) {
                if (length(n$args) !=
                  2) {
                  return(NULL)
                }
                a <- build(n$args[[1]])
                b <- build(n$args[[2]])
                if (is.null(a) ||
                  is.null(b)) {
                  return(NULL)
                }
                mop <- if (n$fn == "pmin")
                  "min" else if (n$fn == "pmax")
                  "max" else n$fn
                return(
                  list(
                    expr = paste0(mop, "(", a$expr, ", ", b$expr, ")"),
                    arrays = unique(c(a$arrays, b$arrays))
                )
              )
            }
            return(NULL)
        }
        NULL
    }
    out <- build(node)
    if (is.null(out)) {
        return(NULL)
    }
    out$arrays <- unique(out$arrays)
    out
}

.mojor_ir_expr_is_simple <- function(node, allow_cast = FALSE, allow_logic = FALSE) {
    if (is.null(node) ||
        is.null(node$kind)) {
        return(FALSE)
    }
    if (node$kind %in% c("const", "var")) {
        return(TRUE)
    }
    if (node$kind == "unop") {
        if (node$op == "!" && !allow_logic) {
            return(FALSE)
        }
        if (!(node$op %in% c("-", "!"))) {
            return(FALSE)
        }
        return(.mojor_ir_expr_is_simple(node$expr, allow_cast, allow_logic))
    }
    if (node$kind == "binop") {
        if (node$op %in% c("&&", "||") &&
            !allow_logic) {
            return(FALSE)
        }
        if (!(node$op %in% c(
            "+", "-", "*", "/", "%%", "%/%", ">", "<", ">=", "<=", "==",
            "!=", "&&", "||"
        ))) {
            return(FALSE)
        }
        return(
            .mojor_ir_expr_is_simple(node$lhs, allow_cast, allow_logic) &&
                .mojor_ir_expr_is_simple(node$rhs, allow_cast, allow_logic)
        )
    }
    if (node$kind == "cast") {
        if (!allow_cast) {
            return(FALSE)
        }
        return(.mojor_ir_expr_is_simple(node$expr, allow_cast, allow_logic))
    }
    FALSE
}

.mojor_ir_normalize_index_expr <- function(node, idx_vars, zero_based_vars = NULL) {
    if (is.null(node)) {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)
    needs_shift <- TRUE
    if (length(idx_vars) >
        0 && all(idx_vars %in% zero_based_vars)) {
        needs_shift <- FALSE
    }
    if (needs_shift) {
        one <- .mojor_ir_const("1")
        node <- .mojor_ir_binop("-", node, one)
    }
    if (!identical(node$kind, "cast") ||
        !identical(node$to, "Int")) {
        node <- .mojor_ir_cast("Int", node)
    }
    node
}

.mojor_ir_structured_range_emit <- function(range_node, type_env = NULL, zero_based_vars = NULL) {
    canonicalize_int_plus_one <- function(expr_str) {
        if (!is.character(expr_str) || length(expr_str) != 1L || !nzchar(expr_str)) {
            return(NULL)
        }
        m <- regexec("^\\(?\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\+\\s*1\\s*\\)?$", expr_str, perl = TRUE)
        g <- regmatches(expr_str, m)[[1]]
        if (length(g) == 2L && nzchar(g[[2L]])) {
            return(paste0("Int(", g[[2L]], ") + 1"))
        }
        NULL
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)
    start_str <- .mojor_ir_expr_emit(range_node$start, type_env = type_env)
    end_str <- .mojor_ir_expr_emit(range_node$end, type_env = type_env)
    if (is.null(start_str) ||
        is.null(end_str)) {
        return(NULL)
    }

    end_exclusive <- isTRUE(range_node$end_exclusive)
    step_str <- NULL
    if (!is.null(range_node$step)) {
        step_str <- .mojor_ir_expr_emit(range_node$step, type_env = type_env)
        if (is.null(step_str) ||
            step_str == "")
            step_str <- NULL
    }
    if (end_exclusive) {
        end_term <- canonicalize_int_plus_one(end_str)
        if (is.null(end_term)) {
            end_term <- paste0("Int(", end_str, ")")
        }
        range_str <- if (!is.null(step_str)) {
            paste0(
                "range(", start_str, ", ", end_term, ", ", step_str,
                ")"
            )
        } else {
            paste0("range(", start_str, ", ", end_term, ")")
        }
    } else {
 # Ascending 1-based range: R end is inclusive, Mojo range()
 # is exclusive
        range_str <- if (!is.null(step_str)) {
            # For negative steps, use end - 1 to include the end value
            # For positive steps, use end + 1 (standard behavior)
            end_adjustment <- paste0("(", end_str, " + 1)")
            if (grepl("-", step_str)) {
                end_adjustment <- paste0("(", end_str, " - 1)")
            }
            end_term <- canonicalize_int_plus_one(end_adjustment)
            if (is.null(end_term)) {
                end_term <- paste0("Int(", end_adjustment, ")")
            }
            paste0(
                "range(", start_str, ", ", end_term, ", ", step_str,
                ")"
            )
        } else {
            paste0("range(", start_str, ", Int(", end_str, ") + 1)")
        }
    }

    start_is_zero_based <- function(node) {
        if (is.null(node) || is.null(node$kind)) {
            return(FALSE)
        }
        if (node$kind == "const") {
            start_val <- as.character(node$value)
            return(!is.na(start_val) && start_val == "0")
        }
        if (node$kind == "var") {
            return(!is.null(node$name) && node$name %in% zero_based_vars)
        }
        if (node$kind == "cast" && !is.null(node$expr)) {
            return(start_is_zero_based(node$expr))
        }
        FALSE
    }
    loop_var_is_zero_based <- isTRUE(end_exclusive) &&
        start_is_zero_based(range_node$start)

    is_simple_1_based <- !end_exclusive
    if (!is.null(step_str) &&
        step_str != "1") {
        is_simple_1_based <- FALSE
    }

    list(
        range = range_str, loop_var_is_zero_based = loop_var_is_zero_based,
        is_simple_1_based = is_simple_1_based, start = start_str, end = end_str
    )
}

.mojor_ir_contains_blocked_elementwise_call <- function(node, blocked_fns) {
    if (is.null(node) ||
        length(blocked_fns) ==
            0) {
        return(FALSE)
    }
    if (is.list(node)) {
        if (!is.null(node$kind) &&
            identical(node$kind, "call")) {
            fn <- if (!is.null(node$fn))
                as.character(node$fn) else ""
            if (length(fn) ==
                1 && nzchar(fn) &&
                fn %in% blocked_fns) {
                return(TRUE)
            }
        }
        for (child in node) {
            if (.mojor_ir_contains_blocked_elementwise_call(child, blocked_fns)) {
                return(TRUE)
            }
        }
    }
    FALSE
}

.mojor_ir_elementwise_emit <- function(
    node, ew, indent = "    ", zero_based_vars = NULL, out_name = NULL,
    na_guard = "forbid", bounds_check = FALSE, loop_var = NULL, scalar_name = NULL,
    type_env = NULL, schedule = NULL
) {
    if (is.null(node) ||
        node$kind != "loop") {
        return(NULL)
    }
    if (is.null(ew) ||
        !is.list(ew)) {
        return(NULL)
    }
    if (!isTRUE(ew$enabled)) {
        return(NULL)
    }
    target <- if (!is.null(ew$target))
        ew$target else "cpu"
    if (!identical(target, "cpu")) {
        return(NULL)
    }
    if (is.null(out_name) ||
        is.null(type_env)) {
        return(NULL)
    }
    out_type <- type_env[[out_name]]
    if (is.null(out_type) ||
        !(out_type %in% c("f64[]", "f32[]"))) {
        return(NULL)
    }
    if (is.null(node$body) ||
        is.null(node$body$stmts)) {
        return(NULL)
    }
 # Some libm-backed ops fail in this elementwise wrapper path
 # under current toolchains. Use regular loop emission for those
 # ops.
    if (.mojor_ir_contains_blocked_elementwise_call(node$body, c("tan", "expm1"))) {
        return(NULL)
    }
    range_info <- .mojor_ir_simple_range_emit(
        node$range,
        type_env,
        zero_based_vars = zero_based_vars
    )
    if (is.null(range_info)) {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

    dtype_tag <- if (out_type == "f32[]")
        "float32" else "float64"
    loop_var_name <- node$var
    kernel_indent <- paste0(indent, "    ")
    kernel_zero_based <- unique(c(zero_based_vars, loop_var_name))
    fallback_zero_based <- zero_based_vars
    if (isTRUE(range_info$loop_var_is_zero_based)) {
        fallback_zero_based <- unique(c(fallback_zero_based, loop_var_name))
    }

    lines <- c(
        paste0(indent, "@parameter"),
        paste0(indent, "@always_inline"),
        paste0(
            indent, "fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
        ),
        paste0(kernel_indent, "var ", loop_var_name, " = Int(indices[0])")
    )
    for (stmt in node$body$stmts) {
        stmt_lines <- .mojor_ir_stmt_emit(
            stmt, indent = kernel_indent, zero_based_vars = kernel_zero_based,
            out_name = out_name, na_guard = na_guard, bounds_check = bounds_check,
            loop_var = loop_var_name, scalar_name = scalar_name, type_env = type_env,
            unroll = NULL, schedule = schedule
        )
        if (is.null(stmt_lines) ||
            length(stmt_lines) ==
                0 || any(!nzchar(stmt_lines))) {
            return(NULL)
        }
        lines <- c(lines, stmt_lines)
    }

    lines <- c(
        lines, paste0(indent, "@parameter"),
        paste0(indent, "@always_inline"),
        paste0(
            indent, "fn _mojor_elementwise_", dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
        ),
        paste0(
            kernel_indent, "_mojor_elementwise[DType.", dtype_tag, ", simd_width, rank, alignment](indices)"
        ),
        paste0(
            indent, "comptime _MOJOR_EW_SIMD = simd_width_of[DType.", dtype_tag,
            "]()"
        ),
        paste0(indent, "try:"),
        paste0(
            indent, "    elementwise[_mojor_elementwise_", dtype_tag, ", _MOJOR_EW_SIMD, target=\"cpu\"](n_i)"
        ),
        paste0(indent, "except:"),
        paste0(
            indent, "    for ", loop_var_name, " in ", range_info$range,
            ":"
        )
    )
    fallback_indent <- paste0(indent, "        ")
    for (stmt in node$body$stmts) {
        stmt_lines <- .mojor_ir_stmt_emit(
            stmt, indent = fallback_indent, zero_based_vars = fallback_zero_based,
            out_name = out_name, na_guard = na_guard, bounds_check = bounds_check,
            loop_var = loop_var_name, scalar_name = scalar_name, type_env = type_env,
            unroll = NULL, schedule = schedule
        )
        if (is.null(stmt_lines) ||
            length(stmt_lines) ==
                0 || any(!nzchar(stmt_lines))) {
            return(NULL)
        }
        lines <- c(lines, stmt_lines)
    }
    lines
}

.mojor_ir_loop_hoist_guards <- function(
    loop_node, indent = "    ", zero_based_vars = NULL, na_guard = "forbid",
    bounds_check = FALSE, type_env = NULL
) {
 # Hoist loop-invariant guard checks to a loop preheader region.
 # Conservative rule: only hoist top-level assignment RHS checks
 # that do not reference the loop variable or any variable
 # assigned in the loop body.
    out <- list(
        lines = character(0),
        bounds_cache = NULL, na_cache = NULL
    )
    if (is.null(loop_node) ||
        is.null(loop_node$body) ||
        loop_node$body$kind != "block") {
        return(out)
    }
    if (is.null(loop_node$body$stmts) ||
        length(loop_node$body$stmts) ==
            0) {
        return(out)
    }

    loop_var <- loop_node$var
    assigned_vars <- setdiff(
        .mojor_ir_collect_assigned_vars(loop_node$body),
        loop_var
    )
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

    for (stmt in loop_node$body$stmts) {
        if (is.null(stmt) ||
            is.null(stmt$kind) ||
            stmt$kind != "assign")
            next
        rhs <- stmt$rhs
        if (is.null(rhs) ||
            is.null(rhs$kind))
            next
        if (.mojor_ir_references_var(rhs, loop_var))
            next

        depends_on_mutated <- FALSE
        if (length(assigned_vars) >
            0) {
            for (v in assigned_vars) {
                if (.mojor_ir_references_var(rhs, v)) {
                  depends_on_mutated <- TRUE
                  break
                }
            }
        }
        if (depends_on_mutated)
            next

        if (isTRUE(bounds_check)) {
            b <- .mojor_ir_emit_bounds_guards(
                rhs, indent = indent, zero_based_vars = zero_based_vars,
                bounds_check = TRUE, loop_var = NULL, guard_cache = out$bounds_cache
            )
            out$lines <- c(out$lines, b$lines)
            out$bounds_cache <- b$guard_cache
        }

        if (identical(na_guard, "forbid") &&
            .mojor_ir_needs_na_guard(rhs, na_guard)) {
            sources <- .mojor_ir_collect_na_sources(rhs, zero_based_vars, type_env)
 # Skip scalar-variable NA checks; they may change across
 # iterations.
            if (length(sources) >
                0) {
                scalar_only <- grepl("^[A-Za-z_][A-Za-z0-9_]*$", sources)
                if (!any(scalar_only)) {
                  n <- .mojor_ir_emit_na_guard(
                    rhs, indent = indent, na_guard = na_guard, zero_based_vars = zero_based_vars,
                    type_env = type_env, guard_cache = out$na_cache
                )
                  out$lines <- c(out$lines, n$lines)
                  out$na_cache <- n$guard_cache
                }
            }
        }
    }

    if (!is.null(out$bounds_cache))
        out$bounds_cache <- unique(out$bounds_cache)
    if (!is.null(out$na_cache))
        out$na_cache <- unique(out$na_cache)
    out
}

.mojor_ir_while_emit <- function(
    node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid",
    bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL,
    schedule = NULL
) {
 # Step 8.2: While loop emission
    if (is.null(node) ||
        node$kind != "while") {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

 # Emit condition
    cond_str <- .mojor_ir_expr_emit(node$cond, zero_based_vars, type_env, loop_vars = loop_var)
    if (is.null(cond_str)) {
        return(NULL)
    }

 # Type-based coercion (mirror if-statement logic)
    cond_type <- if (!is.null(node$cond$type))
        node$cond$type else "unknown"
    if (cond_type != "bool" && cond_type != "unknown") {
 # Non-boolean condition needs coercion
        cond_str <- paste0("(", cond_str, " != 0)")
    }

 # Emit header
    while_header <- paste0(indent, "while ", cond_str, ":")

 # Emit body (no loop_var for while - all variables need bounds
 # checks)
    body_indent <- paste0(indent, "    ")
    body_lines <- .mojor_ir_block_emit(
        node$body, body_indent, zero_based_vars, out_name, na_guard, bounds_check,
        NULL, scalar_name, type_env, schedule
    )
    if (is.null(body_lines)) {
        return(NULL)
    }

 # Combine
    c(while_header, body_lines)
}

.mojor_ir_break_emit <- function(node, indent = "    ") {
 # Step 8.2: Break statement emission
    if (is.null(node) ||
        node$kind != "break") {
        return(NULL)
    }
    paste0(indent, "break")
}

.mojor_ir_next_emit <- function(node, indent = "    ") {
 # Step 8.2: Next statement emission (R next <U+2192> Mojo
 # continue)
    if (is.null(node) ||
        node$kind != "next") {
        return(NULL)
    }
    paste0(indent, "continue")
}

# Step 19: Scalar reduction emitter
# (sum/product/min/max/which.min/which.max) NOTE: sum/product/min/max
# should be lowered before emit (HIR <U+2192> LIR).
# which.min/which.max may also be lowered to scheduled_reduce (tree)
# before emit. Remaining which.* paths still use this fallback
# emitter.
.mojor_ir_scalar_reduce_emit <- function(node, indent = "    ", scalar_name = NULL, type_env = NULL) {
    if (is.null(node) ||
        node$kind != "scalar_reduce") {
        return(NULL)
    }

    op <- node$op

 # Step 20: sum/product/min/max are lowered, should not reach
 # emit
    if (op %in% c("sum", "product", "min", "max")) {
        stop(
            sprintf(
                paste0(
                  "IR emit error: scalar_reduce (sum/product/min/max) reached emit without being lowered. ",
                  "This is a bug in the lowering pass. Op: %s, Acc: %s, Arg: %s"
              ),
                op, node$acc, node$arg
            )
        )
    }

 # which.min/which.max still use emit path (Step 20 limitation)
    acc <- node$acc
    arg <- node$arg
    lv <- "__mojor_ri"  # private loop variable
    i1 <- paste0(indent, "    ")
    i2 <- paste0(indent, "        ")

 # Determine NaN/init sentinel from type_env
    arg_type <- if (!is.null(type_env))
        type_env[[arg]] else NULL
    is_f32 <- identical(arg_type, "f32[]")

    if (op %in% c("min", "max")) {
        nan_val <- if (is_f32)
            "_MOJOR_NAN_F32" else "_MOJOR_NAN"
        init_val <- if (op == "min") {
            if (is_f32)
                "Float32(1.0/0.0)" else "_MOJOR_INF"
        } else {
            if (is_f32)
                "Float32(-1.0/0.0)" else "_MOJOR_NINF"
        }
        c(
            paste0(indent, "if n_i == 0:"),
            paste0(i1, acc, " = ", nan_val),
            paste0(indent, "else:"),
            paste0(i1, acc, " = ", arg, "[0]"),
            paste0(i1, "for ", lv, " in range(2, n_i + 1):"),
            paste0(i2, acc, " = ", op, "(", acc, ", ", arg, "[", lv, " - 1])")
        )
    } else if (op %in% c("which.min", "which.max")) {
        want_min <- op == "which.min"
 # Determine arg Mojo type
        mojo_ty <- if (is_f32)
            "Float32" else "Float64"
        is_i32_like <- !is.null(arg_type) &&
            grepl("^(i32|lgl|bool)", arg_type)
        if (is_i32_like)
            mojo_ty <- "Int32"
        zero_v <- if (mojo_ty %in% c("Float64", "Float32"))
            "0.0" else "0"
        cmp <- if (want_min)
            "v < best" else "v > best"
        read_expr <- paste0(arg, "[", lv, " - 1]")
        if (!is.null(arg_type) &&
            grepl("^(lgl|bool)", arg_type)) {
            read_expr <- paste0("Int32(", read_expr, ")")
        }
        c(
            paste0(indent, "if n_i == 0:"),
            paste0(i1, acc, " = Int32(0)"),
            paste0(indent, "else:"),
            paste0(i1, "var best: ", mojo_ty, " = ", zero_v),
            paste0(i1, "var has_value = False"),
            paste0(i1, acc, " = Int32(0)"),
            paste0(i1, "for ", lv, " in range(1, n_i + 1):"),
            paste0(i2, "var v: ", mojo_ty, " = ", read_expr),
            paste0(i2, "if not has_value:"),
            paste0(i2, "    best = v"),
            paste0(i2, "    ", acc, " = Int32(", lv, ")"),
            paste0(i2, "    has_value = True"),
            paste0(i2, "elif ", cmp, ":"),
            paste0(i2, "    best = v"),
            paste0(i2, "    ", acc, " = Int32(", lv, ")")
        )
    } else {
        NULL
    }
}

.mojor_ir_return_emit <- function(
    node, indent = "    ", out_name = NULL, scalar_name = NULL, zero_based_vars = NULL,
    type_env = NULL
) {
 # Step 8.8: Return statement emission
    if (is.null(node) ||
        node$kind != "return") {
        return(NULL)
    }
    if (is.null(node$value)) {
        return(NULL)
    }

    if (!is.null(node$value$kind) &&
        node$value$kind == "var") {
        ret_name <- node$value$name
        if (!is.null(out_name) &&
            ret_name == out_name) {
            return(paste0(indent, "return"))
        }
        if (!is.null(scalar_name) &&
            ret_name == scalar_name) {
            return(
                c(
                  paste0(indent, scalar_name, "_out[0] = ", scalar_name),
                  paste0(indent, "return")
              )
            )
        }
    }

    NULL
}

.mojor_ir_repeat_emit <- function(
    node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid",
    bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL,
    schedule = NULL
) {
 # Step 8.7: Repeat loop emission (R repeat <U+2192> Mojo while
 # True)
    if (is.null(node) ||
        node$kind != "repeat") {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

 # Emit header: while True:
    repeat_header <- paste0(indent, "while True:")

 # Emit body (no loop_var for repeat - all variables need bounds
 # checks)
    body_indent <- paste0(indent, "    ")
    body_lines <- .mojor_ir_block_emit(
        node$body, body_indent, zero_based_vars, out_name, na_guard, bounds_check,
        NULL, scalar_name, type_env, schedule
    )
    if (is.null(body_lines)) {
        return(NULL)
    }

 # Combine
    c(repeat_header, body_lines)
}

.mojor_ir_if_emit <- function(
    node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid",
    bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL,
    schedule = NULL
) {
 # Step 4.4: Added loop_var parameter for guard optimization
 # Step 5.1: Added type_env parameter for logical array support
    if (is.null(node) ||
        node$kind != "if") {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

 # Emit condition expression
    cond_str <- .mojor_ir_expr_emit(node$cond, zero_based_vars, type_env, loop_vars = loop_var)
    if (is.null(cond_str))
        {
            return(NULL)
        }  # Fallback for complex conditions

 # Determine if boolean coercion is needed Coerce when the
 # condition is not boolean-typed and not a comparison
    cond_type <- if (!is.null(node$cond$type))
        node$cond$type else .mojor_ir_infer_type(node$cond, type_env)
    cond_is_bool_ifelse <- FALSE
    if (!is.null(node$cond) &&
        !is.null(node$cond$kind) &&
        identical(node$cond$kind, "ifelse")) {
        yes_type <- .mojor_ir_infer_type(node$cond$yes, type_env)
        no_type <- .mojor_ir_infer_type(node$cond$no, type_env)
        cond_is_bool_ifelse <- (yes_type %in% c("bool", "lgl")) &&
            (no_type %in% c("bool", "lgl"))
    }
    needs_coercion <- !identical(cond_type, "bool") &&
        !identical(cond_type, "lgl") &&
        !cond_is_bool_ifelse && !.mojor_ir_expr_is_comparison(node$cond)
    if (needs_coercion) {
        cond_str <- paste0("(", cond_str, " != 0)")
    }

 # Emit if header
    if_header <- paste0(indent, "if ", cond_str, ":")

 # Emit then block
    then_indent <- paste0(indent, "    ")
    then_lines <- .mojor_ir_block_emit(
        node$then, then_indent, zero_based_vars, out_name, na_guard, bounds_check,
        loop_var, scalar_name, type_env, schedule
    )
    if (is.null(then_lines))
        {
            return(NULL)
        }  # Fallback if then block can't be emitted

    lines <- c(if_header, then_lines)

 # Emit else block if present
    if (!is.null(node$else_block)) {
        else_header <- paste0(indent, "else:")
        else_lines <- .mojor_ir_block_emit(
            node$else_block, then_indent, zero_based_vars, out_name, na_guard,
            bounds_check, loop_var, scalar_name, type_env, schedule
        )
        if (is.null(else_lines))
            {
                return(NULL)
            }  # Fallback if else block can't be emitted
        lines <- c(lines, else_header, else_lines)
    }

    lines
}

.mojor_ir_expr_is_comparison <- function(node) {
 # Check if an expression is a comparison operator
    if (is.null(node) ||
        is.null(node$kind)) {
        return(FALSE)
    }
    if (node$kind == "binop" && node$op %in% c(">", "<", ">=", "<=", "==", "!=")) {
        return(TRUE)
    }
    if (node$kind == "binop" && node$op %in% c("&&", "||")) {
 # Logical operators are boolean
        return(TRUE)
    }
    if (node$kind == "unop" && node$op == "!") {
 # Logical NOT is boolean
        return(TRUE)
    }
    FALSE
}

# Step 8.5: Slice assignment emission NOTE: Step 22 - Most slice
# cases should be lowered before reaching emit. This function
# handles remaining edge cases and legacy fallbacks.
.mojor_ir_mask_extract_assign_emit <- function(node, indent, zero_based_vars, type_env, loop_var) {
 # Step 8.12: Emit mask extraction assignment: out <- x[mask]
 # Lowers to: for j in range(mask_len): if mask[j]: out[k] = x[j];
 # k += 1 Skips NA in mask (treat as FALSE)

    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) ||
        lhs$kind != "var") {
        return(NULL)
    }

    cast_to <- NULL
    if (!is.null(rhs$kind) &&
        rhs$kind == "cast") {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) {
        cast_to <- NULL
    }
    if (is.null(rhs$kind) ||
        rhs$kind != "index") {
        return(NULL)
    }
    if (length(rhs$indices) !=
        1) {
        return(NULL)
    }

    idx_node <- rhs$indices[[1]]
    if (idx_node$kind != "var") {
        return(NULL)
    }

    mask_name <- idx_node$name
    mask_type <- if (!is.null(type_env))
        type_env[[mask_name]] else NULL
    if (is.null(mask_type) ||
        !.mojor_is_logical_mask_type(mask_type)) {
        return(NULL)
    }

    base_name <- NULL
    if (rhs$base$kind == "var") {
        base_name <- rhs$base$name
    } else {
        return(NULL)
    }

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    mask_len_var <- if (!is.null(len_var_map) &&
        mask_name %in% names(len_var_map)) {
        len_var_map[[mask_name]]
    } else if (!is.null(n_source_name) &&
        identical(mask_name, n_source_name)) {
        "n_i"
    } else {
        "n_i"
    }

    base_len_var <- if (!is.null(len_var_map) &&
        base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) &&
        identical(base_name, n_source_name)) {
        "n_i"
    } else {
        "n_i"
    }

    used_names <- unique(
        c(
            base_name, mask_name, lhs$name, if (!is.null(type_env)) names(type_env) else character(0),
            if (!is.null(loop_var)) loop_var else character(0)
        )
    )
    loop_j <- .mojor_unique_loop_var("__mojor_mask_j", used_names)
    mask_val <- .mojor_unique_loop_var("__mojor_mask_val", used_names)
    out_k <- .mojor_unique_loop_var("__mojor_out_k", used_names)

    base_type <- if (!is.null(type_env))
        type_env[[base_name]] else NULL
    if (!is.null(base_type) &&
        base_type %in% c("lgl[]", "bool[]")) {
        value_str <- .mojor_ir_read_call(
            "_mojor_read_lgl",
            base_name,
            loop_j,
            paste0("Int(", base_len_var, ")")
        )
    } else {
        value_str <- paste0(base_name, "[", loop_j, "]")
    }

    if (!is.null(cast_to)) {
        mojo_type <- .mojor_r_to_mojo_type(cast_to)
        value_str <- paste0(mojo_type, "(", value_str, ")")
    }

    lines <- c(
        paste0(indent, "var ", out_k, ": Int = 0"),
        paste0(indent, "for ", loop_j, " in range(Int(", mask_len_var, ")):"),
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
        paste0(indent, "    ", lhs$name, "[", out_k, "] = ", value_str),
        paste0(indent, "    ", out_k, " += 1")
    )
    lines
}

# Emit positive index subset: out <- x[idx]
# Supports i32[] selector vars and lowered positive selection markers.
.mojor_ir_positive_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, out_name = NULL
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "var") return(NULL)

    cast_to <- NULL
    if (!is.null(rhs$kind) && rhs$kind == "cast") {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) cast_to <- NULL

    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    if (length(rhs$indices) != 1) return(NULL)
    if (is.null(rhs$base) || rhs$base$kind != "var") return(NULL)

    base_name <- rhs$base$name
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    idx_node <- rhs$indices[[1]]

    # Defer to dedicated negative/mask handlers.
    if (!is.null(idx_node$neg_exclusion) || !is.null(idx_node$neg_vec_exclusion)) {
        return(NULL)
    }
    if (identical(idx_node$kind, "var")) {
        idx_type <- if (!is.null(type_env)) type_env[[idx_node$name]] else NULL
        if (!is.null(idx_type) && idx_type %in% c("lgl[]", "bool[]")) {
            return(NULL)
        }
    }

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    base_len_var <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        "n_i"
    }

    idx_len_expr <- NULL
    idx0_expr_for <- NULL
    idx_used_names <- character(0)
    dynamic_i32_selector_name <- NULL
    dynamic_i32_selector_len_var <- NULL

    if (is.list(idx_node) && identical(idx_node$kind, "vec_index") && !is.null(idx_node$expr)) {
        idx_node <- idx_node$expr
    }
    if (is.list(idx_node) && identical(idx_node$kind, "var") && !is.null(idx_node$pos_vec_selection)) {
        sel <- idx_node$pos_vec_selection
        if (is.null(sel$type) || !identical(sel$type, "c") || is.null(sel$indices_0) || length(sel$indices_0) == 0) {
            return(NULL)
        }
        idx_len_expr <- paste0("Int(", length(sel$indices_0), ")")
        idx0_expr_for <- function(loop_j) {
            .mojor_build_ternary_chain(sel$indices_0, loop_j)
        }
    } else if (is.list(idx_node) && identical(idx_node$kind, "c") && is.list(idx_node$parts)) {
        sel_parts <- idx_node$parts
        if (length(sel_parts) == 0L) {
            return(NULL)
        }
        indices_0 <- character(0)
        for (part in sel_parts) {
            if (is.list(part) && identical(part$kind, "const")) {
                val <- suppressWarnings(as.integer(as.numeric(part$value)))
                if (is.na(val)) {
                    return(NULL)
                }
                if (val == 0L) {
                    next
                }
                indices_0 <- c(indices_0, as.character(val - 1L))
            } else if (is.list(part) && identical(part$kind, "var") && is.character(part$name) && nzchar(part$name)) {
                indices_0 <- c(indices_0, paste0("(Int(", part$name, ") - 1)"))
            } else {
                return(NULL)
            }
        }
        if (length(indices_0) == 0L) {
            return(NULL)
        }
        idx_len_expr <- paste0("Int(", length(indices_0), ")")
        idx0_expr_for <- function(loop_j) {
            .mojor_build_ternary_chain(indices_0, loop_j)
        }
    } else if (is.list(idx_node) && identical(idx_node$kind, "var")) {
        idx_name <- idx_node$name
        idx_type <- if (!is.null(type_env)) type_env[[idx_name]] else NULL
        if (is.null(idx_type) || !identical(idx_type, "i32[]")) {
            return(NULL)
        }
        idx_len_var <- if (!is.null(len_var_map) && idx_name %in% names(len_var_map)) {
            len_var_map[[idx_name]]
        } else if (!is.null(n_source_name) && identical(idx_name, n_source_name)) {
            "n_i"
        } else {
            "n_i"
        }
        idx_len_expr <- paste0("Int(", idx_len_var, ")")
        idx0_expr_for <- function(loop_j) {
            paste0(
                "Int(",
                .mojor_ir_read_call(
                    "_mojor_read_i32",
                    idx_name,
                    loop_j,
                    paste0("Int(", idx_len_var, ")")
                ),
                " - 1)"
            )
        }
        dynamic_i32_selector_name <- idx_name
        dynamic_i32_selector_len_var <- idx_len_var
        idx_used_names <- c(idx_used_names, idx_name)
    } else if (is.list(idx_node) && identical(idx_node$kind, "slice_index")) {
        start_expr <- .mojor_ir_expr_emit(idx_node$start, zero_based_vars, type_env, loop_var)
        end_expr <- .mojor_ir_expr_emit(idx_node$end, zero_based_vars, type_env, loop_var)
        if (is.null(start_expr) || is.null(end_expr)) {
            return(NULL)
        }
        idx_len_expr <- paste0("(Int(", end_expr, ") - Int(", start_expr, ") + 1)")
        idx0_expr_for <- function(loop_j) {
            paste0("(Int(", start_expr, ") - 1 + ", loop_j, ")")
        }
    } else if (is.list(idx_node) && identical(idx_node$kind, "missing_index")) {
        base_ndim <- if (!is.null(base_type)) .mojor_type_ndim(base_type) else NA_integer_
        if (is.na(base_ndim) || base_ndim != 1L) {
            return(NULL)
        }
        idx_len_expr <- paste0("Int(", base_len_var, ")")
        idx0_expr_for <- function(loop_j) loop_j
    } else {
        return(NULL)
    }

    if (is.null(idx_len_expr) || is.null(idx0_expr_for)) {
        return(NULL)
    }

    read_helper <- "_mojor_read_f64"
    if (!is.null(base_type)) {
        base_elem <- .mojor_type_base(base_type)
        if (base_elem %in% c("i32")) {
            read_helper <- "_mojor_read_i32"
        } else if (base_elem %in% c("lgl", "bool")) {
            read_helper <- "_mojor_read_lgl"
        } else if (identical(base_elem, "f32")) {
            read_helper <- "_mojor_read_f32"
        }
    }

    lhs_len_var <- if (isTRUE(!is.null(out_name) && identical(lhs$name, out_name))) {
        "n_i"
    } else if (!is.null(len_var_map) && lhs$name %in% names(len_var_map)) {
        len_var_map[[lhs$name]]
    } else {
        paste0("n_", lhs$name, "_i")
    }
    reuse_existing_out <- isTRUE(!is.null(out_name) && identical(lhs$name, out_name))
    in_loop_context <- !is.null(loop_var) && length(loop_var) >= 1L && nzchar(as.character(loop_var[[1L]]))
    need_lhs_alloc <- !reuse_existing_out && (isTRUE(in_loop_context) || is.null(len_var_map) || !(lhs$name %in% names(len_var_map)))
    alloc_lines <- character(0)
    if (need_lhs_alloc) {
        if (is.null(.mojor_state$current_len_var_map)) {
            .mojor_state$current_len_var_map <- list()
        }
        .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var
        lhs_mojo_type <- .mojor_type_env_to_mojo_type(lhs$name, type_env)
        if (is.null(lhs_mojo_type) && !is.null(base_type)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(base_type))
        }
        if (is.null(lhs_mojo_type) && !is.null(cast_to)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(cast_to))
        }
        if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
            lhs_mojo_type <- "Float64"
        }
        alloc_lines <- c(
            paste0(indent, "var ", lhs_len_var, " = ", idx_len_expr),
            paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")")
        )
    }

    used_names <- unique(c(
        base_name, lhs$name, idx_used_names,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    loop_j <- .mojor_unique_loop_var("__mojor_pos_j", used_names)
    idx0_expr <- idx0_expr_for(loop_j)
    loop_pre_lines <- character(0)
    if (!is.null(dynamic_i32_selector_name) && !is.null(dynamic_i32_selector_len_var)) {
        idx_raw_var <- .mojor_unique_loop_var("__mojor_idx_raw", c(used_names, loop_j))
        idx_zero_based_var <- .mojor_unique_loop_var("__mojor_idx0", c(used_names, loop_j, idx_raw_var))
        loop_pre_lines <- c(
            paste0(
                indent, "    var ", idx_raw_var, ": Int32 = ",
                .mojor_ir_read_call(
                    "_mojor_read_i32",
                    dynamic_i32_selector_name,
                    loop_j,
                    paste0("Int(", dynamic_i32_selector_len_var, ")")
                )
            ),
            paste0(indent, "    if ", idx_raw_var, " == -2147483648 or ", idx_raw_var, " <= 0:"),
            paste0(indent, "        _mojor_oob()"),
            paste0(indent, "    var ", idx_zero_based_var, ": Int = Int(", idx_raw_var, " - 1)")
        )
        idx0_expr <- idx_zero_based_var
    }
    value_str <- .mojor_ir_read_call(
        read_helper,
        base_name,
        idx0_expr,
        paste0("Int(", base_len_var, ")")
    )

    if (!is.null(cast_to)) {
        mojo_type <- .mojor_r_to_mojo_type(cast_to)
        value_str <- paste0(mojo_type, "(", value_str, ")")
    }

    c(
        alloc_lines,
        paste0(indent, "for ", loop_j, " in range(", idx_len_expr, "):"),
        loop_pre_lines,
        paste0(indent, "    ", lhs$name, "[", loop_j, "] = ", value_str)
    )
}

# Emit matrix row/column extraction to vector: lhs <- mat[row, ] or lhs <- mat[, col]
# Handles exactly one missing index and one scalar index on 2D inputs.
.mojor_ir_matrix_missing_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, out_name = NULL
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "var") return(NULL)

    cast_to <- NULL
    if (!is.null(rhs$kind) && rhs$kind == "cast" && !is.null(rhs$expr)) {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) cast_to <- NULL

    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    if (length(rhs$indices) != 2L) return(NULL)
    if (is.null(rhs$base) || rhs$base$kind != "var") return(NULL)

    base_name <- rhs$base$name
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    if (is.null(base_type) || .mojor_type_ndim(base_type) != 2L) return(NULL)

    idx1 <- rhs$indices[[1L]]
    idx2 <- rhs$indices[[2L]]
    is_missing1 <- is.list(idx1) && identical(idx1$kind, "missing_index")
    is_missing2 <- is.list(idx2) && identical(idx2$kind, "missing_index")
    if (is_missing1 == is_missing2) return(NULL)

    scalar_idx <- if (is_missing1) idx2 else idx1
    if (is.null(scalar_idx$kind) || !(scalar_idx$kind %in% c("const", "var"))) return(NULL)
    if (!is.null(scalar_idx$neg_exclusion) || !is.null(scalar_idx$neg_vec_exclusion) ||
        !is.null(scalar_idx$pos_vec_selection)) {
        return(NULL)
    }

    index_base <- if (!is.null(rhs$index_base)) rhs$index_base else "one_based"
    scalar_idx_norm <- scalar_idx
    if (identical(index_base, "one_based")) {
        scalar_idx_norm <- .mojor_ir_binop("-", scalar_idx_norm, .mojor_ir_const("1"))
        scalar_idx_norm$`__mojor_index_normalized` <- TRUE
    }

    resolve_matrix_dim_var <- function(var_name, axis_idx) {
        out_var_name <- .mojor_state$current_out_name
        is_out <- !is.null(out_var_name) && identical(var_name, out_var_name)
        if (axis_idx == 1L) {
            if (is_out && !is.null(.mojor_state$current_out_nrow_var)) {
                return(.mojor_state$current_out_nrow_var)
            }
            nrow_map <- .mojor_state$current_nrow_var_map
            if (!is.null(nrow_map) && !is.null(nrow_map[[var_name]])) {
                return(nrow_map[[var_name]])
            }
            local_matrix_dims <- .mojor_state$current_local_matrix_dims
            if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[var_name]])) {
                return(paste0("nrow_", var_name))
            }
            return(NULL)
        }
        if (axis_idx == 2L) {
            if (is_out && !is.null(.mojor_state$current_out_ncol_var)) {
                return(.mojor_state$current_out_ncol_var)
            }
            ncol_map <- .mojor_state$current_ncol_var_map
            if (!is.null(ncol_map) && !is.null(ncol_map[[var_name]])) {
                return(ncol_map[[var_name]])
            }
            local_matrix_dims <- .mojor_state$current_local_matrix_dims
            if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[var_name]])) {
                return(paste0("ncol_", var_name))
            }
            return(NULL)
        }
        NULL
    }

    varying_axis <- if (is_missing1) 1L else 2L
    varying_dim_var <- resolve_matrix_dim_var(base_name, varying_axis)
    if (is.null(varying_dim_var) || !nzchar(varying_dim_var)) return(NULL)

    len_var_map <- .mojor_state$current_len_var_map
    lhs_len_var <- if (isTRUE(!is.null(out_name) && identical(lhs$name, out_name))) {
        "n_i"
    } else if (!is.null(len_var_map) && lhs$name %in% names(len_var_map)) {
        len_var_map[[lhs$name]]
    } else {
        paste0("n_", lhs$name, "_i")
    }
    reuse_existing_out <- isTRUE(!is.null(out_name) && identical(lhs$name, out_name))
    in_loop_context <- !is.null(loop_var) && length(loop_var) >= 1L && nzchar(as.character(loop_var[[1L]]))
    need_lhs_alloc <- !reuse_existing_out && (isTRUE(in_loop_context) || is.null(len_var_map) || !(lhs$name %in% names(len_var_map)))

    alloc_lines <- character(0)
    if (need_lhs_alloc) {
        if (is.null(.mojor_state$current_len_var_map)) {
            .mojor_state$current_len_var_map <- list()
        }
        .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var
        lhs_mojo_type <- .mojor_type_env_to_mojo_type(lhs$name, type_env)
        if (is.null(lhs_mojo_type) && !is.null(base_type)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(base_type))
        }
        if (is.null(lhs_mojo_type) && !is.null(cast_to)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(cast_to))
        }
        if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
            lhs_mojo_type <- "Float64"
        }
        alloc_lines <- c(
            paste0(indent, "var ", lhs_len_var, " = Int(", varying_dim_var, ")"),
            paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")")
        )
    }

    used_names <- unique(c(
        base_name, lhs$name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    loop_i <- .mojor_unique_loop_var("__mojor_miss_j", used_names)

    read_row <- if (is_missing1) .mojor_ir_var(loop_i) else scalar_idx_norm
    read_col <- if (is_missing2) .mojor_ir_var(loop_i) else scalar_idx_norm
    read_node <- .mojor_ir_index(
        .mojor_ir_var(base_name),
        list(read_row, read_col),
        index_base = "zero_based"
    )
    read_str <- .mojor_ir_expr_emit(read_node, zero_based_vars, type_env, loop_var)
    if (is.null(read_str) || !nzchar(read_str)) return(NULL)

    if (!is.null(cast_to)) {
        mojo_type <- .mojor_r_to_mojo_type(cast_to)
        read_str <- paste0(mojo_type, "(", read_str, ")")
    }

    c(
        alloc_lines,
        paste0(indent, "for ", loop_i, " in range(Int(", lhs_len_var, ")):"),
        paste0(indent, "    ", lhs$name, "[", loop_i, "] = ", read_str)
    )
}

# Emit rank-N missing/scalar subset to matrix:
# lhs <- arr[, , k] / arr[, k, ] / arr[k, , ] / arr[, , k, l] / ...
# Handles exactly two missing selectors with remaining selectors scalar-fixed.
.mojor_ir_nd_missing_matrix_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, out_name = NULL
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "var") return(NULL)

    cast_to <- NULL
    if (!is.null(rhs$kind) && rhs$kind == "cast" && !is.null(rhs$expr)) {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) cast_to <- NULL

    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    if (is.null(rhs$base) || rhs$base$kind != "var") return(NULL)

    base_name <- rhs$base$name
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    base_ndim <- if (!is.null(base_type)) .mojor_type_ndim(base_type) else NA_integer_
    if (is.null(base_type) || is.na(base_ndim) || base_ndim < 3L) return(NULL)
    if (length(rhs$indices) != base_ndim) return(NULL)

    unwrap_idx <- function(idx_node) {
        out <- idx_node
        if (is.list(out) && identical(out$kind, "scalar_index") && !is.null(out$expr)) {
            out <- out$expr
        }
        out
    }
    idx_nodes <- lapply(rhs$indices, unwrap_idx)
    is_missing <- vapply(idx_nodes, function(idx_node) {
        is.list(idx_node) && identical(idx_node$kind, "missing_index")
    }, logical(1))
    missing_axes <- which(is_missing)
    if (length(missing_axes) != 2L) return(NULL)

    scalar_axes <- setdiff(seq_len(base_ndim), missing_axes)
    if (length(scalar_axes) < 1L) return(NULL)

    index_base <- if (!is.null(rhs$index_base)) rhs$index_base else "one_based"
    normalize_scalar_idx <- function(idx_node) {
        if (is.null(idx_node$kind) || !(idx_node$kind %in% c("const", "var"))) return(NULL)
        if (!is.null(idx_node$neg_exclusion) || !is.null(idx_node$neg_vec_exclusion) ||
            !is.null(idx_node$pos_vec_selection)) {
            return(NULL)
        }
        idx_norm <- idx_node
        if (identical(index_base, "one_based")) {
            idx_norm <- .mojor_ir_binop("-", idx_norm, .mojor_ir_const("1"))
            idx_norm$`__mojor_index_normalized` <- TRUE
        }
        idx_norm
    }

    resolve_dim_expr <- function(axis_idx) {
        out_var_name <- .mojor_state$current_out_name
        is_out <- !is.null(out_var_name) && identical(base_name, out_var_name)
        if (is_out && !is.null(.mojor_state$current_out_dim_var)) {
            return(paste0(.mojor_state$current_out_dim_var, "[", axis_idx - 1L, "]"))
        }
        dim_map <- .mojor_state$current_dim_var_map
        if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
            return(paste0(dim_map[[base_name]], "[", axis_idx - 1L, "]"))
        }
        NULL
    }

    dim_exprs <- vector("list", base_ndim)
    for (axis_idx in seq_len(base_ndim)) {
        dim_exprs[[axis_idx]] <- resolve_dim_expr(axis_idx)
    }
    if (any(vapply(dim_exprs, is.null, logical(1)))) return(NULL)

    lhs_nrow_var <- paste0("nrow_", lhs$name)
    lhs_ncol_var <- paste0("ncol_", lhs$name)
    lhs_len_var <- paste0("n_", lhs$name, "_i")

    if (is.null(.mojor_state$current_nrow_var_map)) .mojor_state$current_nrow_var_map <- list()
    if (is.null(.mojor_state$current_ncol_var_map)) .mojor_state$current_ncol_var_map <- list()
    if (is.null(.mojor_state$current_len_var_map)) .mojor_state$current_len_var_map <- list()
    .mojor_state$current_nrow_var_map[[lhs$name]] <- lhs_nrow_var
    .mojor_state$current_ncol_var_map[[lhs$name]] <- lhs_ncol_var
    .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var

    lhs_mojo_type <- .mojor_type_env_to_mojo_type(lhs$name, type_env)
    if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
        lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(base_type))
    }
    if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
        lhs_mojo_type <- "Float64"
    }

    used_names <- unique(c(
        base_name, lhs$name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    loop_row <- .mojor_unique_loop_var("__mojor_ndm_row", used_names)
    loop_col <- .mojor_unique_loop_var("__mojor_ndm_col", c(used_names, loop_row))

    idx_exprs <- character(base_ndim)
    idx_exprs[missing_axes[[1L]]] <- loop_row
    idx_exprs[missing_axes[[2L]]] <- loop_col
    for (axis_idx in scalar_axes) {
        scalar_idx_norm <- normalize_scalar_idx(idx_nodes[[axis_idx]])
        if (is.null(scalar_idx_norm)) return(NULL)
        scalar_idx_str <- .mojor_ir_expr_emit(
            scalar_idx_norm,
            zero_based_vars,
            type_env,
            loop_var,
            index_context = TRUE
        )
        if (is.null(scalar_idx_str) || !nzchar(scalar_idx_str)) return(NULL)
        idx_exprs[[axis_idx]] <- scalar_idx_str
    }
    if (any(!nzchar(idx_exprs))) return(NULL)

    stride_expr_for_axis <- function(axis_idx) {
        if (axis_idx <= 1L) return("Int(1)")
        parts <- vapply(seq_len(axis_idx - 1L), function(k) {
            paste0("Int(", dim_exprs[[k]], ")")
        }, character(1))
        paste(parts, collapse = " * ")
    }
    linear_terms <- vapply(seq_len(base_ndim), function(axis_idx) {
        idx_part <- paste0("(", idx_exprs[[axis_idx]], ")")
        if (axis_idx == 1L) {
            idx_part
        } else {
            paste0(idx_part, " * (", stride_expr_for_axis(axis_idx), ")")
        }
    }, character(1))
    src_linear <- paste0("Int(", paste(linear_terms, collapse = " + "), ")")
    dst_linear <- paste0("Int(", loop_row, " + (", loop_col, ") * ", lhs_nrow_var, ")")

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name
    base_len_expr <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        paste(vapply(seq_len(base_ndim), function(k) {
            paste0("Int(", dim_exprs[[k]], ")")
        }, character(1)), collapse = " * ")
    }

    base_elem_type <- .mojor_type_base(base_type)
    if (!is.null(base_elem_type) && base_elem_type %in% c("lgl", "bool")) {
        value_str <- .mojor_ir_read_call(
            "_mojor_read_lgl",
            base_name,
            src_linear,
            paste0("Int(", base_len_expr, ")")
        )
    } else {
        value_str <- paste0(base_name, "[", src_linear, "]")
    }
    if (!is.null(cast_to)) {
        value_str <- paste0(.mojor_r_to_mojo_type(cast_to), "(", value_str, ")")
    }

    nrow_dim_expr <- paste0("Int(", dim_exprs[[missing_axes[[1L]]]], ")")
    ncol_dim_expr <- paste0("Int(", dim_exprs[[missing_axes[[2L]]]], ")")

    c(
        paste0(indent, "var ", lhs_nrow_var, " = ", nrow_dim_expr),
        paste0(indent, "var ", lhs_ncol_var, " = ", ncol_dim_expr),
        paste0(indent, "var ", lhs_len_var, " = ", lhs_nrow_var, " * ", lhs_ncol_var),
        paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")"),
        paste0(indent, "for ", loop_col, " in range(Int(", lhs_ncol_var, ")):"),
        paste0(indent, "    for ", loop_row, " in range(Int(", lhs_nrow_var, ")):"),
        paste0(indent, "        ", lhs$name, "[", dst_linear, "] = ", value_str)
    )
}

# Step 8.13: Emit R-style negative-index exclusion: out <- x[-k]
# Lowers to: for i in range(n_x): if i != (k-1): out[j] = x[i]; j += 1
.mojor_ir_exclusion_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, out_name = NULL
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "var") return(NULL)

    # Unwrap optional cast
    cast_to <- NULL
    if (!is.null(rhs$kind) && rhs$kind == "cast") {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) cast_to <- NULL

    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    if (length(rhs$indices) != 1) return(NULL)

    idx_node <- rhs$indices[[1]]
    if (is.null(idx_node$neg_exclusion) && is.null(idx_node$neg_exclusion_expr_ast)) return(NULL)

    if (rhs$base$kind != "var") return(NULL)
    base_name <- rhs$base$name
    excl_idx <- idx_node$neg_exclusion  # 0-based exclusion index string
    if (is.null(excl_idx) && !is.null(idx_node$neg_exclusion_expr_ast)) {
        excl_ir <- .mojor_ir_expr_build(idx_node$neg_exclusion_expr_ast)
        excl_val <- if (!is.null(excl_ir)) {
            .mojor_ir_expr_emit(excl_ir, zero_based_vars, type_env, loop_var)
        } else {
            NULL
        }
        if (!is.null(excl_val)) {
            excl_idx <- paste0("Int(", excl_val, ") - 1")
        }
    }
    if (is.null(excl_idx)) return(NULL)

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    base_len_var <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        paste0("n_", base_name)
    }
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL

    lhs_len_var <- if (isTRUE(!is.null(out_name) && identical(lhs$name, out_name))) {
        "n_i"
    } else if (!is.null(len_var_map) && lhs$name %in% names(len_var_map)) {
        len_var_map[[lhs$name]]
    } else {
        paste0("n_", lhs$name, "_i")
    }
    reuse_existing_out <- isTRUE(!is.null(out_name) && identical(lhs$name, out_name))
    in_loop_context <- !is.null(loop_var) && length(loop_var) >= 1L && nzchar(as.character(loop_var[[1L]]))
    need_lhs_alloc <- !reuse_existing_out && (isTRUE(in_loop_context) || is.null(len_var_map) || !(lhs$name %in% names(len_var_map)))
    alloc_lines <- character(0)
    if (need_lhs_alloc) {
        if (is.null(.mojor_state$current_len_var_map)) {
            .mojor_state$current_len_var_map <- list()
        }
        .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var
        lhs_mojo_type <- .mojor_type_env_to_mojo_type(lhs$name, type_env)
        if (is.null(lhs_mojo_type) && !is.null(base_type)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(base_type))
        }
        if (is.null(lhs_mojo_type) && !is.null(cast_to)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(cast_to))
        }
        if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
            lhs_mojo_type <- "Float64"
        }
        alloc_lines <- c(
            paste0(indent, "var ", lhs_len_var, " = Int(", base_len_var, ") - 1"),
            paste0(indent, "if ", lhs_len_var, " < 0:"),
            paste0(indent, "    ", lhs_len_var, " = 0"),
            paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")")
        )
    }

    used_names <- unique(c(
        base_name, lhs$name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    lhs_suffix <- gsub("[^A-Za-z0-9_]", "_", lhs$name)
    loop_i <- .mojor_unique_loop_var(paste0("__mojor_excl_i_", lhs_suffix), used_names)
    out_j <- .mojor_unique_loop_var(paste0("__mojor_excl_j_", lhs_suffix), used_names)

    if (!is.null(base_type) && base_type %in% c("lgl[]", "bool[]")) {
        value_str <- .mojor_ir_read_call(
            "_mojor_read_lgl",
            base_name,
            loop_i,
            paste0("Int(", base_len_var, ")")
        )
    } else {
        value_str <- paste0(base_name, "[", loop_i, "]")
    }

    if (!is.null(cast_to)) {
        mojo_type <- .mojor_r_to_mojo_type(cast_to)
        value_str <- paste0(mojo_type, "(", value_str, ")")
    }

    lines <- c(
        alloc_lines,
        paste0(indent, "var ", out_j, ": Int = 0"),
        paste0(indent, "for ", loop_i, " in range(Int(", base_len_var, ")):"),
        paste0(indent, "    if ", loop_i, " != ", excl_idx, ":"),
        paste0(indent, "        ", lhs$name, "[", out_j, "] = ", value_str),
        paste0(indent, "        ", out_j, " += 1")
    )
    lines
}

# Emit write-side exclusion: out[-k] <- val
# Lowers to: for i in range(n_out): if i != (k-1): out[i] = val
.mojor_ir_exclusion_write_emit <- function(node, indent, zero_based_vars, type_env, loop_var) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "index") return(NULL)
    if (length(lhs$indices) != 1) return(NULL)

    idx_node <- lhs$indices[[1]]
    if (is.null(idx_node$neg_exclusion) && is.null(idx_node$neg_exclusion_expr_ast)) return(NULL)

    if (lhs$base$kind != "var") return(NULL)
    base_name <- lhs$base$name
    excl_idx <- idx_node$neg_exclusion  # 0-based exclusion index string
    if (is.null(excl_idx) && !is.null(idx_node$neg_exclusion_expr_ast)) {
        excl_ir <- .mojor_ir_expr_build(idx_node$neg_exclusion_expr_ast)
        excl_val <- if (!is.null(excl_ir)) {
            .mojor_ir_expr_emit(excl_ir, zero_based_vars, type_env, loop_var)
        } else {
            NULL
        }
        if (!is.null(excl_val)) {
            excl_idx <- paste0("Int(", excl_val, ") - 1")
        }
    }
    if (is.null(excl_idx)) return(NULL)

    # Get length variable for the output array
    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    base_len_var <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        paste0("n_", base_name)
    }

    # Emit RHS expression
    rhs_str <- .mojor_ir_expr_emit(rhs, zero_based_vars, type_env, loop_var)
    if (is.null(rhs_str)) return(NULL)

    # Generate unique loop var
    used_names <- unique(c(
        base_name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    loop_i <- .mojor_unique_loop_var("__mojor_excl_w", used_names)

    lines <- c(
        paste0(indent, "for ", loop_i, " in range(Int(", base_len_var, ")):"),
        paste0(indent, "    if ", loop_i, " != ", excl_idx, ":"),
        paste0(indent, "        ", base_name, "[", loop_i, "] = ", rhs_str)
    )
    lines
}

# Emit vector exclusion subset: out <- x[-c(a,b,...)] or out <- x[-(a:b)]
# Lowers to loop with multi-element skip condition
.mojor_ir_vec_exclusion_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, out_name = NULL
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "var") return(NULL)

    cast_to <- NULL
    if (!is.null(rhs$kind) && rhs$kind == "cast") {
        cast_to <- rhs$to
        rhs <- rhs$expr
    }
    if (.mojor_type_is_array(cast_to)) cast_to <- NULL

    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    if (length(rhs$indices) != 1) return(NULL)

    idx_node <- rhs$indices[[1]]
    if (is.null(idx_node$neg_vec_exclusion)) return(NULL)

    if (rhs$base$kind != "var") return(NULL)
    base_name <- rhs$base$name
    excl <- idx_node$neg_vec_exclusion

    emit_excl_ast <- function(ast_node) {
        if (is.null(ast_node)) {
            return(NULL)
        }
        ir_node <- .mojor_ir_expr_build(ast_node)
        if (is.null(ir_node)) {
            return(NULL)
        }
        .mojor_ir_expr_emit(ir_node, zero_based_vars, type_env, loop_var)
    }

    range_start_0 <- excl$start_0
    range_end_0 <- excl$end_0
    if (identical(excl$type, "range")) {
        if (is.null(range_start_0) && !is.null(excl$start_expr_ast)) {
            start_expr <- emit_excl_ast(excl$start_expr_ast)
            if (!is.null(start_expr)) {
                range_start_0 <- paste0("Int(", start_expr, ") - 1")
            }
        }
        if (is.null(range_end_0) && !is.null(excl$end_expr_ast)) {
            end_expr <- emit_excl_ast(excl$end_expr_ast)
            if (!is.null(end_expr)) {
                range_end_0 <- paste0("Int(", end_expr, ")")
            }
        }
    }

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    base_len_var <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        paste0("n_", base_name)
    }
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL

    excl_count_expr <- if (!is.null(excl$count)) {
        as.character(excl$count)
    } else if (identical(excl$type, "c")) {
        as.character(length(excl$indices))
    } else if (identical(excl$type, "range") && !is.null(range_start_0) && !is.null(range_end_0)) {
        paste0("((", range_end_0, ") - (", range_start_0, "))")
    } else {
        "0"
    }
    lhs_len_var <- if (isTRUE(!is.null(out_name) && identical(lhs$name, out_name))) {
        "n_i"
    } else if (!is.null(len_var_map) && lhs$name %in% names(len_var_map)) {
        len_var_map[[lhs$name]]
    } else {
        paste0("n_", lhs$name, "_i")
    }
    reuse_existing_out <- isTRUE(!is.null(out_name) && identical(lhs$name, out_name))
    in_loop_context <- !is.null(loop_var) && length(loop_var) >= 1L && nzchar(as.character(loop_var[[1L]]))
    need_lhs_alloc <- !reuse_existing_out && (isTRUE(in_loop_context) || is.null(len_var_map) || !(lhs$name %in% names(len_var_map)))
    alloc_lines <- character(0)
    if (need_lhs_alloc) {
        if (is.null(.mojor_state$current_len_var_map)) {
            .mojor_state$current_len_var_map <- list()
        }
        .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var
        lhs_mojo_type <- .mojor_type_env_to_mojo_type(lhs$name, type_env)
        if (is.null(lhs_mojo_type) && !is.null(base_type)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(base_type))
        }
        if (is.null(lhs_mojo_type) && !is.null(cast_to)) {
            lhs_mojo_type <- .mojor_r_to_mojo_type(.mojor_type_base(cast_to))
        }
        if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
            lhs_mojo_type <- "Float64"
        }
        alloc_lines <- c(
            paste0(indent, "var ", lhs_len_var, " = Int(", base_len_var, ") - Int(", excl_count_expr, ")"),
            paste0(indent, "if ", lhs_len_var, " < 0:"),
            paste0(indent, "    ", lhs_len_var, " = 0"),
            paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")")
        )
    }

    used_names <- unique(c(
        base_name, lhs$name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    lhs_suffix <- gsub("[^A-Za-z0-9_]", "_", lhs$name)
    loop_i <- .mojor_unique_loop_var(paste0("__mojor_excl_i_", lhs_suffix), used_names)
    out_j <- .mojor_unique_loop_var(paste0("__mojor_excl_j_", lhs_suffix), used_names)

    # Build skip condition
    if (excl$type == "c") {
        # x[-c(a,b,c)] → skip if i == a-1 or i == b-1 or i == c-1
        if (length(excl$indices) == 0L) {
            skip_cond <- "True"
        } else {
            cond_parts <- vapply(excl$indices, function(idx) {
                paste0(loop_i, " != (", idx, ")")
            }, character(1))
            skip_cond <- paste(cond_parts, collapse = " and ")
        }
    } else if (excl$type == "range") {
        # x[-(a:b)] → skip if a-1 <= i < b (0-based)
        if (is.null(range_start_0) || is.null(range_end_0)) {
            return(NULL)
        }
        skip_cond <- paste0(loop_i, " < (", range_start_0, ") or ", loop_i, " >= (", range_end_0, ")")
    } else {
        return(NULL)
    }

    if (!is.null(base_type) && base_type %in% c("lgl[]", "bool[]")) {
        value_str <- .mojor_ir_read_call(
            "_mojor_read_lgl",
            base_name,
            loop_i,
            paste0("Int(", base_len_var, ")")
        )
    } else {
        value_str <- paste0(base_name, "[", loop_i, "]")
    }

    if (!is.null(cast_to)) {
        mojo_type <- .mojor_r_to_mojo_type(cast_to)
        value_str <- paste0(mojo_type, "(", value_str, ")")
    }

    lines <- c(
        alloc_lines,
        paste0(indent, "var ", out_j, ": Int = 0"),
        paste0(indent, "for ", loop_i, " in range(Int(", base_len_var, ")):"),
        paste0(indent, "    if ", skip_cond, ":"),
        paste0(indent, "        ", lhs$name, "[", out_j, "] = ", value_str),
        paste0(indent, "        ", out_j, " += 1")
    )
    lines
}

# Helper: build ternary chain for vector selection indices
# e.g., indices_0 = c("0", "2", "4"), iter_var = "__s1"
# → "0 if __s1 == 0 else (2 if __s1 == 1 else 4)"
.mojor_build_ternary_chain <- function(indices_0, iter_var) {
    n <- length(indices_0)
    if (n == 1L) return(indices_0[1])
    # Build from the end
    result <- indices_0[n]
    for (i in (n - 1L):1L) {
        result <- paste0(indices_0[i], " if ", iter_var, " == ", i - 1L, " else (", result, ")")
    }
    result
}

# Classify N-d indices into dim kinds for emission
# Returns NULL if pattern is not recognized, otherwise a list of classifications
.mojor_classify_nd_indices <- function(indices, n_dims) {
    as_positive_int <- function(node, allow_zero = FALSE) {
        if (is.null(node) || is.null(node$kind)) return(NULL)
        if (identical(node$kind, "const")) {
            val <- suppressWarnings(as.integer(as.numeric(node$value)))
            min_ok <- if (isTRUE(allow_zero)) 0L else 1L
            if (!is.na(val) && val >= min_ok) {
                return(list(kind = "const", value = val))
            }
            return(NULL)
        }
        if (identical(node$kind, "var") && is.character(node$name) && nzchar(node$name)) {
            return(list(kind = "var", name = node$name))
        }
        NULL
    }
    parse_c_selector <- function(node) {
        if (is.null(node) || !identical(node$kind, "c") || !is.list(node$parts)) {
            return(NULL)
        }
        indices_0 <- character(0)
        has_dynamic <- FALSE
        for (part in node$parts) {
            parsed <- as_positive_int(part, allow_zero = TRUE)
            if (is.null(parsed)) {
                return(NULL)
            }
            if (identical(parsed$kind, "const")) {
                if (parsed$value == 0L) next
                indices_0 <- c(indices_0, as.character(parsed$value - 1L))
            } else {
                has_dynamic <- TRUE
                indices_0 <- c(indices_0, paste0("(Int(", parsed$name, ") - 1)"))
            }
        }
        list(
            type = "c",
            indices_0 = indices_0,
            count = if (length(indices_0) == 0L) "0" else as.character(length(indices_0)),
            is_dynamic = has_dynamic
        )
    }
    parse_slice_selector <- function(node) {
        if (is.null(node) || !identical(node$kind, "slice_index")) {
            return(NULL)
        }
        start <- as_positive_int(node$start)
        end <- as_positive_int(node$end)
        if (is.null(start) || is.null(end)) {
            return(NULL)
        }
        start_0 <- if (identical(start$kind, "const")) {
            as.character(start$value - 1L)
        } else {
            paste0("(Int(", start$name, ") - 1)")
        }
        end_0 <- if (identical(end$kind, "const")) {
            as.character(end$value)
        } else {
            paste0("Int(", end$name, ")")
        }
        list(type = "range", start_0 = start_0, end_0 = end_0)
    }
    excl_dims <- integer(0)
    excl_idxs <- character(0)
    scalar_dims <- integer(0)
    scalar_nodes <- list()
    vec_sel_dims <- integer(0)
    vec_sel_nodes <- list()
    neg_vec_excl_dims <- integer(0)
    neg_vec_excl_nodes <- list()

    for (d in seq_len(n_dims)) {
        idx_d <- indices[[d]]
        if (identical(idx_d$kind, "vec_index") && !is.null(idx_d$expr)) {
            idx_d <- idx_d$expr
        }
        if (identical(idx_d$kind, "scalar_index") && !is.null(idx_d$expr)) {
            idx_d <- idx_d$expr
        }
        if (!is.null(idx_d$neg_exclusion)) {
            excl_dims <- c(excl_dims, d)
            excl_idxs <- c(excl_idxs, idx_d$neg_exclusion)
        } else if (!is.null(idx_d$neg_vec_exclusion)) {
            neg_vec_excl_dims <- c(neg_vec_excl_dims, d)
            neg_vec_excl_nodes[[length(neg_vec_excl_nodes) + 1L]] <- idx_d$neg_vec_exclusion
        } else if (!is.null(idx_d$pos_vec_selection)) {
            vec_sel_dims <- c(vec_sel_dims, d)
            vec_sel_nodes[[length(vec_sel_nodes) + 1L]] <- idx_d$pos_vec_selection
        } else if (identical(idx_d$kind, "slice_index")) {
            sel <- parse_slice_selector(idx_d)
            if (is.null(sel)) return(NULL)
            vec_sel_dims <- c(vec_sel_dims, d)
            vec_sel_nodes[[length(vec_sel_nodes) + 1L]] <- sel
        } else if (identical(idx_d$kind, "c")) {
            sel <- parse_c_selector(idx_d)
            if (is.null(sel)) return(NULL)
            vec_sel_dims <- c(vec_sel_dims, d)
            vec_sel_nodes[[length(vec_sel_nodes) + 1L]] <- sel
        } else if (identical(idx_d$kind, "missing_index")) {
            next
        } else if (idx_d$kind %in% c("const", "var")) {
            scalar_dims <- c(scalar_dims, d)
            scalar_nodes[[length(scalar_nodes) + 1L]] <- idx_d
        } else {
            return(NULL)
        }
    }
    n_special <- length(excl_dims) + length(neg_vec_excl_dims) + length(vec_sel_dims)
    if (n_special == 0L) return(NULL)

    list(excl_dims = excl_dims, excl_idxs = excl_idxs,
         scalar_dims = scalar_dims, scalar_nodes = scalar_nodes,
         vec_sel_dims = vec_sel_dims, vec_sel_nodes = vec_sel_nodes,
         neg_vec_excl_dims = neg_vec_excl_dims, neg_vec_excl_nodes = neg_vec_excl_nodes)
}

# Emit loop structure for N-d subscript dims
# Returns list(lines, loop_vars, loop_depth, extra_indent) for building inner body
# loop_depth_start: 1 for read-side (first loop at indent+1), 0 for write-side (first loop at indent+0)
.mojor_emit_nd_loops <- function(cls, n_dims, dim_vars, indent, loop_depth_start = 1L) {
    loop_vars <- character(n_dims)
    lines <- character(0)

    # Pre-compute scalar index expressions (0-based)
    scalar_expr_map <- list()
    for (si in seq_along(cls$scalar_dims)) {
        sd <- cls$scalar_dims[si]
        sn <- cls$scalar_nodes[[si]]
        if (sn$kind == "const") {
            scalar_expr_map[[as.character(sd)]] <- as.character(as.integer(as.numeric(sn$value)) - 1L)
        } else if (sn$kind == "var") {
            scalar_expr_map[[as.character(sd)]] <- paste0("(Int(", sn$name, ") - 1)")
        }
    }

    dim_order <- rev(seq_len(n_dims))
    loop_depth <- 0L
    extra_if_indent <- 0L

    for (idx in seq_along(dim_order)) {
        d <- dim_order[idx]
        if (d %in% cls$scalar_dims) {
            loop_vars[d] <- scalar_expr_map[[as.character(d)]]
            next
        }

        loop_depth <- loop_depth + 1L
        lv <- paste0("__mojor_nd_", d)
        loop_vars[d] <- lv
        cur_indent <- paste0(indent, strrep("    ", loop_depth - 1L + loop_depth_start + extra_if_indent))

        # Scalar exclusion
        excl_pos <- match(d, cls$excl_dims)
        if (!is.na(excl_pos)) {
            lines <- c(lines, paste0(cur_indent, "for ", lv, " in range(Int(", dim_vars[d], ")):"))
            lines <- c(lines, paste0(cur_indent, "    if ", lv, " != (", cls$excl_idxs[excl_pos], "):"))
            extra_if_indent <- extra_if_indent + 1L
            next
        }

        # Neg vector exclusion: -c(1,2) → loop with multi-skip
        nve_pos <- match(d, cls$neg_vec_excl_dims)
        if (!is.na(nve_pos)) {
            nve <- cls$neg_vec_excl_nodes[[nve_pos]]
            lines <- c(lines, paste0(cur_indent, "for ", lv, " in range(Int(", dim_vars[d], ")):"))
            if (nve$type == "c") {
                # Multi-condition skip: if lv != idx0 and lv != idx1 ...
                conds <- vapply(nve$indices, function(idx_0) {
                    paste0(lv, " != (", idx_0, ")")
                }, character(1))
                lines <- c(lines, paste0(cur_indent, "    if ", paste(conds, collapse = " and "), ":"))
            } else if (nve$type == "range") {
                # Range skip: if lv < start_0 or lv >= end_0
                lines <- c(lines, paste0(cur_indent, "    if ", lv, " < (", nve$start_0, ") or ", lv, " >= (", nve$end_0, "):"))
            }
            extra_if_indent <- extra_if_indent + 1L
            next
        }

        # Positive vector selection
        vs_pos <- match(d, cls$vec_sel_dims)
        if (!is.na(vs_pos)) {
            vs <- cls$vec_sel_nodes[[vs_pos]]
            if (vs$type == "range") {
                # Direct range loop: for lv in range(start_0, end_0)
                lines <- c(lines, paste0(cur_indent, "for ", lv, " in range(", vs$start_0, ", ", vs$end_0, "):"))
            } else if (vs$type == "c") {
                # Set selection: loop + ternary lookup
                sel_lv <- paste0("__mojor_sel_", d)
                lines <- c(lines, paste0(cur_indent, "for ", sel_lv, " in range(", vs$count, "):"))
                ternary <- .mojor_build_ternary_chain(vs$indices_0, sel_lv)
                lines <- c(lines, paste0(cur_indent, "    var ", lv, ": Int = ", ternary))
                extra_if_indent <- extra_if_indent + 1L
            }
            next
        }

        # Missing dim — full range
        lines <- c(lines, paste0(cur_indent, "for ", lv, " in range(Int(", dim_vars[d], ")):"))
    }

    list(lines = lines, loop_vars = loop_vars,
         loop_depth = loop_depth, extra_if_indent = extra_if_indent)
}

# Emit N-d subscript subset: out <- mat[-k, ] or out <- mat[c(1,3), ] etc.
# Generates nested loops with appropriate patterns for each dimension kind.
# Output is a flattened 1D vector in column-major order.
.mojor_ir_nd_exclusion_subset_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, bounds_check = FALSE
) {
    lhs <- node$lhs
    rhs <- node$rhs

    # Unwrap cast wrapper if present (type pass may wrap index in cast)
    if (!is.null(rhs$kind) && rhs$kind == "cast" && !is.null(rhs$expr)) {
        rhs <- rhs$expr
    }
    if (is.null(rhs$kind) || rhs$kind != "index") return(NULL)
    indices <- rhs$indices
    n_dims <- length(indices)
    if (n_dims < 2L) return(NULL)

    cls <- .mojor_classify_nd_indices(indices, n_dims)
    if (is.null(cls)) return(NULL)

    if (length(cls$neg_vec_excl_nodes) > 0) {
        for (k in seq_along(cls$neg_vec_excl_nodes)) {
            nve <- cls$neg_vec_excl_nodes[[k]]
            if (!identical(nve$type, "range")) {
                next
            }
            if (is.null(nve$start_0) && !is.null(nve$start_expr_ast)) {
                start_ir <- .mojor_ir_expr_build(nve$start_expr_ast)
                start_expr <- if (!is.null(start_ir)) {
                    .mojor_ir_expr_emit(start_ir, zero_based_vars, type_env, loop_var)
                } else {
                    NULL
                }
                if (!is.null(start_expr)) {
                    nve$start_0 <- paste0("Int(", start_expr, ") - 1")
                }
            }
            if (is.null(nve$end_0) && !is.null(nve$end_expr_ast)) {
                end_ir <- .mojor_ir_expr_build(nve$end_expr_ast)
                end_expr <- if (!is.null(end_ir)) {
                    .mojor_ir_expr_emit(end_ir, zero_based_vars, type_env, loop_var)
                } else {
                    NULL
                }
                if (!is.null(end_expr)) {
                    nve$end_0 <- paste0("Int(", end_expr, ")")
                }
            }
            if (is.null(nve$start_0) || is.null(nve$end_0)) {
                return(NULL)
            }
            cls$neg_vec_excl_nodes[[k]] <- nve
        }
    }

    if (is.null(rhs$base) || rhs$base$kind != "var") return(NULL)
    base_name <- rhs$base$name

    # Resolve dimension expressions from current state maps.
    resolve_dim_expr <- function(axis_idx) {
        out_name <- .mojor_state$current_out_name
        is_out <- !is.null(out_name) && identical(base_name, out_name)

        if (n_dims == 2L) {
            if (axis_idx == 1L) {
                if (is_out && !is.null(.mojor_state$current_out_nrow_var)) {
                    return(.mojor_state$current_out_nrow_var)
                }
                nrow_map <- .mojor_state$current_nrow_var_map
                if (!is.null(nrow_map) && !is.null(nrow_map[[base_name]])) {
                    return(nrow_map[[base_name]])
                }
                local_matrix_dims <- .mojor_state$current_local_matrix_dims
                if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[base_name]])) {
                    return(paste0("nrow_", base_name))
                }
                return(NULL)
            }
            if (axis_idx == 2L) {
                if (is_out && !is.null(.mojor_state$current_out_ncol_var)) {
                    return(.mojor_state$current_out_ncol_var)
                }
                ncol_map <- .mojor_state$current_ncol_var_map
                if (!is.null(ncol_map) && !is.null(ncol_map[[base_name]])) {
                    return(ncol_map[[base_name]])
                }
                local_matrix_dims <- .mojor_state$current_local_matrix_dims
                if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[base_name]])) {
                    return(paste0("ncol_", base_name))
                }
                return(NULL)
            }
        }

        if (is_out && !is.null(.mojor_state$current_out_dim_var)) {
            return(paste0(.mojor_state$current_out_dim_var, "[", axis_idx - 1L, "]"))
        }
        dim_map <- .mojor_state$current_dim_var_map
        if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
            return(paste0(dim_map[[base_name]], "[", axis_idx - 1L, "]"))
        }
        NULL
    }

    dim_vars <- character(n_dims)
    for (d in seq_len(n_dims)) {
        dim_expr <- resolve_dim_expr(d)
        if (is.null(dim_expr) || !nzchar(dim_expr)) return(NULL)
        dim_vars[d] <- dim_expr
    }

    selected_extent_expr <- function(axis_idx) {
        if (axis_idx %in% cls$scalar_dims) {
            return("Int(1)")
        }
        excl_pos <- match(axis_idx, cls$excl_dims)
        if (!is.na(excl_pos)) {
            return(paste0("(Int(", dim_vars[axis_idx], ") - Int(1))"))
        }
        nve_pos <- match(axis_idx, cls$neg_vec_excl_dims)
        if (!is.na(nve_pos)) {
            nve <- cls$neg_vec_excl_nodes[[nve_pos]]
            if (is.list(nve) && identical(nve$type, "c") && !is.null(nve$count)) {
                return(paste0("(Int(", dim_vars[axis_idx], ") - Int(", nve$count, "))"))
            }
            if (is.list(nve) && identical(nve$type, "range") &&
                !is.null(nve$start_0) && !is.null(nve$end_0)) {
                return(paste0("(Int(", dim_vars[axis_idx], ") - (Int(", nve$end_0, ") - Int(", nve$start_0, ")))"))
            }
            return(NULL)
        }
        vs_pos <- match(axis_idx, cls$vec_sel_dims)
        if (!is.na(vs_pos)) {
            vs <- cls$vec_sel_nodes[[vs_pos]]
            if (is.list(vs) && identical(vs$type, "range") &&
                !is.null(vs$start_0) && !is.null(vs$end_0)) {
                return(paste0("(Int(", vs$end_0, ") - Int(", vs$start_0, "))"))
            }
            if (is.list(vs) && identical(vs$type, "c") && !is.null(vs$count)) {
                return(paste0("Int(", vs$count, ")"))
            }
            return(NULL)
        }
        paste0("Int(", dim_vars[axis_idx], ")")
    }

    selected_factors <- vapply(seq_len(n_dims), selected_extent_expr, character(1))
    if (any(is.na(selected_factors) | !nzchar(selected_factors))) return(NULL)
    selected_factors <- selected_factors[selected_factors != "Int(1)"]
    lhs_len_expr <- if (length(selected_factors) == 0L) {
        "Int(1)"
    } else {
        paste(selected_factors, collapse = " * ")
    }

    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    lhs_type <- if (!is.null(type_env)) type_env[[lhs$name]] else NULL
    lhs_mojo_type <- if (!is.null(lhs_type)) .mojor_type_env_to_mojo_type(lhs$name, type_env) else NULL
    if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
        lhs_mojo_type <- if (!is.null(base_type)) .mojor_r_to_mojo_type(.mojor_type_base(base_type)) else NULL
    }
    if (is.null(lhs_mojo_type) || !nzchar(lhs_mojo_type)) {
        lhs_mojo_type <- "Float64"
    }

    lhs_len_var <- paste0("n_", lhs$name, "_i")
    if (is.null(.mojor_state$current_len_var_map)) .mojor_state$current_len_var_map <- list()
    .mojor_state$current_len_var_map[[lhs$name]] <- lhs_len_var

    used_names <- unique(c(
        base_name, lhs$name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    out_j <- .mojor_unique_loop_var("__mojor_nd_j", used_names)

    lines <- c(
        paste0(indent, "var ", lhs_len_var, " = ", lhs_len_expr),
        paste0(indent, "var ", lhs$name, " = alloc[", lhs_mojo_type, "](", lhs_len_var, ")"),
        paste0(indent, "var ", out_j, ": Int = 0")
    )

    loop_result <- .mojor_emit_nd_loops(cls, n_dims, dim_vars, indent, loop_depth_start = 0L)
    lines <- c(lines, loop_result$lines)
    loop_vars <- loop_result$loop_vars

    # Column-major linear index: d1 + d2*dim1 + d3*dim1*dim2 + ...
    terms <- character(n_dims)
    terms[1] <- loop_vars[1]
    if (n_dims >= 2L) {
        for (d in 2:n_dims) {
            stride_parts <- vapply(seq_len(d - 1L), function(k) {
                paste0("Int(", dim_vars[k], ")")
            }, character(1))
            stride <- if (length(stride_parts) == 1) stride_parts else paste0("(", paste(stride_parts, collapse = " * "), ")")
            terms[d] <- paste0("(", loop_vars[d], " * ", stride, ")")
        }
    }
    linear_idx <- paste(terms, collapse = " + ")

    inner_indent <- paste0(indent, strrep("    ", loop_result$loop_depth + loop_result$extra_if_indent))

    # Build the read expression
    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    base_len_var <- paste0("n_", base_name, "_i")
    if (!is.null(base_type) && base_type %in% c("lgl[,]", "bool[,]", "lgl[]", "bool[]")) {
        value_str <- .mojor_ir_read_call(
            "_mojor_read_lgl",
            base_name,
            paste0("Int(", linear_idx, ")"),
            paste0("Int(", base_len_var, ")")
        )
    } else {
        value_str <- paste0(base_name, "[Int(", linear_idx, ")]")
    }

    if (isTRUE(bounds_check)) {
        guard_dims <- sort(unique(c(cls$scalar_dims, cls$vec_sel_dims)))
        lhs_oob_conds <- vapply(guard_dims, function(d) {
            idx_d <- loop_vars[d]
            upper_d <- dim_vars[d]
            paste0(
                "(", idx_d, ") < 0 or (", idx_d, ") >= Int(",
                upper_d, ")"
            )
        }, character(1))
        lhs_oob_conds <- unique(lhs_oob_conds)
        if (length(lhs_oob_conds) > 0) {
            oob_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                paste0(inner_indent, "    __mojor_na_flag[0] = Int32(2)")
            } else {
                paste0(inner_indent, "    _mojor_oob()")
            }
            lines <- c(
                lines,
                paste0(inner_indent, "if ", paste(lhs_oob_conds, collapse = " or "), ":"),
                oob_line,
                paste0(inner_indent, "else:"),
                paste0(inner_indent, "    ", lhs$name, "[", out_j, "] = ", value_str),
                paste0(inner_indent, "    ", out_j, " += 1")
            )
        } else {
            lines <- c(lines,
                paste0(inner_indent, lhs$name, "[", out_j, "] = ", value_str),
                paste0(inner_indent, out_j, " += 1")
            )
        }
    } else {
        lines <- c(lines,
            paste0(inner_indent, lhs$name, "[", out_j, "] = ", value_str),
            paste0(inner_indent, out_j, " += 1")
        )
    }
    lines
}

# Emit N-d subscript write: mat[-1, ] <- val, mat[c(1,3), ] <- val, etc.
.mojor_ir_nd_exclusion_write_emit <- function(
    node, indent, zero_based_vars, type_env, loop_var, bounds_check = FALSE
) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || !(lhs$kind %in% c("subscript", "index"))) return(NULL)
    indices <- lhs$indices
    n_dims <- length(indices)
    if (n_dims < 2L) return(NULL)

    cls <- .mojor_classify_nd_indices(indices, n_dims)
    if (is.null(cls)) return(NULL)

    if (length(cls$neg_vec_excl_nodes) > 0) {
        for (k in seq_along(cls$neg_vec_excl_nodes)) {
            nve <- cls$neg_vec_excl_nodes[[k]]
            if (!identical(nve$type, "range")) {
                next
            }
            if (is.null(nve$start_0) && !is.null(nve$start_expr_ast)) {
                start_ir <- .mojor_ir_expr_build(nve$start_expr_ast)
                start_expr <- if (!is.null(start_ir)) {
                    .mojor_ir_expr_emit(start_ir, zero_based_vars, type_env, loop_var)
                } else {
                    NULL
                }
                if (!is.null(start_expr)) {
                    nve$start_0 <- paste0("Int(", start_expr, ") - 1")
                }
            }
            if (is.null(nve$end_0) && !is.null(nve$end_expr_ast)) {
                end_ir <- .mojor_ir_expr_build(nve$end_expr_ast)
                end_expr <- if (!is.null(end_ir)) {
                    .mojor_ir_expr_emit(end_ir, zero_based_vars, type_env, loop_var)
                } else {
                    NULL
                }
                if (!is.null(end_expr)) {
                    nve$end_0 <- paste0("Int(", end_expr, ")")
                }
            }
            if (is.null(nve$start_0) || is.null(nve$end_0)) {
                return(NULL)
            }
            cls$neg_vec_excl_nodes[[k]] <- nve
        }
    }

    base_name <- if (identical(lhs$kind, "subscript")) lhs$var else if (!is.null(lhs$base) && identical(lhs$base$kind, "var")) lhs$base$name else NULL
    if (is.null(base_name)) return(NULL)

    # Resolve dimension expressions from state maps (output-aware and rank-aware).
    resolve_dim_expr <- function(axis_idx) {
        out_name <- .mojor_state$current_out_name
        is_out <- !is.null(out_name) && identical(base_name, out_name)

        if (n_dims == 2L) {
            if (axis_idx == 1L) {
                if (is_out && !is.null(.mojor_state$current_out_nrow_var)) {
                    return(.mojor_state$current_out_nrow_var)
                }
                nrow_map <- .mojor_state$current_nrow_var_map
                if (!is.null(nrow_map) && !is.null(nrow_map[[base_name]])) {
                    return(nrow_map[[base_name]])
                }
                local_matrix_dims <- .mojor_state$current_local_matrix_dims
                if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[base_name]])) {
                    return(paste0("nrow_", base_name, "_i"))
                }
                return(NULL)
            }
            if (axis_idx == 2L) {
                if (is_out && !is.null(.mojor_state$current_out_ncol_var)) {
                    return(.mojor_state$current_out_ncol_var)
                }
                ncol_map <- .mojor_state$current_ncol_var_map
                if (!is.null(ncol_map) && !is.null(ncol_map[[base_name]])) {
                    return(ncol_map[[base_name]])
                }
                local_matrix_dims <- .mojor_state$current_local_matrix_dims
                if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[base_name]])) {
                    return(paste0("ncol_", base_name, "_i"))
                }
                return(NULL)
            }
        }

        if (is_out && !is.null(.mojor_state$current_out_dim_var)) {
            return(paste0(.mojor_state$current_out_dim_var, "[", axis_idx - 1L, "]"))
        }
        dim_map <- .mojor_state$current_dim_var_map
        if (!is.null(dim_map) && !is.null(dim_map[[base_name]])) {
            return(paste0(dim_map[[base_name]], "[", axis_idx - 1L, "]"))
        }
        NULL
    }

    dim_vars <- character(n_dims)
    for (d in seq_len(n_dims)) {
        dim_expr <- resolve_dim_expr(d)
        if (is.null(dim_expr) || !nzchar(dim_expr)) return(NULL)
        dim_vars[d] <- dim_expr
    }

    # Emit RHS value expression
    rhs_expr <- .mojor_ir_expr_emit(rhs, zero_based_vars, type_env, loop_vars = loop_var)
    if (is.null(rhs_expr)) return(NULL)

    loop_result <- .mojor_emit_nd_loops(cls, n_dims, dim_vars, indent, loop_depth_start = 0L)
    lines <- loop_result$lines
    loop_vars <- loop_result$loop_vars

    inner_indent <- paste0(indent, strrep("    ", loop_result$loop_depth + loop_result$extra_if_indent))

    base_type <- if (!is.null(type_env)) type_env[[base_name]] else NULL
    tensor_map <- .mojor_state$current_tensor_map
    if (!is.null(tensor_map) && !is.list(tensor_map)) {
        tensor_map <- as.list(tensor_map)
    }
    tensor_name <- if (!is.null(tensor_map)) tensor_map[[base_name]] else NULL

    write_stmt <- NULL
    if (!is.null(tensor_name) && nzchar(tensor_name)) {
        tensor_target <- paste0(tensor_name, "[", paste(loop_vars, collapse = ", "), "]")
        write_stmt <- paste0(tensor_target, " = ", rhs_expr)
    } else {
        # Column-major linear index
        terms <- character(n_dims)
        terms[1] <- loop_vars[1]
        if (n_dims >= 2L) {
            for (d in 2:n_dims) {
                stride_parts <- vapply(seq_len(d - 1L), function(k) {
                    paste0("Int(", dim_vars[k], ")")
                }, character(1))
                stride <- if (length(stride_parts) == 1) stride_parts else paste0("(", paste(stride_parts, collapse = " * "), ")")
                terms[d] <- paste0("(", loop_vars[d], " * ", stride, ")")
            }
        }
        linear_idx <- paste(terms, collapse = " + ")

        base_len_var <- paste0("n_", base_name, "_i")
        if (!is.null(base_type) && base_type %in% c("lgl[,]", "bool[,]", "lgl[]", "bool[]")) {
            write_str <- paste0("_mojor_write_lgl(", base_name, ", Int(", linear_idx, "), ", rhs_expr, ", Int(", base_len_var, "))")
            write_stmt <- write_str
        } else {
            write_stmt <- paste0(base_name, "[Int(", linear_idx, ")] = ", rhs_expr)
        }
    }
    if (isTRUE(bounds_check)) {
        guard_dims <- sort(unique(c(cls$scalar_dims, cls$vec_sel_dims)))
        lhs_oob_conds <- vapply(guard_dims, function(d) {
            idx_d <- loop_vars[d]
            upper_d <- dim_vars[d]
            paste0(
                "(", idx_d, ") < 0 or (", idx_d, ") >= Int(",
                upper_d, ")"
            )
        }, character(1))
        lhs_oob_conds <- unique(lhs_oob_conds)
        if (length(lhs_oob_conds) > 0) {
            oob_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
                paste0(inner_indent, "    __mojor_na_flag[0] = Int32(2)")
            } else {
                paste0(inner_indent, "    _mojor_oob()")
            }
            lines <- c(
                lines,
                paste0(inner_indent, "if ", paste(lhs_oob_conds, collapse = " or "), ":"),
                oob_line,
                paste0(inner_indent, "else:"),
                paste0(inner_indent, "    ", write_stmt)
            )
        } else {
            lines <- c(lines, paste0(inner_indent, write_stmt))
        }
    } else {
        lines <- c(lines, paste0(inner_indent, write_stmt))
    }

    lines
}

# Emit vector exclusion write: out[-c(a,b,...)] <- val or out[-(a:b)] <- val
.mojor_ir_vec_exclusion_write_emit <- function(node, indent, zero_based_vars, type_env, loop_var) {
    lhs <- node$lhs
    rhs <- node$rhs

    if (is.null(lhs$kind) || lhs$kind != "index") return(NULL)
    if (length(lhs$indices) != 1) return(NULL)

    idx_node <- lhs$indices[[1]]
    if (is.null(idx_node$neg_vec_exclusion)) return(NULL)

    if (lhs$base$kind != "var") return(NULL)
    base_name <- lhs$base$name
    excl <- idx_node$neg_vec_exclusion

    emit_excl_ast <- function(ast_node) {
        if (is.null(ast_node)) return(NULL)
        ir_node <- .mojor_ir_expr_build(ast_node)
        if (is.null(ir_node)) return(NULL)
        .mojor_ir_expr_emit(ir_node, zero_based_vars, type_env, loop_var)
    }

    range_start_0 <- excl$start_0
    range_end_0 <- excl$end_0
    if (identical(excl$type, "range")) {
        if (is.null(range_start_0) && !is.null(excl$start_expr_ast)) {
            start_expr <- emit_excl_ast(excl$start_expr_ast)
            if (!is.null(start_expr)) {
                range_start_0 <- paste0("Int(", start_expr, ") - 1")
            }
        }
        if (is.null(range_end_0) && !is.null(excl$end_expr_ast)) {
            end_expr <- emit_excl_ast(excl$end_expr_ast)
            if (!is.null(end_expr)) {
                range_end_0 <- paste0("Int(", end_expr, ")")
            }
        }
    }

    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name

    base_len_var <- if (!is.null(len_var_map) && base_name %in% names(len_var_map)) {
        len_var_map[[base_name]]
    } else if (!is.null(n_source_name) && identical(base_name, n_source_name)) {
        "n_i"
    } else {
        paste0("n_", base_name)
    }

    rhs_str <- .mojor_ir_expr_emit(rhs, zero_based_vars, type_env, loop_var)
    if (is.null(rhs_str)) return(NULL)

    used_names <- unique(c(
        base_name,
        if (!is.null(type_env)) names(type_env) else character(0),
        if (!is.null(loop_var)) loop_var else character(0)
    ))
    loop_i <- .mojor_unique_loop_var("__mojor_excl_w", used_names)

    if (excl$type == "c") {
        cond_parts <- vapply(excl$indices, function(idx) {
            paste0(loop_i, " != (", idx, ")")
        }, character(1))
        skip_cond <- paste(cond_parts, collapse = " and ")
    } else if (excl$type == "range") {
        if (is.null(range_start_0) || is.null(range_end_0)) {
            return(NULL)
        }
        skip_cond <- paste0(loop_i, " < (", range_start_0, ") or ", loop_i, " >= (", range_end_0, ")")
    } else {
        return(NULL)
    }

    lines <- c(
        paste0(indent, "for ", loop_i, " in range(Int(", base_len_var, ")):"),
        paste0(indent, "    if ", skip_cond, ":"),
        paste0(indent, "        ", base_name, "[", loop_i, "] = ", rhs_str)
    )
    lines
}

.mojor_ir_block_emit <- function(
    node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid",
    bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL,
    schedule = NULL, bounds_guard_cache = NULL, na_guard_cache = NULL
) {
 # Step 4.4: Added loop_var parameter for guard optimization
 # Step 5.1: Added type_env parameter for logical array support
    if (is.null(node) ||
        node$kind != "block") {
        return(NULL)
    }
    lines <- character(0)
    for (i in seq_along(node$stmts)) {
        stmt <- node$stmts[[i]]
        emitted <- .mojor_ir_stmt_emit(
            stmt, indent, zero_based_vars, out_name, na_guard, bounds_check,
            loop_var, scalar_name, type_env, unroll = NULL, schedule = schedule,
            bounds_guard_cache = bounds_guard_cache, na_guard_cache = na_guard_cache
        )
        if (is.null(emitted) || length(emitted) == 0L || any(!nzchar(emitted))) {
            stmt_kind <- if (!is.null(stmt$kind)) stmt$kind else "<unknown>"
            .mojor_err(
                paste0("IR statement emission failed for block statement kind '", stmt_kind, "'"),
                stmt$src,
                "simplify this statement or use a supported subset form"
            )
        }
        lines <- c(lines, emitted)
    }
    lines
}

# =============================================================================
# Step 3: Typed IR - Type Inference
# =============================================================================

# =============================================================================
# Type vocabulary helpers
# =============================================================================
# These centralise all knowledge about the type string format so that
# callers never inspect raw strings.
#
# Vectors, matrices, and higher-dimensional arrays are all ndarrays
# distinguished only by their rank (ndim):
#
#   Scalar : 'f64', 'i32', 'f32', 'bool', 'unknown'
#   1-D    : 'f64[]'  (canonical)  or  'f64[1d]'  (alias)
#   2-D    : 'f64[,]' or 'f64[2d]'
#   N-D    : 'f64[3d]', 'f64[4d]', ...
#
# The [1d] form is accepted everywhere and normalised to [] internally.

# Normalise [1d] alias to canonical [] form.
.mojor_type_normalize <- function(t) {
    sub("\\[1d\\]$", "[]", t)
}

.mojor_type_is_array <- function(t) {
    if (is.null(t)) {
        return(FALSE)
    }
    t_chr <- as.character(t)
    t_chr[is.na(t_chr)] <- ""
    grepl("\\[", t_chr)
}

.mojor_type_ndim <- function(t) {
    if (is.null(t) ||
        !grepl("\\[", t)) {
        return(1L)
    }
    if (grepl("\\[\\]$", t)) {
        return(1L)
    }
    m_nd <- regmatches(t, regexpr("\\[([0-9]+)d\\]$", t))
    if (length(m_nd) == 1 && nchar(m_nd) > 0) {
        return(as.integer(sub(".*\\[([0-9]+)d\\]$", "\\1", m_nd)))
    }
    m_commas <- regmatches(t, regexpr("\\[([,]+)\\]$", t))
    if (length(m_commas) == 1 && nchar(m_commas) > 0) {
        comma_run <- sub("^\\[([,]+)\\]$", "\\1", m_commas)
        return(as.integer(nchar(comma_run) + 1L))
    }
    1L
}

.mojor_type_base <- function(t) sub("\\[.*\\]$", "", t)

.mojor_type_elem <- function(t) {
    if (.mojor_type_is_array(t))
        .mojor_type_base(t) else t
}

.mojor_type_tag_ndim <- function(t, ndim) {
    base <- .mojor_type_base(t)
    if (ndim <= 1L)
        paste0(base, "[]") else paste0(
        base, "[", as.integer(ndim),
        "d]"
    )
}

.mojor_type_promote <- function(t1, t2) {
 # Determine promoted type for mixed arithmetic Type lattice: bool
 # < i32 < f32 < f64

    if (is.null(t1) ||
        is.null(t2)) {
        return("unknown")
    }
    if (t1 == "unknown" && t2 == "unknown") {
        return("unknown")
    }
    if (t1 == "unknown") {
        return(t2)
    }
    if (t2 == "unknown") {
        return(t1)
    }

    promote_scalar <- function(a, b) {
        if (a == "bool")
            a <- "i32"
        if (b == "bool")
            b <- "i32"
        if (a == b) {
            return(a)
        }
        if (a == "f64" || b == "f64") {
            return("f64")
        }
        if (a == "f32" || b == "f32") {
            return("f32")
        }
        "i32"
    }

    t1_is_array <- .mojor_type_is_array(t1)
    t2_is_array <- .mojor_type_is_array(t2)

    if (!t1_is_array && !t2_is_array) {
        return(promote_scalar(t1, t2))
    }

    t1_elem <- if (t1_is_array) .mojor_type_elem(t1) else t1
    t2_elem <- if (t2_is_array) .mojor_type_elem(t2) else t2
    out_elem <- promote_scalar(t1_elem, t2_elem)

    if (identical(out_elem, "unknown")) {
        return("unknown")
    }

    if (t1_is_array && t2_is_array) {
        n1 <- .mojor_type_ndim(t1)
        n2 <- .mojor_type_ndim(t2)
        if (!identical(n1, n2)) {
            return("unknown")
        }
        return(.mojor_type_tag_ndim(out_elem, n1))
    }

    ndim <- if (t1_is_array) .mojor_type_ndim(t1) else .mojor_type_ndim(t2)
    .mojor_type_tag_ndim(out_elem, ndim)
}

# =============================================================================
# Section 7: Type Inference and Checking
# =============================================================================
