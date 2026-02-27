.mojor_ir_expr_get_call_arg <- function(parts, nms, nm, pos = NULL) {
    if (!is.null(nms) &&
        nm %in% nms) {
        idx <- which(nms == nm)[1]
        return(parts[[idx]])
    }
    if (!is.null(pos) &&
        length(parts) >=
            pos) {
        pos_name <- if (is.null(nms))
            "" else nms[[pos]]
        if (is.null(pos_name) ||
            pos_name == "") {
            return(parts[[pos]])
        }
    }
    NULL
}

.mojor_ir_expr_get_call_arg_idx <- function(parts, nms, nm, pos = NULL) {
    if (!is.null(nms) &&
        nm %in% nms) {
        return(which(nms == nm)[1])
    }
    if (!is.null(pos) &&
        length(parts) >=
            pos) {
        pos_name <- if (is.null(nms))
            "" else nms[[pos]]
        if (is.null(pos_name) ||
            is.na(pos_name) ||
            pos_name == "") {
            return(pos)
        }
    }
    NA_integer_
}

.mojor_ir_expr_used_arg_indices <- function(parts, nms, specs) {
    if (is.null(specs) || length(specs) == 0L) {
        return(integer(0))
    }
    if (!is.list(specs)) {
        specs <- as.list(specs)
    }
    idx <- integer(0)
    for (spec in specs) {
        if (is.null(spec)) {
            next
        }
        if (is.numeric(spec) &&
            length(spec) == 1L &&
            !is.na(spec)) {
            idx <- c(idx, as.integer(spec))
            next
        }
        if (is.list(spec) &&
            !is.null(spec$name) &&
            is.character(spec$name) &&
            length(spec$name) == 1L &&
            nzchar(spec$name)) {
            pos <- if (!is.null(spec$pos) &&
                length(spec$pos) == 1L &&
                !is.na(spec$pos)) as.integer(spec$pos) else NULL
            idx_i <- .mojor_ir_expr_get_call_arg_idx(parts, nms, spec$name, pos)
            if (!is.na(idx_i)) {
                idx <- c(idx, idx_i)
            }
        }
    }
    idx <- unique(idx)
    idx[idx >= 1L & idx <= length(parts)]
}

.mojor_ir_parse_bool_scalar_or_var <- function(arg_expr, default = NULL) {
    if (is.null(arg_expr)) {
        return(default)
    }
    if (is.logical(arg_expr) &&
        length(arg_expr) == 1 &&
        !is.na(arg_expr)) {
        return(isTRUE(arg_expr))
    }
    if (is.numeric(arg_expr) &&
        length(arg_expr) == 1 &&
        !is.na(arg_expr) &&
        arg_expr %in% c(0, 1)) {
        return(isTRUE(as.logical(arg_expr)))
    }
    if (is.name(arg_expr)) {
        nm <- as.character(arg_expr)
        if (identical(nm, "TRUE")) {
            return(TRUE)
        }
        if (identical(nm, "FALSE")) {
            return(FALSE)
        }
        return(.mojor_ir_var(nm))
    }
    NULL
}

.mojor_ir_parse_reduce_op <- function(op_expr, allowed_ops, alias_map = NULL) {
    if (is.null(op_expr)) {
        return(NULL)
    }

    op <- NULL
    if (is.character(op_expr) &&
        length(op_expr) ==
            1 && !is.na(op_expr)) {
        op <- tolower(op_expr)
    } else if (is.name(op_expr) &&
        length(op_expr) ==
            1) {
        op <- tolower(as.character(op_expr))
    }
    if (is.null(op)) {
        return(NULL)
    }

    if (!is.null(alias_map) &&
        length(alias_map) >
            0) {
        alias_names <- names(alias_map)
        if (!is.null(alias_names) &&
            op %in% alias_names) {
            op <- unname(alias_map[[op]])
        }
    }

    if (!op %in% allowed_ops) {
        return(NULL)
    }
    op
}

.mojor_ir_parse_gpu_reduce_op <- function(op_expr) {
    .mojor_ir_parse_reduce_op(
        op_expr, allowed_ops = c("sum", "mean", "prod", "min", "max", "argmin", "argmax"),
        alias_map = c(
            product = "prod",
            pmin = "min",
            pmax = "max",
            which.min = "argmin",
            which_min = "argmin",
            which.max = "argmax",
            which_max = "argmax"
        )
    )
}

.mojor_ir_parse_cpu_reduce_op <- function(op_expr) {
    .mojor_ir_parse_reduce_op(op_expr, allowed_ops = c("sum", "min", "max"))
}

.mojor_ir_build_cpu_reduce_call <- function(expr, default_op) {
    parts <- as.list(expr)[-1]
    nms <- names(parts)

    op <- .mojor_ir_parse_cpu_reduce_op(default_op)
    if (is.null(op)) {
        return(NULL)
    }

    if (length(parts) ==
        0L) {
        return(NULL)
    }
    if (!is.null(nms) &&
        length(nms) >
            0L) {
        nms_norm <- ifelse(
            is.na(nms),
            "", nms
        )
        unsupported_named <- nms_norm[nms_norm != "" & !nms_norm %in% c("x", "na.rm")]
        if (length(unsupported_named) >
            0L) {
            return(NULL)
        }
    } else if (length(parts) >
        2L) {
        return(NULL)
    }

    x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
    na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 2)
    used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, na_rm_idx))
    if (length(parts) !=
        length(used_idx)) {
        return(NULL)
    }

    x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
    if (is.null(x_arg)) {
        return(NULL)
    }
    x_ir <- .mojor_ir_expr_build(x_arg)
    if (is.null(x_ir)) {
        return(NULL)
    }

    na_rm <- FALSE
    na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 2)
    if (!is.null(na_rm_arg)) {
        if (is.logical(na_rm_arg) &&
            length(na_rm_arg) ==
                1 && !is.na(na_rm_arg)) {
            na_rm <- isTRUE(na_rm_arg)
        } else {
            return(NULL)
        }
    }

    out <- .mojor_ir_call(
        op, list(x_ir),
        src = expr
    )
    out$na_rm <- na_rm
    out
}

.mojor_ir_parse_apply_reduce_call <- function(call_expr, param_name) {
    if (!is.call(call_expr) ||
        length(call_expr) <
            2) {
        return(NULL)
    }

    fun_name <- as.character(call_expr[[1]])
    if (!fun_name %in% c("sum", "mean", "min", "max")) {
        return(NULL)
    }

    parts <- as.list(call_expr)[-1]
    nms <- names(parts)

    x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
    na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 2)
    used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, na_rm_idx))
    if (length(parts) !=
        length(used_idx)) {
        return(NULL)
    }

    x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
    if (!(is.name(x_arg) &&
        identical(
            as.character(x_arg),
            param_name
        ))) {
        return(NULL)
    }

    na_rm <- FALSE
    na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 2)
    if (!is.null(na_rm_arg)) {
        if (!(is.logical(na_rm_arg) &&
            length(na_rm_arg) ==
                1 && !is.na(na_rm_arg))) {
            return(NULL)
        }
        na_rm <- isTRUE(na_rm_arg)
    }

    list(fun = fun_name, na_rm = na_rm)
}

.mojor_ir_parse_apply_inline_fun <- function(fun_expr) {
    if (!is.call(fun_expr) ||
        as.character(fun_expr[[1]]) !=
            "function") {
        return(NULL)
    }
    if (length(fun_expr) <
        3) {
        return(NULL)
    }

    fun_formals <- fun_expr[[2]]
    if (!is.pairlist(fun_formals) ||
        length(fun_formals) !=
            1) {
        return(NULL)
    }

    param_name <- names(fun_formals)[1]
    if (is.null(param_name) ||
        !nzchar(param_name) ||
        identical(param_name, "...")) {
        return(NULL)
    }

 # Missing defaults are represented as a 'missing argument' object.
    if (!(is.symbol(list(fun_formals[[1]])[[1]]) &&
        identical(
            as.character(list(fun_formals[[1]])[[1]]),
            ""
        ))) {
        return(NULL)
    }

    body_expr <- fun_expr[[3]]
    if (is.call(body_expr) &&
        as.character(body_expr[[1]]) ==
            "{") {
        body_parts <- as.list(body_expr)[-1]
        if (length(body_parts) !=
            1) {
            return(NULL)
        }
        body_expr <- body_parts[[1]]
    }
    if (is.call(body_expr) &&
        as.character(body_expr[[1]]) ==
            "return" && length(body_expr) ==
        2) {
        body_expr <- body_expr[[2]]
    }

    .mojor_ir_parse_apply_reduce_call(body_expr, param_name)
}

.mojor_ir_build_gpu_reduce_node <- function(expr, default_op = NULL) {
    parts <- as.list(expr)[-1]
    nms <- names(parts)

    x_pos <- if (is.null(default_op))
        1 else 1
    op_pos <- if (is.null(default_op))
        2 else NULL
    dims_pos <- if (is.null(default_op))
        3 else 2
    keepdims_pos <- if (is.null(default_op))
        4 else 3

    x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", x_pos)
    if (is.null(x_arg)) {
        return(NULL)
    }
    arg <- .mojor_ir_expr_build(x_arg)
    if (is.null(arg)) {
        return(NULL)
    }

    op <- default_op
    op_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "op", op_pos)
    if (!is.null(op_arg)) {
        op <- .mojor_ir_parse_gpu_reduce_op(op_arg)
    }
    if (is.null(op)) {
        return(NULL)
    }

    dims_val <- NULL
    dims_expr <- .mojor_ir_expr_get_call_arg(parts, nms, "dims", dims_pos)
    if (!is.null(dims_expr)) {
        if (!(is.name(dims_expr) &&
            as.character(dims_expr) ==
                "NULL")) {
            dims_val <- .mojor_ir_expr_build(dims_expr)
            if (is.null(dims_val)) {
                return(NULL)
            }
        }
    }

    keepdims_val <- FALSE
    keepdims_expr <- .mojor_ir_expr_get_call_arg(parts, nms, "keepdims", keepdims_pos)
    if (!is.null(keepdims_expr)) {
        if (!(is.logical(keepdims_expr) &&
            length(keepdims_expr) ==
                1 && !is.na(keepdims_expr))) {
            return(NULL)
        }
        keepdims_val <- isTRUE(keepdims_expr)
    }

    .mojor_ir_gpu_reduce(op, arg, dims_val, keepdims_val, src = expr)
}

.mojor_ir_expr_build <- function(expr) {
    quote_string_literal <- function(val) {
        encodeString(val, quote = "\"")
    }

    if (is.logical(expr)) {
        if (is.na(expr)) {
            return(.mojor_ir_const("-2147483648"))
        }
        return(.mojor_ir_const(if (isTRUE(expr)) "True" else "False"))
    }
    if (is.integer(expr)) {
        if (length(expr) !=
            1) {
            return(NULL)
        }
        if (is.na(expr)) {
            return(.mojor_ir_const("-2147483648"))
        }
        return(.mojor_ir_const(as.character(expr)))
    }
    if (is.numeric(expr)) {
        if (length(expr) !=
            1) {
            return(NULL)
        }
        if (is.na(expr)) {
            return(.mojor_ir_const("NaN"))
        }
        if (is.nan(expr)) {
            return(.mojor_ir_const("NaN"))
        }
        if (is.infinite(expr)) {
            return(.mojor_ir_const(if (expr > 0) "Inf" else "-Inf"))
        }
        val <- as.character(expr)
        if (!grepl("[eE]", val) &&
            !grepl("\\.", val) &&
            (expr%%1) == 0) {
            val <- paste0(val, ".0")
        }
        return(.mojor_ir_const(val))
    }
    if (is.character(expr)) {
        if (length(expr) != 1) {
            return(NULL)
        }
        if (is.na(expr)) {
            return(NULL)
        }
        return(.mojor_ir_const(quote_string_literal(expr[[1]])))
    }
    if (is.name(expr)) {
        nm <- as.character(expr)
        if (nm %in% c("Inf", "NaN")) {
            return(.mojor_ir_const(nm))
        }
        if (nm %in% c("NA", "NA_real_")) {
            return(.mojor_ir_const("NaN"))
        }
        if (nm %in% c("NA_integer_", "NA_logical_")) {
            return(.mojor_ir_const("-2147483648"))
        }
        return(.mojor_ir_var(nm))
    }
    if (!is.call(expr)) {
        return(NULL)
    }
    op <- as.character(expr[[1]])
    if (op == "(" && length(expr) ==
        2) {
        return(.mojor_ir_expr_build(expr[[2]]))
    }
    if (op == "+" && length(expr) ==
        2) {
        return(.mojor_ir_expr_build(expr[[2]]))
    }
    if (op == "-" && length(expr) ==
        2) {
        inner <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(inner)) {
            return(NULL)
        }
        return(.mojor_ir_unop("-", inner))
    }
    if (op == "!" && length(expr) ==
        2) {
        inner <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(inner)) {
            return(NULL)
        }
        return(.mojor_ir_unop("!", inner))
    }
    if (op %in% c("+", "-", "*", "/", "%%", "%/%") &&
        length(expr) ==
            3) {
        lhs <- .mojor_ir_expr_build(expr[[2]])
        rhs <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        return(.mojor_ir_binop(op, lhs, rhs))
    }
    if (op == "^" && length(expr) ==
        3) {
        lhs <- .mojor_ir_expr_build(expr[[2]])
        rhs <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        return(.mojor_ir_call("pow", list(lhs, rhs)))
    }
    if (op %in% c(">", "<", ">=", "<=", "==", "!=") &&
        length(expr) ==
            3) {
        lhs <- .mojor_ir_expr_build(expr[[2]])
        rhs <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        return(.mojor_ir_binop(op, lhs, rhs))
    }
    if (op %in% c("&&", "||", "&", "|") &&
        length(expr) ==
            3) {
        lhs <- .mojor_ir_expr_build(expr[[2]])
        rhs <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        ir_op <- if (op == "&")
            "&&" else if (op == "|")
            "||" else op
        return(.mojor_ir_binop(ir_op, lhs, rhs))
    }
    if (op == "xor" && length(expr) ==
        3) {
        lhs <- .mojor_ir_expr_build(expr[[2]])
        rhs <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        lhs_bool <- .mojor_ir_call("as.logical", list(lhs))
        rhs_bool <- .mojor_ir_call("as.logical", list(rhs))
        return(.mojor_ir_binop("!=", lhs_bool, rhs_bool))
    }
    if (op %in% c("as.integer", "as.double", "as.single") &&
        length(expr) ==
            2) {
        inner <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(inner)) {
            return(NULL)
        }
        to <- switch(op, as.integer = "Int", as.double = "Float64", as.single = "Float32", NULL)
        if (is.null(to)) {
            return(NULL)
        }
        return(.mojor_ir_cast(to, inner))
    }
    if (op == "length" && length(expr) ==
        2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_call("length", list(arg)))
    }
    if (op %in% c("nrow", "ncol") && length(expr) == 2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_call(op, list(arg)))
    }
    if (op %in% c("is.na", "is.nan", "is.finite", "is.infinite", "as.logical") &&
        length(expr) ==
            2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_call(op, list(arg)))
    }
 # Type predicates (compile-time constant folding)
    type_predicates <- c(
        "is.vector", "is.matrix", "is.array", "is.numeric", "is.integer", "is.double", "is.logical"
    )
    if (op %in% type_predicates && length(expr) ==
        2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_type_predicate(op, arg, src = expr))
    }
 # Type queries (compile-time constant folding)
    type_queries <- c("typeof", "mode", "class")
    if (op %in% type_queries && length(expr) ==
        2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_type_query(op, arg, src = expr))
    }
 # dim() query (runtime evaluation)
    if (op == "dim" && length(expr) ==
        2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_dim(arg, src = expr))
    }
 # Step 7: Build math function calls from R AST
# --- 2-arg log(x, base) lowering: log(x) / log(base) ---
    if (op == "log" && length(expr) == 3) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        base_ir <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(x_ir) || is.null(base_ir)) return(NULL)
        return(.mojor_ir_binop("/",
            .mojor_ir_call("log", list(x_ir)),
            .mojor_ir_call("log", list(base_ir))))
    }
# --- 2-arg round(x, digits) lowering: round(x * 10^d) / 10^d ---
    if (op == "round" && length(expr) == 3) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        d_ir <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(x_ir) || is.null(d_ir)) return(NULL)
        ten <- .mojor_ir_const("10.0")
        scale <- .mojor_ir_call("pow", list(ten, d_ir))
        return(.mojor_ir_binop("/",
            .mojor_ir_call("round", list(.mojor_ir_binop("*", x_ir, scale))),
            scale))
    }
# --- factorial(x) lowering: gamma(x + 1) ---
    if (op == "factorial" && length(expr) == 2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) return(NULL)
        return(.mojor_ir_call("gamma", list(
            .mojor_ir_binop("+", arg, .mojor_ir_const("1.0")))))
    }
# --- beta(a, b) lowering: exp(lgamma(a) + lgamma(b) - lgamma(a+b)) ---
    if (op == "beta" && length(expr) == 3) {
        a_ir <- .mojor_ir_expr_build(expr[[2]])
        b_ir <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(a_ir) || is.null(b_ir)) return(NULL)
        la <- .mojor_ir_call("lgamma", list(a_ir))
        lb <- .mojor_ir_call("lgamma", list(b_ir))
        lab <- .mojor_ir_call("lgamma", list(.mojor_ir_binop("+", a_ir, b_ir)))
        return(.mojor_ir_call("exp", list(
            .mojor_ir_binop("-", .mojor_ir_binop("+", la, lb), lab))))
    }
# --- choose(n, k) lowering: round(exp(lgamma(n+1) - lgamma(k+1) - lgamma(n-k+1))) ---
    if (op == "choose" && length(expr) == 3) {
        n_ir <- .mojor_ir_expr_build(expr[[2]])
        k_ir <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(n_ir) || is.null(k_ir)) return(NULL)
        one <- .mojor_ir_const("1.0")
        ln1 <- .mojor_ir_call("lgamma", list(.mojor_ir_binop("+", n_ir, one)))
        lk1 <- .mojor_ir_call("lgamma", list(.mojor_ir_binop("+", k_ir, one)))
        lnk1 <- .mojor_ir_call("lgamma", list(
            .mojor_ir_binop("+", .mojor_ir_binop("-", n_ir, k_ir), one)))
        inner <- .mojor_ir_call("exp", list(
            .mojor_ir_binop("-", .mojor_ir_binop("-", ln1, lk1), lnk1)))
        return(.mojor_ir_call("round", list(inner)))
    }
    math_fns <- c(
        "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh",
        "atanh", "log", "log10", "log1p", "log2", "exp", "expm1", "sqrt", "abs", "abs2", "floor",
        "ceiling", "trunc", "round", "sign", "cbrt", "lgamma", "erf", "gamma"
    )
    if (op %in% math_fns && length(expr) ==
        2) {
        arg <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(arg)) {
            return(NULL)
        }
        return(.mojor_ir_call(op, list(arg)))
    }
    if (op %in% c("atan2", "hypot") &&
        length(expr) ==
            3) {
        arg1 <- .mojor_ir_expr_build(expr[[2]])
        arg2 <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(arg1) ||
            is.null(arg2)) {
            return(NULL)
        }
        return(.mojor_ir_call(op, list(arg1, arg2)))
    }
 # Step 8.9: Build min/max function calls from R AST Support variadic min/max by
 # folding into nested binary calls.
    if (op == "sum" && length(expr) >=
        2) {
        return(.mojor_ir_build_cpu_reduce_call(expr, "sum"))
    }
    if (op %in% c("min", "max") &&
        length(expr) >=
            2) {
        unary_reduce <- .mojor_ir_build_cpu_reduce_call(expr, op)
        if (!is.null(unary_reduce)) {
            return(unary_reduce)
        }
    }
    if (op %in% c("min", "max") &&
        length(expr) >=
            3) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm")
        if (!is.null(na_rm_arg)) {
            if (!(is.logical(na_rm_arg) &&
                length(na_rm_arg) ==
                  1 && !is.na(na_rm_arg))) {
                return(NULL)
            }
        }
        if (!is.null(nms) &&
            length(nms) >
                0L) {
            nms_norm <- ifelse(
                is.na(nms),
                "", nms
            )
            unsupported_named <- nms_norm[nms_norm != "" & nms_norm != "na.rm"]
            if (length(unsupported_named) >
                0L) {
                return(NULL)
            }
            keep_idx <- which(nms_norm == "")
            parts <- parts[keep_idx]
        }
        if (length(parts) <
            2L) {
            return(NULL)
        }
        args <- lapply(parts, .mojor_ir_expr_build)
        if (any(vapply(args, is.null, logical(1)))) {
            return(NULL)
        }
        cur <- args[[1]]
        if (length(args) >
            1) {
            for (k in 2:length(args)) {
                cur <- .mojor_ir_call(op, list(cur, args[[k]]))
            }
        }
        return(cur)
    }
    if (op %in% c("pmin", "pmax") &&
        length(expr) >=
            3) {
        fn <- if (op == "pmin")
            "min" else "max"
        args <- lapply(
            as.list(expr)[-1],
            .mojor_ir_expr_build
        )
        if (any(vapply(args, is.null, logical(1)))) {
            return(NULL)
        }
        cur <- args[[1]]
        if (length(args) >
            1) {
            for (k in 2:length(args)) {
                cur <- .mojor_ir_call(fn, list(cur, args[[k]]))
            }
        }
        return(cur)
    }
    if (op %in% c("[", "[[")) {
        return(.mojor_ir_build_index(expr))
    }
 # Lower expression-form if/else to ifelse IR node.
    if (op == "if" && length(expr) ==
        4) {
        cond <- .mojor_ir_expr_build(expr[[2]])
        yes <- .mojor_ir_expr_build(expr[[3]])
        no <- .mojor_ir_expr_build(expr[[4]])
        if (is.null(cond) ||
            is.null(yes) ||
            is.null(no)) {
            return(NULL)
        }
        return(.mojor_ir_ifelse(cond, yes, no, src = expr))
    }
 # Step 6.2: Build ifelse from R AST
    if (op == "ifelse" && length(expr) ==
        4) {
        cond <- .mojor_ir_expr_build(expr[[2]])
        yes <- .mojor_ir_expr_build(expr[[3]])
        no <- .mojor_ir_expr_build(expr[[4]])
        if (is.null(cond) ||
            is.null(yes) ||
            is.null(no)) {
            return(NULL)
        }
 # Emit warning if option enabled
        if (isTRUE(.mojor_state$options$warn_ifelse)) {
            .mojor_warn(
                "ifelse() expression lowered to ternary", expr, "prefer explicit if/else for best performance"
            )
        }
        return(.mojor_ir_ifelse(cond, yes, no))
    }

 # Constructor calls used in strict pre-loop allocation lowering paths.
 # matrix()/array() keep src-driven lowering paths even when auxiliary
 # arguments (for example dimnames=list(...)) are not IR-buildable.
    if (op %in% c("numeric", "integer", "logical", "matrix", "array")) {
        parts <- as.list(expr)[-1]
        args_ir <- list()
        allow_partial_args <- op %in% c("matrix", "array")
        if (length(parts) >
            0) {
            for (part in parts) {
                if (identical(part, quote(expr =))) {
                  return(NULL)
                }
                part_ir <- .mojor_ir_expr_build(part)
                if (is.null(part_ir)) {
                  if (isTRUE(allow_partial_args)) {
                    next
                  }
                  return(NULL)
                }
                args_ir[[length(args_ir) +
                  1L]] <- part_ir
            }
        }
        return(.mojor_ir_call(op, args_ir, src = expr))
    }

 # Step 8.12: Build constructor calls from R AST
    if (op == "rep" && length(expr) >=
        2) {
 # rep(x, times=, each=, length.out=)
        parts <- as.list(expr)[-1]
        nms <- names(parts)

        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        if (is.null(x_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        times_ir <- NULL
        times_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "times", 2)
        if (!is.null(times_arg)) {
            times_ir <- .mojor_ir_expr_build(times_arg)
        }

        each_ir <- NULL
        each_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "each", 4)
        if (!is.null(each_arg)) {
            each_ir <- .mojor_ir_expr_build(each_arg)
        }

        length_out_ir <- NULL
        length_out_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "length.out", 3)
        if (!is.null(length_out_arg)) {
            length_out_ir <- .mojor_ir_expr_build(length_out_arg)
        }

        return(.mojor_ir_rep(x_ir, times_ir, each_ir, length_out_ir, src = expr))
    }

    if (op == "rep_len" && length(expr) >=
        2) {
 # rep_len(x, length.out)
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        len_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "length.out", 2)
        if (is.null(x_arg) ||
            is.null(len_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        length_out_ir <- .mojor_ir_expr_build(len_arg)
        if (is.null(x_ir) ||
            is.null(length_out_ir)) {
            return(NULL)
        }
        return(.mojor_ir_rep_len(x_ir, length_out_ir, src = expr))
    }

    if (op == "rep.int" && length(expr) >=
        2) {
 # rep.int(x, times) - same as rep(x, times=)
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        times_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "times", 2)
        if (is.null(x_arg) ||
            is.null(times_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        times_ir <- .mojor_ir_expr_build(times_arg)
        if (is.null(x_ir) ||
            is.null(times_ir)) {
            return(NULL)
        }
        return(.mojor_ir_rep(x_ir, times_ir, NULL, NULL, src = expr))
    }

    if (op == "c") {
 # c(...) - concatenation
        parts <- as.list(expr)[-1]
        if (length(parts) ==
            0) {
            return(NULL)
        }

 # Capture element names for named vectors like c(a=1, b=2)
        nms <- names(parts)
        element_names <- NULL
        if (!is.null(nms)) {
            has_names <- nms != ""
            if (any(has_names)) {
                element_names <- nms
            }
        }

        parts_ir <- lapply(parts, .mojor_ir_expr_build)
        if (any(vapply(parts_ir, is.null, logical(1)))) {
            return(NULL)
        }
        node <- .mojor_ir_c(parts_ir, src = expr)
        if (!is.null(element_names)) {
            node$element_names <- element_names
        }
        return(node)
    }

 # PR-B3: seq(from, to, length.out) - sequence generation
    if (op %in% c("seq", "seq.int") &&
        length(expr) >=
            2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)

 # Handle along.with: convert to length.out = length(array)
        along_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "along.with", 4)
        if (!is.null(along_arg)) {
 # along.with: use length of the specified array
            if (is.name(along_arg)) {
                arr_name <- as.character(along_arg)
                len_expr <- call("length", as.name(arr_name))
                length_out_ir <- .mojor_ir_expr_build(len_expr)
                if (is.null(length_out_ir)) {
                  return(NULL)
                }
 # Use default from=1, to=length
                from_ir <- .mojor_ir_const("1")
                to_ir <- NULL
                return(.mojor_ir_seq(from_ir, to_ir, length_out_ir, src = expr))
            }
            return(NULL)
        }

        from_ir <- NULL
        from_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "from", 1)
        if (!is.null(from_arg)) {
            from_ir <- .mojor_ir_expr_build(from_arg)
        } else {
            from_ir <- .mojor_ir_const("1.0")  # Default from=1
        }

        to_ir <- NULL
        to_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "to", 2)
        if (!is.null(to_arg)) {
            to_ir <- .mojor_ir_expr_build(to_arg)
        }

        length_out_ir <- NULL
        len_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "length.out", 3)
        if (!is.null(len_arg)) {
            length_out_ir <- .mojor_ir_expr_build(len_arg)
        }

 # Require at least from and length.out for now
        if (is.null(from_ir) ||
            is.null(length_out_ir)) {
            return(NULL)
        }

        return(.mojor_ir_seq(from_ir, to_ir, length_out_ir, src = expr))
    }

 # PR-B3: seq_along(x) and seq_len(n) - sequence generation Also handles
 # seq_along(along.with = x) and seq_len(along.with = n)
    if (op %in% c("seq_along", "seq_len") &&
        length(expr) >=
            2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)

 # Handle along.with: convert to length.out = length(array)
        along_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "along.with", 4)
        if (!is.null(along_arg)) {
 # along.with: use length of the specified array
            if (is.name(along_arg)) {
                arr_name <- as.character(along_arg)
                len_expr <- call("length", as.name(arr_name))
                length_out_ir <- .mojor_ir_expr_build(len_expr)
                if (is.null(length_out_ir)) {
                  return(NULL)
                }
 # Use default from=1, to=length
                from_ir <- .mojor_ir_const("1")
                to_ir <- NULL
                return(.mojor_ir_seq(from_ir, to_ir, length_out_ir, src = expr))
            }
            return(NULL)
        }

 # Handle regular seq_along(x) - use length of array
        if (op == "seq_along") {
            arr_arg <- expr[[2]]
            if (is.name(arr_arg)) {
                arr_name <- as.character(arr_arg)
                len_expr <- call("length", as.name(arr_name))
                length_out_ir <- .mojor_ir_expr_build(len_expr)
                if (is.null(length_out_ir)) {
                  return(NULL)
                }
                from_ir <- .mojor_ir_const("1")
                to_ir <- NULL
                return(.mojor_ir_seq(from_ir, to_ir, length_out_ir, src = expr))
            }
            return(NULL)
        }

 # Handle regular seq_len(n) - use scalar n
        if (op == "seq_len") {
            len_arg <- expr[[2]]
            length_out_ir <- .mojor_ir_expr_build(len_arg)
            if (is.null(length_out_ir)) {
                return(NULL)
            }
            from_ir <- .mojor_ir_const("1")
            to_ir <- NULL
            return(.mojor_ir_seq(from_ir, to_ir, length_out_ir, src = expr))
        }

        return(NULL)
    }

 # Step RNG-IR: Build RNG function calls from R AST Support vectorized RNG
 # (runif(n), rnorm(n, ...))
    if (op == "runif" && length(expr) >=
        2) {
 # runif(n, min=0, max=1) First argument is n (number of samples)
        n_arg <- expr[[2]]
        n_ir <- .mojor_ir_expr_build(n_arg)
        if (is.null(n_ir)) {
            return(NULL)
        }

 # Collect additional parameters (min, max)
        params <- list()
        if (length(expr) >=
            3) {
            for (i in 3:length(expr)) {
                param_ir <- .mojor_ir_expr_build(expr[[i]])
                if (is.null(param_ir)) {
                  return(NULL)
                }
                params[[i - 2]] <- param_ir
            }
        }
        return(.mojor_ir_rng_vec("runif", n_ir, params, src = expr))
    }

    if (op == "rnorm" && length(expr) >=
        2) {
 # rnorm(n, mean=0, sd=1) First argument is n (number of samples)
        n_arg <- expr[[2]]
        n_ir <- .mojor_ir_expr_build(n_arg)
        if (is.null(n_ir)) {
            return(NULL)
        }

 # Collect additional parameters (mean, sd)
        params <- list()
        if (length(expr) >=
            3) {
            for (i in 3:length(expr)) {
                param_ir <- .mojor_ir_expr_build(expr[[i]])
                if (is.null(param_ir)) {
                  return(NULL)
                }
                params[[i - 2]] <- param_ir
            }
        }
        return(.mojor_ir_rng_vec("rnorm", n_ir, params, src = expr))
    }

    rng_call_fns <- setdiff(.mojor_ir_rng_call_fns(), c("runif", "rnorm"))
    if (op %in% rng_call_fns && length(expr) >=
        2) {
 # Scalar RNG call form (typically n=1 inside loops).
        args_ir <- list()
        for (i in 2:length(expr)) {
            arg_ir <- .mojor_ir_expr_build(expr[[i]])
            if (is.null(arg_ir)) {
                return(NULL)
            }
            args_ir[[i - 1]] <- arg_ir
        }
        return(.mojor_ir_call(op, args_ir))
    }

 # PR-B3 Step 3: Array utility functions t(x) - matrix transpose
    if (op == "t" && length(expr) ==
        2) {
        x_arg <- expr[[2]]
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }
        return(.mojor_ir_transpose(x_ir, src = expr))
    }

 # cbind(...) - column binding
    if (op == "cbind" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
 # Filter out named parameters
        nms <- names(parts)
        if (!is.null(nms)) {
            named_idx <- which(nms != "")
            if (length(named_idx) >
                0) {
                parts <- parts[-named_idx]
            }
        }
        if (length(parts) ==
            0) {
            return(NULL)
        }
        parts_ir <- lapply(parts, .mojor_ir_expr_build)
        if (any(vapply(parts_ir, is.null, logical(1)))) {
            return(NULL)
        }
        return(.mojor_ir_cbind(parts_ir, src = expr))
    }

 # rbind(...) - row binding
    if (op == "rbind" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
 # Filter out named parameters
        nms <- names(parts)
        if (!is.null(nms)) {
            named_idx <- which(nms != "")
            if (length(named_idx) >
                0) {
                parts <- parts[-named_idx]
            }
        }
        if (length(parts) ==
            0) {
            return(NULL)
        }
        parts_ir <- lapply(parts, .mojor_ir_expr_build)
        if (any(vapply(parts_ir, is.null, logical(1)))) {
            return(NULL)
        }
        return(.mojor_ir_rbind(parts_ir, src = expr))
    }

 # PR-B4: diag() - matrix diagonal operations
    if (op == "diag" && length(expr) >=
        2) {
 # diag(x) or diag(n)
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }

 # Determine if it's identity mode (scalar) or extraction/creation (vector/matrix)
 # We can't know at build time, so we emit both possibilities The emitter will
 # handle the different modes
        return(.mojor_ir_diag(x = arg_ir, src = expr))
    }

 # PR-B5: Cumulative operations
    if (op == "cumsum" && length(expr) >=
        2) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(x_ir)) {
            return(NULL)
        }
        return(.mojor_ir_cumsum(x = x_ir, src = expr))
    }

    if (op == "cumprod" && length(expr) >=
        2) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(x_ir)) {
            return(NULL)
        }
        return(.mojor_ir_cumprod(x = x_ir, src = expr))
    }

 # PR-B5 Step 3: Additional cumulative operations
    if (op == "cummax" && length(expr) >=
        2) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(x_ir)) {
            return(NULL)
        }
        return(.mojor_ir_cummax(x = x_ir, src = expr))
    }

    if (op == "cummin" && length(expr) >=
        2) {
        x_ir <- .mojor_ir_expr_build(expr[[2]])
        if (is.null(x_ir)) {
            return(NULL)
        }
        return(.mojor_ir_cummin(x = x_ir, src = expr))
    }

 # PR-B5: Statistical functions
    if (op == "mean" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        if (length(parts) ==
            0L) {
            return(NULL)
        }

        if (!is.null(nms) &&
            length(nms) >
                0L) {
            nms_norm <- ifelse(
                is.na(nms),
                "", nms
            )
            unsupported_named <- nms_norm[nms_norm != "" & !nms_norm %in% c("x", "na.rm")]
            if (length(unsupported_named) >
                0L) {
                return(NULL)
            }
        } else if (length(parts) >
            2L) {
            return(NULL)
        }

        x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
        na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 2)
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, na_rm_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }

        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        if (is.null(x_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        na_rm <- FALSE
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 2)
        if (!is.null(na_rm_arg)) {
            if (!(is.logical(na_rm_arg) &&
                length(na_rm_arg) ==
                  1 && !is.na(na_rm_arg))) {
                return(NULL)
            }
            na_rm <- isTRUE(na_rm_arg)
        }

        return(.mojor_ir_mean(x = x_ir, na_rm = na_rm, src = expr))
    }

 # PR-B5 Step 3: Variance and standard deviation
    if (op == "var" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        if (length(parts) ==
            0L) {
            return(NULL)
        }

        if (!is.null(nms) &&
            length(nms) >
                0L) {
            nms_norm <- ifelse(
                is.na(nms),
                "", nms
            )
            unsupported_named <- nms_norm[nms_norm != "" & !nms_norm %in% c("x", "na.rm")]
            if (length(unsupported_named) >
                0L) {
                return(NULL)
            }
        } else if (length(parts) >
            2L) {
            return(NULL)
        }

        x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
        na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 2)
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, na_rm_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }

        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        if (is.null(x_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        na_rm <- FALSE
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 2)
        if (!is.null(na_rm_arg)) {
            if (!(is.logical(na_rm_arg) &&
                length(na_rm_arg) ==
                  1 && !is.na(na_rm_arg))) {
                return(NULL)
            }
            na_rm <- isTRUE(na_rm_arg)
        }

        return(.mojor_ir_variance(x = x_ir, na_rm = na_rm, src = expr))
    }

    if (op == "sd" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        if (length(parts) ==
            0L) {
            return(NULL)
        }

        if (!is.null(nms) &&
            length(nms) >
                0L) {
            nms_norm <- ifelse(
                is.na(nms),
                "", nms
            )
            unsupported_named <- nms_norm[nms_norm != "" & !nms_norm %in% c("x", "na.rm")]
            if (length(unsupported_named) >
                0L) {
                return(NULL)
            }
        } else if (length(parts) >
            2L) {
            return(NULL)
        }

        x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
        na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 2)
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, na_rm_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }

        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        if (is.null(x_arg)) {
            return(NULL)
        }
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        na_rm <- FALSE
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 2)
        if (!is.null(na_rm_arg)) {
            if (!(is.logical(na_rm_arg) &&
                length(na_rm_arg) ==
                  1 && !is.na(na_rm_arg))) {
                return(NULL)
            }
            na_rm <- isTRUE(na_rm_arg)
        }

        return(.mojor_ir_sd(x = x_ir, na_rm = na_rm, src = expr))
    }

 # apply() - rewrite to loops + reductions
    if (op == "apply" && length(expr) >=
        3) {
 # apply(X, MARGIN, FUN, ...) MVP: MARGIN = 1 (rows) or 2 (columns) MVP: FUN =
 # sum/mean/min/max symbol or simple inline lambda Optional: na.rm = TRUE/FALSE
 # (symbol FUN only)
        parts <- as.list(expr)[-1]
        nms <- names(parts)

        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "X", 1)
        if (is.null(x_arg))
            x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        margin_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "MARGIN", 2)
        if (is.null(margin_arg))
            margin_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "margin", 2)
        fun_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "FUN", 3)
        if (is.null(fun_arg))
            fun_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "fun", 3)

        if (is.null(x_arg) ||
            is.null(margin_arg) ||
            is.null(fun_arg)) {
            return(NULL)
        }

        x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "X", 1)
        if (is.na(x_idx))
            x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
        margin_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "MARGIN", 2)
        if (is.na(margin_idx))
            margin_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "margin", 2)
        fun_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "FUN", 3)
        if (is.na(fun_idx))
            fun_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "fun", 3)
        na_rm_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "na.rm", 4)
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, margin_idx, fun_idx, na_rm_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }

        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

 # Parse MARGIN (must be constant 1 or 2 for MVP)
        if (!is.integer(margin_arg) &&
            !is.numeric(margin_arg)) {
            return(NULL)
        }
        margin <- as.integer(margin_arg)
        if (length(margin) !=
            1 || !(margin %in% c(1, 2))) {
            return(NULL)
        }

        na_rm <- FALSE
        has_outer_na_rm <- FALSE
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 4)
        if (!is.null(na_rm_arg)) {
            na_rm_parsed <- .mojor_ir_parse_bool_scalar_or_var(na_rm_arg, default = FALSE)
            if (is.null(na_rm_parsed)) {
                return(NULL)
            }
            has_outer_na_rm <- TRUE
            na_rm <- na_rm_parsed
        }

 # Parse FUN (symbol or strict inline lambda).
        if (is.name(fun_arg)) {
            fun_name <- as.character(fun_arg)
            allowed_funs <- c("sum", "mean", "min", "max")
            if (!(fun_name %in% allowed_funs)) {
                return(NULL)
            }
        } else {
            fun_spec <- .mojor_ir_parse_apply_inline_fun(fun_arg)
            if (is.null(fun_spec)) {
                return(NULL)
            }
            if (has_outer_na_rm) {
                return(NULL)
            }
            fun_name <- fun_spec$fun
            na_rm <- fun_spec$na_rm
        }

 # Reject unsupported named extras to avoid silently dropping arguments.
        if (!is.null(nms) &&
            length(nms) >
                0) {
            named <- unique(nms[nzchar(nms)])
            if (length(setdiff(named, c("X", "x", "MARGIN", "margin", "FUN", "fun", "na.rm"))) >
                0) {
                return(NULL)
            }
        }

 # Return a special node that will be lowered during emit.
        return(.mojor_ir_apply(x = x_ir, margin = margin, fun = fun_name, na_rm = na_rm, src = expr))
    }

 # sample.int() - rewrite to sampling loop
    if (op == "sample.int" && length(expr) >=
        2) {
        sample_int_sig_err <- "mojor_transpile: sample.int() supports only n, size, replace, prob arguments"
 # sample.int(n, size = n, replace = FALSE, prob = NULL)
        n_arg <- expr[[2]]
        n_ir <- .mojor_ir_expr_build(n_arg)
        if (is.null(n_ir)) {
            return(NULL)
        }

        size_arg <- NULL
        replace_val <- NULL
        prob_val <- NULL

        if (length(expr) >=
            3) {
            for (i in 3:length(expr)) {
                arg <- expr[[i]]
                if (is.call(arg) &&
                  as.character(arg[[1]]) ==
                    "=") {
                  name <- as.character(arg[[2]])
                  if (name == "size") {
                    if (!is.null(size_arg)) {
                      stop(sample_int_sig_err)
                    }
                    size_arg <- arg[[3]]
                  } else if (name == "replace") {
                    if (!is.null(replace_val)) {
                      stop(sample_int_sig_err)
                    }
                    replace_val <- arg[[3]]
                  } else if (name == "prob") {
                    if (!is.null(prob_val)) {
                      stop(sample_int_sig_err)
                    }
                    prob_val <- arg[[3]]
                  } else {
                    stop(sample_int_sig_err)
                  }
                } else {
                  if (is.null(size_arg)) {
                    size_arg <- arg
                  } else if (is.null(replace_val)) {
                    replace_val <- arg
                  } else if (is.null(prob_val)) {
                    prob_val <- arg
                  } else {
                    stop(sample_int_sig_err)
                  }
                }
            }
        }

        if (is.null(size_arg))
            size_arg <- n_arg
        size_ir <- .mojor_ir_expr_build(size_arg)
        if (is.null(size_ir)) {
            return(NULL)
        }
        replace_ir <- if (!is.null(replace_val))
            .mojor_ir_expr_build(replace_val) else NULL
        if (!is.null(replace_val) &&
            is.null(replace_ir)) {
            return(NULL)
        }
        prob_ir <- if (!is.null(prob_val))
            .mojor_ir_expr_build(prob_val) else NULL
        if (!is.null(prob_val) &&
            is.null(prob_ir)) {
            return(NULL)
        }

        return(
            .mojor_ir_sample_int(n = n_ir, size = size_ir, replace = replace_ir, prob = prob_ir, src = expr)
        )
    }

 # sample() - rewrite to sample.int()
    if (op == "sample" && length(expr) >=
        2) {
        sample_sig_err <- "mojor_transpile: sample() supports only x, X, size, replace, prob arguments"
 # sample(x, size = length(x), replace = FALSE, prob = NULL)
        x_arg <- expr[[2]]
        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        size_arg <- NULL
        replace_val <- NULL
        prob_val <- NULL

        if (length(expr) >=
            3) {
            for (i in 3:length(expr)) {
                arg <- expr[[i]]
                if (is.call(arg) &&
                  as.character(arg[[1]]) ==
                    "=") {
                  name <- as.character(arg[[2]])
                  if (name == "size") {
                    if (!is.null(size_arg)) {
                      stop(sample_sig_err)
                    }
                    size_arg <- arg[[3]]
                  } else if (name == "replace") {
                    if (!is.null(replace_val)) {
                      stop(sample_sig_err)
                    }
                    replace_val <- arg[[3]]
                  } else if (name == "prob") {
                    if (!is.null(prob_val)) {
                      stop(sample_sig_err)
                    }
                    prob_val <- arg[[3]]
                  } else {
                    stop(sample_sig_err)
                  }
                } else {
                  if (is.null(size_arg)) {
                    size_arg <- arg
                  } else if (is.null(replace_val)) {
                    replace_val <- arg
                  } else if (is.null(prob_val)) {
                    prob_val <- arg
                  } else {
                    stop(sample_sig_err)
                  }
                }
            }
        }

        if (is.null(size_arg)) {
            size_arg <- .mojor_ir_call("length", list(x_ir))
        } else {
            size_arg <- .mojor_ir_expr_build(size_arg)
            if (is.null(size_arg)) {
                return(NULL)
            }
        }

        replace_ir <- if (!is.null(replace_val))
            .mojor_ir_expr_build(replace_val) else NULL
        if (!is.null(replace_val) &&
            is.null(replace_ir)) {
            return(NULL)
        }
        prob_ir <- if (!is.null(prob_val))
            .mojor_ir_expr_build(prob_val) else NULL
        if (!is.null(prob_val) &&
            is.null(prob_ir)) {
            return(NULL)
        }

        return(
            .mojor_ir_sample(x = x_ir, size = size_arg, replace = replace_ir, prob = prob_ir, src = expr)
        )
    }

 # Set/Match Primitives unique(x)
    if (op == "unique" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_unique(x = arg_ir, src = expr))
    }

 # duplicated(x)
    if (op == "duplicated" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_duplicated(x = arg_ir, src = expr))
    }

 # anyDuplicated(x)
    if (op == "anyDuplicated" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_any_duplicated(x = arg_ir, src = expr))
    }

 # match(x, table)
    if (op == "match" && length(expr) ==
        3) {
        x_arg <- expr[[2]]
        table_arg <- expr[[3]]
        x_ir <- .mojor_ir_expr_build(x_arg)
        table_ir <- .mojor_ir_expr_build(table_arg)
        if (is.null(x_ir) ||
            is.null(table_ir)) {
            return(NULL)
        }
        return(.mojor_ir_match(x = x_ir, table = table_ir, src = expr))
    }

 # %in% operator (x %in% table)
    if (op == "%in%" && length(expr) ==
        3) {
        x_arg <- expr[[2]]
        table_arg <- expr[[3]]
        x_ir <- .mojor_ir_expr_build(x_arg)
        table_ir <- .mojor_ir_expr_build(table_arg)
        if (is.null(x_ir) ||
            is.null(table_ir)) {
            return(NULL)
        }
        return(.mojor_ir_in(x = x_ir, table = table_ir, src = expr))
    }

 # Quantiles & Robust Stats median(x)
    if (op == "median" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_median(x = arg_ir, src = expr))
    }

 # quantile(x, probs, na.rm = FALSE, type = 7)
    if (op == "quantile" && length(expr) >=
        2) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        x_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "x", 1)
        probs_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "probs", 2)
        na_rm_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "na.rm", 3)
        type_arg <- .mojor_ir_expr_get_call_arg(parts, nms, "type", 4)

        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        probs_ir <- NULL
        if (!is.null(probs_arg)) {
            probs_ir <- .mojor_ir_expr_build(probs_arg)
            if (is.null(probs_ir)) {
                return(NULL)
            }
        }

        na_rm_ir <- NULL
        if (!is.null(na_rm_arg)) {
            na_rm_ir <- .mojor_ir_expr_build(na_rm_arg)
            if (is.null(na_rm_ir)) {
                return(NULL)
            }
        }

        type_ir <- NULL
        if (!is.null(type_arg)) {
            type_ir <- .mojor_ir_expr_build(type_arg)
            if (is.null(type_ir)) {
                return(NULL)
            }
        }

        return(
            .mojor_ir_quantile(x = x_ir, probs = probs_ir, na_rm = na_rm_ir, type = type_ir, src = expr)
        )
    }

 # IQR(x)
    if (op == "IQR" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_iqr(x = arg_ir, src = expr))
    }

 # mad(x)
    if (op == "mad" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_mad(x = arg_ir, src = expr))
    }

 # ============================================================================= Tier
 # 8: Higher-Order Functions, Strings, and Sampling
 # =============================================================================

 # Higher-Order Functions

 # vapply(x, FUN, FUN.VALUE) - apply function with known return type
    if (op == "vapply" && length(expr) >=
        3 && length(expr) <=
        4) {
        x_arg <- expr[[2]]
        fun_arg <- expr[[3]]
        fun_value_arg <- expr[[4]]

        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

 # Parse FUN.VALUE - expected return type in numeric(1)/integer(1)/logical(1)
 # form.
        if (is.call(fun_value_arg) &&
            length(fun_value_arg) ==
                3 && as.character(fun_value_arg[[1]]) %in%
            c("=", "<-") &&
            as.character(fun_value_arg[[2]]) %in%
                c("FUN.VALUE", "FUN_VALUE")) {
            fun_value_arg <- fun_value_arg[[3]]
        }
        parse_fun_value_type <- function(node) {
            if (is.null(node)) {
                return(NULL)
            }
            if (is.character(node) &&
                length(node) ==
                  1 && !is.na(node)) {
                token <- node
            } else if (is.name(node)) {
                token <- as.character(node)
            } else if (is.call(node) &&
                length(node) ==
                  2) {
                ctor <- as.character(node[[1]])
                n_arg <- suppressWarnings(as.integer(node[[2]]))
                if (is.na(n_arg) ||
                  n_arg != 1L) {
                  return(NULL)
                }
                token <- ctor
            } else {
                return(NULL)
            }
            if (token %in% c("f64", "numeric", "double")) {
                return("f64")
            }
            if (token %in% c("i32", "integer")) {
                return("i32")
            }
            if (token %in% c("lgl", "bool", "logical")) {
                return("lgl")
            }
            NULL
        }
        fun_value_type <- parse_fun_value_type(fun_value_arg)

        if (is.null(fun_value_type)) {
            return(NULL)
        }

        return(.mojor_ir_vapply(x = x_ir, fun = fun_arg, fun_value_type = fun_value_type, src = expr))
    }

 # sapply(x, FUN) - apply function, simplify to vector
    if (op == "sapply" && length(expr) ==
        3) {
        x_arg <- expr[[2]]
        fun_arg <- expr[[3]]

        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        return(.mojor_ir_sapply(x = x_ir, fun = fun_arg, src = expr))
    }

 # lapply(x, FUN) - apply function, return list (deferred to future)
    if (op == "lapply" && length(expr) ==
        3) {
        x_arg <- expr[[2]]
        fun_arg <- expr[[3]]

        x_ir <- .mojor_ir_expr_build(x_arg)
        if (is.null(x_ir)) {
            return(NULL)
        }

        return(.mojor_ir_lapply(x = x_ir, fun = fun_arg, src = expr))
    }

 # mapply(FUN, ...) - multivariate apply
    if (op == "mapply" && length(expr) >=
        2) {
        fun_arg <- expr[[2]]
        args <- lapply(
            expr[3:length(expr)],
            .mojor_ir_expr_build
        )
        if (any(sapply(args, is.null))) {
            return(NULL)
        }

        return(.mojor_ir_mapply(fun = fun_arg, args = args, src = expr))
    }

 # String Basics

 # nchar(x) - number of characters
    if (op == "nchar" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_nchar(x = arg_ir, src = expr))
    }

 # nzchar(x) - non-zero length strings
    if (op == "nzchar" && length(expr) ==
        2) {
        arg <- expr[[2]]
        arg_ir <- .mojor_ir_expr_build(arg)
        if (is.null(arg_ir)) {
            return(NULL)
        }
        return(.mojor_ir_nzchar(x = arg_ir, src = expr))
    }

    is_null_expr <- function(node) {
        is.null(node) ||
            (is.name(node) &&
                identical(
                  as.character(node),
                  "NULL"
              ))
    }

 # substr(x, start, stop) / substring(text, first, last)
    if (op %in% c("substr", "substring")) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )

        allowed_named <- if (op == "substring")
            c("text", "first", "last", "x", "start", "stop") else c("x", "start", "stop")
        unsupported_named <- nms[nms != "" & !nms %in% allowed_named]
        if (length(unsupported_named) >
            0) {
            return(NULL)
        }

        if (op == "substring") {
            x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "text", 1)
            if (is.na(x_idx))
                x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
            start_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "first", 2)
            if (is.na(start_idx))
                start_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "start", 2)
            stop_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "last", 3)
            if (is.na(stop_idx))
                stop_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "stop", 3)
        } else {
            x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
            start_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "start", 2)
            stop_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "stop", 3)
        }

        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, list(x_idx, start_idx, stop_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }
        if (length(used_idx) !=
            3) {
            return(NULL)
        }

        x_arg <- parts[[x_idx]]
        start_arg <- parts[[start_idx]]
        stop_arg <- parts[[stop_idx]]

        x_ir <- .mojor_ir_expr_build(x_arg)
        start_ir <- .mojor_ir_expr_build(start_arg)
        stop_ir <- .mojor_ir_expr_build(stop_arg)
        if (is.null(x_ir) ||
            is.null(start_ir) ||
            is.null(stop_ir)) {
            return(NULL)
        }

        return(.mojor_ir_substr(x = x_ir, start = start_ir, stop = stop_ir, src = expr))
    }

 # paste(x, y, ..., sep = ' ', collapse = NULL)
    if (op == "paste") {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )

        unsupported_named <- nms[nms != "" & !nms %in% c("sep", "collapse")]
        if (length(unsupported_named) >
            0) {
            return(NULL)
        }

        positional_idx <- which(nms == "")
        sep_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "sep")
        collapse_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "collapse")
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, c(positional_idx, sep_idx, collapse_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }
        if (length(positional_idx) <
            1) {
            return(NULL)
        }

        args <- lapply(parts[positional_idx], .mojor_ir_expr_build)
        if (any(vapply(args, is.null, logical(1)))) {
            return(NULL)
        }

        sep <- "\" \""
        if (!is.na(sep_idx)) {
            sep_expr <- parts[[sep_idx]]
            if (!(is.character(sep_expr) &&
                length(sep_expr) ==
                  1 && !is.na(sep_expr))) {
                return(NULL)
            }
            sep <- quote_string_literal(sep_expr[[1]])
        }

        collapse <- NULL
        if (!is.na(collapse_idx)) {
            collapse_expr <- parts[[collapse_idx]]
            if (is_null_expr(collapse_expr)) {
                collapse <- "NULL"
            } else if (is.character(collapse_expr) &&
                length(collapse_expr) ==
                  1 && !is.na(collapse_expr)) {
                collapse <- quote_string_literal(collapse_expr[[1]])
            } else {
                return(NULL)
            }
        }

        return(.mojor_ir_paste(args = args, sep = sep, collapse = collapse, src = expr))
    }

 # paste0(x, y, ..., collapse = NULL)
    if (op == "paste0") {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )

        unsupported_named <- nms[nms != "" & !nms %in% "collapse"]
        if (length(unsupported_named) >
            0) {
            return(NULL)
        }

        positional_idx <- which(nms == "")
        collapse_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "collapse")
        used_idx <- .mojor_ir_expr_used_arg_indices(parts, nms, c(positional_idx, collapse_idx))
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }
        if (length(positional_idx) <
            1) {
            return(NULL)
        }

        args <- lapply(parts[positional_idx], .mojor_ir_expr_build)
        if (any(vapply(args, is.null, logical(1)))) {
            return(NULL)
        }

        collapse <- NULL
        if (!is.na(collapse_idx)) {
            collapse_expr <- parts[[collapse_idx]]
            if (is_null_expr(collapse_expr)) {
                collapse <- "NULL"
            } else if (is.character(collapse_expr) &&
                length(collapse_expr) ==
                  1 && !is.na(collapse_expr)) {
                collapse <- quote_string_literal(collapse_expr[[1]])
            } else {
                return(NULL)
            }
        }

        return(.mojor_ir_paste0(args = args, collapse = collapse, src = expr))
    }

 # /9.3: Regex and table utilities (strict direct-arg subset)
    parse_tier9_direct_var <- function(arg_expr) {
        if (!is.name(arg_expr)) {
            return(NULL)
        }
        .mojor_ir_var(as.character(arg_expr))
    }
    parse_tier9_chr_scalar_or_literal <- function(arg_expr) {
        if (is.name(arg_expr)) {
            return(.mojor_ir_var(as.character(arg_expr)))
        }
        if (is.character(arg_expr) &&
            length(arg_expr) ==
                1 && !is.na(arg_expr)) {
            return(
                .mojor_ir_const(
                  as.character(arg_expr[[1]]),
                  src = arg_expr
              )
            )
        }
        NULL
    }

    if (op %in% c("grepl", "grep", "sub", "gsub")) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )
        max_parts <- if (op %in% c("grepl", "grep"))
            4L else 5L
        allowed_named <- if (op %in% c("grepl", "grep")) {
            c("pattern", "x", "X", "fixed", "perl")
        } else {
            c("pattern", "replacement", "x", "X", "fixed", "perl")
        }
        bad_named <- nms[nms != "" & !nms %in% allowed_named]
        if (length(bad_named) >
            0 || length(parts) >
            max_parts) {
            return(NULL)
        }

        pattern_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "pattern", 1)
        x_idx <- .mojor_ir_expr_get_call_arg_idx(
            parts, nms, "x", if (op %in% c("grepl", "grep"))
                2 else 3
        )
        if (is.na(x_idx)) {
            x_idx <- .mojor_ir_expr_get_call_arg_idx(
                parts, nms, "X", if (op %in% c("grepl", "grep"))
                  2 else 3
            )
        }
        replacement_idx <- if (op %in% c("sub", "gsub")) {
            .mojor_ir_expr_get_call_arg_idx(parts, nms, "replacement", 2)
        } else {
            NA_integer_
        }
        fixed_idx <- .mojor_ir_expr_get_call_arg_idx(
            parts, nms, "fixed", if (op %in% c("grepl", "grep"))
                3 else 4
        )
        perl_idx <- .mojor_ir_expr_get_call_arg_idx(
            parts, nms, "perl", if (op %in% c("grepl", "grep"))
                4 else 5
        )
        used_idx <- .mojor_ir_expr_used_arg_indices(
            parts, nms, list(pattern_idx, replacement_idx, x_idx, fixed_idx, perl_idx)
        )
        if (length(parts) !=
            length(used_idx)) {
            return(NULL)
        }
        if (is.na(pattern_idx) ||
            is.na(x_idx)) {
            return(NULL)
        }
        if (op %in% c("sub", "gsub") &&
            is.na(replacement_idx)) {
            return(NULL)
        }

        pattern_ir <- parse_tier9_chr_scalar_or_literal(parts[[pattern_idx]])
        x_ir <- parse_tier9_direct_var(parts[[x_idx]])
        if (is.null(pattern_ir) ||
            is.null(x_ir)) {
            return(NULL)
        }

        fixed <- .mojor_ir_parse_bool_scalar_or_var(
            if (!is.na(fixed_idx))
                parts[[fixed_idx]] else NULL, default = FALSE
        )
        perl <- .mojor_ir_parse_bool_scalar_or_var(
            if (!is.na(perl_idx))
                parts[[perl_idx]] else NULL, default = TRUE
        )
        if (is.null(fixed) ||
            is.null(perl)) {
            return(NULL)
        }
        if (isTRUE(fixed) &&
            isTRUE(perl)) {
            return(NULL)
        }

        if (op == "grepl") {
            return(
                .mojor_ir_regex_grepl(
                  pattern = pattern_ir, x = x_ir, ignore_case = FALSE, perl = perl, fixed = fixed,
                  src = expr
              )
            )
        }
        if (op == "grep") {
            return(
                .mojor_ir_regex_grep(
                  pattern = pattern_ir, x = x_ir, value = FALSE, ignore_case = FALSE, perl = perl,
                  fixed = fixed, src = expr
              )
            )
        }

        replacement_ir <- parse_tier9_chr_scalar_or_literal(parts[[replacement_idx]])
        if (is.null(replacement_ir)) {
            return(NULL)
        }
        return(
            .mojor_ir_regex_sub(
                pattern = pattern_ir, replacement = replacement_ir, x = x_ir, ignore_case = FALSE,
                perl = perl, fixed = fixed, global = identical(op, "gsub"),
                src = expr
            )
        )
    }

    if (op %in% c("row", "col")) {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )
        bad_named <- nms[nms != "" & !nms %in% c("x", "X")]
        if (length(bad_named) >
            0 || length(parts) !=
            1) {
            return(NULL)
        }
        x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "x", 1)
        if (is.na(x_idx))
            x_idx <- .mojor_ir_expr_get_call_arg_idx(parts, nms, "X", 1)
        if (is.na(x_idx)) {
            return(NULL)
        }
        x_ir <- parse_tier9_direct_var(parts[[x_idx]])
        if (is.null(x_ir)) {
            return(NULL)
        }
        if (op == "row") {
            return(.mojor_ir_row_matrix(x_ir, src = expr))
        }
        return(.mojor_ir_col_matrix(x_ir, src = expr))
    }

    if (op == "expand.grid") {
        parts <- as.list(expr)[-1]
        nms <- names(parts)
        nms <- if (is.null(nms))
            rep("", length(parts)) else ifelse(
            is.na(nms),
            "", nms
        )
        if (any(nms != "")) {
            return(NULL)
        }
        if (length(parts) <
            2 || length(parts) >
            4) {
            return(NULL)
        }
        arg_irs <- lapply(parts, parse_tier9_direct_var)
        if (any(vapply(arg_irs, is.null, logical(1)))) {
            return(NULL)
        }
        return(.mojor_ir_expand_grid(arg_irs, src = expr))
    }

 # Step 24: GPU Array - Reduction and Matmul gpu_reduce(x, op, dims = NULL, keepdims
 # = FALSE) gpu_sum/gpu_mean/gpu_prod/gpu_min/gpu_max/gpu_pmin/gpu_pmax/gpu_argmin/gpu_argmax
 # share the same node.
    if (op %in% c(
        "gpu_reduce", "gpu_sum", "gpu_mean", "gpu_prod", "gpu_min", "gpu_max",
        "gpu_pmin", "gpu_pmax", "gpu_argmin", "gpu_argmax"
    )) {
        default_op <- switch(
            op, gpu_sum = "sum", gpu_mean = "mean", gpu_prod = "prod", gpu_min = "min",
            gpu_max = "max", gpu_pmin = "min", gpu_pmax = "max", gpu_argmin = "argmin",
            gpu_argmax = "argmax", NULL
        )
        return(.mojor_ir_build_gpu_reduce_node(expr, default_op = default_op))
    }

 # gpu_matmul(x, y, transpose_a = FALSE, transpose_b = FALSE)
    if (op == "gpu_matmul" && length(expr) >=
        3) {
        arg1 <- .mojor_ir_expr_build(expr[[2]])
        arg2 <- .mojor_ir_expr_build(expr[[3]])
        if (is.null(arg1) ||
            is.null(arg2)) {
            return(NULL)
        }

        transpose_a <- FALSE
        transpose_b <- FALSE
        arg_names <- names(expr)
        unnamed_pos <- 0L
        if (length(expr) >=
            4L) {
            for (i in seq.int(4L, length(expr))) {
                arg_value <- expr[[i]]
                arg_name <- if (!is.null(arg_names) &&
                  length(arg_names) >=
                    i)
                  arg_names[[i]] else ""
                arg_name <- if (is.null(arg_name))
                  "" else as.character(arg_name)
                if (!is.logical(arg_value) ||
                  length(arg_value) !=
                    1)
                  next
                if (identical(arg_name, "transpose_a")) {
                  transpose_a <- isTRUE(arg_value)
                } else if (identical(arg_name, "transpose_b")) {
                  transpose_b <- isTRUE(arg_value)
                } else {
                  unnamed_pos <- unnamed_pos + 1L
                  if (unnamed_pos == 1L) {
                    transpose_a <- isTRUE(arg_value)
                  } else if (unnamed_pos == 2L) {
                    transpose_b <- isTRUE(arg_value)
                  }
                }
            }
        }

        return(.mojor_ir_gpu_matmul(arg1, arg2, transpose_a, transpose_b, src = expr))
    }

 # FFI - mojor_c_call('func_name', arg1, arg2, ...)
    if (op == "mojor_c_call" && length(expr) >=
        2) {
        fn_name <- expr[[2]]
        if (!is.character(fn_name) ||
            length(fn_name) !=
                1) {
            return(NULL)
        }

        decl <- .mojor_state$declared_c_functions[[fn_name]]
        if (is.null(decl)) {
            stop("mojor_c_call: function '", fn_name, "' not declared. Use mojor_declare_c() first.")
        }
        expected_arity <- length(decl$args)
        decl_arg_types <- unname(decl$args)
        decl_arg_names <- names(decl$args)
        actual_arity <- max(
            0L, length(expr) -
                2L
        )
        if (!identical(actual_arity, expected_arity)) {
            stop(
                "mojor_c_call: argument count mismatch for '", fn_name, "': expected ", expected_arity,
                ", got ", actual_arity
            )
        }

        args_ir <- list()
        if (length(expr) >=
            3) {
            for (k in 3:length(expr)) {
                a <- .mojor_ir_expr_build(expr[[k]])
                if (is.null(a)) {
                  return(NULL)
                }
                args_ir[[length(args_ir) +
                  1]] <- a
            }
        }

        return(
            .mojor_ir_c_call(
                fn_name, args_ir, decl$returns, decl$library, expected_arity = expected_arity,
                arg_types = decl_arg_types, arg_names = decl_arg_names
            )
        )
    }

    NULL
}
