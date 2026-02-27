# No-loop route classifier and internal canonical normalization helpers.

.mojor_transpile_route_metadata <- function(route_info) {
    if (!is.list(route_info)) {
        return(list(route = "loop", reason = "missing_route_info"))
    }
    norm <- route_info$normalization
    list(
        route = "loop",
        reason = if (!is.null(route_info$reason))
            as.character(route_info$reason) else "unspecified",
        expression_candidate = isTRUE(route_info$expression_candidate),
        normalization_enabled = isTRUE(route_info$normalization_enabled),
        normalization_applied = is.list(norm) && isTRUE(norm$applied),
        normalized_kind = if (is.list(norm) && !is.null(norm$kind))
            as.character(norm$kind) else NULL,
        unified_gpu_ops = if (!is.null(route_info$unified_gpu_ops))
            as.character(route_info$unified_gpu_ops) else character(0)
    )
}

.mojor_transpile_internal_no_loop_normalization_enabled <- function() {
    flag <- Sys.getenv("MOJOR_CANONICAL_NO_LOOP", unset = "")
    if (!nzchar(flag)) {
        return(TRUE)
    }
    tolower(flag) %in% c("1", "true", "yes", "on")
}

.mojor_collect_unified_gpu_expr_ops <- function(blocks) {
    .mojor_collect_from_blocks(
        blocks, function(expr) {
            op <- as.character(expr[[1]])
            if (!is.character(op) || length(op) == 0L) {
                return(NULL)
            }
            op <- op[[1]]
            if (op %in% c(
                "gpu_matmul", "gpu_reduce", "gpu_sum", "gpu_mean", "gpu_min",
                "gpu_max", "gpu_argmin", "gpu_argmax"
            )) {
                op
            } else {
                NULL
            }
        }
    )
}

.mojor_transpile_is_expression_candidate <- function(blocks, loops, no_loop_scalar_any_all) {
    length(loops) == 0 &&
        !isTRUE(no_loop_scalar_any_all) &&
        (length(blocks) == 1 || !is.null(.mojor_inline_let_bindings(blocks)))
}

.mojor_no_loop_vector_ctor_call <- function(out_type, len_expr) {
    if (identical(out_type, "f64[]")) {
        return(call("numeric", len_expr))
    }
    if (identical(out_type, "i32[]")) {
        return(call("integer", len_expr))
    }
    if (identical(out_type, "lgl[]")) {
        return(call("logical", len_expr))
    }
    NULL
}

.mojor_no_loop_extract_scalar_expr <- function(blocks) {
    expr <- NULL
    if (length(blocks) == 1L) {
        expr <- blocks[[1L]]
    } else {
        expr <- .mojor_inline_let_bindings(blocks)
    }
    if (!is.call(expr)) {
        return(expr)
    }
    if (as.character(expr[[1L]]) == "return" && length(expr) >= 2L) {
        return(expr[[2L]])
    }
    expr
}

.mojor_no_loop_is_scalar_arg_name <- function(name, args, types) {
    if (!is.character(name) || length(name) != 1L || !(name %in% args)) {
        return(FALSE)
    }
    spec <- types[[name]]
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
        return(FALSE)
    }
    !isTRUE(.mojor_is_array(spec)) && !identical(spec, "df")
}

.mojor_no_loop_is_supported_vector_arg <- function(name, args, types) {
    if (!is.character(name) || length(name) != 1L || !(name %in% args)) {
        return(FALSE)
    }
    spec <- types[[name]]
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
        return(FALSE)
    }
    spec %in% c("f64[]", "i32[]", "lgl[]", "f64", "f32", "i32", "lgl", "bool")
}

.mojor_no_loop_is_supported_vector_expr <- function(expr, args, types) {
    if (is.name(expr)) {
        return(.mojor_no_loop_is_supported_vector_arg(as.character(expr), args, types))
    }
    if (is.numeric(expr) || is.integer(expr) || is.logical(expr)) {
        return(length(expr) == 1L && !anyNA(expr))
    }
    if (!is.call(expr)) {
        return(FALSE)
    }
    op <- as.character(expr[[1L]])
    if (op == "(" && length(expr) == 2L) {
        return(.mojor_no_loop_is_supported_vector_expr(expr[[2L]], args, types))
    }
    if (op %in% c("+", "-", "!") && length(expr) == 2L) {
        return(.mojor_no_loop_is_supported_vector_expr(expr[[2L]], args, types))
    }
    if (op %in% c(
        "+", "-", "*", "/", "^", "%%", "%/%", "<", "<=", ">", ">=", "==", "!=",
        "&", "|", "&&", "||"
    ) && length(expr) == 3L) {
        return(
            .mojor_no_loop_is_supported_vector_expr(expr[[2L]], args, types) &&
                .mojor_no_loop_is_supported_vector_expr(expr[[3L]], args, types)
        )
    }
    FALSE
}

.mojor_no_loop_is_supported_scalar_expr <- function(expr, args, types) {
    if (is.name(expr)) {
        return(.mojor_no_loop_is_scalar_arg_name(as.character(expr), args, types))
    }
    if (is.numeric(expr) || is.logical(expr)) {
        return(length(expr) == 1L && !is.na(expr))
    }
    if (!is.call(expr) || length(expr) < 2L) {
        return(FALSE)
    }
    op <- as.character(expr[[1L]])
    if (op == "(" && length(expr) == 2L) {
        return(.mojor_no_loop_is_supported_scalar_expr(expr[[2L]], args, types))
    }
    if (op %in% c("!", "+", "-") && length(expr) == 2L) {
        return(.mojor_no_loop_is_supported_scalar_expr(expr[[2L]], args, types))
    }
    if (op %in% c("+", "-", "*", "/", "^", "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "&", "|", "&&", "||")) {
        vals <- as.list(expr)[-1L]
        if (length(vals) < 2L) {
            return(FALSE)
        }
        return(all(vapply(vals, .mojor_no_loop_is_supported_scalar_expr, logical(1), args = args, types = types)))
    }
    FALSE
}

.mojor_no_loop_scalar_return_type <- function(expr_type) {
    if (expr_type %in% c("f64", "Float64")) {
        return("Float64")
    }
    if (expr_type %in% c("f32", "Float32")) {
        return("Float32")
    }
    if (expr_type %in% c("i32", "Int32", "Int")) {
        return("Int32")
    }
    if (expr_type %in% c("lgl", "bool", "Bool")) {
        return("Bool")
    }
    NULL
}

.mojor_no_loop_scalar_init_value <- function(expr_type) {
    if (expr_type %in% c("f64", "Float64", "f32", "Float32")) {
        return(0)
    }
    if (expr_type %in% c("i32", "Int32", "Int")) {
        return(0L)
    }
    if (expr_type %in% c("lgl", "bool", "Bool")) {
        return(FALSE)
    }
    NULL
}

.mojor_no_loop_unique_name <- function(prefix, blocks, args) {
    used <- as.character(args)
    if (length(blocks) > 0) {
        used <- unique(c(used, unlist(lapply(blocks, function(b) {
            tryCatch(all.vars(b, functions = FALSE), error = function(e) character(0))
        }), use.names = FALSE)))
    }
    nm <- prefix
    i <- 0L
    while (nm %in% used) {
        i <- i + 1L
        nm <- paste0(prefix, "_", i)
    }
    nm
}

.mojor_no_loop_normalize_inferred_type <- function(spec) {
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
        return(NULL)
    }
    if (spec %in% c("bool[]", "Bool[]")) {
        return("lgl[]")
    }
    if (spec %in% c("bool", "Bool")) {
        return("lgl")
    }
    if (spec %in% c("Int", "Int32")) {
        return("i32")
    }
    if (spec %in% c("Float64")) {
        return("f64")
    }
    if (spec %in% c("Float32")) {
        return("f32")
    }
    if (spec %in% c("i32[2d]", "Int32[2d]")) {
        return("i32[,]")
    }
    if (spec %in% c("f64[2d]", "Float64[2d]")) {
        return("f64[,]")
    }
    if (spec %in% c("f32[2d]", "Float32[2d]")) {
        return("f32[,]")
    }
    if (spec %in% c("lgl[2d]", "bool[2d]", "Bool[2d]")) {
        return("lgl[,]")
    }
    spec
}

.mojor_no_loop_ir_var_name <- function(node) {
    if (!is.list(node) || !identical(node$kind, "var") || is.null(node$name)) {
        return("")
    }
    nm <- as.character(node$name)
    if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
        return("")
    }
    nm
}

.mojor_no_loop_hof_fun_returns_constructor <- function(fun_expr) {
    if (!(is.call(fun_expr) && identical(as.character(fun_expr[[1L]]), "function"))) {
        return(FALSE)
    }
    blocks <- tryCatch(.mojor_extract_block(fun_expr[[3L]]), error = function(e) NULL)
    if (!is.list(blocks) || length(blocks) == 0L) {
        return(FALSE)
    }
    body_expr <- if (length(blocks) == 1L) {
        blocks[[1L]]
    } else {
        .mojor_inline_let_bindings(blocks)
    }
    if (is.null(body_expr)) {
        return(FALSE)
    }
    if (is.call(body_expr) && identical(as.character(body_expr[[1L]]), "return") && length(body_expr) >= 2L) {
        body_expr <- body_expr[[2L]]
    }
    body_ir <- tryCatch(.mojor_ir_expr_build(body_expr), error = function(e) NULL)
    is.list(body_ir) && identical(body_ir$kind, "c")
}

.mojor_no_loop_dim_source_name <- function(expr, args, types) {
    if (!is.call(expr) || as.character(expr[[1L]]) != "dim" || length(expr) != 2L || !is.name(expr[[2L]])) {
        return(NULL)
    }
    nm <- as.character(expr[[2L]])
    if (!(nm %in% args)) {
        return(NULL)
    }
    spec <- types[[nm]]
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec) || !isTRUE(.mojor_is_array(spec))) {
        return(NULL)
    }
    nm
}

.mojor_no_loop_dim_rank <- function(spec) {
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
        return(NA_integer_)
    }
    if (isTRUE(.mojor_is_matrix(spec))) {
        return(2L)
    }
    rk <- .mojor_type_ndim(spec)
    if (is.numeric(rk) && length(rk) == 1L && is.finite(rk) && rk >= 2L) {
        return(as.integer(rk))
    }
    NA_integer_
}

.mojor_no_loop_dim_component_expr <- function(arr_name, spec, idx) {
    if (!is.character(arr_name) || length(arr_name) != 1L || !nzchar(arr_name)) {
        return(NULL)
    }
    if (!is.numeric(idx) || length(idx) != 1L || !is.finite(idx)) {
        return(NULL)
    }
    idx <- as.integer(idx)
    if (idx < 1L) {
        return(NULL)
    }
    if (isTRUE(.mojor_is_matrix(spec))) {
        if (idx > 2L) {
            return(NULL)
        }
        return(call("[", call("dim", as.name(arr_name)), as.integer(idx)))
    }
    rk <- .mojor_no_loop_dim_rank(spec)
    if (is.na(rk) || idx > rk) {
        return(NULL)
    }
    call("[", call("dim", as.name(arr_name)), as.integer(idx))
}

.mojor_no_loop_rewrite_dim_call <- function(call_expr, args, types) {
    out <- list(applied = FALSE, expr = NULL, out_type = NULL, is_vector = FALSE)
    if (!is.call(call_expr)) {
        return(out)
    }
    op <- as.character(call_expr[[1L]])

    if (identical(op, "dim")) {
        src <- .mojor_no_loop_dim_source_name(call_expr, args, types)
        if (is.null(src)) {
            return(out)
        }
        spec <- types[[src]]
        rk <- .mojor_no_loop_dim_rank(spec)
        if (is.na(rk) || rk < 2L) {
            return(out)
        }
        parts <- vector("list", rk)
        for (i in seq_len(rk)) {
            comp <- .mojor_no_loop_dim_component_expr(src, spec, i)
            if (is.null(comp)) {
                return(out)
            }
            parts[[i]] <- comp
        }
        out$applied <- TRUE
        out$expr <- as.call(c(as.name("c"), parts))
        out$out_type <- "i32[]"
        out$is_vector <- TRUE
        return(out)
    }

    if (identical(op, "length") && length(call_expr) == 2L) {
        src <- .mojor_no_loop_dim_source_name(call_expr[[2L]], args, types)
        if (is.null(src)) {
            return(out)
        }
        spec <- types[[src]]
        rk <- .mojor_no_loop_dim_rank(spec)
        if (is.na(rk) || rk < 2L) {
            return(out)
        }
        out$applied <- TRUE
        out$expr <- as.integer(rk)
        out$out_type <- "i32"
        out$is_vector <- FALSE
        return(out)
    }

    if (op %in% c("[", "[[") && length(call_expr) == 3L) {
        base <- call_expr[[2L]]
        idx <- call_expr[[3L]]
        src <- .mojor_no_loop_dim_source_name(base, args, types)
        if (is.null(src)) {
            return(out)
        }
        spec <- types[[src]]
        rk <- .mojor_no_loop_dim_rank(spec)
        if (is.na(rk) || rk < 2L) {
            return(out)
        }
        idx_lit <- NA_integer_
        if (is.numeric(idx) || is.integer(idx)) {
            idx_lit <- suppressWarnings(as.integer(idx))
        }
        if ((is.numeric(idx) || is.integer(idx)) && length(idx) == 1L && !is.na(idx_lit)) {
            comp <- .mojor_no_loop_dim_component_expr(src, spec, idx_lit)
            if (is.null(comp)) {
                return(out)
            }
            out$applied <- TRUE
            out$expr <- comp
            out$out_type <- "i32"
            out$is_vector <- FALSE
            return(out)
        }
        # Dynamic dim index lowered to nested ifelse over fixed-rank components.
        expr <- 0L
        for (i in rk:1L) {
            comp <- .mojor_no_loop_dim_component_expr(src, spec, i)
            if (is.null(comp)) {
                return(out)
            }
            expr <- call("ifelse", call("==", idx, as.integer(i)), comp, expr)
        }
        out$applied <- TRUE
        out$expr <- expr
        out$out_type <- "i32"
        out$is_vector <- FALSE
        return(out)
    }

    out
}

.mojor_no_loop_is_supported_ir_whole_vector_call <- function(ir_node, kind) {
    if (!is.list(ir_node) || !is.character(kind) || length(kind) != 1L || !nzchar(kind)) {
        return(FALSE)
    }
    x_name <- .mojor_no_loop_ir_var_name(ir_node$x)
    if (kind %in% c(
        "unique", "duplicated", "vapply", "sapply", "lapply",
        "nchar", "nzchar", "substr", "regex_grepl", "regex_grep", "regex_sub",
        "row_matrix", "col_matrix"
    )) {
        if (!nzchar(x_name)) {
            return(FALSE)
        }
        return(TRUE)
    }
    if (kind == "paste") {
        if (is.null(ir_node$args) || length(ir_node$args) == 0L) {
            return(FALSE)
        }
        arg_names <- vapply(ir_node$args, .mojor_no_loop_ir_var_name, character(1))
        return(all(nzchar(arg_names)))
    }
    if (kind %in% c("match", "in")) {
        return(nzchar(x_name) && nzchar(.mojor_no_loop_ir_var_name(ir_node$table)))
    }
    if (kind == "expand_grid") {
        if (is.null(ir_node$args) || length(ir_node$args) == 0L) {
            return(FALSE)
        }
        arg_names <- vapply(ir_node$args, .mojor_no_loop_ir_var_name, character(1))
        return(all(nzchar(arg_names)))
    }
    if (kind == "quantile") {
        return(nzchar(x_name) && !is.null(ir_node$probs))
    }
    if (kind == "mapply") {
        if (is.null(ir_node$args) || length(ir_node$args) < 2L) {
            return(FALSE)
        }
        arg_names <- vapply(ir_node$args, .mojor_no_loop_ir_var_name, character(1))
        return(all(nzchar(arg_names)))
    }
    if (kind == "sample_int") {
        return(!is.null(ir_node$n) && !is.null(ir_node$size))
    }
    if (kind == "sample") {
        return(!is.null(ir_node$x) && !is.null(ir_node$size))
    }
    FALSE
}

.mojor_try_normalize_no_loop <- function(blocks, args, types, name = "mojor_kernel") {
    out <- list(
        applied = FALSE,
        kind = NULL,
        reason = "no_supported_normalization",
        blocks = blocks,
        expression_compat = NULL
    )
    .mark_prebuilt <- function(trans_out, tag) {
        tag <- gsub("[^A-Za-z0-9_]", "_", as.character(tag))
        out$applied <<- TRUE
        out$kind <<- paste0("shared_prebuilt_", tag)
        out$reason <<- paste0("normalized_shared_prebuilt_", tag)
        out$blocks <<- blocks
        compat <- list(
            prebuilt_trans = trans_out,
            is_expression_kernel = isTRUE(trans_out$is_expression_kernel),
            return_type = trans_out$return_type,
            out_type = trans_out$out_type,
            is_vector_output = isTRUE(trans_out$is_vector_output)
        )
        passthrough_fields <- c(
            "kernel_args", "literal_array_args", "vector_len_const",
            "dim_builtin", "dim_builtin_mode", "dim_x_name", "dim_expected_ndim",
            "dim_index_kind", "dim_index_value", "dim_index_name",
            "dim_index_c_expr", "dim_index_arg_names",
            "preview_rewrite_fallback", "interpreted_fallback", "na_skip_args"
        )
        for (nm in passthrough_fields) {
            if (!is.null(trans_out[[nm]])) {
                compat[[nm]] <- trans_out[[nm]]
            }
        }
        out$expression_compat <<- compat
    }

    info <- .mojor_detect_vectorized_expression_operation(blocks, args, types)
    if (isTRUE(info$is_vectorized_expression)) {
        expr_payload <- tryCatch(
            .mojor_transpile_vectorized_expression_operation(info, args, types, name = name),
            error = function(e) e
        )
        if (inherits(expr_payload, "error")) {
            out$reason <- paste0("vectorized_expr_payload_failed: ", conditionMessage(expr_payload))
            return(out)
        }
        out_type <- expr_payload$out_type
        if (!is.character(out_type) || length(out_type) != 1L || !nzchar(out_type)) {
            out$reason <- "vectorized_expr_missing_out_type"
            return(out)
        }
        len_arg <- if (length(info$arg_names) > 0L)
            as.character(info$arg_names[[1L]]) else NULL
        if (!is.character(len_arg) || length(len_arg) != 1L || !nzchar(len_arg)) {
            out$reason <- "vectorized_expr_missing_len_arg"
            return(out)
        }
        ctor_call <- .mojor_no_loop_vector_ctor_call(out_type, call("length", as.name(len_arg)))
        if (is.null(ctor_call)) {
            out$reason <- paste0("unsupported_normalized_out_type: ", out_type)
            return(out)
        }

        out_var <- .mojor_no_loop_unique_name("__mojor_norm_out", blocks, args)
        loop_var <- .mojor_no_loop_unique_name("__mojor_norm_i", blocks, c(args, out_var))
        elem_expr <- .mojor_vector_expr_to_element(info$expr, loop_var, types, args)
        elem_expr <- .mojor_simplify_selector_index_expr(elem_expr)

        init_stmt <- call("<-", as.name(out_var), ctor_call)
        body_stmt <- call("<-", call("[", as.name(out_var), as.name(loop_var)), elem_expr)
        loop_stmt <- call("for", as.name(loop_var), call("seq_along", as.name(len_arg)), body_stmt)
        out$applied <- TRUE
        out$kind <- "vectorized_expression_loop"
        out$reason <- "normalized_vectorized_expression"
        out$blocks <- list(init_stmt, loop_stmt, as.name(out_var))
        out$expression_compat <- list(
            prebuilt_trans = expr_payload,
            is_expression_kernel = TRUE,
            return_type = if (!is.null(expr_payload$return_type))
                expr_payload$return_type else "Int32",
            out_type = out_type,
            is_vector_output = isTRUE(expr_payload$is_vector_output)
        )
        return(out)
    }

    call_expr <- .mojor_no_loop_extract_scalar_expr(blocks)
    dim_info <- .mojor_detect_dim_operation(blocks, args, types)
    dim_prebuilt <- NULL
    if (is.list(dim_info) && isTRUE(dim_info$is_dim)) {
        dim_prebuilt <- tryCatch(
            .mojor_transpile_dim_operation(
                dim_info = dim_info,
                args = args,
                types = types,
                name = name
            ),
            error = function(e) NULL
        )
    }

    dim_rewrite <- .mojor_no_loop_rewrite_dim_call(call_expr, args, types)
    if (isTRUE(dim_rewrite$applied) && !is.null(dim_rewrite$expr)) {
        if (isTRUE(dim_rewrite$is_vector)) {
            out_var <- .mojor_no_loop_unique_name("__mojor_norm_dim_out", blocks, args)
            dim_expr <- dim_rewrite$expr
            dim_init <- NULL
            dim_writes <- list()
            if (is.call(dim_expr) && identical(as.character(dim_expr[[1L]]), "c")) {
                dim_parts <- as.list(dim_expr)[-1L]
                dim_init <- call("<-", as.name(out_var), call("integer", as.integer(length(dim_parts))))
                if (length(dim_parts) > 0L) {
                    dim_writes <- lapply(seq_along(dim_parts), function(i) {
                        call("<-", call("[", as.name(out_var), as.integer(i)), dim_parts[[i]])
                    })
                }
            }
            if (is.null(dim_init)) {
                dim_init <- call("<-", as.name(out_var), dim_expr)
            }
            out$applied <- TRUE
            out$kind <- "dim_metadata_vector_return"
            out$reason <- "normalized_dim_metadata_vector_return"
            out$blocks <- c(list(dim_init), dim_writes, list(as.name(out_var)))
            out$expression_compat <- list(
                prebuilt_trans = dim_prebuilt,
                is_expression_kernel = TRUE,
                return_type = if (!is.null(dim_prebuilt$return_type))
                    dim_prebuilt$return_type else "Int32",
                out_type = dim_rewrite$out_type,
                is_vector_output = TRUE,
                dim_builtin = TRUE,
                dim_builtin_mode = "vector",
                vector_len_const = if (!is.null(dim_prebuilt$vector_len_const))
                    dim_prebuilt$vector_len_const else as.integer(length(as.list(dim_rewrite$expr)[-1L])),
                dim_x_name = if (!is.null(dim_prebuilt$dim_x_name))
                    dim_prebuilt$dim_x_name else if (is.list(dim_info) && isTRUE(dim_info$is_dim))
                        as.character(dim_info$x_expr) else NULL,
                dim_expected_ndim = if (!is.null(dim_prebuilt$dim_expected_ndim))
                    dim_prebuilt$dim_expected_ndim else NULL
            )
            return(out)
        }
        scalar_var <- .mojor_no_loop_unique_name("__mojor_norm_dim_scalar", blocks, args)
        out$applied <- TRUE
        out$kind <- "dim_metadata_scalar_return"
        out$reason <- "normalized_dim_metadata_scalar_return"
        out$blocks <- list(
            call("<-", as.name(scalar_var), 0L),
            call("<-", as.name(scalar_var), dim_rewrite$expr),
            as.name(scalar_var)
        )
        compat <- list(
            prebuilt_trans = dim_prebuilt,
            is_expression_kernel = TRUE,
            return_type = if (!is.null(dim_prebuilt$return_type))
                dim_prebuilt$return_type else "Int32",
            out_type = if (!is.null(dim_prebuilt$out_type))
                dim_prebuilt$out_type else "i32",
            dim_builtin = TRUE,
            dim_x_name = if (!is.null(dim_prebuilt$dim_x_name))
                dim_prebuilt$dim_x_name else if (is.list(dim_info) && isTRUE(dim_info$is_dim))
                    as.character(dim_info$x_expr) else NULL,
            dim_expected_ndim = if (!is.null(dim_prebuilt$dim_expected_ndim))
                dim_prebuilt$dim_expected_ndim else NULL
        )
        if (!is.null(dim_prebuilt$dim_builtin_mode)) {
            compat$dim_builtin_mode <- dim_prebuilt$dim_builtin_mode
        } else if (is.list(dim_info) && isTRUE(dim_info$is_dim)) {
            compat$dim_builtin_mode <- switch(
                as.character(dim_info$operation),
                dim_ndim = "ndim",
                "index"
            )
        } else {
            compat$dim_builtin_mode <- "index"
        }
        for (nm in c(
            "dim_index_kind", "dim_index_value", "dim_index_name",
            "dim_index_c_expr", "dim_index_arg_names"
        )) {
            if (!is.null(dim_prebuilt[[nm]])) {
                compat[[nm]] <- dim_prebuilt[[nm]]
            }
        }
        out$expression_compat <- compat
        return(out)
    }

    string_info <- .mojor_detect_string_operation(blocks)
    if (is.list(string_info) && isTRUE(string_info$is_string)) {
        string_out <- .mojor_transpile_string_operation(
            string_info = string_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(string_out, paste0("string_", string_info$operation))
        return(out)
    }

    hof_info <- .mojor_detect_hof_operation(blocks)
    if (is.list(hof_info) && isTRUE(hof_info$is_hof)) {
        hof_out <- .mojor_transpile_hof_operation(
            hof_info = hof_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(hof_out, paste0("hof_", hof_info$operation))
        return(out)
    }

    matrix_info <- .mojor_detect_matrix_operation(blocks)
    if (is.list(matrix_info) && isTRUE(matrix_info$is_matmul)) {
        matrix_out <- .mojor_transpile_matrix_operation(
            matmul_info = matrix_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(matrix_out, paste0("matrix_", matrix_info$operation))
        return(out)
    }

    cov_cor_info <- .mojor_detect_cov_cor_operation(blocks)
    if (is.list(cov_cor_info) && isTRUE(cov_cor_info$is_cov_cor)) {
        cov_cor_out <- .mojor_transpile_cov_cor_operation(
            cov_cor_info = cov_cor_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(cov_cor_out, paste0("cov_cor_", cov_cor_info$operation))
        return(out)
    }

    sampling_info <- .mojor_detect_sampling_operation(blocks)
    if (is.list(sampling_info) && isTRUE(sampling_info$is_sampling)) {
        sampling_out <- .mojor_transpile_sampling_operation(
            sampling_info = sampling_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(sampling_out, paste0("sampling_", sampling_info$operation))
        return(out)
    }

    if (is.list(dim_info) && isTRUE(dim_info$is_dim)) {
        dim_out <- .mojor_transpile_dim_operation(
            dim_info = dim_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(dim_out, paste0("dim_", dim_info$operation))
        return(out)
    }

    tier9_info <- .mojor_detect_tier9_regex_table_operation(blocks)
    if (is.list(tier9_info) && isTRUE(tier9_info$is_tier9_regex_table)) {
        tier9_out <- .mojor_transpile_tier9_regex_table_operation(
            tier9_info = tier9_info,
            args = args,
            types = types,
            name = name
        )
        .mark_prebuilt(tier9_out, paste0("tier9_regex_table_", tier9_info$operation))
        return(out)
    }

    if (is.call(call_expr)) {
        call_kind <- NULL
        call_type <- NULL
        call_op <- as.character(call_expr[[1L]])
        if (is.character(call_op) && length(call_op) == 1L && nzchar(call_op) && identical(call_op, "quantile")) {
            call_kind <- "quantile"
            call_type <- "f64[]"
        }
        call_ir <- tryCatch(.mojor_ir_expr_build(call_expr), error = function(e) e)
        if (inherits(call_ir, "error")) {
            if (is.character(call_op) &&
                length(call_op) == 1L &&
                call_op %in% c("sample", "sample.int")) {
                stop(call_ir)
            }
        }
        if (!inherits(call_ir, "error") && is.list(call_ir) && !is.null(call_ir$kind)) {
            call_kind <- as.character(call_ir$kind)
            if (length(call_kind) != 1L || !nzchar(call_kind)) {
                call_kind <- NULL
            }
            call_type <- tryCatch(.mojor_ir_infer_type(call_ir, types), error = function(e) e)
            if (!inherits(call_type, "error")) {
                call_type <- .mojor_no_loop_normalize_inferred_type(call_type)
            }
        }
        if (!is.null(call_kind) &&
            identical(call_kind, "quantile") &&
            (!is.character(call_type) || length(call_type) != 1L || !nzchar(call_type) || identical(call_type, "unknown"))) {
            call_type <- "f64[]"
        }
        vector_types <- c(
            "f64[]", "f32[]", "i32[]", "lgl[]", "chr[]",
            "f64[,]", "f32[,]", "i32[,]", "lgl[,]", "df"
        )
        scalar_types <- c("f64", "f32", "i32", "lgl")

        supports_vector_call <- isTRUE(
            .mojor_no_loop_is_supported_ir_whole_vector_call(call_ir, call_kind)
        )
        if (!supports_vector_call && identical(call_kind, "quantile")) {
            supports_vector_call <- TRUE
        }
        if (!is.null(call_kind) &&
            is.character(call_type) &&
            length(call_type) == 1L &&
            call_type %in% vector_types &&
            isTRUE(supports_vector_call)) {
            normalized_call_expr <- call_expr
            prefix_stmts <- list()
            if (identical(call_kind, "quantile") &&
                is.call(call_expr) &&
                length(call_expr) >= 3L) {
                quantile_parts <- as.list(call_expr)[-1L]
                quantile_names <- names(quantile_parts)
                probs_idx <- .mojor_ir_expr_get_call_arg_idx(quantile_parts, quantile_names, "probs", 2)
                if (!is.na(probs_idx)) {
                    probs_expr <- quantile_parts[[probs_idx]]
                    if (!is.name(probs_expr)) {
                        if (is.call(probs_expr)) {
                            vars <- tryCatch(all.vars(probs_expr, functions = FALSE), error = function(e) character(0))
                            if (length(vars) == 0L) {
                                lit_vals <- tryCatch(eval(probs_expr, envir = baseenv()), error = function(e) NULL)
                                if (is.numeric(lit_vals) && length(lit_vals) > 0L && all(is.finite(lit_vals))) {
                                    probs_expr <- as.call(c(as.name("c"), as.list(as.numeric(lit_vals))))
                                }
                            }
                        }
                        probs_tmp <- .mojor_no_loop_unique_name("__mojor_norm_probs", blocks, args)
                        prefix_stmts <- c(prefix_stmts, list(call("<-", as.name(probs_tmp), probs_expr)))
                        quantile_parts[[probs_idx]] <- as.name(probs_tmp)
                        normalized_call_expr <- as.call(c(call_expr[[1L]], quantile_parts))
                    }
                }
            }
            out_var <- .mojor_no_loop_unique_name("__mojor_norm_call_out", blocks, args)
            assign_stmt <- call("<-", as.name(out_var), normalized_call_expr)
            out$applied <- TRUE
            out$kind <- paste0("ir_whole_vector_call_", call_kind)
            out$reason <- paste0("normalized_ir_whole_vector_call_", call_kind)
            out$blocks <- c(prefix_stmts, list(assign_stmt, as.name(out_var)))
            compat <- list(
                is_expression_kernel = TRUE,
                return_type = "Int32",
                out_type = call_type,
                is_vector_output = !identical(call_type, "df")
            )
            prebuilt_call <- NULL
            prebuilt_call_err <- NULL
            if (call_kind %in% c("unique", "duplicated", "anyDuplicated", "any_duplicated", "match", "in")) {
                set_match_info <- .mojor_detect_set_match_operation(blocks)
                if (is.list(set_match_info) && isTRUE(set_match_info$is_set_match)) {
                    prebuilt_call <- tryCatch(
                        .mojor_transpile_set_match_operation(set_match_info, args, types, name),
                        error = function(e) {
                            prebuilt_call_err <<- e
                            NULL
                        }
                    )
                }
            } else if (call_kind %in% c("quantile", "median", "IQR", "mad")) {
                quantile_info <- .mojor_detect_quantile_operation(blocks)
                if (is.list(quantile_info) && isTRUE(quantile_info$is_quantile)) {
                    prebuilt_call <- tryCatch(
                        .mojor_transpile_quantile_operation(quantile_info, args, types, name),
                        error = function(e) {
                            prebuilt_call_err <<- e
                            NULL
                        }
                    )
                }
            }
            if (inherits(prebuilt_call_err, "error")) {
                stop(prebuilt_call_err)
            }
            if (is.list(prebuilt_call)) {
                compat$prebuilt_trans <- prebuilt_call
                passthrough_fields <- c(
                    "kernel_args", "literal_array_args", "vector_len_const",
                    "preview_rewrite_fallback", "interpreted_fallback", "na_skip_args"
                )
                for (nm in passthrough_fields) {
                    if (!is.null(prebuilt_call[[nm]])) {
                        compat[[nm]] <- prebuilt_call[[nm]]
                    }
                }
            }
            out$expression_compat <- compat
            return(out)
        }

        non_scalar_index_kind <- isTRUE(!is.null(call_kind) && call_kind %in% c("index", "subscript"))
        if (!is.null(call_kind) &&
            is.character(call_type) &&
            length(call_type) == 1L &&
            call_type %in% scalar_types &&
            !isTRUE(non_scalar_index_kind)) {
            scalar_return_type <- .mojor_no_loop_scalar_return_type(call_type)
            scalar_init <- .mojor_no_loop_scalar_init_value(call_type)
            if (is.character(scalar_return_type) && !is.null(scalar_init)) {
                scalar_var <- .mojor_no_loop_unique_name("__mojor_norm_call_scalar", blocks, args)
                init_stmt <- call("<-", as.name(scalar_var), scalar_init)
                assign_stmt <- call("<-", as.name(scalar_var), call_expr)
                out$applied <- TRUE
                out$kind <- paste0("ir_scalar_call_", call_kind)
                out$reason <- paste0("normalized_ir_scalar_call_", call_kind)
                out$blocks <- list(init_stmt, assign_stmt, as.name(scalar_var))
                compat <- list(
                    is_expression_kernel = TRUE,
                    return_type = scalar_return_type
                )
                prebuilt_scalar <- NULL
                prebuilt_scalar_err <- NULL
                if (call_kind %in% c("anyDuplicated", "any_duplicated")) {
                    set_match_info <- .mojor_detect_set_match_operation(blocks)
                    if (is.list(set_match_info) && isTRUE(set_match_info$is_set_match)) {
                        prebuilt_scalar <- tryCatch(
                            .mojor_transpile_set_match_operation(set_match_info, args, types, name),
                            error = function(e) {
                                prebuilt_scalar_err <<- e
                                NULL
                            }
                        )
                    }
                } else if (call_kind %in% c("median", "IQR", "mad")) {
                    quantile_info <- .mojor_detect_quantile_operation(blocks)
                    if (is.list(quantile_info) && isTRUE(quantile_info$is_quantile)) {
                        prebuilt_scalar <- tryCatch(
                            .mojor_transpile_quantile_operation(quantile_info, args, types, name),
                            error = function(e) {
                                prebuilt_scalar_err <<- e
                                NULL
                            }
                        )
                    }
                }
                if (inherits(prebuilt_scalar_err, "error")) {
                    stop(prebuilt_scalar_err)
                }
                if (is.list(prebuilt_scalar)) {
                    compat$prebuilt_trans <- prebuilt_scalar
                    passthrough_fields <- c(
                        "kernel_args", "literal_array_args", "vector_len_const",
                        "preview_rewrite_fallback", "interpreted_fallback", "na_skip_args"
                    )
                    for (nm in passthrough_fields) {
                        if (!is.null(prebuilt_scalar[[nm]])) {
                            compat[[nm]] <- prebuilt_scalar[[nm]]
                        }
                    }
                }
                out$expression_compat <- compat
                return(out)
            }
        }
    }

    scalar_expr <- .mojor_no_loop_extract_scalar_expr(blocks)
    if (is.null(scalar_expr) ||
        !isTRUE(.mojor_no_loop_is_supported_scalar_expr(scalar_expr, args, types))) {
        return(out)
    }

    scalar_ir <- tryCatch(
        .mojor_ir_expr_build(scalar_expr),
        error = function(e) e
    )
    if (inherits(scalar_ir, "error")) {
        out$reason <- paste0("scalar_expr_ir_build_failed: ", conditionMessage(scalar_ir))
        return(out)
    }
    scalar_type <- tryCatch(
        .mojor_ir_infer_type(scalar_ir, types),
        error = function(e) e
    )
    if (inherits(scalar_type, "error") ||
        !is.character(scalar_type) ||
        length(scalar_type) != 1L) {
        out$reason <- "scalar_expr_type_inference_failed"
        return(out)
    }
    scalar_return_type <- .mojor_no_loop_scalar_return_type(scalar_type)
    scalar_init <- .mojor_no_loop_scalar_init_value(scalar_type)
    if (!is.character(scalar_return_type) || is.null(scalar_init)) {
        out$reason <- paste0("unsupported_scalar_expr_type: ", scalar_type)
        return(out)
    }
    scalar_var <- .mojor_no_loop_unique_name("__mojor_norm_scalar", blocks, args)
    init_stmt <- call("<-", as.name(scalar_var), scalar_init)
    assign_stmt <- call("<-", as.name(scalar_var), scalar_expr)
    out$applied <- TRUE
    out$kind <- "scalar_expression_return"
    out$reason <- "normalized_scalar_expression_return"
    out$blocks <- list(init_stmt, assign_stmt, as.name(scalar_var))
    out$expression_compat <- list(
        is_expression_kernel = TRUE,
        return_type = scalar_return_type
    )
    out
}

.mojor_classify_transpile_route <- function(
    blocks, loops, no_loop_scalar_any_all, gpu_jit_mode, args, types,
    name = "mojor_kernel", normalization_enabled = .mojor_transpile_internal_no_loop_normalization_enabled()
) {
    expression_candidate <- .mojor_transpile_is_expression_candidate(
        blocks = blocks,
        loops = loops,
        no_loop_scalar_any_all = no_loop_scalar_any_all
    )
    out <- list(
        route = "loop",
        reason = "loop_path_default",
        expression_candidate = expression_candidate,
        normalization_enabled = isTRUE(normalization_enabled),
        normalization = list(applied = FALSE),
        unified_gpu_ops = character(0),
        normalized_blocks = NULL,
        expression_compat = NULL
    )

    if (!expression_candidate) {
        out$reason <- if (length(loops) > 0L) {
            "loops_present"
        } else if (isTRUE(no_loop_scalar_any_all)) {
            "no_loop_any_all_synthesis"
        } else {
            "non_expression_body"
        }
        return(out)
    }

    out$unified_gpu_ops <- .mojor_collect_unified_gpu_expr_ops(blocks)
    if (length(out$unified_gpu_ops) > 0L) {
        out$reason <- "unified_gpu_expr_route"
        return(out)
    }

    if (isTRUE(normalization_enabled)) {
        norm <- .mojor_try_normalize_no_loop(blocks = blocks, args = args, types = types, name = name)
        out$normalization <- norm
        if (isTRUE(norm$applied) && is.list(norm$blocks) && length(norm$blocks) > 0L) {
            out$reason <- if (!is.null(norm$reason))
                as.character(norm$reason) else "normalized_no_loop"
            out$normalized_blocks <- norm$blocks
            out$expression_compat <- norm$expression_compat
            return(out)
        }
    }

    out$reason <- "no_loop_loop_ir_fallback"
    out
}
