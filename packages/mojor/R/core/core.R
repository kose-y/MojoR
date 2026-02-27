# Minimal R-side API for the MojoR prototype.

.mojor_type_registry <- list(
    f64 = list(
        check = function(x) is.numeric(x) &&
            length(x) ==
                1, coerce = as.double
    ),
    `f64[]` = list(
        check = function(x) is.numeric(x) &&
            is.vector(x),
        coerce = as.double
    ),
    i32 = list(
        check = function(x) is.integer(x) &&
            length(x) ==
                1, coerce = as.integer
    ),
    `i32[]` = list(
        check = function(x) is.integer(x) &&
            is.vector(x),
        coerce = as.integer
    ),
    chr = list(
        check = function(x) is.character(x) &&
            length(x) ==
                1, coerce = as.character
    ),
    `chr[]` = list(
        check = function(x) is.character(x) &&
            is.vector(x),
        coerce = as.character
    ),
    df = list(
        check = function(x) is.data.frame(x),
        coerce = identity
    ),
    bool = list(
        check = function(x) is.logical(x) &&
            length(x) ==
                1, coerce = as.logical
    ),
    `bool[]` = list(
        check = function(x) is.logical(x) &&
            is.vector(x),
        coerce = as.logical
    )
)

# is default-enabled after closeout. Keep explicit
# controls for: - forced enable: options(mojor._force = TRUE) -
# strict gate testing / temporary disable:
# options(mojor._complete = FALSE)
.mojor_tier9_ready <- function() {
    if (isTRUE(getOption("mojor.tier9_force", FALSE))) {
        return(TRUE)
    }
    isTRUE(getOption("mojor.tier8_complete", TRUE))
}

.mojor_require_tier9_ready <- function(feature = "compiled subset feature") {
    if (.mojor_tier9_ready()) {
        return(invisible(TRUE))
    }
    stop(
        feature, " is currently gated by configuration. ", "Set options(mojor.tier8_complete = TRUE) to enable compiled subset ",
        "(default after compiled subset closeout) or options(mojor.tier9_force = TRUE) ",
        "for explicit overrides."
    )
}

# Shared RNG catalog (single source of truth for default RNG
# distribution metadata).
.mojor_rng_catalog_metadata <- function() {
    list(
        runif = list(
            min_params = 0L, max_params = 2L, helper_symbols = c("_rng_next_f64"),
            needs_tables = FALSE
        ),
        rnorm = list(
            min_params = 0L, max_params = 2L, helper_symbols = c("_random_standard_normal"),
            needs_tables = TRUE
        ),
        rgamma = list(
            min_params = 1L, max_params = 2L, helper_symbols = c("_random_standard_gamma"),
            needs_tables = TRUE
        ),
        rbinom = list(
            min_params = 2L, max_params = 2L, helper_symbols = c("_random_binomial"),
            needs_tables = FALSE
        ),
        rexp = list(
            min_params = 0L, max_params = 1L, helper_symbols = c("_rng_next_f64"),
            needs_tables = FALSE
        ),
        rpois = list(
            min_params = 1L, max_params = 1L, helper_symbols = c("_random_poisson"),
            needs_tables = TRUE
        ),
        rlnorm = list(
            min_params = 0L, max_params = 2L, helper_symbols = c("_random_standard_normal"),
            needs_tables = TRUE
        ),
        rchisq = list(
            min_params = 1L, max_params = 1L, helper_symbols = c("_random_chisq"),
            needs_tables = TRUE
        ),
        rt = list(
            min_params = 1L, max_params = 1L, helper_symbols = c("_random_standard_normal", "_random_chisq"),
            needs_tables = TRUE
        ),
        rf = list(
            min_params = 2L, max_params = 2L, helper_symbols = c("_random_chisq"),
            needs_tables = TRUE
        ),
        rbeta = list(
            min_params = 2L, max_params = 2L, helper_symbols = c("_random_beta"),
            needs_tables = TRUE
        ),
        rweibull = list(
            min_params = 1L, max_params = 2L, helper_symbols = c("_random_weibull"),
            needs_tables = FALSE
        ),
        rlogis = list(
            min_params = 0L, max_params = 2L, helper_symbols = c("_random_logistic"),
            needs_tables = FALSE
        ),
        rcauchy = list(
            min_params = 0L, max_params = 2L, helper_symbols = c("_random_cauchy"),
            needs_tables = FALSE
        ),
        rgeom = list(
            min_params = 1L, max_params = 1L, helper_symbols = c("_random_geometric"),
            needs_tables = FALSE
        ),
        rnbinom = list(
            min_params = 2L, max_params = 2L, helper_symbols = c("_random_standard_gamma", "_random_poisson"),
            needs_tables = TRUE
        ),
        rhyper = list(
            min_params = 3L, max_params = 3L, helper_symbols = c("_random_hypergeometric"),
            needs_tables = FALSE
        ),
        rsignrank = list(
            min_params = 1L, max_params = 1L, helper_symbols = c("_random_signrank"),
            needs_tables = FALSE
        ),
        rwilcox = list(
            min_params = 2L, max_params = 2L, helper_symbols = c("_random_wilcox"),
            needs_tables = FALSE
        )
    )
}

.mojor_rng_catalog_call_fns <- function() {
    names(.mojor_rng_catalog_metadata())
}

.mojor_rng_catalog_table_fns <- function() {
    meta <- .mojor_rng_catalog_metadata()
    names(meta)[vapply(
        meta, function(x) isTRUE(x$needs_tables),
        logical(1)
    )]
}

.mojor_rng_catalog_helper_symbol_map <- function() {
    meta <- .mojor_rng_catalog_metadata()
    lapply(meta, function(x) unique(as.character(x$helper_symbols)))
}

.mojor_rng_catalog_helper_symbols <- function() {
    unique(unlist(.mojor_rng_catalog_helper_symbol_map(), use.names = FALSE))
}

.mojor_parse_signature <- function(signature) {
    if (!is.character(signature) ||
        length(signature) !=
            1 || !nzchar(signature)) {
        stop("mojor_fn: signature must be a non-empty string")
    }
    sig <- trimws(signature)
    sig <- sub("^fn\\s+", "", sig)
    ret_type <- NULL
    if (grepl("->", sig, fixed = TRUE)) {
        parts <- strsplit(sig, "->", fixed = TRUE)[[1]]
        sig <- trimws(parts[[1]])
        ret_type <- trimws(parts[[2]])
    }
    name <- NULL
    args_str <- NULL
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*\\((.*)\\)$", sig)
    mm <- regmatches(sig, m)[[1]]
    if (length(mm) >=
        3) {
        name <- mm[[2]]
        args_str <- trimws(mm[[3]])
    } else {
        m2 <- regexec("^\\((.*)\\)$", sig)
        mm2 <- regmatches(sig, m2)[[1]]
        if (length(mm2) >=
            2) {
            args_str <- trimws(mm2[[2]])
        } else if (grepl(":", sig, fixed = TRUE)) {
            args_str <- sig
        }
    }
    if (is.null(args_str)) {
        stop(
            "mojor_fn: signature must look like name(x: f64[], y: i32) or (x: f64[], y: i32)"
        )
    }
    if (!nzchar(args_str)) {
        return(
            list(
                name = name, args = character(0),
                types = list(), return = ret_type
            )
        )
    }
    parts <- strsplit(args_str, ",")[[1]]
    args <- character(0)
    types <- list()
    for (p in parts) {
        p <- trimws(p)
        if (!nzchar(p))
            next
        kv <- strsplit(p, ":", fixed = TRUE)[[1]]
        if (length(kv) <
            2)
            stop("mojor_fn: each argument must be annotated like x: f64[]")
        arg <- trimws(kv[[1]])
        ty <- trimws(paste(kv[-1], collapse = ":"))
        if (!nzchar(arg) ||
            !nzchar(ty)) {
            stop("mojor_fn: invalid argument annotation: ", p)
        }
        if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", arg)) {
            stop("mojor_fn: invalid argument name: ", arg)
        }
        args <- c(args, arg)
        types[[arg]] <- ty
    }
    list(name = name, args = args, types = types, return = ret_type)
}

.mojor_hash_text <- function(text) {
    tmp <- tempfile("mojor_sig_")
    on.exit(
        unlink(tmp),
        add = TRUE
    )
    writeLines(text, tmp)
    as.character(tools::md5sum(tmp))
}

.mojor_parse_body_expr <- function(expr, env = parent.frame()) {
    if (is.name(expr)) {
        val <- get(
            as.character(expr),
            envir = env, inherits = TRUE
        )
        if (is.character(val)) {
            expr <- val
        } else {
            return(val)
        }
    }
    if (is.character(expr)) {
        if (length(expr) !=
            1 || !nzchar(expr))
            stop("mojor_fn: body string must be non-empty")
        parsed <- parse(text = expr)
        if (length(parsed) ==
            1) {
            return(parsed[[1]])
        }
        return(
            as.call(
                c(
                  as.name("{"),
                  as.list(parsed)
              )
            )
        )
    }
    expr
}

.mojor_replace_symbol_expr <- function(node, sym_name, replacement) {
    if (is.null(node)) {
        return(NULL)
    }
    if (is.name(node)) {
        if (identical(
            as.character(node),
            sym_name
        )) {
            return(replacement)
        }
        return(node)
    }
    if (!is.call(node)) {
        return(node)
    }
    as.call(
        lapply(
            as.list(node),
            function(x) .mojor_replace_symbol_expr(x, sym_name, replacement)
        )
    )
}

.mojor_extract_single_call_expr <- function(fn) {
    blocks <- .mojor_extract_block(body(fn))
    if (length(blocks) !=
        1) {
        return(NULL)
    }
    expr <- blocks[[1]]
    if (is.call(expr) &&
        identical(
            as.character(expr[[1]]),
            "return"
        ) &&
        length(expr) >=
            2) {
        expr <- expr[[2]]
    }
    if (!is.call(expr)) {
        return(NULL)
    }
    expr
}

.mojor_rewrite_expression_preview_fn <- function(fn) {
    if (!is.function(fn)) {
        return(NULL)
    }
    expr <- .mojor_extract_single_call_expr(fn)
    if (is.null(expr)) {
        return(NULL)
    }

    op <- as.character(expr[[1]])
    env <- environment(fn)

    make_fn <- function(stmts) {
        body_expr <- as.call(
            c(
                as.name("{"),
                stmts
            )
        )
        as.function(
            c(
                formals(fn),
                list(body_expr)
            ),
            env = env
        )
    }

    make_vector_out <- function(out_ctor, len_expr) {
        make_fn(
            list(
                call(
                  "<-", as.name("out"),
                  call(out_ctor, len_expr)
              ),
                call(
                  "<-", as.name("out"),
                  expr
              ),
                as.name("out")
            )
        )
    }

    make_scalar_out <- function(init_value) {
        make_fn(
            list(
                call(
                  "<-", as.name("acc"),
                  init_value
              ),
                call(
                  "<-", as.name("acc"),
                  expr
              ),
                as.name("acc")
            )
        )
    }

    if (op == "unique" && length(expr) >=
        2) {
        x <- expr[[2]]
        return(make_vector_out("numeric", call("length", x)))
    }
    if (op == "duplicated" && length(expr) >=
        2) {
        x <- expr[[2]]
        return(make_vector_out("logical", call("length", x)))
    }
    if (op == "anyDuplicated" && length(expr) >=
        2) {
        return(make_scalar_out(0L))
    }
    if (op == "match" && length(expr) >=
        3) {
        x <- expr[[2]]
        return(make_vector_out("integer", call("length", x)))
    }
    if (op == "%in%" && length(expr) >=
        3) {
        x <- expr[[2]]
        return(make_vector_out("logical", call("length", x)))
    }
    if (op %in% c("median", "IQR", "mad") &&
        length(expr) >=
            2) {
        return(make_scalar_out(0))
    }
    if (op == "quantile" && length(expr) >=
        2) {
        args <- as.list(expr)[-1]
        arg_names <- names(args)
        if (is.null(arg_names))
            arg_names <- rep("", length(args))
        probs_idx <- NULL
        if ("probs" %in% arg_names) {
            probs_idx <- which(arg_names == "probs")[1]
        } else if (length(args) >=
            2) {
            probs_idx <- 2
        }
        if (is.null(probs_idx)) {
            return(NULL)
        }
        probs_expr <- args[[probs_idx]]
        probs_name <- as.name("__mojor_probs")
        args[[probs_idx]] <- probs_name
        quant_call <- as.call(
            c(
                list(as.name("quantile")),
                args
            )
        )

        literal_probs <- NULL
        if (is.call(probs_expr) &&
            identical(
                as.character(probs_expr[[1]]),
                "c"
            ) &&
            length(probs_expr) >=
                2) {
            parts <- as.list(probs_expr)[-1]
            vals <- vapply(
                parts, function(p) {
                  if ((is.numeric(p) ||
                    is.integer(p)) &&
                    length(p) ==
                      1 && !is.na(p)) {
                    return(as.numeric(p))
                  }
                  NA_real_
                }, numeric(1)
            )
            if (all(!is.na(vals)))
                literal_probs <- vals
        }

        if (!is.null(literal_probs)) {
            init_stmts <- list(
                call("<-", probs_name, call("numeric", as.integer(length(literal_probs))))
            )
            for (i in seq_along(literal_probs)) {
                init_stmts[[length(init_stmts) +
                  1]] <- call(
                  "<-", call("[", probs_name, as.integer(i)),
                  as.numeric(literal_probs[[i]])
              )
            }
            return(
                make_fn(
                  c(
                    init_stmts, list(
                      call(
                        "<-", as.name("out"),
                        call("numeric", as.integer(length(literal_probs)))
                    ),
                      call(
                        "<-", as.name("out"),
                        quant_call
                    ),
                      as.name("out")
                  )
                )
              )
            )
        }

        return(
            make_fn(
                list(
                  call("<-", probs_name, probs_expr),
                  call(
                    "<-", as.name("out"),
                    call("numeric", call("length", probs_name))
                ),
                  call(
                    "<-", as.name("out"),
                    quant_call
                ),
                  as.name("out")
              )
            )
        )
    }

    extract_inline_fun <- function(fun_expr, op_name, expected_arity) {
        if (!(is.call(fun_expr) &&
            identical(
                as.character(fun_expr[[1]]),
                "function"
            ))) {
            stop(
                "mojor_fn: ", op_name, "() requires FUN to be an anonymous function in this preview path"
            )
        }
        fmls <- fun_expr[[2]]
        arg_names <- names(fmls)
        if (is.null(arg_names) ||
            length(arg_names) !=
                expected_arity || any(!nzchar(arg_names))) {
            stop(
                "mojor_fn: ", op_name, "() FUN arity must match the provided inputs in this preview path"
            )
        }
        body_blocks <- .mojor_extract_block(fun_expr[[3]])
        if (length(body_blocks) !=
            1) {
            stop(
                "mojor_fn: ", op_name, "() FUN must be a single-expression body in this preview path"
            )
        }
        mapped <- body_blocks[[1]]
        if (is.call(mapped) &&
            identical(
                as.character(mapped[[1]]),
                "return"
            ) &&
            length(mapped) >=
                2) {
            mapped <- mapped[[2]]
        }
        if (is.call(mapped)) {
            op_inner <- as.character(mapped[[1]])
            if (op_inner %in% c("c", "list")) {
                stop(
                  "mojor_fn: ", op_name, "() preview path only supports scalar FUN return values"
              )
            }
        }
        captures <- setdiff(
            all.vars(mapped, functions = FALSE, unique = TRUE),
            arg_names
        )
        if (length(captures) >
            0) {
            stop(
                "mojor_fn: ", op_name, "() preview path does not support closures/non-inlineable captures: ",
                paste(captures, collapse = ", ")
            )
        }
        list(arg_names = arg_names, mapped = mapped)
    }

    if (op %in% c("vapply", "sapply", "lapply") &&
        length(expr) >=
            3) {
        x <- expr[[2]]
        if (!is.name(x)) {
            stop("mojor_fn: ", op, "() preview path requires direct vector arguments")
        }
        fun <- expr[[3]]
        fun_info <- extract_inline_fun(fun, op, expected_arity = 1L)
        mapped <- .mojor_replace_symbol_expr(fun_info$mapped, fun_info$arg_names[[1]], call("[", x, as.name("i")))
        out_ctor <- "numeric"
        if (op == "vapply") {
            if (length(expr) <
                4) {
                stop("mojor_fn: vapply() preview path requires FUN.VALUE")
            }
            fun_value <- expr[[4]]
            if (is.call(fun_value) &&
                length(fun_value) ==
                  2) {
                ctor <- as.character(fun_value[[1]])
                n_arg <- fun_value[[2]]
                if ((is.numeric(n_arg) ||
                  is.integer(n_arg)) &&
                  length(n_arg) ==
                    1 && as.integer(n_arg) ==
                  1L) {
                  if (ctor %in% c("numeric", "double"))
                    out_ctor <- "numeric"
                  if (ctor %in% c("integer"))
                    out_ctor <- "integer"
                  if (ctor %in% c("logical"))
                    out_ctor <- "logical"
                }
            }
            if (!out_ctor %in% c("numeric", "integer", "logical")) {
                stop(
                  "mojor_fn: vapply() preview path supports FUN.VALUE in numeric(1)/integer(1)/logical(1) form"
              )
            }
        }
        return(
            make_fn(
                list(
                  call(
                    "<-", as.name("out"),
                    call(out_ctor, call("length", x))
                ),
                  call(
                    "for", as.name("i"),
                    call("seq_along", x),
                    call(
                      "<-", call(
                        "[", as.name("out"),
                        as.name("i")
                    ),
                      mapped
                  )
                ),
                  as.name("out")
              )
            )
        )
    }

    if (op == "mapply" && length(expr) >=
        4) {
        fun <- expr[[2]]
        call_args <- as.list(expr)[3:length(expr)]
        if (length(call_args) <
            2) {
            stop(
                "mojor_fn: mapply() preview path requires at least two direct vector arguments"
            )
        }
        if (!all(vapply(call_args, is.name, logical(1)))) {
            stop("mojor_fn: mapply() preview path requires direct vector arguments")
        }
        fun_info <- extract_inline_fun(fun, "mapply", expected_arity = length(call_args))
        mapped <- fun_info$mapped
        for (j in seq_along(fun_info$arg_names)) {
            mapped <- .mojor_replace_symbol_expr(
                mapped, fun_info$arg_names[[j]], call("[", call_args[[j]], as.name("i"))
            )
        }
        x <- call_args[[1]]
        return(
            make_fn(
                list(
                  call(
                    "<-", as.name("out"),
                    call("numeric", call("length", x))
                ),
                  call(
                    "for", as.name("i"),
                    call("seq_along", x),
                    call(
                      "<-", call(
                        "[", as.name("out"),
                        as.name("i")
                    ),
                      mapped
                  )
                ),
                  as.name("out")
              )
            )
        )
    }

    NULL
}

.mojor_preview_single_call_name <- function(fn) {
    expr <- .mojor_extract_single_call_expr(fn)
    if (is.null(expr)) {
        return(NULL)
    }
    op <- as.character(expr[[1]])
    if (length(op) !=
        1) {
        return(NULL)
    }
    op
}

.mojor_build_preview_interpreted_result <- function(fn) {
    list(
        func = fn, object_mode = FALSE, object_mode_kind = "off", object_mode_reason = "preview_interpreted_fallback",
        interpreted_fallback = TRUE
    )
}

.mojor_parse_logical_flag <- function(value, src = "ir_only", context = "mojor_fn") {
    if (is.null(value)) {
        return(NULL)
    }
    if (!is.logical(value) ||
        length(value) !=
            1 || is.na(value)) {
        stop(context, ": ", src, " must be TRUE or FALSE")
    }
    isTRUE(value)
}

.mojor_parse_ir_only_flag <- function(value, src = "ir_only") {
    .mojor_parse_logical_flag(value, src = src, context = "mojor_fn")
}

.mojor_resolve_effective_ir_only <- function(
    primary = list(), secondary = list(), context = "mojor_fn", default = isTRUE(.mojor_state$options$ir_only)
) {
    effective <- isTRUE(default)
    for (arg_set in list(primary, secondary)) {
        if (!is.list(arg_set) ||
            length(arg_set) ==
                0)
            next
        nms <- names(arg_set)
        if (is.null(nms) ||
            !("ir_only" %in% nms))
            next
        parsed <- .mojor_parse_logical_flag(arg_set$ir_only, src = "ir_only", context = context)
        if (!is.null(parsed))
            effective <- parsed
    }
    effective
}

.mojor_strict_fallback_error <- function(reason) {
    stop(
        "mojor_fn: strict mode (ir_only=TRUE) forbids expression/object fallback: ",
        reason, call. = FALSE
    )
}

.mojor_warn_preview_off_mode <- function(op_name) {
    warning(
        "mojor_fn: preview op '", op_name, "' currently runs interpreted with object_mode='off'",
        call. = FALSE
    )
}

.mojor_build_with_expression_fallback <- function(build_args, user_extra = list()) {
    maybe_fn <- build_args[[1]]
    user_set_object_mode <- "object_mode" %in% names(user_extra)
    preview_op <- .mojor_preview_single_call_name(maybe_fn)
    interpreted_preview_ops <- c(
        "median", "quantile", "IQR", "mad"
    )
    tier8_hof_ops <- c("vapply", "sapply", "lapply", "mapply")
    effective_ir_only <- .mojor_resolve_effective_ir_only(primary = build_args, secondary = user_extra, context = "mojor_fn")

    built <- tryCatch(
        do.call(mojor_build, build_args),
        error = function(e) e
    )
    if (!inherits(built, "error")) {
        return(built)
    }
    err_msg <- conditionMessage(built)
    quantile_preview_transpile_error <- !is.null(preview_op) &&
        preview_op %in% interpreted_preview_ops &&
        grepl("mojor_transpile:", err_msg, fixed = TRUE)
    needs_expr_fallback <- grepl(
        "expression-only mode currently only supports",
        err_msg,
        fixed = TRUE
    ) || grepl(
        "mojor_transpile: couldn't determine output kind",
        err_msg,
        fixed = TRUE
    ) || isTRUE(quantile_preview_transpile_error)
    if (!isTRUE(needs_expr_fallback)) {
        stop(built)
    }
    if (isTRUE(effective_ir_only)) {
        .mojor_strict_fallback_error(err_msg)
    }

 # First try lowering known preview expressions into explicit
 # output/loop forms.
    rewritten <- .mojor_rewrite_expression_preview_fn(maybe_fn)
    if (is.function(rewritten)) {
        op_for_diag <- .mojor_preview_single_call_name(maybe_fn)
        is_tier8_rewrite <- !is.null(op_for_diag) &&
            op_for_diag %in% tier8_hof_ops
        if (isTRUE(is_tier8_rewrite)) {
            stop(
                "mojor_fn: unexpected compiled subset preview rewrite fallback for '",
                op_for_diag, "'; strict IR-native path should handle this form",
                call. = FALSE
            )
        } else {
            warning(
                "mojor_fn: using preview rewrite fallback for '", if (is.null(op_for_diag))
                  "<unknown>" else op_for_diag, "'; this form is not on the IR-native expression path yet",
                call. = FALSE
            )
        }
        retry_args <- build_args
        retry_args[[1]] <- rewritten
        retry_built <- tryCatch(
            do.call(mojor_build, retry_args),
            error = function(e) e
        )
        if (!inherits(retry_built, "error")) {
            retry_built$preview_rewrite_fallback <- TRUE
            retry_built$preview_rewrite_op <- if (is.null(op_for_diag))
                NA_character_ else op_for_diag
            retry_built$preview_rewrite_unexpected <- isTRUE(is_tier8_rewrite)
            return(retry_built)
        }
        built <- retry_built
    }

    if (isTRUE(user_set_object_mode)) {
        if (!is.null(preview_op) &&
            preview_op %in% interpreted_preview_ops) {
            .mojor_warn_preview_off_mode(preview_op)
            return(.mojor_build_preview_interpreted_result(maybe_fn))
        }
        stop(built)
    }

    if (isTRUE(effective_ir_only)) {
        .mojor_strict_fallback_error("hybrid fallback path")
    }
    build_args$object_mode <- "hybrid"
    do.call(mojor_build, build_args)
}

mojor_fn <- function(
    signature, body, ..., name = NULL, cache = TRUE, load = TRUE, env = parent.frame()
) {
    if (is.function(signature)) {
        if (!missing(body)) {
            stop(
                "mojor_fn: when the first argument is a function, omit the body and pass type hints as named args"
            )
        }
        arg_names <- names(formals(signature))
        if (length(arg_names) >
            0) {
            hinted <- names(list(...))
            missing_hints <- setdiff(arg_names, hinted)
            if (length(missing_hints) >
                0) {
                stop(
                  "mojor_fn: missing type hints for: ", paste(missing_hints, collapse = ", ")
              )
            }
        }
        if (is.null(name) ||
            !nzchar(name)) {
            hash <- .mojor_hash_text(
                paste(
                  deparse(signature),
                  collapse = "\n"
              )
            )
            name <- paste0("mojor_fn_", substr(hash, 1, 10))
        }
        user_extra <- list(...)
        args <- c(
            list(signature, name = name, cache = cache, load = load),
            user_extra
        )
        built <- .mojor_build_with_expression_fallback(args, user_extra = user_extra)
        if (isTRUE(load)) {
            return(built$func)
        }
        return(built)
    }

    sig <- .mojor_parse_signature(signature)
    if (!is.null(name) &&
        nzchar(name)) {
        sig$name <- name
    } else if (is.null(sig$name) ||
        !nzchar(sig$name)) {
        hash <- .mojor_hash_text(
            paste(
                signature, paste(
                  deparse(body),
                  collapse = "\n"
              ),
                sep = "\n"
            )
        )
        sig$name <- paste0("mojor_fn_", substr(hash, 1, 10))
    }
    extra <- list(...)
    if (length(extra) >
        0) {
        type_specs <- c(
            "f64", "f64[]", "i32", "i32[]", "lgl", "lgl[]", "bool", "bool[]",
            "f32", "f32[]", "chr", "chr[]", "chr[,]"
        )
        type_hint_args <- names(extra)[vapply(
            extra, function(v) {
                is.character(v) &&
                  length(v) ==
                    1 && !is.na(v) &&
                  v %in% type_specs
            }, logical(1)
        )]
        if (length(type_hint_args) >
            0) {
            unknown <- setdiff(type_hint_args, sig$args)
            if (length(unknown) >
                0) {
                stop(
                  "mojor_fn: signature string already defines types; remove type hints for: ",
                  paste(unknown, collapse = ", ")
              )
            }
            mismatched <- type_hint_args[vapply(
                type_hint_args, function(nm) {
                  !isTRUE(sig$types[[nm]] == extra[[nm]])
                }, logical(1)
            )]
            if (length(mismatched) >
                0) {
                stop(
                  "mojor_fn: type hints conflict with signature for: ",
                  paste(mismatched, collapse = ", ")
              )
            }
            warning(
                "mojor_fn: redundant type hints ignored for: ", paste(type_hint_args, collapse = ", "),
                call. = FALSE
            )
            extra[type_hint_args] <- NULL
        }
    }
    body_expr <- .mojor_parse_body_expr(
        substitute(body),
        env = env
    )
    formals <- as.pairlist(
        setNames(
            vector("list", length(sig$args)),
            sig$args
        )
    )
    fn <- as.function(
        c(formals, list(body_expr)),
        env = env
    )
    args <- c(
        list(fn, name = sig$name, cache = cache, load = load),
        sig$types, extra
    )
    built <- .mojor_build_with_expression_fallback(args, user_extra = extra)
    if (isTRUE(load)) {
        return(built$func)
    }
    built
}

mojor_ir_dump <- function(expr, env = parent.frame()) {
    body_expr <- .mojor_parse_body_expr(
        substitute(expr),
        env = env
    )
    .mojor_ir_dump(body_expr)
}

mojor_ir_print <- function(expr, env = parent.frame()) {
    ir <- if (is.list(expr) &&
        !is.null(expr$kind)) {
        expr
    } else {
        body_expr <- .mojor_parse_body_expr(
            substitute(expr),
            env = env
        )
        .mojor_ir_dump(body_expr)
    }
    formatted <- .mojor_ir_format(ir)
    if (length(formatted) >
        1) {
        formatted <- paste(formatted, collapse = "\n")
    }
    cat(formatted, "\n")
    invisible(ir)
}

.mojor_with_library_path <- function(dir, fn) {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Linux") {
        old <- Sys.getenv("LD_LIBRARY_PATH", "")
        new <- if (nzchar(old))
            paste(dir, old, sep = ":") else dir
        Sys.setenv(LD_LIBRARY_PATH = new)
        on.exit(
            Sys.setenv(LD_LIBRARY_PATH = old),
            add = TRUE
        )
        return(fn())
    }
    if (sysname == "Darwin") {
        old <- Sys.getenv("DYLD_LIBRARY_PATH", "")
        new <- if (nzchar(old))
            paste(dir, old, sep = ":") else dir
        Sys.setenv(DYLD_LIBRARY_PATH = new)
        on.exit(
            Sys.setenv(DYLD_LIBRARY_PATH = old),
            add = TRUE
        )
        return(fn())
    }
    fn()
}

# Back-compat alias for older callers in build/test code.
.mojor_with_ld_library_path <- function(dir, fn) {
    .mojor_with_library_path(dir, fn)
}

.mojor_bridge_candidates <- function() {
    dlls <- getLoadedDLLs()
    if (length(dlls) ==
        0L) {
        return(
            list(
                pkg = character(0),
                path = character(0)
            )
        )
    }
    paths <- vapply(
        dlls, function(d) getElement(d, "path"),
        character(1)
    )
    idx <- which(endsWith(paths, "mojor_bridge.so"))
    if (length(idx) ==
        0L) {
        return(
            list(
                pkg = character(0),
                path = character(0)
            )
        )
    }
    list(
        pkg = names(paths)[idx],
        path = normalizePath(paths[idx], winslash = "/", mustWork = FALSE)
    )
}

.mojor_bind_bridge <- function(pkg, path = NULL) {
    old_pkg <- NULL
    if (exists(".mojor_state", inherits = TRUE)) {
        old_pkg <- .mojor_state$bridge_pkg
    }
    if (!is.null(old_pkg) &&
        is.character(old_pkg) &&
        length(old_pkg) ==
            1L && nzchar(old_pkg) &&
        !identical(old_pkg, pkg) &&
        exists(".mojor_state", inherits = TRUE) &&
        !is.null(.mojor_state$gpu_ctx)) {
        .mojor_state$gpu_ctx <- NULL
    }
    if (exists(".mojor_state", inherits = TRUE)) {
        .mojor_state$bridge_pkg <- pkg
        .mojor_state$bridge_path <- if (is.null(path))
            NULL else normalizePath(path, winslash = "/", mustWork = FALSE)
        .mojor_state$runtime_v1_symbol_cache <- list()
    }
    pkg
}

.mojor_bridge_pkg <- function(required = TRUE) {
    if (exists(".mojor_state", inherits = TRUE)) {
        pkg <- .mojor_state$bridge_pkg
        if (is.character(pkg) &&
            length(pkg) ==
                1 && nzchar(pkg) &&
            (is.loaded("mojor_sum_f64", PACKAGE = pkg) ||
                is.loaded("mojor_sum_f64_std", PACKAGE = pkg))) {
            return(pkg)
        }
    }

    candidates <- .mojor_bridge_candidates()
    if (length(candidates$pkg) ==
        0L) {
        if (isTRUE(required)) {
            stop("Mojo backend not loaded; call mojor_load() first")
        }
        return(NULL)
    }

 # First preference: exact previously bound bridge path.
    if (exists(".mojor_state", inherits = TRUE)) {
        prev_path <- .mojor_state$bridge_path
        if (is.character(prev_path) &&
            length(prev_path) ==
                1L && nzchar(prev_path)) {
            prev_norm <- normalizePath(prev_path, winslash = "/", mustWork = FALSE)
            hit <- which(candidates$path == prev_norm)
            if (length(hit) >
                0L) {
                i <- hit[[length(hit)]]
                return(.mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]]))
            }
        }
    }

 # Second preference: currently configured MOJOR_LIB_PATH.
    mojor_lib <- Sys.getenv("MOJOR_LIB_PATH", "")
    if (nzchar(mojor_lib)) {
        wanted <- normalizePath(
            file.path(mojor_lib, "mojor_bridge.so"),
            winslash = "/", mustWork = FALSE
        )
        hit <- which(candidates$path == wanted)
        if (length(hit) >
            0L) {
            i <- hit[[length(hit)]]
            return(.mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]]))
        }
    }

 # Final fallback: newest loaded bridge candidate.
    i <- length(candidates$pkg)
    .mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]])
}

.mojor_bridge_pkg_for_symbol <- function(symbol) {
    pkg <- .mojor_bridge_pkg(required = TRUE)
    if (is.loaded(symbol, PACKAGE = pkg)) {
        return(pkg)
    }

    candidates <- .mojor_bridge_candidates()
    if (length(candidates$pkg) >
        0L) {
 # Prefer bridge from MOJOR_LIB_PATH if it is loaded and
 # exports the symbol.
        mojor_lib <- Sys.getenv("MOJOR_LIB_PATH", "")
        if (nzchar(mojor_lib)) {
            wanted <- normalizePath(
                file.path(mojor_lib, "mojor_bridge.so"),
                winslash = "/", mustWork = FALSE
            )
            hit <- which(candidates$path == wanted)
            if (length(hit) >
                0L) {
                i <- hit[[length(hit)]]
                cand_pkg <- candidates$pkg[[i]]
                if (is.loaded(symbol, PACKAGE = cand_pkg)) {
                  return(.mojor_bind_bridge(cand_pkg, candidates$path[[i]]))
                }
            }
        }

 # Otherwise use any loaded bridge candidate that exports the
 # symbol.
        for (i in rev(seq_along(candidates$pkg))) {
            cand_pkg <- candidates$pkg[[i]]
            if (is.loaded(symbol, PACKAGE = cand_pkg)) {
                return(.mojor_bind_bridge(cand_pkg, candidates$path[[i]]))
            }
        }
    }

 # Last resort: try loading bridge from MOJOR_LIB_PATH and
 # re-check.
    mojor_lib <- Sys.getenv("MOJOR_LIB_PATH", "")
    if (nzchar(mojor_lib)) {
        try(
            mojor_load(mojor_lib),
            silent = TRUE
        )
        pkg <- .mojor_bridge_pkg(required = TRUE)
        if (is.loaded(symbol, PACKAGE = pkg)) {
            return(pkg)
        }
    }

    pkg
}

.mojor_runtime_engine <- function() {
    default_engine <- "mojo_rt_v1"
    raw <- Sys.getenv("MOJOR_RUNTIME_ENGINE", default_engine)
    cache <- .mojor_state$runtime_env_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    if (is.character(cache$engine_raw) &&
        length(cache$engine_raw) == 1L &&
        identical(cache$engine_raw, raw) &&
        is.character(cache$engine_value) &&
        length(cache$engine_value) == 1L &&
        nzchar(cache$engine_value)) {
        return(cache$engine_value)
    }
    engine <- raw
    if (!is.character(engine) ||
        length(engine) != 1L ||
        !nzchar(engine)) {
        cache$engine_raw <- raw
        cache$engine_value <- default_engine
        .mojor_state$runtime_env_cache <- cache
        return(default_engine)
    }
    engine <- tolower(trimws(engine))
    if (!(engine %in% c("legacy", "mojo_rt_v1"))) {
        engine <- default_engine
    }
    cache$engine_raw <- raw
    cache$engine_value <- engine
    .mojor_state$runtime_env_cache <- cache
    engine
}

.mojor_runtime_shadow_enabled <- function() {
    env_raw <- Sys.getenv("MOJOR_RUNTIME_SHADOW", "0")
    cache <- .mojor_state$runtime_env_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    if (is.character(cache$shadow_raw) &&
        length(cache$shadow_raw) == 1L &&
        identical(cache$shadow_raw, env_raw) &&
        is.logical(cache$shadow_value) &&
        length(cache$shadow_value) == 1L &&
        !is.na(cache$shadow_value)) {
        return(cache$shadow_value)
    }
    raw <- tolower(trimws(env_raw))
    value <- raw %in% c("1", "true", "yes", "on")
    cache$shadow_raw <- env_raw
    cache$shadow_value <- value
    .mojor_state$runtime_env_cache <- cache
    value
}

.mojor_runtime_require_v1 <- function() {
    env_raw <- Sys.getenv("MOJOR_RUNTIME_REQUIRE_V1", "0")
    cache <- .mojor_state$runtime_env_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    if (is.character(cache$require_v1_raw) &&
        length(cache$require_v1_raw) == 1L &&
        identical(cache$require_v1_raw, env_raw) &&
        is.logical(cache$require_v1_value) &&
        length(cache$require_v1_value) == 1L &&
        !is.na(cache$require_v1_value)) {
        return(cache$require_v1_value)
    }
    raw <- tolower(trimws(env_raw))
    value <- raw %in% c("1", "true", "yes", "on")
    cache$require_v1_raw <- env_raw
    cache$require_v1_value <- value
    .mojor_state$runtime_env_cache <- cache
    value
}

.mojor_runtime_shadow_tolerance <- function() {
    tol <- suppressWarnings(as.double(Sys.getenv("MOJOR_RUNTIME_SHADOW_TOL", "1e-8")))
    if (!is.finite(tol) ||
        tol < 0) {
        return(1e-8)
    }
    tol
}

.mojor_runtime_symbol_v1_candidates <- function(symbol) {
    if (startsWith(symbol, "mojor_rt_v1_")) {
        return(symbol)
    }
    out <- paste0("mojor_rt_v1_", symbol)
    if (startsWith(symbol, "mojor_")) {
        out <- c(out, paste0("mojor_rt_v1_", sub("^mojor_", "", symbol)))
    }
    unique(out)
}

.mojor_runtime_v1_cache_key <- function(pkg, symbol) {
    paste0(pkg, "::", symbol)
}

.mojor_runtime_v1_resolve_symbol <- function(pkg, symbol) {
    if (!is.character(pkg) ||
        length(pkg) != 1L ||
        !nzchar(pkg)) {
        return(NULL)
    }
    key <- .mojor_runtime_v1_cache_key(pkg, symbol)
    cache <- .mojor_state$runtime_v1_symbol_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    cached <- cache[[key]]
    if (is.character(cached) &&
        length(cached) == 1L) {
        if (!is.na(cached)) {
            return(cached)
        }
        return(NULL)
    }
    candidates <- .mojor_runtime_symbol_v1_candidates(symbol)
    resolved <- NULL
    for (symbol_v1 in candidates) {
        if (isTRUE(is.loaded(symbol_v1, PACKAGE = pkg))) {
            resolved <- symbol_v1
            break
        }
    }
    cache[[key]] <- if (is.null(resolved))
        NA_character_ else resolved
    .mojor_state$runtime_v1_symbol_cache <- cache
    resolved
}

.mojor_runtime_symbol_is_deterministic <- function(symbol) {
    cache <- .mojor_state$runtime_deterministic_symbol_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    cached <- cache[[symbol]]
    if (is.logical(cached) &&
        length(cached) == 1L &&
        !is.na(cached)) {
        return(cached)
    }
    out <- !grepl(
        "rng|seed|runif|rnorm|rgamma|rbinom|rexp|rpois|rlnorm|rchisq|rweibull|rlogis|rcauchy|rgeom|rnbinom|rhyper|rsignrank|rwilcox|\\bmojor_rt\\b|\\bmojor_rf\\b",
        symbol,
        ignore.case = TRUE
    )
    cache[[symbol]] <- out
    .mojor_state$runtime_deterministic_symbol_cache <- cache
    out
}

.mojor_runtime_warn_legacy_once <- function() {
    if (isTRUE(.mojor_state$runtime_legacy_warned)) {
        return(invisible(FALSE))
    }
    warning(
        "mojor runtime engine 'legacy' is rollback-only; ",
        "set MOJOR_RUNTIME_ENGINE=mojo_rt_v1 to use the default migration path",
        call. = FALSE
    )
    .mojor_state$runtime_legacy_warned <- TRUE
    invisible(TRUE)
}

.mojor_runtime_results_equivalent <- function(lhs, rhs, tolerance = .mojor_runtime_shadow_tolerance()) {
    if (!identical(class(lhs), class(rhs))) {
        return(list(ok = FALSE, reason = "class_mismatch"))
    }
    if (!identical(dim(lhs), dim(rhs))) {
        return(list(ok = FALSE, reason = "dim_mismatch"))
    }
    if (!identical(dimnames(lhs), dimnames(rhs))) {
        return(list(ok = FALSE, reason = "dimnames_mismatch"))
    }
    if (!identical(names(lhs), names(rhs))) {
        return(list(ok = FALSE, reason = "names_mismatch"))
    }
    if (is.numeric(lhs) || is.numeric(rhs) ||
        is.complex(lhs) || is.complex(rhs)) {
        ok <- isTRUE(all.equal(lhs, rhs, tolerance = tolerance, check.attributes = TRUE))
        return(list(ok = ok, reason = if (isTRUE(ok)) "ok" else "numeric_mismatch"))
    }
    ok <- identical(lhs, rhs)
    list(ok = ok, reason = if (isTRUE(ok)) "ok" else "value_mismatch")
}

.mojor_runtime_shadow_warning <- function(symbol, reason, engine, detail = NULL) {
    msg <- paste0(
        "mojor runtime shadow mismatch [symbol=",
        symbol,
        ", engine=",
        engine,
        ", reason=",
        reason,
        "]"
    )
    if (!is.null(detail) &&
        is.character(detail) &&
        length(detail) == 1L &&
        nzchar(detail)) {
        msg <- paste0(msg, ": ", detail)
    }
    warning(msg, call. = FALSE)
}

.mojor_runtime_v1_strict_error <- function(symbol, reason, detail = NULL) {
    msg <- paste0(
        "mojor runtime strict-v1 failure [symbol=",
        symbol,
        ", reason=",
        reason,
        "]"
    )
    if (!is.null(detail) &&
        is.character(detail) &&
        length(detail) == 1L &&
        nzchar(detail)) {
        msg <- paste0(msg, ": ", detail)
    }
    stop(msg, call. = FALSE)
}

.mojor_runtime_dispatch_call <- function(symbol, pkg, deterministic = TRUE, ...) {
    engine <- .mojor_runtime_engine()
    do_shadow <- isTRUE(deterministic) &&
        isTRUE(.mojor_runtime_shadow_enabled())
    bridge_pkg <- .mojor_state$bridge_pkg
    if (!is.character(bridge_pkg) ||
        length(bridge_pkg) != 1L ||
        !nzchar(bridge_pkg)) {
        bridge_pkg <- .mojor_bridge_pkg(required = FALSE)
    }
    bridge_scope <- !is.null(pkg) &&
        is.character(pkg) &&
        length(pkg) == 1L &&
        !is.null(bridge_pkg) &&
        identical(pkg, bridge_pkg)
    require_v1 <- FALSE
    if (!isTRUE(do_shadow) &&
        isTRUE(.mojor_runtime_require_v1())) {
        require_v1 <- isTRUE(bridge_scope)
    }

    call_legacy <- function() {
        .Call(symbol, ..., PACKAGE = pkg)
    }
    call_legacy_checked <- function() {
        legacy_res <- tryCatch(call_legacy(), error = function(e) e)
        if (!inherits(legacy_res, "error")) {
            return(legacy_res)
        }
        if (identical(engine, "mojo_rt_v1") && isTRUE(bridge_scope)) {
            .mojor_runtime_v1_strict_error(
                symbol = symbol,
                reason = "runtime v1 dispatch failure",
                detail = conditionMessage(legacy_res)
            )
        }
        stop(legacy_res)
    }

 # Fast-path: in non-shadow mojo_rt_v1 mode, when no v1 symbol alias exists
 # for this call, go directly to legacy without per-call tryCatch overhead.
    if (!isTRUE(do_shadow) &&
        identical(engine, "mojo_rt_v1") &&
        !isTRUE(require_v1)) {
        symbol_v1 <- .mojor_runtime_v1_resolve_symbol(pkg = pkg, symbol = symbol)
        if (is.null(symbol_v1)) {
            return(call_legacy_checked())
        }
        v1_res <- tryCatch(.Call(symbol_v1, ..., PACKAGE = pkg), error = function(e) e)
        if (!inherits(v1_res, "error")) {
            return(v1_res)
        }
        warning(
            "mojor runtime fallback [engine=mojo_rt_v1, symbol=",
            symbol,
            "]: ",
            conditionMessage(v1_res),
            call. = FALSE
        )
        return(call_legacy_checked())
    }

    call_v1 <- function() {
        symbol_v1 <- .mojor_runtime_v1_resolve_symbol(pkg = pkg, symbol = symbol)
        if (!is.null(symbol_v1)) {
            if (!isTRUE(require_v1)) {
                return(.Call(symbol_v1, ..., PACKAGE = pkg))
            }
            v1_res <- tryCatch(.Call(symbol_v1, ..., PACKAGE = pkg), error = function(e) e)
            if (inherits(v1_res, "error")) {
                .mojor_runtime_v1_strict_error(
                    symbol = symbol,
                    reason = "v1_call_error",
                    detail = conditionMessage(v1_res)
                )
            }
            return(v1_res)
        }
        if (isTRUE(require_v1)) {
            candidates <- .mojor_runtime_symbol_v1_candidates(symbol)
            .mojor_runtime_v1_strict_error(
                symbol = symbol,
                reason = "v1_symbol_not_loaded",
                detail = paste(candidates, collapse = ",")
            )
        }
        call_legacy_checked()
    }

    if (identical(engine, "legacy")) {
        .mojor_runtime_warn_legacy_once()
    }

    if (isTRUE(do_shadow)) {
        legacy_res <- tryCatch(call_legacy(), error = function(e) e)
        if (inherits(legacy_res, "error")) {
            stop(legacy_res)
        }
        v1_res <- tryCatch(call_v1(), error = function(e) e)
        if (inherits(v1_res, "error")) {
            .mojor_runtime_shadow_warning(
                symbol = symbol,
                reason = "mojo_rt_v1_error",
                engine = engine,
                detail = conditionMessage(v1_res)
            )
            return(legacy_res)
        }
        cmp <- .mojor_runtime_results_equivalent(legacy_res, v1_res)
        if (!isTRUE(cmp$ok)) {
            .mojor_runtime_shadow_warning(
                symbol = symbol,
                reason = cmp$reason,
                engine = engine
            )
        }
        return(legacy_res)
    }

    if (identical(engine, "mojo_rt_v1")) {
        if (isTRUE(require_v1)) {
            return(call_v1())
        }
        v1_res <- tryCatch(call_v1(), error = function(e) e)
        if (!inherits(v1_res, "error")) {
            return(v1_res)
        }
        warning(
            "mojor runtime fallback [engine=mojo_rt_v1, symbol=",
            symbol,
            "]: ",
            conditionMessage(v1_res),
            call. = FALSE
        )
    }
    call_legacy_checked()
}

.mojor_call_bridge <- function(symbol, ...) {
    pkg <- .mojor_bridge_pkg_for_symbol(symbol)
    dispatch_once <- function() {
        .mojor_runtime_dispatch_call(
            symbol = symbol,
            pkg = pkg,
            deterministic = .mojor_runtime_symbol_is_deterministic(symbol),
            ...
        )
    }
    tryCatch(
        dispatch_once(),
        error = function(e) {
            msg <- conditionMessage(e)
            invalid_ctx <- grepl("invalid context", msg, ignore.case = TRUE)
            if (!isTRUE(invalid_ctx) || identical(symbol, "mojor_gpu_ctx_create")) {
                stop(e)
            }
            epoch <- .mojor_state$gpu_ctx_epoch
            if (is.null(epoch) ||
                length(epoch) != 1L ||
                is.na(epoch)) {
                epoch <- 0L
            }
            .mojor_state$gpu_ctx <- NULL
            .mojor_state$gpu_capability_cache <- list()
            .mojor_state$gpu_ctx_epoch <- as.integer(as.integer(epoch) + 1L)
            dispatch_once()
        }
    )
}

.mojor_find_loaded_dll_pkg <- function(path) {
    if (is.null(path) ||
        !is.character(path) ||
        length(path) !=
            1L || !nzchar(path)) {
        return(NULL)
    }
    target <- normalizePath(path, winslash = "/", mustWork = FALSE)
    dlls <- getLoadedDLLs()
    for (pkg in names(dlls)) {
        dll_path <- tryCatch(
            normalizePath(
                getElement(dlls[[pkg]], "path"),
                winslash = "/", mustWork = FALSE
            ),
            error = function(e) NA_character_
        )
        if (!is.na(dll_path) &&
            identical(dll_path, target)) {
            return(pkg)
        }
    }
    NULL
}

.mojor_find_loaded_dll_path <- function(path = NULL, pkg = NULL) {
    dlls <- getLoadedDLLs()
    if (!is.null(pkg) &&
        is.character(pkg) &&
        length(pkg) == 1L &&
        nzchar(pkg) &&
        pkg %in% names(dlls)) {
        raw <- tryCatch(
            as.character(getElement(dlls[[pkg]], "path")),
            error = function(e) NA_character_
        )
        if (!is.na(raw) && nzchar(raw)) {
            return(raw)
        }
    }
    if (is.null(path) ||
        !is.character(path) ||
        length(path) !=
            1L || !nzchar(path)) {
        return(NULL)
    }
    target <- normalizePath(path, winslash = "/", mustWork = FALSE)
    for (dll in dlls) {
        raw <- tryCatch(
            as.character(getElement(dll, "path")),
            error = function(e) NA_character_
        )
        if (is.na(raw) || !nzchar(raw)) {
            next
        }
        norm <- tryCatch(
            normalizePath(raw, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (!is.na(norm) && identical(norm, target)) {
            return(raw)
        }
    }
    NULL
}

.mojor_resolve_loaded_dll_pkg <- function(path, dll = NULL) {
    if (is.list(dll) &&
        !is.null(dll[["name"]])) {
        pkg <- as.character(dll[["name"]][[1]])
        if (is.character(pkg) &&
            length(pkg) ==
                1L && nzchar(pkg)) {
            return(pkg)
        }
    }
    .mojor_find_loaded_dll_pkg(path)
}

.mojor_is_dll_limit_error <- function(err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "")
    is.character(msg) &&
        length(msg) == 1L &&
        nzchar(msg) &&
        grepl("maximal number of DLLs reached", msg, fixed = TRUE)
}

.mojor_dyn_load_with_recovery <- function(
    so_path,
    lib_dir = NULL,
    retry_on_limit = TRUE
) {
    if (is.null(lib_dir) ||
        !is.character(lib_dir) ||
        length(lib_dir) !=
            1L ||
        !nzchar(lib_dir)) {
        lib_dir <- dirname(so_path)
    }
    load_once <- function() {
        .mojor_with_ld_library_path(lib_dir, function() dyn.load(so_path))
    }
    tryCatch(
        load_once(),
        error = function(e) {
            if (!isTRUE(retry_on_limit) ||
                !.mojor_is_dll_limit_error(e)) {
                stop(e)
            }
            .mojor_unload_tracked_wrappers()
            load_once()
        }
    )
}

.mojor_load_wrapper_pkg <- function(so_path, kind = "kernel") {
    if (is.null(so_path) ||
        !is.character(so_path) ||
        length(so_path) !=
            1L || !nzchar(so_path)) {
        return(NULL)
    }
    so_norm <- normalizePath(so_path, winslash = "/", mustWork = FALSE)
    resolved <- .mojor_find_loaded_dll_pkg(so_norm)
    if (!is.null(resolved) &&
        nzchar(resolved)) {
        return(resolved)
    }
    if (!file.exists(so_norm)) {
        return(NULL)
    }
    dll <- tryCatch(
        .mojor_dyn_load_with_recovery(
            so_path = so_norm,
            lib_dir = dirname(so_norm),
            retry_on_limit = TRUE
        ),
        error = function(e) NULL
    )
    resolved <- .mojor_resolve_loaded_dll_pkg(so_norm, dll)
    if (!is.null(resolved) &&
        nzchar(resolved)) {
        .mojor_register_loaded_wrapper(resolved, so_norm, kind = kind)
        return(resolved)
    }
    NULL
}

.mojor_make_package_scoped_call <- function(
    symbol,
    so_path,
    pkg = NULL,
    missing_message = NULL,
    wrapper_kind = "kernel",
    autoload = TRUE
) {
    deterministic <- .mojor_runtime_symbol_is_deterministic(symbol)
    pkg_cached <- pkg
    so_norm <- normalizePath(so_path, winslash = "/", mustWork = FALSE)
    resolve_pkg <- function(load_if_missing = TRUE) {
        resolved <- .mojor_find_loaded_dll_pkg(so_norm)
        if ((is.null(resolved) || !nzchar(resolved)) &&
            isTRUE(load_if_missing)) {
            resolved <- .mojor_load_wrapper_pkg(so_norm, kind = wrapper_kind)
        }
        resolved
    }
    function(...) {
        resolved <- pkg_cached
        if (!is.null(resolved) &&
            is.character(resolved) &&
            length(resolved) ==
                1L &&
            nzchar(resolved)) {
            dlls <- getLoadedDLLs()
            if (!(resolved %in% names(dlls))) {
                resolved <- NULL
            }
        } else {
            resolved <- NULL
        }
        if (is.null(resolved) ||
            !is.character(resolved) ||
            length(resolved) !=
                1L || !nzchar(resolved)) {
            resolved <- resolve_pkg(load_if_missing = autoload)
            if (is.null(resolved) ||
                !nzchar(resolved)) {
                if (is.null(missing_message) ||
                  !nzchar(missing_message)) {
                  stop(
                    "mojor_build: shared library is not loaded: ", so_norm,
                    call. = FALSE
                )
                }
                stop(missing_message, call. = FALSE)
            }
            pkg_cached <<- resolved
        }
        .mojor_runtime_dispatch_call(
            symbol = symbol,
            pkg = resolved,
            deterministic = deterministic,
            ...
        )
    }
}

.mojor_normalize_kernel_result <- function(value) {
    if (!inherits(value, "float32")) {
        return(value)
    }
    dims <- tryCatch(dim(value), error = function(e) NULL)
    if (is.null(dims) || length(dims) <= 1L) {
        return(value)
    }
    if (length(dims) == 2L && !is.na(dims[[2]]) && as.integer(dims[[2]]) <= 1L) {
        return(value)
    }
    out <- tryCatch(
        {
            if (requireNamespace("float", quietly = TRUE)) {
                float::dbl(value)
            } else {
                as.numeric(value)
            }
        },
        error = function(e) NULL
    )
    if (is.null(out)) {
        return(value)
    }
    if (is.null(dim(out))) {
        dim(out) <- as.integer(dims)
    }
    dim_names <- tryCatch(dimnames(value), error = function(e) NULL)
    if (!is.null(dim_names)) {
        dimnames(out) <- dim_names
    }
    out
}

.mojor_make_kernel_call_wrapper <- function(
    kernel, pkg, rng_needed = FALSE, na_mode = NULL, na_preflight = NULL,
    error_state = NULL, wrapper_so = NULL
) {
    kernel_call_symbol <- paste0(kernel, "_call")
    kernel_deterministic <- !isTRUE(rng_needed)
    rng_seed_symbol <- if (isTRUE(rng_needed))
        paste0(kernel, "_rng_seed_call") else NULL
    kernel_call <- if (!is.null(wrapper_so) &&
        nzchar(wrapper_so)) {
        .mojor_make_package_scoped_call(
            symbol = kernel_call_symbol,
            so_path = wrapper_so,
            pkg = pkg,
            missing_message = paste0(
                "mojor_build: shared library is not loaded: ",
                wrapper_so
            ),
            wrapper_kind = "kernel",
            autoload = TRUE
        )
    } else {
        function(...) {
            .mojor_runtime_dispatch_call(
                symbol = kernel_call_symbol,
                pkg = pkg,
                deterministic = kernel_deterministic,
                ...
            )
        }
    }
    rng_seed_call <- if (isTRUE(rng_needed) &&
        !is.null(wrapper_so) &&
        nzchar(wrapper_so)) {
        .mojor_make_package_scoped_call(
            symbol = rng_seed_symbol,
            so_path = wrapper_so,
            pkg = pkg,
            missing_message = paste0(
                "mojor_build: shared library is not loaded: ",
                wrapper_so
            ),
            wrapper_kind = "kernel",
            autoload = TRUE
        )
    } else if (isTRUE(rng_needed)) {
        function(seed) {
            .mojor_runtime_dispatch_call(
                symbol = rng_seed_symbol,
                pkg = pkg,
                deterministic = FALSE,
                seed
            )
        }
    } else {
        NULL
    }
    last_seed <- NULL
    last_seed_token <- NULL

    run_call <- function(...) {
        if (isTRUE(rng_needed)) {
            cur_seed <- .mojor_state$last_rng_seed
            cur_seed_token <- .mojor_state$last_rng_seed_token
            should_reseed <- !is.null(cur_seed) &&
                (is.null(last_seed) ||
                  !identical(last_seed, cur_seed) ||
                  !identical(last_seed_token, cur_seed_token))
            if (isTRUE(should_reseed)) {
                rng_seed_call(cur_seed)
                last_seed <<- cur_seed
                last_seed_token <<- cur_seed_token
            }
        }
        if (identical(na_mode, "forbid") &&
            is.function(na_preflight)) {
            na_preflight(...)
        }
        out <- kernel_call(...)
        .mojor_normalize_kernel_result(out)
    }

    function(...) {
        if (is.null(error_state) ||
            identical(error_state$error_mode, "stop")) {
            run_call(...)
        } else {
            .mojor_execute_with_state(run_call, error_state, ...)
        }
    }
}

.mojor_default_bridge <- function() {
    mojor_lib <- Sys.getenv("MOJOR_LIB_PATH", "")
    if (nzchar(mojor_lib)) {
        bridge <- file.path(mojor_lib, "mojor_bridge.so")
        if (file.exists(bridge)) {
            return(normalizePath(bridge, winslash = "/", mustWork = FALSE))
        }
    }

    rels <- c(
        file.path("prototype", "build", "mojor_bridge.so"),
        file.path("packages", "mojor", "build", "mojor_bridge.so"),
        file.path("build", "mojor_bridge.so")
    )
    for (rel in rels) {
        hit <- .mojor_find_upward(rel)
        if (!is.null(hit) &&
            nzchar(hit) &&
            file.exists(hit)) {
            return(normalizePath(hit, winslash = "/", mustWork = FALSE))
        }
    }

    NULL
}

mojor_load <- function(path = NULL) {
    if (is.null(path) ||
        !nzchar(path)) {
        bridge_auto <- .mojor_default_bridge()
        if (!is.null(bridge_auto) &&
            nzchar(bridge_auto)) {
            path <- dirname(bridge_auto)
        } else {
            pkg_lib <- system.file(
                "libs", paste0("mojor", .Platform$dynlib.ext),
                package = "mojor"
            )
            if (nzchar(pkg_lib) &&
                file.exists(pkg_lib)) {
                pkg_norm <- normalizePath(pkg_lib, winslash = "/", mustWork = FALSE)
                candidates <- .mojor_bridge_candidates()
                hit <- which(candidates$path == pkg_norm)
                if (length(hit) >
                  0L) {
                  i <- hit[[length(hit)]]
                  .mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]])
                  return(invisible(TRUE))
                }
                dll <- dyn.load(pkg_lib, local = FALSE)
                .mojor_bind_bridge(dll[["name"]], pkg_norm)
                return(invisible(TRUE))
            }
            path <- file.path(getwd(), "build")
        }
    }

    bridge <- file.path(path, "mojor_bridge.so")
    if (!file.exists(bridge)) {
        stop("mojor_load: bridge not found at ", bridge)
    }
    bridge_norm <- normalizePath(bridge, winslash = "/", mustWork = FALSE)

    pkg <- .mojor_bridge_pkg(required = FALSE)
    if (!is.null(pkg) &&
        pkg %in% names(getLoadedDLLs())) {
        dll <- getLoadedDLLs()[[pkg]]
        dll_path <- normalizePath(
            getElement(dll, "path"),
            winslash = "/", mustWork = FALSE
        )
        if (identical(dll_path, bridge_norm) &&
            (is.loaded("mojor_sum_f64", PACKAGE = pkg) ||
                is.loaded("mojor_sum_f64_std", PACKAGE = pkg))) {
            .mojor_bind_bridge(pkg, bridge_norm)
            return(invisible(TRUE))
        }
    }

 # If the requested bridge is already loaded under a different
 # package name, bind to it directly to avoid duplicate ambiguous
 # bridge state.
    candidates <- .mojor_bridge_candidates()
    hit <- which(candidates$path == bridge_norm)
    if (length(hit) >
        0L) {
        i <- hit[[length(hit)]]
        .mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]])
        return(invisible(TRUE))
    }

 # Requested bridge is not loaded yet. If another mojor_bridge is
 # already loaded from a different path, unload it to avoid
 # name-collision reuse.
    conflict <- which(candidates$path != bridge_norm)
    if (length(conflict) >
        0L) {
        for (i in conflict) {
            try(
                dyn.unload(candidates$path[[i]]),
                silent = TRUE
            )
        }
        candidates <- .mojor_bridge_candidates()
        hit <- which(candidates$path == bridge_norm)
        if (length(hit) >
            0L) {
            i <- hit[[length(hit)]]
            .mojor_bind_bridge(candidates$pkg[[i]], candidates$path[[i]])
            return(invisible(TRUE))
        }
    }

    dll <- .mojor_with_library_path(path, function() dyn.load(bridge, local = FALSE))
    .mojor_bind_bridge(dll[["name"]], bridge_norm)
    invisible(TRUE)
}

mojor_is_loaded <- function() {
    pkg <- .mojor_bridge_pkg(required = FALSE)
    if (!is.null(pkg)) {
        return(
            is.loaded("mojor_sum_f64", PACKAGE = pkg) ||
                is.loaded("mojor_sum_f64_std", PACKAGE = pkg)
        )
    }
    FALSE
}

.mojor_truthy_env <- function(name) {
    tolower(Sys.getenv(name, unset = "")) %in% c("1", "true", "yes", "y", "on")
}

mojor_has_gpu <- function() {
    if (.mojor_truthy_env("MOJOR_TEST_ASSUME_NO_GPU") ||
        .mojor_truthy_env("MOJOR_ASSUME_NO_GPU")) {
        return(FALSE)
    }
    if (!mojor_is_loaded()) {
        tryCatch(mojor_load(), error = function(e) NULL)
    }
    if (!mojor_is_loaded()) {
        return(FALSE)
    }
    result <- tryCatch(
        .mojor_call_bridge("mojor_has_gpu"),
        error = function(e) NULL
    )
    if (is.null(result)) {
        tryCatch(mojor_load(), error = function(e) NULL)
        result <- tryCatch(
            .mojor_call_bridge("mojor_has_gpu"),
            error = function(e) NULL
        )
    }
    isTRUE(result)
}

.mojor_gpu_ctx_get <- function() {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    ctx <- .mojor_state$gpu_ctx
    if (is.null(ctx) ||
        !inherits(ctx, "mojor_gpu_ctx")) {
        ctx <- .mojor_call_bridge("mojor_gpu_ctx_create")
        if (is.null(ctx)) {
            stop("mojor: GPU context unavailable", call. = FALSE)
        }
        class(ctx) <- c("mojor_gpu_ctx", class(ctx))
        .mojor_state$gpu_ctx <- ctx
        .mojor_state$gpu_ctx_epoch <- as.integer(.mojor_gpu_ctx_epoch_get() + 1L)
    }
    ctx
}

.mojor_gpu_ctx_epoch_get <- function() {
    epoch <- .mojor_state$gpu_ctx_epoch
    if (is.null(epoch) ||
        length(epoch) !=
            1L || is.na(epoch)) {
        return(0L)
    }
    as.integer(epoch)
}

mojor_gpu_ctx_free <- function() {
    ctx <- .mojor_state$gpu_ctx
    if (is.null(ctx) ||
        !inherits(ctx, "mojor_gpu_ctx")) {
        return(invisible(FALSE))
    }
    if (mojor_gpu_buf_f32_live_count() > 0L) {
        stop("mojor_gpu_ctx_free: GPU buffers still live", call. = FALSE)
    }
    .mojor_call_bridge("mojor_gpu_ctx_free", ctx)
    .mojor_state$gpu_ctx <- NULL
    .mojor_state$gpu_capability_cache <- list()
    invisible(TRUE)
}

mojor_gpu_ctx_reset <- function(mode = c("soft", "hard")) {
    mode <- match.arg(mode)
    ctx <- .mojor_state$gpu_ctx
    if (is.null(ctx) ||
        !inherits(ctx, "mojor_gpu_ctx")) {
        return(invisible(FALSE))
    }
    if (mojor_gpu_buf_f32_live_count() > 0L) {
        stop("mojor_gpu_ctx_reset: GPU buffers still live", call. = FALSE)
    }
    if (mode == "soft") {
        .mojor_state$gpu_ctx <- NULL
        .mojor_state$gpu_capability_cache <- list()
        warning(
            "mojor_gpu_ctx_reset: soft reset drops cached context without freeing backend resources; use mode='hard' for a full in-process reset",
            call. = FALSE
        )
        return(invisible(TRUE))
    }
    mojor_gpu_ctx_free()
    invisible(TRUE)
}

mojor_gpu_meminfo <- function() {
    if (!mojor_is_loaded()) {
        stop("mojor_gpu_meminfo: Mojo backend not loaded")
    }
    if (!mojor_has_gpu()) {
        stop("mojor_gpu_meminfo: GPU not available")
    }
    raw <- .mojor_call_bridge("mojor_gpu_meminfo", .mojor_gpu_ctx_get())
    status <- attr(raw, "status")
    free <- unname(raw[1])
    total <- unname(raw[2])
    if (!is.numeric(status) ||
        length(status) !=
            1 || status <= 0) {
        free <- NA_real_
        total <- NA_real_
    }
    list(
        free_bytes = free, total_bytes = total, free_gb = free/1024^3,
        total_gb = total/1024^3, status = status
    )
}

.mojor_find_upward <- function(rel, max_levels = 6L) {
    cur <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    for (i in 0:max_levels) {
        cand <- file.path(cur, rel)
        if (file.exists(cand)) {
            return(cand)
        }
        parent <- dirname(cur)
        if (parent == cur)
            break
        cur <- parent
    }
    NULL
}

mojor_gpu_ctx_smoke <- function(script = NULL, strict = FALSE) {
    if (is.null(script) ||
        !nzchar(script)) {
        script <- .mojor_find_upward(file.path("prototype", "tools", "gpu_ctx_smoke.mojo"))
        if (is.null(script)) {
            script <- .mojor_find_upward(file.path("tools", "gpu_ctx_smoke.mojo"))
        }
    }
    if (is.null(script) ||
        !nzchar(script) ||
        !file.exists(script)) {
        stop("mojor_gpu_ctx_smoke: gpu_ctx_smoke.mojo not found", call. = FALSE)
    }
    mojo_bin <- Sys.which("mojo")
    if (!nzchar(mojo_bin)) {
        alt_bin <- "/opt/anaconda3/bin/mojo"
        if (file.exists(alt_bin)) {
            mojo_bin <- alt_bin
        }
    }
    if (!nzchar(mojo_bin)) {
        stop("mojor_gpu_ctx_smoke: mojo binary not found", call. = FALSE)
    }
    out <- tryCatch(
        system2(mojo_bin, script, stdout = TRUE, stderr = TRUE),
        error = function(e) {
            stop("mojor_gpu_ctx_smoke: failed to run mojo", call. = FALSE)
        }
    )
    ok <- any(grepl("gpu_ctx_smoke: ok", out, fixed = TRUE))
    if (!ok) {
        msg <- "mojor_gpu_ctx_smoke: failed (see output)"
        if (isTRUE(strict)) {
            stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
    }
    invisible(list(ok = ok, output = out, script = script))
}

.mojor_float_available <- function() {
    requireNamespace("float", quietly = TRUE)
}

.mojor_as_float32 <- function(x) {
    if (!.mojor_float_available()) {
        stop("float package not installed")
    }
    if (float::is.float(x)) {
        return(x)
    }
    float::fl(x)
}

# Backward-compatible helper used in tests and user scripts.
as.float <- function(x) {
    .mojor_as_float32(x)
}

.mojor_gpu_require <- function(label, out_data) {
    gpu_status <- attr(out_data, "gpu_status")
    if (is.numeric(gpu_status) &&
        length(gpu_status) ==
            1 && gpu_status > 0) {
        return(invisible(TRUE))
    }
    status <- if (length(gpu_status))
        as.character(gpu_status) else "unknown"
    stop(
        "mojor: GPU execution failed for ", label, " (status=", status,
        ")", call. = FALSE
    )
}

.mojor_gpu_estimate_bytes <- function(n, buffers = 4L, bytes_per = 4L) {
    if (is.null(n) ||
        length(n) !=
            1L || is.na(n)) {
        return(NA_real_)
    }
    n <- as.double(n)
    buffers <- as.double(buffers)
    bytes_per <- as.double(bytes_per)
    n * buffers * bytes_per
}

.mojor_gpu_max_bytes <- function() {
    env_bytes <- Sys.getenv("MOJOR_GPU_MAX_BYTES", "")
    if (nzchar(env_bytes)) {
        val <- suppressWarnings(as.double(env_bytes))
        if (!is.na(val) &&
            val > 0) {
            return(val)
        }
    }
    env_gb <- Sys.getenv("MOJOR_GPU_MAX_GB", "")
    if (nzchar(env_gb)) {
        val <- suppressWarnings(as.double(env_gb))
        if (!is.na(val) &&
            val > 0) {
            return(val * 1024^3)
        }
    }
    opt <- .mojor_state$options$gpu_max_bytes
    if (!is.null(opt)) {
        val <- suppressWarnings(as.double(opt))
        if (!is.na(val) &&
            val > 0) {
            return(val)
        }
    }
    16 * 1024^3
}

.mojor_gpu_check_limit <- function(label, n, buffers = 4L, bytes_per = 4L) {
    cap <- .mojor_gpu_max_bytes()
    if (is.na(cap) ||
        cap <= 0) {
        return(invisible(TRUE))
    }
    est <- .mojor_gpu_estimate_bytes(n, buffers, bytes_per)
    if (!is.na(est) &&
        est > cap) {
        msg <- sprintf(
            "mojor: %s estimated %.2f MB exceeds gpu_max_bytes (%.2f MB)",
            label, est/(1024^2), cap/(1024^2)
        )
        msg <- paste0(msg, "; set MOJOR_GPU_MAX_GB or mojor_options(gpu_max_bytes=...)")
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}

.mojor_gpu_log <- function(label, n, buffers = 4L, bytes_per = 4L) {
    .mojor_gpu_check_limit(label, n, buffers = buffers, bytes_per = bytes_per)
    if (!isTRUE(.mojor_state$options$gpu_debug)) {
        return(invisible(NULL))
    }
    est <- .mojor_gpu_estimate_bytes(n, buffers, bytes_per)
    cap <- .mojor_gpu_max_bytes()
    mem_note <- ""
    if (mojor_is_loaded() && mojor_has_gpu()) {
        info <- tryCatch(mojor_gpu_meminfo(), error = function(e) NULL)
        if (is.list(info) &&
            is.numeric(info$status) &&
            info$status > 0) {
            mem_note <- sprintf(
                ", free=%.0f MB total=%.0f MB", info$free_bytes/(1024^2),
                info$total_bytes/(1024^2)
            )
        }
    }
    msg <- sprintf(
        "mojor: gpu est for %s: %.2f MB (buffers=%d, bytes=%d, cap=%.0f MB%s)",
        label, est/(1024^2), as.integer(buffers),
        as.integer(bytes_per),
        cap/(1024^2), mem_note
    )
    message(msg)
    invisible(est)
}

.mojor_wrap_float32_gpu <- function(out_data, label = "kernel") {
    .mojor_gpu_require(label, out_data)
    gpu_status <- attr(out_data, "gpu_status")
    out <- float::float32(out_data)
    attr(out, "gpu_status") <- gpu_status
    out
}

.mojor_prepare_float32_gpu_data <- function(x) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    x <- .mojor_as_float32(x)
    methods::slot(x, "Data")
}

.mojor_call_float32_gpu_kernel <- function(x, label, bridge_name, ..., buffers = 4L, bytes_per = 4L) {
    data <- .mojor_prepare_float32_gpu_data(x)
    .mojor_gpu_log(
        label, length(data),
        buffers = buffers, bytes_per = bytes_per
    )
    out_data <- .mojor_call_bridge(bridge_name, .mojor_gpu_ctx_get(), data, ...)
    .mojor_wrap_float32_gpu(out_data, label = label)
}

mojor_sigmoid_f32_gpu <- function(x) {
    .mojor_call_float32_gpu_kernel(
        x, label = "sigmoid_f32_gpu", bridge_name = "mojor_sigmoid_f32_gpu_int"
    )
}

mojor_sigmoid_f64_gpu <- function(x) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    if (!is.numeric(x)) {
        stop("mojor_sigmoid_f64_gpu: x must be numeric")
    }
    data <- as.double(x)
    .mojor_gpu_log(
        "sigmoid_f64_gpu", length(data),
        buffers = 4L, bytes_per = 8L
    )
    out_data <- .mojor_call_bridge("mojor_sigmoid_f64_gpu", .mojor_gpu_ctx_get(), data)
    .mojor_gpu_require("sigmoid_f64_gpu", out_data)
    out_data
}

mojor_sigmoid_f32_gpu_iters <- function(x, iters = 5L) {
    .mojor_call_float32_gpu_kernel(
        x, label = "sigmoid_f32_gpu_iters", bridge_name = "mojor_sigmoid_f32_gpu_iters_int",
        as.integer(iters)
    )
}

mojor_sigmoid_affine_f32_gpu <- function(x, scale = 1, bias = 0) {
    out <- .mojor_call_float32_gpu_kernel(
        x, label = "sigmoid_affine_f32_gpu", bridge_name = "mojor_sigmoid_affine_f32_gpu_int",
        as.numeric(scale),
        as.numeric(bias)
    )
    attr(out, "gpu_scale") <- scale
    attr(out, "gpu_bias") <- bias
    out
}

mojor_sigmoid_affine_f32_gpu_iters <- function(x, iters = 5L, scale = 1, bias = 0) {
    out <- .mojor_call_float32_gpu_kernel(
        x, label = "sigmoid_affine_f32_gpu_iters", bridge_name = "mojor_sigmoid_affine_f32_gpu_iters_int",
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias)
    )
    attr(out, "gpu_scale") <- scale
    attr(out, "gpu_bias") <- bias
    attr(out, "gpu_iters") <- as.integer(iters)
    out
}

mojor_sigmoid_affine_f32_gpu_chain <- function(
    x, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L
) {
    out <- .mojor_call_float32_gpu_kernel(
        x, label = "sigmoid_affine_f32_gpu_chain", bridge_name = "mojor_sigmoid_affine_f32_gpu_chain_int",
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
    )
    attr(out, "gpu_scale") <- scale
    attr(out, "gpu_bias") <- bias
    attr(out, "gpu_iters") <- as.integer(iters)
    attr(out, "gpu_post_scale") <- post_scale
    attr(out, "gpu_post_bias") <- post_bias
    attr(out, "gpu_post_iters") <- as.integer(post_iters)
    out
}

mojor_sigmoid_affine_f32_gpu_chain_sum <- function(
    x, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L, reduction = c("block", "warp")
) {
    reduction <- match.arg(reduction)
    data <- .mojor_prepare_float32_gpu_data(x)
    log_label <- if (reduction == "warp") {
        "sigmoid_affine_f32_gpu_chain_sum_warp"
    } else {
        "sigmoid_affine_f32_gpu_chain_sum"
    }
    .mojor_gpu_log(
        log_label, length(data),
        buffers = 5L, bytes_per = 4L
    )
    out <- if (reduction == "warp") {
        .mojor_call_bridge(
            "mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int", .mojor_gpu_ctx_get(),
            data, as.integer(iters),
            as.numeric(scale),
            as.numeric(bias),
            as.numeric(post_scale),
            as.numeric(post_bias),
            as.integer(post_iters)
        )
    } else {
        .mojor_call_bridge(
            "mojor_sigmoid_affine_f32_gpu_chain_sum_int", .mojor_gpu_ctx_get(),
            data, as.integer(iters),
            as.numeric(scale),
            as.numeric(bias),
            as.numeric(post_scale),
            as.numeric(post_bias),
            as.integer(post_iters)
        )
    }
    .mojor_gpu_require(log_label, out)
    out
}

mojor_gpu_chain <- function(
    x, iters = 5L, op = "sigmoid", scale = 1, bias = 0, post_scale = 1,
    post_bias = 0, post_iters = 0L
) {
    op <- match.arg(op, c("sigmoid"))
    data <- .mojor_prepare_float32_gpu_data(x)
    iters <- as.integer(iters)
    scale <- as.numeric(scale)
    bias <- as.numeric(bias)
    post_scale <- as.numeric(post_scale)
    post_bias <- as.numeric(post_bias)
    post_iters <- as.integer(post_iters)
    use_post <- post_iters > 0L || !isTRUE(all.equal(post_scale, 1)) ||
        !isTRUE(all.equal(post_bias, 0))

    out_data <- NULL
    label <- NULL
    if (op == "sigmoid") {
        if (use_post) {
            .mojor_gpu_log(
                "gpu_chain_sigmoid_affine_chain", length(data),
                buffers = 4L, bytes_per = 4L
            )
            out_data <- .mojor_call_bridge(
                "mojor_sigmoid_affine_f32_gpu_chain_int", .mojor_gpu_ctx_get(),
                data, iters, scale, bias, post_scale, post_bias, post_iters
            )
            label <- "gpu_chain_sigmoid_affine_chain"
        } else if (iters <= 1L && isTRUE(all.equal(scale, 1)) &&
            isTRUE(all.equal(bias, 0))) {
            .mojor_gpu_log(
                "gpu_chain_sigmoid", length(data),
                buffers = 4L, bytes_per = 4L
            )
            out_data <- .mojor_call_bridge("mojor_sigmoid_f32_gpu_int", .mojor_gpu_ctx_get(), data)
            label <- "gpu_chain_sigmoid"
        } else if (iters <= 1L) {
            .mojor_gpu_log(
                "gpu_chain_sigmoid_affine", length(data),
                buffers = 4L, bytes_per = 4L
            )
            out_data <- .mojor_call_bridge(
                "mojor_sigmoid_affine_f32_gpu_int", .mojor_gpu_ctx_get(),
                data, scale, bias
            )
            label <- "gpu_chain_sigmoid_affine"
        } else {
            .mojor_gpu_log(
                "gpu_chain_sigmoid_affine_iters", length(data),
                buffers = 4L, bytes_per = 4L
            )
            out_data <- .mojor_call_bridge(
                "mojor_sigmoid_affine_f32_gpu_iters_int", .mojor_gpu_ctx_get(),
                data, iters, scale, bias
            )
            label <- "gpu_chain_sigmoid_affine_iters"
        }
    }

    out <- .mojor_wrap_float32_gpu(out_data, label = label)
    attr(out, "gpu_op") <- op
    attr(out, "gpu_scale") <- scale
    attr(out, "gpu_bias") <- bias
    attr(out, "gpu_iters") <- iters
    attr(out, "gpu_post_scale") <- post_scale
    attr(out, "gpu_post_bias") <- post_bias
    attr(out, "gpu_post_iters") <- post_iters
    out
}

.mojor_options_defaults <- function() {
    list(
        na_mode = "forbid", simd_mode = "explicit", assume_aligned = NULL,
        warn_ifelse = TRUE, warn_constructor = TRUE, warn_parallel = TRUE,
        warn_recycle = FALSE, gpu_debug = FALSE, gpu_max_bytes = NULL,
        gpu_reject_fallback = FALSE,
        gpu_jit_mode = "auto", elementwise_cpu = FALSE, elementwise_gpu_layouttensor = NULL,
        sd_mode = "welford", jit_cache_max_bytes = NULL, jit_cache_max_entries = NULL,
        jit_cache_max_age_days = NULL, jit_disk_cache = TRUE, jit_strict_signatures = FALSE,
        jit_profile = NULL, index_bounds = FALSE, index_base = "one_based",
        array_layout = "col_major", fast_math = FALSE, fast_math_flag = NULL,
        ir_only = TRUE, opt_level = 2L, r_jit_level = 0L, fusion_allow_control_flow_simple = FALSE,
        fusion_allow_broadcast_nd_identity = FALSE
    )
}

.mojor_default_error_recovery_state <- function() {
    if (exists(
        ".mojor_init_error_recovery_state", mode = "function", inherits = TRUE
    )) {
        return(.mojor_init_error_recovery_state())
    }
    list(
        error_mode = "stop", max_retries = 3L, retry_delay = 0.1, retry_on = character(0),
        current_retry = 0L
    )
}

.mojor_state_scratch_defaults <- function() {
    list(
        diagnostics = list(), diagnostics_sink = list(), current_srcref = NULL,
        current_function_name = character(0),
        current_scalar_names = NULL, current_declared_scalars = NULL, current_local_types = list(),
        current_nrow_var_map = NULL, current_dim_var_map = NULL, current_ndim_var_map = NULL,
        current_ncol_var_map = NULL, current_out_name = NULL, current_out_nrow_var = NULL,
        current_out_ncol_var = NULL, current_out_dim_var = NULL, current_out_ndim_var = NULL,
        current_out_is_matrix = FALSE, current_args = NULL, current_arg_specs = NULL,
        current_tensor_map = NULL, current_tensor_ranks = NULL, current_broadcast_nd = FALSE,
        current_broadcast = NULL, current_parallel = FALSE, current_fusion = NULL,
        current_len_hint = list(), current_iter_map = list(), current_len_var_map = NULL,
        current_n_source_name = NULL, current_df_schema = NULL, current_constructor_mode = FALSE,
        current_local_matrix_dims = list(), current_local_array_dims = list(),
        current_local_vector_lengths = list(), current_const_array_vars = list(),
        current_typed_lir_payloads = list(),
        current_ir_emit_diagnostics = list(),
        current_scalar_inits = NULL, current_suppress_len_checks = FALSE,
        current_suppress_read_helpers = FALSE, current_index_context = FALSE,
        current_index_base = NULL, current_array_layout = NULL, current_ir_only = isTRUE(.mojor_options_defaults()$ir_only),
        current_na_mode = NULL, current_len_checks = list(), current_len_checks_c = list(),
        current_cumul_acc_counter = 0L, current_cumul_accs = list(), current_cumul_updates = list(),
        current_cumul_node_map = list(), needs_index_helpers = FALSE, needs_rng_tables = FALSE,
        needs_mojo_random = FALSE, needs_ffi = FALSE, needs_na_helpers = FALSE,
        needs_set_match = FALSE, needs_quantile = FALSE, recycle_warnings = list(),
        drop_warned = FALSE, matrix_dimnames = NULL, declared_c_functions = list(),
        loaded_wrapper_dlls = list(), error_recovery = .mojor_default_error_recovery_state(),
        last_rng_seed = NULL, last_rng_seed_token = 0L
    )
}

.mojor_state_scratch_fields <- function() {
    explicit <- names(.mojor_state_scratch_defaults())
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(explicit)
    }
    state_names <- ls(.mojor_state, all.names = TRUE)
    dynamic <- state_names[grepl("^(current_|needs_)", state_names)]
    extra <- c(
        "diagnostics", "diagnostics_sink", "recycle_warnings", "drop_warned",
        "matrix_dimnames", "declared_c_functions", "loaded_wrapper_dlls",
        "error_recovery", "last_rng_seed", "last_rng_seed_token"
    )
    unique(c(explicit, dynamic, extra))
}

.mojor_state_clone_value <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    unserialize(serialize(x, NULL, version = 2))
}

.mojor_state_snapshot <- function(fields = NULL) {
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(list())
    }
    if (is.null(fields)) {
        fields <- ls(.mojor_state, all.names = TRUE)
    }
    out <- list()
    for (nm in fields) {
        if (exists(nm, envir = .mojor_state, inherits = FALSE)) {
            out[[nm]] <- .mojor_state_clone_value(get(nm, envir = .mojor_state, inherits = FALSE))
        } else {
            out[[nm]] <- NULL
        }
    }
    out
}

.mojor_state_restore <- function(snapshot) {
    if (!is.list(snapshot)) {
        stop(".mojor_state_restore: snapshot must be a list")
    }
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        stop(".mojor_state_restore: .mojor_state is not initialized")
    }
    for (nm in names(snapshot)) {
        assign(
            nm, .mojor_state_clone_value(snapshot[[nm]]),
            envir = .mojor_state
        )
    }
    invisible(TRUE)
}

.mojor_prune_loaded_wrapper_registry <- function() {
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(FALSE))
    }
    regs <- .mojor_state$loaded_wrapper_dlls
    if (is.null(regs) ||
        length(regs) ==
            0) {
        .mojor_state$loaded_wrapper_dlls <- list()
        return(invisible(TRUE))
    }
    dlls <- getLoadedDLLs()
    keep <- list()
    for (entry in regs) {
        if (!is.list(entry) || is.null(entry$path))
            next
        entry_path <- entry$path
        if (!is.character(entry_path) ||
            length(entry_path) !=
                1L ||
            is.na(entry_path) ||
            !nzchar(entry_path))
            next
        path <- tryCatch(
            normalizePath(entry_path, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (is.na(path) || !nzchar(path))
            next
        pkg <- if (!is.null(entry$pkg))
            as.character(entry$pkg[[1]]) else NA_character_
        if (!is.na(pkg) &&
            nzchar(pkg) &&
            pkg %in% names(dlls)) {
            dll_path <- normalizePath(
                getElement(dlls[[pkg]], "path"),
                winslash = "/", mustWork = FALSE
            )
            if (identical(path, dll_path)) {
                keep[[length(keep) +
                  1L]] <- list(
                  pkg = pkg, path = path, kind = if (is.null(entry$kind)) "kernel" else as.character(entry$kind[[1]])
              )
            }
        }
    }
    .mojor_state$loaded_wrapper_dlls <- keep
    invisible(TRUE)
}

.mojor_register_loaded_wrapper <- function(pkg, path, kind = "kernel") {
    if (is.null(pkg) ||
        !is.character(pkg) ||
        length(pkg) !=
            1 || !nzchar(pkg)) {
        return(invisible(FALSE))
    }
    if (is.null(path) ||
        !is.character(path) ||
        length(path) !=
            1 || !nzchar(path)) {
        return(invisible(FALSE))
    }
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(FALSE))
    }
    .mojor_prune_loaded_wrapper_registry()
    regs <- .mojor_state$loaded_wrapper_dlls
    if (is.null(regs) ||
        !is.list(regs))
        regs <- list()
    loaded_path <- .mojor_find_loaded_dll_path(path = path, pkg = pkg)
    stored_path <- if (!is.null(loaded_path) &&
        is.character(loaded_path) &&
        length(loaded_path) == 1L &&
        nzchar(loaded_path)) {
        loaded_path
    } else {
        path
    }
    norm <- normalizePath(stored_path, winslash = "/", mustWork = FALSE)
    keep <- list()
    for (entry in regs) {
        if (!is.list(entry) || is.null(entry$path))
            next
        entry_path <- entry$path
        if (!is.character(entry_path) ||
            length(entry_path) !=
                1L ||
            is.na(entry_path) ||
            !nzchar(entry_path))
            next
        entry_norm <- tryCatch(
            normalizePath(entry_path, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (!is.character(entry_norm) ||
            length(entry_norm) !=
                1L ||
            is.na(entry_norm) ||
            !nzchar(entry_norm))
            next
        if (!identical(entry_norm, norm)) {
            keep[[length(keep) +
                1L]] <- entry
        }
    }
    keep[[length(keep) +
        1L]] <- list(pkg = pkg, path = stored_path, kind = kind)
    .mojor_state$loaded_wrapper_dlls <- keep
    .mojor_enforce_loaded_wrapper_limit(current_path = norm)
    invisible(TRUE)
}

.mojor_loaded_dll_paths <- function() {
    dlls <- getLoadedDLLs()
    if (length(dlls) == 0L) {
        return(character(0))
    }
    paths <- character(0)
    for (nm in names(dlls)) {
        path <- tryCatch(
            as.character(getElement(dlls[[nm]], "path")),
            error = function(e) NA_character_
        )
        if (!is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            !nzchar(path)) {
            next
        }
        norm <- tryCatch(
            normalizePath(path, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (!is.character(norm) ||
            length(norm) != 1L ||
            is.na(norm) ||
            !nzchar(norm)) {
            next
        }
        paths <- c(paths, norm)
    }
    unique(paths)
}

.mojor_wrapper_registry_paths <- function(regs = NULL) {
    if (is.null(regs)) {
        if (!exists(".mojor_state", inherits = TRUE) ||
            !is.environment(.mojor_state)) {
            return(character(0))
        }
        regs <- .mojor_state$loaded_wrapper_dlls
    }
    if (is.null(regs) ||
        !is.list(regs) ||
        length(regs) == 0L) {
        return(character(0))
    }
    out <- character(0)
    for (entry in regs) {
        if (!is.list(entry) || is.null(entry$path)) {
            next
        }
        path <- entry$path
        if (!is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            !nzchar(path)) {
            next
        }
        norm <- tryCatch(
            normalizePath(path, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (!is.character(norm) ||
            length(norm) != 1L ||
            is.na(norm) ||
            !nzchar(norm)) {
            next
        }
        out <- c(out, norm)
    }
    unique(out)
}

.mojor_wrapper_leak_snapshot <- function(prune = TRUE) {
    if (isTRUE(prune) &&
        exists(".mojor_prune_loaded_wrapper_registry", mode = "function", inherits = TRUE)) {
        .mojor_prune_loaded_wrapper_registry()
    }
    regs <- if (exists(".mojor_state", inherits = TRUE) &&
        is.environment(.mojor_state)) {
        .mojor_state$loaded_wrapper_dlls
    } else {
        list()
    }
    if (is.null(regs) || !is.list(regs)) {
        regs <- list()
    }
    reg_paths <- .mojor_wrapper_registry_paths(regs)
    loaded_paths <- .mojor_loaded_dll_paths()
    tracked_paths <- intersect(reg_paths, loaded_paths)

    list(
        timestamp = as.numeric(Sys.time()),
        registered_count = as.integer(length(regs)),
        registered_unique_paths = as.integer(length(reg_paths)),
        loaded_dll_count = as.integer(length(loaded_paths)),
        tracked_loaded_dll_count = as.integer(length(tracked_paths))
    )
}

.mojor_wrapper_leak_delta <- function(snapshot, baseline = NULL) {
    metric <- function(x, name) {
        if (!is.list(x) || is.null(x[[name]])) {
            return(0L)
        }
        val <- suppressWarnings(as.integer(x[[name]][[1L]]))
        if (!is.finite(val) || is.na(val)) {
            return(0L)
        }
        as.integer(val)
    }
    if (is.null(baseline)) {
        baseline <- list()
    }
    list(
        registered_unique_paths = metric(snapshot, "registered_unique_paths") -
            metric(baseline, "registered_unique_paths"),
        loaded_dll_count = metric(snapshot, "loaded_dll_count") -
            metric(baseline, "loaded_dll_count"),
        tracked_loaded_dll_count = metric(snapshot, "tracked_loaded_dll_count") -
            metric(baseline, "tracked_loaded_dll_count")
    )
}

.mojor_max_loaded_wrappers <- function() {
    raw <- Sys.getenv("MOJOR_MAX_LOADED_WRAPPERS", unset = "64")
    if (tolower(raw) %in% c("0", "off", "false", "no", "none")) {
        return(0L)
    }
    limit <- suppressWarnings(as.integer(raw))
    if (is.na(limit) || limit < 1L) {
        return(0L)
    }
    limit
}

.mojor_unload_wrapper_entry <- function(entry) {
    if (!is.list(entry)) {
        return(NA_character_)
    }

    pkg <- if (!is.null(entry$pkg))
        as.character(entry$pkg[[1]]) else NA_character_
    path <- if (!is.null(entry$path))
        as.character(entry$path[[1]]) else NA_character_

    candidates <- character(0)
    dlls <- getLoadedDLLs()
    if (!is.na(pkg) &&
        nzchar(pkg) &&
        pkg %in% names(dlls)) {
        dll_path <- tryCatch(
            as.character(getElement(dlls[[pkg]], "path")),
            error = function(e) NA_character_
        )
        if (is.character(dll_path) &&
            length(dll_path) == 1L &&
            !is.na(dll_path) &&
            nzchar(dll_path)) {
            candidates <- c(candidates, dll_path)
        }
    }
    if (is.character(path) &&
        length(path) == 1L &&
        !is.na(path) &&
        nzchar(path)) {
        candidates <- c(candidates, path)
        norm <- tryCatch(
            normalizePath(path, winslash = "/", mustWork = FALSE),
            error = function(e) NA_character_
        )
        if (is.character(norm) &&
            length(norm) == 1L &&
            !is.na(norm) &&
            nzchar(norm)) {
            candidates <- c(candidates, norm)
        }
    }
    candidates <- unique(candidates)
    if (length(candidates) == 0L) {
        return(NA_character_)
    }

    for (cand in candidates) {
        ok <- tryCatch(
            {
                dyn.unload(cand)
                TRUE
            },
            error = function(e) FALSE
        )
        if (isTRUE(ok)) {
            return(cand)
        }
    }
    NA_character_
}

.mojor_enforce_loaded_wrapper_limit <- function(current_path = NULL) {
    if (!exists(".mojor_state", envir = .GlobalEnv, inherits = FALSE) ||
        !is.environment(.mojor_state)) {
        return(invisible(character(0)))
    }
    limit <- .mojor_max_loaded_wrappers()
    if (limit < 1L) {
        return(invisible(character(0)))
    }
    .mojor_prune_loaded_wrapper_registry()
    regs <- .mojor_state$loaded_wrapper_dlls
    if (!is.list(regs) ||
        length(regs) <=
            limit) {
        return(invisible(character(0)))
    }
    current_norm <- NULL
    if (!is.null(current_path) &&
        is.character(current_path) &&
        length(current_path) ==
            1L &&
        nzchar(current_path)) {
        current_norm <- tryCatch(
            normalizePath(current_path, winslash = "/", mustWork = FALSE),
            error = function(e) NULL
        )
    }
    removed <- character(0)
    keep <- regs
    for (entry in regs) {
        if (length(keep) <= limit) {
            break
        }
        if (!is.list(entry) ||
            is.null(entry$path)) {
            next
        }
        entry_path <- entry$path[[1]]
        if (!is.character(entry_path) ||
            !nzchar(entry_path)) {
            next
        }
        entry_norm <- tryCatch(
            normalizePath(entry_path, winslash = "/", mustWork = FALSE),
            error = function(e) NULL
        )
        if (is.null(entry_norm) ||
            !nzchar(entry_norm)) {
            next
        }
        if (!is.null(current_norm) &&
            identical(entry_norm, current_norm)) {
            next
        }
        entry_pkg <- if (!is.null(entry$pkg)) {
            as.character(entry$pkg[[1]])
        } else {
            NA_character_
        }
        unload_path <- .mojor_find_loaded_dll_path(
            path = entry_norm,
            pkg = entry_pkg
        )
        if (is.null(unload_path) ||
            !is.character(unload_path) ||
            length(unload_path) != 1L ||
            !nzchar(unload_path)) {
            unload_path <- entry_path
        }
        ok <- tryCatch(
            {
                dyn.unload(unload_path)
                TRUE
            },
            error = function(e) FALSE
        )
        if (!isTRUE(ok)) {
            next
        }
        removed <- c(removed, entry_norm)
        keep <- Filter(
            f = function(x) {
                if (!is.list(x) || is.null(x$path)) {
                    return(FALSE)
                }
                x_path <- x$path[[1]]
                if (!is.character(x_path) || !nzchar(x_path)) {
                    return(FALSE)
                }
                x_norm <- tryCatch(
                    normalizePath(x_path, winslash = "/", mustWork = FALSE),
                    error = function(e) NULL
                )
                !identical(x_norm, entry_norm)
            },
            x = keep
        )
    }
    .mojor_state$loaded_wrapper_dlls <- keep
    .mojor_prune_loaded_wrapper_registry()
    invisible(unique(removed))
}

.mojor_unload_tracked_wrappers <- function() {
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(character(0)))
    }
    .mojor_prune_loaded_wrapper_registry()
    regs <- .mojor_state$loaded_wrapper_dlls
    if (is.null(regs) ||
        length(regs) ==
            0) {
        .mojor_state$loaded_wrapper_dlls <- list()
        return(invisible(character(0)))
    }
    unloaded <- character(0)
    if (length(regs) >
        0) {
        seen <- new.env(parent = emptyenv())
        for (entry in rev(regs)) {
            key <- paste0(
                if (!is.null(entry$pkg))
                    as.character(entry$pkg[[1]]) else "",
                "::",
                if (!is.null(entry$path))
                    as.character(entry$path[[1]]) else ""
            )
            if (exists(key, envir = seen, inherits = FALSE)) {
                next
            }
            assign(key, TRUE, envir = seen)
            unloaded_path <- .mojor_unload_wrapper_entry(entry)
            if (is.character(unloaded_path) &&
                length(unloaded_path) == 1L &&
                !is.na(unloaded_path) &&
                nzchar(unloaded_path))
                unloaded <- c(unloaded, unloaded_path)
        }
    }
    .mojor_prune_loaded_wrapper_registry()
    invisible(unloaded)
}

.mojor_is_dll_limit_error <- function(err) {
    if (is.null(err)) {
        return(FALSE)
    }
    msg <- conditionMessage(err)
    is.character(msg) &&
        length(msg) == 1L &&
        grepl("maximal number of DLLs reached", msg, fixed = TRUE)
}

.mojor_dyn_load_with_recovery <- function(
    so_path,
    lib_dir = NULL,
    local = TRUE,
    retry_on_limit = TRUE
) {
    load_once <- function() {
        if (!is.null(lib_dir) &&
            is.character(lib_dir) &&
            length(lib_dir) == 1L &&
            nzchar(lib_dir)) {
            return(.mojor_with_ld_library_path(lib_dir, function() dyn.load(so_path, local = local)))
        }
        dyn.load(so_path, local = local)
    }

    tryCatch(
        load_once(),
        error = function(e) {
            if (!isTRUE(retry_on_limit) ||
                !isTRUE(.mojor_is_dll_limit_error(e))) {
                stop(e)
            }
            .mojor_unload_tracked_wrappers()
            load_once()
        }
    )
}

.mojor_test_sensitive_env_vars <- function() {
    c(
        "MOJOR_PARALLEL_RUNTIME", "MOJOR_GPU_F64_PROBE_MODE", "MOJOR_GPU_F64_PROBE_MAX_SHAPES",
        "MOJOR_GPU_F64_PROBE_TIMEOUT_SEC"
    )
}

.mojor_configure_test_baseline <- function(options = list(), env = list()) {
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(FALSE))
    }
    if (!is.list(options))
        options <- list()
    if (!is.list(env))
        env <- list()
    .mojor_state$test_baseline_options <- options
    .mojor_state$test_baseline_env <- env
    invisible(TRUE)
}

.mojor_test_restore_function_bindings <- function() {
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(FALSE))
    }
    baseline <- .mojor_state$test_function_baseline
    if (!is.list(baseline) || length(baseline) == 0L) {
        return(invisible(FALSE))
    }
    for (nm in names(baseline)) {
        fn <- baseline[[nm]]
        if (!is.function(fn)) {
            next
        }
        if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
            cur <- get(nm, envir = .GlobalEnv, inherits = FALSE)
            if (identical(cur, fn)) {
                next
            }
        }
        assign(nm, fn, envir = .GlobalEnv)
    }
    invisible(TRUE)
}

.mojor_test_reset <- function(
    mode = c("soft", "hard"),
    keep_backend = TRUE
) {
    mode <- match.arg(mode)
    if (!exists(".mojor_state", inherits = TRUE) ||
        !is.environment(.mojor_state)) {
        return(invisible(FALSE))
    }
    keep_backend <- isTRUE(keep_backend)
    pre_reset_wrappers <- .mojor_state$loaded_wrapper_dlls
    preserved <- list(
        bridge_pkg = .mojor_state$bridge_pkg, bridge_path = .mojor_state$bridge_path,
        gpu_ctx = .mojor_state$gpu_ctx, gpu_ctx_epoch = .mojor_state$gpu_ctx_epoch
    )
    defaults <- .mojor_state_scratch_defaults()
    for (nm in names(defaults)) {
        assign(
            nm, .mojor_state_clone_value(defaults[[nm]]),
            envir = .mojor_state
        )
    }
    for (nm in setdiff(.mojor_state_scratch_fields(), names(defaults))) {
        assign(nm, NULL, envir = .mojor_state)
    }
    .mojor_state$options <- .mojor_options_defaults()
    baseline_opts <- .mojor_state$test_baseline_options
    if (is.list(baseline_opts) &&
        length(baseline_opts) >
            0) {
        known <- intersect(
            names(baseline_opts),
            names(.mojor_state$options)
        )
        if (length(known) >
            0) {
            do.call(mojor_options, baseline_opts[known])
        }
    }
    baseline_env <- .mojor_state$test_baseline_env
    if (!is.list(baseline_env))
        baseline_env <- list()
    env_vars <- unique(c(.mojor_test_sensitive_env_vars(), names(baseline_env)))
    for (var in env_vars) {
        val <- baseline_env[[var]]
        if (is.null(val) ||
            length(val) ==
                0 || is.na(val)) {
            Sys.unsetenv(var)
        } else {
            do.call(
                Sys.setenv, stats::setNames(
                  list(as.character(val[[1]])),
                  var
              )
            )
        }
    }
    if (keep_backend) {
        .mojor_state$bridge_pkg <- preserved$bridge_pkg
        .mojor_state$bridge_path <- preserved$bridge_path
        .mojor_state$gpu_ctx <- preserved$gpu_ctx
        .mojor_state$gpu_ctx_epoch <- preserved$gpu_ctx_epoch
    } else {
        .mojor_state$bridge_pkg <- NULL
        .mojor_state$bridge_path <- NULL
        .mojor_state$gpu_ctx <- NULL
        .mojor_state$gpu_ctx_epoch <- 0L
    }
    if (identical(mode, "hard")) {
        .mojor_unload_tracked_wrappers()
        .mojor_state$loaded_wrapper_dlls <- list()
        if (exists("mojor_jit_clear_cache", mode = "function", inherits = TRUE)) {
            try(
                mojor_jit_clear_cache(remove_builds = FALSE),
                silent = TRUE
            )
        }
    } else {
        # Soft reset is non-destructive by default: avoid automatic wrapper
        # unloads during routine test cycles. Opt-in aggressive unloading can
        # be enabled for manual crash triage.
        regs <- if (is.list(pre_reset_wrappers))
            pre_reset_wrappers else list()
        pre_regs_count <- length(regs)
        .mojor_state$loaded_wrapper_dlls <- regs
        .mojor_prune_loaded_wrapper_registry()

        aggressive_soft_unload <- tolower(
            Sys.getenv("MOJOR_SOFT_RESET_AGGRESSIVE_UNLOAD", unset = "false")
        ) %in% c("1", "true", "yes", "y", "on")
        if (isTRUE(aggressive_soft_unload)) {
            soft_limit <- suppressWarnings(
                as.integer(Sys.getenv("MOJOR_SOFT_RESET_MAX_WRAPPERS", unset = "128"))
            )
            if (is.na(soft_limit) || soft_limit < 1L) {
                soft_limit <- 128L
            }
            regs <- .mojor_state$loaded_wrapper_dlls
            effective_count <- max(pre_regs_count, length(regs))
            if (is.list(regs) && effective_count >= soft_limit) {
                .mojor_state$loaded_wrapper_dlls <- regs
                .mojor_unload_tracked_wrappers()
                .mojor_state$loaded_wrapper_dlls <- list()
            }
        }
    }
    .mojor_test_restore_function_bindings()
    invisible(TRUE)
}

.mojor_prev_bridge_pkg <- NULL
.mojor_prev_bridge_path <- NULL
.mojor_prev_gpu_ctx <- NULL
.mojor_prev_gpu_ctx_epoch <- 0L
if (exists(".mojor_state", inherits = FALSE) &&
    is.environment(.mojor_state)) {
    .mojor_prev_bridge_pkg <- .mojor_state$bridge_pkg
    .mojor_prev_bridge_path <- .mojor_state$bridge_path
    .mojor_prev_gpu_ctx <- .mojor_state$gpu_ctx
    .mojor_prev_gpu_ctx_epoch <- .mojor_state$gpu_ctx_epoch
}

.mojor_state <- new.env(parent = emptyenv())

.mojor_state$options <- .mojor_options_defaults()

.mojor_state$mojo_flag_cache <- list()
.mojor_state$gpu_capability_cache <- list()
.mojor_state$runtime_v1_symbol_cache <- list()
.mojor_state$runtime_deterministic_symbol_cache <- list()
.mojor_state$runtime_env_cache <- list()
.mojor_state$fast_math_warned <- FALSE
.mojor_state$runtime_legacy_warned <- FALSE
.mojor_state$gpu_ctx <- if (inherits(.mojor_prev_gpu_ctx, "mojor_gpu_ctx")) .mojor_prev_gpu_ctx else NULL
.mojor_state$gpu_ctx_epoch <- if (is.numeric(.mojor_prev_gpu_ctx_epoch) &&
    length(.mojor_prev_gpu_ctx_epoch) ==
        1L && !is.na(.mojor_prev_gpu_ctx_epoch)) as.integer(.mojor_prev_gpu_ctx_epoch) else 0L
.mojor_state$bridge_pkg <- .mojor_prev_bridge_pkg
.mojor_state$bridge_path <- .mojor_prev_bridge_path
.mojor_state$loaded_wrapper_dlls <- list()
.mojor_state$test_baseline_options <- list()
.mojor_state$test_baseline_env <- list()
.mojor_state$transpile_version <- "2026-02-24-loop-index-hardening-v4"
.mojor_state$api_surface_version <- "2026-Q1"

.mojor_scratch_defaults <- .mojor_state_scratch_defaults()
for (nm in names(.mojor_scratch_defaults)) {
    assign(
        nm, .mojor_state_clone_value(.mojor_scratch_defaults[[nm]]),
        envir = .mojor_state
    )
}
.mojor_state$current_ir_only <- isTRUE(.mojor_state$options$ir_only)
rm(.mojor_scratch_defaults)

.mojor_state$`%||%` <- function(x, y) if (is.null(x) ||
    length(x) ==
        0) y else x
.mojor_set_option <- function(name, value) {
    .mojor_state$options[name] <- list(value)
}

.mojor_apply_jit_profile <- function(profile) {
    if (is.null(profile)) {
        return(invisible(NULL))
    }
    profile <- match.arg(profile, c("dev", "bench", "prod"))
    bundle <- switch(
        profile,
        dev = list(
            jit_disk_cache = TRUE,
            jit_strict_signatures = FALSE,
            r_jit_level = 0L
        ),
        bench = list(
            jit_disk_cache = TRUE,
            jit_strict_signatures = TRUE,
            r_jit_level = 0L
        ),
        prod = list(
            jit_disk_cache = TRUE,
            jit_strict_signatures = TRUE,
            r_jit_level = 0L
        )
    )
    for (nm in names(bundle)) {
        .mojor_set_option(nm, bundle[[nm]])
    }
    invisible(bundle)
}

.mojor_add_recycle_warning <- function(entry) {
    .mojor_state$recycle_warnings <- c(.mojor_state$recycle_warnings, list(entry))
}

mojor_options <- function(...) {
    opts <- list(...)
    if (length(opts) ==
        0) {
        return(.mojor_state$options)
    }

    if (is.null(names(opts)) ||
        all(
            names(opts) ==
                ""
        )) {
        if (length(opts) !=
            1 || !is.character(opts[[1]])) {
            stop(
                "mojor_options: unnamed arguments must be a character vector of option names"
            )
        }
        keys <- opts[[1]]
        out <- .mojor_state$options[keys]
        return(out)
    }

    unknown <- setdiff(
        names(opts),
        names(.mojor_state$options)
    )
    if (length(unknown) >
        0) {
        stop(
            "mojor_options: unknown option(s): ", paste(unknown, collapse = ", ")
        )
    }

    profile_old_keys <- character(0)
    if ("jit_profile" %in% names(opts) &&
        !is.null(opts$jit_profile)) {
        profile_old_keys <- c(
            "jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"
        )
    }
    old <- .mojor_state$options[unique(c(names(opts), profile_old_keys))]
    if ("jit_profile" %in% names(opts)) {
        if (is.null(opts$jit_profile)) {
            .mojor_set_option("jit_profile", NULL)
        } else {
            if (!is.character(opts$jit_profile) ||
                length(opts$jit_profile) !=
                    1 || !nzchar(opts$jit_profile)) {
                stop("mojor_options: jit_profile must be NULL or one of dev, bench, prod")
            }
            profile <- match.arg(opts$jit_profile, c("dev", "bench", "prod"))
            .mojor_set_option("jit_profile", profile)
            .mojor_apply_jit_profile(profile)
        }
    }
    if (!is.null(opts$na_mode)) {
        mode <- match.arg(opts$na_mode, c("forbid", "unsafe", "propagate", "na_rm"))
        .mojor_state$options$na_mode <- mode
    }
    if (!is.null(opts$simd_mode)) {
        mode <- match.arg(opts$simd_mode, c("explicit", "auto"))
        .mojor_state$options$simd_mode <- mode
    }
    if ("assume_aligned" %in% names(opts)) {
        if (is.null(opts$assume_aligned)) {
            .mojor_set_option("assume_aligned", NULL)
        } else {
            a <- as.integer(opts$assume_aligned)
            if (is.na(a) ||
                a <= 0) {
                stop("mojor_options: assume_aligned must be a positive integer or NULL")
            }
            .mojor_set_option("assume_aligned", a)
        }
    }
    if ("warn_ifelse" %in% names(opts)) {
        if (!is.logical(opts$warn_ifelse) ||
            length(opts$warn_ifelse) !=
                1) {
            stop("mojor_options: warn_ifelse must be TRUE or FALSE")
        }
        .mojor_state$options$warn_ifelse <- isTRUE(opts$warn_ifelse)
    }
    if ("warn_constructor" %in% names(opts)) {
        if (!is.logical(opts$warn_constructor) ||
            length(opts$warn_constructor) !=
                1) {
            stop("mojor_options: warn_constructor must be TRUE or FALSE")
        }
        .mojor_state$options$warn_constructor <- isTRUE(opts$warn_constructor)
    }
    if ("warn_parallel" %in% names(opts)) {
        if (!is.logical(opts$warn_parallel) ||
            length(opts$warn_parallel) !=
                1) {
            stop("mojor_options: warn_parallel must be TRUE or FALSE")
        }
        .mojor_state$options$warn_parallel <- isTRUE(opts$warn_parallel)
    }
    if ("warn_recycle" %in% names(opts)) {
        if (!is.logical(opts$warn_recycle) ||
            length(opts$warn_recycle) !=
                1) {
            stop("mojor_options: warn_recycle must be TRUE or FALSE")
        }
        .mojor_state$options$warn_recycle <- isTRUE(opts$warn_recycle)
    }
    if ("gpu_debug" %in% names(opts)) {
        if (!is.logical(opts$gpu_debug) ||
            length(opts$gpu_debug) !=
                1) {
            stop("mojor_options: gpu_debug must be TRUE or FALSE")
        }
        .mojor_state$options$gpu_debug <- isTRUE(opts$gpu_debug)
    }
    if ("gpu_reject_fallback" %in% names(opts)) {
        if (!is.logical(opts$gpu_reject_fallback) ||
            length(opts$gpu_reject_fallback) !=
                1 || is.na(opts$gpu_reject_fallback)) {
            stop("mojor_options: gpu_reject_fallback must be TRUE or FALSE")
        }
        .mojor_state$options$gpu_reject_fallback <- isTRUE(opts$gpu_reject_fallback)
    }
    if ("gpu_max_bytes" %in% names(opts)) {
        if (is.null(opts$gpu_max_bytes)) {
            .mojor_set_option("gpu_max_bytes", NULL)
        } else {
            val <- suppressWarnings(as.double(opts$gpu_max_bytes))
            if (is.na(val) ||
                val <= 0) {
                stop("mojor_options: gpu_max_bytes must be NULL or a positive number")
            }
            .mojor_set_option("gpu_max_bytes", val)
        }
    }
    if ("gpu_jit_mode" %in% names(opts)) {
        mode <- match.arg(opts$gpu_jit_mode, c("auto", "helper_shim", "unified_preview"))
        .mojor_state$options$gpu_jit_mode <- mode
    }
    if ("elementwise_cpu" %in% names(opts)) {
        if (!is.logical(opts$elementwise_cpu) ||
            length(opts$elementwise_cpu) !=
                1) {
            stop("mojor_options: elementwise_cpu must be TRUE or FALSE")
        }
        .mojor_state$options$elementwise_cpu <- isTRUE(opts$elementwise_cpu)
    }
    if ("elementwise_gpu_layouttensor" %in% names(opts)) {
        if (is.null(opts$elementwise_gpu_layouttensor)) {
            .mojor_state$options$elementwise_gpu_layouttensor <- NULL
        } else {
            if (!is.logical(opts$elementwise_gpu_layouttensor) ||
                length(opts$elementwise_gpu_layouttensor) !=
                  1) {
                stop(
                  "mojor_options: elementwise_gpu_layouttensor must be TRUE, FALSE, or NULL"
              )
            }
            .mojor_state$options$elementwise_gpu_layouttensor <- isTRUE(opts$elementwise_gpu_layouttensor)
        }
    }
    if ("index_bounds" %in% names(opts)) {
        if (!is.logical(opts$index_bounds) ||
            length(opts$index_bounds) !=
                1) {
            stop("mojor_options: index_bounds must be TRUE or FALSE")
        }
        .mojor_state$options$index_bounds <- isTRUE(opts$index_bounds)
    }
    if (!is.null(opts$index_base)) {
        base <- match.arg(opts$index_base, c("one_based", "zero_based"))
        .mojor_state$options$index_base <- base
    }
    if (!is.null(opts$array_layout)) {
        layout <- match.arg(opts$array_layout, c("col_major", "row_major"))
        .mojor_state$options$array_layout <- layout
    }
    if ("ir_only" %in% names(opts)) {
        if (!is.logical(opts$ir_only) ||
            length(opts$ir_only) !=
                1) {
            stop("mojor_options: ir_only must be TRUE or FALSE")
        }
        .mojor_state$options$ir_only <- isTRUE(opts$ir_only)
    }
    if ("opt_level" %in% names(opts)) {
        if (is.null(opts$opt_level)) {
            .mojor_set_option("opt_level", NULL)
        } else {
            val <- suppressWarnings(as.integer(opts$opt_level))
            if (is.na(val) ||
                val < 0 || val > 3) {
                stop(
                  "mojor_options: opt_level must be an integer between 0 and 3 (or NULL)"
              )
            }
            .mojor_set_option("opt_level", val)
        }
    }
    if ("r_jit_level" %in% names(opts)) {
        val <- suppressWarnings(as.integer(opts$r_jit_level))
        if (length(val) !=
            1 || is.na(val) || val < 0 || val > 3) {
            stop("mojor_options: r_jit_level must be an integer between 0 and 3")
        }
        .mojor_set_option("r_jit_level", val)
    }
    if ("jit_disk_cache" %in% names(opts)) {
        if (is.null(opts$jit_disk_cache)) {
            .mojor_set_option("jit_disk_cache", NULL)
        } else {
            if (!is.logical(opts$jit_disk_cache) ||
                length(opts$jit_disk_cache) !=
                    1 || is.na(opts$jit_disk_cache)) {
                stop("mojor_options: jit_disk_cache must be TRUE, FALSE, or NULL")
            }
            .mojor_set_option("jit_disk_cache", isTRUE(opts$jit_disk_cache))
        }
    }
    if ("jit_strict_signatures" %in% names(opts)) {
        if (!is.logical(opts$jit_strict_signatures) ||
            length(opts$jit_strict_signatures) !=
                1 || is.na(opts$jit_strict_signatures)) {
            stop("mojor_options: jit_strict_signatures must be TRUE or FALSE")
        }
        .mojor_set_option("jit_strict_signatures", isTRUE(opts$jit_strict_signatures))
    }
    if ("fusion_allow_control_flow_simple" %in% names(opts)) {
        if (!is.logical(opts$fusion_allow_control_flow_simple) ||
            length(opts$fusion_allow_control_flow_simple) !=
                1) {
            stop(
                "mojor_options: fusion_allow_control_flow_simple must be TRUE or FALSE"
            )
        }
        .mojor_state$options$fusion_allow_control_flow_simple <- isTRUE(opts$fusion_allow_control_flow_simple)
    }
    if ("fusion_allow_broadcast_nd_identity" %in% names(opts)) {
        if (!is.logical(opts$fusion_allow_broadcast_nd_identity) ||
            length(opts$fusion_allow_broadcast_nd_identity) !=
                1) {
            stop(
                "mojor_options: fusion_allow_broadcast_nd_identity must be TRUE or FALSE"
            )
        }
        .mojor_state$options$fusion_allow_broadcast_nd_identity <- isTRUE(opts$fusion_allow_broadcast_nd_identity)
    }
    if ("fast_math" %in% names(opts)) {
        if (!is.logical(opts$fast_math) ||
            length(opts$fast_math) !=
                1) {
            stop("mojor_options: fast_math must be TRUE or FALSE")
        }
        .mojor_state$options$fast_math <- isTRUE(opts$fast_math)
    }
    if ("fast_math_flag" %in% names(opts)) {
        if (is.null(opts$fast_math_flag) ||
            (is.character(opts$fast_math_flag) &&
                !nzchar(opts$fast_math_flag))) {
            .mojor_set_option("fast_math_flag", NULL)
        } else if (!is.character(opts$fast_math_flag) ||
            length(opts$fast_math_flag) !=
                1) {
            stop("mojor_options: fast_math_flag must be NULL or a single string")
        } else {
            .mojor_set_option("fast_math_flag", opts$fast_math_flag)
        }
    }
    if (!is.null(opts$sd_mode)) {
        mode <- match.arg(opts$sd_mode, c("welford", "two_pass"))
        .mojor_state$options$sd_mode <- mode
    }
    if ("jit_cache_max_bytes" %in% names(opts)) {
        if (!is.null(opts$jit_cache_max_bytes) &&
            (!is.numeric(opts$jit_cache_max_bytes) ||
                opts$jit_cache_max_bytes <= 0)) {
            stop(
                "mojor_options: jit_cache_max_bytes must be NULL or a positive number"
            )
        }
        .mojor_set_option("jit_cache_max_bytes", opts$jit_cache_max_bytes)
    }
    if ("jit_cache_max_entries" %in% names(opts)) {
        if (!is.null(opts$jit_cache_max_entries) &&
            (!is.numeric(opts$jit_cache_max_entries) ||
                opts$jit_cache_max_entries <= 0)) {
            stop(
                "mojor_options: jit_cache_max_entries must be NULL or a positive number"
            )
        }
        .mojor_set_option("jit_cache_max_entries", as.integer(opts$jit_cache_max_entries))
    }
    if ("jit_cache_max_age_days" %in% names(opts)) {
        if (!is.null(opts$jit_cache_max_age_days) &&
            (!is.numeric(opts$jit_cache_max_age_days) ||
                opts$jit_cache_max_age_days <= 0)) {
            stop(
                "mojor_options: jit_cache_max_age_days must be NULL or a positive number"
            )
        }
        .mojor_set_option("jit_cache_max_age_days", opts$jit_cache_max_age_days)
    }
    invisible(old)
}

.mojor_effective_na_mode <- function(na_mode = NULL) {
    mode <- if (is.null(na_mode))
        .mojor_state$`%||%`(.mojor_state$options$na_mode, "forbid") else na_mode
    match.arg(mode, c("forbid", "unsafe", "assign", "propagate", "na_rm"))
}

.mojor_effective_r_jit_level <- function(r_jit_level = NULL, context = "mojor_build") {
    val <- if (is.null(r_jit_level))
        .mojor_state$`%||%`(.mojor_state$options$r_jit_level, 0L) else r_jit_level
    val <- suppressWarnings(as.integer(val))
    if (length(val) !=
        1 || is.na(val) || val < 0 || val > 3) {
        stop(context, ": r_jit_level must be an integer between 0 and 3")
    }
    val
}

.mojor_effective_jit_disk_cache <- function(disk_cache = NULL, context = "mojor_jit") {
    val <- if (is.null(disk_cache))
        .mojor_state$`%||%`(.mojor_state$options$jit_disk_cache, TRUE) else disk_cache
    if (!is.logical(val) ||
        length(val) !=
            1 || is.na(val)) {
        stop(context, ": disk_cache must be TRUE, FALSE, or NULL")
    }
    isTRUE(val)
}

.mojor_effective_jit_strict_signatures <- function(strict_signatures = NULL, context = "mojor_jit") {
    val <- if (is.null(strict_signatures))
        .mojor_state$`%||%`(.mojor_state$options$jit_strict_signatures, FALSE) else strict_signatures
    if (!is.logical(val) ||
        length(val) !=
            1 || is.na(val)) {
        stop(context, ": strict_signatures must be TRUE, FALSE, or NULL")
    }
    isTRUE(val)
}

.mojor_with_r_jit_level <- function(level, expr) {
    if (!requireNamespace("compiler", quietly = TRUE)) {
        return(force(expr))
    }
    old <- compiler::enableJIT(level)
    on.exit(invisible(compiler::enableJIT(old)), add = TRUE)
    force(expr)
}

.mojor_has_na_value <- function(val, spec) {
    if (is.null(spec)) {
        return(FALSE)
    }
    if (spec %in% c("lgl[]", "bool[]")) {
 # Allow NA in logical arrays (mask semantics handle NA as
 # 'skip')
        return(FALSE)
    }
    if (spec %in% c("f64", "f64[]", "i32", "i32[]", "lgl", "bool")) {
        return(anyNA(val))
    }
    if (spec %in% c("f32", "f32[]")) {
        if (!methods::is(val, "float32")) {
            return(FALSE)
        }
        dbl <- tryCatch(
            as.double(val),
            error = function(e) NULL
        )
        if (is.null(dbl)) {
            return(FALSE)
        }
        return(anyNA(dbl))
    }
    FALSE
}

.mojor_check_no_na <- function(val, spec, name = "argument") {
    if (.mojor_has_na_value(val, spec)) {
        stop(
            "mojor: NA/NaN not supported in compiled path (na_mode='forbid'): ",
            name, call. = FALSE
        )
    }
    invisible(TRUE)
}

.mojor_check_no_na_args <- function(args, arg_specs, arg_names) {
    if (length(arg_names) ==
        0) {
        return(invisible(TRUE))
    }
    arg_labels <- names(args)
    if (is.null(arg_labels)) {
        arg_labels <- rep("", length(args))
    } else if (length(arg_labels) < length(args)) {
        arg_labels <- c(arg_labels, rep("", length(args) - length(arg_labels)))
    }
    for (i in seq_along(arg_names)) {
        nm <- arg_names[[i]]
        spec <- arg_specs[[nm]]
        if (is.null(spec))
            next
        val <- NULL
        if (i <= length(args)) {
            arg_i <- arg_labels[[i]]
            if (!is.na(arg_i) && nzchar(arg_i) &&
                arg_i == nm) {
                val <- args[[i]]
            }
        }
        if (is.null(val) && nm %in% arg_labels) {
            val <- args[[nm]]
        } else if (is.null(val) && i <= length(args)) {
            val <- args[[i]]
        }
        if (is.null(val))
            next
        .mojor_check_no_na(val, spec, nm)
    }
    invisible(TRUE)
}

.mojor_check_no_na_reduction_arg <- function(
    args, arg_specs, arg_names, reduction_op = NULL, reduction_arg = NULL
) {
    if (is.null(reduction_op) ||
        !is.character(reduction_op) ||
        length(reduction_op) !=
            1) {
        return(invisible(TRUE))
    }
    if (!reduction_op %in% c(
        "sum", "product", "mean", "var", "sd",
        "min", "max", "which.min", "which.max"
    )) {
        return(invisible(TRUE))
    }
    if (is.null(reduction_arg) ||
        !is.character(reduction_arg) ||
        length(reduction_arg) !=
            1) {
        return(invisible(TRUE))
    }
    spec <- arg_specs[[reduction_arg]]
    if (is.null(spec) ||
        !spec %in% c("lgl[]", "bool[]")) {
        return(invisible(TRUE))
    }

    val <- NULL
    if (!is.null(names(args)) &&
        reduction_arg %in% names(args)) {
        val <- args[[reduction_arg]]
    } else {
        idx <- match(reduction_arg, arg_names, nomatch = 0L)
        if (idx > 0L && idx <= length(args)) {
            val <- args[[idx]]
        }
    }
    if (is.null(val)) {
        return(invisible(TRUE))
    }

    if (anyNA(val)) {
        stop(
            "mojor: NA/NaN not supported in compiled path (na_mode='forbid'): ",
            reduction_arg, call. = FALSE
        )
    }
    invisible(TRUE)
}
