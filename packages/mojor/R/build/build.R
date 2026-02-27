.mojor_is_object_mode_fallback_error <- function(err_msg) {
    if (!is.character(err_msg) ||
        length(err_msg) !=
            1 || !nzchar(err_msg)) {
        return(FALSE)
    }
    msg <- tolower(err_msg)
    if (grepl("unsupported type spec", msg, fixed = TRUE)) {
        return(FALSE)
    }
    if (grepl("unsupported scalar type", msg, fixed = TRUE)) {
        return(FALSE)
    }
    if (grepl("provide type annotations", msg, fixed = TRUE)) {
        return(FALSE)
    }
    if (grepl("fn must be a function", msg, fixed = TRUE)) {
        return(FALSE)
    }
    if (grepl("expression-only mode currently only supports", msg, fixed = TRUE)) {
        return(TRUE)
    }
    if (grepl("strict no-loop path currently only supports", msg, fixed = TRUE)) {
        return(TRUE)
    }
    if (grepl("couldn't determine output kind", msg, fixed = TRUE)) {
        return(TRUE)
    }
    grepl(
        "loop not supported|unsupported|failed to emit|raw fallback|code generation failed",
        msg
    )
}

.mojor_object_mode_entry_reason <- function(err_msg) {
    if (!is.character(err_msg) || length(err_msg) != 1L || !nzchar(err_msg)) {
        return("unknown")
    }
    msg <- tolower(err_msg)
    if (grepl("loop not supported", msg, fixed = TRUE)) {
        return("loop_not_supported")
    }
    if (grepl("failed to emit", msg, fixed = TRUE)) {
        return("failed_to_emit")
    }
    if (grepl("raw fallback", msg, fixed = TRUE)) {
        return("raw_fallback")
    }
    if (grepl("code generation failed", msg, fixed = TRUE)) {
        return("code_generation_failed")
    }
    if (grepl("unsupported", msg, fixed = TRUE)) {
        return("unsupported")
    }
    "other"
}

.mojor_object_mode_entry_summary <- function(err_msg) {
    data.frame(
        reason = .mojor_object_mode_entry_reason(err_msg),
        count = 1L,
        stringsAsFactors = FALSE
    )
}

.mojor_make_object_mode_call_wrapper <- function(run_call, error_state = NULL) {
    function(...) {
        if (is.null(error_state) ||
            identical(error_state$error_mode, "stop")) {
            run_call(...)
        } else {
            .mojor_execute_with_state(run_call, error_state, ...)
        }
    }
}

.mojor_object_name_call_args <- function(args_in, arg_names) {
    if (length(args_in) >
        0 && length(arg_names) >
        0) {
        if (is.null(names(args_in))) {
            names(args_in) <- arg_names[seq_along(args_in)]
        } else {
            blank <- names(args_in) ==
                ""
            names(args_in)[blank] <- arg_names[seq_along(args_in)][blank]
        }
    }
    args_in
}

.mojor_object_resolve_call_args <- function(fn, args_in, call_env = parent.frame()) {
    fmls <- formals(fn)
    if (is.null(fmls) ||
        length(fmls) ==
            0) {
        return(list())
    }
    arg_names <- names(fmls)
    if (is.null(arg_names) ||
        length(arg_names) ==
            0) {
        return(list())
    }
    args_named <- .mojor_object_name_call_args(args_in, arg_names)
    if ("..." %in% arg_names) {
        return(args_named)
    }
    capture_body <- as.call(
        c(
            as.name("list"),
            lapply(arg_names, as.name)
        )
    )
    capture_fn <- as.function(
        c(fmls, list(capture_body)),
        env = environment(fn)
    )
    out <- do.call(capture_fn, args_named, envir = call_env)
    if (is.null(names(out)))
        names(out) <- arg_names
    out
}

.mojor_object_has_nested_return <- function(expr, top_level = TRUE) {
    if (!is.call(expr)) {
        return(FALSE)
    }
    op <- as.character(expr[[1]])
    if (identical(op, "return")) {
        if (!isTRUE(top_level)) {
            return(TRUE)
        }
        if (length(expr) >=
            2 && .mojor_object_has_nested_return(expr[[2]], top_level = FALSE)) {
            return(TRUE)
        }
        return(FALSE)
    }
    if (length(expr) >=
        2) {
        for (i in 2:length(expr)) {
            if (.mojor_object_has_nested_return(expr[[i]], top_level = FALSE)) {
                return(TRUE)
            }
        }
    }
    FALSE
}

.mojor_object_extract_guard_return <- function(stmt) {
    if (!is.call(stmt) || !identical(as.character(stmt[[1L]]), "if")) {
        return(NULL)
    }
    if (length(stmt) < 3L || length(stmt) > 4L) {
        return(NULL)
    }
    normalize_branch <- function(branch) {
        out <- branch
        if (is.call(out) && identical(as.character(out[[1L]]), "{")) {
            if (length(out) != 2L) {
                return(NULL)
            }
            out <- out[[2L]]
        }
        if (!is.call(out) || !identical(as.character(out[[1L]]), "return")) {
            return(NULL)
        }
        if (length(out) >= 2L) out[[2L]] else quote(NULL)
    }
    expr_true <- normalize_branch(stmt[[3L]])
    if (is.null(expr_true)) {
        return(NULL)
    }
    expr_false <- NULL
    if (length(stmt) == 4L) {
        expr_false <- normalize_branch(stmt[[4L]])
        if (is.null(expr_false)) {
            return(NULL)
        }
    }
    list(cond = stmt[[2L]], expr_true = expr_true, expr_false = expr_false)
}

.mojor_object_extract_guard_assign <- function(stmt) {
    if (!is.call(stmt) || !identical(as.character(stmt[[1L]]), "if")) {
        return(NULL)
    }
    if (length(stmt) != 4L) {
        return(NULL)
    }
    normalize_assign <- function(branch) {
        out <- branch
        if (is.call(out) && identical(as.character(out[[1L]]), "{")) {
            if (length(out) != 2L) {
                return(NULL)
            }
            out <- out[[2L]]
        }
        if (!is.call(out) ||
            !(as.character(out[[1L]]) %in% c("<-", "=")) ||
            length(out) < 3L ||
            !is.name(out[[2L]])) {
            return(NULL)
        }
        list(lhs = as.character(out[[2L]]), expr = out[[3L]])
    }
    true_assign <- normalize_assign(stmt[[3L]])
    false_assign <- normalize_assign(stmt[[4L]])
    if (is.null(true_assign) || is.null(false_assign)) {
        return(NULL)
    }
    if (!identical(true_assign$lhs, false_assign$lhs)) {
        return(NULL)
    }
    list(
        cond = stmt[[2L]],
        lhs = true_assign$lhs,
        expr_true = true_assign$expr,
        expr_false = false_assign$expr
    )
}

.mojor_object_expr_accelerable <- function(expr) {
    disallowed <- c(
        "<-", "=", "<<-", "assign", "for", "while", "repeat", "if", "{",
        "return", "break", "next", "function", "local", "eval", "evalq",
        "on.exit"
    )
    walk <- function(node) {
        if (is.null(node)) {
            return(TRUE)
        }
        if (is.atomic(node) ||
            is.name(node)) {
            return(TRUE)
        }
        if (!is.call(node)) {
            return(FALSE)
        }
        op <- as.character(node[[1]])
        if (op %in% disallowed) {
            return(FALSE)
        }
        if (length(node) >=
            2) {
            for (i in 2:length(node)) {
                if (!walk(node[[i]])) {
                  return(FALSE)
                }
            }
        }
        TRUE
    }
    walk(expr)
}

.mojor_object_expr_deps <- function(expr, scope_vars) {
    vars <- all.vars(expr, functions = FALSE, unique = TRUE)
    if (length(vars) ==
        0) {
        return(character(0))
    }
    vars[vars %in% scope_vars]
}

.mojor_object_plan_build <- function(fn, arg_names) {
    blocks <- .mojor_extract_block(body(fn))
    if (length(blocks) ==
        0) {
        return(
            list(
                whole_fallback = FALSE, reason = "empty function body",
                steps = list(), stats = list(
                  steps_total = 0L, steps_accelerable = 0L, steps_compiled = 0L,
                  steps_deopted = 0L
              )
            )
        )
    }

    scope_vars <- arg_names
    steps <- list()
    for (i in seq_along(blocks)) {
        stmt <- blocks[[i]]
        step <- list(
            id = as.integer(i),
            stmt = stmt, deps = character(0),
            kind = "stmt_eval"
        )

        guard_return <- .mojor_object_extract_guard_return(stmt)
        if (!is.null(guard_return)) {
            step$kind <- "guard_return_eval"
            step$cond <- guard_return$cond
            step$expr_true <- guard_return$expr_true
            step$expr_false <- guard_return$expr_false
            step$deps <- unique(c(
                .mojor_object_expr_deps(guard_return$cond, scope_vars),
                .mojor_object_expr_deps(guard_return$expr_true, scope_vars),
                if (is.null(guard_return$expr_false)) {
                    character(0)
                } else {
                    .mojor_object_expr_deps(guard_return$expr_false, scope_vars)
                }
            ))
            steps[[length(steps) + 1L]] <- step
            next
        }
        guard_assign <- .mojor_object_extract_guard_assign(stmt)
        if (!is.null(guard_assign)) {
            step$kind <- "guard_assign_eval"
            step$cond <- guard_assign$cond
            step$lhs <- guard_assign$lhs
            step$expr_true <- guard_assign$expr_true
            step$expr_false <- guard_assign$expr_false
            step$deps <- unique(c(
                .mojor_object_expr_deps(guard_assign$cond, scope_vars),
                .mojor_object_expr_deps(guard_assign$expr_true, scope_vars),
                .mojor_object_expr_deps(guard_assign$expr_false, scope_vars)
            ))
            scope_vars <- unique(c(scope_vars, guard_assign$lhs))
            steps[[length(steps) + 1L]] <- step
            next
        }

        if (is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("<-", "=") &&
            length(stmt) >=
                3 && is.name(stmt[[2]])) {
            lhs <- as.character(stmt[[2]])
            rhs <- stmt[[3]]
            deps <- .mojor_object_expr_deps(rhs, scope_vars)
            accel <- .mojor_object_expr_accelerable(rhs)
            step$kind <- if (accel)
                "assign_accel" else "assign_eval"
            step$lhs <- lhs
            step$expr <- rhs
            step$deps <- deps
            scope_vars <- unique(c(scope_vars, lhs))
        } else if (is.call(stmt) &&
            as.character(stmt[[1]]) ==
                "return") {
            ret_expr <- if (length(stmt) >=
                2)
                stmt[[2]] else quote(NULL)
            deps <- .mojor_object_expr_deps(ret_expr, scope_vars)
            accel <- .mojor_object_expr_accelerable(ret_expr)
            step$kind <- if (accel)
                "return_accel" else "return_eval"
            step$expr <- ret_expr
            step$deps <- deps
        } else {
            accel <- .mojor_object_expr_accelerable(stmt)
            if (accel) {
                step$kind <- "expr_accel"
                step$expr <- stmt
                step$deps <- .mojor_object_expr_deps(stmt, scope_vars)
            }
        }

        steps[[length(steps) +
            1L]] <- step
    }

    accel_kinds <- c("assign_accel", "return_accel", "expr_accel")
    stats <- list(
        steps_total = as.integer(length(steps)),
        steps_accelerable = as.integer(
            sum(
                vapply(
                  steps, function(s) s$kind %in%
                    accel_kinds, logical(1)
              )
            )
        ),
        steps_compiled = 0L, steps_deopted = 0L, deopt_cache_hits = 0L
    )
    list(whole_fallback = FALSE, reason = NULL, steps = steps, stats = stats)
}

.mojor_object_make_expr_fn <- function(dep_names, expr, fn_env) {
    fmls <- as.pairlist(
        setNames(
            vector("list", length(dep_names)),
            dep_names
        )
    )
    as.function(
        c(fmls, list(expr)),
        env = fn_env
    )
}

.mojor_object_step_cache_env <- function(root_env, key) {
    if (!exists(key, envir = root_env, inherits = FALSE)) {
        assign(
            key, new.env(parent = emptyenv()),
            envir = root_env
        )
    }
    get(key, envir = root_env, inherits = FALSE)
}

.mojor_object_record_deopt <- function(
    runtime_state, step, stage, reason, sig_key = NA_character_, count_event = TRUE
) {
    if (isTRUE(count_event)) {
        runtime_state$stats$steps_deopted <- runtime_state$stats$steps_deopted +
            1L
    } else {
        runtime_state$stats$deopt_cache_hits <- runtime_state$stats$deopt_cache_hits +
            1L
    }

    reason_chr <- if (is.character(reason) &&
        length(reason) >=
            1)
        reason[[1]] else as.character(reason)
    if (!nzchar(reason_chr))
        reason_chr <- "<empty>"
    reason_chr <- gsub("[\r\n\t]+", " ", reason_chr)
    reason_chr <- trimws(reason_chr)
    if (nchar(reason_chr, type = "chars") >
        280) {
        reason_chr <- paste0(
            substr(reason_chr, 1, 277),
            "..."
        )
    }

    entry <- list(
        ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        step_id = step$id, step_kind = step$kind, stage = stage, reason = reason_chr,
        signature = sig_key
    )

    if (length(runtime_state$deopt_log) <
        runtime_state$deopt_log_limit) {
        runtime_state$deopt_log[[length(runtime_state$deopt_log) +
            1L]] <- entry
    } else {
        runtime_state$deopt_log_dropped <- runtime_state$deopt_log_dropped +
            1L
    }

    key <- paste(step$kind, stage, reason_chr, sep = " | ")
    counts <- runtime_state$deopt_reason_counts
    prev <- counts[[key]]
    if (is.null(prev) ||
        is.na(prev))
        prev <- 0L
    counts[[key]] <- as.integer(prev) +
        1L
    runtime_state$deopt_reason_counts <- counts
    invisible(NULL)
}

.mojor_object_step_try_accelerate <- function(step, exec_env, runtime_state, build_ctx, fn_env) {
    dep_names <- step$deps
    dep_vals <- if (length(dep_names) >
        0) {
        mget(dep_names, envir = exec_env, inherits = TRUE)
    } else {
        list()
    }

    dep_types <- tryCatch(
        .mojor_infer_types(dep_vals, dep_names, broadcast = "none"),
        error = function(e) e
    )
    if (inherits(dep_types, "error")) {
        .mojor_object_record_deopt(
            runtime_state = runtime_state, step = step, stage = "infer_types",
            reason = conditionMessage(dep_types),
            sig_key = NA_character_, count_event = TRUE
        )
        return(list(ok = FALSE, reason = conditionMessage(dep_types)))
    }

    promotions <- attr(dep_types, "promotions")
    dep_vals <- .mojor_apply_promotions(dep_vals, promotions)
    opts_sig <- list(
        na_mode = build_ctx$na_mode, semantics = build_ctx$semantics, opt_level = if (is.null(build_ctx$opt_level)) "" else as.character(build_ctx$opt_level),
        debug = as.character(isTRUE(build_ctx$debug)),
        trace = as.character(isTRUE(build_ctx$trace)),
        memory_check = as.character(isTRUE(build_ctx$memory_check))
    )
    sig_key <- .mojor_signature_key(dep_types, opts_sig)

    step_key <- paste0("s", step$id)
    step_cache <- .mojor_object_step_cache_env(runtime_state$step_cache, step_key)
    step_deopt <- .mojor_object_step_cache_env(runtime_state$deopt_cache, step_key)
    if (exists(sig_key, envir = step_deopt, inherits = FALSE)) {
        cached <- get(sig_key, envir = step_deopt, inherits = FALSE)
        cached_reason <- if (is.list(cached) &&
            !is.null(cached$reason))
            cached$reason else "deoptimized signature"
        cached_stage <- if (is.list(cached) &&
            !is.null(cached$stage))
            cached$stage else "cached_signature"
        .mojor_object_record_deopt(
            runtime_state = runtime_state, step = step, stage = cached_stage,
            reason = cached_reason, sig_key = sig_key, count_event = FALSE
        )
        return(list(ok = FALSE, reason = cached_reason))
    }

    if (!exists(sig_key, envir = step_cache, inherits = FALSE)) {
        expr_fn <- .mojor_object_make_expr_fn(dep_names, step$expr, fn_env)
        sig_hash <- tryCatch(
            .mojor_hash(sig_key),
            error = function(e) paste0("sig", step$id)
        )
        kernel_name <- paste0(
            .mojor_safe_name(build_ctx$name),
            "_obj_", step_key, "_", substr(sig_hash, 1, 8)
        )
        compile_args <- c(
            list(
                fn = expr_fn, name = kernel_name, cache = TRUE, na_mode = build_ctx$na_mode,
                semantics = build_ctx$semantics, opt_level = build_ctx$opt_level,
                debug = isTRUE(build_ctx$debug),
                trace = isTRUE(build_ctx$trace),
                memory_check = isTRUE(build_ctx$memory_check),
                object_mode = "off"
            ),
            dep_types
        )
        built <- tryCatch(
            do.call(mojor_build, compile_args),
            error = function(e) e
        )
        if (inherits(built, "error")) {
            reason <- conditionMessage(built)
            assign(
                sig_key, list(stage = "compile", reason = reason),
                envir = step_deopt
            )
            .mojor_object_record_deopt(
                runtime_state = runtime_state, step = step, stage = "compile",
                reason = reason, sig_key = sig_key, count_event = TRUE
            )
            return(list(ok = FALSE, reason = reason))
        }
        assign(sig_key, built$func, envir = step_cache)
        runtime_state$stats$steps_compiled <- runtime_state$stats$steps_compiled +
            1L
    }

    compiled_fn <- get(sig_key, envir = step_cache, inherits = FALSE)
    out <- tryCatch(
        do.call(compiled_fn, dep_vals),
        error = function(e) e
    )
    if (inherits(out, "error")) {
        reason <- conditionMessage(out)
        assign(
            sig_key, list(stage = "execute", reason = reason),
            envir = step_deopt
        )
        .mojor_object_record_deopt(
            runtime_state = runtime_state, step = step, stage = "execute",
            reason = reason, sig_key = sig_key, count_event = TRUE
        )
        return(list(ok = FALSE, reason = reason))
    }
    list(ok = TRUE, value = out)
}

.mojor_object_make_hybrid_runner <- function(fn, plan, build_ctx) {
    arg_names <- names(formals(fn))
    if (is.null(arg_names))
        arg_names <- character(0)
    fn_env <- environment(fn)
    runtime_state <- new.env(parent = emptyenv())
    runtime_state$step_cache <- new.env(parent = emptyenv())
    runtime_state$deopt_cache <- new.env(parent = emptyenv())
    runtime_state$deopt_log <- list()
    runtime_state$deopt_reason_counts <- list()
    runtime_state$deopt_log_limit <- 256L
    runtime_state$deopt_log_dropped <- 0L
    runtime_state$stats <- plan$stats
    if (is.null(runtime_state$stats$steps_compiled))
        runtime_state$stats$steps_compiled <- 0L
    if (is.null(runtime_state$stats$steps_deopted))
        runtime_state$stats$steps_deopted <- 0L
    if (is.null(runtime_state$stats$deopt_cache_hits))
        runtime_state$stats$deopt_cache_hits <- 0L

    run_call <- function(...) {
        args_in <- list(...)
        resolved <- .mojor_object_resolve_call_args(fn, args_in, call_env = parent.frame())
        exec_env <- new.env(parent = fn_env)
        if (length(resolved) >
            0) {
            for (nm in names(resolved)) {
                assign(nm, resolved[[nm]], envir = exec_env)
            }
        }

        last_value <- NULL
        for (step in plan$steps) {
            if (identical(step$kind, "assign_accel")) {
                accel <- .mojor_object_step_try_accelerate(step, exec_env, runtime_state, build_ctx, fn_env)
                if (isTRUE(accel$ok)) {
                  assign(step$lhs, accel$value, envir = exec_env)
                  last_value <- accel$value
                } else {
                  last_value <- eval(step$stmt, envir = exec_env)
                }
            } else if (identical(step$kind, "assign_eval")) {
                last_value <- eval(step$stmt, envir = exec_env)
            } else if (identical(step$kind, "expr_accel")) {
                accel <- .mojor_object_step_try_accelerate(step, exec_env, runtime_state, build_ctx, fn_env)
                if (isTRUE(accel$ok)) {
                  last_value <- accel$value
                } else {
                  last_value <- eval(step$stmt, envir = exec_env)
                }
            } else if (identical(step$kind, "return_accel")) {
                accel <- .mojor_object_step_try_accelerate(step, exec_env, runtime_state, build_ctx, fn_env)
                if (isTRUE(accel$ok)) {
                  return(accel$value)
                }
                return(eval(step$expr, envir = exec_env))
            } else if (identical(step$kind, "return_eval")) {
                return(eval(step$expr, envir = exec_env))
            } else if (identical(step$kind, "guard_return_eval")) {
                guard_hit <- eval(
                    as.call(list(as.name("if"), step$cond, TRUE, FALSE)),
                    envir = exec_env
                )
                if (isTRUE(guard_hit)) {
                    return(eval(step$expr_true, envir = exec_env))
                }
                if (!is.null(step$expr_false)) {
                    return(eval(step$expr_false, envir = exec_env))
                }
                last_value <- NULL
            } else if (identical(step$kind, "guard_assign_eval")) {
                guard_hit <- eval(
                    as.call(list(as.name("if"), step$cond, TRUE, FALSE)),
                    envir = exec_env
                )
                assigned <- if (isTRUE(guard_hit)) {
                    eval(step$expr_true, envir = exec_env)
                } else {
                    eval(step$expr_false, envir = exec_env)
                }
                assign(step$lhs, assigned, envir = exec_env)
                last_value <- assigned
            } else {
                last_value <- eval(step$stmt, envir = exec_env)
            }
        }
        last_value
    }

    attr(run_call, "object_runtime_state") <- runtime_state
    run_call
}

.mojor_build_object_mode_result <- function(
    fn, name, arg_specs, na_mode, semantics, error_state, reason, profile = FALSE,
    mode = c("fallback", "hybrid"),
    run_call = NULL, object_plan_stats = NULL, object_runtime_state = NULL,
    entry_summary = NULL, entry_message = NULL
) {
    mode <- match.arg(mode)
    if (!is.function(run_call)) {
        run_call <- function(...) fn(...)
    }
    if (is.null(object_plan_stats)) {
        object_plan_stats <- list(
            steps_total = 0L, steps_accelerable = 0L, steps_compiled = 0L,
            steps_deopted = 0L, deopt_cache_hits = 0L
        )
    }
    if (is.null(entry_summary)) {
        entry_summary <- .mojor_object_mode_entry_summary(reason)
    }
    if (!is.data.frame(entry_summary) || !all(c("reason", "count") %in% names(entry_summary))) {
        entry_summary <- .mojor_object_mode_entry_summary(reason)
    }
    if (is.null(entry_message) || !is.character(entry_message) || !nzchar(entry_message[[1L]])) {
        entry_message <- reason
    }
    trans_stub <- list(
        types = arg_specs, na_mode = na_mode, semantics = semantics, out_type = "object",
        out_kind = "object", simd = list(emitted = FALSE, safe = FALSE),
        object_mode = TRUE, object_mode_kind = mode, object_mode_reason = reason,
        object_mode_entry_summary = entry_summary,
        object_mode_entry_message = entry_message,
        mojo = ""
    )
    wrapped_call <- .mojor_make_object_mode_call_wrapper(run_call, error_state = error_state)
    if (!is.null(object_runtime_state)) {
        attr(wrapped_call, "object_runtime_state") <- object_runtime_state
    }
    built <- list(
        func = wrapped_call, gpu_func = NULL, gpu_func_raw = NULL, build_dir = NULL,
        kernel = name, cache_key = NULL, trans = trans_stub, wrapper_so = NULL,
        success = TRUE, compiled = FALSE, object_mode = TRUE, object_mode_kind = mode,
        object_mode_reason = reason, object_plan_stats = object_plan_stats,
        object_mode_entry_summary = entry_summary,
        object_mode_entry_message = entry_message
    )
    if (!is.null(object_runtime_state)) {
        built$object_runtime_state <- object_runtime_state
    }
    if (isTRUE(profile)) {
        built <- .mojor_add_profiling(built, profile_enabled = TRUE)
    }
    built
}

.mojor_df_make_runtime_call_wrapper <- function(fn, compiled_func, arg_specs, df_schema, hidden_map, dynamic_selectors = NULL, compiled_arg_order = NULL) {
    force(fn)
    force(compiled_func)
    force(arg_specs)
    force(df_schema)
    force(hidden_map)
    force(dynamic_selectors)
    force(compiled_arg_order)
    function(...) {
        resolved <- .mojor_object_resolve_call_args(
            fn, list(...),
            call_env = parent.frame()
        )
        compiled_args <- .mojor_df_prepare_compiled_args(
            resolved_args = resolved, arg_specs = arg_specs, df_schema = df_schema,
            hidden_map = hidden_map, dynamic_selectors = dynamic_selectors,
            compiled_arg_order = compiled_arg_order, context = "mojor_build"
        )
        do.call(compiled_func, compiled_args)
    }
}

.mojor_tier9_make_return_ctor_wrapper <- function(fn, ctor_spec) {
    force(fn)
    force(ctor_spec)
    function(...) {
        raw <- do.call(
            fn, list(...),
            envir = parent.frame()
        )
        if (identical(ctor_spec$kind, "data.frame")) {
            if (!is.data.frame(raw)) {
                stop("mojor_build: compiled subset expected data.frame return")
            }
            if (!identical(
                names(raw),
                ctor_spec$names
            )) {
                stop(
                  "mojor_build: compiled subset returned data.frame column names differ from constructor declaration"
              )
            }
            values <- lapply(ctor_spec$names, function(nm) raw[[nm]])
            return(
                .mojor_tier9_runtime_make_data_frame(values, ctor_spec$names, context = "mojor_build")
            )
        }
        if (!is.list(raw) ||
            is.data.frame(raw)) {
            stop("mojor_build: compiled subset expected list return")
        }
        if (length(raw) !=
            length(ctor_spec$entries)) {
            stop("mojor_build: compiled subset returned list length mismatch")
        }
        if (!is.null(names(raw)) &&
            length(ctor_spec$names) ==
                length(raw)) {
 # Empty names are allowed; named entries must match
 # declared names.
            for (i in seq_along(raw)) {
                if (nzchar(ctor_spec$names[[i]]) &&
                  !identical(
                    names(raw)[[i]],
                    ctor_spec$names[[i]]
                )) {
                  stop(
                    "mojor_build: compiled subset returned list names differ from constructor declaration"
                )
                }
            }
        }
        .mojor_tier9_runtime_make_list(raw, ctor_spec$names, context = "mojor_build")
    }
}

.mojor_signature_param_names <- function(signature) {
    if (is.null(signature) || length(signature) == 0L) {
        return(character(0))
    }
    sig_chr <- as.character(signature)
    out <- vapply(sig_chr, function(sig_arg) {
        if (!nzchar(sig_arg)) {
            return(NA_character_)
        }
        pieces <- strsplit(sig_arg, ":", fixed = TRUE)[[1]]
        if (length(pieces) < 1L) {
            return(NA_character_)
        }
        nm <- trimws(pieces[[1L]])
        if (!nzchar(nm)) NA_character_ else nm
    }, character(1))
    unname(out)
}

.mojor_expected_kernel_signature_param_names <- function(
    args,
    arg_specs = list(),
    out_kind,
    out_name,
    rng_needed = FALSE,
    len_arrays = character(0),
    nrow_arrays = character(0),
    ncol_arrays = character(0),
    dim_arrays = character(0),
    out_matrix = FALSE,
    out_array = FALSE,
    broadcast_nd = FALSE,
    na_needed = FALSE) {
    params <- character(0)
    for (a in args) {
        spec <- arg_specs[[a]]
        if (!is.null(spec) && .mojor_is_array(spec)) {
            params <- c(params, paste0(a, "_ptr"))
        } else {
            params <- c(params, a)
        }
    }
    out_ptr_param <- if (identical(out_kind, "vector")) {
        if (!is.null(out_name) && out_name %in% args) "__mojor_out_ptr" else paste0(out_name, "_ptr")
    } else {
        paste0(out_name, "_ptr")
    }
    params <- c(params, out_ptr_param, "__mojor_n")
    if (isTRUE(rng_needed)) {
        params <- c(params, "__mojor_rng_state_ptr")
    }
    if (length(len_arrays) > 0L) {
        for (a in len_arrays) {
            params <- c(params, .mojor_len_param_name(a))
        }
    }
    if (length(nrow_arrays) > 0L) {
        for (a in nrow_arrays) {
            params <- c(params, .mojor_nrow_param_name(a))
        }
    }
    if (length(ncol_arrays) > 0L) {
        for (a in ncol_arrays) {
            if (!(a %in% nrow_arrays)) {
                params <- c(params, .mojor_ncol_param_name(a))
            }
        }
    }
    if (length(dim_arrays) > 0L) {
        for (a in dim_arrays) {
            params <- c(
                params,
                paste0(.mojor_dim_param_name(a), "_ptr"),
                .mojor_ndim_param_name(a)
            )
        }
    }
    if (isTRUE(out_matrix)) {
        params <- c(
            params,
            .mojor_out_nrow_param_name(),
            .mojor_out_ncol_param_name()
        )
    }
    if (isTRUE(out_array) || isTRUE(broadcast_nd)) {
        params <- c(
            params,
            paste0(.mojor_out_dim_param_name(), "_ptr"),
            .mojor_out_ndim_param_name()
        )
    }
    if (isTRUE(na_needed)) {
        params <- c(params, "__mojor_na_flag_ptr")
    }
    params
}

.mojor_validate_kernel_signature_contract <- function(
    trans,
    args,
    arg_specs = list(),
    out_kind,
    out_name,
    rng_needed = FALSE,
    len_arrays = character(0),
    nrow_arrays = character(0),
    ncol_arrays = character(0),
    dim_arrays = character(0),
    out_matrix = FALSE,
    out_array = FALSE,
    broadcast_nd = FALSE,
    na_needed = FALSE) {
    actual <- .mojor_signature_param_names(trans$signature)
    if (length(actual) == 0L) {
        stop("mojor_build: transpile output did not provide kernel signature parameters")
    }
    if (any(is.na(actual) | !nzchar(actual))) {
        stop("mojor_build: transpile output contains invalid kernel signature parameter name(s)")
    }
    dup_actual <- unique(actual[duplicated(actual)])
    if (length(dup_actual) > 0L) {
        stop(
            "mojor_build: transpile output contains duplicate kernel signature parameter(s): ",
            paste(dup_actual, collapse = ", ")
        )
    }

    expected <- .mojor_expected_kernel_signature_param_names(
        args = args,
        arg_specs = arg_specs,
        out_kind = out_kind,
        out_name = out_name,
        rng_needed = rng_needed,
        len_arrays = len_arrays,
        nrow_arrays = nrow_arrays,
        ncol_arrays = ncol_arrays,
        dim_arrays = dim_arrays,
        out_matrix = out_matrix,
        out_array = out_array,
        broadcast_nd = broadcast_nd,
        na_needed = na_needed
    )
    if (identical(out_kind, "vector") &&
        !is.null(out_name) &&
        out_name %in% args &&
        !is.null(arg_specs[[out_name]]) &&
        .mojor_is_array(arg_specs[[out_name]])) {
        alias_len_param <- .mojor_len_param_name(out_name)
        if (!(alias_len_param %in% expected)) {
            stop(
                "mojor_build: alias-output kernel missing expected length parameter ",
                alias_len_param
            )
        }
    }

    if (length(actual) != length(expected)) {
        stop(
            "mojor_build: kernel signature parameter count mismatch (expected ",
            length(expected), ", got ", length(actual), ")"
        )
    }
    mismatch <- which(actual != expected)
    if (length(mismatch) > 0L) {
        mi <- mismatch[[1L]]
        stop(
            "mojor_build: kernel signature parameter order mismatch at position ",
            mi, " (expected ", expected[[mi]], ", got ", actual[[mi]], ")"
        )
    }
    invisible(TRUE)
}

mojor_build <- function(
    fn, ..., name = "mojor_kernel", build_dir = tempdir(), load = TRUE,
    verbose = FALSE, cache = TRUE, cache_dir = NULL, na_mode = NULL, fusion_debug = FALSE,
    assume_aligned = NULL, simd_mode = NULL, elementwise = FALSE, elementwise_target = c("cpu", "gpu"),
    elementwise_size = NULL, elementwise_cpu = NULL, elementwise_gpu_layouttensor = NULL,
    gpu_jit_mode = NULL, broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    ),
    fast_math = NULL, parallel = FALSE, semantics = c("r", "raw"),
    unroll = NULL, reduction = NULL, bounds_check = NULL, index_base = NULL,
    array_layout = NULL, tile = NULL, opt_level = NULL, r_jit_level = NULL, debug = FALSE,
    trace = FALSE, memory_check = FALSE, profile = FALSE, error_mode = c("stop", "partial", "retry"),
    max_retries = 3, retry_delay = 0.1, object_mode = c("off", "fallback", "hybrid"),
    df_schema = NULL
) {
    gpu_jit_mode_explicit <- !missing(gpu_jit_mode) && !is.null(gpu_jit_mode)
    na_mode <- .mojor_effective_na_mode(na_mode)
    broadcast <- match.arg(broadcast)
    semantics <- match.arg(semantics)
    object_mode <- match.arg(object_mode)
    arg_specs_input <- list(...)
    effective_ir_only <- .mojor_resolve_effective_ir_only(primary = arg_specs_input, context = "mojor_build")
    if (isTRUE(effective_ir_only) &&
        !identical(object_mode, "off")) {
        stop("mojor_build: object_mode must be 'off' when ir_only=TRUE")
    }
    formal_args <- names(formals(fn))
    if (is.null(formal_args))
        formal_args <- character(0)
    arg_specs_norm <- .mojor_normalize_arg_specs(formal_args, arg_specs_input)
    return_ctor_kind <- .mojor_return_ctor_for_mode(
        return_ctor_kind = .mojor_tier9_return_ctor_kind(fn),
        strict_ir_only = effective_ir_only, context = "mojor_build"
    )
    tier9_requested <- !is.null(df_schema) ||
        any(
            vapply(arg_specs_norm[formal_args], .mojor_type_is_tier9, logical(1))
        ) ||
        !is.null(return_ctor_kind)
    if (isTRUE(tier9_requested)) {
        .mojor_require_tier9_ready("compiled subset build")
    }
    df_schema_norm <- .mojor_validate_df_schema(formal_args, arg_specs_norm, df_schema)
    if (!is.null(return_ctor_kind)) {
        ctor <- .mojor_tier9_parse_return_ctor(fn, context = "mojor_build")
        .mojor_tier9_validate_ctor_arg_specs(ctor = ctor, arg_specs = arg_specs_norm, context = "mojor_build")
        out <- list(
            func = if (isTRUE(load)) .mojor_tier9_make_return_ctor_wrapper(fn = fn, ctor_spec = ctor) else NULL,
            gpu_func = NULL, gpu_func_raw = NULL, build_dir = NULL, kernel = name,
            cache_key = NULL, trans = list(
                name = name, mojo = "", signature = character(0),
                ir = NULL, notes = c("compiled subset constructor runtime assembly path"),
                types = arg_specs_norm
            ),
            wrapper_so = NULL, success = TRUE, compiled = FALSE, object_mode = TRUE,
            object_mode_kind = "tier9_ctor_runtime"
        )
        out$tier9_return_ctor <- list(
            kind = ctor$kind, names = ctor$names, part_kernels = paste0(
                name, "__tier9_", if (identical(ctor$kind, "data.frame")) "df" else "list",
                "_", seq_along(ctor$entries)
            )
        )
        out$tier9_part_builds <- list()
        if (!is.null(out$trans) &&
            is.list(out$trans)) {
            out$trans$tier9_return_ctor <- out$tier9_return_ctor
            out$trans$tier9_ctor_rewritten <- TRUE
            out$trans$tier9_part_trans <- list()
        }
        out$tier9_ctor_rewritten <- TRUE
        if (isTRUE(profile)) {
            out <- .mojor_add_profiling(out, profile_enabled = TRUE)
        }
        return(out)
    }
    df_args <- formal_args[vapply(
        formal_args, function(a) identical(arg_specs_norm[[a]], "df"),
        logical(1)
    )]
    if (length(df_args) >
        0) {
        rewrite <- .mojor_df_rewrite_callable(
            fn = fn, arg_specs = arg_specs_norm, df_schema = df_schema_norm,
            context = "mojor_build"
        )
        built_inner <- do.call(
            mojor_build, c(
                list(fn = rewrite$fn),
                rewrite$arg_specs, list(
                  name = name, build_dir = build_dir, load = load, verbose = verbose,
                  cache = cache, cache_dir = cache_dir, na_mode = na_mode,
                  fusion_debug = fusion_debug, assume_aligned = assume_aligned,
                  simd_mode = simd_mode, elementwise = elementwise, elementwise_target = elementwise_target,
                  elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
                  elementwise_gpu_layouttensor = elementwise_gpu_layouttensor,
                  gpu_jit_mode = gpu_jit_mode, broadcast = broadcast, fast_math = fast_math,
                  parallel = parallel, semantics = semantics, unroll = unroll,
                  reduction = reduction, bounds_check = bounds_check, index_base = index_base,
                  array_layout = array_layout, tile = tile, opt_level = opt_level,
                  r_jit_level = r_jit_level,
                  debug = debug, trace = trace, memory_check = memory_check,
                  profile = profile, error_mode = error_mode, max_retries = max_retries,
                  retry_delay = retry_delay, object_mode = object_mode,
                  df_schema = NULL
              )
            )
        )
        if (isTRUE(load) &&
            !is.null(built_inner$func) &&
            is.function(built_inner$func)) {
            built_inner$func <- .mojor_df_make_runtime_call_wrapper(
                fn = fn, compiled_func = built_inner$func, arg_specs = arg_specs_norm,
                df_schema = df_schema_norm, hidden_map = rewrite$hidden_map,
                dynamic_selectors = rewrite$dynamic_selectors,
                compiled_arg_order = names(rewrite$arg_specs)
            )
        }
        built_inner$gpu_func <- NULL
        built_inner$gpu_func_raw <- NULL
        if (!is.null(built_inner$trans) &&
            is.list(built_inner$trans)) {
            built_inner$trans$df_schema <- df_schema_norm
            built_inner$trans$df_hidden_map <- rewrite$hidden_map
            built_inner$trans$df_dynamic_selectors <- rewrite$dynamic_selectors
            built_inner$trans$df_rewritten <- TRUE
        }
        built_inner$df_rewritten <- TRUE
        return(built_inner)
    }
    broadcast_nd <- broadcast %in% c("broadcast_nd", "broadcast_nd_warn")
    .mojor_state$current_broadcast_nd <- isTRUE(broadcast_nd)
    if (is.null(elementwise_gpu_layouttensor)) {
        elementwise_gpu_layouttensor <- .mojor_state$options$elementwise_gpu_layouttensor
    }
    if (!is.null(elementwise_gpu_layouttensor) &&
        (!is.logical(elementwise_gpu_layouttensor) ||
            length(elementwise_gpu_layouttensor) !=
                1)) {
        stop(
            "mojor_build: elementwise_gpu_layouttensor must be TRUE, FALSE, or NULL"
        )
    }
    if (is.null(gpu_jit_mode)) {
        gpu_jit_mode <- .mojor_state$options$gpu_jit_mode
    }
    gpu_jit_mode <- match.arg(gpu_jit_mode, c("auto", "helper_shim", "unified_preview"))
    gpu_jit_mode_for_transpile <- if (isTRUE(gpu_jit_mode_explicit))
        gpu_jit_mode else NULL
 # Stage B: memory_check implies debug
    if (isTRUE(memory_check))
        debug <- TRUE
    if (!is.logical(profile) ||
        length(profile) !=
            1) {
        stop("mojor_build: profile must be TRUE or FALSE")
    }

 # Error recovery configuration (Stage C)
    error_mode <- match.arg(error_mode)
    .mojor_state$error_recovery <- list(
        error_mode = error_mode, max_retries = max_retries, retry_delay = retry_delay,
        retry_on = character(0),
        current_retry = 0
    )
    r_jit_level <- .mojor_effective_r_jit_level(r_jit_level, context = "mojor_build")

    trans <- .mojor_with_r_jit_level(
        r_jit_level,
        tryCatch(
            mojor_transpile(
                fn, ..., name = name, na_mode = na_mode, fusion_debug = fusion_debug,
                assume_aligned = assume_aligned, simd_mode = simd_mode, elementwise = elementwise,
                elementwise_target = elementwise_target, elementwise_size = elementwise_size,
                elementwise_cpu = elementwise_cpu, elementwise_gpu_layouttensor = elementwise_gpu_layouttensor,
                gpu_jit_mode = gpu_jit_mode_for_transpile, broadcast = broadcast, parallel = parallel,
                semantics = semantics, unroll = unroll, reduction = reduction,
                bounds_check = bounds_check, index_base = index_base, array_layout = array_layout,
                tile = tile, opt_level = opt_level, debug = debug, trace = trace,
                memory_check = memory_check, df_schema = df_schema, profile = profile
            ),
            error = function(e) e
        )
    )
    if (inherits(trans, "error")) {
        err_msg <- conditionMessage(trans)
        entry_summary <- .mojor_object_mode_entry_summary(err_msg)
        if (isTRUE(effective_ir_only) &&
            .mojor_is_object_mode_fallback_error(err_msg)) {
            stop(
                "mojor_build: strict mode (ir_only=TRUE) forbids object-mode fallback: ",
                err_msg, call. = FALSE
            )
        }
        if (!(object_mode %in% c("fallback", "hybrid")) ||
            !.mojor_is_object_mode_fallback_error(err_msg)) {
            stop(trans)
        }
        if (identical(object_mode, "fallback")) {
            message("mojor_build: object_mode fallback enabled: ", err_msg)
            return(
                .mojor_build_object_mode_result(
                  fn = fn, name = name, arg_specs = arg_specs_input, na_mode = na_mode,
                  semantics = semantics, error_state = .mojor_state$error_recovery,
                  reason = err_msg, profile = profile, mode = "fallback",
                  entry_summary = entry_summary, entry_message = err_msg
              )
            )
        }

        plan <- .mojor_object_plan_build(fn, names(formals(fn)))
        build_ctx <- list(
            name = name, na_mode = na_mode, semantics = semantics,
            opt_level = opt_level, debug = debug, trace = trace, memory_check = memory_check
        )
        run_call <- .mojor_object_make_hybrid_runner(fn, plan, build_ctx = build_ctx)
        runtime_state <- attr(run_call, "object_runtime_state")
        reason <- err_msg
        if (is.character(plan$reason) && length(plan$reason) >= 1L && nzchar(plan$reason[[1L]])) {
            reason <- paste0(err_msg, "; hybrid plan note: ", plan$reason[[1L]])
        }
        message(
            "mojor_build: hybrid object mode enabled: ", reason, " (accelerable steps: ",
            plan$stats$steps_accelerable, "/", plan$stats$steps_total,
            ")"
        )
        return(
            .mojor_build_object_mode_result(
                fn = fn, name = name, arg_specs = arg_specs_input, na_mode = na_mode,
                semantics = semantics, error_state = .mojor_state$error_recovery,
                reason = reason, profile = profile, mode = "hybrid", run_call = run_call,
                object_plan_stats = plan$stats, object_runtime_state = runtime_state,
                entry_summary = entry_summary, entry_message = err_msg
            )
        )
    }

    parallel_runtime_mode <- .mojor_parallel_runtime_mode()
    parallel_runtime_inprocess <- .mojor_parallel_inprocess_enabled()
    parallel_runtime_reason <- .mojor_parallel_runtime_reason(parallel_runtime_mode)
    parallel_uses_parallelize <- .mojor_trans_uses_parallelize(trans)
    if (is.null(trans$parallel) ||
        !is.list(trans$parallel)) {
        trans$parallel <- list()
    }
    trans$parallel$runtime_mode <- parallel_runtime_mode
    trans$parallel$runtime_reason <- parallel_runtime_reason
    trans$parallel$inprocess_enabled <- parallel_runtime_inprocess
    trans$parallel$uses_parallelize <- parallel_uses_parallelize

 # Check if this should use the expression-kernel build path.
    is_expression_kernel <- .mojor_use_expression_kernel_build_path(trans)

 # Route to expression kernel build if applicable
    if (is_expression_kernel) {
        return(
            .mojor_build_expression_kernel(
                trans = trans, fn = fn, name = name, build_dir = build_dir,
                load = load, verbose = verbose, cache = cache, cache_dir = cache_dir,
                fast_math = fast_math, ...
            )
        )
    }

    rng_needed <- isTRUE(trans$rng_needed)
    if (isTRUE(fusion_debug) &&
        !is.null(trans$simd)) {
        simd_lines <- .mojor_simd_report_lines(trans$simd)
        message(paste(simd_lines, collapse = "\n"))
    }
    cache_dir <- .mojor_resolve_cache_dir(cache_dir)

    mojo_version <- .mojor_mojo_version_string()
    len_checks_sig <- ""
    if (!is.null(trans$len_checks_c) &&
        length(trans$len_checks_c) >
            0) {
        len_checks_sig <- paste(
            vapply(
                trans$len_checks_c, function(chk) {
                  if (is.null(chk$kind)) {
                    return("none")
                  }
                  if (identical(chk$kind, "sum_len")) {
                    parts <- vapply(
                      chk$parts, function(p) {
                        if (!is.null(p$kind) &&
                          identical(p$kind, "array") &&
                          !is.null(p$name)) {
                          paste0("array:", p$name)
                        } else if (!is.null(p$kind) &&
                          identical(p$kind, "expr") &&
                          !is.null(p$expr_c)) {
                          paste0("expr:", p$expr_c)
                        } else {
                          "scalar"
                        }
                      }, character(1)
                  )
                    paste0(
                      "sum_len(", paste(parts, collapse = ","),
                      ")"
                  )
                  } else if (identical(chk$kind, "rep_len")) {
                    if (!is.null(chk$len_value)) {
                      paste0("rep_len:", chk$len_value)
                    } else if (!is.null(chk$len_name)) {
                      paste0("rep_len:", chk$len_name)
                    } else {
                      "rep_len:unknown"
                    }
                  } else {
                    paste0("len_check:", chk$kind)
                  }
                }, character(1)
            ),
            collapse = ";"
        )
    }
    build_flags <- .mojor_fast_math_flags(fast_math)
    out_len_source_sig <- if (!is.null(trans$out_len_source)) {
        paste(capture.output(dput(trans$out_len_source)), collapse = "")
    } else {
        "NULL"
    }
    key_input <- paste(
        trans$mojo, paste(
            names(trans$types),
            trans$types, sep = "=", collapse = ";"
        ),
        paste0("broadcast=", broadcast),
        paste0("elementwise_cpu=", isTRUE(elementwise_cpu)),
        paste0(
            "elementwise_gpu_layouttensor=", isTRUE(elementwise_gpu_layouttensor)
        ),
        paste0("gpu_jit_mode=", gpu_jit_mode),
        paste0("len_checks_c=", len_checks_sig),
        paste0("na_mode=", trans$na_mode),
        paste0(
            "opt_level=", if (!is.null(trans$opt_level))
                trans$opt_level else opt_level
        ),
        paste0("fast_math=", isTRUE(fast_math)),
        paste0("build_flags=", paste(build_flags, collapse = " ")),
        paste0("debug=", isTRUE(debug)),
        paste0("trace=", isTRUE(trace)),
        paste0("memory_check=", isTRUE(memory_check)),
        "kernel_wrapper_abi=v2",
        paste0("out_len_source=", out_len_source_sig),
        paste0("transpile_version=", .mojor_state$transpile_version),
        paste(mojo_version, collapse = "\n"),
        sep = "\n"
    )
    key <- .mojor_build_hash_from_key_input(key_input)
    trans$kernel_hash <- key

    dir <- if (cache)
        file.path(cache_dir, key) else file.path(build_dir, paste0(name, "_", as.integer(Sys.time())))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    mojo_file <- file.path(dir, paste0(name, ".mojo"))
    writeLines(trans$mojo, mojo_file)

    if (grepl("ziggurat_constants", trans$mojo, fixed = TRUE)) {
        .mojor_copy_helper_mojo(dir, "ziggurat_constants.mojo")
    }
    if (isTRUE(trans$rng_needed)) {
        .mojor_copy_helper_mojo(dir, "rng_helpers.mojo")
    }

    if (isTRUE(load) &&
        isTRUE(parallel_uses_parallelize) &&
        !isTRUE(parallel_runtime_inprocess)) {
        warning(
            "mojor_build: ", parallel_runtime_reason, "; using subprocess fallback wrapper",
            call. = FALSE
        )
        return(
            .mojor_parallel_runtime_fallback_result(
                fn = fn, trans = trans, name = name, build_dir = dir, cache_key = key,
                reason = parallel_runtime_reason
            )
        )
    }

 # Copy set/match helpers
    if (isTRUE(trans$set_match_needed)) {
        .mojor_copy_helper_mojo(dir, "set_match_helpers.mojo")
    }

 # Copy quantile helpers
    if (isTRUE(trans$quantile_needed)) {
        .mojor_copy_helper_mojo(dir, "quantile_helpers.mojo")
    }

 # Copy NA helpers if na_mode is propagate or na_rm
    if (isTRUE(trans$na_needed)) {
        .mojor_copy_helper_mojo(dir, "na_helpers.mojo")
    }

 # Copy debug helpers if debug mode is enabled (Stage A)
    if (isTRUE(debug) ||
        isTRUE(trace)) {
        .mojor_copy_helper_mojo(dir, "debug_helpers.mojo")
    }

    lib_ext <- if (Sys.info()[["sysname"]] == "Darwin")
        "dylib" else "so"
    lib_kernel_name <- paste0(name, "_", substr(key, 1, 8))
    lib_path <- file.path(dir, paste0("lib", lib_kernel_name, ".", lib_ext))

 # Early type/shape validation for explicit value specs.
 # For declared type annotations, ambient same-name bindings are only consulted
 # on cached builds; uncached test lanes are intentionally isolated from caller
 # scope prechecks.
    .mojor_is_declared_type_annotation <- function(raw) {
        if (!is.character(raw) ||
            length(raw) !=
                1 ||
            is.na(raw)) {
            return(FALSE)
        }
        spec <- .mojor_normalize_type_spec(raw)
        if (is.null(spec) ||
            !is.character(spec) ||
            length(spec) !=
                1 ||
            is.na(spec)) {
            return(FALSE)
        }
        grepl(
            "^(f64|f32|i32|lgl|bool|chr|df)(\\[\\]|\\[,\\]|\\[[0-9]+d\\])?$",
            spec
        )
    }
    caller_env <- parent.frame()
    for (a in names(trans$types)) {
        if (!(a %in% names(arg_specs_input))) {
            next
        }
        raw <- arg_specs_input[[a]]
        if (.mojor_is_declared_type_annotation(raw)) {
            if (!isTRUE(cache)) {
                next
            }
            if (exists(a, envir = caller_env, inherits = FALSE)) {
                val <- get(a, envir = caller_env, inherits = FALSE)
                if (is.function(val)) {
                    next
                }
            } else {
                next
            }
        } else {
            val <- raw
        }
        spec <- trans$types[[a]]
        if (.mojor_is_array(spec)) {
            if (spec == "f32[]") {
                if (!methods::is(val, "float32"))
                    .mojor_err(paste0("argument ", a, " must be float32 for type ", spec))
            } else {
                if (!(is.vector(val) ||
                    is.array(val)))
                    .mojor_err(
                        paste0(
                            "argument ", a, " must be a vector/array for type ",
                            spec
                        )
                    )
                if (spec == "f64[]" && !is.double(val))
                    .mojor_err(paste0("argument ", a, " must be double for type ", spec))
                if (spec == "i32[]" && !is.integer(val))
                    .mojor_err(paste0("argument ", a, " must be integer for type ", spec))
                if (spec == "lgl[]" && !is.logical(val))
                    .mojor_err(paste0("argument ", a, " must be logical for type ", spec))
                if (spec == "chr[]" && !is.character(val))
                    .mojor_err(paste0("argument ", a, " must be character for type ", spec))
            }
        } else {
            if (length(val) !=
                1)
                .mojor_err(paste0("argument ", a, " must be length 1 for type ", spec))
            if (spec == "f64" && !is.double(val))
                .mojor_err(paste0("argument ", a, " must be double for type ", spec))
            if (spec == "i32" && !is.integer(val))
                .mojor_err(paste0("argument ", a, " must be integer for type ", spec))
            if (spec == "lgl" && !is.logical(val))
                .mojor_err(paste0("argument ", a, " must be logical for type ", spec))
            if (spec == "chr" && !is.character(val))
                .mojor_err(paste0("argument ", a, " must be character for type ", spec))
            if (spec == "f32" && !methods::is(val, "float32"))
                .mojor_err(paste0("argument ", a, " must be float32 for type ", spec))
        }
    }

    flags <- paste(build_flags, collapse = " ")

 # Add debug flags (Stage A)
    if (isTRUE(debug) ||
        isTRUE(trace)) {
        flags <- paste(flags, "--debug-level=full")
        flags <- paste(flags, "-D MOJOR_DEBUG=1")
        if (isTRUE(trace)) {
            flags <- paste(flags, "-D MOJOR_TRACE=1")
        }
    }

 # FFI library linking flags
    if (!is.null(.mojor_state$declared_c_functions) &&
        length(.mojor_state$declared_c_functions) >
            0) {
        ffi_libs <- unique(
            Filter(
              Negate(is.null),
              lapply(.mojor_state$declared_c_functions, function(d) d$library)
          )
        )
        for (lib in ffi_libs) {
            lib_dir <- dirname(lib)
            lib_name <- sub("^lib", "", tools::file_path_sans_ext(basename(lib)))
            flags <- paste(
              flags, sprintf(
                "-L%s -l%s", shQuote(lib_dir),
                lib_name
            )
          )
        }
    }

    .mojor_build_shared_lib_if_missing(
        lib_path = lib_path,
        mojo_file = mojo_file,
        flags = flags,
        verbose = verbose
    )

 # Generate C wrapper
    wrapper_c <- file.path(dir, paste0(name, "_wrapper.c"))
    arg_specs <- list(...)
    kernel_arg_specs <- trans$kernel_types
    if (is.null(kernel_arg_specs) ||
        !is.list(kernel_arg_specs)) {
        kernel_arg_specs <- arg_specs
    }
    missing_kernel_specs <- setdiff(names(arg_specs), names(kernel_arg_specs))
    if (length(missing_kernel_specs) > 0) {
        kernel_arg_specs[missing_kernel_specs] <- arg_specs[missing_kernel_specs]
    }
    kernel_arg_specs <- kernel_arg_specs[names(arg_specs)]
    chr_index_hashed_args <- trans$chr_index_hashed_args
    if (is.null(chr_index_hashed_args)) {
        chr_index_hashed_args <- character(0)
    } else {
        chr_index_hashed_args <- as.character(chr_index_hashed_args)
    }
    args <- names(formals(fn))
    chr_index_hashed_args <- intersect(chr_index_hashed_args, args)
    na_check_args <- args
    if (!is.null(trans$na_skip_args) &&
        length(trans$na_skip_args) >
            0) {
        na_check_args <- setdiff(args, trans$na_skip_args)
    }
    .mojor_name_call_args <- function(args_in) {
        if (length(args_in) >
            0 && length(args) >
            0) {
            if (is.null(names(args_in))) {
                names(args_in) <- args[seq_along(args_in)]
            } else {
                blank <- names(args_in) ==
                  ""
                names(args_in)[blank] <- args[seq_along(args_in)][blank]
            }
        }
        args_in
    }
    .mojor_run_na_preflight <- function(...) {
        args_in <- .mojor_name_call_args(list(...))
        .mojor_check_no_na_args(args_in, arg_specs, na_check_args)
        .mojor_check_no_na_reduction_arg(
            args_in, arg_specs, args, reduction_op = trans$scalar_reduction_op,
            reduction_arg = trans$scalar_reduction_arg
        )
    }
    array_args <- args[sapply(arg_specs[args], .mojor_is_array)]
    len_arrays <- trans$len_arrays
    if (is.null(len_arrays))
        len_arrays <- character(0)
    if (length(array_args) ==
        0 && trans$n_source$kind %in% c("array", "iter")) {
        stop(
            "mojor_build: need at least one array arg when loop length comes from an array"
        )
    }
    if (trans$n_source$kind %in% c("array", "iter")) {
        n_expr <- .mojor_len_expr_c(trans$n_source$name, arg_specs[[trans$n_source$name]])
    } else if (trans$n_source$kind == "scalar") {
        if (!is.null(trans$n_source$expr_c)) {
            n_expr <- trans$n_source$expr_c
        } else {
            n_expr <- paste0(trans$n_source$name, "_val")
        }
    } else if (trans$n_source$kind == "expr") {
        n_expr <- trans$n_source$expr_c
    } else if (trans$n_source$kind == "none") {
 # No-loop scalar kernels (e.g. folded any/all constants).
        n_expr <- "1"
    } else {
        stop("mojor_build: unsupported loop length source")
    }
    if (!is.null(trans$out_len_source) &&
        (trans$n_source$kind == "expr" ||
        (trans$n_source$kind == "scalar" &&
        !(trans$n_source$name %in% args)))) {
        if (trans$out_len_source$kind == "array") {
            n_expr <- .mojor_len_expr_c(trans$out_len_source$name, arg_specs[[trans$out_len_source$name]])
        } else if (trans$out_len_source$kind == "scalar") {
            if (!is.null(trans$out_len_source$name) &&
                trans$out_len_source$name %in% args) {
                n_expr <- paste0(trans$out_len_source$name, "_val")
            } else if (!is.null(trans$out_len_source$expr_c)) {
                n_expr <- trans$out_len_source$expr_c
            } else if (!is.null(trans$out_len_source$expr)) {
                len_var_map <- setNames(
                    vapply(len_arrays, .mojor_len_param_name, character(1)),
                    len_arrays
                )
                n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                    trans$n_source$name else NULL
                n_expr <- .mojor_dim_expr_to_c(
                    trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                    n_source_name = n_source_name
                )$c_expr
            } else {
                n_expr <- paste0(trans$out_len_source$name, "_val")
            }
        } else if (trans$out_len_source$kind == "mask_true") {
            n_expr <- paste0("__mojor_mask_true_", trans$out_len_source$name)
        } else if (trans$out_len_source$kind == "exclusion") {
            excl_base <- trans$out_len_source$base
            excl_count <- if (!is.null(trans$out_len_source$excl_count)) trans$out_len_source$excl_count else "1"
            n_expr <- paste0("(", .mojor_len_expr_c(excl_base, arg_specs[[excl_base]]), " - ", excl_count, ")")
        } else if (trans$out_len_source$kind == "nd_exclusion") {
            n_expr <- .mojor_nd_exclusion_len_expr_c(trans$out_len_source, arg_specs)
        } else if (trans$out_len_source$kind == "expr") {
            len_var_map <- setNames(
                vapply(len_arrays, .mojor_len_param_name, character(1)),
                len_arrays
            )
            n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                trans$n_source$name else NULL
            n_expr <- .mojor_dim_expr_to_c(
                trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                n_source_name = n_source_name
            )$c_expr
        }
    }
    n_var <- "__mojor_n"
    out_kind <- trans$out_kind
    out_name <- trans$out_name
    len_check_arrays <- trans$len_check_arrays
    if (is.null(len_check_arrays))
        len_check_arrays <- array_args
    recycle_arrays <- trans$recycle_arrays
    if (is.null(recycle_arrays))
        recycle_arrays <- character(0)
    len_checks_c <- trans$len_checks_c
    if (is.null(len_checks_c))
        len_checks_c <- list()
    nrow_arrays <- trans$nrow_arrays
    if (is.null(nrow_arrays))
        nrow_arrays <- character(0)
    ncol_arrays <- trans$ncol_arrays
    if (is.null(ncol_arrays))
        ncol_arrays <- character(0)
    nd_arrays <- trans$nd_arrays
    if (is.null(nd_arrays))
        nd_arrays <- character(0)
    dim_arrays <- trans$dim_arrays
    if (is.null(dim_arrays))
        dim_arrays <- character(0)
    matrix_dim_arrays <- trans$matrix_dim_arrays
    if (is.null(matrix_dim_arrays))
        matrix_dim_arrays <- character(0)
    broadcast_nd <- isTRUE(trans$broadcast_nd)
    out_matrix <- isTRUE(trans$out_matrix)
    out_nrow_expr <- trans$out_nrow_expr
    out_ncol_expr <- trans$out_ncol_expr
    out_array <- isTRUE(trans$out_array)
    out_dim_exprs <- trans$out_dim_exprs
    out_dim_name <- trans$out_dim_name
    gpu_buf_enabled <- isTRUE(trans$elementwise$gpu_buf_emitted)
    gpu_buf_name <- if (gpu_buf_enabled)
        trans$elementwise$gpu_buf_name else NULL
    gpu_buf_dtype <- if (gpu_buf_enabled)
        trans$elementwise$gpu_buf_dtype else NULL
    gpu_buf_index_mode <- if (gpu_buf_enabled)
        trans$elementwise$gpu_buf_index_mode else NULL
    gpu_buf_matrix_dim_source <- if (gpu_buf_enabled)
        trans$elementwise$gpu_buf_matrix_dim_source else NULL
    gpu_buf_matrix_dim_arrays <- if (gpu_buf_enabled)
        trans$elementwise$gpu_buf_matrix_dim_arrays else character(0)
    if (is.null(gpu_buf_index_mode) ||
        !nzchar(gpu_buf_index_mode))
        gpu_buf_index_mode <- "linear1d"
    if (is.null(gpu_buf_matrix_dim_arrays))
        gpu_buf_matrix_dim_arrays <- character(0)
    if (is.null(gpu_buf_dtype) ||
        !nzchar(gpu_buf_dtype))
        gpu_buf_dtype <- "f32"
    gpu_fallback_ok <- isTRUE(trans$elementwise$enabled) &&
        identical(trans$elementwise$target, "gpu")


    extra_len_types <- if (length(len_arrays) >
        0) {
        rep("int", length(len_arrays))
    } else {
        character(0)
    }
    extra_nrow_types <- if (length(nrow_arrays) >
        0) {
        rep("int", length(nrow_arrays))
    } else {
        character(0)
    }
    extra_ncol_types <- if (length(ncol_arrays) >
        0) {
 # Only include ncol for arrays not already in nrow_arrays
        rep("int", sum(!(ncol_arrays %in% nrow_arrays)))
    } else {
        character(0)
    }
    extra_dim_types <- if (length(dim_arrays) >
        0) {
        as.vector(
            rbind(
                rep("int*", length(dim_arrays)),
                rep("int", length(dim_arrays))
            )
        )
    } else {
        character(0)
    }
    extra_out_types <- character(0)
    if (out_matrix)
        extra_out_types <- c(extra_out_types, "int", "int")
    if (out_array || broadcast_nd)
        extra_out_types <- c(extra_out_types, "int*", "int")
    extra_na_types <- if (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds))
        "int*" else character(0)
    .mojor_validate_kernel_signature_contract(
        trans = trans,
        args = args,
        arg_specs = kernel_arg_specs,
        out_kind = out_kind,
        out_name = out_name,
        rng_needed = rng_needed,
        len_arrays = len_arrays,
        nrow_arrays = nrow_arrays,
        ncol_arrays = ncol_arrays,
        dim_arrays = dim_arrays,
        out_matrix = out_matrix,
        out_array = out_array,
        broadcast_nd = broadcast_nd,
        na_needed = (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds))
    )
    c_lines <- c(
        "#include <R.h>", "#include <R_ext/RS.h>", "#include <Rinternals.h>",
        "#include <R_ext/Rdynload.h>", "#include <Rdefines.h>", "#include <math.h>",
        "#include <stdint.h>", "#include <string.h>", "", paste0(
            "extern void ", name, "(", paste(
                c(
                  lapply(
                    args, function(a) {
                      spec <- kernel_arg_specs[[a]]
                      .mojor_c_type(spec)
                    }
                ),
                  .mojor_c_ptr_type(trans$out_type),
                  "int", if (isTRUE(rng_needed)) "uint64_t*" else character(0),
                  extra_len_types, extra_nrow_types, extra_ncol_types,
                  extra_dim_types, extra_out_types, extra_na_types
              ),
                collapse = ", "
            ),
            ");"
        )
    )

    if (length(chr_index_hashed_args) > 0) {
        c_lines <- c(
            c_lines, "",
            "static int __mojor_fnv1a32_utf8(SEXP ch) {",
            "  const unsigned char* s = (const unsigned char*) Rf_translateCharUTF8(ch);",
            "  uint32_t h = 2166136261u;",
            "  while (*s) {",
            "    h ^= (uint32_t) (*s++);",
            "    h *= 16777619u;",
            "  }",
            "  return (int32_t) h;",
            "}"
        )
    }

    if (isTRUE(rng_needed)) {
        c_lines <- c(
            c_lines, "", "static uint64_t __mojor_rng_state[4] = {0, 0, 0, 0};",
            "static int __mojor_rng_seeded = 0;", "static uint64_t __mojor_splitmix64(uint64_t* x) {",
            "  uint64_t z = (*x += 0x9e3779b97f4a7c15ULL);", "  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;",
            "  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;", "  return z ^ (z >> 31);",
            "}", "static void __mojor_rng_seed_state(uint64_t seed) {",
            "  uint64_t x = seed;", "  __mojor_rng_state[0] = __mojor_splitmix64(&x);",
            "  __mojor_rng_state[1] = __mojor_splitmix64(&x);", "  __mojor_rng_state[2] = __mojor_splitmix64(&x);",
            "  __mojor_rng_state[3] = __mojor_splitmix64(&x);", "  __mojor_rng_seeded = 1;",
            "}", "static void __mojor_rng_ensure_seeded(void) {", "  if (!__mojor_rng_seeded) {",
            "    __mojor_rng_seed_state(0x106689d45497fdb5ULL);", "  }",
            "}"
        )
    }

    c_lines <- c_lines[!sapply(c_lines, is.list)]
    c_lines <- c_lines[!is.na(c_lines)]
    c_lines <- unlist(c_lines)

    c_lines <- c(
        c_lines, "", paste0(
            "SEXP ", name, "_call(SEXP ", paste(args, collapse = ", SEXP "),
            ") {"
        )
    )
 # f32 bitcast helper is only needed for float32 scalar inputs.
    if (any(
        vapply(
            arg_specs, identical, logical(1),
            "f32"
        )
    )) {
        c_lines <- c(
            c_lines, "  union { int i; float f; } __mojor_f32_bits;", "  #define MOJOR_BITS_TO_F32(bits) (__mojor_f32_bits.i = (bits), __mojor_f32_bits.f)"
        )
    }
 # Track modified args that need MAYBE_SHARED check (for in-place
 # operations) Only apply copy-on-modify when semantics='r' (R
 # semantics)
    semantics <- if (!is.null(trans$semantics))
        trans$semantics else "r"
    modified_args <- if (semantics == "r" && !is.null(trans$modified_args))
        trans$modified_args else character(0)

 # Stage B: Memory safety guards in C wrapper
    if (isTRUE(memory_check)) {
        for (a in args) {
            spec <- arg_specs[[a]]
            c_lines <- c(
                c_lines, sprintf(
                  "  if (%s == R_NilValue) error(\"%s: received NULL (memory_check)\");",
                  a, a
              )
            )
            if (.mojor_is_array(spec)) {
                if (identical(spec, "f32[]")) {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (IS_S4_OBJECT(%s)) { SEXP __mc_%s = R_do_slot(%s, install(\"Data\")); if (__mc_%s == R_NilValue || LENGTH(__mc_%s) == 0) error(\"%s: zero-length float32 array (memory_check)\"); }",
                      a, a, a, a, a, a
                  )
                )
                } else {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (LENGTH(%s) == 0) error(\"%s: zero-length array (memory_check)\");",
                      a, a
                  )
                )
                }
            } else {
 # Scalar NA checks
                sexp_type <- .mojor_sexp_type(spec)
                if (sexp_type == "REALSXP") {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) == REALSXP && LENGTH(%s) > 0 && ISNAN(REAL(%s)[0])) error(\"%s: scalar is NA/NaN (memory_check)\");",
                      a, a, a, a
                  )
                )
                } else if (sexp_type == "INTSXP") {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) == INTSXP && LENGTH(%s) > 0 && INTEGER(%s)[0] == NA_INTEGER) error(\"%s: scalar is NA (memory_check)\");",
                      a, a, a, a
                  )
                )
                } else if (sexp_type == "LGLSXP") {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) == LGLSXP && LENGTH(%s) > 0 && LOGICAL(%s)[0] == NA_LOGICAL) error(\"%s: scalar is NA (memory_check)\");",
                      a, a, a, a
                  )
                )
                }
            }
        }
    }

    for (a in args) {
        spec <- arg_specs[[a]]
        if (.mojor_is_array(spec)) {
            sexp_type <- .mojor_sexp_type(spec)
            kernel_c_type <- .mojor_c_type(kernel_arg_specs[[a]])
            is_f64_array <- .mojor_is_array(spec) && identical(.mojor_type_base(spec), "f64")
            if (a %in% chr_index_hashed_args) {
                c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) != STRSXP) error(\"%s: expected character vector\");",
                      a, a
                  ),
                    sprintf("  int __mojor_chr_len_%s = LENGTH(%s);", a, a),
                    sprintf(
                      "  int* %s_ptr = (int*) R_alloc((size_t) __mojor_chr_len_%s, sizeof(int));",
                      a, a
                  ),
                    sprintf("  for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_chr_len_%s; __mojor_i_%s++) {", a, a, a, a),
                    sprintf("    SEXP __mojor_ch_%s = STRING_ELT(%s, __mojor_i_%s);", a, a, a),
                    sprintf(
                      "    if (__mojor_ch_%s == NA_STRING) error(\"%s: character index cannot contain NA\");",
                      a, a
                  ),
                    sprintf(
                      "    %s_ptr[__mojor_i_%s] = __mojor_fnv1a32_utf8(__mojor_ch_%s);",
                      a, a, a
                  ),
                    "  }"
                )
                next
            }
            if (identical(spec, "f32[]")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (!IS_S4_OBJECT(%s)) error(\"%s: expected float32 S4 object\");",
                    a, a
                ),
                  sprintf(
                    "  SEXP %s_data = R_do_slot(%s, install(\"Data\"));",
                    a, a
                ),
                  sprintf(
                    "  if (TYPEOF(%s_data) != INTSXP) error(\"%s: expected float32 data slot\");",
                    a, a
                )
              )
 # MAYBE_SHARED check for modified float32 arguments
                if (a %in% modified_args) {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (MAYBE_SHARED(%s_data)) { %s_data = duplicate(%s_data); }",
                      a, a, a
                  ),
                    sprintf("  PROTECT(%s_data);", a)
                )
                }
                c_lines <- c(
                  c_lines, sprintf("  int __mojor_len_%s = LENGTH(%s_data);", a, a),
                  sprintf(
                    "  float* %s_ptr = (float*) INTEGER(%s_data);", a,
                    a
                )
              )
            } else {
                if (is_f64_array ||
                  identical(kernel_c_type, "float*")) {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) error(\"%s: expected numeric vector\");",
                      a, a, a
                  )
                )
                } else {
                  type_msg <- if (sexp_type == "INTSXP")
                    "integer vector" else if (sexp_type == "LGLSXP")
                    "logical vector" else "numeric vector"
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (TYPEOF(%s) != %s) error(\"%s: expected %s\");",
                      a, sexp_type, a, type_msg
                  )
                )
                }
 # MAYBE_SHARED check for modified arguments
                if (a %in% modified_args) {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (MAYBE_SHARED(%s)) { %s = duplicate(%s); }",
                      a, a, a
                  ),
                    sprintf("  PROTECT(%s);", a)
                )
                }
                if (is_f64_array) {
                  c_lines <- c(
                    c_lines, sprintf("  double* %s_ptr = NULL;", a),
                    sprintf("  if (TYPEOF(%s) == REALSXP) {", a),
                    sprintf("    %s_ptr = REAL(%s);", a, a),
                    "  } else {", sprintf("    int __mojor_len_%s_cast = LENGTH(%s);", a, a),
                    sprintf(
                      "    double* __mojor_%s_cast = (double*) R_alloc((size_t) __mojor_len_%s_cast, sizeof(double));",
                      a, a
                  ),
                    sprintf(
                      "    for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) __mojor_%s_cast[__mojor_i_%s] = (double) INTEGER(%s)[__mojor_i_%s];",
                      a, a, a, a, a, a, a, a
                  ),
                    sprintf("    %s_ptr = __mojor_%s_cast;", a, a),
                    "  }"
                )
                } else if (sexp_type == "LGLSXP") {
                  c_lines <- c(c_lines, sprintf("  int* %s_ptr = LOGICAL(%s);", a, a))
                } else if (kernel_c_type == "int*") {
                  c_lines <- c(c_lines, sprintf("  int* %s_ptr = INTEGER(%s);", a, a))
                } else if (kernel_c_type == "float*") {
                  c_lines <- c(
                    c_lines, sprintf("  int __mojor_len_%s_cast = LENGTH(%s);", a, a),
                    sprintf(
                      "  float* %s_ptr = (float*) R_alloc((size_t) __mojor_len_%s_cast, sizeof(float));",
                      a, a
                  ),
                    sprintf("  if (TYPEOF(%s) == REALSXP) {", a),
                    sprintf(
                      "    for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) REAL(%s)[__mojor_i_%s];",
                      a, a, a, a, a, a, a, a
                  ),
                    "  } else {", sprintf(
                      "    for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) INTEGER(%s)[__mojor_i_%s];",
                      a, a, a, a, a, a, a, a
                  ),
                    "  }"
                )
                } else {
                  c_lines <- c(c_lines, sprintf("  double* %s_ptr = REAL(%s);", a, a))
                }
            }
        } else {
            sexp_type <- .mojor_sexp_type(spec)
            c_type <- .mojor_c_type(spec)
            if (identical(spec, "f32")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (!IS_S4_OBJECT(%s)) error(\"%s: expected float32 scalar\");",
                    a, a
                ),
                  sprintf(
                    "  SEXP %s_data = R_do_slot(%s, install(\"Data\"));",
                    a, a
                ),
                  sprintf(
                    "  if (TYPEOF(%s_data) != INTSXP || LENGTH(%s_data) != 1) error(\"%s: expected float32 scalar data\");",
                    a, a, a
                ),
                  sprintf(
                    "  float %s_val = MOJOR_BITS_TO_F32(INTEGER(%s_data)[0]);",
                    a, a
                )
              )
            } else if (identical(spec, "f64")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");",
                    a, a, a, a
                ),
                  sprintf(
                    "  double %s_val = (TYPEOF(%s) == REALSXP) ? REAL(%s)[0] : (double) INTEGER(%s)[0];",
                    a, a, a, a
                )
              )
            } else {
                type_msg <- if (sexp_type == "INTSXP")
                  "integer scalar" else if (sexp_type == "LGLSXP")
                  "logical scalar" else "numeric scalar"
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (TYPEOF(%s) != %s || LENGTH(%s) != 1) error(\"%s: expected %s\");",
                    a, sexp_type, a, a, type_msg
                )
              )
                if (sexp_type == "LGLSXP") {
                  c_lines <- c(c_lines, sprintf("  int %s_val = LOGICAL(%s)[0];", a, a))
                } else if (c_type == "int") {
                  c_lines <- c(c_lines, sprintf("  int %s_val = INTEGER(%s)[0];", a, a))
                } else {
                  c_lines <- c(c_lines, sprintf("  double %s_val = REAL(%s)[0];", a, a))
                }
            }
        }
    }
    if (!is.null(trans$float_seq_check)) {
        chk <- trans$float_seq_check
        c_lines <- c(
            c_lines, sprintf("  double __mojor_seq_from = %s;", chk$from_c),
            sprintf("  double __mojor_seq_to = %s;", chk$to_c),
            sprintf("  double __mojor_seq_by = %s;", chk$by_c),
            "  if (__mojor_seq_by == 0.0) error(\"seq(): invalid '(to - from)/by'\");",
            "  if ((__mojor_seq_by > 0.0 && __mojor_seq_to < __mojor_seq_from) || (__mojor_seq_by < 0.0 && __mojor_seq_to > __mojor_seq_from)) error(\"seq(): wrong sign in 'by' argument\");"
        )
    }
    if (broadcast_nd) {
        if (length(dim_arrays) ==
            0) {
            c_lines <- c(c_lines, sprintf("  int %s = %s;", n_var, n_expr))
        } else {
            for (a in dim_arrays) {
                dim_decl <- if (a %in% nrow_arrays)
                  NULL else sprintf("  SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a)
                c_lines <- c(
                  c_lines, dim_decl, sprintf("  int* %s_ptr = NULL;", .mojor_dim_param_name(a)),
                  sprintf("  int %s = 1;", .mojor_ndim_param_name(a)),
                  sprintf("  if (dim_%s == R_NilValue) {", a),
                  sprintf(
                    "    %s_ptr = (int*) R_alloc(1, sizeof(int));", .mojor_dim_param_name(a)
                ),
                  sprintf(
                    "    %s_ptr[0] = %s;", .mojor_dim_param_name(a),
                    .mojor_len_expr_c(a, arg_specs[[a]])
                ),
                  "  } else {", sprintf(
                    "    %s_ptr = INTEGER(dim_%s);", .mojor_dim_param_name(a),
                    a
                ),
                  sprintf(
                    "    %s = LENGTH(dim_%s);", .mojor_ndim_param_name(a),
                    a
                ),
                  "  }"
              )
            }
            c_lines <- c(c_lines, "  int __mojor_bc_ndim = 1;")
            for (a in dim_arrays) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (%s > __mojor_bc_ndim) __mojor_bc_ndim = %s;",
                    .mojor_ndim_param_name(a),
                    .mojor_ndim_param_name(a)
                )
              )
            }
            c_lines <- c(
                c_lines, "  int* __mojor_bc_dim = (int*) R_alloc((size_t) __mojor_bc_ndim, sizeof(int));",
                "  for (int i = 0; i < __mojor_bc_ndim; i++) __mojor_bc_dim[i] = 1;"
            )
            for (a in dim_arrays) {
                c_lines <- c(
                  c_lines, sprintf("  for (int di = 0; di < %s; di++) {", .mojor_ndim_param_name(a)),
                  sprintf(
                    "    int axis = __mojor_bc_ndim - %s + di;", .mojor_ndim_param_name(a)
                ),
                  sprintf("    int d = %s_ptr[di];", .mojor_dim_param_name(a)),
                  "    if (d < 0) error(\"broadcast: dim must be non-negative\");",
                  "    int cur = __mojor_bc_dim[axis];", "    if (cur == 1) __mojor_bc_dim[axis] = d;",
                  "    else if (d != 1 && d != cur) error(\"broadcast: incompatible dims\");",
                  "  }"
              )
            }
            c_lines <- c(
                c_lines, sprintf("  int %s = 1;", n_var),
                sprintf(
                  "  for (int i = 0; i < __mojor_bc_ndim; i++) %s *= __mojor_bc_dim[i];",
                  n_var
              )
            )
            if (!out_array && !out_matrix) {
                c_lines <- c(
                  c_lines, sprintf("  int %s = __mojor_bc_ndim;", .mojor_out_ndim_param_name()),
                  sprintf("  int* %s_ptr = __mojor_bc_dim;", .mojor_out_dim_param_name())
              )
            }
        }
    } else {
        c_lines <- c(c_lines, sprintf("  int %s = %s;", n_var, n_expr))
    }
    if (isTRUE(.mojor_state$options$warn_recycle) &&
        !is.null(trans$recycle_warnings) &&
        length(trans$recycle_warnings) >
            0) {
        emitted_recycle_warn <- character(0)
        for (w in trans$recycle_warnings) {
            if (is.null(w$kind))
                next
            if (identical(w$kind, "slice_mask")) {
                mask <- w$mask
                start <- w$start
                end_expr <- w$end_expr_c
                warn_key <- paste(
                  "slice_mask", mask, as.character(start),
                  as.character(end_expr),
                  sep = "|"
              )
                if (warn_key %in% emitted_recycle_warn)
                  next
                emitted_recycle_warn <- c(emitted_recycle_warn, warn_key)
                rhs_count <- paste0("__mojor_rhs_count_", mask)
                slice_end <- paste0("__mojor_slice_end_", mask)
                slice_len <- paste0("__mojor_slice_len_", mask)
                c_lines <- c(
                  c_lines, sprintf("  int %s = 0;", rhs_count),
                  sprintf(
                    "  for (int i = 0; i < LENGTH(%s); i++) if (LOGICAL(%s)[i] == 1) %s++;",
                    mask, mask, rhs_count
                ),
                  sprintf("  int %s = %s;", slice_end, end_expr),
                  sprintf(
                    "  if (%s > %s) %s = %s;", slice_end, n_var, slice_end,
                    n_var
                ),
                  sprintf(
                    "  int %s = %s - %d;", slice_len, slice_end, start -
                      1L
                ),
                  sprintf("  if (%s < 0) %s = 0;", slice_len, slice_len),
                  sprintf(
                    "  if (%s > 0 && (%s %% %s) != 0) warning(\"mojor: compressed RHS recycled\");",
                    rhs_count, slice_len, rhs_count
                )
              )
            } else if (identical(w$kind, "mask_mask")) {
                lhs <- w$lhs_mask
                rhs <- w$rhs_mask
                warn_key <- paste("mask_mask", lhs, rhs, sep = "|")
                if (warn_key %in% emitted_recycle_warn)
                  next
                emitted_recycle_warn <- c(emitted_recycle_warn, warn_key)
                lhs_count <- paste0("__mojor_lhs_count_", lhs)
                rhs_count <- paste0("__mojor_rhs_count_", rhs)
                c_lines <- c(
                  c_lines, sprintf("  int %s = 0;", lhs_count),
                  sprintf(
                    "  for (int i = 0; i < LENGTH(%s); i++) if (LOGICAL(%s)[i] == 1) %s++;",
                    lhs, lhs, lhs_count
                ),
                  sprintf("  int %s = 0;", rhs_count),
                  sprintf(
                    "  for (int i = 0; i < LENGTH(%s); i++) if (LOGICAL(%s)[i] == 1) %s++;",
                    rhs, rhs, rhs_count
                ),
                  sprintf(
                    "  if (%s > 0 && (%s %% %s) != 0) warning(\"mojor: compressed RHS recycled\");",
                    rhs_count, lhs_count, rhs_count
                )
              )
            }
        }
    }
    if (!is.null(trans$out_len_source) &&
        identical(trans$out_len_source$kind, "mask_true")) {
        mask <- trans$out_len_source$name
        count_var <- paste0("__mojor_mask_true_", mask)
        c_lines <- c(
            c_lines, sprintf("  int %s = 0;", count_var),
            sprintf(
                "  for (int i = 0; i < LENGTH(%s); i++) if (LOGICAL(%s)[i] == 1) %s++;",
                mask, mask, count_var
            )
        )
    }
    if (length(len_arrays) >
        0) {
        for (a in len_arrays) {
            if (identical(arg_specs[[a]], "f32[]"))
                next
            c_lines <- c(
                c_lines, sprintf(
                  "  int %s = LENGTH(%s);", .mojor_len_param_name(a),
                  a
              )
            )
        }
    }
    if (length(len_checks_c) >
        0) {
        for (chk in len_checks_c) {
            if (is.null(chk$kind))
                next
            if (identical(chk$kind, "sum_len")) {
                terms <- vapply(
                  chk$parts, function(p) {
                    if (!is.null(p$kind) &&
                      identical(p$kind, "array") &&
                      !is.null(p$name)) {
                      .mojor_len_param_name(p$name)
                    } else if (!is.null(p$kind) &&
                      identical(p$kind, "expr") &&
                      !is.null(p$expr_c)) {
                      p$expr_c
                    } else {
                      "1"
                    }
                  }, character(1)
              )
                sum_expr <- paste(terms, collapse = " + ")
                msg <- if (!is.null(chk$message))
                  chk$message else "length mismatch"
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (%s != (%s)) error(\"%s\");", n_var, sum_expr,
                    msg
                )
              )
            } else if (identical(chk$kind, "rep_len")) {
                if (!is.null(chk$len_value)) {
                  len_expr <- as.character(chk$len_value)
                } else if (!is.null(chk$len_name)) {
                  len_expr <- paste0(chk$len_name, "_val")
                } else {
                  next
                }
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (%s != %s) error(\"rep_len(): output length must match length.out\");",
                    n_var, len_expr
                )
              )
            }
        }
    }
    if (length(nrow_arrays) >
        0) {
        for (a in nrow_arrays) {
            c_lines <- c(
                c_lines, sprintf("  SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
                sprintf(
                  "  if (dim_%s == R_NilValue || LENGTH(dim_%s) < 2) error(\"%s: expected matrix with dim\");",
                  a, a, a
              ),
                sprintf(
                  "  int %s = INTEGER(dim_%s)[0];", .mojor_nrow_param_name(a),
                  a
              )
            )
        }
    }
 # PR-B3: Extract ncol for arrays that need it
    if (length(ncol_arrays) >
        0) {
        for (a in ncol_arrays) {
 # ncol is dim[1], only extract if not already extracted
 # as part of nrow_arrays
            if (!(a %in% nrow_arrays)) {
                c_lines <- c(
                  c_lines, sprintf("  SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
                  sprintf(
                    "  if (dim_%s == R_NilValue || LENGTH(dim_%s) < 2) error(\"%s: expected matrix with dim\");",
                    a, a, a
                )
              )
                c_lines <- c(
                  c_lines, sprintf(
                    "  int %s = INTEGER(dim_%s)[1];", .mojor_ncol_param_name(a),
                    a
                )
              )
            }
        }
    }
    if (length(dim_arrays) >
        0 && !broadcast_nd) {
        for (a in dim_arrays) {
            if (broadcast_nd) {
                c_lines <- c(
                  c_lines, if (a %in% nrow_arrays) NULL else sprintf("  SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
                  sprintf("  int* %s_ptr = NULL;", .mojor_dim_param_name(a)),
                  sprintf("  int %s = 1;", .mojor_ndim_param_name(a)),
                  sprintf("  if (dim_%s == R_NilValue) {", a),
                  sprintf(
                    "    %s_ptr = (int*) R_alloc(1, sizeof(int));", .mojor_dim_param_name(a)
                ),
                  sprintf(
                    "    %s_ptr[0] = %s;", .mojor_dim_param_name(a),
                    .mojor_len_expr_c(a, arg_specs[[a]])
                ),
                  "  } else {", sprintf(
                    "    %s_ptr = INTEGER(dim_%s);", .mojor_dim_param_name(a),
                    a
                ),
                  sprintf(
                    "    %s = LENGTH(dim_%s);", .mojor_ndim_param_name(a),
                    a
                ),
                  "  }"
              )
            } else {
                min_dim <- if (a %in% matrix_dim_arrays)
                  2 else 3
                c_lines <- c(
                  c_lines, if (a %in% nrow_arrays) NULL else sprintf("  SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
                  sprintf(
                    "  if (dim_%s == R_NilValue || LENGTH(dim_%s) < %d) error(\"%s: expected array with dim >= %d\");",
                    a, a, min_dim, a, min_dim
                ),
                  sprintf(
                    "  int* %s_ptr = INTEGER(dim_%s);", .mojor_dim_param_name(a),
                    a
                ),
                  sprintf(
                    "  int %s = LENGTH(dim_%s);", .mojor_ndim_param_name(a),
                    a
                )
              )
            }
        }
    }
    if (out_array) {
        if (!is.null(out_dim_name)) {
            c_lines <- c(
                c_lines, sprintf(
                  "  if (TYPEOF(%s) != INTSXP) error(\"%s: dim must be integer vector\");",
                  out_dim_name, out_dim_name
              ),
                sprintf(
                  "  int %s = LENGTH(%s);", .mojor_out_ndim_param_name(),
                  out_dim_name
              ),
                sprintf(
                  "  if (%s < 1) error(\"%s: dim must have length >= 1\");",
                  .mojor_out_ndim_param_name(), out_dim_name
              ),
                sprintf(
                  "  int* %s_ptr = INTEGER(%s);", .mojor_out_dim_param_name(),
                  out_dim_name
              )
            )
        } else {
            if (is.null(out_dim_exprs) ||
                length(out_dim_exprs) <
                  1) {
                stop("mojor_build: array output requires dim=c(...) with length >= 1")
            }
            c_lines <- c(
                c_lines, sprintf(
                  "  int %s = %d;", .mojor_out_ndim_param_name(), length(out_dim_exprs)
              )
            )
            c_lines <- c(
                c_lines, sprintf(
                  "  int* %s_ptr = (int*) R_alloc((size_t) %s, sizeof(int));",
                  .mojor_out_dim_param_name(), .mojor_out_ndim_param_name()
              )
            )
            for (i in seq_along(out_dim_exprs)) {
                expr_c <- .mojor_dim_expr_to_c(
                  out_dim_exprs[[i]], args, arg_specs, len_var_map = setNames(
                    vapply(len_arrays, .mojor_len_param_name, character(1)),
                    len_arrays
                ),
                  n_source_name = if (trans$n_source$kind %in% c("array", "iter"))
                    trans$n_source$name else NULL
              )$c_expr
                c_lines <- c(
                  c_lines, sprintf(
                    "  %s_ptr[%d] = %s;", .mojor_out_dim_param_name(),
                    i - 1L, expr_c
                )
              )
            }
        }
    }
    c_lines <- c(
        c_lines, sprintf("  if (%s < 0) error(\"n must be non-negative\");", n_var)
    )
    skip_len_checks <- isTRUE(out_matrix) ||
        isTRUE(out_array)
    is_nd_len_exempt <- function(name) {
        spec <- arg_specs[[name]]
        !is.null(spec) && .mojor_is_array(spec) && .mojor_type_ndim(spec) > 1L
    }
    if (!broadcast_nd) {
        if (!skip_len_checks && trans$n_source$kind %in% c("array", "iter")) {
            for (a in setdiff(array_args, trans$n_source$name)) {
                if (!(a %in% len_check_arrays))
                  next
                if (is_nd_len_exempt(a))
                  next
                if (a %in% recycle_arrays)
                  next
                if (a %in% nrow_arrays)
                  next
                if (a %in% ncol_arrays)
                  next
                if (broadcast == "scalar") {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s && %s != 1) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                      a
                  )
                )
                } else if (broadcast %in% c("recycle", "recycle_warn")) {
                  if (broadcast == "recycle_warn") {
                    c_lines <- c(
                      c_lines, sprintf(
                        "  if (%s != %s) warning(\"%s: recycling length %%d to %%d; check intent\", %s, %s);",
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var, a, .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var
                    )
                  )
                  }
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s && (%s %% %s) != 0) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                      a
                  )
                )
                } else {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, a
                  )
                )
                }
            }
            if (!skip_len_checks && !is.null(trans$n_source$extra)) {
                for (a in trans$n_source$extra) {
                  if (!(a %in% array_args))
                    next
                  if (!(a %in% len_check_arrays))
                    next
                  if (is_nd_len_exempt(a))
                    next
                  if (a %in% recycle_arrays)
                    next
                  if (a %in% nrow_arrays)
                    next
                  if (a %in% ncol_arrays)
                    next
                  if (broadcast == "scalar") {
                    c_lines <- c(
                      c_lines, sprintf(
                        "  if (%s != %s && %s != 1) error(\"%s: length mismatch\");",
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                        a
                    )
                  )
                  } else if (broadcast %in% c("recycle", "recycle_warn")) {
                    if (broadcast == "recycle_warn") {
                      c_lines <- c(
                        c_lines, sprintf(
                          "  if (%s != %s) warning(\"%s: recycling length %%d to %%d; check intent\", %s, %s);",
                          .mojor_len_expr_c(a, arg_specs[[a]]),
                          n_var, a, .mojor_len_expr_c(a, arg_specs[[a]]),
                          n_var
                      )
                    )
                    }
                    c_lines <- c(
                      c_lines, sprintf(
                        "  if (%s != %s && (%s %% %s) != 0) error(\"%s: length mismatch\");",
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var, n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                        a
                    )
                  )
                  } else {
                    c_lines <- c(
                      c_lines, sprintf(
                        "  if (%s != %s) error(\"%s: length mismatch\");",
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var, a
                    )
                  )
                  }
                }
            }
        } else if (!skip_len_checks) {
            for (a in array_args) {
                if (!(a %in% len_check_arrays))
                  next
                if (is_nd_len_exempt(a))
                  next
                if (a %in% recycle_arrays)
                  next
                if (a %in% nrow_arrays)
                  next
                if (a %in% ncol_arrays)
                  next
                if (broadcast == "scalar") {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s && %s != 1) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                      a
                  )
                )
                } else if (broadcast %in% c("recycle", "recycle_warn")) {
                  if (broadcast == "recycle_warn") {
                    c_lines <- c(
                      c_lines, sprintf(
                        "  if (%s != %s) warning(\"%s: recycling length %%d to %%d; check intent\", %s, %s);",
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var, a, .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var
                    )
                  )
                  }
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s && (%s %% %s) != 0) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, n_var, .mojor_len_expr_c(a, arg_specs[[a]]),
                      a
                  )
                )
                } else {
                  c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s != %s) error(\"%s: length mismatch\");",
                      .mojor_len_expr_c(a, arg_specs[[a]]),
                      n_var, a
                  )
                )
                }
            }
        }
    }
    out_sexp <- .mojor_sexp_type(trans$out_type)
    out_c <- .mojor_c_ptr_type(trans$out_type)
    if (out_kind == "scalar") {
        c_lines <- c(
            c_lines, sprintf("  SEXP result = PROTECT(allocVector(%s, 1));", out_sexp)
        )
    } else if (out_matrix) {
        nrow_c <- .mojor_dim_expr_to_c(
            out_nrow_expr, args, arg_specs, len_var_map = setNames(
                vapply(len_arrays, .mojor_len_param_name, character(1)),
                len_arrays
            ),
            n_source_name = if (trans$n_source$kind %in% c("array", "iter"))
                trans$n_source$name else NULL
        )$c_expr
        ncol_c <- .mojor_dim_expr_to_c(
            out_ncol_expr, args, arg_specs, len_var_map = setNames(
                vapply(len_arrays, .mojor_len_param_name, character(1)),
                len_arrays
            ),
            n_source_name = if (trans$n_source$kind %in% c("array", "iter"))
                trans$n_source$name else NULL
        )$c_expr
        c_lines <- c(
            c_lines, sprintf("  int %s = %s;", .mojor_out_nrow_param_name(), nrow_c)
        )
        c_lines <- c(c_lines, sprintf("  int __mojor_out_ncol = %s;", ncol_c))
        c_lines <- c(
            c_lines, "  if (__mojor_out_nrow <= 0 || __mojor_out_ncol <= 0) error(\"output matrix dims must be positive\");"
        )
        c_lines <- c(
            c_lines, "  int __mojor_out_len = __mojor_out_nrow * __mojor_out_ncol;"
        )
        if (broadcast_nd) {
            c_lines <- c(
                c_lines, sprintf("  int %s = 2;", .mojor_out_ndim_param_name()),
                sprintf(
                  "  int* %s_ptr = (int*) R_alloc(2, sizeof(int));", .mojor_out_dim_param_name()
              ),
                sprintf("  %s_ptr[0] = __mojor_out_nrow;", .mojor_out_dim_param_name()),
                sprintf("  %s_ptr[1] = __mojor_out_ncol;", .mojor_out_dim_param_name())
            )
        }
        c_lines <- c(
            c_lines, sprintf(
                "  SEXP result = PROTECT(allocVector(%s, __mojor_out_len));",
                out_sexp
            )
        )
    } else if (out_array) {
        c_lines <- c(
            c_lines, "  if (", .mojor_out_ndim_param_name(), " <= 0) error(\"output array dims must be positive\");"
        )
        c_lines <- c(c_lines, "  int __mojor_out_len = 1;")
        c_lines <- c(
            c_lines, paste0(
                "  for (int di = 0; di < ", .mojor_out_ndim_param_name(),
                "; di++) {", " if (", .mojor_out_dim_param_name(), "_ptr[di] <= 0) error(\"output array dims must be positive\");",
                " __mojor_out_len *= ", .mojor_out_dim_param_name(), "_ptr[di]; }"
            )
        )
        c_lines <- c(
            c_lines, sprintf(
                "  SEXP result = PROTECT(allocVector(%s, __mojor_out_len));",
                out_sexp
            )
        )
    } else {
 # Use output length source if available (for histogram/count
 # patterns)
        if (!is.null(trans$out_len_source)) {
            if (trans$out_len_source$kind == "array") {
                out_n_expr <- .mojor_len_expr_c(trans$out_len_source$name, arg_specs[[trans$out_len_source$name]])
            } else if (trans$out_len_source$kind == "scalar") {
                if (!is.null(trans$out_len_source$name) &&
                    trans$out_len_source$name %in% args) {
                    out_n_expr <- paste0(trans$out_len_source$name, "_val")
                } else if (!is.null(trans$out_len_source$expr_c)) {
                    out_n_expr <- trans$out_len_source$expr_c
                } else if (!is.null(trans$out_len_source$expr)) {
                    len_var_map <- setNames(
                        vapply(len_arrays, .mojor_len_param_name, character(1)),
                        len_arrays
                    )
                    n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                        trans$n_source$name else NULL
                    out_n_expr <- .mojor_dim_expr_to_c(
                        trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                        n_source_name = n_source_name
                    )$c_expr
                } else {
                    out_n_expr <- paste0(trans$out_len_source$name, "_val")
                }
            } else if (trans$out_len_source$kind == "mask_true") {
                out_n_expr <- paste0("__mojor_mask_true_", trans$out_len_source$name)
            } else if (trans$out_len_source$kind == "exclusion") {
                excl_base <- trans$out_len_source$base
                excl_count <- if (!is.null(trans$out_len_source$excl_count)) trans$out_len_source$excl_count else "1"
                out_n_expr <- paste0("(", .mojor_len_expr_c(excl_base, arg_specs[[excl_base]]), " - ", excl_count, ")")
            } else if (trans$out_len_source$kind == "nd_exclusion") {
                out_n_expr <- .mojor_nd_exclusion_len_expr_c(trans$out_len_source, arg_specs)
            } else if (trans$out_len_source$kind == "expr") {
                len_var_map <- setNames(
                  vapply(len_arrays, .mojor_len_param_name, character(1)),
                  len_arrays
              )
                n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                  trans$n_source$name else NULL
                out_n_expr <- .mojor_dim_expr_to_c(
                  trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                  n_source_name = n_source_name
              )$c_expr
            } else {
                out_n_expr <- n_var
            }
        } else {
            out_n_expr <- n_var
        }
        c_lines <- c(
            c_lines, sprintf(
                "  SEXP result = PROTECT(allocVector(%s, %s));", out_sexp,
                out_n_expr
            )
        )
    }
    if (out_sexp == "LGLSXP") {
        c_lines <- c(c_lines, "  int* out_ptr = LOGICAL(result);")
    } else if (out_c == "int*") {
        c_lines <- c(c_lines, "  int* out_ptr = INTEGER(result);")
    } else if (!trans$out_type %in% c("f32", "f32[]")) {
        c_lines <- c(c_lines, "  double* out_ptr = REAL(result);")
    }
    if (trans$out_type %in% c("f32", "f32[]")) {
        c_lines <- c(
            c_lines, "  SEXP __mojor_out_data = PROTECT(allocVector(INTSXP, LENGTH(result)));",
            "  setAttrib(__mojor_out_data, R_DimSymbol, getAttrib(result, R_DimSymbol));",
            "  setAttrib(__mojor_out_data, R_DimNamesSymbol, getAttrib(result, R_DimNamesSymbol));",
            "  SEXP __mojor_out_obj = PROTECT(R_do_new_object(R_getClassDef(\"float32\")));",
            "  R_do_slot_assign(__mojor_out_obj, install(\"Data\"), __mojor_out_data);",
            "  int* __mojor_out_bits = INTEGER(__mojor_out_data);", "  float* out_ptr = (float*) __mojor_out_bits;"
        )
    }
    if (out_kind != "scalar") {
        if (out_matrix || out_array) {
            out_len_var <- "__mojor_out_len"
        } else if (!is.null(trans$out_len_source)) {
            if (trans$out_len_source$kind == "array") {
                out_len_var <- .mojor_len_expr_c(trans$out_len_source$name, arg_specs[[trans$out_len_source$name]])
            } else if (trans$out_len_source$kind == "scalar") {
                if (!is.null(trans$out_len_source$name) &&
                    trans$out_len_source$name %in% args) {
                    out_len_var <- paste0(trans$out_len_source$name, "_val")
                } else if (!is.null(trans$out_len_source$expr_c)) {
                    out_len_var <- trans$out_len_source$expr_c
                } else if (!is.null(trans$out_len_source$expr)) {
                    len_var_map <- setNames(
                        vapply(len_arrays, .mojor_len_param_name, character(1)),
                        len_arrays
                    )
                    n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                        trans$n_source$name else NULL
                    out_len_var <- .mojor_dim_expr_to_c(
                        trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                        n_source_name = n_source_name
                    )$c_expr
                } else {
                    out_len_var <- paste0(trans$out_len_source$name, "_val")
                }
            } else if (trans$out_len_source$kind == "mask_true") {
                out_len_var <- paste0("__mojor_mask_true_", trans$out_len_source$name)
            } else if (trans$out_len_source$kind == "exclusion") {
                excl_base <- trans$out_len_source$base
                excl_count <- if (!is.null(trans$out_len_source$excl_count)) trans$out_len_source$excl_count else "1"
                out_len_var <- paste0("(", .mojor_len_expr_c(excl_base, arg_specs[[excl_base]]), " - ", excl_count, ")")
            } else if (trans$out_len_source$kind == "nd_exclusion") {
                out_len_var <- .mojor_nd_exclusion_len_expr_c(trans$out_len_source, arg_specs)
            } else if (trans$out_len_source$kind == "expr") {
                len_var_map <- setNames(
                  vapply(len_arrays, .mojor_len_param_name, character(1)),
                  len_arrays
              )
                n_source_name <- if (trans$n_source$kind %in% c("array", "iter"))
                  trans$n_source$name else NULL
                out_len_var <- .mojor_dim_expr_to_c(
                  trans$out_len_source$expr, args, arg_specs, len_var_map = len_var_map,
                  n_source_name = n_source_name
              )$c_expr
            } else {
                out_len_var <- n_var
            }
        } else {
            out_len_var <- n_var
        }
        zero_line <- if (out_sexp == "REALSXP")
            "0.0" else "0"
        if (trans$out_type %in% c("f32", "f32[]")) {
            c_lines <- c(
                c_lines, sprintf(
                  "  for (int i = 0; i < %s; i++) out_ptr[i] = 0.0f;",
                  out_len_var
              )
            )
        } else {
            c_lines <- c(
                c_lines, sprintf(
                  "  for (int i = 0; i < %s; i++) out_ptr[i] = %s;", out_len_var,
                  zero_line
              )
            )
        }
    }
    if (out_kind != "scalar" && !out_matrix && !out_array && broadcast_nd) {
        c_lines <- c(
            c_lines, paste0(
                "  SEXP dim = PROTECT(allocVector(INTSXP, ", .mojor_out_ndim_param_name(),
                "));"
            ),
            paste0(
                "  for (int di = 0; di < ", .mojor_out_ndim_param_name(),
                "; di++) INTEGER(dim)[di] = ", .mojor_out_dim_param_name(),
                "_ptr[di];"
            ),
            "  setAttrib(result, R_DimSymbol, dim);"
        )
    } else if (out_kind != "scalar" && !out_matrix && !out_array) {
        dim_source <- NULL
        if (trans$n_source$kind %in% c("array", "iter")) {
            dim_source <- trans$n_source$name
        } else if (length(array_args) >
            0) {
            dim_source <- array_args[[1]]
        }
 # PR-B3: Don't copy dim from arrays used only for nrow/ncol
 # helpers
        if (!is.null(dim_source) &&
            ((dim_source %in% nrow_arrays) || (dim_source %in% ncol_arrays))) {
            dim_source <- NULL
        }
 # If output length is explicitly overridden, only preserve
 # dim when it is tied to the same array length source.
        if (!is.null(dim_source) &&
            !is.null(trans$out_len_source)) {
            if (!(identical(trans$out_len_source$kind, "array") &&
                identical(trans$out_len_source$name, dim_source) &&
                !identical(trans$out_len_source$preserve_dim, FALSE))) {
                dim_source <- NULL
            }
        }
        if (!is.null(dim_source)) {
            c_lines <- c(
                c_lines, sprintf("  SEXP dim = getAttrib(%s, R_DimSymbol);", dim_source),
                "  if (dim != R_NilValue) {", "    setAttrib(result, R_DimSymbol, dim);",
                sprintf(
                  "    SEXP dimnames = getAttrib(%s, R_DimNamesSymbol);",
                  dim_source
              ),
                "    if (dimnames != R_NilValue) setAttrib(result, R_DimNamesSymbol, dimnames);",
                "  }"
            )
        }
    } else if (out_kind != "scalar" && out_matrix) {
        c_lines <- c(
            c_lines, "  SEXP dim = PROTECT(allocVector(INTSXP, 2));", paste0("  INTEGER(dim)[0] = ", .mojor_out_nrow_param_name(), ";"),
            "  INTEGER(dim)[1] = __mojor_out_ncol;", "  setAttrib(result, R_DimSymbol, dim);"
        )
    } else if (out_kind != "scalar" && out_array) {
        c_lines <- c(
            c_lines, paste0(
                "  SEXP dim = PROTECT(allocVector(INTSXP, ", .mojor_out_ndim_param_name(),
                "));"
            ),
            paste0(
                "  for (int di = 0; di < ", .mojor_out_ndim_param_name(),
                "; di++) INTEGER(dim)[di] = ", .mojor_out_dim_param_name(),
                "_ptr[di];"
            ),
            "  setAttrib(result, R_DimSymbol, dim);"
        )
    }

 # Add dimnames for matrix output if specified
    if (out_kind != "scalar" && out_matrix && !is.null(trans$matrix_dimnames) &&
        !is.null(trans$matrix_dimnames[[trans$out_name]])) {
        dn <- trans$matrix_dimnames[[trans$out_name]]
        row_names <- dn$row_names
        col_names <- dn$col_names
        row_names_c <- paste(
            vapply(
                row_names, function(s) paste0("\"", s, "\""),
                character(1)
            ),
            collapse = ", "
        )
        col_names_c <- paste(
            vapply(
                col_names, function(s) paste0("\"", s, "\""),
                character(1)
            ),
            collapse = ", "
        )
        c_lines <- c(
            c_lines, "  {", "    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));",
            paste0(
                "    SET_VECTOR_ELT(dimnames, 0, allocVector(STRSXP, ",
                length(row_names),
                "));"
            ),
            paste0(
                "    SET_VECTOR_ELT(dimnames, 1, allocVector(STRSXP, ",
                length(col_names),
                "));"
            ),
            paste0("    const char* row_names[] = {", row_names_c, "};"),
            paste0("    const char* col_names[] = {", col_names_c, "};"),
            paste0(
                "    for (int i = 0; i < ", length(row_names),
                "; i++) SET_STRING_ELT(VECTOR_ELT(dimnames, 0), i, mkChar(row_names[i]));"
            ),
            paste0(
                "    for (int i = 0; i < ", length(col_names),
                "; i++) SET_STRING_ELT(VECTOR_ELT(dimnames, 1), i, mkChar(col_names[i]));"
            ),
            "    setAttrib(result, R_DimNamesSymbol, dimnames);", "    UNPROTECT(1);",
            "  }"
        )
    }
    if (trans$out_type %in% c("f32", "f32[]")) {
        c_lines <- c(
            c_lines, "  setAttrib(__mojor_out_data, R_DimSymbol, getAttrib(result, R_DimSymbol));",
            "  setAttrib(__mojor_out_data, R_DimNamesSymbol, getAttrib(result, R_DimNamesSymbol));"
        )
    }

    call_args <- c()
    for (a in args) {
        spec <- arg_specs[[a]]
        if (.mojor_is_array(spec)) {
            call_args <- c(call_args, paste0(a, "_ptr"))
        } else {
            call_args <- c(call_args, paste0(a, "_val"))
        }
    }
    call_args <- c(call_args, "out_ptr", n_var)
    if (isTRUE(rng_needed))
        call_args <- c(call_args, "__mojor_rng_state")
    if (length(len_arrays) >
        0) {
        for (a in len_arrays) {
            call_args <- c(call_args, .mojor_len_param_name(a))
        }
    }
    if (length(nrow_arrays) >
        0) {
        for (a in nrow_arrays) {
            call_args <- c(call_args, .mojor_nrow_param_name(a))
        }
    }
    if (length(ncol_arrays) >
        0) {
        for (a in ncol_arrays) {
 # Only add ncol if not already added as part of nrow
 # (same array)
            if (!(a %in% nrow_arrays)) {
                call_args <- c(call_args, .mojor_ncol_param_name(a))
            }
        }
    }
    if (length(dim_arrays) >
        0) {
        for (a in dim_arrays) {
            call_args <- c(
                call_args, paste0(
                  .mojor_dim_param_name(a),
                  "_ptr"
              ),
                .mojor_ndim_param_name(a)
            )
        }
    }
    if (out_matrix) {
        call_args <- c(
            call_args, .mojor_out_nrow_param_name(), .mojor_out_ncol_param_name()
        )
    }
    if (out_array || broadcast_nd) {
        call_args <- c(
            call_args, paste0(.mojor_out_dim_param_name(), "_ptr"),
            .mojor_out_ndim_param_name()
        )
    }
    if (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds)) {
        call_args <- c(call_args, "&__mojor_na_flag")
    }

    if (broadcast %in% c("scalar", "recycle", "recycle_warn")) {
        for (a in array_args) {
            len_param <- .mojor_len_param_name(a)
            kernel_c_type <- .mojor_c_type(kernel_arg_specs[[a]])
            c_lines <- c(
                c_lines, sprintf(
                  "  if (%s != %s) {", .mojor_len_expr_c(a, arg_specs[[a]]),
                  n_var
              )
            )
            if (broadcast == "recycle_warn") {
                c_lines <- c(
                    c_lines,
                    sprintf(
                        "    warning(\"%s: recycling length %%d to %%d; check intent\", %s, %s);",
                        a,
                        .mojor_len_expr_c(a, arg_specs[[a]]),
                        n_var
                    )
                )
            }
            if (kernel_c_type == "float*") {
                c_lines <- c(c_lines, sprintf("    float* src_%s = %s_ptr;", a, a))
                c_lines <- c(
                  c_lines, sprintf(
                    "    float* tmp_%s = (float*) R_alloc((size_t) %s, sizeof(float));",
                    a, n_var
                )
              )
                c_lines <- c(
                  c_lines, sprintf("    int len_%s = %s;", a, .mojor_len_expr_c(a, arg_specs[[a]]))
              )
                c_lines <- c(
                  c_lines, sprintf(
                    "    for (int i = 0; i < %s; i++) tmp_%s[i] = src_%s[i %% len_%s];",
                    n_var, a, a, a
                )
              )
                c_lines <- c(c_lines, sprintf("    %s_ptr = tmp_%s;", a, a))
            } else if (kernel_c_type == "int*") {
                c_lines <- c(c_lines, sprintf("    int* src_%s = %s_ptr;", a, a))
                c_lines <- c(
                  c_lines, sprintf(
                    "    int* tmp_%s = (int*) R_alloc((size_t) %s, sizeof(int));",
                    a, n_var
                )
              )
                c_lines <- c(
                  c_lines, sprintf("    int len_%s = %s;", a, .mojor_len_expr_c(a, arg_specs[[a]]))
              )
                c_lines <- c(
                  c_lines, sprintf(
                    "    for (int i = 0; i < %s; i++) tmp_%s[i] = src_%s[i %% len_%s];",
                    n_var, a, a, a
                )
              )
                c_lines <- c(c_lines, sprintf("    %s_ptr = tmp_%s;", a, a))
            } else {
                c_lines <- c(c_lines, sprintf("    double* src_%s = %s_ptr;", a, a))
                c_lines <- c(
                  c_lines, sprintf(
                    "    double* tmp_%s = (double*) R_alloc((size_t) %s, sizeof(double));",
                    a, n_var
                )
              )
                c_lines <- c(
                  c_lines, sprintf("    int len_%s = %s;", a, .mojor_len_expr_c(a, arg_specs[[a]]))
              )
                c_lines <- c(
                  c_lines, sprintf(
                    "    for (int i = 0; i < %s; i++) tmp_%s[i] = src_%s[i %% len_%s];",
                    n_var, a, a, a
                )
              )
                c_lines <- c(c_lines, sprintf("    %s_ptr = tmp_%s;", a, a))
            }
 # Update the length parameter to match the expanded array
 # so bounds checks pass
            if (len_param %in% call_args) {
                c_lines <- c(c_lines, sprintf("    %s = %s;", len_param, n_var))
            }
            c_lines <- c(c_lines, "  }")
        }
    }

    if (isTRUE(rng_needed)) {
        c_lines <- c(c_lines, "  __mojor_rng_ensure_seeded();")
    }
    if (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds)) {
        c_lines <- c(c_lines, "  int __mojor_na_flag = 0;")
    }
    if (isTRUE(out_kind == "scalar") &&
        isTRUE(!is.null(trans$scalar_reduction_op)) &&
        trans$scalar_reduction_op %in% c("min", "max")) {
        empty_warn_msg <- if (identical(trans$scalar_reduction_op, "min")) {
            "no non-missing arguments to min; returning Inf"
        } else {
            "no non-missing arguments to max; returning -Inf"
        }
        empty_scalar_value <- if (identical(trans$scalar_reduction_op, "min"))
            "R_PosInf" else "R_NegInf"
        c_lines <- c(
            c_lines, sprintf("  if (%s == 0) {", n_var),
            sprintf("    warningcall(R_NilValue, \"%s\");", empty_warn_msg),
            sprintf("    out_ptr[0] = %s;", empty_scalar_value),
            "  } else {", sprintf("    %s(%s);", name, paste(call_args, collapse = ", ")),
            "  }"
        )
    } else {
        c_lines <- c(
            c_lines, sprintf("  %s(%s);", name, paste(call_args, collapse = ", "))
        )
    }
    if (isTRUE(trans$index_bounds)) {
        c_lines <- c(
            c_lines, "  if (__mojor_na_flag == 2) error(\"mojor: Index out of bounds\");"
        )
    }
    if (identical(trans$na_mode, "forbid")) {
        c_lines <- c(
            c_lines, "  if (__mojor_na_flag == 1) error(\"mojor: NA detected (na_mode=forbid)\");"
        )
    }

 # Compute base unprotect count for early returns. Empty
 # which.min/which.max path protects both `result` and `empty`.
    n_modified_early <- length(modified_args)
    early_unprotect <- 2 + n_modified_early
    early_unprotect_str <- as.character(early_unprotect)

    if (isTRUE(out_kind == "scalar") &&
        isTRUE(!is.null(trans$scalar_reduction_op)) &&
        trans$scalar_reduction_op %in% c("which.min", "which.max") &&
        out_sexp == "INTSXP") {
        c_lines <- c(
            c_lines, sprintf("  if (out_ptr[0] == 0) {"),
            sprintf("    SEXP empty = PROTECT(allocVector(INTSXP, 0));"),
            sprintf("    UNPROTECT(%s);", early_unprotect_str),
            sprintf("    return empty;"),
            sprintf("  }")
        )
    }
    needs_dim_protect <- (out_matrix || out_array) || (out_kind != "scalar" &&
        broadcast_nd)
    if (trans$out_type %in% c("f32", "f32[]")) {
        if (isTRUE(out_kind == "scalar") &&
            isTRUE(!is.null(trans$scalar_reduction_op)) &&
            trans$scalar_reduction_op %in% c("which.min", "which.max") &&
            out_sexp == "INTSXP") {
 # which.* scalar uses integer output; keep existing
 # result
        } else {
            c_lines <- c(c_lines, "  result = __mojor_out_obj;")
        }
    }

 # Compute UNPROTECT count accounting for modified args (each adds
 # 1 PROTECT)
    n_modified <- length(modified_args)
    base_unprotect <- if (needs_dim_protect)
        2 else 1
    if (trans$out_type %in% c("f32", "f32[]"))
        base_unprotect <- base_unprotect + 2
    unprotect_count <- base_unprotect + n_modified

    c_lines <- c(
        c_lines, sprintf("  UNPROTECT(%d);", unprotect_count),
        "  return result;", "}", ""
    )

    if (isTRUE(rng_needed)) {
        c_lines <- c(
            c_lines, paste0("SEXP ", name, "_rng_seed_call(SEXP r_seed) {"),
            "  uint64_t seed = (uint64_t) asInteger(r_seed);", "  __mojor_rng_seed_state(seed);",
            "  return R_NilValue;", "}", ""
        )
    }

    gpu_call_name <- NULL
    gpu_call_arity <- NULL
    if (isTRUE(gpu_buf_enabled)) {
        gpu_call_name <- paste0(name, "_gpu_buf_", gpu_buf_dtype, "_call")
        gpu_call_arity <- length(args) +
            1
        gpu_n_source <- trans$n_source
        gpu_n_name <- if (!is.null(gpu_n_source) &&
            gpu_n_source$kind %in% c("array", "iter"))
            gpu_n_source$name else NULL
        if (is.null(gpu_n_name) ||
            !(gpu_n_name %in% array_args)) {
            gpu_n_name <- if (length(array_args) >
                0)
                array_args[[1]] else NULL
        }
        if (!is.null(gpu_n_name)) {
            gpu_lines <- .mojor_emit_gpu_buf_wrapper(
                gpu_buf_dtype, gpu_buf_name, gpu_call_name, args, arg_specs,
                array_args, gpu_n_name, broadcast_nd = broadcast_nd, dim_arrays = intersect(dim_arrays, array_args),
                index_mode = gpu_buf_index_mode, matrix_dim_source = gpu_buf_matrix_dim_source,
                matrix_dim_arrays = intersect(gpu_buf_matrix_dim_arrays, array_args)
            )
            c_lines <- c(c_lines, gpu_lines)
        } else {
            gpu_call_name <- NULL
            gpu_call_arity <- NULL
        }
    }

    call_entries <- c(
        paste0(
            "  {\"", name, "_call\", (DL_FUNC) &", name, "_call, ", length(args),
            "},"
        )
    )
    if (isTRUE(rng_needed)) {
        call_entries <- c(
            call_entries, paste0(
                "  {\"", name, "_rng_seed_call\", (DL_FUNC) &", name, "_rng_seed_call, 1},"
            )
        )
    }
    if (!is.null(gpu_call_name)) {
        call_entries <- c(
            call_entries, paste0(
                "  {\"", gpu_call_name, "\", (DL_FUNC) &", gpu_call_name,
                ", ", gpu_call_arity, "},"
            )
        )
    }
    c_lines <- c(
        c_lines, "static const R_CallMethodDef CallEntries[] = {", call_entries,
        "  {NULL, NULL, 0}", "};", "void R_init_mojor_kernel(DllInfo *dll) {",
        "  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);", "  R_useDynamicSymbols(dll, FALSE);",
        "}"
    )

    writeLines(c_lines, wrapper_c)

    .mojor_gpu_require_ready <- function() {
        if (!mojor_is_loaded()) {
            stop("Mojo backend not loaded; call mojor_load() first")
        }
        if (!mojor_has_gpu()) {
            stop("GPU not available")
        }
    }

    gpu_needs_float <- any(
        unlist(arg_specs[array_args]) %in%
            c("f32[]", "f32")
    )
    scalar_f32_args <- setdiff(
        args[vapply(args, function(nm) identical(arg_specs[[nm]], "f32"), logical(1))],
        array_args
    )

    .mojor_coerce_f32_scalars <- function(args_in, context) {
        if (length(scalar_f32_args) == 0L) {
            return(args_in)
        }
        for (nm in scalar_f32_args) {
            if (!nm %in% names(args_in)) {
                next
            }
            val <- args_in[[nm]]
            if (methods::is(val, "float32")) {
                next
            }
            ok_scalar <- is.numeric(val) &&
                length(val) == 1L &&
                is.null(dim(val))
            if (!isTRUE(ok_scalar)) {
                stop(
                    context,
                    ": argument '",
                    nm,
                    "' must be a numeric scalar for spec f32"
                )
            }
            if (!requireNamespace("float", quietly = TRUE)) {
                stop(
                    context,
                    ": float package required for f32 scalar argument '",
                    nm,
                    "'"
                )
            }
            args_in[[nm]] <- float::fl(as.numeric(val))
        }
        args_in
    }

    .mojor_make_gpu_host_wrapper <- function(f) {
        function(...) {
            args_in <- .mojor_name_call_args(list(...))
            .mojor_gpu_require_ready()
            first_gpu <- NULL
            for (nm in array_args) {
                if (!nm %in% names(args_in))
                  next
                val <- args_in[[nm]]
                if (inherits(val, "mojor_gpu_array")) {
                  if (is.null(first_gpu))
                    first_gpu <- val
                  args_in[[nm]] <- mojor_gpu_array_read(val)
                } else if (is.numeric(val)) {
                  args_in[[nm]] <- as.numeric(val)
                } else {
                  stop(
                    "gpu_func: expected mojor_gpu_array or numeric inputs for array args"
                )
                }
            }
            args_in <- .mojor_coerce_f32_scalars(args_in, "gpu_func")
            if (isTRUE(gpu_needs_float) &&
                requireNamespace("float", quietly = TRUE)) {
                for (nm in array_args) {
                  if (nm %in% names(args_in)) {
                    args_in[[nm]] <- float::fl(args_in[[nm]])
                  }
                }
                out_host <- do.call(f, args_in)
                if (float::is.float(out_host)) {
                  out_host <- float::dbl(out_host)
                }
            } else if (isTRUE(gpu_needs_float)) {
                out_host <- do.call(fn, args_in)
            } else {
                out_host <- do.call(f, args_in)
            }
            api <- if (!is.null(first_gpu))
                .mojor_gpu_array_api(first_gpu) else .mojor_gpu_api_resolve("auto")
            out <- mojor_gpu_array(out_host, api = api, dtype = gpu_buf_dtype)
            attr(out, "gpu_status") <- 1L
            out <- .mojor_gpu_route_tag(
                out,
                "cpu_kernel_host",
                reason = "gpu_func used host fallback wrapper"
            )
            out
        }
    }

    .mojor_make_gpu_raw_wrapper <- function(pkg, gpu_call_name, gpu_buf_class, gpu_buf_len_call) {
        function(...) {
            args_in <- .mojor_name_call_args(list(...))
            .mojor_gpu_require_ready()
            first_gpu <- NULL
            args_host <- args_in
            tmp_cast_gpu <- list()
            on.exit(
                .mojor_gpu_free_temp_arrays(tmp_cast_gpu),
                add = TRUE
            )
            for (nm in array_args) {
                if (!nm %in% names(args_in))
                  next
                val <- args_in[[nm]]
                if (inherits(val, "mojor_gpu_array")) {
                  val_dtype <- .mojor_gpu_array_dtype(val)
                  if (!identical(val_dtype, gpu_buf_dtype)) {
                    val_cast <- gpu_cast(val, dtype = gpu_buf_dtype)
                    tmp_cast_gpu[[length(tmp_cast_gpu) + 1L]] <- val_cast
                    val <- val_cast
                  }
                  if (is.null(first_gpu))
                    first_gpu <- val
                  handle <- .mojor_gpu_array_handle(val)
                  if (isTRUE(broadcast_nd) || identical(gpu_buf_index_mode, "matrix2d")) {
                    val_dim <- NULL
                    if (is.list(val) && !is.null(val$dim)) {
                      val_dim <- val$dim
                    }
                    if (is.null(val_dim)) {
                      val_dim <- tryCatch(dim(val), error = function(e) NULL)
                    }
                    if (!is.null(val_dim) && length(val_dim) > 0) {
                      attr(handle, "mojor_dim") <- as.integer(val_dim)
                    }
                  }
                  args_in[[nm]] <- handle
                } else if (inherits(val, gpu_buf_class)) {
                  if (is.null(first_gpu)) {
                    first_gpu <- .mojor_gpu_new_object(
                      list(
                        handle = val, api = .mojor_gpu_api_resolve("auto"),
                        dtype = gpu_buf_dtype, n = 0L
                    ),
                      class = "mojor_gpu_array"
                  )
                  }
                  args_in[[nm]] <- val
                } else {
                  stop("gpu_func: expected mojor_gpu_array inputs for array args")
                }
            }
            call_result <- tryCatch(
                list(
                  ok = TRUE, out_handle = do.call(
                    .Call, c(
                      list(gpu_call_name, .mojor_gpu_ctx_get()),
                      args_in, list(PACKAGE = pkg)
                  )
                )
              ),
                error = function(e) list(ok = FALSE, error = e)
            )
            if (!isTRUE(call_result$ok)) {
                if (!isTRUE(broadcast_nd))
                  stop(call_result$error)
                if (!isTRUE(.mojor_state$gpu_broadcast_nd_fallback_warned)) {
                  message(
                    "gpu_func_raw: broadcast_nd GPU launch failed; falling back to CPU host execution"
                )
                  .mojor_state$gpu_broadcast_nd_fallback_warned <- TRUE
                }
                host_args <- args_host
                host_arrays <- list()
                host_dims <- list()
                for (nm in array_args) {
                  if (!nm %in% names(host_args))
                    next
                  val <- host_args[[nm]]
                  arr <- NULL
                  if (inherits(val, "mojor_gpu_array")) {
                    arr <- mojor_gpu_array_read(val)
                  } else if (inherits(val, gpu_buf_class)) {
                    tmp_gpu <- .mojor_gpu_new_object(
                      list(
                        handle = val, api = .mojor_gpu_api_resolve("auto"),
                        dtype = gpu_buf_dtype, n = 0L
                    ),
                      class = "mojor_gpu_array"
                  )
                    arr <- mojor_gpu_array_read(tmp_gpu)
                  } else if (is.numeric(val)) {
                    arr <- val
                  } else {
                    stop(
                      "gpu_func_raw: CPU fallback expects mojor_gpu_array/gpu buffer/numeric inputs"
                  )
                  }
                  host_arrays[[nm]] <- as.numeric(arr)
                  d <- dim(arr)
                  if (is.null(d))
                    d <- length(host_arrays[[nm]])
                  host_dims[[nm]] <- as.integer(d)
                }
                out_ndim <- 1L
                if (length(host_dims) >
                  0)
                  out_ndim <- max(vapply(host_dims, length, integer(1)))
                out_dim <- rep.int(1L, out_ndim)
                for (nm in names(host_dims)) {
                  d <- host_dims[[nm]]
                  if (length(d) <
                    out_ndim)
                    d <- c(
                      rep.int(1L, out_ndim - length(d)),
                      d
                  )
                  host_dims[[nm]] <- d
                  for (ai in seq_len(out_ndim)) {
                    cur <- out_dim[[ai]]
                    dd <- d[[ai]]
                    if (cur == 1L) {
                      out_dim[[ai]] <- dd
                    } else if (dd != 1L && dd != cur) {
                      stop("gpu_func_raw fallback: incompatible dims")
                    }
                  }
                }
                expand_arg <- function(vec, arg_dim, out_dim) {
                  n_out <- prod(out_dim)
                  if (length(arg_dim) ==
                    length(out_dim) &&
                    all(arg_dim == out_dim)) {
                    return(vec[seq_len(n_out)])
                  }
                  out_vec <- numeric(n_out)
                  out_nd <- length(out_dim)
                  arg_nd <- length(arg_dim)
                  shift <- out_nd - arg_nd
                  for (oi in seq_len(n_out)) {
                    idx <- oi - 1L
                    arg_idx <- 0L
                    stride <- 1L
                    for (axis in seq_len(out_nd)) {
                      od <- out_dim[[axis]]
                      coord <- if (od > 1L)
                        idx%%od else 0L
                      if (od > 1L)
                        idx <- idx%/%od
                      if (axis > shift) {
                        ad <- arg_dim[[axis - shift]]
                        acoord <- if (ad > 1L)
                          coord else 0L
                        arg_idx <- arg_idx + (acoord * stride)
                        stride <- stride * ad
                      }
                    }
                    out_vec[[oi]] <- vec[[arg_idx + 1L]]
                  }
                  out_vec
                }
                for (nm in names(host_arrays)) {
                  host_args[[nm]] <- expand_arg(host_arrays[[nm]], host_dims[[nm]], out_dim)
                }
                out_host <- do.call(fn, host_args)
                if (requireNamespace("float", quietly = TRUE) &&
                  float::is.float(out_host))
                  out_host <- float::dbl(out_host)
                api <- if (!is.null(first_gpu))
                  .mojor_gpu_array_api(first_gpu) else .mojor_gpu_api_resolve("auto")
                out <- mojor_gpu_array(out_host, api = api, dtype = gpu_buf_dtype)
                attr(out, "gpu_status") <- -2L
                out <- .mojor_gpu_route_tag(
                    out,
                    "cpu_broadcast_nd",
                    reason = "broadcast_nd gpu launch failed"
                )
                return(out)
            }
            out_handle <- call_result$out_handle
            class(out_handle) <- c(gpu_buf_class, class(out_handle))
            api <- if (!is.null(first_gpu))
                .mojor_gpu_array_api(first_gpu) else .mojor_gpu_api_resolve("auto")
            out_dim <- NULL
            if (identical(gpu_buf_index_mode, "matrix2d")) {
                dim_name <- gpu_buf_matrix_dim_source
                if ((is.null(dim_name) || !nzchar(dim_name)) &&
                    length(gpu_buf_matrix_dim_arrays) > 0) {
                    dim_name <- gpu_buf_matrix_dim_arrays[[1]]
                }
                if (!is.null(dim_name) &&
                    nzchar(dim_name) &&
                    dim_name %in% names(args_host)) {
                    src_val <- args_host[[dim_name]]
                    src_dim <- NULL
                    if (inherits(src_val, "mojor_gpu_array")) {
                        src_dim <- src_val$dim
                    } else if (is.numeric(src_val)) {
                        src_dim <- dim(src_val)
                    }
                    if (!is.null(src_dim) &&
                        length(src_dim) == 2) {
                        out_dim <- as.integer(src_dim)
                    }
                }
            }
            out <- .mojor_gpu_wrap_buffer_result(
                out_handle, n = as.integer(.mojor_call_bridge(gpu_buf_len_call, out_handle)),
                api = api, dtype = gpu_buf_dtype, include_shape = !is.null(out_dim),
                dim = out_dim, dimnames = NULL, strides = if (is.null(out_dim))
                    NULL else .mojor_dim_strides(out_dim),
                class = if (!is.null(first_gpu) && inherits(first_gpu, "GPUArray"))
                    "GPUArray" else "mojor_gpu_array"
            )
            attr(out, "gpu_status") <- attr(out_handle, "gpu_status")
            out
        }
    }

    .mojor_make_gpu_wrappers <- function(f, pkg) {
        gpu_raw <- NULL
        if (!is.null(gpu_call_name)) {
            gpu_buf_class <- paste0("mojor_gpu_buf_", gpu_buf_dtype)
            gpu_buf_len_call <- paste0("mojor_gpu_buf_", gpu_buf_dtype, "_len")
            gpu_raw <- .mojor_make_gpu_raw_wrapper(pkg, gpu_call_name, gpu_buf_class, gpu_buf_len_call)
        }
        gpu_f <- NULL
        if (!is.null(gpu_raw) ||
            isTRUE(gpu_fallback_ok)) {
            gpu_f <- .mojor_make_gpu_host_wrapper(f)
        }
        list(gpu_raw = gpu_raw, gpu_f = gpu_f)
    }

    .mojor_load_kernel_wrapper <- function(wrapper_so_path) {
        dyn_loader <- get0(
            ".mojor_dyn_load_with_recovery",
            mode = "function",
            inherits = TRUE
        )
        if (is.function(dyn_loader)) {
            dll <- dyn_loader(wrapper_so_path, lib_dir = dir)
        } else {
            with_lib_path <- get0(
                ".mojor_with_ld_library_path",
                mode = "function",
                inherits = TRUE
            )
            if (is.function(with_lib_path)) {
                dll <- with_lib_path(dir, function() dyn.load(wrapper_so_path))
            } else {
                sysname <- Sys.info()[["sysname"]]
                if (identical(sysname, "Linux")) {
                    old <- Sys.getenv("LD_LIBRARY_PATH", "")
                    new <- if (nzchar(old))
                        paste(dir, old, sep = ":") else dir
                    Sys.setenv(LD_LIBRARY_PATH = new)
                    on.exit(
                        Sys.setenv(LD_LIBRARY_PATH = old),
                        add = TRUE
                    )
                } else if (identical(sysname, "Darwin")) {
                    old <- Sys.getenv("DYLD_LIBRARY_PATH", "")
                    new <- if (nzchar(old))
                        paste(dir, old, sep = ":") else dir
                    Sys.setenv(DYLD_LIBRARY_PATH = new)
                    on.exit(
                        Sys.setenv(DYLD_LIBRARY_PATH = old),
                        add = TRUE
                    )
                }
                dll <- dyn.load(wrapper_so_path)
            }
        }
        pkg <- dll[["name"]]
        if (!is.null(pkg) &&
            nzchar(pkg)) {
            .mojor_register_loaded_wrapper(pkg, wrapper_so_path, kind = "kernel")
        }
 # Capture error state at build time (not from mutable global
 # state).
        error_state <- .mojor_state$error_recovery
        f <- .mojor_make_kernel_call_wrapper(
            kernel = name, pkg = pkg, rng_needed = rng_needed, na_mode = trans$na_mode,
            na_preflight = .mojor_run_na_preflight, error_state = error_state,
            wrapper_so = wrapper_so_path
        )
        gpu_wrappers <- .mojor_make_gpu_wrappers(f = f, pkg = pkg)
        list(
            func = f, gpu_func = gpu_wrappers$gpu_f, gpu_func_raw = gpu_wrappers$gpu_raw,
            wrapper_pkg = pkg
        )
    }

    .mojor_build_success_result <- function(load_payload = NULL) {
        result <- list(
            build_dir = dir, kernel = name, name = name, cache_key = key, trans = trans,
            types = trans$types, wrapper_so = wrapper_so, success = TRUE
        )
        if (!is.null(load_payload)) {
            result <- c(load_payload, result)
        }
        if (isTRUE(profile)) {
            result <- .mojor_add_profiling(result, profile_enabled = TRUE)
        }
        result
    }

    wrapper_so <- file.path(dir, paste0(name, "_", key, ".so"))
    if (cache && file.exists(wrapper_so)) {
        if (load) {
            return(.mojor_build_success_result(.mojor_load_kernel_wrapper(wrapper_so)))
        }
        return(.mojor_build_success_result())
    }
    r_bin_sh <- shQuote(.mojor_r_binary_path())
    if (Sys.info()[["sysname"]] == "Darwin") {
        cmd_c <- sprintf(
            "%s CMD SHLIB -o %s %s -L%s -l%s -Wl,-rpath,@loader_path", r_bin_sh, shQuote(wrapper_so),
            shQuote(wrapper_c),
            shQuote(dir),
            lib_kernel_name
        )
    } else {
        cmd_c <- sprintf(
            "%s CMD SHLIB -o %s %s -L%s -l%s -Wl,-rpath,%s", r_bin_sh, shQuote(wrapper_so),
            shQuote(wrapper_c),
            shQuote(dir),
            lib_kernel_name, shQuote(dir)
        )
    }
    if (verbose)
        message("C build: ", cmd_c)
    status <- system(cmd_c)
    if (status != 0)
        stop("C wrapper build failed")

    if (load) {
        return(.mojor_build_success_result(.mojor_load_kernel_wrapper(wrapper_so)))
    }
    .mojor_build_success_result()
}

mojor_cache_info <- function(cache_dir = NULL, include_entries = TRUE) {
    if (!is.logical(include_entries) ||
        length(include_entries) !=
            1 || is.na(include_entries)) {
        stop("mojor_cache_info: include_entries must be TRUE or FALSE")
    }
    cache_dir <- .mojor_resolve_cache_dir(cache_dir)
    jit_policy <- list(
        max_bytes = .mojor_state$options$jit_cache_max_bytes,
        max_entries = .mojor_state$options$jit_cache_max_entries,
        max_age_days = .mojor_state$options$jit_cache_max_age_days
    )
    if (!dir.exists(cache_dir)) {
        jit_index <- list()
        return(
            list(
                path = cache_dir, entries = data.frame(), total_bytes = 0,
                jit_entries = data.frame(), jit_total_bytes = 0, jit_policy = jit_policy,
                jit_compat_summary = .mojor_jit_cache_compat_summary(jit_index)
            )
        )
    }
    entries <- .mojor_cache_entries(cache_dir)
    if (length(entries) ==
        0) {
        info <- data.frame()
        total <- 0
    } else if (isTRUE(include_entries)) {
        entry_info <- .mojor_cache_entries_df(entries)
        info <- data.frame(
            key = basename(entries),
            path = entry_info$path, bytes = entry_info$bytes, stringsAsFactors = FALSE
        )
        total <- sum(info$bytes, na.rm = TRUE)
    } else {
        entry_sizes <- vapply(entries, .mojor_cache_entry_bytes, numeric(1))
        info <- data.frame()
        total <- sum(as.numeric(entry_sizes), na.rm = TRUE)
    }
    jit_index <- .mojor_jit_cache_read(cache_dir)
    compat_summary <- .mojor_jit_cache_compat_summary(
        jit_index,
        current = .mojor_jit_cache_compat_current(mojo_version = NA_character_)
    )
    if (length(jit_index) ==
        0) {
        jit_info <- data.frame()
        jit_total <- 0
    } else if (isTRUE(include_entries)) {
        jit_rows <- lapply(
            names(jit_index),
            function(key) {
                entry <- jit_index[[key]]
                wrapper <- entry$wrapper_so
                bytes <- if (!is.null(wrapper) &&
                    file.exists(wrapper))
                    file.info(wrapper)$size else NA_real_
                mtime <- if (!is.null(wrapper) &&
                    file.exists(wrapper))
                    file.info(wrapper)$mtime else as.POSIXct(NA)
                data.frame(
                    key = key, build_dir = if (is.null(entry$build_dir))
                        NA_character_ else entry$build_dir, wrapper_so = if (is.null(wrapper))
                        NA_character_ else wrapper, bytes = as.numeric(bytes),
                    mtime = as.character(mtime),
                    stringsAsFactors = FALSE
                )
            }
        )
        jit_info <- do.call(rbind, jit_rows)
        jit_total <- sum(jit_info$bytes, na.rm = TRUE)
    } else {
        jit_info <- data.frame()
        jit_sizes <- vapply(
            names(jit_index),
            function(key) {
                entry <- jit_index[[key]]
                wrapper <- if (is.list(entry))
                    entry$wrapper_so else NULL
                if (!is.null(wrapper) &&
                    file.exists(wrapper)) {
                    as.numeric(file.info(wrapper)$size)
                } else {
                    NA_real_
                }
            },
            numeric(1)
        )
        jit_total <- sum(jit_sizes, na.rm = TRUE)
    }
    list(
        path = cache_dir,
        entries = if (isTRUE(include_entries) &&
            nrow(info) >
                0) info[order(info$bytes, decreasing = TRUE),
            ] else info,
        total_bytes = total,
        jit_entries = jit_info,
        jit_total_bytes = jit_total,
        jit_policy = jit_policy,
        jit_compat_summary = compat_summary
    )
}

mojor_cache_print <- function(cache_dir = NULL) {
    info <- mojor_cache_info(cache_dir = cache_dir)
    fmt <- function(x) {
        if (is.null(x) ||
            is.na(x)) {
            return("NA")
        }
        if (x < 1024) {
            return(sprintf("%d B", as.integer(x)))
        }
        if (x < 1024^2) {
            return(sprintf("%.2f KB", x/1024))
        }
        if (x < 1024^3) {
            return(sprintf("%.2f MB", x/1024^2))
        }
        sprintf("%.2f GB", x/1024^3)
    }
    cat("cache_path:", info$path, "\n")
    cat(
        "cache_bytes:", fmt(info$total_bytes),
        "\n"
    )
    cat(
        "jit_entries:", nrow(info$jit_entries),
        "\n"
    )
    cat(
        "jit_bytes:", fmt(info$jit_total_bytes),
        "\n"
    )
    invisible(info)
}
.mojor_hash <- function(x) {
    f <- tempfile(pattern = "mojor_hash_")
    writeLines(x, f)
    h <- as.character(tools::md5sum(f))
    unlink(f)
    h
}

.mojor_hash_serialized <- function(x) {
    f <- tempfile(pattern = "mojor_hash_")
    writeBin(
        serialize(x, NULL),
        f
    )
    h <- as.character(tools::md5sum(f))
    unlink(f)
    h
}

.mojor_safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (grepl("^[0-9]", x))
        x <- paste0("m_", x)
    x
}

.mojor_jit_cache_path <- function(cache_dir) {
    file.path(cache_dir, "jit_index.rds")
}

.mojor_jit_cache_schema_version <- 2L

.mojor_jit_cache_mojo_version <- function() {
    ver <- .mojor_mojo_version_string()
    ver <- ver[nzchar(ver)]
    if (length(ver) ==
        0) {
        return(NA_character_)
    }
    paste(ver, collapse = "\n")
}

.mojor_jit_cache_bridge_path_hash <- function(path) {
    if (is.null(path) ||
        !is.character(path) ||
        length(path) !=
            1 || !nzchar(path)) {
        return("")
    }
    .mojor_hash(normalizePath(path, winslash = "/", mustWork = FALSE))
}

.mojor_jit_cache_compat_current <- function(mojo_version = NULL) {
    has_state <- exists(".mojor_state", envir = .GlobalEnv, inherits = FALSE) &&
        is.environment(.mojor_state)
    bridge_pkg <- ""
    bridge_path <- NULL
    transpile_version <- NA_character_
    api_surface_version <- NA_character_
    if (isTRUE(has_state)) {
        if (!is.null(.mojor_state$bridge_pkg) &&
            nzchar(as.character(.mojor_state$bridge_pkg[[1]]))) {
            bridge_pkg <- as.character(.mojor_state$bridge_pkg[[1]])
        }
        bridge_path <- .mojor_state$bridge_path
        if (!is.null(.mojor_state$transpile_version)) {
            transpile_version <- as.character(.mojor_state$transpile_version[[1]])
        }
        if (!is.null(.mojor_state$api_surface_version)) {
            api_surface_version <- as.character(.mojor_state$api_surface_version[[1]])
        }
    }
    if (is.null(mojo_version)) {
        mojo_version <- .mojor_jit_cache_mojo_version()
    }
    list(
        cache_schema_version = as.integer(.mojor_jit_cache_schema_version),
        transpile_version = transpile_version,
        api_surface_version = api_surface_version,
        r_version = as.character(getRversion()),
        platform = as.character(R.version$platform),
        bridge_pkg = bridge_pkg,
        bridge_path_hash = .mojor_jit_cache_bridge_path_hash(bridge_path),
        mojo_version = as.character(mojo_version[[1]])
    )
}

.mojor_jit_cache_compat_check <- function(entry_compat, current = NULL) {
    if (is.null(entry_compat)) {
        return(list(ok = TRUE, reason = "legacy"))
    }
    if (!is.list(entry_compat)) {
        return(list(ok = FALSE, reason = "compat metadata is not a list"))
    }
    if (is.null(current)) {
        current <- .mojor_jit_cache_compat_current(mojo_version = NA_character_)
    }
    required <- c(
        "cache_schema_version", "transpile_version", "api_surface_version",
        "r_version", "platform", "bridge_pkg", "bridge_path_hash"
    )
    missing <- setdiff(required, names(entry_compat))
    if (length(missing) >
        0) {
        return(list(
            ok = FALSE,
            reason = paste0("missing compat field(s): ", paste(missing, collapse = ", "))
        ))
    }
    normalize <- function(x) {
        if (is.null(x) ||
            length(x) ==
                0 || is.na(x[[1]])) {
            return("")
        }
        as.character(x[[1]])
    }
    for (field in required) {
        lhs <- normalize(entry_compat[[field]])
        rhs <- normalize(current[[field]])
        if (!identical(lhs, rhs)) {
            return(list(ok = FALSE, reason = paste0("compat mismatch: ", field)))
        }
    }
    entry_mojo <- normalize(entry_compat$mojo_version)
    current_mojo <- normalize(current$mojo_version)
    if (nzchar(entry_mojo) &&
        nzchar(current_mojo) &&
        !identical(entry_mojo, current_mojo)) {
        return(list(ok = FALSE, reason = "compat mismatch: mojo_version"))
    }
    list(ok = TRUE, reason = "match")
}

.mojor_jit_cache_compat_summary <- function(jit_index, current = NULL) {
    empty_reasons <- data.frame(
        reason = character(0),
        count = integer(0),
        stringsAsFactors = FALSE
    )
    if (is.null(jit_index) || !is.list(jit_index) ||
        length(jit_index) ==
            0) {
        return(list(
            total_index_entries = 0L,
            stamped_entries = 0L,
            legacy_entries = 0L,
            compatible_entries = 0L,
            incompatible_entries = 0L,
            incompatible_reasons = empty_reasons
        ))
    }
    if (is.null(current)) {
        current <- .mojor_jit_cache_compat_current(mojo_version = NA_character_)
    }
    total <- as.integer(length(jit_index))
    stamped <- 0L
    legacy <- 0L
    compatible <- 0L
    incompatible <- 0L
    reasons <- character(0)
    for (entry in jit_index) {
        compat <- if (is.list(entry))
            entry$compat else NULL
        if (is.null(compat)) {
            legacy <- legacy + 1L
        } else {
            stamped <- stamped + 1L
        }
        check <- .mojor_jit_cache_compat_check(compat, current = current)
        if (isTRUE(check$ok)) {
            compatible <- compatible + 1L
        } else {
            incompatible <- incompatible + 1L
            reasons <- c(reasons, as.character(check$reason[[1]]))
        }
    }
    reason_df <- empty_reasons
    if (length(reasons) >
        0) {
        tab <- sort(table(reasons), decreasing = TRUE)
        reason_df <- data.frame(
            reason = as.character(names(tab)),
            count = as.integer(tab),
            stringsAsFactors = FALSE
        )
    }
    list(
        total_index_entries = total,
        stamped_entries = as.integer(stamped),
        legacy_entries = as.integer(legacy),
        compatible_entries = as.integer(compatible),
        incompatible_entries = as.integer(incompatible),
        incompatible_reasons = reason_df
    )
}

.mojor_resolve_cache_dir <- function(cache_dir = NULL) {
    if (!is.null(cache_dir)) {
        return(cache_dir)
    }
    env_cache_dir <- Sys.getenv("MOJOR_CACHE_DIR", unset = "")
    if (is.character(env_cache_dir) &&
        length(env_cache_dir) == 1L &&
        nzchar(env_cache_dir)) {
        return(env_cache_dir)
    }
    tryCatch(
        tools::R_user_dir("mojor", "cache"),
        error = function(e) tempdir()
    )
}

.mojor_cache_entries <- function(cache_dir) {
    list.dirs(cache_dir, full.names = TRUE, recursive = FALSE)
}

.mojor_cache_entry_bytes <- function(entry_dir) {
    files <- list.files(entry_dir, recursive = TRUE, full.names = TRUE)
    sum(
        file.info(files)$size,
        na.rm = TRUE
    )
}

.mojor_cache_entries_df <- function(entries, include_mtime = FALSE) {
    sizes <- vapply(entries, .mojor_cache_entry_bytes, numeric(1))
    info <- data.frame(
        path = entries, bytes = as.numeric(sizes),
        stringsAsFactors = FALSE
    )
    if (isTRUE(include_mtime)) {
        info$mtime <- as.POSIXct(file.info(entries)$mtime)
    }
    info
}

.mojor_jit_cache_read <- function(cache_dir) {
    if (is.null(cache_dir) ||
        !dir.exists(cache_dir)) {
        return(list())
    }
    path <- .mojor_jit_cache_path(cache_dir)
    if (!file.exists(path)) {
        return(list())
    }
    out <- tryCatch(
        readRDS(path),
        error = function(e) list()
    )
    if (!is.list(out))
        list() else out
}

.mojor_jit_cache_write <- function(cache_dir, key, entry) {
    if (is.null(cache_dir)) {
        return(invisible(NULL))
    }
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    path <- .mojor_jit_cache_path(cache_dir)
    index <- .mojor_jit_cache_read(cache_dir)
    index[[key]] <- entry
    saveRDS(index, path)
    invisible(NULL)
}

mojor_cache_evict <- function(
    cache_dir = NULL, max_bytes = NULL, max_age_days = NULL, max_entries = NULL,
    dry_run = FALSE
) {
    cache_dir <- .mojor_resolve_cache_dir(cache_dir)
    if (!dir.exists(cache_dir)) {
        return(
            invisible(
                list(
                  removed = character(), remaining_bytes = 0, jit_removed = 0,
                  jit_remaining = 0
              )
            )
        )
    }
    if (length(max_bytes) ==
        0 || (length(max_bytes) ==
        1 && is.na(max_bytes)))
        max_bytes <- NULL
    if (length(max_age_days) ==
        0 || (length(max_age_days) ==
        1 && is.na(max_age_days)))
        max_age_days <- NULL
    if (length(max_entries) ==
        0 || (length(max_entries) ==
        1 && is.na(max_entries)))
        max_entries <- NULL
    jit_before <- length(.mojor_jit_cache_read(cache_dir))
    entries <- .mojor_cache_entries(cache_dir)
    entries <- entries[basename(entries) !=
        "jit_index.rds"]
    if (length(entries) ==
        0) {
        jit_after <- length(.mojor_jit_cache_read(cache_dir))
        return(
            invisible(
                list(
                  removed = character(), remaining_bytes = 0, jit_removed = max(0, jit_before - jit_after),
                  jit_remaining = jit_after
              )
            )
        )
    }
    info <- .mojor_cache_entries_df(entries, include_mtime = TRUE)
    info <- info[order(info$mtime, info$bytes, decreasing = FALSE),
        ]
    remove_idx <- rep(FALSE, nrow(info))
    if (!is.null(max_age_days)) {
        cutoff <- Sys.time() - as.difftime(max_age_days, units = "days")
        remove_idx <- remove_idx | info$mtime < cutoff
    }
    if (!is.null(max_entries) &&
        nrow(info) >
            max_entries) {
        extra <- seq_len(
            nrow(info) -
                max_entries
        )
        remove_idx[extra] <- TRUE
    }
    if (!is.null(max_bytes)) {
        total <- sum(info$bytes, na.rm = TRUE)
        i <- 1
        while (total > max_bytes && i <= nrow(info)) {
            remove_idx[i] <- TRUE
            total <- total - info$bytes[i]
            i <- i + 1
        }
    }
    removed <- info$path[remove_idx]
    if (!isTRUE(dry_run)) {
        for (p in removed) unlink(p, recursive = TRUE, force = TRUE)
        .mojor_jit_cache_prune(cache_dir)
    }
    remaining <- sum(info$bytes[!remove_idx], na.rm = TRUE)
    jit_after <- length(.mojor_jit_cache_read(cache_dir))
    invisible(
        list(
            removed = removed, remaining_bytes = remaining, jit_removed = max(0, jit_before - jit_after),
            jit_remaining = jit_after
        )
    )
}

mojor_jit_clear_cache <- function(cache_dir = NULL, remove_builds = FALSE) {
    cache_dir <- .mojor_resolve_cache_dir(cache_dir)
    if (!dir.exists(cache_dir)) {
        return(invisible(FALSE))
    }
    if (isTRUE(remove_builds)) {
        index <- .mojor_jit_cache_read(cache_dir)
        for (entry in index) {
            if (!is.null(entry$build_dir) &&
                dir.exists(entry$build_dir)) {
                unlink(entry$build_dir, recursive = TRUE, force = TRUE)
            }
        }
    }
    path <- .mojor_jit_cache_path(cache_dir)
    if (file.exists(path))
        unlink(path, force = TRUE)
    invisible(TRUE)
}

.mojor_jit_cache_prune <- function(cache_dir) {
    if (is.null(cache_dir) ||
        !dir.exists(cache_dir)) {
        return(invisible(NULL))
    }
    index <- .mojor_jit_cache_read(cache_dir)
    if (length(index) ==
        0) {
        return(invisible(NULL))
    }
    keep <- list()
    for (key in names(index)) {
        entry <- index[[key]]
        if (!is.null(entry$wrapper_so) &&
            file.exists(entry$wrapper_so)) {
            keep[[key]] <- entry
        }
    }
    saveRDS(keep, .mojor_jit_cache_path(cache_dir))
    invisible(NULL)
}

.mojor_jit_cache_enforce <- function(cache_dir) {
    max_bytes <- .mojor_state$options$jit_cache_max_bytes
    max_entries <- .mojor_state$options$jit_cache_max_entries
    max_age_days <- .mojor_state$options$jit_cache_max_age_days
    if (is.null(max_bytes) &&
        is.null(max_entries) &&
        is.null(max_age_days)) {
        return(invisible(NULL))
    }
    mojor_cache_evict(
        cache_dir = cache_dir, max_bytes = max_bytes, max_entries = max_entries,
        max_age_days = max_age_days, dry_run = FALSE
    )
    invisible(NULL)
}

mojor_cache_prune <- function(cache_dir = NULL, dry_run = FALSE) {
    cache_dir <- .mojor_resolve_cache_dir(cache_dir)
    mojor_cache_evict(
        cache_dir = cache_dir, max_bytes = .mojor_state$options$jit_cache_max_bytes,
        max_entries = .mojor_state$options$jit_cache_max_entries, max_age_days = .mojor_state$options$jit_cache_max_age_days,
        dry_run = dry_run
    )
}
