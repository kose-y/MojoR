# Transpile IR helper bundle (consolidated). Contract: shared IR
# defaults/layout/prepare/emit/pipeline helpers for loop transpile.

.mojor_assert_transpile_ctx <- function(ctx, fn_name) {
    if (!is.environment(ctx)) {
        stop(fn_name, ": ctx must be an environment")
    }
}

.mojor_set_missing_ctx_defaults <- function(ctx, defaults) {
    for (nm in names(defaults)) {
        if (!exists(nm, envir = ctx, inherits = FALSE)) {
            assign(nm, defaults[[nm]], envir = ctx)
        }
    }
    invisible(NULL)
}

.mojor_transpile_try_stage <- function(expr, strict_ir, strict_prefix) {
    res <- tryCatch(expr, error = function(e) e)
    if (inherits(res, "error")) {
        msg <- conditionMessage(res)
        if (isTRUE(strict_ir) &&
            is.character(strict_prefix) &&
            length(strict_prefix) == 1L &&
            grepl("strict IR emission failed:", strict_prefix, fixed = TRUE) &&
            grepl("unified IR emitter does not support IR kind", msg, fixed = TRUE)) {
            return(NULL)
        }
        if (isTRUE(strict_ir)) {
            stop(
                strict_prefix, msg,
                call. = FALSE
            )
        }
        return(NULL)
    }
    res
}

.mojor_verify_ir_stmt <- function(ir, verify_ctx, strict_verify) {
    if (isTRUE(strict_verify)) {
        .mojor_ir_verify(ir, ctx = verify_ctx)
        return(ir)
    }
    verify_ok <- tryCatch(
        {
            .mojor_ir_verify(ir, ctx = verify_ctx)
            TRUE
        }, error = function(e) FALSE
    )
    if (isTRUE(verify_ok))
        ir else NULL
}

.mojor_ir_service_mode <- function() {
    "unified"
}

.mojor_ir_service_envvar_is_set <- function(var) {
    raw <- Sys.getenv(var, unset = NA_character_)
    !is.na(raw)
}

.mojor_warn_ignored_ir_service_envvars <- function() {
    if (.mojor_ir_service_envvar_is_set("MOJOR_IR_SERVICE") ||
        .mojor_ir_service_envvar_is_set("MOJOR_IR_EMIT_SHADOW")) {
        warning(
            "mojor_transpile: MOJOR_IR_SERVICE and MOJOR_IR_EMIT_SHADOW are deprecated and ignored; unified IR emitter is always used",
            call. = FALSE
        )
    }
    invisible(NULL)
}

.mojor_emit_typed_lir_payload_enabled <- function() {
    raw <- tolower(trimws(Sys.getenv("MOJOR_EMIT_TYPED_LIR_PAYLOAD", "0")))
    raw %in% c("1", "true", "yes", "on")
}

.mojor_ir_emit_node_kind <- function(node) {
    if (is.null(node) ||
        is.null(node$kind) ||
        !is.character(node$kind) ||
        length(node$kind) != 1L ||
        !nzchar(node$kind)) {
        return("unknown")
    }
    node$kind
}

.mojor_ir_serialize_typed_lir <- function(ir_typed) {
    if (!isTRUE(.mojor_emit_typed_lir_payload_enabled()) ||
        is.null(ir_typed)) {
        return(NULL)
    }
    payload <- paste(capture.output(dput(ir_typed)), collapse = "\n")
    list(
        schema_version = 1L,
        format = "dput",
        service = "unified",
        payload = payload
    )
}

.mojor_ir_emit_stmt_with_service <- function(
    ir_typed, indent, zero_based_vars, out_name, na_guard, bounds_check,
    loop_var, scalar_name, type_env, unroll, schedule
) {
    kind <- .mojor_ir_emit_node_kind(ir_typed)
    if (identical(kind, "unknown")) {
        stop(
            "mojor_transpile: unified IR emitter requires a typed node with a non-empty kind",
            call. = FALSE
        )
    }
    emitted <- .mojor_ir_stmt_emit(
        ir_typed,
        indent = indent,
        zero_based_vars = zero_based_vars,
        out_name = out_name,
        na_guard = na_guard,
        bounds_check = bounds_check,
        loop_var = loop_var,
        scalar_name = scalar_name,
        type_env = type_env,
        unroll = unroll,
        schedule = schedule
    )
    if (is.null(emitted)) {
        stop(
            "mojor_transpile: strict IR emission produced empty output; unified IR emitter does not support IR kind '", kind, "'",
            call. = FALSE
        )
    }
    emitted
}

.mojor_assert_no_unresolved_selector_symbols <- function(mojo_lines) {
    if (is.null(mojo_lines) || length(mojo_lines) == 0L) {
        return(invisible(NULL))
    }
    combined <- paste(mojo_lines, collapse = "\n")
    if (grepl("__pos_vec_sel__", combined, fixed = TRUE)) {
        stop(
            "mojor_transpile: unresolved selector marker leaked into emitted code",
            call. = FALSE
        )
    }
    invisible(NULL)
}

# Transpile loop context defaults helper. Contract (ctx):
# initializes optional split-module context fields used by loop
# helpers.

.mojor_init_transpile_loop_ctx_defaults <- function(ctx) {
    .mojor_assert_transpile_ctx(ctx, ".mojor_init_transpile_loop_ctx_defaults")
    .mojor_set_missing_ctx_defaults(
        ctx, list(
            custom_reduction_emitted = FALSE, run_loop_emit = FALSE, short_ir_fallback = NULL,
            elementwise_plan = NULL, elementwise_gpu_buf_emitted = FALSE,
            elementwise_gpu_buf_reason = NULL, elementwise_gpu_buf_info = NULL,
            gpu_jit_gate_info = NULL
        )
    )
}

# Transpile layout-context helper. Contract (ctx): builds loop
# lowering layout context and updates `schedule$type_env`.

.mojor_build_transpile_layout_ctx <- function(ctx) {
    .mojor_assert_transpile_ctx(ctx, ".mojor_build_transpile_layout_ctx")
    evalq(
        {
            layout_ctx <- .mojor_ir_layout_ctx(
                n_var = "n_i", nrow_var = .mojor_state$current_out_nrow_var,
                ncol_var = .mojor_state$current_out_ncol_var, dim_var_map = .mojor_state$current_dim_var_map,
                ndim_var_map = .mojor_state$current_ndim_var_map, nrow_var_map = .mojor_state$current_nrow_var_map,
                ncol_var_map = .mojor_state$current_ncol_var_map, len_var_map = .mojor_state$current_len_var_map,
                tensor_map = .mojor_state$current_tensor_map, index_base = .mojor_state$current_index_base,
                array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
                fusion_allow_broadcast_nd_identity = isTRUE(.mojor_state$options$fusion_allow_broadcast_nd_identity)
            )
            layout_ctx$gpu_jit_mode <- if (exists("gpu_jit_mode", inherits = TRUE))
                gpu_jit_mode else .mojor_state$options$gpu_jit_mode
            layout_ctx$n_source_name <- .mojor_state$current_n_source_name
            layout_ctx$type_env <- local_types
            layout_ctx$reduction_mode <- reduction
            layout_ctx$out_name <- if (exists("out_name", inherits = TRUE))
                out_name else NULL
            schedule$type_env <- local_types
            layout_ctx
        }, envir = ctx
    )
}

# Transpile IR statement helpers. Contract: shared IR
# prepare/fusion/type+emit utilities for split loop transpile
# modules.

.mojor_prepare_transpile_ir_stmt <- function(ir, local_types, dim_map, verify_strict = TRUE) {
    if (is.null(ir)) {
        return(NULL)
    }
    strict_ir <- isTRUE(.mojor_state$options$ir_only)
    effective_verify_strict <- isTRUE(verify_strict) ||
        isTRUE(strict_ir)
    ir <- .mojor_transpile_try_stage(
        .mojor_ir_normalize(ir),
        strict_ir = strict_ir, strict_prefix = "mojor_transpile: strict IR normalize failed: "
    )
    if (is.null(ir)) {
        return(NULL)
    }
    if (!is.null(ir))
        ir <- .mojor_ir_fold_type_stmt(ir, local_types)
    if (!is.null(ir))
        ir <- .mojor_ir_lower_dim_stmt(ir, dim_map)
    if (!is.null(ir)) {
        verify_ctx <- list(
            ir_only = isTRUE(.mojor_state$options$ir_only),
            type_env = local_types
        )
        ir <- .mojor_verify_ir_stmt(ir, verify_ctx = verify_ctx, strict_verify = effective_verify_strict)
    }
    ir
}

.mojor_apply_transpile_loop_fusion <- function(ir, layout_ctx) {
    if (is.null(ir) ||
        is.null(ir$kind) ||
        !(ir$kind %in% c("loop", "while", "repeat"))) {
        return(ir)
    }

    fusion_enabled <- TRUE
    if (!is.null(.mojor_state$current_fusion)) {
        fusion_enabled <- .mojor_state$current_fusion
    }
    if (!isTRUE(fusion_enabled)) {
        return(ir)
    }

    stmts <- if (ir$kind == "loop") {
        list(ir)
    } else if (ir$kind == "block") {
        ir$stmts
    } else {
        list(ir)
    }

    fused_stmts <- .mojor_ir_fusion_pass(stmts, layout_ctx)
    if (length(fused_stmts) ==
        1 && !is.null(fused_stmts[[1]])) {
        return(fused_stmts[[1]])
    }
    if (length(fused_stmts) >
        0) {
        return(.mojor_ir_block(fused_stmts))
    }
    ir
}

.mojor_transpile_type_and_emit_stmt <- function(
    ir, local_types, out_name, na_mode, bounds_check, scalar_name, schedule,
    loop_var = NULL, unroll = NULL, require_kinds = NULL, indent = "    ",
    zero_based_vars = character(0),
    allow_empty = FALSE
) {
    ir_typed <- NULL
    mojo_lines <- NULL
    strict_ir <- isTRUE(.mojor_state$options$ir_only)
    if (!is.null(ir) &&
        (is.null(require_kinds) ||
            (!is.null(ir$kind) &&
                ir$kind %in% require_kinds))) {
        ir_typed <- .mojor_transpile_try_stage(
            .mojor_ir_type_check_stmt(ir, local_types),
            strict_ir = strict_ir, strict_prefix = "mojor_transpile: strict IR type check failed: "
        )
        if (is.null(ir_typed) &&
            isTRUE(strict_ir) &&
            !isTRUE(allow_empty)) {
            stop(
                "mojor_transpile: strict IR type check produced no typed IR",
                call. = FALSE
            )
        }
        if (!is.null(ir_typed))
            ir_typed <- .mojor_ir_annotate_typed(ir_typed, local_types)
        if (!is.null(ir_typed)) {
            emit_zero_based_vars <- if (is.null(zero_based_vars))
                character(0) else zero_based_vars
            mojo_lines <- .mojor_transpile_try_stage(
                .mojor_ir_emit_stmt_with_service(
                    ir_typed = ir_typed,
                    indent = indent,
                    zero_based_vars = emit_zero_based_vars,
                    out_name = out_name,
                    na_guard = na_mode,
                    bounds_check = isTRUE(bounds_check),
                    loop_var = loop_var,
                    scalar_name = scalar_name,
                    type_env = local_types,
                    unroll = unroll,
                    schedule = schedule
                ),
                strict_ir = strict_ir, strict_prefix = "mojor_transpile: strict IR emission failed: "
            )
        }
        if (isTRUE(strict_ir) &&
            !isTRUE(allow_empty) &&
            (is.null(mojo_lines) ||
                length(mojo_lines) ==
                  0 || any(!nzchar(mojo_lines)))) {
            stop(
                "mojor_transpile: strict IR emission produced empty output",
                call. = FALSE
            )
        }
        .mojor_assert_no_unresolved_selector_symbols(mojo_lines)
    }
    list(
        ir_typed = ir_typed,
        mojo_lines = mojo_lines
    )
}

# Transpile IR pipeline helper. Contract (ctx): runs shared IR
# post-build pipeline (metadata attach, optimize, lower, schedule,
# optional fusion) and returns transformed IR.

.mojor_run_transpile_ir_pipeline <- function(
    ctx, ir, opt_level, schedule, elementwise_plan = NULL, elementwise_target = NULL,
    enable_fusion = FALSE
) {
    .mojor_assert_transpile_ctx(ctx, ".mojor_run_transpile_ir_pipeline")
    if (is.null(ir)) {
        return(NULL)
    }
    .has_suspicious_licm_block <- function(node) {
        if (is.null(node) ||
            is.null(node$kind) ||
            !identical(node$kind, "block")) {
            return(FALSE)
        }
        if (is.null(node$stmts) ||
            length(node$stmts) ==
                0) {
            return(FALSE)
        }
        has_loop <- any(
            vapply(
                node$stmts, function(st) {
                  !is.null(st$kind) &&
                    st$kind %in% c("loop", "while", "repeat")
                }, logical(1)
            )
        )
        if (!isTRUE(has_loop)) {
            return(FALSE)
        }
        any(
            vapply(
                node$stmts, function(st) {
                  if (is.null(st$kind) ||
                    !identical(st$kind, "assign")) {
                    return(FALSE)
                  }
                  if (is.null(st$lhs) ||
                    is.null(st$lhs$kind) ||
                    !identical(st$lhs$kind, "var")) {
                    return(FALSE)
                  }
                  startsWith(st$lhs$name, "__licm_tmp")
                }, logical(1)
            )
        )
    }

    if (!is.null(ir$kind) &&
        ir$kind == "loop" && !is.null(elementwise_plan) &&
        isTRUE(elementwise_plan$ok)) {
        if (is.null(ir$metadata))
            ir$metadata <- list()
        ir$metadata$elementwise <- list(
            enabled = TRUE, target = elementwise_target, inplace = isTRUE(elementwise_plan$inplace)
        )
    }

    ir_pre_opt <- ir
    ir <- .mojor_ir_optimize(ir, opt_level = opt_level)
    if (!is.null(ir) &&
        isTRUE(opt_level > 1L) &&
        .has_suspicious_licm_block(ir)) {
 # Guard against invalid LICM hoists that reference loop-local
 # vars from preheaders.
        ir <- .mojor_ir_optimize(ir_pre_opt, opt_level = 1L)
    }
    if (is.null(ir)) {
        return(NULL)
    }

    layout_ctx <- .mojor_build_transpile_layout_ctx(ctx)
    ir <- .mojor_ir_lower(ir, ctx = layout_ctx)
    ir <- .mojor_ir_schedule(ir, schedule = schedule)
    if (isTRUE(enable_fusion)) {
        ir <- .mojor_apply_transpile_loop_fusion(ir, layout_ctx)
    }
    ir
}

.mojor_ir_service_prepare_transpile_ir_stmt <- function(...) {
    .mojor_prepare_transpile_ir_stmt(...)
}

.mojor_ir_service_run_transpile_ir_pipeline <- function(...) {
    .mojor_run_transpile_ir_pipeline(...)
}

.mojor_ir_service_type_and_emit_stmt <- function(...) {
    .mojor_transpile_type_and_emit_stmt(...)
}
