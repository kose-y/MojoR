# Transpile prelude/short-circuit helper bundle (consolidated).
# Contract: shared elementwise prelude + short-circuit helpers for
# loop transpile.

# Transpile loop elementwise prelude helper. Contract (ctx):
# initializes elementwise planning fields and computes
# `elementwise_plan`.

.mojor_prepare_transpile_elementwise_prelude <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(
            ".mojor_prepare_transpile_elementwise_prelude: ctx must be an environment"
        )
    }
    evalq(
        {
            elementwise_enabled <- isTRUE(elementwise) &&
                !has_while
            elementwise_emitted <- FALSE
            elementwise_reason <- NULL
            elementwise_inplace <- FALSE
            elementwise_gpu_buf_emitted <- FALSE
            elementwise_gpu_buf_reason <- NULL
            elementwise_gpu_buf_info <- NULL
            elementwise_plan <- NULL
            allow_dynamic_gpu_size <- isTRUE(gpu_jit_elementwise_auto)

            if (elementwise_enabled && length(loop_infos) ==
                1) {
                elementwise_plan <- .mojor_elementwise_plan(
                  loop_infos[[1]], out_name, out_kind, out_type, elementwise_target,
                  elementwise_cpu, non_unit_range, args = args, arg_specs = arg_specs,
                  elementwise_size = elementwise_size, seq_info = loop_infos[[1]]$seq_info,
                  local_types = local_types, name = name, broadcast_nd = broadcast_nd,
                  out_matrix = out_matrix,
                  allow_dynamic_gpu_size = allow_dynamic_gpu_size,
                  elementwise_gpu_layouttensor = elementwise_gpu_layouttensor
              )
                if (!is.null(elementwise_plan$out_type) &&
                    !identical(elementwise_plan$out_type, out_type)) {
                  out_type <- elementwise_plan$out_type
                  if (!is.null(out_name) && identical(out_kind, "vector")) {
                    local_types[[out_name]] <- if (isTRUE(out_matrix)) {
                      .mojor_type_tag_ndim(out_type, 2L)
                    } else if (isTRUE(out_array) && !is.null(out_dim_exprs)) {
                      .mojor_type_tag_ndim(out_type, length(out_dim_exprs))
                    } else {
                      out_type
                    }
                  }
                }
                if (isTRUE(elementwise_plan$ok)) {
                  elementwise_emitted <- TRUE
                  elementwise_inplace <- isTRUE(elementwise_plan$inplace)
                  if (!is.null(elementwise_plan$gpu_buf_info)) {
                    elementwise_gpu_buf_info <- elementwise_plan$gpu_buf_info
                  } else if (!is.null(elementwise_plan$gpu_buf_reason) &&
                    is.null(elementwise_gpu_buf_reason)) {
                    elementwise_gpu_buf_reason <- elementwise_plan$gpu_buf_reason
                  }
                } else if (is.null(elementwise_reason) &&
                  !is.null(elementwise_plan$reason)) {
                  elementwise_reason <- elementwise_plan$reason
                }
            }
        }, envir = ctx
    )
}

# Transpile loop short-circuit prelude helper. Contract (ctx): emits
# direct short-circuit loop prelude when available; returns
# TRUE/FALSE.

.mojor_emit_transpile_short_prelude <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(".mojor_emit_transpile_short_prelude: ctx must be an environment")
    }
    evalq(
        {
            emitted <- FALSE
            if (!custom_reduction_emitted) {
                emitted <- .mojor_emit_transpile_short_loop(environment(), short)
            }
            emitted
        }, envir = ctx
    )
}

# Transpile shared short-circuit loop emission helper. Contract
# (ctx): emits short-circuit loop body for `short_node` and returns
# TRUE/FALSE.

.mojor_emit_transpile_short_loop <- function(ctx, short_node) {
    if (!is.environment(ctx)) {
        stop(".mojor_emit_transpile_short_loop: ctx must be an environment")
    }
    assign(".mojor_short_node__", short_node, envir = ctx)
    on.exit(
        {
            if (exists(".mojor_short_node__", envir = ctx, inherits = FALSE)) {
                rm(".mojor_short_node__", envir = ctx)
            }
        }, add = TRUE
    )
    evalq(
        {
            emitted <- FALSE
            if (!is.null(.mojor_short_node__) &&
                length(loop_infos) ==
                  1 && length(loop_infos[[1]]$blocks) ==
                1) {
                loop_var <- loop_infos[[1]]$var
                if (!is.null(loop_infos[[1]]$iter_value)) {
                  if (!is.null(loop_infos[[1]]$iter_expr_ast)) {
                    .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- build_iter_expr_entry(loop_infos[[1]], loop_var)
                  } else {
                    .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- list(source = loop_infos[[1]]$iter_source, index = loop_var)
                  }
                }
                cond_mojo <- .mojor_expr_to_mojo(
                  .mojor_short_node__$cond, c(loop_var),
                  local_types, in_cond = TRUE
              )
                na_checks <- .mojor_na_checks(
                  .mojor_short_node__$cond, c(loop_var),
                  local_types, include_inf = TRUE
              )
                mojo_lines <- c(
                  mojo_lines, paste0(
                    "    for ", loop_var, " in ", loop_infos[[1]]$range_expr,
                    ":"
                )
              )
                if (length(na_checks) >
                  0) {
                  na_cond <- paste(na_checks, collapse = " or ")
                  mojo_lines <- c(mojo_lines, paste0("        if ", na_cond, ":"))
                  mojo_lines <- c(mojo_lines, paste0("            ", scalar_init, " = -2147483648"))
                  mojo_lines <- c(mojo_lines, paste0("        elif ", cond_mojo, ":"))
                } else {
                  mojo_lines <- c(mojo_lines, paste0("        if ", cond_mojo, ":"))
                }
                mojo_lines <- c(
                  mojo_lines, paste0("            ", scalar_init, " = ", .mojor_short_node__$set)
              )
                mojo_lines <- c(mojo_lines, "            break")
                emitted <- TRUE
            }
            emitted
        }, envir = ctx
    )
}

# Transpile loop short-circuit fallback helper. Contract (ctx):
# emits legacy any/all short-circuit fallback loop when IR loop emit
# fails.

.mojor_emit_transpile_short_ir_fallback <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(
            ".mojor_emit_transpile_short_ir_fallback: ctx must be an environment"
        )
    }
    evalq(
        {
            if (isTRUE(.mojor_state$options$ir_only) &&
                !is.null(short_ir_fallback)) {
                stop(
                  "mojor_transpile: strict IR mode forbids legacy short-circuit fallback",
                  call. = FALSE
              )
            }
            .mojor_emit_transpile_short_loop(environment(), short_ir_fallback)
        }, envir = ctx
    )
}
