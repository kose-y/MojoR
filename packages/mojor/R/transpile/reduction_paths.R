# Transpile loop reduction-path helpers (consolidated). Contract:
# scalar reduction IR path + argext fallback path for loop transpile.

# Transpile scalar reduction IR-path helper. Contract (ctx):
# attempts scalar reduction IR emission and sets
# `custom_reduction_emitted`.

.mojor_emit_transpile_scalar_reduce_ir_path <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(
            ".mojor_emit_transpile_scalar_reduce_ir_path: ctx must be an environment"
        )
    }
    evalq(
        {
            .mojor_init_transpile_loop_ctx_defaults(environment())
            strict_ir <- isTRUE(.mojor_state$options$ir_only)
 # Step 19: scalar_reduce IR path
 # (sum/product/min/max/which.min/which.max)
            if (!has_while && out_kind == "scalar" && !is.null(scalar_reduce_ir_node) &&
                scalar_reduction_op %in% c("sum", "product", "min", "max", "which.min", "which.max") &&
                length(loop_infos) ==
                  1 && !is.null(scalar_reduction_arg)) {
                reduction_ir <- scalar_reduce_ir_node
                reduction_ir <- .mojor_ir_service_prepare_transpile_ir_stmt(
                  reduction_ir, local_types, dim_map, verify_strict = isTRUE(strict_ir)
              )
                if (isTRUE(strict_ir) &&
                  is.null(reduction_ir)) {
                  stop(
                    "mojor_transpile: strict scalar reduction IR preparation produced no IR",
                    call. = FALSE
                )
                }
                reduction_ir <- .mojor_ir_service_run_transpile_ir_pipeline(
                  ctx = environment(), ir = reduction_ir, opt_level = opt_level,
                  schedule = schedule, enable_fusion = FALSE
              )
                if (isTRUE(strict_ir) &&
                  is.null(reduction_ir)) {
                  stop(
                    "mojor_transpile: strict scalar reduction IR pipeline produced no IR",
                    call. = FALSE
                )
                }
                emit_res <- .mojor_ir_service_type_and_emit_stmt(
                  ir = reduction_ir, local_types = local_types, out_name = out_name,
                  na_mode = na_mode, bounds_check = .mojor_state$options$index_bounds,
                  scalar_name = scalar_init, schedule = schedule, loop_var = NULL,
                  unroll = NULL, allow_empty = !isTRUE(strict_ir)
              )
                ir_typed <- emit_res$ir_typed
                ir_lines <- emit_res$mojo_lines
                if (isTRUE(strict_ir) &&
                  (is.null(ir_lines) ||
                    length(ir_lines) ==
                      0)) {
                  stop(
                    "mojor_transpile: strict scalar reduction emission produced no output",
                    call. = FALSE
                )
                }
                if (!is.null(ir_lines) &&
                  length(ir_lines) >
                    0) {
                  mojo_lines <- c(mojo_lines, ir_lines)
                  custom_reduction_emitted <- TRUE
                }
            }
        }, envir = ctx
    )
}

# Transpile scalar argext reduction-path helper. Contract (ctx):
# emits argext fast path fallback for which.min/which.max and sets
# `custom_reduction_emitted`.

.mojor_emit_transpile_argext_reduction_path <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(
            ".mojor_emit_transpile_argext_reduction_path: ctx must be an environment"
        )
    }
    evalq(
        {
            .mojor_init_transpile_loop_ctx_defaults(environment())
            strict_ir <- isTRUE(.mojor_state$options$ir_only)
            if (isTRUE(strict_ir) &&
                !custom_reduction_emitted && !has_while && out_kind ==
                "scalar" && !is.null(scalar_reduction_op) &&
                scalar_reduction_op %in% c("which.min", "which.max") &&
                length(loop_infos) ==
                  1 && !is.null(scalar_reduction_arg)) {
                stop(
                  "mojor_transpile: strict IR mode forbids argext scalar reduction fallback",
                  call. = FALSE
              )
            }
            if (!custom_reduction_emitted && !has_while && out_kind ==
                "scalar" && !is.null(scalar_reduction_op) &&
                scalar_reduction_op %in% c("which.min", "which.max") &&
                length(loop_infos) ==
                  1 && !is.null(scalar_reduction_arg) &&
                isTRUE(use_argext)) {
                loop_var <- loop_infos[[1]]$var
                arg_spec <- arg_specs[[scalar_reduction_arg]]
                arg_ty <- .mojor_mojo_type(arg_spec)
                if (is.null(arg_ty))
                  arg_ty <- "Float64"
                want_min <- scalar_reduction_op == "which.min"
                zero_val <- if (arg_ty %in% c("Float64", "Float32"))
                  "0.0" else "0"
                na_cond <- NULL
                na_checks <- .mojor_na_checks(
                  call(
                    "[", as.name(scalar_reduction_arg),
                    as.name(loop_var)
                ),
                  c(loop_var),
                  local_types, include_inf = FALSE
              )
                if (length(na_checks) >
                  0)
                  na_cond <- paste(na_checks, collapse = " or ")
                arg_dtype <- if (arg_ty == "Float64")
                  "DType.float64" else if (arg_ty == "Float32")
                  "DType.float32" else "DType.int32"
                indent_lines <- function(lines, ind) paste0(ind, lines)
                fallback_lines <- c(
                  paste0("var best: ", arg_ty, " = ", zero_val),
                  "var has_value = False", paste0(scalar_init, " = Int32(0)"),
                  paste0("for ", loop_var, " in range(1, n_i + 1):"),
                  paste0(
                    "    var v: ", arg_ty, " = ", scalar_reduction_arg,
                    "[", loop_var, " - 1]"
                )
              )
                if (!is.null(na_cond)) {
                  if (identical(na_guard, "ignore")) {
                    fallback_lines <- c(
                      fallback_lines, paste0("    if ", na_cond, ":"),
                      "        continue"
                  )
                  } else {
                    fallback_lines <- c(
                      fallback_lines, paste0("    if ", na_cond, ":"),
                      paste0("        ", scalar_init, " = Int32(-2147483648)"),
                      "        break"
                  )
                  }
                }
                cond <- if (want_min)
                  "v < best" else "v > best"
                fallback_lines <- c(
                  fallback_lines, "    if not has_value:", "        best = v",
                  paste0("        ", scalar_init, " = Int32(", loop_var, ")"),
                  "        has_value = True", paste0("    elif ", cond, ":"),
                  "        best = v", paste0("        ", scalar_init, " = Int32(", loop_var, ")")
              )
                mojo_lines <- c(mojo_lines, "    if n_i == 0:")
                mojo_lines <- c(mojo_lines, paste0("        ", scalar_init, " = Int32(0)"))
                mojo_lines <- c(mojo_lines, "    else:")
                if (!is.null(na_cond)) {
                  mojo_lines <- c(mojo_lines, "        var has_na = False")
                  mojo_lines <- c(
                    mojo_lines, paste0("        for ", loop_var, " in range(1, n_i + 1):")
                )
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "            var v: ", arg_ty, " = ", scalar_reduction_arg,
                      "[", loop_var, " - 1]"
                  )
                )
                  mojo_lines <- c(mojo_lines, paste0("            if ", na_cond, ":"))
                  mojo_lines <- c(mojo_lines, "                has_na = True")
                  mojo_lines <- c(mojo_lines, "                break")
                  mojo_lines <- c(mojo_lines, "        if has_na:")
                  mojo_lines <- c(mojo_lines, indent_lines(fallback_lines, "            "))
                  mojo_lines <- c(mojo_lines, "        else:")
                  mojo_lines <- c(mojo_lines, "            try:")
                  mojo_lines <- c(
                    mojo_lines, indent_lines(
                      c(
                        paste0(
                          "var _mojor_arg_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](n_i)")
                      ),
                        paste0(
                          "var _mojor_arg_tensor = LayoutTensor[mut=False, ",
                          arg_dtype, ", _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](",
                          scalar_reduction_arg, ", _mojor_arg_layout)"
                      ),
                        "var _mojor_out_ptr = alloc[Int64](1)", paste0(
                          "var _mojor_out_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](1)")
                      ),
                        paste0(
                          "var _mojor_out_tensor = LayoutTensor[mut=True, ",
                          "DType.int64, _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin]",
                          "(_mojor_out_ptr, _mojor_out_layout)"
                      ),
                        paste0(
                          if (want_min) "argmin" else "argmax", "(_mojor_arg_tensor, 0, _mojor_out_tensor)"
                      ),
                        paste0(scalar_init, " = Int32(_mojor_out_ptr[0] + 1)"),
                        "_mojor_out_ptr.free()"
                    ),
                      "                "
                  )
                )
                  mojo_lines <- c(mojo_lines, "            except:")
                  mojo_lines <- c(mojo_lines, indent_lines(fallback_lines, "                "))
                } else {
                  mojo_lines <- c(mojo_lines, "        try:")
                  mojo_lines <- c(
                    mojo_lines, indent_lines(
                      c(
                        paste0(
                          "var _mojor_arg_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](n_i)")
                      ),
                        paste0(
                          "var _mojor_arg_tensor = LayoutTensor[mut=False, ",
                          arg_dtype, ", _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](",
                          scalar_reduction_arg, ", _mojor_arg_layout)"
                      ),
                        "var _mojor_out_ptr = alloc[Int64](1)", paste0(
                          "var _mojor_out_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](1)")
                      ),
                        paste0(
                          "var _mojor_out_tensor = LayoutTensor[mut=True, ",
                          "DType.int64, _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin]",
                          "(_mojor_out_ptr, _mojor_out_layout)"
                      ),
                        paste0(
                          if (want_min) "argmin" else "argmax", "(_mojor_arg_tensor, 0, _mojor_out_tensor)"
                      ),
                        paste0(scalar_init, " = Int32(_mojor_out_ptr[0] + 1)"),
                        "_mojor_out_ptr.free()"
                    ),
                      "            "
                  )
                )
                  mojo_lines <- c(mojo_lines, "        except:")
                  mojo_lines <- c(mojo_lines, indent_lines(fallback_lines, "            "))
                }
                custom_reduction_emitted <- TRUE
            }
        }, envir = ctx
    )
}
