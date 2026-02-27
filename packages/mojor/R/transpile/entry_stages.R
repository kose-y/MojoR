# Transpile entrypoint phases: state context, postamble finalization,
# SSA backend.

.mojor_prune_schedule_nulls <- function(schedule) {
    if (is.null(schedule) ||
        !is.list(schedule)) {
        return(list())
    }
    schedule[!vapply(schedule, is.null, logical(1))]
}

.mojor_assert_scalar_logical <- function(value, arg_name, context = "mojor_transpile") {
    if (!is.logical(value) ||
        length(value) !=
            1) {
        stop(context, ": ", arg_name, " must be TRUE or FALSE")
    }
    value
}

.mojor_assert_scalar_logical_or_null <- function(value, arg_name, context = "mojor_transpile") {
    if (is.null(value)) {
        return(NULL)
    }
    .mojor_assert_scalar_logical(value, arg_name = arg_name, context = context)
}

.mojor_parse_positive_integer_or_null <- function(value, arg_name, context = "mojor_transpile") {
    if (is.null(value)) {
        return(NULL)
    }
    if (!is.numeric(value) ||
        length(value) !=
            1 || value <= 0) {
        stop(context, ": ", arg_name, " must be a positive integer or NULL")
    }
    out <- as.integer(value)
    if (is.na(out) ||
        out <= 0) {
        stop(context, ": ", arg_name, " must be a positive integer or NULL")
    }
    out
}

.mojor_apply_auto_schedule_policy <- function(
    schedule, reduction_mode, unroll, tile, opt_level, loop_infos, out_kind,
    out_matrix, parallel_safe, simd_safe, scalar_reduction_eligible, scalar_reduction_op,
    scalar_reduction_arg, arg_specs, broadcast_nd, unroll_explicit = FALSE,
    reduction_explicit = FALSE, tile_explicit = FALSE
) {
    reduction_schedule <- if (!is.null(reduction_mode) &&
        reduction_mode %in% c("tree", "simd"))
        reduction_mode else NULL
    reduction_effective <- reduction_mode
    unroll_effective <- unroll
    tile_effective <- tile
    policy <- list(
        reduction = list(mode = "explicit_or_linear", reason = NULL),
        unroll = list(mode = "explicit_or_none", reason = NULL),
        tile = list(mode = "explicit_or_none", reason = NULL)
    )

    if (isTRUE(reduction_mode == "auto") &&
        !isTRUE(reduction_explicit)) {
        if (isTRUE(scalar_reduction_eligible)) {
            arg_spec <- if (!is.null(scalar_reduction_arg))
                arg_specs[[scalar_reduction_arg]] else NULL
            simd_dtype_ok <- !is.null(arg_spec) &&
                arg_spec %in% c("f64[]", "f32[]", "i32[]")
            simd_op_ok <- !is.null(scalar_reduction_op) &&
                scalar_reduction_op %in% c("sum", "product", "min", "max", "which.min", "which.max")
            if (isTRUE(simd_safe) &&
                isTRUE(simd_dtype_ok) &&
                isTRUE(simd_op_ok)) {
                reduction_schedule <- "simd"
                reduction_effective <- "simd"
                policy$reduction$mode <- "auto"
                policy$reduction$reason <- "scalar reduction eligible and simd-safe"
            } else {
                reduction_schedule <- "tree"
                reduction_effective <- "tree"
                policy$reduction$mode <- "auto"
                policy$reduction$reason <- "scalar reduction eligible but simd constraints not met"
            }
        } else {
            policy$reduction$mode <- "auto"
            policy$reduction$reason <- "no scalar reduction candidate"
        }
    } else if (isTRUE(reduction_mode == "linear")) {
        policy$reduction$mode <- "linear"
        policy$reduction$reason <- "explicit linear reduction requested"
    }

    if (is.null(unroll_effective) &&
        !isTRUE(unroll_explicit)) {
        if (!is.null(opt_level) &&
            !is.na(opt_level) &&
            opt_level >= 2 && length(loop_infos) ==
            1 && !isTRUE(broadcast_nd)) {
            if (identical(out_kind, "vector") &&
                isTRUE(parallel_safe)) {
                unroll_effective <- 4L
                policy$unroll$mode <- "auto"
                policy$unroll$reason <- "single parallel-safe vector loop"
            } else if (isTRUE(scalar_reduction_eligible)) {
                unroll_effective <- 2L
                policy$unroll$mode <- "auto"
                policy$unroll$reason <- "scalar reduction loop"
            }
        }
    }

    if (is.null(tile_effective) &&
        !isTRUE(tile_explicit)) {
        if (!is.null(opt_level) &&
            !is.na(opt_level) &&
            opt_level >= 3 && isTRUE(out_matrix) &&
            length(loop_infos) ==
                1 && !isTRUE(broadcast_nd)) {
            has_nested <- any(
                vapply(
                  loop_infos[[1]]$blocks, function(s) {
                    is.call(s) &&
                      as.character(s[[1]]) ==
                        "for"
                  }, logical(1)
              )
            )
            if (isTRUE(has_nested)) {
                tile_effective <- c(32L, 32L)
                policy$tile$mode <- "auto"
                policy$tile$reason <- "nested matrix loop at high opt level"
            }
        }
    }

    schedule_eff <- if (is.null(schedule) ||
        !is.list(schedule))
        list() else schedule
    schedule_eff$unroll <- unroll_effective
    schedule_eff$tile <- tile_effective
    schedule_eff$reduction <- reduction_schedule
    schedule_eff <- .mojor_prune_schedule_nulls(schedule_eff)

    list(
        schedule = schedule_eff, unroll_effective = unroll_effective, tile_effective = tile_effective,
        reduction_effective = reduction_effective, reduction_schedule = reduction_schedule,
        policy = policy
    )
}

.mojor_prepare_transpile_entry_options <- function(
    fn, na_mode, semantics, emit_ir, emit_ssa_backend, opt_level, unroll,
    reduction, bounds_check, index_base, array_layout, tile, simd_mode,
    assume_aligned, elementwise, elementwise_target, elementwise_size,
    elementwise_cpu, elementwise_gpu_layouttensor, gpu_jit_mode, broadcast,
    parallel, allow_in_place, fusion
) {
    na_mode <- .mojor_effective_na_mode(na_mode)
    semantics <- match.arg(semantics, c("r", "raw"))
    emit_ir <- .mojor_assert_scalar_logical(emit_ir, "emit_ir")
    emit_ssa_backend <- .mojor_assert_scalar_logical(emit_ssa_backend, "emit_ssa_backend")
    ir_dump <- if (isTRUE(emit_ir))
        .mojor_ir_dump(fn) else NULL
    if (is.null(opt_level)) {
        opt_level <- .mojor_state$`%||%`(.mojor_state$options$opt_level, 2L)
    }
    opt_level <- suppressWarnings(as.integer(opt_level))
    if (is.na(opt_level) ||
        opt_level < 0 || opt_level > 3) {
        stop(
            "mojor_transpile: opt_level must be an integer between 0 and 3 (or NULL)"
        )
    }
    unroll_explicit <- !is.null(unroll)
    reduction_explicit <- !is.null(reduction)
    tile_explicit <- !is.null(tile)

    if (!is.null(unroll)) {
        unroll <- as.integer(unroll)
        if (is.na(unroll) ||
            unroll < 1 || unroll > 16) {
            stop("mojor_transpile: unroll must be an integer between 1 and 16")
        }
    }

    if (!is.null(reduction)) {
        reduction <- match.arg(reduction, c("auto", "linear", "tree", "simd"))
    } else {
        reduction <- "auto"
    }

    bounds_check_input <- bounds_check
    if (is.null(bounds_check)) {
        bounds_check <- .mojor_state$`%||%`(.mojor_state$options$bounds_check, TRUE)
    }
    bounds_check <- .mojor_assert_scalar_logical(bounds_check, "bounds_check")

    if (is.null(index_base)) {
        index_base <- .mojor_state$`%||%`(.mojor_state$options$index_base, "one_based")
    }
    index_base <- match.arg(index_base, c("one_based", "zero_based"))
    if (is.null(array_layout)) {
        array_layout <- .mojor_state$`%||%`(.mojor_state$options$array_layout, "col_major")
    }
    array_layout <- match.arg(array_layout, c("col_major", "row_major"))
    .mojor_state$current_index_base <- index_base
    .mojor_state$current_array_layout <- array_layout
    layout_kind <- if (identical(array_layout, "row_major"))
        "row_major" else "col_major"
    layout_ctor <- function(rank, zeros) {
        paste0("Layout.", layout_kind, "(IndexList[", rank, "](", zeros, "))")
    }
    layout_runtime <- function(layout_name, index_list) {
        paste0(
            "RuntimeLayout[", layout_name, "].", layout_kind, "(", index_list,
            ")"
        )
    }

    if (!is.null(tile)) {
        if (!is.numeric(tile) ||
            length(tile) <
                1 || length(tile) >
            3) {
            stop(
                "mojor_transpile: tile must be a numeric vector of length 1, 2, or 3"
            )
        }
        tile <- as.integer(tile)
        if (any(is.na(tile)) ||
            any(tile < 1)) {
            stop("mojor_transpile: tile values must be positive integers")
        }
        if (length(tile) ==
            1) {
            tile <- c(tile, tile)
        }
    }

    schedule <- list(
        unroll = unroll, tile = tile, reduction = if (!is.null(reduction) &&
            reduction != "auto" && reduction != "linear") reduction else NULL,
        n_var = "n_i", type_env = NULL
    )
    schedule <- schedule[!vapply(schedule, is.null, logical(1))]

    if (isTRUE(allow_in_place)) {
        warning(
            "mojor_transpile: allow_in_place is not yet implemented and has no effect"
        )
    }

    na_guard <- "assign"
    na_scan <- FALSE
    if (is.null(simd_mode)) {
        simd_mode <- .mojor_state$`%||%`(.mojor_state$options$simd_mode, "explicit")
    }
    simd_mode <- match.arg(simd_mode, c("explicit", "auto", "off"))
    if (is.null(assume_aligned)) {
        assume_aligned <- .mojor_state$`%||%`(.mojor_state$options$assume_aligned, NULL)
    }

    # Strict index-bounds mode is opt-in:
    # - explicit bounds_check=TRUE/FALSE mirrors into index_bounds
    # - otherwise honor mojor_options(index_bounds=...)
    index_bounds <- if (!is.null(bounds_check_input)) {
        bounds_check
    } else {
        .mojor_state$`%||%`(.mojor_state$options$index_bounds, FALSE)
    }
    index_bounds <- .mojor_assert_scalar_logical(index_bounds, "index_bounds")
    old_index_bounds <- .mojor_state$options$index_bounds
    .mojor_state$options$index_bounds <- index_bounds
    restore_index_bounds <- function() {
        .mojor_state$options$index_bounds <- old_index_bounds
    }

    elementwise <- .mojor_assert_scalar_logical(elementwise, "elementwise")
    elementwise_target <- match.arg(elementwise_target, c("cpu", "gpu"))
    if (is.null(elementwise_cpu)) {
        elementwise_cpu <- isTRUE(.mojor_state$options$elementwise_cpu)
    } else {
        elementwise_cpu <- .mojor_assert_scalar_logical(elementwise_cpu, "elementwise_cpu")
    }
    if (is.null(elementwise_gpu_layouttensor)) {
        elementwise_gpu_layouttensor <- .mojor_state$options$elementwise_gpu_layouttensor
    }
    elementwise_gpu_layouttensor <- .mojor_assert_scalar_logical_or_null(elementwise_gpu_layouttensor, "elementwise_gpu_layouttensor")
    if (is.null(gpu_jit_mode)) {
        gpu_jit_mode <- .mojor_state$options$gpu_jit_mode
    }
    gpu_jit_mode <- match.arg(gpu_jit_mode, c("auto", "unified_preview"))
    broadcast <- match.arg(
        broadcast, c(
            "none", "scalar", "recycle", "recycle_warn", "broadcast_nd",
            "broadcast_nd_warn"
        )
    )
    broadcast_nd <- broadcast %in% c("broadcast_nd", "broadcast_nd_warn")
    .mojor_state$current_broadcast_nd <- isTRUE(broadcast_nd)
    .mojor_state$current_broadcast <- broadcast
    parallel <- .mojor_assert_scalar_logical(parallel, "parallel")
    .mojor_state$current_parallel <- isTRUE(parallel)
    fusion <- .mojor_assert_scalar_logical(fusion, "fusion")
    .mojor_state$current_fusion <- fusion
    elementwise_size <- .mojor_parse_positive_integer_or_null(elementwise_size, "elementwise_size")
    assume_aligned <- .mojor_parse_positive_integer_or_null(assume_aligned, "assume_aligned")
    if (exists("schedule", inherits = FALSE)) {
        schedule$vectorize <- !is.null(assume_aligned) &&
            !identical(simd_mode, "off")
        schedule <- .mojor_prune_schedule_nulls(schedule)
    }
    if (exists(".mojor_warn_ignored_ir_service_envvars", mode = "function", inherits = TRUE)) {
        .mojor_warn_ignored_ir_service_envvars()
    }

    list(
        na_mode = na_mode, semantics = semantics, ir_dump = ir_dump, opt_level = opt_level,
        unroll = unroll, reduction = reduction, bounds_check = bounds_check,
        bounds_check_input = bounds_check_input,
        index_base = index_base, array_layout = array_layout, layout_kind = layout_kind,
        layout_ctor = layout_ctor, layout_runtime = layout_runtime, tile = tile,
        schedule = schedule, na_guard = na_guard, na_scan = na_scan, simd_mode = simd_mode,
        assume_aligned = assume_aligned, index_bounds = index_bounds, elementwise = elementwise,
        elementwise_target = elementwise_target, elementwise_size = elementwise_size,
        elementwise_cpu = elementwise_cpu, elementwise_gpu_layouttensor = elementwise_gpu_layouttensor,
        gpu_jit_mode = gpu_jit_mode, broadcast = broadcast, broadcast_nd = broadcast_nd,
        parallel = parallel, fusion = fusion,
        unroll_explicit = unroll_explicit,
        reduction_explicit = reduction_explicit, tile_explicit = tile_explicit,
        fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
        fusion_allow_broadcast_nd_identity = isTRUE(.mojor_state$options$fusion_allow_broadcast_nd_identity),
        ir_service = .mojor_ir_service_mode(),
        ir_emit_shadow = FALSE,
        emit_typed_lir_payload = .mojor_emit_typed_lir_payload_enabled(),
        restore_index_bounds = restore_index_bounds
    )
}

.mojor_setup_transpile_state_context <- function(na_mode) {
    ctx_fields <- c(
        "diagnostics", "needs_index_helpers", "needs_mojo_random", "needs_rng_tables",
        "needs_ffi", "matrix_dimnames", "vector_names", "current_na_mode",
        "current_broadcast_nd",
        "current_broadcast", "current_ndim_var_map", "current_out_ndim_var",
        "current_iter_map", "recycle_warnings", "current_len_checks", "current_len_checks_c",
        "current_constructor_mode", "current_args", "current_arg_specs",
        "current_parallel", "current_local_matrix_dims", "current_local_array_dims",
        "current_local_vector_lengths", "current_const_array_vars", "current_df_schema",
        "current_chr_index_hashed_args", "current_chr_index_local_vars"
    )
    snapshot <- .mojor_state_snapshot(ctx_fields)
    .mojor_state$diagnostics <- list()
    .mojor_state$needs_index_helpers <- FALSE
    .mojor_state$needs_mojo_random <- FALSE
    .mojor_state$needs_rng_tables <- FALSE
    .mojor_state$needs_ffi <- FALSE
    .mojor_state$matrix_dimnames <- NULL
    .mojor_state$vector_names <- NULL
    .mojor_state$current_na_mode <- na_mode
    .mojor_state$recycle_warnings <- list()
    .mojor_state$current_len_checks <- list()
    .mojor_state$current_len_checks_c <- list()
    .mojor_state$current_constructor_mode <- FALSE
    .mojor_state$current_local_matrix_dims <- list()
    .mojor_state$current_local_array_dims <- list()
    .mojor_state$current_local_vector_lengths <- list()
    .mojor_state$current_const_array_vars <- list()
    .mojor_state$current_df_schema <- NULL
    .mojor_state$current_chr_index_hashed_args <- character(0)
    .mojor_state$current_chr_index_local_vars <- list()

    restore <- function() {
        .mojor_state_restore(snapshot)
    }

    list(restore = restore)
}

.mojor_finalize_mojo_postamble <- function(mojo_lines, index_bounds) {
    .mojor_insert_relative_export <- function(lines, inserted, when_no_export = c("append", "prepend")) {
        when_no_export <- match.arg(when_no_export)
        if (length(inserted) ==
            0) {
            return(lines)
        }
        export_idx <- match(TRUE, grepl("^@export", lines))
        if (is.na(export_idx)) {
            if (identical(when_no_export, "prepend")) {
                return(c(inserted, lines))
            }
            return(c(lines, inserted))
        }
        append(lines, inserted, after = export_idx - 1L)
    }
    .mojor_collect_math_imports <- function(lines) {
        math_import_lines <- lines[grepl("^from math import", lines)]
        if (length(math_import_lines) ==
            0) {
            return(character(0))
        }
        unique(
            trimws(
                unlist(
                  strsplit(
                    sub("^from math import\\s*", "", math_import_lines),
                    ","
                )
              )
            )
        )
    }
    .mojor_add_math_imports <- function(lines, symbols) {
        if (length(symbols) ==
            0) {
            return(lines)
        }
        import_line <- paste0(
            "from math import ", paste(
                unique(symbols),
                collapse = ", "
            )
        )
        .mojor_insert_relative_export(lines, import_line, when_no_export = "prepend")
    }

    needs_oob <- any(grepl("_mojor_oob", mojo_lines, fixed = TRUE))
    needs_index_helpers <- isTRUE(index_bounds) ||
        isTRUE(.mojor_state$needs_index_helpers) ||
        needs_oob ||
        any(grepl("_mojor_read_", mojo_lines, fixed = TRUE))
    if (needs_index_helpers) {
        needs_read_f64 <- any(grepl("_mojor_read_f64", mojo_lines, fixed = TRUE))
        needs_read_f32 <- any(grepl("_mojor_read_f32", mojo_lines, fixed = TRUE))
        needs_read_i32 <- any(grepl("_mojor_read_i32", mojo_lines, fixed = TRUE))
        needs_read_lgl <- any(grepl("_mojor_read_lgl", mojo_lines, fixed = TRUE))
        helper_lines <- character(0)
        if (needs_oob) {
            helper_lines <- c(helper_lines, "fn _mojor_oob():", "    pass")
        }
        if (needs_read_f64) {
            if (isTRUE(index_bounds)) {
                helper_lines <- c(
                    helper_lines,
                    "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int, na_flag: MutInt32Ptr) -> Float64:",
                    "    if idx < 0:",
                    "        na_flag[0] = Int32(2)",
                    "        return _MOJOR_NAN",
                    "    if idx >= upper:",
                    "        na_flag[0] = Int32(2)",
                    "        return _MOJOR_NAN",
                    "    return ptr[idx]",
                    "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int) -> Float64:",
                    "    if idx < 0: return _MOJOR_NAN",
                    "    if idx >= upper: return _MOJOR_NAN",
                    "    return ptr[idx]"
                )
 # In debug mode, emit version with mojor_check_bounds
            } else if (isTRUE(.mojor_state$debug_mode)) {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float64:",
                    "    mojor_check_bounds(idx, upper, var_name, file, line)",
                    "    return ptr[idx]"
                )
            } else {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int) -> Float64:",
                    "    if idx < 0: return _MOJOR_NAN", "    if idx >= upper: return _MOJOR_NAN",
                    "    return ptr[idx]"
                )
            }
        }
        if (needs_read_f32) {
            if (isTRUE(index_bounds)) {
                helper_lines <- c(
                    helper_lines,
                    "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int, na_flag: MutInt32Ptr) -> Float32:",
                    "    if idx < 0:",
                    "        na_flag[0] = Int32(2)",
                    "        return _MOJOR_NAN_F32",
                    "    if idx >= upper:",
                    "        na_flag[0] = Int32(2)",
                    "        return _MOJOR_NAN_F32",
                    "    return ptr[idx]",
                    "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int) -> Float32:",
                    "    if idx < 0: return _MOJOR_NAN_F32",
                    "    if idx >= upper: return _MOJOR_NAN_F32",
                    "    return ptr[idx]"
                )
            } else if (isTRUE(.mojor_state$debug_mode)) {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float32:",
                    "    mojor_check_bounds(idx, upper, var_name, file, line)",
                    "    return ptr[idx]"
                )
            } else {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int) -> Float32:",
                    "    if idx < 0: return _MOJOR_NAN_F32", "    if idx >= upper: return _MOJOR_NAN_F32",
                    "    return ptr[idx]"
                )
            }
        }
        if (needs_read_i32) {
            if (isTRUE(index_bounds)) {
                helper_lines <- c(
                    helper_lines,
                    "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int, na_flag: MutInt32Ptr) -> Int32:",
                    "    if idx < 0:",
                    "        na_flag[0] = Int32(2)",
                    "        return Int32(-2147483648)",
                    "    if idx >= upper:",
                    "        na_flag[0] = Int32(2)",
                    "        return Int32(-2147483648)",
                    "    return ptr[idx]",
                    "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int) -> Int32:",
                    "    if idx < 0: return Int32(-2147483648)",
                    "    if idx >= upper: return Int32(-2147483648)",
                    "    return ptr[idx]"
                )
            } else if (isTRUE(.mojor_state$debug_mode)) {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
                    "    mojor_check_bounds(idx, upper, var_name, file, line)",
                    "    return ptr[idx]"
                )
            } else {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int) -> Int32:",
                    "    if idx < 0: return Int32(-2147483648)", "    if idx >= upper: return Int32(-2147483648)",
                    "    return ptr[idx]"
                )
            }
        }
        if (needs_read_lgl) {
            if (isTRUE(index_bounds)) {
                helper_lines <- c(
                    helper_lines,
                    "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int, na_flag: MutInt32Ptr) -> Int32:",
                    "    if idx < 0:",
                    "        na_flag[0] = Int32(2)",
                    "        return Int32(0)",
                    "    if idx >= upper:",
                    "        na_flag[0] = Int32(2)",
                    "        return Int32(0)",
                    "    return ptr[idx]",
                    "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int) -> Int32:",
                    "    if idx < 0: return Int32(0)",
                    "    if idx >= upper: return Int32(0)",
                    "    return ptr[idx]"
                )
            } else if (isTRUE(.mojor_state$debug_mode)) {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
                    "    mojor_check_bounds(idx, upper, var_name, file, line)",
                    "    return ptr[idx]"
                )
            } else {
                helper_lines <- c(
                    helper_lines, "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int) -> Int32:",
                    "    if idx < 0: return Int32(0)", "    if idx >= upper: return Int32(0)",
                    "    return ptr[idx]"
                )
            }
        }
        mojo_lines <- .mojor_insert_relative_export(mojo_lines, helper_lines, when_no_export = "append")
    }

    needs_gpu_reduce_helper <- any(grepl("_mojor_gpu_reduce\\(", mojo_lines))
    needs_gpu_matmul_helper <- any(grepl("_mojor_gpu_matmul\\(", mojo_lines)) ||
        any(grepl("_mojor_gpu_matmul_into\\(", mojo_lines))
    if (needs_gpu_reduce_helper || needs_gpu_matmul_helper) {
        gpu_helper_lines <- character(0)
        if (needs_gpu_reduce_helper) {
            gpu_helper_lines <- c(
                gpu_helper_lines, "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF64Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float64:",
                "    _ = dims", "    if not dims_default or keepdims:",
                "        return _MOJOR_NAN", "    if n_i <= 0:", "        return _MOJOR_NAN",
                "    if op == \"sum\":", "        var acc: Float64 = 0.0",
                "        for i in range(n_i):", "            acc += value[i]",
                "        return acc", "    if op == \"mean\":", "        var acc: Float64 = 0.0",
                "        for i in range(n_i):", "            acc += value[i]",
                "        return acc / Float64(n_i)", "    if op == \"min\":",
                "        var acc: Float64 = value[0]", "        for i in range(1, n_i):",
                "            if value[i] < acc:", "                acc = value[i]",
                "        return acc", "    if op == \"max\":", "        var acc: Float64 = value[0]",
                "        for i in range(1, n_i):", "            if value[i] > acc:",
                "                acc = value[i]", "        return acc",
                "    if op == \"argmin\":", "        var best: Float64 = value[0]",
                "        var best_i: Int = 0", "        for i in range(1, n_i):",
                "            if value[i] < best:", "                best = value[i]",
                "                best_i = i", "        return Float64(best_i + 1)",
                "    if op == \"argmax\":", "        var best: Float64 = value[0]",
                "        var best_i: Int = 0", "        for i in range(1, n_i):",
                "            if value[i] > best:", "                best = value[i]",
                "                best_i = i", "        return Float64(best_i + 1)",
                "    return _MOJOR_NAN", "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF32Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float32:",
                "    _ = dims", "    if not dims_default or keepdims:",
                "        return _MOJOR_NAN_F32", "    if n_i <= 0:", "        return _MOJOR_NAN_F32",
                "    if op == \"sum\":", "        var acc: Float32 = Float32(0.0)",
                "        for i in range(n_i):", "            acc += value[i]",
                "        return acc", "    if op == \"mean\":", "        var acc: Float32 = Float32(0.0)",
                "        for i in range(n_i):", "            acc += value[i]",
                "        return acc / Float32(n_i)", "    if op == \"min\":",
                "        var acc: Float32 = value[0]", "        for i in range(1, n_i):",
                "            if value[i] < acc:", "                acc = value[i]",
                "        return acc", "    if op == \"max\":", "        var acc: Float32 = value[0]",
                "        for i in range(1, n_i):", "            if value[i] > acc:",
                "                acc = value[i]", "        return acc",
                "    if op == \"argmin\":", "        var best: Float32 = value[0]",
                "        var best_i: Int = 0", "        for i in range(1, n_i):",
                "            if value[i] < best:", "                best = value[i]",
                "                best_i = i", "        return Float32(best_i + 1)",
                "    if op == \"argmax\":", "        var best: Float32 = value[0]",
                "        var best_i: Int = 0", "        for i in range(1, n_i):",
                "            if value[i] > best:", "                best = value[i]",
                "                best_i = i", "        return Float32(best_i + 1)",
                "    return _MOJOR_NAN_F32"
            )
        }
        if (needs_gpu_matmul_helper) {
            gpu_helper_lines <- c(
                gpu_helper_lines, "fn _mojor_gpu_matmul_into(out: MutF64Ptr, a: ImmutF64Ptr, b: ImmutF64Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF64Ptr:",
                "    if a_nrow <= 0 or a_ncol <= 0 or b_nrow <= 0 or b_ncol <= 0 or out_nrow <= 0 or out_ncol <= 0:",
                "        return out", "    var a_rows = a_nrow", "    var a_cols = a_ncol",
                "    if transpose_a:", "        a_rows = a_ncol", "        a_cols = a_nrow",
                "    var b_rows = b_nrow", "    var b_cols = b_ncol", "    if transpose_b:",
                "        b_rows = b_ncol", "        b_cols = b_nrow", "    if a_cols != b_rows:",
                "        return out", "    if out_nrow != a_rows or out_ncol != b_cols:",
                "        return out", "    for j in range(out_ncol):",
                "        for i in range(out_nrow):", "            var acc: Float64 = 0.0",
                "            for k in range(a_cols):", "                var a_idx = i + k * a_nrow",
                "                if transpose_a:", "                    a_idx = k + i * a_nrow",
                "                var b_idx = k + j * b_nrow", "                if transpose_b:",
                "                    b_idx = j + k * b_nrow", "                acc += a[a_idx] * b[b_idx]",
                "            out[i + j * out_nrow] = acc", "    return out",
                "fn _mojor_gpu_matmul_into(out: MutF32Ptr, a: ImmutF32Ptr, b: ImmutF32Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF32Ptr:",
                "    if a_nrow <= 0 or a_ncol <= 0 or b_nrow <= 0 or b_ncol <= 0 or out_nrow <= 0 or out_ncol <= 0:",
                "        return out", "    var a_rows = a_nrow", "    var a_cols = a_ncol",
                "    if transpose_a:", "        a_rows = a_ncol", "        a_cols = a_nrow",
                "    var b_rows = b_nrow", "    var b_cols = b_ncol", "    if transpose_b:",
                "        b_rows = b_ncol", "        b_cols = b_nrow", "    if a_cols != b_rows:",
                "        return out", "    if out_nrow != a_rows or out_ncol != b_cols:",
                "        return out", "    for j in range(out_ncol):",
                "        for i in range(out_nrow):", "            var acc: Float32 = Float32(0.0)",
                "            for k in range(a_cols):", "                var a_idx = i + k * a_nrow",
                "                if transpose_a:", "                    a_idx = k + i * a_nrow",
                "                var b_idx = k + j * b_nrow", "                if transpose_b:",
                "                    b_idx = j + k * b_nrow", "                acc += a[a_idx] * b[b_idx]",
                "            out[i + j * out_nrow] = acc", "    return out",
                "fn _mojor_gpu_matmul[a_t: AnyType, b_t: AnyType](a: a_t, b: b_t, transpose_a: Bool, transpose_b: Bool) raises -> a_t:",
                "    # Expression-form matmul has no output buffer contract. Use assign-lowered _into helper.",
                "    _ = a", "    _ = b", "    _ = transpose_a", "    _ = transpose_b",
                "    raise Error(\"_mojor_gpu_matmul expression form is not executable; use assignment form\")"
            )
        }
        mojo_lines <- .mojor_insert_relative_export(mojo_lines, gpu_helper_lines, when_no_export = "append")
    }

    imported_math <- .mojor_collect_math_imports(mojo_lines)
    mojo_math_fns <- c(
        "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
        "tanh", "log", "log1p", "exp", "expm1", "sqrt", "floor", "ceil",
        "trunc", "hypot", "cbrt", "lgamma", "erf", "tgamma"
    )
    used_math <- mojo_math_fns[vapply(
        mojo_math_fns, function(fn) {
            any(
                grepl(
                  paste0("\\b", fn, "\\s*\\("),
                  mojo_lines, perl = TRUE
              )
            )
        }, logical(1)
    )]
    missing_math <- setdiff(used_math, imported_math)
    if (length(missing_math) >
        0) {
        mojo_lines <- .mojor_add_math_imports(mojo_lines, missing_math)
    }
    math_import_lines <- mojo_lines[grepl("^from math import", mojo_lines)]
    needs_isnan <- any(grepl("isnan(", mojo_lines, fixed = TRUE)) &&
        !any(grepl("\\bisnan\\b", math_import_lines))
    needs_isinf <- any(grepl("isinf(", mojo_lines, fixed = TRUE)) &&
        !any(grepl("\\bisinf\\b", math_import_lines))
    needs_isfinite <- any(grepl("isfinite(", mojo_lines, fixed = TRUE)) &&
        !any(grepl("\\bisfinite\\b", math_import_lines))
    if (needs_isnan || needs_isinf || needs_isfinite) {
        math_extra <- character(0)
        if (needs_isnan)
            math_extra <- c(math_extra, "isnan")
        if (needs_isinf)
            math_extra <- c(math_extra, "isinf")
        if (needs_isfinite)
            math_extra <- c(math_extra, "isfinite")
        mojo_lines <- .mojor_add_math_imports(mojo_lines, math_extra)
    }

    mojo_lines
}

.mojor_ssa_stage_defaults <- function() {
    list(
        ssa_backend = NULL, ssa_backend_error = NULL, ssa_backend_opt_fallback = FALSE,
        ssa_backend_opt_fallback_reason = NULL
    )
}

.mojor_ssa_stage_normalize <- function(stage) {
    out <- .mojor_ssa_stage_defaults()
    if (is.null(stage) ||
        !is.list(stage)) {
        return(out)
    }
    common <- intersect(
        names(out),
        names(stage)
    )
    if (length(common) >
        0) {
        out[common] <- stage[common]
    }
    out$ssa_backend_opt_fallback <- isTRUE(out$ssa_backend_opt_fallback)
    out
}

.mojor_ssa_stage_status <- function(requested, stage = NULL, phase = NULL) {
 # Backward-compat alias: older callsites may still pass `phase=`.
    if (is.null(stage) && !is.null(phase)) {
        stage <- phase
    }
    stage <- .mojor_ssa_stage_normalize(stage)
    list(
        requested = isTRUE(requested),
        strict_failure = isTRUE(.mojor_state$options$ir_only),
        built = !is.null(stage$ssa_backend),
        opt_level_fallback = stage$ssa_backend_opt_fallback, opt_level_fallback_reason = stage$ssa_backend_opt_fallback_reason
    )
}

.mojor_prepare_ssa_backend_stage <- function(emit_ssa_backend, fn, local_types, reduction, schedule, opt_level) {
    ssa_backend <- NULL
    ssa_backend_error <- NULL
    ssa_backend_opt_fallback <- FALSE
    ssa_backend_opt_fallback_reason <- NULL
    if (isTRUE(emit_ssa_backend)) {
        layout_ctx_ssa <- .mojor_ir_layout_ctx(
            n_var = "n_i", nrow_var = .mojor_state$current_out_nrow_var,
            ncol_var = .mojor_state$current_out_ncol_var, dim_var_map = .mojor_state$current_dim_var_map,
            ndim_var_map = .mojor_state$current_ndim_var_map, nrow_var_map = .mojor_state$current_nrow_var_map,
            ncol_var_map = .mojor_state$current_ncol_var_map, len_var_map = .mojor_state$current_len_var_map,
            tensor_map = .mojor_state$current_tensor_map, index_base = .mojor_state$current_index_base,
            array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
            fusion_allow_broadcast_nd_identity = isTRUE(.mojor_state$options$fusion_allow_broadcast_nd_identity)
        )
        layout_ctx_ssa$n_source_name <- .mojor_state$current_n_source_name
        layout_ctx_ssa$type_env <- local_types
        layout_ctx_ssa$reduction_mode <- reduction
        schedule_ssa <- schedule
        schedule_ssa$type_env <- local_types
        backend_res <- tryCatch(
            .mojor_ir_prepare_ssa_backend(
                fn, layout_ctx = layout_ctx_ssa, schedule = schedule_ssa,
                opt_level = opt_level
            ),
            error = function(e) e
        )
        if (inherits(backend_res, "error") &&
            !is.na(opt_level) &&
            opt_level > 0) {
            ssa_backend_opt_fallback <- TRUE
            ssa_backend_opt_fallback_reason <- conditionMessage(backend_res)
            backend_res <- tryCatch(
                .mojor_ir_prepare_ssa_backend(
                  fn, layout_ctx = layout_ctx_ssa, schedule = schedule_ssa,
                  opt_level = 0
              ),
                error = function(e) e
            )
        }
        if (inherits(backend_res, "error")) {
            ssa_backend_error <- conditionMessage(backend_res)
        } else {
            ssa_backend <- backend_res
        }
    }

    .mojor_ssa_stage_normalize(
        list(
            ssa_backend = ssa_backend, ssa_backend_error = ssa_backend_error,
            ssa_backend_opt_fallback = ssa_backend_opt_fallback, ssa_backend_opt_fallback_reason = ssa_backend_opt_fallback_reason
        )
    )
}
