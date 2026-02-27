# Transpile output assembly helper.

.mojor_detect_gpu_jit_coverage <- function(
    mojo_lines,
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off") {
  lines <- as.character(mojo_lines)
  if (length(lines) == 1 && grepl("\n", lines, fixed = TRUE)) {
    lines <- strsplit(lines, "\n", fixed = TRUE)[[1]]
  }
  lines <- lines[nzchar(lines)]

  helper_defs <- list(
    reduce = sum(grepl("^\\s*fn\\s+_mojor_gpu_reduce\\[", lines, perl = TRUE)),
    matmul = sum(grepl("^\\s*fn\\s+_mojor_gpu_matmul\\[", lines, perl = TRUE)),
    matmul_into = sum(grepl("^\\s*fn\\s+_mojor_gpu_matmul_into\\(", lines, perl = TRUE))
  )
  non_def_lines <- lines[!grepl("^\\s*fn\\s+_mojor_gpu_", lines, perl = TRUE)]
  helper_calls <- list(
    reduce = sum(grepl("_mojor_gpu_reduce\\(", non_def_lines, perl = TRUE)),
    matmul = sum(grepl("_mojor_gpu_matmul\\(", non_def_lines, perl = TRUE)),
    matmul_into = sum(grepl("_mojor_gpu_matmul_into\\(", non_def_lines, perl = TRUE))
  )
  preview_lowering_counts <- list(
    reduce_assign = sum(grepl("__mojor_gpu_reduce_assign_", non_def_lines, fixed = TRUE)),
    reduce_ret = sum(grepl("__mojor_gpu_reduce_ret_", non_def_lines, fixed = TRUE)),
    matmul = sum(grepl("__mojor_gpu_mm_", non_def_lines, fixed = TRUE))
  )
  preview_lowering_total <- sum(unlist(preview_lowering_counts, use.names = FALSE))
  preview_lowering_used <- preview_lowering_total > 0
  preview_lowered_ops <- character(0)
  if ((preview_lowering_counts$reduce_assign + preview_lowering_counts$reduce_ret) > 0) {
    preview_lowered_ops <- c(preview_lowered_ops, "reduce")
  }
  if (preview_lowering_counts$matmul > 0) {
    preview_lowered_ops <- c(preview_lowered_ops, "matmul")
  }
  preview_lowered_ops <- unique(preview_lowered_ops)
  reduce_call_lines <- non_def_lines[grepl("_mojor_gpu_reduce\\(", non_def_lines, perl = TRUE)]
  extract_reduce_op <- function(line) {
    m <- regexec('_mojor_gpu_reduce\\([^,]+,\\s*"([^"]+)"', line, perl = TRUE)
    parts <- regmatches(line, m)[[1]]
    if (length(parts) >= 2) parts[[2]] else NA_character_
  }
  reduce_ops <- unique(stats::na.omit(vapply(reduce_call_lines, extract_reduce_op, character(1))))
  preview_blockers <- list(
    reduce_ops = reduce_ops,
    reduce_calls = helper_calls$reduce,
    matmul_calls = helper_calls$matmul,
    matmul_into_calls = helper_calls$matmul_into
  )

  helper_total <- sum(unlist(helper_calls, use.names = FALSE))
  helper_backed_used <- helper_total > 0 || sum(unlist(helper_defs, use.names = FALSE)) > 0
  unified_loop_used <- (isTRUE(elementwise_gpu_emitted) && identical(elementwise_target, "gpu")) ||
    preview_lowering_used
  route <- "loop"
  route_reason <- if (unified_loop_used && helper_backed_used) {
    "helper_backed_and_unified"
  } else if (unified_loop_used) {
    "unified_only"
  } else if (helper_backed_used) {
    "helper_backed_only"
  } else {
    "no_gpu_lowering"
  }

  list(
    route = route,
    route_reason = route_reason,
    helper_backed_used = helper_backed_used,
    unified_loop_used = unified_loop_used,
    preview_lowering_used = preview_lowering_used,
    preview_lowering_total = preview_lowering_total,
    preview_lowering_counts = preview_lowering_counts,
    preview_lowered_ops = preview_lowered_ops,
    helper_total = helper_total,
    helper_calls = helper_calls,
    helper_defs = helper_defs,
    preview_blockers = preview_blockers
  )
}

.mojor_build_transpile_output <- function(ctx) {
  if (!is.environment(ctx)) {
    stop(".mojor_build_transpile_output: ctx must be an environment")
  }
  evalq(
    {
      .ctx_val <- function(name, default = NULL) {
        if (!exists(name, inherits = TRUE)) {
          return(default)
        }
        get(name, inherits = TRUE)
      }
      .ctx_gpu_buf_field <- function(field, default = NULL) {
        info <- .ctx_val("elementwise_gpu_buf_info", default = NULL)
        if (is.null(info) || is.null(info[[field]])) {
          return(default)
        }
        info[[field]]
      }
      gpu_jit_info <- .ctx_val("gpu_jit_gate_info", default = NULL)
      if (is.null(gpu_jit_info)) {
        gpu_jit_info <- .mojor_detect_gpu_jit_coverage(
          mojo_lines = mojo_lines,
          elementwise_gpu_emitted = .ctx_val("elementwise_gpu_buf_emitted", default = FALSE),
          elementwise_target = if (isTRUE(elementwise)) elementwise_target else "off"
        )
      }
      if (is.null(gpu_jit_info$mode)) {
        gpu_jit_info$mode <- .ctx_val("gpu_jit_mode", default = "auto")
      }
      parallel_requested <- isTRUE(.ctx_val("parallel", default = FALSE))
      loop_infos_ctx <- .ctx_val("loop_infos", default = list())
      parallel_explicit <- FALSE
      if (is.list(loop_infos_ctx) && length(loop_infos_ctx) > 0) {
        parallel_explicit <- any(vapply(
          loop_infos_ctx,
          function(li) {
            is.list(li) &&
              is.list(li$seq_info) &&
              isTRUE(li$seq_info$parallel)
          },
          logical(1)
        ))
      }
      parallel_effective_default <- parallel_requested && isTRUE(parallel_safe)
      if (isTRUE(parallel_explicit)) {
        parallel_effective_default <- TRUE
      }
      parallel_effective <- isTRUE(.ctx_val("any_parallel", default = parallel_effective_default))
      parallel_effective_reason <- parallel_reason
      if (!isTRUE(parallel_effective) &&
          (is.null(parallel_effective_reason) || !nzchar(parallel_effective_reason))) {
        parallel_effective_reason <- "parallel disabled (parallel=FALSE)"
      }

      trans_out <- list(
        name = name,
        mojo = paste(mojo_lines, collapse = "\n"),
        signature = sig_args,
        ir = ir_dump,
        ssa_backend = ssa_backend,
        ssa_backend_error = ssa_backend_error,
        ssa_backend_status = .mojor_ssa_stage_status(
          requested = emit_ssa_backend,
          list(
            ssa_backend = ssa_backend,
            ssa_backend_opt_fallback = ssa_backend_opt_fallback,
            ssa_backend_opt_fallback_reason = ssa_backend_opt_fallback_reason
          )
        ),
        compatibility = list(
          api_surface_version = .mojor_state$api_surface_version,
          transpile_version = .mojor_state$transpile_version,
          strict_ir_default = isTRUE(.mojor_state$options$ir_only)
        ),
        transpile_route = .ctx_val(
          "transpile_route",
          default = list(route = "loop", reason = "loop_path_default")
        ),
        opt_level = opt_level,
        notes = c("Subset: single for-loop, array assignment or scalar update, simple arithmetic, optional if/return"),
        types = if (exists("types_input", inherits = TRUE)) types_input else arg_specs,
        kernel_types = arg_specs,
        chr_index_hashed_args = .ctx_val("chr_index_hashed_args", default = character(0)),
        out_kind = out_kind,
        out_name = if (out_kind == "vector") out_name else scalar_init,
        out_type = if (out_kind == "vector") out_type else scalar_type,
        return_name = return_name,
        n_source = seq_info,
        float_seq_check = if (!is.null(seq_info$float_seq_check)) seq_info$float_seq_check else NULL,
        len_arrays = len_arrays,
        recycle_arrays = recycle_arrays_expr,
        len_checks_c = .mojor_state$current_len_checks_c,
        len_check_arrays = len_check_arrays,
        nrow_arrays = nrow_arrays,
        ncol_arrays = ncol_arrays,
        nd_arrays = nd_arrays,
        dim_arrays = dim_arrays,
        matrix_dim_arrays = matrix_dim_arrays,
        tensor_ranks = tensor_ranks,
        out_matrix = out_matrix,
        out_nrow_expr = out_nrow_expr,
        out_ncol_expr = out_ncol_expr,
        out_array = out_array,
        out_len_source = out_len_source,
        matrix_dimnames = .mojor_state$matrix_dimnames,
        out_dim_exprs = out_dim_exprs,
        out_dim_name = out_dim_name,
        rng_needed = isTRUE(needs_rng),
        na_needed = isTRUE(needs_na),
        set_match_needed = isTRUE(needs_set_match),
        quantile_needed = isTRUE(needs_quantile),
        scalar_reduction_op = scalar_reduction_op,
        scalar_reduction_arg = scalar_reduction_arg,
        scalar_reduction_arg_spec = if (!is.null(scalar_reduction_arg)) arg_specs[[scalar_reduction_arg]] else NULL,
        simd = list(
          safe = simd_safe,
          reason = simd_reason,
          mode = simd_mode,
          explicit_disabled = identical(simd_mode, "off"),
          assume_aligned = assume_aligned,
          na_checks_skipped = simd_na_skipped,
          na_pre_scan = simd_na_scan,
          emitted = simd_emitted
        ),
        elementwise = list(
          enabled = isTRUE(elementwise),
          target = if (isTRUE(elementwise)) elementwise_target else "off",
          emitted = .ctx_val("elementwise_emitted", default = FALSE),
          reason = .ctx_val("elementwise_reason", default = NULL),
          inplace = .ctx_val("elementwise_inplace", default = FALSE),
          gpu_buf_emitted = .ctx_val("elementwise_gpu_buf_emitted", default = FALSE),
          gpu_buf_reason = .ctx_val("elementwise_gpu_buf_reason", default = NULL),
          gpu_buf_name = .ctx_gpu_buf_field("name", default = NULL),
          gpu_buf_dtype = .ctx_gpu_buf_field("buf_dtype", default = NULL),
          gpu_buf_index_mode = .ctx_gpu_buf_field("index_mode", default = "linear1d"),
          gpu_buf_matrix_dim_source = .ctx_gpu_buf_field("matrix_dim_source", default = NULL),
          gpu_buf_matrix_dim_arrays = .ctx_gpu_buf_field("matrix_dim_arrays", default = character(0))
        ),
        gpu_jit = gpu_jit_info,
        parallel = list(
          requested = parallel_requested,
          explicit = parallel_explicit,
          effective = parallel_effective,
          safe = parallel_safe,
          reason = parallel_effective_reason
        ),
        modified_args = if (is.null(modified_args))
            character(0) else unique(as.character(modified_args)),
        allow_in_place = isTRUE(allow_in_place), # accepted but currently has no codegen effect
        semantics = semantics,
        unroll = .ctx_val("unroll_requested", default = unroll),
        reduction = .ctx_val("reduction_requested", default = reduction),
        bounds_check = bounds_check,
        index_bounds = isTRUE(index_bounds),
        tile = .ctx_val("tile_requested", default = tile),
        unroll_effective = .ctx_val("unroll_effective", default = unroll),
        reduction_effective = .ctx_val("reduction_effective", default = reduction),
        tile_effective = .ctx_val("tile_effective", default = tile),
        schedule = .ctx_val("schedule_effective", default = schedule),
        auto_schedule = .ctx_val("auto_schedule_policy", default = NULL),
        diagnostics = .mojor_state$diagnostics,
        na_mode = na_mode,
        na_guard = na_guard,
        recycle_warnings = .mojor_state$recycle_warnings,
        broadcast = broadcast,
        broadcast_nd = isTRUE(broadcast_nd),
        fused = fusion_applied,
        fusion = list(
          applied = fusion_applied,
          reason = fusion_reason,
          loop_vars = fusion_vars,
          debug = isTRUE(fusion_debug)
        ),
        memory_check = isTRUE(memory_check),
        df_schema = if (!is.null(.mojor_state$current_df_schema)) .mojor_state$current_df_schema else NULL
      )
      expression_compat <- .ctx_val("expression_origin_compat", default = NULL)
      if (is.list(expression_compat) && length(expression_compat) > 0L) {
        if (!is.null(expression_compat$is_expression_kernel)) {
          trans_out$is_expression_kernel <- isTRUE(expression_compat$is_expression_kernel)
        }
        if (!is.null(expression_compat$return_type)) {
          trans_out$return_type <- expression_compat$return_type
        }
        if (!is.null(expression_compat$out_type)) {
          trans_out$out_type <- expression_compat$out_type
        }
        if (!is.null(expression_compat$is_vector_output)) {
          trans_out$is_vector_output <- isTRUE(expression_compat$is_vector_output)
        }
        passthrough_fields <- c(
          "kernel_args", "literal_array_args", "vector_len_const",
          "dim_builtin", "dim_builtin_mode", "dim_x_name", "dim_expected_ndim",
          "dim_index_kind", "dim_index_value", "dim_index_name",
          "dim_index_c_expr", "dim_index_arg_names",
          "preview_rewrite_fallback", "interpreted_fallback", "na_skip_args"
        )
        for (nm in passthrough_fields) {
          if (!is.null(expression_compat[[nm]])) {
            trans_out[[nm]] <- expression_compat[[nm]]
          }
        }
      }
      trans_out$na_skip_args <- na_skip_args
      if (isTRUE(fusion_debug)) {
        simd_lines <- .mojor_simd_report_lines(trans_out$simd)
        ew_lines <- .mojor_elementwise_report_lines(trans_out$elementwise)
        message(paste(c(simd_lines, ew_lines), collapse = "\n"))
      }
      trans_out
    },
    envir = ctx
  )
}
