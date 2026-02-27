# Transpile loop codegen units (consolidated). Contract: tensor
# wrapper, tiling/reduction/IR prelude-body, and top-level
# orchestration.

.mojor_assert_transpile_loop_ctx <- function(ctx, fn_name) {
    if (!is.environment(ctx)) {
        stop(fn_name, ": ctx must be an environment")
    }
}

# Transpile loop matrix tensor wrapper helper. Contract (ctx):
# appends matrix output LayoutTensor setup to `mojo_lines` and
# updates `.mojor_state$current_tensor_map` when needed.

.mojor_emit_loop_matrix_tensor_wrapper <- function(
    ctx, out_name = NULL, out_type = NULL, skip_when_tiled = FALSE, tiling_applied = NULL
) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_loop_matrix_tensor_wrapper")
    if (is.null(out_name) &&
        exists("out_name", envir = ctx, inherits = TRUE)) {
        out_name <- get("out_name", envir = ctx, inherits = TRUE)
    }
    if (is.null(out_type) &&
        exists("out_type", envir = ctx, inherits = TRUE)) {
        out_type <- get("out_type", envir = ctx, inherits = TRUE)
    }
    if (is.null(tiling_applied) &&
        exists("tiling_applied", envir = ctx, inherits = TRUE)) {
        tiling_applied <- get("tiling_applied", envir = ctx, inherits = TRUE)
    }

    had_out_name <- exists("out_name", envir = ctx, inherits = FALSE)
    had_out_type <- exists("out_type", envir = ctx, inherits = FALSE)
    had_skip_when_tiled <- exists("skip_when_tiled", envir = ctx, inherits = FALSE)
    had_tiling_applied <- exists("tiling_applied", envir = ctx, inherits = FALSE)

    prev_out_name <- if (had_out_name)
        ctx$out_name else NULL
    prev_out_type <- if (had_out_type)
        ctx$out_type else NULL
    prev_skip_when_tiled <- if (had_skip_when_tiled)
        ctx$skip_when_tiled else NULL
    prev_tiling_applied <- if (had_tiling_applied)
        ctx$tiling_applied else NULL

    ctx$out_name <- out_name
    ctx$out_type <- out_type
    ctx$skip_when_tiled <- isTRUE(skip_when_tiled)
    ctx$tiling_applied <- tiling_applied

    on.exit(
        {
            if (had_out_name) ctx$out_name <- prev_out_name else rm(list = "out_name", envir = ctx, inherits = FALSE)
            if (had_out_type) ctx$out_type <- prev_out_type else rm(list = "out_type", envir = ctx, inherits = FALSE)
            if (had_skip_when_tiled) ctx$skip_when_tiled <- prev_skip_when_tiled else rm(list = "skip_when_tiled", envir = ctx, inherits = FALSE)
            if (had_tiling_applied) ctx$tiling_applied <- prev_tiling_applied else rm(list = "tiling_applied", envir = ctx, inherits = FALSE)
        }, add = TRUE
    )

    evalq(
        {
            if (!is.null(out_name) &&
                (!isTRUE(skip_when_tiled) ||
                  !isTRUE(tiling_applied)) &&
                isTRUE(.mojor_state$current_out_is_matrix) &&
                !is.null(.mojor_state$current_out_nrow_var) &&
                !is.null(.mojor_state$current_out_ncol_var)) {
                nrow_var <- .mojor_state$current_out_nrow_var
                ncol_var <- .mojor_state$current_out_ncol_var
                dtype_str <- if (out_type %in% c("i32[]", "i32[,]"))
                  "DType.int32" else if (out_type %in% c("lgl[]", "lgl[,]"))
                  "DType.int32" else if (out_type %in% c("f32[]", "f32[,]"))
                  "DType.float32" else "DType.float64"
                tensor_name <- paste0(out_name, "_tensor")
                default_tensor_name <- paste0("__mojor_tensor_", out_name)
                current_tensor <- NULL
                if (!is.null(.mojor_state$current_tensor_map)) {
                  current_tensor <- .mojor_state$current_tensor_map[[out_name]]
                }
                if (is.null(current_tensor) ||
                  identical(current_tensor, default_tensor_name)) {
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "    var ", out_name, "_layout = ", layout_runtime(
                        "_MOJOR_MATRIX_LAYOUT", paste0(
                          "IndexList[2](Int(", nrow_var, "), Int(", ncol_var,
                          "))"
                      )
                    )
                  )
                )
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "    var ", tensor_name, " = LayoutTensor[mut=True, ",
                      dtype_str, ", _MOJOR_MATRIX_LAYOUT, MutAnyOrigin](",
                      out_name, ", ", out_name, "_layout)"
                  )
                )
                  if (is.null(.mojor_state$current_tensor_map)) {
                    .mojor_state$current_tensor_map <- list()
                  }
                  .mojor_state$current_tensor_map[[out_name]] <- tensor_name
                }
            }
        }, envir = ctx
    )
}

# Transpile loop setup and tiling helper. Contract (ctx):
# initializes loop-local flags (`short`, `custom_reduction_emitted`,
# `elementwise_gpu_buf_info`, `tiling_applied`) and may append tiled
# loop code to `mojo_lines`.

.mojor_emit_transpile_loop_tiling <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_tiling")
    evalq(
        {
 # Loop
            short <- NULL
            custom_reduction_emitted <- FALSE
            elementwise_gpu_buf_info <- NULL
 # Step 8.4: Scalar outputs are now supported by IR
 # (removed out_kind != 'vector' check) Step 8.2: while
 # loops are now handled by IR (removed has_while check)

 # Loop tiling for nested 2D loops (matrix operations)
            use_schedule_tiling <- TRUE
            tiling_applied <- FALSE
            if (!use_schedule_tiling && !is.null(tile) &&
                length(loop_infos) ==
                  1 && out_matrix && !has_while) {
 # Check if the single loop has a nested for loop in
 # its blocks
                li_outer <- loop_infos[[1]]

                if (li_outer$kind == "for" && length(li_outer$blocks) >
                  0) {
 # Look for inner for loop in the outer loop's
 # blocks
                  inner_loop_idx <- .mojor_find_transpile_inner_for_index(li_outer$blocks)

                  if (!is.null(inner_loop_idx)) {
                    inner_block <- li_outer$blocks[[inner_loop_idx]]
                    inner_var <- as.character(inner_block[[2]])
                    inner_seq <- inner_block[[3]]
                    inner_body <- inner_block[[4]]

 # Parse inner loop range
                    scalar_inits <- .mojor_state$current_scalar_inits
                    if (is.null(scalar_inits))
                      scalar_inits <- list()
                    inner_info <- .mojor_parse_loop_seq(inner_seq, arg_specs, args, scalar_inits)
                    if (!is.null(inner_info) &&
                      identical(inner_info$kind, "scalar") &&
                      !is.null(inner_info$name) &&
                      inner_info$name %in% names(len_scalar_map)) {
                      len_info <- len_scalar_map[[inner_info$name]]
                      inner_info$kind <- "array"
                      inner_info$name <- len_info$name
                      inner_info$end_delta <- if (is.null(len_info$delta))
                        0L else len_info$delta
                    }

                    if (li_outer$seq_info$kind %in% c("len", "along", "range", "scalar") &&
                      inner_info$kind %in% c("len", "along", "range", "scalar")) {
                      tiling_applied <- TRUE
                      tile_i <- tile[1]
                      tile_j <- tile[2]

 # Get outer loop range
                      outer_bounds <- .mojor_parse_transpile_range_bounds(li_outer$range_expr, default_start = "1")
                      start_i <- outer_bounds$start
                      end_i <- outer_bounds$end

 # Get inner loop range - use same end as outer
 # for square matrices For matrix operations,
 # both dimensions are typically the same (n) We
 # use the outer loop's end for both to ensure
 # consistency
                      if (inner_info$kind == "scalar" && !is.null(inner_info$name)) {
 # For scalar kind, construct range using the
 # scalar name directly instead of the
 # converted n_i
                        end_j_expr <- paste0("(Int(", inner_info$name, ") + 1)")
                      } else {
                        inner_rng <- .mojor_range_expr(inner_info)
                        inner_bounds <- .mojor_parse_transpile_range_bounds(inner_rng$range, default_start = "1")
                        end_j_expr <- inner_bounds$end
                      }
                      start_j <- "1"
                      end_j <- end_j_expr

                      var_i <- li_outer$var
                      var_j <- inner_var

 # Ensure output tensor wrapper exists before
 # emitting tiled body
                      .mojor_emit_loop_matrix_tensor_wrapper(environment(), skip_when_tiled = FALSE)

 # Get inner loop body blocks
                      inner_blocks <- .mojor_extract_block(inner_body)

 # Generate tiled loop structure
                      mojo_lines <- c(
                        mojo_lines, .mojor_transpile_tiled_loop_header_lines(
                          tile_i = tile_i, tile_j = tile_j, start_i = start_i,
                          end_i = end_i, start_j = start_j, end_j = end_j,
                          var_i = var_i, var_j = var_j
                      )
                    )

 # Generate inner loop body
                      for (s in inner_blocks) {
                        stmt_lines <- .mojor_stmt_to_mojo(
                          s, c(var_i, var_j),
                          inner_info, local_types, out_name, out_type,
                          scalar_name = if (out_kind == "scalar")
                            scalar_init else NULL, scalar_type = scalar_type, indent = "                    ",
                          na_mode = na_mode, na_guard = na_guard
                      )
                        mojo_lines <- c(mojo_lines, stmt_lines)
                      }
                    }
                  }
                }
            }
        }, envir = ctx
    )
}

# Transpile loop reduction and short-circuit prelude helper.
# Contract (ctx): delegates scalar reduction paths and computes
# `short_ir_fallback` for downstream IR loop emission.

.mojor_emit_transpile_loop_reduction_prelude <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_reduction_prelude")
    evalq(
        {
            .mojor_emit_transpile_scalar_reduce_ir_path(environment())
            .mojor_emit_transpile_argext_reduction_path(environment())
            if (!has_while && out_kind == "scalar" && .mojor_is_logical(scalar_type) &&
                length(loop_infos) ==
                  1 && length(loop_infos[[1]]$blocks) ==
                1) {
                short_stmt <- loop_infos[[1]]$blocks[[1]]
                short <- .mojor_detect_short_circuit(short_stmt, scalar_init, scalar_init_value)
            }
            if (!custom_reduction_emitted && !is.null(short) &&
                length(loop_infos) ==
                  1 && length(loop_infos[[1]]$blocks) ==
                1) {
                loop_var <- loop_infos[[1]]$var
                if (.mojor_expr_contains_ifelse(short$cond)) {
                  short <- NULL
                }
            }
 # Route any/all through IR pipeline; keep legacy as
 # fallback in error handler
            short_ir_fallback <- short
            short <- NULL
        }, envir = ctx
    )
}

# Transpile loop IR prelude helper. Contract (ctx): prepares
# short-circuit or elementwise plan state and sets `run_loop_emit`.

.mojor_emit_transpile_loop_ir_prelude <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_ir_prelude")
    evalq(
        {
            .mojor_init_transpile_loop_ctx_defaults(environment())
            run_loop_emit <- FALSE
            if (!.mojor_emit_transpile_short_prelude(environment()) &&
                !custom_reduction_emitted) {
                .mojor_prepare_transpile_elementwise_prelude(environment())
 # Step 8.11: Matrix/array outputs rely on the
 # standard LayoutTensor setup (tensor_ranks) Step
 # 8.11.2: Emit LayoutTensor wrapper for matrix/array
 # outputs in IR mode
                .mojor_emit_loop_matrix_tensor_wrapper(environment(), skip_when_tiled = TRUE)
                iter_len_expr <- .mojor_transpile_iter_len_expr
                .mojor_prepare_transpile_iter_map(environment())
                run_loop_emit <- TRUE
            }
        }, envir = ctx
    )
}

# Transpile loop IR body helper. Contract (ctx): consumes
# `run_loop_emit`/loop planning state and appends emitted loop Mojo
# lines.

.mojor_emit_transpile_loop_ir_body <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_ir_body")
    evalq(
        {
            .mojor_init_transpile_loop_ctx_defaults(environment())
            iter_len_fn <- if (exists("iter_len_expr", inherits = FALSE) &&
                is.function(iter_len_expr))
                iter_len_expr else NULL
            if (isTRUE(run_loop_emit)) {
 # Skip regular loop processing if tiling was applied
                if (tiling_applied) {
 # Tiling already generated all the loop code above
                } else {
                  for (li in loop_infos) {
 # Step 8.2/8.7: Handle for/while/repeat loops in
 # IR
                    loop_body_expr <- .mojor_build_transpile_loop_body_expr(li, short_ir_fallback = short_ir_fallback, scalar_init = scalar_init)

                    loop_ast <- .mojor_build_transpile_loop_ast(li, loop_body_expr, iter_len_expr = iter_len_fn)

                    ir_build_res <- tryCatch(
                      .mojor_ir_build_stmt(loop_ast),
                      error = function(e) e
                  )
                    if (inherits(ir_build_res, "error")) {
                      if (isTRUE(.mojor_state$options$ir_only)) {
                        stop(
                          "mojor_transpile: strict IR build failed: ",
                          conditionMessage(ir_build_res),
                          call. = FALSE
                      )
                      }
                      ir_build_res <- NULL
                    }
                    ir <- ir_build_res
                    ir <- .mojor_ir_service_prepare_transpile_ir_stmt(ir, local_types, dim_map, verify_strict = TRUE)
                    ir <- .mojor_ir_service_run_transpile_ir_pipeline(
                      ctx = environment(), ir = ir, opt_level = opt_level,
                      schedule = schedule, elementwise_plan = elementwise_plan,
                      elementwise_target = elementwise_target, enable_fusion = TRUE
                  )
                    emit_res <- .mojor_ir_service_type_and_emit_stmt(
                      ir = ir, local_types = local_types, out_name = out_name,
                      na_mode = na_mode, bounds_check = .mojor_state$options$index_bounds,
                      scalar_name = scalar_init, schedule = schedule, loop_var = li$var,
                      unroll = unroll, require_kinds = c("loop", "while", "repeat", "block")
                  )
                    ir_typed <- emit_res$ir_typed
                    mojo_loop <- emit_res$mojo_lines
                    if (is.null(mojo_loop) ||
                      length(mojo_loop) ==
                        0 || any(!nzchar(mojo_loop))) {
                      if (.mojor_emit_transpile_short_ir_fallback(environment())) {
                        next
                      }

 # Enhanced error diagnostics
                      diag <- .mojor_diagnose_loop_failure(loop_ast, ir, ir_typed)
                      error_msg <- paste0("Loop not supported: ", diag$reason)
                      .mojor_err(error_msg, loop_ast, diag$hint)
                    }
                    mojo_lines <- c(mojo_lines, mojo_loop)
                    next
                  }
                }
            }
        }, envir = ctx
    )
}

# Transpile loop orchestration helpers. Contract: top-level
# orchestration for loop prelude, loop IR emit, and loop codegen.

.mojor_emit_transpile_loop_prelude <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_prelude")
    .mojor_emit_transpile_loop_tiling(ctx)
    .mojor_emit_transpile_loop_reduction_prelude(ctx)
}

.mojor_emit_transpile_loop_ir_emit <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_ir_emit")
    .mojor_emit_transpile_loop_ir_prelude(ctx)
    .mojor_emit_transpile_loop_ir_body(ctx)
}

.mojor_emit_transpile_loop_codegen <- function(ctx) {
    .mojor_assert_transpile_loop_ctx(ctx, ".mojor_emit_transpile_loop_codegen")
    .mojor_emit_transpile_loop_prelude(ctx)
    .mojor_emit_transpile_loop_ir_emit(ctx)
}
