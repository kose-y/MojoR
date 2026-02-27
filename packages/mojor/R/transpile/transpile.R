# Transpile entrypoints: mojor_transpile and report helpers.

.mojor_assign_local_fields <- function(
    source, fields, env = parent.frame(), allow_missing = FALSE, missing_value = NULL
) {
    if (is.environment(source)) {
        source_names <- ls(source, all.names = TRUE)
        has_field <- function(nm) exists(nm, envir = source, inherits = FALSE)
        get_field <- function(nm) get(nm, envir = source, inherits = FALSE)
    } else {
        if (!is.list(source)) {
            stop(
                ".mojor_assign_local_fields: source must be a named list or environment"
            )
        }
        source_names <- names(source)
        if (is.null(source_names)) {
            source_names <- character(0)
        }
        has_field <- function(nm) nm %in% source_names
        get_field <- function(nm) source[[nm]]
    }
    missing_fields <- setdiff(fields, source_names)
    if (!isTRUE(allow_missing) &&
        length(missing_fields) >
            0L) {
        stop(
            ".mojor_assign_local_fields: missing fields: ", paste(missing_fields, collapse = ", ")
        )
    }
    for (nm in fields) {
        value <- if (isTRUE(has_field(nm)))
            get_field(nm) else missing_value
        assign(nm, value, envir = env)
    }
    invisible(NULL)
}

.mojor_walk_call_parts_safe <- function(parts, walker) {
    if (length(parts) ==
        0) {
        return(invisible(NULL))
    }
    for (idx in seq_along(parts)) {
        is_missing_part <- FALSE
        if (is.symbol(parts[[idx]])) {
            name <- as.character(parts[[idx]])
            is_missing_part <- length(name) == 1L && identical(name[[1]], "")
        }
        if (isTRUE(is_missing_part))
            next
        walker(parts[[idx]])
    }
    invisible(NULL)
}

.mojor_collect_from_call_tree <- function(expr, collector) {
    out <- character(0)
    walk <- function(node) {
        if (is.null(node)) {
            return(invisible(NULL))
        }
        if (is.symbol(node)) {
            sym_name <- as.character(node)
            if (length(sym_name) == 1L && identical(sym_name[[1L]], "")) {
                return(invisible(NULL))
            }
        }
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        found <- collector(node)
        if (!is.null(found) &&
            length(found) >
                0) {
            out <<- c(out, found)
        }
        parts <- as.list(node)[-1]
        if (length(parts) >
            0) {
            .mojor_walk_call_parts_safe(parts, walk)
        }
        invisible(NULL)
    }
    walk(expr)
    unique(out)
}

.mojor_collect_from_blocks <- function(blocks, collector) {
    if (length(blocks) ==
        0) {
        return(character(0))
    }
    unique(
        unlist(
            lapply(
                blocks, function(expr) .mojor_collect_from_call_tree(expr, collector)
            ),
            use.names = FALSE
        )
    )
}

.mojor_attach_transpile_route_metadata <- function(trans_out, route_info) {
    if (!is.list(trans_out)) {
        return(trans_out)
    }
    trans_out$transpile_route <- .mojor_transpile_route_metadata(route_info)
    trans_out
}

.mojor_transpile_entry_opt_fields <- c(
    "na_mode", "semantics", "ir_dump", "opt_level", "unroll", "reduction",
    "bounds_check", "bounds_check_input", "index_base", "array_layout", "layout_kind", "layout_ctor",
    "layout_runtime", "tile", "schedule", "na_guard", "na_scan", "simd_mode",
    "assume_aligned", "index_bounds", "elementwise", "elementwise_target",
    "elementwise_size", "elementwise_cpu", "elementwise_gpu_layouttensor",
    "gpu_jit_mode", "broadcast", "broadcast_nd", "parallel"
)

.mojor_transpile_analysis_fields <- c(
    "out_name", "out_type", "out_matrix", "out_nrow_expr", "out_ncol_expr",
    "out_array", "out_dim_exprs", "out_dim_name", "out_len_source", "loops",
    "scalar_inits", "array_aliases", "pre_loop_out_assigns", "scalar_init", "scalar_type",
    "scalar_init_value", "scalar_reduction_op", "scalar_reduction_arg",
    "scalar_reduce_ir_node", "return_name", "return_expr", "return_div",
    "mean_ignore_pending", "mean_ignore_expr", "mean_ignore_loop_var",
    "any_all_call", "any_all_arg", "any_all_mode", "seq_extra", "constructor_len_arrays",
    "na_rm", "modified_args"
)

.mojor_transpile_no_loop_fields <- c(
    "blocks", "loops", "any_all_call", "any_all_arg", "any_all_mode", "seq_extra",
    "scalar_init", "scalar_type", "scalar_init_value", "scalar_inits",
    "return_name", "scalar_reduction_op", "scalar_reduction_arg", "scalar_reduce_ir_node",
    "out_name", "out_type", "out_matrix", "out_array", "out_dim_exprs",
    "out_dim_name", "constructor_len_arrays"
)

mojor_transpile <- function(
    fn, ..., name = "mojor_kernel", na_mode = NULL, fusion_debug = FALSE,
    assume_aligned = NULL, simd_mode = NULL, elementwise = FALSE, elementwise_target = c("cpu", "gpu"),
    elementwise_size = NULL, elementwise_cpu = NULL, elementwise_gpu_layouttensor = NULL,
    gpu_jit_mode = NULL, broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    ),
    parallel = FALSE, allow_in_place = FALSE, semantics = c("r", "raw"),
    unroll = NULL, reduction = NULL, bounds_check = NULL, index_base = NULL,
    array_layout = NULL, tile = NULL, emit_ir = FALSE, emit_ssa_backend = FALSE,
    opt_level = NULL, fusion = TRUE, debug = FALSE, trace = FALSE, memory_check = FALSE,
    df_schema = NULL, profile = FALSE
) {
    if (!is.function(fn))
        stop("mojor_transpile: fn must be a function")
    types <- list(...)
    gpu_jit_mode_user_set <- !is.null(gpu_jit_mode)
    entry_opts <- .mojor_prepare_transpile_entry_options(
        fn = fn, na_mode = na_mode, semantics = semantics, emit_ir = emit_ir,
        emit_ssa_backend = emit_ssa_backend, opt_level = opt_level, unroll = unroll,
        reduction = reduction, bounds_check = bounds_check, index_base = index_base,
        array_layout = array_layout, tile = tile, simd_mode = simd_mode,
        assume_aligned = assume_aligned, elementwise = elementwise, elementwise_target = elementwise_target,
        elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
        elementwise_gpu_layouttensor = elementwise_gpu_layouttensor, gpu_jit_mode = gpu_jit_mode,
        broadcast = broadcast, parallel = parallel, allow_in_place = allow_in_place,
        fusion = fusion
    )

    .mojor_assign_local_fields(entry_opts, .mojor_transpile_entry_opt_fields, env = environment())
    unroll_explicit <- isTRUE(entry_opts$unroll_explicit)
    reduction_explicit <- isTRUE(entry_opts$reduction_explicit)
    tile_explicit <- isTRUE(entry_opts$tile_explicit)
    unroll_requested <- unroll
    reduction_requested <- reduction
    tile_requested <- tile
    unroll_effective <- unroll
    reduction_effective <- reduction
    tile_effective <- tile
    auto_schedule_policy <- NULL
    schedule_effective <- schedule
    gpu_jit_elementwise_auto <- FALSE
    route_info <- list(route = "loop", reason = "loop_path_default")
    transpile_route <- .mojor_transpile_route_metadata(route_info)
    expression_origin_compat <- NULL
    on.exit(entry_opts$restore_index_bounds(), add = TRUE)

 # IR is now always used (legacy transpiler removed)

    state_ctx <- .mojor_setup_transpile_state_context(na_mode = na_mode)
    on.exit(state_ctx$restore(), add = TRUE)

 # Store debug mode flags (Stage A) memory_check implies
 # debug mode (Stage B)
    if (isTRUE(memory_check))
        debug <- TRUE
    .mojor_state$debug_mode <- isTRUE(debug)
    .mojor_state$trace_mode <- isTRUE(trace)
    .mojor_state$memory_check <- isTRUE(memory_check)
    .mojor_state$current_typed_lir_payloads <- list()
    .mojor_state$current_ir_emit_diagnostics <- list()

 # Step 5: Initialize line number estimate Start at line
 # 2 (after function signature)
    .mojor_state$current_line_estimate <- 2

    formals_fn <- formals(fn)
    args <- names(formals_fn)
    if (is.null(args)) {
        if (length(formals_fn) == 0L) {
            args <- character(0)
        } else {
            stop("mojor_transpile: function formals must be named")
        }
    }
    if (length(args) > 0L && any(!nzchar(args))) {
        stop("mojor_transpile: function formals must be named")
    }
    if (length(args) > 0L && anyDuplicated(args)) {
        stop("mojor_transpile: function formals must have unique names")
    }
    if (length(args) > 0L && length(types) == 0L) {
        stop("mojor_transpile: provide type annotations")
    }
    if (length(args) == 0L && length(types) > 0L) {
        stop("mojor_transpile: function has no formal arguments but type annotations were provided")
    }
    types <- .mojor_normalize_arg_specs(args, types)
    types_input <- types
    strict_ir_only <- isTRUE(.mojor_state$options$ir_only)
    detected_return_ctor_kind <- .mojor_tier9_return_ctor_kind(fn)
    return_ctor_kind <- .mojor_return_ctor_for_mode(
        return_ctor_kind = detected_return_ctor_kind, strict_ir_only = strict_ir_only,
        context = "mojor_transpile"
    )
    tier9_requested <- !is.null(df_schema) ||
        any(vapply(types[args], .mojor_type_is_tier9, logical(1))) ||
        !is.null(return_ctor_kind)
    if (isTRUE(tier9_requested)) {
        .mojor_require_tier9_ready("compiled subset transpilation")
    }
    df_schema_norm <- .mojor_validate_df_schema(args, types, df_schema)
    df_args <- args[vapply(
        args, function(a) identical(types[[a]], "df"),
        logical(1)
    )]
    if (length(df_args) >
        0) {
        rewrite <- .mojor_df_rewrite_callable(
            fn = fn, arg_specs = types, df_schema = df_schema_norm, context = "mojor_transpile"
        )
        trans_df <- do.call(
            mojor_transpile, c(
                list(fn = rewrite$fn),
                rewrite$arg_specs, list(
                  name = name, na_mode = na_mode, fusion_debug = fusion_debug,
                  assume_aligned = assume_aligned, simd_mode = simd_mode,
                  elementwise = elementwise, elementwise_target = elementwise_target,
                  elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
                  elementwise_gpu_layouttensor = elementwise_gpu_layouttensor,
                  gpu_jit_mode = gpu_jit_mode, broadcast = broadcast, parallel = parallel,
                  allow_in_place = allow_in_place, semantics = semantics,
                  unroll = unroll, reduction = reduction, bounds_check = bounds_check_input,
                  index_base = index_base, array_layout = array_layout,
                  tile = tile, emit_ir = emit_ir, emit_ssa_backend = emit_ssa_backend,
                  opt_level = opt_level, fusion = fusion, debug = debug,
                  trace = trace, memory_check = memory_check, df_schema = NULL
              )
            )
        )
        trans_df$df_schema <- df_schema_norm
        trans_df$df_hidden_map <- rewrite$hidden_map
        trans_df$df_dynamic_selectors <- rewrite$dynamic_selectors
        trans_df$df_rewritten <- TRUE
        return(trans_df)
    }
    if (!is.null(return_ctor_kind)) {
        ctor <- .mojor_tier9_parse_return_ctor(fn, context = "mojor_transpile")
        .mojor_tier9_validate_ctor_arg_specs(ctor = ctor, arg_specs = types, context = "mojor_transpile")
        return(
            list(
                name = name, mojo = "", signature = character(0),
                ir = NULL, notes = c("compiled subset constructor metadata path (runtime assembly in mojor_build)"),
                types = types, tier9_return_ctor = list(
                  kind = ctor$kind, names = ctor$names, part_names = paste0(
                    name, "__tier9_", if (identical(ctor$kind, "data.frame")) "df" else "list",
                    "_", seq_along(ctor$entries)
                )
              ),
                tier9_part_trans = list(), tier9_ctor_runtime_only = TRUE,
                tier9_ctor_rewritten = TRUE, df_schema = df_schema_norm
            )
        )
    }
    .mojor_state$current_df_schema <- df_schema_norm

    blocks <- .mojor_extract_block(body(fn))
    blocks <- .mojor_expand_chained_assignments_blocks(blocks)
    .mojor_validate_negative_indices(blocks)
    blocks <- .mojor_canonicalize_no_loop_apply_blocks(blocks)
    .mojor_scan_named_vector_assignments(blocks)
    chr_index_hashed_args <- .mojor_collect_chr_index_arg_vars(blocks, types_input)
    chr_index_hashed_args <- intersect(chr_index_hashed_args, names(types))
    if (length(chr_index_hashed_args) > 0) {
        types[chr_index_hashed_args] <- rep("i32[]", length(chr_index_hashed_args))
    }
    local_chr_info <- .mojor_collect_chr_index_local_vars(blocks)
    if (length(local_chr_info$used) > 0) {
        for (lcv in local_chr_info$used) {
            types[[lcv]] <- "i32[]"
        }
        # Local chr vars use compile-time position resolution, NOT hash maps.
        # Do NOT add to chr_index_hashed_args (that triggers hash map lowering).
    }
    .mojor_state$current_chr_index_hashed_args <- chr_index_hashed_args
    .mojor_state$current_chr_index_local_vars <- local_chr_info

 # Scan blocks for RNG usage before code generation.
    rng_calls <- .mojor_blocks_collect_rng_calls(blocks)
    .mojor_state$current_rng_calls <- rng_calls
    if (length(rng_calls) >
        0) {
        .mojor_state$needs_mojo_random <- TRUE
    }
    .mojor_state$needs_rng_tables <- .mojor_blocks_need_rng_tables(blocks)

 # Scan blocks for FFI (mojor_c_call) usage
    if (.mojor_blocks_need_ffi(blocks)) {
        .mojor_state$needs_ffi <- TRUE
    }

 # Track NA mode for helper emission
    .mojor_state$current_na_mode <- na_mode
    .mojor_state$needs_na_helpers <- FALSE

 # /7.4: Track set/match and quantile helper needs
    .mojor_state$needs_set_match <- FALSE
    .mojor_state$needs_quantile <- FALSE

 # Detect output vector allocation and loop/return structure.
    analysis_state <- .mojor_scan_blocks_assignments_and_returns(blocks = blocks, args = args, types = types)
    .mojor_assign_local_fields(
        analysis_state, .mojor_transpile_analysis_fields, env = environment(),
        allow_missing = TRUE
    )
    .mojor_validate_negative_write_targets(
        blocks,
        out_name = out_name,
        return_name = return_name,
        modified_args = modified_args
    )
    if (is.null(array_aliases)) {
        array_aliases <- list()
    }
    .mojor_state$current_scalar_vars <- names(scalar_inits)
    no_loop_scalar_any_all <- FALSE

    pre_loop_out_assigns <- .mojor_collect_pre_loop_out_assignments(
        blocks = blocks, out_name = out_name, scalar_inits = scalar_inits,
        args = args, types = types, array_aliases = array_aliases
    )
    post_loop_out_assigns <- .mojor_collect_post_loop_out_assignments(
        blocks = blocks, out_name = out_name, scalar_inits = scalar_inits,
        args = args, types = types, array_aliases = array_aliases
    )

    no_loop_state <- .mojor_resolve_no_loop_synthesis(
        blocks = blocks, loops = loops, args = args, types = types, any_all_call = any_all_call,
        any_all_arg = any_all_arg, any_all_mode = any_all_mode, seq_extra = seq_extra,
        scalar_init = scalar_init, scalar_type = scalar_type, scalar_init_value = scalar_init_value,
        scalar_inits = scalar_inits, return_name = return_name, scalar_reduction_op = scalar_reduction_op,
        scalar_reduction_arg = scalar_reduction_arg, scalar_reduce_ir_node = scalar_reduce_ir_node,
        out_name = out_name, out_type = out_type, out_matrix = out_matrix,
        out_array = out_array, out_dim_exprs = out_dim_exprs, out_dim_name = out_dim_name,
        constructor_len_arrays = constructor_len_arrays, na_rm = na_rm
    )
    .mojor_assign_local_fields(
        no_loop_state, .mojor_transpile_no_loop_fields, env = environment(),
        allow_missing = TRUE
    )
    no_loop_scalar_any_all <- isTRUE(no_loop_state$no_loop_scalar_any_all)

    if (isTRUE(index_bounds)) {
        route_info <- list(
            route = "loop",
            reason = "strict_index_bounds_loop_route",
            expression_candidate = FALSE,
            normalization_enabled = FALSE,
            normalization = list(applied = FALSE),
            unified_gpu_ops = character(0),
            normalized_blocks = NULL,
            expression_compat = NULL
        )
    } else {
        route_info <- .mojor_classify_transpile_route(
            blocks = blocks,
            loops = loops,
            no_loop_scalar_any_all = no_loop_scalar_any_all,
            gpu_jit_mode = gpu_jit_mode,
            args = args,
            types = types,
            name = name
        )
    }
    if (is.list(route_info$normalization) &&
        isTRUE(route_info$normalization$applied) &&
        is.list(route_info$normalized_blocks) &&
        length(route_info$normalized_blocks) > 0L) {
        blocks <- route_info$normalized_blocks
        expression_origin_compat <- route_info$expression_compat
        analysis_state <- .mojor_scan_blocks_assignments_and_returns(
            blocks = blocks, args = args, types = types
        )
        .mojor_assign_local_fields(
            analysis_state, .mojor_transpile_analysis_fields, env = environment(),
            allow_missing = TRUE
        )
        .mojor_validate_negative_write_targets(
            blocks,
            out_name = out_name,
            return_name = return_name,
            modified_args = modified_args
        )
        if (is.null(array_aliases)) {
            array_aliases <- list()
        }
        .mojor_state$current_scalar_vars <- names(scalar_inits)
        pre_loop_out_assigns <- .mojor_collect_pre_loop_out_assignments(
            blocks = blocks, out_name = out_name, scalar_inits = scalar_inits,
            args = args, types = types, array_aliases = array_aliases
        )
        post_loop_out_assigns <- .mojor_collect_post_loop_out_assignments(
            blocks = blocks, out_name = out_name, scalar_inits = scalar_inits,
            args = args, types = types, array_aliases = array_aliases
        )
        no_loop_state <- .mojor_resolve_no_loop_synthesis(
            blocks = blocks, loops = loops, args = args, types = types, any_all_call = any_all_call,
            any_all_arg = any_all_arg, any_all_mode = any_all_mode, seq_extra = seq_extra,
            scalar_init = scalar_init, scalar_type = scalar_type, scalar_init_value = scalar_init_value,
            scalar_inits = scalar_inits, return_name = return_name, scalar_reduction_op = scalar_reduction_op,
            scalar_reduction_arg = scalar_reduction_arg, scalar_reduce_ir_node = scalar_reduce_ir_node,
            out_name = out_name, out_type = out_type, out_matrix = out_matrix,
            out_array = out_array, out_dim_exprs = out_dim_exprs, out_dim_name = out_dim_name,
            constructor_len_arrays = constructor_len_arrays, na_rm = na_rm
        )
        .mojor_assign_local_fields(
            no_loop_state, .mojor_transpile_no_loop_fields, env = environment(),
            allow_missing = TRUE
        )
        no_loop_scalar_any_all <- isTRUE(no_loop_state$no_loop_scalar_any_all)
    }
    transpile_route <- .mojor_transpile_route_metadata(route_info)
    if (is.list(route_info$expression_compat) &&
        is.list(route_info$expression_compat$prebuilt_trans)) {
        return(
            .mojor_attach_transpile_route_metadata(
                route_info$expression_compat$prebuilt_trans,
                route_info
            )
        )
    }

    reduction_return_div_map <- c(mean = "count", var = "var_count", sd = "sd_count")
    if (!is.null(scalar_reduction_op) &&
        scalar_reduction_op %in% names(reduction_return_div_map)) {
        return_div <- unname(reduction_return_div_map[[scalar_reduction_op]])
    }

    if (!is.null(out_name)) {
        mask_name <- .mojor_detect_mask_output_assign(blocks, out_name, types)
        if (!is.null(mask_name)) {
            out_len_source <- list(kind = "mask_true", name = mask_name)
        }
    }

    # Detect R-style exclusion: out <- x[-k] (post-loop re-assignment)
    # Keep explicit expression-based output lengths inferred during analysis.
    if (!is.null(out_name) &&
        (is.null(out_len_source) || !identical(out_len_source$kind, "expr"))) {
        for (b in blocks) {
            op_b <- if (is.call(b)) as.character(b[[1]])[1L] else NULL
            if (is.call(b) && (op_b %in% c("<-", "=")) && length(b) >= 3) {
                lhs_b <- b[[2]]
                rhs_b <- b[[3]]
                rhs_op_b <- if (is.call(rhs_b)) as.character(rhs_b[[1]])[1L] else NULL
                if (is.name(lhs_b) && as.character(lhs_b) == out_name &&
                    is.call(rhs_b) && identical(rhs_op_b, "[")) {
                    base_b <- rhs_b[[2]]
                    n_idx <- length(rhs_b) - 2L
                    if (is.name(base_b) && n_idx == 1L) {
                        # 1D exclusion: x[-k] or x[-c(a,b)]
                        idx_b <- rhs_b[[3]]
                        neg_info <- .mojor_ir_detect_negative_index(idx_b)
                        if (neg_info$is_negative && !isTRUE(neg_info$is_vector)) {
                            base_name_b <- as.character(base_b)
                            out_len_source <- list(kind = "exclusion", base = base_name_b,
                                                   neg_value = neg_info$value,
                                                   is_dynamic = isTRUE(neg_info$is_dynamic))
                        } else if (neg_info$is_negative && isTRUE(neg_info$is_vector)) {
                            vec_excl <- .mojor_ir_build_vector_exclusion_index(neg_info)
                            if (!is.null(vec_excl)) {
                                base_name_b <- as.character(base_b)
                                out_len_source <- list(kind = "exclusion", base = base_name_b,
                                                       neg_value = neg_info$value,
                                                       is_dynamic = FALSE,
                                                       excl_count = vec_excl$neg_vec_exclusion$count)
                            }
                        }
                    } else if (is.name(base_b) && n_idx >= 2L) {
                        # N-d subscript: mat[-1, ], arr[-1, , -2], arr[c(1,3), ], etc.
                        # At least one dim must be special (neg excl, neg vec excl, or pos vec sel)
                        # Rest can be missing or positive scalar
                        base_name_b <- as.character(base_b)
                        excl_dims_b <- integer(0)
                        excl_infos_b <- list()
                        scalar_dims_b <- integer(0)
                        vec_sel_dims_b <- integer(0)
                        vec_sel_infos_b <- list()
                        neg_vec_excl_dims_b <- integer(0)
                        neg_vec_excl_infos_b <- list()
                        all_valid <- TRUE
                        for (d in seq_len(n_idx)) {
                            idx_d <- tryCatch(rhs_b[[d + 2L]], error = function(e) NULL)
                            is_missing_d <- tryCatch({
                                is.symbol(idx_d) && identical(as.character(idx_d), "")
                            }, error = function(e) {
                                msg <- conditionMessage(e)
                                grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)
                            })
                            if (isTRUE(is_missing_d)) {
                                next
                            }
                            neg_d <- .mojor_ir_detect_negative_index(idx_d)
                            if (neg_d$is_negative && !isTRUE(neg_d$is_vector)) {
                                # Scalar negative exclusion: -1, -k
                                excl_dims_b <- c(excl_dims_b, d)
                                excl_infos_b[[length(excl_infos_b) + 1L]] <- neg_d
                            } else if (neg_d$is_negative && isTRUE(neg_d$is_vector)) {
                                # Vector negative exclusion: -c(1,2), -(1:3)
                                neg_vec_excl_dims_b <- c(neg_vec_excl_dims_b, d)
                                neg_vec_excl_infos_b[[length(neg_vec_excl_infos_b) + 1L]] <- neg_d
                            } else if (is.call(idx_d) && !neg_d$is_negative) {
                                # Positive vector selection: c(1,3), 1:3
                                idx_op <- as.character(idx_d[[1]])
                                if (idx_op == "c") {
                                    elems <- as.list(idx_d)[-1]
                                    sel_ok <- all(vapply(elems, function(e) {
                                        (is.numeric(e) && length(e) == 1 && !is.na(e) && e > 0) || is.name(e)
                                    }, logical(1)))
                                    if (sel_ok && length(elems) > 0L) {
                                        vec_sel_dims_b <- c(vec_sel_dims_b, d)
                                        vec_sel_infos_b[[length(vec_sel_infos_b) + 1L]] <- list(
                                            type = "c", count = length(elems))
                                    } else { all_valid <- FALSE; break }
                                } else if (idx_op == ":" && length(idx_d) == 3) {
                                    a <- idx_d[[2]]; b <- idx_d[[3]]
                                    a_ok <- (is.numeric(a) && a > 0) || is.name(a)
                                    b_ok <- (is.numeric(b) && b > 0) || is.name(b)
                                    if (a_ok && b_ok) {
                                        cnt <- if (is.numeric(a) && is.numeric(b)) {
                                            as.integer(b) - as.integer(a) + 1L
                                        } else { NULL }
                                        vec_sel_dims_b <- c(vec_sel_dims_b, d)
                                        vec_sel_infos_b[[length(vec_sel_infos_b) + 1L]] <- list(
                                            type = "range", count = cnt,
                                            start_r = a, end_r = b)
                                    } else { all_valid <- FALSE; break }
                                } else {
                                    all_valid <- FALSE; break
                                }
                            } else if (is.numeric(idx_d) || is.name(idx_d)) {
                                # Positive scalar index (literal or variable)
                                scalar_dims_b <- c(scalar_dims_b, d)
                            } else {
                                all_valid <- FALSE
                                break
                            }
                        }
                        n_special <- length(excl_dims_b) + length(neg_vec_excl_dims_b) + length(vec_sel_dims_b)
                        if (all_valid && n_special > 0L) {
                            neg_values <- if (length(excl_infos_b) > 0L) {
                                vapply(excl_infos_b, function(info) as.character(info$value), character(1))
                            } else { character(0) }
                            is_dyn <- any(vapply(excl_infos_b, function(info) isTRUE(info$is_dynamic), logical(1)))
                            out_len_source <- list(kind = "nd_exclusion",
                                                   base = base_name_b,
                                                   n_dims = n_idx,
                                                   excl_dims = excl_dims_b,
                                                   excl_dim = if (length(excl_dims_b) > 0L) excl_dims_b[1L] else NULL,
                                                   scalar_dims = scalar_dims_b,
                                                   vec_sel_dims = vec_sel_dims_b,
                                                   vec_sel_infos = vec_sel_infos_b,
                                                   neg_vec_excl_dims = neg_vec_excl_dims_b,
                                                   neg_vec_excl_infos = neg_vec_excl_infos_b,
                                                   neg_values = neg_values,
                                                   neg_value = if (length(neg_values) > 0L) neg_values[1L] else NULL,
                                                   is_dynamic = is_dyn)
                        }
                    }
                }
            }
        }
    }

    preview_ops <- .mojor_collect_from_blocks(
        blocks, function(expr) {
            op <- as.character(expr[[1]])
            if (!is.character(op) ||
                length(op) ==
                  0L) {
                return(NULL)
            }
            op[[1]]
        }
    )
    if (length(preview_ops) >
        0) {
        .mojor_state$needs_set_match <- any(
            preview_ops %in% c("unique", "duplicated", "anyDuplicated", "match", "%in%")
        )
        .mojor_state$needs_quantile <- any(preview_ops %in% c("median", "quantile", "IQR", "mad"))
    }

    .mojor_validate_char_indexing(blocks, .mojor_state$matrix_dimnames)

 # Route to expression-only transpiler via centralized classifier.
 # Any expression-lowering failure now falls through to loop/IR lowering so
 # no-loop bodies can share loop capabilities when available.
    reroute_expression_only <- FALSE
    reroute_expression_only_reason <- NULL
    expression_only_error <- NULL
    has_local_length_ctor <- .mojor_collect_from_blocks(
        blocks,
        function(expr) {
            expr_op <- if (is.call(expr)) as.character(expr[[1]])[1L] else NULL
            if (!is.call(expr) || !(expr_op %in% c("<-", "=")) || length(expr) < 3L) {
                return(NULL)
            }
            rhs <- expr[[3L]]
            if (!is.call(rhs)) {
                return(NULL)
            }
            rhs_op <- as.character(rhs[[1L]])[1L]
            if (!(rhs_op %in% c("numeric", "integer", "logical")) || length(rhs) < 2L) {
                return(NULL)
            }
            len_arg <- rhs[[2L]]
            if (!is.call(len_arg) || !identical(as.character(len_arg[[1L]]), "length") || length(len_arg) != 2L) {
                return(NULL)
            }
            len_name <- len_arg[[2L]]
            if (!is.name(len_name)) {
                return(NULL)
            }
            local_nm <- as.character(len_name)
            if (local_nm %in% args) {
                return(NULL)
            }
            TRUE
        }
    )
    has_local_length_ref <- .mojor_collect_from_blocks(
        blocks,
        function(expr) {
            if (!is.call(expr) || !identical(as.character(expr[[1L]]), "length") || length(expr) != 2L) {
                return(NULL)
            }
            len_name <- expr[[2L]]
            if (!is.name(len_name)) {
                return(NULL)
            }
            local_nm <- as.character(len_name)
            if (local_nm %in% args) {
                return(NULL)
            }
            local_nm
        }
    )
    if (length(has_local_length_ctor) > 0L || length(has_local_length_ref) > 0L) {
        reroute_expression_only <- TRUE
        reroute_expression_only_reason <- "expression_only_local_length_ctor_to_ir_loop"
    }
    if (identical(route_info$route, "expression_only") &&
        !is.null(out_len_source) &&
        identical(out_len_source$kind, "expr")) {
        out_len_expr <- out_len_source$expr
        if (is.name(out_len_expr) || is.call(out_len_expr)) {
            refs <- all.names(out_len_expr, functions = FALSE, unique = TRUE)
            refs <- setdiff(
                refs,
                c(
                    "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf",
                    "NA_real_", "NA_integer_", "NA_character_", "NA_complex_"
                )
            )
            local_scalar_names <- if (!is.null(scalar_inits) && length(scalar_inits) > 0) {
                names(scalar_inits)
            } else {
                character(0)
            }
            allowed_refs <- unique(c(args, local_scalar_names))
            if (length(setdiff(refs, allowed_refs)) > 0) {
                reroute_expression_only <- TRUE
                reroute_expression_only_reason <- "expression_only_local_out_len_expr_to_ir_loop"
            }
        }
    }
    if (identical(route_info$route, "expression_only") &&
        length(args) == 0L &&
        length(loops) == 0L) {
        stop("mojor_transpile: while loops require at least one array argument to determine n")
    }
    if (identical(route_info$route, "expression_only")) {
        if (!isTRUE(reroute_expression_only)) {
            expr_out <- tryCatch(
                .mojor_transpile_expression_only(blocks, args, types, name = name),
                error = function(e) e
            )
            if (!inherits(expr_out, "error")) {
                return(.mojor_attach_transpile_route_metadata(expr_out, route_info))
            }
            expr_err <- conditionMessage(expr_out)
            expression_only_error <- expr_err
            route_info$route <- "loop"
            route_info$reason <- if (
                is.character(expr_err) &&
                length(expr_err) == 1L &&
                grepl("expression-only mode currently only supports", expr_err, fixed = TRUE)
            ) {
                "expression_only_rejected_to_ir_loop"
            } else {
                "expression_only_error_to_ir_loop"
            }
        } else {
            route_info$route <- "loop"
            route_info$reason <- if (!is.null(reroute_expression_only_reason)) {
                reroute_expression_only_reason
            } else {
                "expression_only_local_out_len_expr_to_ir_loop"
            }
        }
    }
    if (!isTRUE(.mojor_state$options$ir_only) &&
        isTRUE(route_info$expression_candidate) &&
        identical(route_info$reason, "no_loop_loop_ir_fallback") &&
        length(loops) == 0L) {
        expr_out <- tryCatch(
            .mojor_transpile_expression_only(blocks, args, types, name = name),
            error = function(e) e
        )
        if (!inherits(expr_out, "error")) {
            return(.mojor_attach_transpile_route_metadata(expr_out, route_info))
        }
        expression_only_error <- conditionMessage(expr_out)
    }

    if (is.null(scalar_init) &&
        !is.null(return_name) &&
        (is.null(out_name) ||
            return_name != out_name) && return_name %in% names(scalar_inits)) {
        scalar_init <- return_name
        scalar_type <- scalar_inits[[return_name]]$type
        scalar_init_value <- scalar_inits[[return_name]]$value
    }
    if (is.null(scalar_init) &&
        is.null(return_name) &&
        is.null(out_name) &&
        length(scalar_inits) ==
            1) {
        scalar_init <- names(scalar_inits)[1]
        scalar_type <- scalar_inits[[scalar_init]]$type
        scalar_init_value <- scalar_inits[[scalar_init]]$value
    }

 # Extract seq aliases (v <- seq_len(n), v <- seq_along(x), etc.)
    seq_aliases <- list()
    for (b in blocks) {
        b_op <- if (is.call(b)) as.character(b[[1]])[1L] else NULL
        if (is.call(b) && identical(b_op, "<-") &&
            is.name(b[[2]])) {
            lhs <- as.character(b[[2]])
            rhs <- b[[3]]
            if (is.call(rhs)) {
                rhs_op <- as.character(rhs[[1]])[1L]
                if (rhs_op %in% c("seq_len", "seq_along", "seq", "seq.int")) {
                    seq_aliases[[lhs]] <- rhs
                }
            }
        }
    }
    prev_seq_aliases <- tryCatch(.mojor_state$current_seq_aliases, error = function(e) NULL)
    .mojor_state$current_seq_aliases <- seq_aliases
    on.exit(
        {
            .mojor_state$current_seq_aliases <- prev_seq_aliases
        }, add = TRUE
    )

    loop_infos <- .mojor_collect_loop_infos(loops)

 # Build Mojo code
    arg_specs <- types[args]
    .mojor_state$current_args <- args
    .mojor_state$current_arg_specs <- arg_specs
    prev_array_aliases <- .mojor_state$current_array_aliases
    .mojor_state$current_array_aliases <- array_aliases
    on.exit(
        {
            .mojor_state$current_array_aliases <- prev_array_aliases
        }, add = TRUE
    )
    len_scalar_map <- list()
    if (length(scalar_inits) >
        0) {
        for (nm in names(scalar_inits)) {
            val <- scalar_inits[[nm]]$value
            if (is.list(val) &&
                identical(val$kind, "len")) {
                len_scalar_map[[nm]] <- list(name = val$name, delta = val$delta)
            }
        }
    }
    len_scalar_specs <- list()
    if (length(len_scalar_map) >
        0) {
        len_scalar_specs <- setNames(
            rep("i32", length(len_scalar_map)),
            names(len_scalar_map)
        )
    }
    loop_args <- unique(c(args, names(len_scalar_specs)))
    loop_arg_specs <- c(arg_specs, len_scalar_specs)
    for (k in names(arg_specs)) {
        if (is.null(.mojor_mojo_type(arg_specs[[k]]))) {
            stop("mojor_transpile: unsupported type spec for ", k)
        }
    }
    prev_scalar_inits_for_loop_seq <- .mojor_state$current_scalar_inits
    .mojor_state$current_scalar_inits <- scalar_inits
    on.exit(
        {
            .mojor_state$current_scalar_inits <- prev_scalar_inits_for_loop_seq
        }, add = TRUE
    )
    loop_seq_infos <- .mojor_collect_loop_seq_infos(blocks, loop_arg_specs, loop_args, scalar_inits)
    length_arrays_expr <- .mojor_collect_length_arrays_blocks(blocks)
    recycle_arrays_expr <- .mojor_collect_recycle_arrays_blocks(blocks, arg_specs)
    slice_rhs_arrays <- .mojor_collect_slice_rhs_arrays_blocks(blocks, arg_specs, args)
    if (length(slice_rhs_arrays) > 0) {
        recycle_arrays_expr <- unique(c(recycle_arrays_expr, slice_rhs_arrays))
    }
    matrix_arrays_expr <- .mojor_collect_matrix_arrays_blocks(blocks)
    nd_arrays_expr <- .mojor_collect_nd_arrays_blocks(blocks)
    tensor_ranks <- .mojor_collect_tensor_ranks_blocks(blocks)
    array_args <- args[vapply(arg_specs[args], .mojor_is_array, logical(1))]
    array_index_map <- .mojor_collect_array_index_map(blocks, array_args)
    .call_parts <- function(node) {
        parts <- as.list(node)[-1]
        nms <- names(parts)
        if (is.null(nms))
            nms <- rep("", length(parts))
        list(parts = parts, nms = nms)
    }
    .call_arg <- function(parts, nms, names, pos = NULL) {
        for (nm in names) {
            if (nm %in% nms) {
                return(parts[[which(nms == nm)[1]]])
            }
        }
        if (!is.null(pos) &&
            length(parts) >=
                pos) {
            return(parts[[pos]])
        }
        NULL
    }
    .filter_array_args <- function(names_vec) {
        if (length(names_vec) ==
            0) {
            return(character(0))
        }
        names_vec[vapply(
            names_vec, function(nm) nm %in%
                args && .mojor_is_array(arg_specs[[nm]]),
            logical(1)
        )]
    }
    preview_secondary_arrays <- .mojor_collect_from_blocks(
        blocks, function(node) {
            op <- as.character(node[[1]])
            if (length(op) !=
                1)
                op <- op[[1]]
            out <- character(0)
            if (op %in% c("match", "%in%") &&
                length(node) >=
                  3 && is.name(node[[3]])) {
                out <- c(out, as.character(node[[3]]))
            } else if (op == "quantile" && length(node) >=
                2) {
                info <- .call_parts(node)
                probs_expr <- .call_arg(
                  info$parts, info$nms, c("probs"),
                  pos = 2
              )
                if (is.name(probs_expr)) {
                  out <- c(out, as.character(probs_expr))
                }
            }
            .filter_array_args(out)
        }
    )
    sampling_source_arrays <- .mojor_collect_from_blocks(
        blocks, function(node) {
            op <- as.character(node[[1]])
            if (length(op) !=
                1)
                op <- op[[1]]
            if (!identical(op, "sample")) {
                return(character(0))
            }
            info <- .call_parts(node)
            x_expr <- .call_arg(
                info$parts, info$nms, c("x", "X"),
                pos = 1
            )
            if (!is.name(x_expr)) {
                return(character(0))
            }
            .filter_array_args(as.character(x_expr))
        }
    )

    seq_info <- NULL
    range_mismatch <- NULL
    local_vector_lengths <- .mojor_state$current_local_vector_lengths
    resolve_local_len_expr_name <- NULL
    resolve_local_len_expr_ast <- function(expr, seen = character(0)) {
        if (is.null(expr)) {
            return(NULL)
        }
        if (is.name(expr)) {
            nm <- as.character(expr)
            if (!(nm %in% args) &&
                !is.null(scalar_inits[[nm]]) &&
                is.list(scalar_inits[[nm]]$value)) {
                sval <- scalar_inits[[nm]]$value
                if (identical(sval$kind, "len") && !is.null(sval$name)) {
                    base_expr <- call("length", as.name(sval$name))
                    if (!is.null(sval$delta) && as.integer(sval$delta) != 0L) {
                        delta <- as.integer(sval$delta)
                        base_expr <- if (delta > 0L) {
                            call("+", base_expr, delta)
                        } else {
                            call("-", base_expr, abs(delta))
                        }
                    }
                    return(resolve_local_len_expr_ast(base_expr, c(seen, nm)))
                }
                if (identical(sval$kind, "expr") && !is.null(sval$expr)) {
                    return(resolve_local_len_expr_ast(sval$expr, c(seen, nm)))
                }
            }
            return(expr)
        }
        if (!is.call(expr)) {
            return(expr)
        }
        op <- as.character(expr[[1]])
        if (op == "length" && length(expr) == 2 && is.name(expr[[2]])) {
            target_name <- as.character(expr[[2]])
            if (!(target_name %in% args)) {
                replacement <- resolve_local_len_expr_name(target_name, seen)
                if (!is.null(replacement)) {
                    return(replacement)
                }
            }
            return(expr)
        }
        parts <- as.list(expr)
        if (length(parts) >= 2) {
            for (j in 2:length(parts)) {
                parts[[j]] <- resolve_local_len_expr_ast(parts[[j]], seen)
            }
        }
        as.call(parts)
    }
    resolve_local_len_expr_name <- function(name, seen = character(0)) {
        if (is.null(name) || !nzchar(name) || name %in% seen) {
            return(NULL)
        }
        if (!is.null(local_vector_lengths[[name]])) {
            runtime_len <- local_vector_lengths[[name]]$length_runtime
            if (is.null(runtime_len)) {
                runtime_len <- local_vector_lengths[[name]]$length
            }
            if (!is.null(runtime_len)) {
                return(resolve_local_len_expr_ast(runtime_len, c(seen, name)))
            }
        }
        if (!is.null(len_scalar_map[[name]]) &&
            !is.null(len_scalar_map[[name]]$name)) {
            len_info <- len_scalar_map[[name]]
            expr <- call("length", as.name(len_info$name))
            delta <- if (is.null(len_info$delta)) 0L else as.integer(len_info$delta)
            if (!is.na(delta) && delta != 0L) {
                expr <- if (delta > 0L) call("+", expr, delta) else call("-", expr, abs(delta))
            }
            return(resolve_local_len_expr_ast(expr, c(seen, name)))
        }
        NULL
    }
    materialize_local_array_seq <- function(info) {
        if (is.null(info) ||
            is.null(info$kind) ||
            !identical(info$kind, "array") ||
            is.null(info$name) ||
            info$name %in% args) {
            return(info)
        }
        len_expr <- resolve_local_len_expr_name(info$name)
        if (is.null(len_expr)) {
            return(info)
        }
        delta <- if (is.null(info$end_delta)) 0L else as.integer(info$end_delta)
        if (!is.na(delta) && delta != 0L) {
            len_expr <- if (delta > 0L) call("+", len_expr, delta) else call("-", len_expr, abs(delta))
        }
        expr_c <- tryCatch(
            .mojor_len_expr_to_c(len_expr, args, arg_specs, allow_scalar_cast = TRUE),
            error = function(e) NULL
        )
        if (is.null(expr_c)) {
            return(info)
        }
        info$kind <- "expr"
        info$name <- NULL
        info$end_expr_ast <- len_expr
        info$expr_key <- paste(deparse(len_expr, width.cutoff = 500), collapse = "")
        info$expr_c <- expr_c
        info$end_delta <- 0L
        info
    }
    for (i in seq_along(loop_infos)) {
        li <- loop_infos[[i]]
        if (identical(li$kind, "while")) {
            if (is.null(seq_info)) {
                seq_info <- .mojor_default_n_source(
                    arg_specs,
                    args,
                    allow_scalar_i32 = TRUE,
                    ref_exprs = li$cond,
                    context_kind = "while",
                    scalar_inits = scalar_inits
                )
            }
            next
        }
        if (identical(li$kind, "repeat")) {
            if (is.null(seq_info)) {
                seq_info <- .mojor_default_n_source(
                    arg_specs,
                    args,
                    allow_scalar_i32 = TRUE,
                    ref_exprs = li$blocks,
                    context_kind = "repeat",
                    scalar_inits = scalar_inits
                )
            }
            next
        }
        info_i <- .mojor_parse_loop_seq(li$seq, loop_arg_specs, loop_args, scalar_inits)
        if (info_i$kind == "scalar" && info_i$name %in% names(len_scalar_map)) {
            len_info <- len_scalar_map[[info_i$name]]
            info_i$kind <- "array"
            info_i$name <- len_info$name
            info_i$end_delta <- if (is.null(len_info$delta))
                0L else len_info$delta
        }
        info_i <- materialize_local_array_seq(info_i)
        if (!is.null(info_i$iter_expr_ast) ||
            isTRUE(info_i$iter_expr_from_loopvar) ||
            !is.null(info_i$iter_expr_float)) {
            iter_value <- li$var
            used <- unique(
                c(
                  args, .mojor_loop_vars_from_blocks(li$blocks),
                  iter_value
              )
            )
            idx_var <- .mojor_unique_loop_var(
                paste0(iter_value, "_i"),
                used
            )
            loop_infos[[i]]$iter_value <- iter_value
            if (!is.null(info_i$iter_expr_float)) {
                idx_sym <- as.name(idx_var)
                idx_zero <- call("-", idx_sym, 1L)
                from_ast <- info_i$iter_expr_float$from
                by_ast <- info_i$iter_expr_float$by
                iter_ast <- call(
                  "+", call("as.double", from_ast),
                  call(
                    "*", call("as.double", idx_zero),
                    call("as.double", by_ast)
                )
              )
                loop_infos[[i]]$iter_expr_ast <- iter_ast
                loop_infos[[i]]$iter_expr_spec <- "f64"
            } else {
                loop_infos[[i]]$iter_expr_ast <- if (isTRUE(info_i$iter_expr_from_loopvar))
                  as.name(idx_var) else info_i$iter_expr_ast
                loop_infos[[i]]$iter_expr_spec <- info_i$iter_expr_spec
            }
            loop_infos[[i]]$end_expr_ast <- info_i$end_expr_ast
            loop_infos[[i]]$iter_expr_rev <- info_i$iter_expr_rev
            loop_infos[[i]]$iter_expr_transform <- info_i$iter_expr_transform  # Preserve division/multiplication transforms
            loop_infos[[i]]$var <- idx_var
        } else if (identical(info_i$kind, "iter")) {
            iter_value <- li$var
            iter_source <- info_i$name
            used <- unique(
                c(
                  args, .mojor_loop_vars_from_blocks(li$blocks),
                  iter_value
              )
            )
            idx_var <- .mojor_unique_loop_var(
                paste0(iter_value, "_i"),
                used
            )
            loop_infos[[i]]$iter_value <- iter_value
            loop_infos[[i]]$iter_source <- iter_source
            loop_infos[[i]]$var <- idx_var
        }
        loop_infos[[i]]$seq_info <- info_i
        if (is.null(seq_info)) {
            seq_info <- info_i
        } else {
            kind_a <- if (info_i$kind %in% c("array", "iter"))
                "array" else info_i$kind
            kind_b <- if (seq_info$kind %in% c("array", "iter"))
                "array" else seq_info$kind
            if (kind_a != kind_b) {
                range_mismatch <- list(expr = li$seq, kind_a = kind_a, kind_b = kind_b)
            } else if (!is.null(info_i$expr_key) ||
                !is.null(seq_info$expr_key)) {
                key_a <- info_i$expr_key
                key_b <- seq_info$expr_key
                if (is.null(key_a) ||
                  is.null(key_b) ||
                  !identical(key_a, key_b)) {
                  range_mismatch <- list(expr = li$seq, kind_a = kind_a, kind_b = kind_b)
                }
            } else if (info_i$kind == "expr") {
                key_a <- info_i$expr_key
                key_b <- seq_info$expr_key
                if (is.null(key_a) ||
                  is.null(key_b) ||
                  !identical(key_a, key_b)) {
                  range_mismatch <- list(expr = li$seq, kind_a = "expr", kind_b = "expr")
                }
            } else if (info_i$kind == "scalar" && info_i$name != seq_info$name) {
                range_mismatch <- list(expr = li$seq, kind_a = "scalar", kind_b = "scalar")
            } else if (info_i$kind %in% c("array", "iter") &&
                info_i$name != seq_info$name) {
                range_mismatch <- list(expr = li$seq, kind_a = "array", kind_b = "array")
                seq_info$extra <- unique(c(seq_info$extra, info_i$name))
            }
        }
    }
    if (is.null(seq_info)) {
        if (length(loop_infos) ==
            0 && length(array_args) == 0) {
            seq_info <- list(kind = "none", name = NULL, extra = NULL, end_delta = 0L)
        } else if (isTRUE(no_loop_scalar_any_all)) {
            seq_info <- list(kind = "none", name = NULL, extra = NULL, end_delta = 0L)
        } else {
            seq_info <- .mojor_default_n_source(arg_specs, args)
        }
    }
    if (!is.null(seq_extra))
        seq_info$extra <- unique(c(seq_info$extra, seq_extra))
    n_source_name <- if (seq_info$kind %in% c("array", "iter"))
        seq_info$name else NULL
    len_arrays <- character(0)
    append_len_arrays <- function(values) {
        if (length(values) ==
            0) {
            return(invisible(NULL))
        }
        len_arrays <<- unique(c(len_arrays, setdiff(values, n_source_name)))
        invisible(NULL)
    }
    seq_len_arrays <- character(0)
    if (length(loop_seq_infos) >
        0) {
        append_len_arrays(
            vapply(
                loop_seq_infos, function(info) {
                  if (!is.null(info) &&
                    info$kind %in% c("array", "iter"))
                    info$name else NA_character_
                }, character(1)
            )
        )
        len_arrays <- setdiff(len_arrays, NA_character_)
        seq_len_arrays <- unique(
            unlist(
                lapply(
                  loop_seq_infos, function(info) {
                    if (!is.null(info) &&
                      !is.null(info$len_arrays))
                      info$len_arrays else character(0)
                  }
              )
            )
        )
        append_len_arrays(seq_len_arrays)
    }
    append_len_arrays(length_arrays_expr)
    append_len_arrays(constructor_len_arrays)
    append_len_arrays(slice_rhs_arrays)
    append_len_arrays(preview_secondary_arrays)
    append_len_arrays(sampling_source_arrays)
    i32_array_args <- args[vapply(args, function(nm) identical(arg_specs[[nm]], "i32[]"), logical(1))]
    append_len_arrays(i32_array_args)
    if (length(array_index_map) >
        0)
        append_len_arrays(names(array_index_map))
    collect_selector_len_arrays <- function(blocks, arg_specs) {
        if (length(blocks) == 0 || length(arg_specs) == 0) {
            return(character(0))
        }
        selector_specs <- c("i32[]", "lgl[]", "bool[]")
        selector_args <- names(arg_specs)[vapply(names(arg_specs), function(nm) {
            spec <- arg_specs[[nm]]
            is.character(spec) && length(spec) == 1 && spec %in% selector_specs
        }, logical(1))]
        if (length(selector_args) == 0) {
            return(character(0))
        }
        refs <- .mojor_collect_from_blocks(
            blocks,
            function(expr) {
                op <- as.character(expr[[1]])
                if (length(op) != 1 || !(op %in% c("[", "[[")) || length(expr) < 3L) {
                    return(character(0))
                }
                out <- character(0)
                expr_names <- names(expr)
                for (pos in seq.int(3L, length(expr))) {
                    idx_name <- if (!is.null(expr_names) && length(expr_names) >= pos) expr_names[[pos]] else ""
                    if (nzchar(idx_name)) {
                        idx_name <- tolower(idx_name)
                        if (idx_name %in% c("drop", "exact")) {
                            next
                        }
                    }
                    idx_expr <- tryCatch(expr[[pos]], error = function(e) NULL)
                    is_missing_idx <- tryCatch(
                        is.null(idx_expr) ||
                            identical(idx_expr, quote(expr = )) ||
                            (is.symbol(idx_expr) && identical(as.character(idx_expr), "")),
                        error = function(e) TRUE
                    )
                    if (isTRUE(is_missing_idx)) {
                        next
                    }
                    syms <- tryCatch(all.names(idx_expr, functions = FALSE, unique = TRUE), error = function(e) character(0))
                    if (length(syms) > 0) {
                        out <- c(out, intersect(syms, selector_args))
                    }
                }
                unique(out)
            }
        )
        unique(refs)
    }
    selector_len_arrays <- collect_selector_len_arrays(blocks, arg_specs)
    append_len_arrays(selector_len_arrays)
    len_arrays <- sort(unique(len_arrays))
    if (length(len_arrays) >
        0) {
        len_arrays <- len_arrays[len_arrays %in% names(arg_specs)]
        if (length(len_arrays) >
            0) {
            len_arrays <- len_arrays[vapply(
                len_arrays, function(nm) .mojor_is_array(arg_specs[[nm]]),
                logical(1)
            )]
        }
    }
    if (!is.null(out_len_source) &&
        identical(out_len_source$kind, "mask_true")) {
        mask_name <- out_len_source$name
        if (!is.null(mask_name) &&
            mask_name %in% names(arg_specs) &&
            .mojor_is_array(arg_specs[[mask_name]])) {
            len_arrays <- unique(c(len_arrays, mask_name))
        }
    }
    if (!is.null(out_len_source) &&
        identical(out_len_source$kind, "exclusion")) {
        excl_base <- out_len_source$base
        if (!is.null(excl_base) &&
            excl_base %in% names(arg_specs) &&
            .mojor_is_array(arg_specs[[excl_base]])) {
            len_arrays <- unique(c(len_arrays, excl_base))
        }
    }
    primary_loop_vars <- vapply(
        loop_infos, function(li) {
            if (!is.null(li$kind) &&
                identical(li$kind, "for") &&
                !is.null(li$var))
                li$var else NA_character_
        }, character(1)
    )
    primary_loop_vars <- unique(primary_loop_vars[!is.na(primary_loop_vars)])
    local_matrix_data_args <- character(0)
    local_matrix_data_only_args <- character(0)
    if (length(.mojor_state$current_local_matrix_dims) >
        0) {
        local_matrix_data_args <- unique(
            vapply(
                .mojor_state$current_local_matrix_dims, function(info) {
                  if (!is.null(info$data) &&
                    is.character(info$data) &&
                    length(info$data) ==
                      1) {
                    return(info$data)
                  }
                  NA_character_
                }, character(1)
            )
        )
        local_matrix_data_args <- local_matrix_data_args[!is.na(local_matrix_data_args)]
        local_matrix_data_args <- intersect(local_matrix_data_args, args)
        if (length(local_matrix_data_args) >
            0) {
            local_matrix_data_only_args <- local_matrix_data_args[vapply(
                local_matrix_data_args, function(a) {
                  is.null(array_index_map[[a]])
                }, logical(1)
            )]
        }
    }
    .collect_na_rm_matrix_data_args <- function(expr) {
        if (missing(expr) ||
            is.null(expr)) {
            return(character(0))
        }
        found <- character(0)
        if (!is.call(expr)) {
            return(found)
        }
        op <- as.character(expr[[1]])
        if (length(op) !=
            1)
            op <- op[[1]]
        if (op %in% c("sum", "mean", "min", "max")) {
            parts <- as.list(expr)[-1]
            nms <- names(parts)
            na_rm_true <- FALSE
            if (!is.null(nms) &&
                "na.rm" %in% nms) {
                na_rm_val <- parts[[which(nms == "na.rm")[1]]]
                na_rm_true <- is.logical(na_rm_val) &&
                  length(na_rm_val) ==
                    1 && !is.na(na_rm_val) &&
                  isTRUE(na_rm_val)
            }
            if (isTRUE(na_rm_true) &&
                length(parts) >
                  0) {
                target_arg <- parts[[1]]
                if (!is.null(nms) &&
                  "x" %in% nms) {
                  target_arg <- parts[[which(nms == "x")[1]]]
                }
                if (is.call(target_arg) &&
                  as.character(target_arg[[1]]) ==
                    "[" && length(target_arg) >=
                  2 && is.name(target_arg[[2]])) {
                  mat_name <- as.character(target_arg[[2]])
                  mat_info <- .mojor_state$current_local_matrix_dims[[mat_name]]
                  if (!is.null(mat_info) &&
                    !is.null(mat_info$data) &&
                    is.character(mat_info$data) &&
                    length(mat_info$data) ==
                      1) {
                    found <- c(found, mat_info$data)
                  }
                }
            }
        }
        .mojor_walk_call_parts_safe(
            as.list(expr)[-1],
            function(p) {
                found <<- c(found, .collect_na_rm_matrix_data_args(p))
            }
        )
        unique(found)
    }
    na_skip_args <- character(0)
    if (length(.mojor_state$current_local_matrix_dims) >
        0 && length(blocks) >
        0) {
        na_skip_args <- unique(
            unlist(
                lapply(blocks, .collect_na_rm_matrix_data_args),
                use.names = FALSE
            )
        )
        na_skip_args <- intersect(na_skip_args, args)
        if (length(na_skip_args) >
            0) {
            na_skip_args <- na_skip_args[vapply(
                na_skip_args, function(nm) {
                  !is.null(arg_specs[[nm]]) &&
                    .mojor_is_array(arg_specs[[nm]])
                }, logical(1)
            )]
        }
    }
    len_check_arrays <- array_args
    if (length(array_args) >
        0 && length(primary_loop_vars) >
        0) {
        len_check_arrays <- character(0)
        for (a in array_args) {
            info <- array_index_map[[a]]
                keep <- TRUE
                if (a %in% local_matrix_data_only_args) {
                  keep <- FALSE
                } else if (!is.null(info)) {
                if (isTRUE(info$unknown)) {
                  # Unknown/non-loop index access (e.g. x[idx], x[const]) is not
                  # an elementwise same-length contract with the primary loop.
                  keep <- FALSE
                } else if (length(info$vars) ==
                  0) {
                  keep <- TRUE
                } else if (!is.null(info$simple_vars) &&
                  any(info$simple_vars %in% primary_loop_vars)) {
                  keep <- TRUE
                } else if (any(info$vars %in% primary_loop_vars) &&
                  isTRUE(info$complex)) {
                  keep <- FALSE
                } else {
                  keep <- FALSE
                }
                if (keep && a %in% seq_len_arrays && length(info$vars) ==
                  0) {
                  keep <- FALSE
                }
            } else {
                if (a %in% seq_len_arrays)
                  keep <- FALSE
            }
            if (keep)
                len_check_arrays <- c(len_check_arrays, a)
        }
        len_check_arrays <- unique(len_check_arrays)
    }
    if (length(local_matrix_data_only_args) >
        0) {
        len_check_arrays <- setdiff(len_check_arrays, local_matrix_data_only_args)
    }
    if (length(sampling_source_arrays) >
        0 && length(len_check_arrays) >
        0) {
        sampling_only_arrays <- sampling_source_arrays[vapply(
            sampling_source_arrays, function(a) {
                info <- array_index_map[[a]]
                if (is.null(info)) {
                  return(TRUE)
                }
                if (!is.list(info)) {
                  return(FALSE)
                }
                if (isTRUE(info$unknown)) {
                  return(FALSE)
                }
                length(info$vars) ==
                  0
            }, logical(1)
        )]
        if (length(sampling_only_arrays) >
            0) {
            len_check_arrays <- setdiff(len_check_arrays, sampling_only_arrays)
        }
    }
    if (length(selector_len_arrays) > 0 && length(len_check_arrays) > 0) {
        selector_len_check_exempt <- selector_len_arrays[vapply(
            selector_len_arrays,
            function(nm) {
                spec <- arg_specs[[nm]]
                is.character(spec) && length(spec) == 1 && spec %in% c("i32[]", "lgl[]", "bool[]")
            },
            logical(1)
        )]
        if (length(selector_len_check_exempt) > 0) {
            len_check_arrays <- setdiff(len_check_arrays, selector_len_check_exempt)
        }
    }
    if (!is.null(seq_info) && identical(seq_info$kind, "scalar") && length(len_check_arrays) > 1) {
        scalar_loop_selector_specs <- c("i32[]", "lgl[]", "bool[]")
        scalar_loop_selector_arrays <- len_check_arrays[vapply(
            len_check_arrays,
            function(nm) {
                spec <- arg_specs[[nm]]
                is.character(spec) && length(spec) == 1 && spec %in% scalar_loop_selector_specs
            },
            logical(1)
        )]
        if (length(scalar_loop_selector_arrays) > 0) {
            len_check_arrays <- setdiff(len_check_arrays, scalar_loop_selector_arrays)
        }
    }
    if (!is.null(seq_info) && identical(seq_info$kind, "scalar")) {
        # For scalar-driven loops (e.g. seq_len(n)), element access paths already
        # emit bounds-checked reads. Enforcing whole-array length equality against
        # n is overly strict and causes false "length mismatch" failures in
        # indexed patterns like x[idx[i]] and prefix scans.
        len_check_arrays <- character(0)
    }
    if (length(tensor_ranks) >
        0) {
        tensor_names <- intersect(
            names(tensor_ranks),
            unique(c(args, out_name))
        )
        tensor_ranks <- tensor_ranks[tensor_names]
    }
    matrix_dim_arrays <- character(0)
    if (length(tensor_ranks) >
        0) {
        matrix_dim_arrays <- names(tensor_ranks)[vapply(
            tensor_ranks, function(r) r ==
                2, logical(1)
        )]
    }
    nd_arrays <- sort(unique(intersect(nd_arrays_expr, args)))
    loop_dim_arrays <- character(0)
    resolve_dim_array_name <- function(name) {
        .mojor_resolve_array_source_name(name, args, arg_specs, alias_map = array_aliases)
    }
    loop_dim_idx_supported <- function(expr) {
        !is.null(tryCatch(.mojor_len_expr_to_c(expr, loop_args, loop_arg_specs), error = function(e) NULL))
    }
    collect_loop_dim_arrays <- function(expr) {
        if (is.null(expr) || !is.call(expr)) {
            return(invisible(NULL))
        }
        op <- as.character(expr[[1]])
        if (length(op) == 1 &&
          identical(op, "dim") &&
          length(expr) == 2 &&
          is.name(expr[[2]])) {
            arr_name <- resolve_dim_array_name(as.character(expr[[2]]))
            if (!is.null(arr_name)) {
              loop_dim_arrays <<- c(loop_dim_arrays, arr_name)
            }
        }
        if (length(op) == 1 &&
          op %in% c("[", "[[") &&
          length(expr) >= 3) {
            base <- expr[[2]]
            idx <- expr[[3]]
            if (is.call(base) &&
              as.character(base[[1]]) == "dim" &&
              length(base) == 2 &&
              is.name(base[[2]]) &&
              loop_dim_idx_supported(idx)) {
                    arr_name <- resolve_dim_array_name(as.character(base[[2]]))
                    if (!is.null(arr_name)) {
                      loop_dim_arrays <<- c(loop_dim_arrays, arr_name)
                    }
            }
        }
        for (k in 2:length(expr)) {
            collect_loop_dim_arrays(expr[[k]])
        }
        invisible(NULL)
    }
    if (length(loop_infos) > 0) {
        for (li in loop_infos) {
            if (!is.list(li) || identical(li$kind, "while")) {
                next
            }
            info_i <- li$seq_info
            if (is.null(info_i) || !is.list(info_i)) {
                next
            }
            collect_loop_dim_arrays(info_i$start_expr_ast)
            collect_loop_dim_arrays(info_i$end_expr_ast)
            collect_loop_dim_arrays(info_i$by_expr_ast)
        }
    }
    loop_dim_arrays <- sort(unique(intersect(loop_dim_arrays, args)))
    loop_nd_dim_arrays <- character(0)
    if (length(loop_dim_arrays) > 0) {
        loop_nd_dim_arrays <- loop_dim_arrays[vapply(loop_dim_arrays, function(nm) {
            spec <- arg_specs[[nm]]
            if (is.null(spec) || !.mojor_is_array(spec)) {
                return(FALSE)
            }
            !isTRUE(.mojor_is_matrix(spec)) && .mojor_type_ndim(spec) >= 3L
        }, logical(1))]
    }
    if (length(nd_arrays) >
        0) {
        matrix_arrays_expr <- setdiff(matrix_arrays_expr, nd_arrays)
    }
    nrow_arrays <- character(0)
    ncol_arrays <- character(0)
    if (length(matrix_arrays_expr) >
        0) {
        nrow_arrays <- unique(setdiff(matrix_arrays_expr, c(NA_character_, n_source_name)))
        ncol_arrays <- nrow_arrays  # Matrices have both nrow and ncol
    }
    loop_matrix_arrays <- character(0)
    if (length(loop_dim_arrays) > 0) {
        loop_matrix_arrays <- loop_dim_arrays[vapply(loop_dim_arrays, function(nm) {
            spec <- arg_specs[[nm]]
            if (is.null(spec) || !.mojor_is_array(spec)) {
                return(FALSE)
            }
            isTRUE(.mojor_is_matrix(spec)) || identical(.mojor_type_ndim(spec), 2L)
        }, logical(1))]
        if (length(loop_matrix_arrays) > 0) {
            nrow_arrays <- c(nrow_arrays, loop_matrix_arrays)
            ncol_arrays <- c(ncol_arrays, loop_matrix_arrays)
        }
    }
 # PR-B3: Include arrays used with nrow()/ncol()/dim() in scalar_inits.
    if (length(scalar_inits) >
        0) {
        collect_dim_refs_from_expr <- function(expr, refs = character(0)) {
            if (is.null(expr) || !is.call(expr)) {
                return(refs)
            }
            op <- as.character(expr[[1]])
            if (length(op) == 1 &&
                identical(op, "dim") &&
                length(expr) == 2 &&
                is.name(expr[[2]])) {
                arr_name <- resolve_dim_array_name(as.character(expr[[2]]))
                if (!is.null(arr_name)) {
                    refs <- c(refs, arr_name)
                }
            }
            if (length(expr) >= 2) {
                for (k in 2:length(expr)) {
                    refs <- collect_dim_refs_from_expr(expr[[k]], refs)
                }
            }
            refs
        }
        for (nm in names(scalar_inits)) {
            val <- scalar_inits[[nm]]$value
            if (is.list(val) &&
                identical(val$kind, "dim")) {
                arr_name <- resolve_dim_array_name(val$name)
                if (is.null(arr_name)) {
                    next
                }
                dim_idx <- val$dim
                if (dim_idx == 1) {
                  nrow_arrays <- c(nrow_arrays, arr_name)
                } else if (dim_idx == 2) {
                  ncol_arrays <- c(ncol_arrays, arr_name)
                } else {
                  loop_nd_dim_arrays <- c(loop_nd_dim_arrays, arr_name)
                }
            } else if (is.list(val) &&
                identical(val$kind, "expr") &&
                !is.null(val$expr)) {
                expr_dim_refs <- unique(collect_dim_refs_from_expr(val$expr))
                if (length(expr_dim_refs) == 0L) {
                    next
                }
                for (arr_name in expr_dim_refs) {
                    spec <- arg_specs[[arr_name]]
                    if (is.null(spec) || !.mojor_is_array(spec)) {
                        next
                    }
                    if (isTRUE(.mojor_is_matrix(spec)) || identical(.mojor_type_ndim(spec), 2L)) {
                        nrow_arrays <- c(nrow_arrays, arr_name)
                        ncol_arrays <- c(ncol_arrays, arr_name)
                    } else if (.mojor_type_ndim(spec) >= 3L) {
                        loop_nd_dim_arrays <- c(loop_nd_dim_arrays, arr_name)
                    }
                }
            }
        }
    }
    gpu_matmul_matrix_args <- intersect(
        .mojor_collect_from_blocks(
            blocks, function(expr) {
                op <- as.character(expr[[1]])
                if (length(op) !=
                  1 || !identical(op, "gpu_matmul") ||
                  length(expr) <
                    3) {
                  return(character(0))
                }
                out <- character(0)
                if (is.name(expr[[2]]))
                  out <- c(out, as.character(expr[[2]]))
                if (is.name(expr[[3]]))
                  out <- c(out, as.character(expr[[3]]))
                out
            }
        ),
        args
    )
    if (length(gpu_matmul_matrix_args) >
        0) {
        nrow_arrays <- c(nrow_arrays, gpu_matmul_matrix_args)
        ncol_arrays <- c(ncol_arrays, gpu_matmul_matrix_args)
    }
    collect_matrix_dim_refs <- function(expr) {
        refs <- list(
            nrow = character(0),
            ncol = character(0)
        )
        walk <- function(e) {
            if (is.null(e) ||
                !(is.call(e) ||
                  is.name(e))) {
                return(invisible(NULL))
            }
            if (is.call(e)) {
                op <- as.character(e[[1]])
                if (length(op) ==
                  1 && op %in% c("nrow", "ncol") &&
                  length(e) ==
                    2 && is.name(e[[2]])) {
                  arr <- resolve_dim_array_name(as.character(e[[2]]))
                  if (is.null(arr)) {
                    return(invisible(NULL))
                  }
                  if (identical(op, "nrow")) {
                    refs$nrow <<- c(refs$nrow, arr)
                  } else {
                    refs$ncol <<- c(refs$ncol, arr)
                  }
                  return(invisible(NULL))
                }
                parts <- as.list(e)[-1]
                if (length(parts) >
                  0) {
                  .mojor_walk_call_parts_safe(parts, walk)
                }
            }
            invisible(NULL)
        }
        walk(expr)
        refs$nrow <- unique(refs$nrow)
        refs$ncol <- unique(refs$ncol)
        refs
    }
    if (isTRUE(out_matrix)) {
        out_nrow_refs <- collect_matrix_dim_refs(out_nrow_expr)
        out_ncol_refs <- collect_matrix_dim_refs(out_ncol_expr)
        nrow_arrays <- c(nrow_arrays, out_nrow_refs$nrow, out_ncol_refs$nrow)
        ncol_arrays <- c(ncol_arrays, out_nrow_refs$ncol, out_ncol_refs$ncol)
    }
    if (length(loop_infos) > 0) {
        loop_seq_refs <- lapply(loop_infos, function(li) collect_matrix_dim_refs(li$seq))
        if (length(loop_seq_refs) > 0) {
            nrow_arrays <- c(
                nrow_arrays,
                unlist(lapply(loop_seq_refs, function(refs) refs$nrow), use.names = FALSE)
            )
            ncol_arrays <- c(
                ncol_arrays,
                unlist(lapply(loop_seq_refs, function(refs) refs$ncol), use.names = FALSE)
            )
        }
    }
    nrow_arrays <- sort(unique(nrow_arrays))
    ncol_arrays <- sort(unique(ncol_arrays))
 # Only function array arguments require wrapper-passed matrix
 # dims. Local matrices allocate their dims inside the kernel
 # body.
    nrow_arrays <- intersect(nrow_arrays, args)
    ncol_arrays <- intersect(ncol_arrays, args)
 # When both nrow and ncol are needed for matrix args, ensure
 # length metadata is available so ncol can be derived as len/nrow
 # in kernels that don't pass ncol directly.
    ncol_len_backfill <- intersect(ncol_arrays, nrow_arrays)
    if (length(ncol_len_backfill) >
        0) {
        len_arrays <- unique(c(len_arrays, ncol_len_backfill))
    }
    if (!is.null(out_name)) {
        nrow_arrays <- setdiff(nrow_arrays, out_name)
        ncol_arrays <- setdiff(ncol_arrays, out_name)
    }
 # Step broadcast_nd: All array args need explicit length
 # parameters for bounds checks since any input can be smaller
 # than the output due to broadcasting
    if (isTRUE(broadcast_nd) &&
        length(array_args) >
            0) {
        len_arrays <- unique(c(len_arrays, array_args))
    }
    if (!is.null(out_name) &&
        out_name %in% args &&
        out_name %in% names(arg_specs) &&
        .mojor_is_array(arg_specs[[out_name]])) {
        # Ensure alias-input outputs always have a stable explicit length
        # parameter for wrapper/kernel ABI alignment.
        len_arrays <- unique(c(len_arrays, out_name))
    }
    .mojor_named_chr_map <- function(keys, mapper) {
        if (length(keys) ==
            0) {
            return(list())
        }
        as.list(
            setNames(
                vapply(keys, mapper, character(1)),
                keys
            )
        )
    }
    len_param_map <- .mojor_named_chr_map(len_arrays, .mojor_len_param_name)
    len_var_map <- .mojor_named_chr_map(len_arrays, .mojor_len_var_name)
    if (length(scalar_inits) >
        0) {
        for (nm in names(scalar_inits)) {
            val <- scalar_inits[[nm]]$value
            if (is.list(val) &&
                identical(val$kind, "arg")) {
                scalar_inits[[nm]]$value <- val$name
            } else if (is.list(val) &&
                identical(val$kind, "len")) {
                len_var <- .mojor_len_var_for(
                  list(kind = "array", name = val$name),
                  n_source_name, len_var_map
              )
                if (is.null(len_var)) {
                  .mojor_err(
                    "length() scalar not available for loop ranges", val$name,
                    "use seq_len(length(x)) or add length params"
                )
                }
                len_expr <- len_var
                if (!is.null(val$delta) &&
                  val$delta != 0L) {
                  sign <- if (val$delta < 0L)
                    "-" else "+"
                  len_expr <- paste0(
                    "(", len_expr, " ", sign, " ", abs(val$delta),
                    ")"
                )
                }
                scalar_inits[[nm]]$value <- paste0("Int32(", len_expr, ")")
            }
        }
    }
    nrow_param_map <- .mojor_named_chr_map(nrow_arrays, .mojor_nrow_param_name)
    nrow_var_map <- .mojor_named_chr_map(nrow_arrays, .mojor_nrow_var_name)
    ncol_param_map <- .mojor_named_chr_map(ncol_arrays, .mojor_ncol_param_name)
    ncol_var_map <- .mojor_named_chr_map(ncol_arrays, .mojor_ncol_var_name)
    dim_arrays <- if (isTRUE(broadcast_nd))
        array_args else unique(c(nd_arrays, matrix_dim_arrays, loop_nd_dim_arrays))
    if (!is.null(out_name)) {
        nrow_arrays <- setdiff(nrow_arrays, out_name)
        dim_arrays <- setdiff(dim_arrays, out_name)
        matrix_dim_arrays <- setdiff(matrix_dim_arrays, out_name)
    }
    if (length(tensor_ranks) >
        0) {
        for (nm in names(tensor_ranks)) {
            rank <- tensor_ranks[[nm]]
            if (!is.null(out_name) &&
                nm == out_name) {
                if (rank == 2 && !isTRUE(out_matrix)) {
                  stop("mojor_transpile: matrix indexing on output requires matrix output")
                }
                if (rank >= 3 && !isTRUE(out_array) &&
                  !isTRUE(broadcast_nd)) {
                  stop("mojor_transpile: array indexing on output requires array output")
                }
                next
            }
            if (rank >= 3 && !(nm %in% dim_arrays)) {
                stop(
                  "mojor_transpile: array indexing requires dim metadata for ",
                  nm
              )
            }
            if (rank == 2 && !(nm %in% dim_arrays) && !(nm %in% nrow_arrays)) {
                stop(
                  "mojor_transpile: matrix indexing requires dim metadata for ",
                  nm
              )
            }
        }
    }
    dim_param_map <- .mojor_named_chr_map(dim_arrays, .mojor_dim_param_name)
    ndim_param_map <- .mojor_named_chr_map(dim_arrays, .mojor_ndim_param_name)
    dim_var_map <- .mojor_named_chr_map(dim_arrays, .mojor_dim_var_name)
    ndim_var_map <- .mojor_named_chr_map(dim_arrays, .mojor_ndim_var_name)

 # Build dimension map for dim() lowering Consolidates
 # nrow_var_map, ncol_var_map, dim_var_map, ndim_var_map into a
 # single map for the IR lowering pass
    .build_dim_map <- function(nrow_map, ncol_map, dim_map, ndim_map) {
        result <- list()
 # Process matrices (have nrow/ncol)
        for (nm in names(nrow_map)) {
            result[[nm]] <- list(
                kind = "matrix", nrow = nrow_map[[nm]], ncol = if (nm %in%
                  names(ncol_map)) ncol_map[[nm]] else NULL
            )
        }
 # Process arrays (have dim/ndim)
        for (nm in names(dim_map)) {
            if (!is.null(result[[nm]]))
                next  # skip if already added as matrix
            result[[nm]] <- list(
                kind = "array", dim = dim_map[[nm]], ndim = if (nm %in%
                  names(ndim_map)) ndim_map[[nm]] else NULL
            )
        }
        result
    }

    dim_map <- .build_dim_map(nrow_var_map, ncol_var_map, dim_var_map, ndim_var_map)

 # PR-B3: Shape helpers - process nrow/ncol/dim scalar inits after
 # all maps are defined
    if (length(scalar_inits) >
        0) {
        for (nm in names(scalar_inits)) {
            val <- scalar_inits[[nm]]$value
            if (is.list(val) &&
                identical(val$kind, "dim")) {
                arr_name <- val$name
                dim_idx <- val$dim  # 1-based: 1=nrow, 2=ncol, etc.
 # Get the appropriate dim variable
                if (!is.null(nrow_var_map) &&
                  !is.null(nrow_var_map[[arr_name]]) &&
                  dim_idx == 1) {
                  scalar_inits[[nm]]$value <- paste0("Int32(", nrow_var_map[[arr_name]], ")")
                } else if (!is.null(ncol_var_map) &&
                  !is.null(ncol_var_map[[arr_name]]) &&
                  dim_idx == 2) {
                  scalar_inits[[nm]]$value <- paste0("Int32(", ncol_var_map[[arr_name]], ")")
                } else if (!is.null(dim_var_map) &&
                  !is.null(dim_var_map[[arr_name]])) {
                  scalar_inits[[nm]]$value <- paste0(
                    "Int32(", dim_var_map[[arr_name]], "[", dim_idx - 1L,
                    "])"
                )
                } else {
 # Fallback for output matrix
                  if (identical(arr_name, out_name)) {
                    if (dim_idx == 1 && !is.null(out_nrow_var)) {
                      scalar_inits[[nm]]$value <- paste0("Int32(", out_nrow_var, ")")
                    } else if (dim_idx == 2 && !is.null(out_ncol_var)) {
                      scalar_inits[[nm]]$value <- paste0("Int32(", out_ncol_var, ")")
                    }
                  }
                }
            }
        }
    }
    for (i in seq_along(loop_infos)) {
        li <- loop_infos[[i]]
        if (identical(li$kind, "while")) {
            next
        }
        info_i <- loop_infos[[i]]$seq_info
        info_i$len_var <- .mojor_len_var_for(info_i, n_source_name, len_var_map)
        if (identical(info_i$kind, "array") &&
            is.null(info_i$len_var)) {
            .mojor_err(
                "loop uses array length not available", li$seq, "use seq_len() with a scalar or add length params"
            )
        }
        if (!isTRUE(info_i$float_seq) &&
            !is.null(info_i$start_expr_ast)) {
            info_i$start_expr <- .mojor_scalar_range_expr_to_mojo(
                info_i$start_expr_ast, loop_arg_specs, loop_args, len_var_map,
                n_source_name, scalar_inits, nrow_var_map = nrow_var_map,
                ncol_var_map = ncol_var_map, dim_var_map = dim_var_map
            )
        }
        if (!isTRUE(info_i$float_seq) &&
            !is.null(info_i$end_expr_ast)) {
            info_i$end_expr <- .mojor_scalar_range_expr_to_mojo(
                info_i$end_expr_ast, loop_arg_specs, loop_args, len_var_map,
                n_source_name, scalar_inits, nrow_var_map = nrow_var_map,
                ncol_var_map = ncol_var_map, dim_var_map = dim_var_map
            )
        }
        if (!isTRUE(info_i$float_seq) &&
            !is.null(info_i$by_expr_ast)) {
            info_i$by_expr <- .mojor_scalar_range_expr_to_mojo(
                info_i$by_expr_ast, loop_arg_specs, loop_args, len_var_map,
                n_source_name, scalar_inits, nrow_var_map = nrow_var_map,
                ncol_var_map = ncol_var_map, dim_var_map = dim_var_map
            )
        }
        rng <- .mojor_range_expr(info_i)
        loop_infos[[i]]$seq_info <- info_i
        loop_infos[[i]]$range_expr <- rng$range
        loop_infos[[i]]$range_start <- rng$start_idx
        loop_infos[[i]]$range_start_expr <- rng$start_expr
        loop_infos[[i]]$range_end <- rng$end_expr
        loop_infos[[i]]$range_step <- rng$step_expr
    }

    if (is.null(return_name)) {
        last_block <- blocks[[length(blocks)]]
        if (is.name(last_block)) {
            return_name <- as.character(last_block)
        }
    }

    return_num_expr_ast <- NULL
    if (is.null(return_name) &&
        !is.null(return_expr)) {
        unwrap_return_num <- function(expr) {
            cur <- expr
            repeat {
                if (!is.call(cur)) {
                    break
                }
                op <- as.character(cur[[1]])
                if (op == "(" &&
                    length(cur) == 2) {
                    cur <- cur[[2]]
                    next
                }
                if (op %in% c("as.double", "as.numeric", "as.single") &&
                    length(cur) == 2) {
                    cur <- cur[[2]]
                    next
                }
                break
            }
            cur
        }
        num <- unwrap_return_num(return_expr[[2]])
        if (is.null(scalar_init) &&
            is.name(num)) {
            num_name <- as.character(num)
            num_init <- scalar_inits[[num_name]]
            if (!is.null(num_init)) {
                scalar_init <- num_name
                if (is.null(scalar_type)) {
                    scalar_type <- num_init$type
                }
                if (is.null(scalar_init_value)) {
                    scalar_init_value <- num_init$value
                }
            }
        }
        if (is.null(scalar_init)) {
            stop("mojor_transpile: return(acc / n) requires a scalar accumulator")
        }
        if (is.null(scalar_type) ||
            !(scalar_type %in% c("f64", "f32"))) {
            stop(
                "mojor_transpile: return(acc / n) only supported for f64/f32 accumulators"
            )
        }
        den <- return_expr[[3]]
        if (!(is.name(num) &&
            as.character(num) ==
                scalar_init)) {
            return_num_expr_ast <- num
        }
        return_seq_info <- if (length(loop_infos) >= 1) {
            loop_infos[[1]]$seq_info
        } else {
            NULL
        }
        den_mojo <- tryCatch(
            .mojor_scalar_range_expr_to_mojo(
                den, loop_arg_specs, loop_args, len_var_map,
                n_source_name, scalar_inits, nrow_var_map = nrow_var_map,
                ncol_var_map = ncol_var_map, dim_var_map = dim_var_map
            ),
            error = function(e) NULL
        )
        if (is.null(den_mojo)) {
            den_type_env <- arg_specs
            if (length(scalar_inits) > 0) {
                for (nm in names(scalar_inits)) {
                    ty <- scalar_inits[[nm]]$type
                    if (!is.null(ty) &&
                        nzchar(ty)) {
                        den_type_env[[nm]] <- ty
                    }
                }
            }
            if (!is.null(scalar_init) &&
                !is.null(scalar_type) &&
                nzchar(scalar_type)) {
                den_type_env[[scalar_init]] <- scalar_type
            }
            den_mojo_general <- tryCatch(
                .mojor_expr_to_mojo(
                    den, character(0), den_type_env, in_cond = FALSE
                ),
                error = function(e) NULL
            )
            if (!is.null(den_mojo_general)) {
                den_ir <- tryCatch(
                    .mojor_ir_expr_build(den),
                    error = function(e) NULL
                )
                den_type <- if (!is.null(den_ir)) {
                    tryCatch(
                        .mojor_ir_infer_type(den_ir, den_type_env),
                        error = function(e) NULL
                    )
                } else {
                    NULL
                }
                if (!is.null(den_type) &&
                    den_type %in% c(
                        "i32", "f32", "f64", "Int", "Int32", "Float32",
                        "Float64"
                    )) {
                    den_mojo <- den_mojo_general
                }
            }
        }
        if (!is.null(return_seq_info) &&
            return_seq_info$kind == "scalar" && is.name(den) &&
            as.character(den) ==
                return_seq_info$name) {
            return_div <- return_seq_info$name
        } else if (!is.null(return_seq_info) &&
            return_seq_info$kind == "array" && is.call(den) &&
            as.character(den[[1]]) ==
                "length" && is.name(den[[2]]) &&
            as.character(den[[2]]) ==
                return_seq_info$name) {
            return_div <- "n_i"
        } else if (!is.null(den_mojo)) {
            return_div <- den_mojo
        } else {
            stop(
                "mojor_transpile: return(acc / n) denominator must be a numeric scalar expression"
            )
        }
        return_name <- scalar_init
        if (identical(na_guard, "ignore")) {
            mean_ignore_pending <- TRUE
        }
    }

    out_kind <- NULL
    if (!is.null(return_name)) {
        if (!is.null(out_name) &&
            return_name == out_name) {
            out_kind <- "vector"
            out_type_norm <- .mojor_normalize_type_spec(out_type)
            if (!isTRUE(out_matrix) &&
                is.character(out_type_norm) &&
                length(out_type_norm) == 1L &&
                !is.na(out_type_norm) &&
                grepl("\\[,\\]$", out_type_norm)) {
                out_matrix <- TRUE
                out_array <- FALSE
                out_nrow_expr <- as.call(list(as.name("nrow"), as.name(out_name)))
                out_ncol_expr <- as.call(list(as.name("ncol"), as.name(out_name)))
                out_dim_exprs <- NULL
                out_dim_name <- NULL
            }
        } else if (!is.null(scalar_init) &&
            return_name == scalar_init) {
            out_kind <- "scalar"
        } else if (!is.null(scalar_inits[[return_name]])) {
            scalar_init <- return_name
            scalar_type <- scalar_inits[[return_name]]$type
            scalar_init_value <- scalar_inits[[return_name]]$value
            out_kind <- "scalar"
        } else if (return_name %in% modified_args) {
            # Allow returning a modified input argument (in-place update)
            out_name <- return_name
            out_type <- types[[return_name]]
            out_kind <- "vector"
        } else {
            stop(
                "mojor_transpile: return() must return the output vector or scalar accumulator"
            )
        }
    } else if (!is.null(out_name)) {
        out_kind <- "vector"
    } else if (!is.null(scalar_init) ||
        length(scalar_inits) >
            0) {
        if (is.null(scalar_init) &&
            length(scalar_inits) ==
                1) {
            scalar_init <- names(scalar_inits)[1]
            scalar_type <- scalar_inits[[scalar_init]]$type
            scalar_init_value <- scalar_inits[[scalar_init]]$value
        }
        out_kind <- "scalar"
    }

    if (is.null(out_kind)) {
        if (is.character(expression_only_error) &&
            length(expression_only_error) == 1L &&
            nzchar(expression_only_error)) {
            stop(expression_only_error)
        }
        stop("mojor_transpile: couldn't determine output kind")
    }
    if (identical(out_kind, "vector") &&
        !is.null(out_name) &&
        !isTRUE(out_matrix) &&
        !isTRUE(out_array)) {
        out_spec <- out_type
        if (is.null(out_spec) &&
            out_name %in% names(types)) {
            out_spec <- types[[out_name]]
        }
        out_spec_norm <- .mojor_normalize_type_spec(out_spec)
        if (is.character(out_spec_norm) &&
            length(out_spec_norm) == 1L &&
            !is.na(out_spec_norm) &&
            grepl("\\[,\\]$", out_spec_norm)) {
            out_matrix <- TRUE
            out_array <- FALSE
            out_nrow_expr <- as.call(list(as.name("nrow"), as.name(out_name)))
            out_ncol_expr <- as.call(list(as.name("ncol"), as.name(out_name)))
            out_dim_exprs <- NULL
            out_dim_name <- NULL
        }
    }
    has_array_arg <- any(vapply(
        args,
        function(a) !is.null(types[[a]]) && .mojor_is_array(types[[a]]),
        logical(1)
    ))
    has_vector_n_source <- isTRUE(has_array_arg) ||
        !is.null(out_len_source) ||
        isTRUE(out_matrix) ||
        isTRUE(out_array)
    if (length(loop_infos) == 0L &&
        identical(out_kind, "vector") &&
        !isTRUE(has_vector_n_source)) {
        stop("mojor_transpile: require at least one array argument or inferable output length to determine n")
    }
    if (length(loop_infos) == 0L &&
        identical(out_kind, "scalar") &&
        length(args) == 0L) {
        stop("mojor_transpile: while loops require at least one array argument to determine n")
    }
    if (out_kind == "scalar" && is.null(scalar_type))
        scalar_type <- "f64"
 # For broadcast_nd, output length comes from broadcasted dims,
 # not constructor length.
    if (isTRUE(broadcast_nd) &&
        out_kind == "vector" && !isTRUE(out_matrix) &&
        !isTRUE(out_array)) {
        out_len_source <- NULL
    }
    allow_multi_range <- out_kind == "vector" && (isTRUE(out_matrix) ||
        isTRUE(out_array))
    if (!is.null(range_mismatch) &&
        out_kind != "scalar" && !allow_multi_range) {
        .mojor_err(
            "multiple loops must share the same range for vector outputs",
            range_mismatch$expr, "use the same seq_* or restrict to scalar output"
        )
    }
    if (out_kind == "scalar" && length(len_check_arrays) >
        0) {
        len_check_arrays <- len_check_arrays[vapply(
            len_check_arrays, function(a) {
                info <- array_index_map[[a]]
                if (!is.null(info)) {
                  if (isTRUE(info$unknown)) {
                    return(TRUE)
                  }
                  if (length(info$vars) ==
                    0) {
                    return(FALSE)
                  }
                  return(TRUE)
                }
                FALSE
            }, logical(1)
        )]
    }

    local_types <- arg_specs
    set_local_type <- function(name, spec) {
        if (!is.null(spec) &&
            nzchar(spec)) {
            local_types[[name]] <<- spec
        }
    }
    merge_local_types_from_map <- function(info_map, spec_fn) {
        if (length(info_map) ==
            0) {
            return(invisible(NULL))
        }
        for (nm in names(info_map)) {
            set_local_type(nm, spec_fn(info_map[[nm]]))
        }
        invisible(NULL)
    }
    if (out_kind == "vector") {
        tagged_out_type <- if (isTRUE(out_matrix)) {
            .mojor_type_tag_ndim(out_type, 2L)
        } else if (isTRUE(out_array) &&
            !is.null(out_dim_exprs)) {
            .mojor_type_tag_ndim(out_type, length(out_dim_exprs))
        } else {
            out_type
        }
        local_types[[out_name]] <- tagged_out_type
    }
    if (out_kind == "scalar" && !is.null(scalar_init))
        local_types[[scalar_init]] <- scalar_type
    if (length(scalar_inits) >
        0) {
        for (nm in names(scalar_inits)) {
            set_local_type(nm, scalar_inits[[nm]]$type)
        }
    }
    merge_local_types_from_map(
        .mojor_state$current_local_vector_lengths, function(vec_info) {
            vec_info$type
        }
    )
    merge_local_types_from_map(
        .mojor_state$current_local_matrix_dims, function(mat_info) {
            if (is.null(mat_info$type) ||
                !nzchar(mat_info$type)) {
                return(NULL)
            }
            .mojor_type_tag_ndim(mat_info$type, 2L)
        }
    )
    merge_local_types_from_map(
        .mojor_state$current_local_array_dims, function(arr_info) {
            if (is.null(arr_info$type) ||
                !nzchar(arr_info$type)) {
                return(NULL)
            }
            rank <- if (!is.null(arr_info$dim))
                length(arr_info$dim) else NULL
            if (!is.null(rank) &&
                rank > 0) {
                return(.mojor_type_tag_ndim(arr_info$type, rank))
            }
            arr_info$type
        }
    )
    loop_var_names <- unique(
        unlist(
            lapply(
                loop_infos, function(li) {
                  vars <- character(0)
                  if (!is.null(li$var))
                    vars <- c(vars, li$var)
                  if (!is.null(li$loop_vars))
                    vars <- c(vars, li$loop_vars)
                  vars
                }
            )
        )
    )
    if (length(loop_var_names) >
        0) {
        for (nm in loop_var_names) {
            if (is.null(local_types[[nm]]))
                local_types[[nm]] <- "i32"
        }
    }
    if (length(loop_infos) >
        0) {
        for (li in loop_infos) {
            if (is.null(li$iter_value) ||
                !is.null(local_types[[li$iter_value]]))
                next
            if (!is.null(li$iter_expr_spec)) {
                local_types[[li$iter_value]] <- li$iter_expr_spec
                next
            }
            if (!is.null(li$iter_source)) {
                src_spec <- arg_specs[[li$iter_source]]
                if (!is.null(src_spec) &&
                  .mojor_is_array(src_spec)) {
                  local_types[[li$iter_value]] <- sub("\\[.*$", "", src_spec)
                }
            }
        }
    }
    build_iter_expr_entry <- function(loop_info, loop_var) {
        prev_ctor_mode <- .mojor_state$current_constructor_mode
        prev_warn_ctor <- .mojor_state$options$warn_constructor
        prev_len_checks <- .mojor_state$current_len_checks_c
        .mojor_state$current_constructor_mode <- TRUE
        .mojor_state$options$warn_constructor <- FALSE
        on.exit(
            {
                .mojor_state$current_constructor_mode <- prev_ctor_mode
                .mojor_state$options$warn_constructor <- prev_warn_ctor
                .mojor_state$current_len_checks_c <- prev_len_checks
            }, add = TRUE
        )
        idx_override <- NULL
        if (isTRUE(loop_info$iter_expr_rev)) {
            if (is.null(loop_info$end_expr_ast)) {
                stop(
                  "mojor_transpile: rev() requires a known length for constructor loop ranges"
              )
            }
            len_expr_mojo <- .mojor_scalar_range_expr_to_mojo(
                expr = loop_info$end_expr_ast, arg_specs = arg_specs, args = args,
                len_var_map = len_var_map, n_source_name = n_source_name,
                scalar_inits = scalar_inits, nrow_var_map = nrow_var_map,
                ncol_var_map = ncol_var_map, dim_var_map = dim_var_map
            )
            idx_override <- paste0("(", len_expr_mojo, " - ", loop_var, ")")
        }

 # Special handling for literal constructors like c(1L, 2L) in
 # loop ranges These can't be emitted as normal expressions,
 # so we emit them as inline value lookups
        iter_ast <- loop_info$iter_expr_ast
 # DEBUG
        if (isTRUE(getOption("mojor.debug.iter_expr"))) {
            cat(
                "DEBUG build_iter_expr_entry: iter_expr_ast =", deparse(iter_ast),
                "\n"
            )
        }

        if (is.call(iter_ast) &&
            as.character(iter_ast[[1]]) ==
                "c") {
 # Check if all parts are literals (not variables or
 # expressions)
            parts <- as.list(iter_ast)[-1]
            if (length(parts) >
                0) {
                all_literals <- all(
                  vapply(
                    parts, function(p) {
                      is.numeric(p) ||
                        is.integer(p) ||
                        is.logical(p)
                    }, logical(1)
                )
              )

                if (all_literals) {
 # Extract literal values
                  values_raw <- vapply(
                    parts, function(p) {
                      if (is.integer(p)) {
                        p
                      } else if (is.numeric(p)) {
                        p
                      } else if (is.logical(p)) {
                        as.integer(p)
                      } else {
                        NA_real_
                      }
                    }, numeric(1)
                )

 # Convert to strings
                  values <- vapply(
                    values_raw, function(p) {
                      if (is.integer(p) ||
                        (is.numeric(p) &&
                          (p%%1) == 0)) {
                        val_str <- as.character(as.integer(p))
                        if (is.numeric(p) &&
                          !is.integer(p)) {
                          paste0(val_str, ".0")
                        } else {
                          val_str
                        }
                      } else {
                        as.character(p)
                      }
                    }, character(1)
                )

 # Build inline if/else chain for value lookup
 # Convert loop_var to 0-based for indexing Loop
 # variables are 1-based (range(1, n+1)), so
 # subtract 1
                  pos_var <- if (!is.null(idx_override)) {
                    paste0("(", idx_override, ")")
                  } else {
                    paste0("(", loop_var, " - 1)")
                  }

 # Build deterministic inline lookup chain:
 # (value[0] if pos == 0 else (value[1] if pos == 1 else ...))
                  expr_parts <- character(length(values))
                  for (i in seq_along(values)) {
                    idx <- i - 1L  # 0-based
                    expr_parts[i] <- paste0(values[i], " if ", pos_var, " == ", idx, " else ")
                  }
 # Last value as default (handles pos >= len via modulo)
                  expr_mojo <- paste0(
                    "(", paste(expr_parts, collapse = ""),
                    values[length(values)],
                    ")"
                )

                  return(list(expr = expr_mojo, spec = loop_info$iter_expr_spec))
                }
            }
        }

        expr_mojo <- .mojor_expr_to_mojo(
            loop_info$iter_expr_ast, c(loop_var),
            local_types, in_cond = FALSE, idx_override = idx_override,
            idx_zero_based = TRUE
        )

 # Apply stored transformation if present (e.g., seq_len(n) /
 # 2)
        if (!is.null(loop_info$iter_expr_transform)) {
            transform <- loop_info$iter_expr_transform
            op <- transform$op
            scalar_mojo <- .mojor_expr_to_mojo(
                transform$scalar, character(0),
                local_types, in_cond = FALSE
            )

            if (transform$side == "rhs") {
 # expr op scalar: e.g., seq_len(n) / 2
                expr_mojo <- paste0("(", expr_mojo, " ", op, " ", scalar_mojo, ")")
            } else {
 # scalar op expr: e.g., 10 / seq_len(n)
                expr_mojo <- paste0("(", scalar_mojo, " ", op, " ", expr_mojo, ")")
            }

 # Update spec: division always produces float
            if (op == "/") {
                loop_info$iter_expr_spec <- "f64"
            }
        }

        list(expr = expr_mojo, spec = loop_info$iter_expr_spec)
    }
    scalar_names <- setdiff(
        names(scalar_inits),
        scalar_init
    )
    prev_scalar_names <- .mojor_state$current_scalar_names
    prev_declared_scalars <- .mojor_state$current_declared_scalars
    prev_local_types <- .mojor_state$current_local_types
    .mojor_state$current_scalar_names <- scalar_names
    .mojor_state$current_declared_scalars <- unique(
        c(
            names(scalar_inits),
            scalar_init
        )
    )
    .mojor_state$current_local_types <- list()
    .mojor_state$current_scalar_inits <- scalar_inits
    on.exit(
        {
            .mojor_state$current_scalar_names <- prev_scalar_names
            .mojor_state$current_declared_scalars <- prev_declared_scalars
            .mojor_state$current_local_types <- prev_local_types
            .mojor_state$current_scalar_inits <- list()
        }, add = TRUE
    )
    scalar_accum_present <- any(
        vapply(
            loop_infos, function(li) {
                .mojor_blocks_assign_scalar(li$blocks, names(scalar_inits))
            }, logical(1)
        )
    )

 # SIMD safety (conservative). Auto mode can still be a candidate
 # for scalar reductions.
    simd_safe <- FALSE
    simd_reason <- NULL
    simd_emitted <- FALSE
    simd_na_skipped <- FALSE
    simd_na_scan <- FALSE
    has_while <- any(
        vapply(
            loop_infos, function(li) li$kind %in%
                c("while", "repeat"),
            logical(1)
        )
    )
    if (!isTRUE(elementwise) &&
        isTRUE(gpu_jit_mode_user_set) &&
        gpu_jit_mode %in% c("auto", "unified_preview") &&
        length(loop_infos) ==
            1 && !has_while && identical(out_kind, "vector")) {
 # Auto-promote simple vector loops into unified GPU
 # elementwise lane when gpu_jit mode is explicitly requested.
        elementwise <- TRUE
        elementwise_target <- "gpu"
        gpu_jit_elementwise_auto <- TRUE
    }
    has_nested <- any(
        vapply(
            loop_infos, function(li) {
                any(
                  vapply(
                    li$blocks, function(s) is.call(s) &&
                      as.character(s[[1]]) ==
                        "for", logical(1)
                )
              )
            }, logical(1)
        )
    )
    non_unit_range <- any(
        vapply(
            loop_infos, function(li) {
                if (li$kind %in% c("while", "repeat")) {
                  return(FALSE)
                }
                info <- li$seq_info
                if (isTRUE(info$colon_dynamic)) {
                  return(TRUE)
                }
                if (!is.null(info$by_expr)) {
                  return(TRUE)
                }
                if (!is.null(info$by) &&
                  info$by != 1L) {
                  return(TRUE)
                }
                if (!is.null(info$start_expr)) {
                  return(TRUE)
                }
                start <- .mojor_state$`%||%`(info$start, 1L)
                end_delta <- .mojor_state$`%||%`(info$end_delta, 0L)
                if (start != 1L) {
                  return(TRUE)
                }
                if (!is.null(end_delta) &&
                  end_delta != 0L) {
                  return(TRUE)
                }
                FALSE
            }, logical(1)
        )
    )
    if (isTRUE(broadcast_nd)) {
        simd_reason <- "broadcast_nd uses index remap"
    } else if (has_while) {
        simd_reason <- "while loop present"
    } else if (has_nested) {
        simd_reason <- "nested loops present"
    } else if (non_unit_range) {
        simd_reason <- "non-1-based range"
    } else if (length(loop_infos) !=
        1) {
        simd_reason <- "multiple loops (pre-fusion)"
    } else if (out_kind == "vector") {
        simd_safe <- TRUE
    } else if (out_kind == "scalar") {
        if (scalar_accum_present && identical(simd_mode, "auto")) {
            red_probe <- .mojor_scalar_reduction(
                loop_infos[[1]]$blocks, loop_infos[[1]]$var, scalar_init,
                local_types
            )
            if (!is.null(red_probe) &&
                (!is.null(scalar_type) &&
                  scalar_type %in% c("f64", "f32"))) {
                simd_safe <- TRUE
            } else {
                simd_reason <- "scalar reduction not recognized for auto"
            }
        } else if (scalar_accum_present) {
            simd_reason <- "scalar accumulator present"
        } else {
            simd_reason <- "scalar output not eligible"
        }
    } else {
        simd_reason <- "output is not a vector"
    }

 # Optional loop fusion: vector output, no nested loops, no scalar
 # accumulator.
    fusion_applied <- FALSE
    fusion_reason <- NULL
    fusion_vars <- NULL
    if (has_while) {
        fusion_reason <- "while loop present"
    } else if (length(loop_infos) >
        1 && out_kind == "vector" && !scalar_accum_present) {
        loop_vars <- vapply(
            loop_infos, function(li) li$var,
            character(1)
        )
        has_nested <- any(
            vapply(
                loop_infos, function(li) {
                  any(
                    vapply(
                      li$blocks, function(s) is.call(s) &&
                        as.character(s[[1]]) ==
                          "for", logical(1)
                  )
                )
                }, logical(1)
            )
        )
        if (has_nested) {
            fusion_reason <- "nested loops present"
        } else if (!all(
            vapply(
                loop_infos, function(li) {
                  same_start <- isTRUE(all.equal(li$range_start, loop_infos[[1]]$range_start))
                  same_start_expr <- identical(li$range_start_expr, loop_infos[[1]]$range_start_expr)
                  same_step <- identical(li$range_step, loop_infos[[1]]$range_step)
                  same_end <- identical(li$range_end, loop_infos[[1]]$range_end)
                  (same_start || same_start_expr) && same_step && same_end
                }, logical(1)
            )
        )) {
            fusion_reason <- "non-matching loop ranges"
        } else {
            canonical_var <- loop_vars[[1]]
            fused_blocks <- list()
            for (li in loop_infos) {
                blocks <- li$blocks
                if (li$var != canonical_var) {
                  blocks <- .mojor_rename_block(blocks, li$var, canonical_var)
                }
                fused_blocks <- c(fused_blocks, blocks)
            }
            loop_infos <- list(
                list(
                  kind = "for", var = canonical_var, seq = loop_infos[[1]]$seq,
                  blocks = fused_blocks, seq_info = loop_infos[[1]]$seq_info,
                  range_expr = loop_infos[[1]]$range_expr, range_start = loop_infos[[1]]$range_start,
                  range_end = loop_infos[[1]]$range_end
              )
            )
            fusion_applied <- TRUE
            fusion_vars <- loop_vars
        }
    } else if (length(loop_infos) >
        1 && out_kind != "vector") {
        fusion_reason <- "output is not a vector"
    } else if (length(loop_infos) >
        1 && scalar_accum_present) {
        fusion_reason <- "scalar accumulator present"
    } else if (length(loop_infos) <=
        1) {
        fusion_reason <- "single loop"
    }
    if (fusion_applied && simd_safe && length(loop_infos) ==
        1) {
        simd_reason <- NULL
    }
    if (fusion_applied && out_kind == "vector" && !scalar_accum_present &&
        !has_nested && length(loop_infos) ==
        1) {
        simd_safe <- TRUE
        simd_reason <- NULL
    }

 # Parallel safety (conservative): single loop, vector output, no
 # accumulator, no out[j] dependencies. Exception: scalar
 # reductions eligible for IR reduction (tree/simd) are considered
 # 'parallel safe' in the sense that they can use parallelized
 # reduction strategies.
    parallel_safe <- FALSE
    parallel_reason <- NULL
    has_nested_after <- any(
        vapply(
            loop_infos, function(li) {
                any(
                  vapply(
                    li$blocks, function(s) is.call(s) &&
                      as.character(s[[1]]) ==
                        "for", logical(1)
                )
              )
            }, logical(1)
        )
    )
 # Check if this is a scalar reduction eligible for IR reduction
 # (tree/simd)
    scalar_reduction_eligible <- out_kind == "scalar" && !is.null(scalar_reduce_ir_node) &&
        !is.null(scalar_reduction_op) &&
        scalar_reduction_op %in% c("sum", "product", "min", "max", "which.min", "which.max") &&
        length(loop_infos) ==
            1 && !is.null(scalar_reduction_arg)
    if (scalar_reduction_eligible) {
 # Scalar reductions can use tree/simd strategies, so consider
 # them parallel-safe
        parallel_safe <- TRUE
    } else if (has_while) {
        parallel_reason <- "while loop present"
    } else if (out_kind != "vector") {
        parallel_reason <- "output is not a vector"
    } else if (scalar_accum_present) {
        parallel_reason <- "scalar accumulator present"
    } else if (length(loop_infos) !=
        1) {
        parallel_reason <- "multiple loops"
    } else if (has_nested_after) {
        parallel_reason <- "nested loops present"
    } else if (!.mojor_loop_parallel_safe(loop_infos[[1]]$blocks, loop_infos[[1]]$var, out_name)) {
        parallel_reason <- "loop-carried dependency on output"
    } else {
        parallel_safe <- TRUE
    }
 # Store parallel_safe in each loop info for use during code
 # emission
    for (i in seq_along(loop_infos)) {
        loop_infos[[i]]$parallel_safe <- parallel_safe && length(loop_infos) ==
            1
    }
    if (isTRUE(.mojor_state$options$warn_parallel) &&
        !parallel_safe && !is.null(parallel_reason)) {
        .mojor_warn("parallelization skipped", NULL, parallel_reason)
    }
    auto_schedule <- .mojor_apply_auto_schedule_policy(
        schedule = schedule, reduction_mode = reduction, unroll = unroll,
        tile = tile, opt_level = opt_level, loop_infos = loop_infos, out_kind = out_kind,
        out_matrix = out_matrix, parallel_safe = parallel_safe, simd_safe = simd_safe,
        scalar_reduction_eligible = scalar_reduction_eligible, scalar_reduction_op = scalar_reduction_op,
        scalar_reduction_arg = scalar_reduction_arg, arg_specs = arg_specs,
        broadcast_nd = broadcast_nd, unroll_explicit = unroll_explicit,
        reduction_explicit = reduction_explicit, tile_explicit = tile_explicit
    )
    schedule <- auto_schedule$schedule
    unroll_effective <- auto_schedule$unroll_effective
    reduction_effective <- auto_schedule$reduction_effective
    tile_effective <- auto_schedule$tile_effective
    auto_schedule_policy <- auto_schedule$policy
    unroll <- unroll_effective
    reduction <- reduction_effective
    tile <- tile_effective
    schedule_effective <- schedule
    if (is.list(schedule_effective)) {
        schedule_effective$type_env <- NULL
        schedule_effective <- .mojor_prune_schedule_nulls(schedule_effective)
    }
    if (isTRUE(fusion_debug)) {
        fusion_reason <- if (fusion_applied) {
            paste0(
                "fused ", length(fusion_vars),
                " loops (vars: ", paste(fusion_vars, collapse = ", "),
                ")"
            )
        } else {
            paste0("fusion not applied (", fusion_reason, ")")
        }
    }

    use_argext <- FALSE
    needs_argext_import <- FALSE

    mojo_lines <- c(
        "# Generated by mojor_transpile (stable subset)", "from memory import OpaquePointer, UnsafePointer, alloc",
        "from math import inf, nan", "comptime ImmutOpaqueAny = OpaquePointer[mut=False, origin=ImmutAnyOrigin]",
        "comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]",
        "comptime ImmutF32Ptr = UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin]",
        "comptime MutF32Ptr = UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin]",
        "comptime ImmutF64Ptr = UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin]",
        "comptime MutF64Ptr = UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin]",
        "comptime _MOJOR_INF = inf[DType.float64]()", "comptime _MOJOR_NINF = -inf[DType.float64]()",
        "comptime _MOJOR_NAN = nan[DType.float64]()", "comptime _MOJOR_INF_F32 = inf[DType.float32]()",
        "comptime _MOJOR_NINF_F32 = -inf[DType.float32]()", "comptime _MOJOR_NAN_F32 = nan[DType.float32]()"
    )

 # Stage A/B: Import debug helpers if debug mode enabled
    if (isTRUE(.mojor_state$debug_mode) ||
        isTRUE(.mojor_state$trace_mode)) {
        debug_imports <- "from debug_helpers import mojor_assert, mojor_check_bounds, mojor_check_bounds_2d, mojor_trace, mojor_trace_value, mojor_trace_value_int, mojor_trace_enter, mojor_trace_exit"
 # Stage B: add memory safety helpers
        if (isTRUE(.mojor_state$memory_check)) {
            debug_imports <- paste0(
                debug_imports, ", mojor_check_ptr_f64, mojor_check_ptr_f64_mut, mojor_check_ptr_f32, mojor_check_ptr_f32_mut, mojor_check_ptr_i32, mojor_check_ptr_i32_mut, mojor_check_length"
            )
        }
        mojo_lines <- c(mojo_lines, debug_imports)
    }
 # FFI import when external_call is used
    if (isTRUE(.mojor_state$needs_ffi)) {
        mojo_lines <- c(mojo_lines, "from sys.ffi import external_call")
    }
 # Step 8.11: Always import Layout/LayoutTensor <U+2014> needed
 # for any matrix/array output and safe to include even when
 # unused (zero runtime cost for comptime-only symbols)
    needs_layout_tensor <- TRUE
    if (isTRUE(elementwise)) {
        mojo_lines <- c(
            mojo_lines, "from algorithm.functional import elementwise",
            "from sys.info import simd_width_of", "from utils import IndexList"
        )
        if (identical(elementwise_target, "gpu")) {
            gpu_api_const <- .mojor_gpu_api_override()
            if (!nzchar(gpu_api_const)) {
                gpu_api_const <- .mojor_gpu_api_platform_default()
            } else if (!gpu_api_const %in% c("metal", "cuda", "amd", "auto")) {
                warning(
                  "mojor: invalid MOJOR_GPU_API value for transpile; defaulting to platform default",
                  call. = FALSE
              )
                gpu_api_const <- .mojor_gpu_api_platform_default()
            } else if (gpu_api_const == "auto") {
                gpu_api_const <- .mojor_gpu_api_platform_default()
            }
            if (is.null(elementwise_gpu_layouttensor)) {
                elementwise_gpu_layouttensor <- identical(gpu_api_const, "metal")
            }
            cap_bytes <- .mojor_gpu_max_bytes()
            if (is.na(cap_bytes) ||
                cap_bytes <= 0) {
                cap_bytes <- 16 * 1024^3
            }
            cap_str <- sprintf("%.0f", cap_bytes)
            mojo_lines <- c(
                mojo_lines, "from sys import has_accelerator", "from sys import align_of",
                "from gpu.host import DeviceContext, DeviceBuffer", "from gpu import block_idx, block_dim, thread_idx",
                "from gpu.host.compile import get_gpu_target", "from layout import Layout, LayoutTensor, RuntimeLayout",
                "from utils import Index", paste0(
                  "comptime MOJOR_GPU_API: String = \"", gpu_api_const,
                  "\""
              ),
                "comptime MutCtxPtr = UnsafePointer[mut=True, type=DeviceContext, origin=MutAnyOrigin]",
                "comptime MutBufI32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.int32], origin=MutAnyOrigin]",
                "comptime MutBufF32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin]",
                "comptime MutBufF64Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin]",
                "comptime NULL_CTX = MutCtxPtr(unsafe_from_address=0)",
                "comptime NULL_BUF_I32 = MutBufI32Ptr(unsafe_from_address=0)",
                "comptime NULL_BUF_F32 = MutBufF32Ptr(unsafe_from_address=0)",
                "comptime NULL_BUF_F64 = MutBufF64Ptr(unsafe_from_address=0)",
                "@fieldwise_init", "struct GpuBufF32(Movable):", "    var buf: MutBufF32Ptr",
                "    var len: Int", "@fieldwise_init", "struct GpuBufF64(Movable):",
                "    var buf: MutBufF64Ptr", "    var len: Int", "comptime MutGpuBufF32Ptr = UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin]",
                "comptime MutGpuBufF64Ptr = UnsafePointer[mut=True, type=GpuBufF64, origin=MutAnyOrigin]",
                "comptime NULL_GPUBUF = MutGpuBufF32Ptr(unsafe_from_address=0)",
                "comptime NULL_GPUBUF_F64 = MutGpuBufF64Ptr(unsafe_from_address=0)",
                paste0("comptime MOJOR_GPU_MAX_BYTES: Int64 = ", cap_str),
                "fn _mojor_gpu_limit_ok(n_i: Int, buffers: Int, bytes_per: Int) -> Bool:",
                "    if n_i <= 0: return True", "    var total: Int64 = Int64(n_i) * Int64(buffers) * Int64(bytes_per)",
                "    return total <= MOJOR_GPU_MAX_BYTES", "fn _mojor_set_status(ptr: MutOpaqueAny, value: Int32) -> None:",
                "    var out: UnsafePointer[mut=True, type=Int32, origin=MutAnyOrigin] = ptr.bitcast[Int32]()",
                "    out[0] = value"
            )
        }
    }
    if (needs_layout_tensor) {
        mojo_lines <- c(
            mojo_lines, "from layout import Layout, LayoutTensor, RuntimeLayout",
            "from utils import IndexList"
        )
    }
    if (needs_argext_import) {
        mojo_lines <- c(
            mojo_lines, "from nn.argmaxmin import argmax, argmin", paste0("comptime _MOJOR_ARGEXT_LAYOUT = ", layout_ctor(1, "0"))
        )
    }

 # Check if RNG is needed
    needs_rng <- isTRUE(.mojor_state$needs_mojo_random)
    needs_rng_tables <- isTRUE(.mojor_state$needs_rng_tables)
    if (needs_rng) {
        rng_calls <- .mojor_state$current_rng_calls
        if (is.null(rng_calls))
            rng_calls <- character(0)
        rng_helper_syms <- .mojor_rng_helper_symbols_for_calls(rng_calls)
        if (length(rng_helper_syms) ==
            0) {
            stop("mojor_transpile: RNG helper symbol catalog is unavailable")
        }
        if (!("_mojor_sample_pick_index" %in% rng_helper_syms)) {
            rng_helper_syms <- c(rng_helper_syms, "_mojor_sample_pick_index")
        }
        rng_imports <- c(
            "comptime MutU64Ptr = UnsafePointer[mut=True, type=UInt64, origin=MutAnyOrigin]",
            "from math import exp, log, log1p, sqrt", paste0("from rng_helpers import ", paste(rng_helper_syms, collapse = ", "))
        )
        if (needs_rng_tables) {
            rng_imports <- c(
                rng_imports, "from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE, _ZIGGURAT_NOR_R, _ZIGGURAT_NOR_INV_R"
            )
        }
        mojo_lines <- c(mojo_lines, rng_imports)
    }

 # NA helper imports
    needs_nan_predicate <- any(
        grepl(
            "\\bis\\.nan\\s*\\(", deparse(body(fn)),
            perl = TRUE
        )
    )
    needs_na <- isTRUE(.mojor_state$needs_na_helpers) ||
        na_mode %in% c("propagate", "na_rm") ||
        isTRUE(needs_nan_predicate)
    if (needs_na) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "from na_helpers import _mojor_is_na_f64, _mojor_is_na_f32, _mojor_is_na_i32, _mojor_is_na_lgl"
            ),
            paste0("from na_helpers import _mojor_is_nan_f64, _mojor_is_nan_f32"),
            paste0("from na_helpers import _mojor_r_na_f64, _mojor_r_na_f32"),
            paste0(
                "from na_helpers import _mojor_add_f64_na, _mojor_sub_f64_na, _mojor_mul_f64_na, _mojor_div_f64_na"
            ),
            paste0(
                "from na_helpers import _mojor_lt_f64_na, _mojor_gt_f64_na, _mojor_le_f64_na, _mojor_ge_f64_na, _mojor_eq_f64_na, _mojor_ne_f64_na"
            ),
            paste0(
                "from na_helpers import _mojor_add_f32_na, _mojor_sub_f32_na, _mojor_mul_f32_na, _mojor_div_f32_na"
            ),
            paste0(
                "from na_helpers import _mojor_lt_f32_na, _mojor_gt_f32_na, _mojor_le_f32_na, _mojor_ge_f32_na, _mojor_eq_f32_na, _mojor_ne_f32_na"
            ),
            paste0(
                "from na_helpers import _mojor_skip_na_f64, _mojor_skip_na_f32, _mojor_skip_na_i32"
            )
        )
    }

 # Set/Match helper imports
    needs_set_match <- isTRUE(.mojor_state$needs_set_match)
    if (needs_set_match) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "from set_match_helpers import mojor_unique_f64, mojor_unique_f32, mojor_unique_i32"
            ),
            paste0(
                "from set_match_helpers import mojor_duplicated_f64, mojor_duplicated_f32, mojor_duplicated_i32"
            ),
            paste0(
                "from set_match_helpers import mojor_any_duplicated_f64, mojor_any_duplicated_f32, mojor_any_duplicated_i32"
            ),
            paste0(
                "from set_match_helpers import mojor_match_f64, mojor_match_f32, mojor_match_i32"
            ),
            paste0(
                "from set_match_helpers import mojor_in_f64, mojor_in_f32, mojor_in_i32"
            )
        )
    }

 # Quantile helper imports
    needs_quantile <- isTRUE(.mojor_state$needs_quantile)
    if (needs_quantile) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "from quantile_helpers import mojor_median_f64, mojor_median_f32, mojor_median_i32"
            ),
            paste0(
                "from quantile_helpers import mojor_quantile_f64, mojor_quantile_f32, mojor_quantile_i32"
            ),
            paste0(
                "from quantile_helpers import mojor_iqr_f64, mojor_iqr_f32, mojor_iqr_i32"
            ),
            paste0(
                "from quantile_helpers import mojor_mad_f64, mojor_mad_f32, mojor_mad_i32"
            )
        )
    }

 # Detect if any loop uses parallel
    any_parallel <- isTRUE(.mojor_state$current_parallel) &&
        parallel_safe
    if (!any_parallel) {
        for (li in loop_infos) {
            if (isTRUE(li$seq_info$parallel)) {
                any_parallel <- TRUE
                break
            }
        }
    }
    if (any_parallel) {
        mojo_lines <- c(mojo_lines, "from algorithm import parallelize")
    }
    if (!is.null(assume_aligned)) {
        mojo_lines <- c(
            mojo_lines, paste0("comptime _MOJOR_ALIGN = ", as.integer(assume_aligned)),
            paste0("comptime _MOJOR_ALIGN_BYTES = ", as.integer(assume_aligned)),
            paste0("comptime _MOJOR_ALIGN_F64 = Int(_MOJOR_ALIGN_BYTES / 8)"),
            paste0("comptime _MOJOR_ALIGN_F32 = Int(_MOJOR_ALIGN_BYTES / 4)"),
            paste0("# assume_aligned: ", as.integer(assume_aligned))
        )
    }
    math_fns <- c(
        "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
        "tanh", "log", "log1p", "exp", "expm1", "floor", "ceiling", "trunc",
        "sign", "abs2", "hypot", "cbrt", "lgamma", "erf", "gamma"
    )
    used_math_fns <- character(0)
    for (lp in loop_infos) {
        if (identical(lp$kind, "for") ||
            identical(lp$kind, "while")) {
            for (s in lp$blocks) {
                if (is.call(s)) {
                  if (as.character(s[[1]]) %in%
                    c("<-", "=")) {
                    rhs <- s[[3]]
                    found <- .mojor_find_used_math_fns(rhs, math_fns)
                    if (length(found) >
                      0) {
                      used_math_fns <- unique(c(used_math_fns, found))
                    }
                  } else if (as.character(s[[1]]) ==
                    "if") {
                    found <- .mojor_find_used_math_fns(s[[2]], math_fns)
                    if (length(found) >
                      0) {
                      used_math_fns <- unique(c(used_math_fns, found))
                    }
                  }
                }
            }
        }
    }
    if (length(used_math_fns) >
        0) {
 # Map R names to Mojo names (ceiling -> ceil)
        mojo_names <- used_math_fns
        mojo_names[mojo_names == "ceiling"] <- "ceil"
        mojo_lines <- c(
            paste0("from math import ", paste(mojo_names, collapse = ", ")),
            mojo_lines
        )
    }
    if (identical(return_div, "sd_count") &&
        !any(grepl("\\bfrom math import\\b.*\\bsqrt\\b", mojo_lines))) {
        mojo_lines <- c("from math import sqrt", mojo_lines)
    }
 # Pointer aliases
    ptr_specs_raw <- c(
        arg_specs[vapply(arg_specs, .mojor_is_array, logical(1))],
        if (out_kind == "vector") out_type else scalar_type
    )
    ptr_specs_raw <- ptr_specs_raw[
        !vapply(ptr_specs_raw, is.null, logical(1))
    ]
    ptr_specs <- vapply(
        ptr_specs_raw,
        function(spec) {
            if (!is.character(spec) || length(spec) != 1L || is.na(spec)) {
                return(NA_character_)
            }
            spec
        },
        character(1)
    )
    if (anyNA(ptr_specs)) {
        stop("mojor_transpile: internal pointer type derivation received non-scalar type spec")
    }
    ptr_bases <- unique(sub("\\[\\]$", "", ptr_specs))
    if (length(nd_arrays) >
        0) {
        ptr_bases <- unique(c(ptr_bases, "i32"))
    }
    if (isTRUE(out_array)) {
        ptr_bases <- unique(c(ptr_bases, "i32"))
    }
    if (isTRUE(broadcast_nd)) {
        ptr_bases <- unique(c(ptr_bases, "i32"))
    }
    ptr_bases <- unique(c(ptr_bases, "i32"))
    ptr_types <- unique(vapply(
        ptr_bases,
        function(base) {
            ty <- .mojor_mojo_type(base)
            if (is.null(ty) || !is.character(ty) || length(ty) != 1L || !nzchar(ty)) {
                stop(
                    "mojor_transpile: internal pointer type derivation produced unsupported base '",
                    base, "'"
                )
            }
            ty
        },
        character(1)
    ))
    for (ty in ptr_types) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "comptime Immut", ty, "Ptr = UnsafePointer[mut=False, type=",
                ty, ", origin=ImmutAnyOrigin]"
            ),
            paste0(
                "comptime Mut", ty, "Ptr = UnsafePointer[mut=True, type=",
                ty, ", origin=MutAnyOrigin]"
            )
        )
    }
    if (length(tensor_ranks) >
        0) {
        for (nm in names(tensor_ranks)) {
            rank <- tensor_ranks[[nm]]
            zeros <- paste(
                rep("0", rank),
                collapse = ", "
            )
            mojo_lines <- c(
                mojo_lines, paste0("comptime _MOJOR_LAYOUT_", nm, " = ", layout_ctor(rank, zeros))
            )
        }
    }

 # Add comptime layout for 2D matrix outputs (for mat[i, ] <- c(x,
 # y) pattern) needs_layout_tensor is already TRUE (set
 # unconditionally above), so Layout is imported
    if (isTRUE(out_matrix) ||
        isTRUE(out_array)) {
        mojo_lines <- c(
            mojo_lines, paste0("comptime _MOJOR_MATRIX_LAYOUT = ", layout_ctor(2, "0, 0"))
        )
    }

    if (isTRUE(broadcast_nd)) {
 # Step broadcast_nd: Emit broadcast index helper function
        mojo_lines <- c(
            mojo_lines, "fn _mojor_bcast_index(out_idx: Int, out_dim: ImmutInt32Ptr, out_ndim: Int, arg_dim: ImmutInt32Ptr, arg_ndim: Int) -> Int:",
            "    if out_ndim <= 0: return 0", "    var idx = out_idx",
            "    var arg_idx = 0", "    var arg_stride = 1", "    var shift = out_ndim - arg_ndim",
            "    var axis = 0", "    while axis < out_ndim:", "        var out_d = Int(out_dim[axis])",
            "        var coord = 0", "        if out_d > 0:", "            coord = idx % out_d",
            "            idx = Int(idx / out_d)", "        if axis >= shift:",
            "            var ad = Int(arg_dim[axis - shift])", "            var acoord = 0",
            "            if ad > 1:", "                acoord = coord",
            "            arg_idx += acoord * arg_stride", "            arg_stride *= ad",
            "        axis += 1", "    return arg_idx"
        )
    }

 # Signature: input arrays + out + n
    out_alias_source <- NULL
    if (isTRUE(out_kind == "vector") &&
        !is.null(out_name) &&
        !(out_name %in% args) &&
        !is.null(array_aliases[[out_name]])) {
        alias_nm <- array_aliases[[out_name]]
        alias_spec <- arg_specs[[alias_nm]]
        if (!is.null(alias_nm) && alias_nm %in% args &&
            !is.null(alias_spec) && .mojor_is_array(alias_spec)) {
            out_alias_source <- alias_nm
        }
    }
    out_alias_input <- isTRUE(out_kind == "vector") &&
        !is.null(out_name) && out_name %in% args
    out_ptr_param <- if (out_kind == "vector") {
        if (out_alias_input) "__mojor_out_ptr" else paste0(out_name, "_ptr")
    } else {
        paste0(scalar_init, "_ptr")
    }
    n_param <- "__mojor_n"
    sig_args <- c()
    for (arg in args) {
        spec <- arg_specs[[arg]]
        if (.mojor_is_array(spec)) {
            sig_args <- c(sig_args, paste0(arg, "_ptr: ImmutOpaqueAny"))
        } else {
            sig_args <- c(sig_args, paste0(arg, ": ", .mojor_mojo_type(spec)))
        }
    }
    sig_args <- c(
        sig_args, paste0(out_ptr_param, ": MutOpaqueAny"),
        paste0(n_param, ": Int32")
    )
    if (isTRUE(.mojor_state$needs_mojo_random)) {
        sig_args <- c(sig_args, "__mojor_rng_state_ptr: MutOpaqueAny")
    }
    if (length(len_param_map) >
        0) {
        for (nm in names(len_param_map)) {
            sig_args <- c(sig_args, paste0(len_param_map[[nm]], ": Int32"))
        }
    }
    if (length(nrow_param_map) >
        0) {
        for (nm in names(nrow_param_map)) {
            sig_args <- c(sig_args, paste0(nrow_param_map[[nm]], ": Int32"))
        }
    }
    if (length(ncol_param_map) >
        0) {
        for (nm in names(ncol_param_map)) {
 # Only add ncol if not already added as part of nrow
 # (same array) This matches the logic in 04_build.R
 # call_args construction
            if (!(nm %in% names(nrow_param_map))) {
                sig_args <- c(sig_args, paste0(ncol_param_map[[nm]], ": Int32"))
            }
        }
    }
    if (length(dim_param_map) >
        0) {
        for (nm in names(dim_param_map)) {
            sig_args <- c(sig_args, paste0(dim_param_map[[nm]], "_ptr: ImmutOpaqueAny"))
            sig_args <- c(sig_args, paste0(ndim_param_map[[nm]], ": Int32"))
        }
    }
    if (isTRUE(out_matrix)) {
        sig_args <- c(
            sig_args, paste0(.mojor_out_nrow_param_name(), ": Int32"),
            paste0(.mojor_out_ncol_param_name(), ": Int32")
        )
    }
    if (isTRUE(out_array) ||
        isTRUE(broadcast_nd)) {
        sig_args <- c(
            sig_args, paste0(.mojor_out_dim_param_name(), "_ptr: ImmutOpaqueAny")
        )
        sig_args <- c(sig_args, paste0(.mojor_out_ndim_param_name(), ": Int32"))
    }
    if (na_mode %in% c("forbid", "propagate") || isTRUE(index_bounds)) {
        sig_args <- c(sig_args, "__mojor_na_flag_ptr: MutOpaqueAny")
    }
    parse_sig_param_name <- function(sig_arg) {
        if (!is.character(sig_arg) || length(sig_arg) != 1L || !nzchar(sig_arg)) {
            return(NA_character_)
        }
        pieces <- strsplit(sig_arg, ":", fixed = TRUE)[[1]]
        if (length(pieces) < 1L) {
            return(NA_character_)
        }
        trimws(pieces[[1L]])
    }
    sig_param_names <- vapply(sig_args, parse_sig_param_name, character(1))
    bad_sig_names <- which(!nzchar(sig_param_names) | is.na(sig_param_names))
    if (length(bad_sig_names) > 0L) {
        stop(
            "mojor_transpile: internal signature assembly produced invalid parameter name(s)"
        )
    }
    dup_sig_names <- unique(sig_param_names[duplicated(sig_param_names)])
    if (length(dup_sig_names) > 0L) {
        stop(
            "mojor_transpile: internal signature assembly produced duplicate parameter(s): ",
            paste(dup_sig_names, collapse = ", ")
        )
    }
    if (isTRUE(out_alias_input)) {
        alias_len_param <- .mojor_len_param_name(out_name)
        if (!(alias_len_param %in% sig_param_names)) {
            stop(
                "mojor_transpile: alias-output kernel requires explicit length parameter ",
                alias_len_param
            )
        }
    }

    mojo_lines <- c(mojo_lines, "", paste0("@export(\"", name, "\", ABI=\"C\")"))
 # Add 'raises' to kernel signature in debug mode.
    kernel_raises <- if (isTRUE(.mojor_state$debug_mode))
        " raises" else ""
    mojo_lines <- c(
        mojo_lines, paste0(
            "fn ", name, "(", paste(sig_args, collapse = ", "),
            ")", kernel_raises, " -> None:"
        )
    )
    mojo_lines <- c(mojo_lines, paste0("    var n_i = Int(", n_param, ")"))
    if (na_mode %in% c("forbid", "propagate") || isTRUE(index_bounds)) {
        mojo_lines <- c(
            mojo_lines, "    var __mojor_na_flag: MutInt32Ptr = __mojor_na_flag_ptr.bitcast[Int32]()",
            "    __mojor_na_flag[0] = 0"
        )
    }
    if (length(len_param_map) >
        0) {
        for (nm in names(len_param_map)) {
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", len_var_map[[nm]], " = Int(", len_param_map[[nm]],
                  ")"
              )
            )
        }
    }
    if (length(nrow_param_map) >
        0) {
        for (nm in names(nrow_param_map)) {
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", nrow_var_map[[nm]], " = Int(", nrow_param_map[[nm]],
                  ")"
              )
            )
        }
    }
 # PR-B3: Emit ncol variables (from param if available, else
 # computed from length/nrow)
    if (length(ncol_arrays) >
        0) {
        for (nm in ncol_arrays) {
            if (!is.null(ncol_var_map[[nm]])) {
 # If dim metadata is available, materialize ncol from
 # dim_ptr[1] so downstream emitters can safely
 # reference ncol_var_map[[nm]].
                if (!is.null(dim_param_map[[nm]])) {
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "    var ", ncol_var_map[[nm]], " = Int(", dim_param_map[[nm]],
                      "_ptr.bitcast[Int32]()[1])"
                  )
                )
                  next
                }
                if (!is.null(ncol_param_map[[nm]]) &&
                  is.null(nrow_param_map[[nm]])) {
 # Use ncol from parameter (passed from wrapper)
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "    var ", ncol_var_map[[nm]], " = Int(", ncol_param_map[[nm]],
                      ")"
                  )
                )
                } else if (!is.null(nrow_var_map[[nm]]) &&
                  !is.null(len_var_map[[nm]])) {
 # Compute ncol from length/nrow
                  mojo_lines <- c(mojo_lines, paste0("    var ", ncol_var_map[[nm]], ": Int = 0"))
                  mojo_lines <- c(mojo_lines, paste0("    if ", nrow_var_map[[nm]], " > 0:"))
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "        ", ncol_var_map[[nm]], " = Int(", len_var_map[[nm]],
                      " / ", nrow_var_map[[nm]], ")"
                  )
                )
                }
            }
        }
    }
    if (length(dim_param_map) >
        0) {
        for (nm in names(dim_param_map)) {
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", dim_var_map[[nm]], ": ImmutInt32Ptr = ",
                  dim_param_map[[nm]], "_ptr.bitcast[Int32]()"
              )
            )
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", ndim_var_map[[nm]], " = Int(", ndim_param_map[[nm]],
                  ")"
              )
            )
        }
    }
    if (isTRUE(out_matrix)) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", .mojor_out_nrow_var_name(), " = Int(", .mojor_out_nrow_param_name(),
                ")"
            )
        )
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", .mojor_out_ncol_var_name(), " = Int(", .mojor_out_ncol_param_name(),
                ")"
            )
        )
    }
    if (isTRUE(out_array) ||
        isTRUE(broadcast_nd)) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", .mojor_out_dim_var_name(), ": ImmutInt32Ptr = ",
                .mojor_out_dim_param_name(), "_ptr.bitcast[Int32]()"
            )
        )
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", .mojor_out_ndim_var_name(), " = Int(", .mojor_out_ndim_param_name(),
                ")"
            )
        )
    }

 # Cast pointers (MOVED BEFORE local allocations to ensure params
 # are available)
    for (arg in args) {
        spec <- arg_specs[[arg]]
        ty <- .mojor_mojo_type(spec)
        if (.mojor_is_array(spec)) {
            is_alias_arg <- isTRUE(out_alias_input && identical(arg, out_name))
            arg_var <- if (is_alias_arg) paste0(arg, "_in") else arg
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", arg_var, ": Immut", ty, "Ptr = ", arg, "_ptr.bitcast[",
                  ty, "]()"
              )
            )
 # Stage B: Memory safety - validate pointer +
 # length after bitcast
            if (isTRUE(.mojor_state$memory_check)) {
                check_fn <- switch(
                  spec, `f64[]` = "mojor_check_ptr_f64", `f32[]` = "mojor_check_ptr_f32",
                  `i32[]` = "mojor_check_ptr_i32", `lgl[]` = "mojor_check_ptr_i32",
                  NULL
              )
                if (!is.null(check_fn)) {
                  len_var <- if (arg %in% names(len_var_map))
                    len_var_map[[arg]] else "n_i"
                  source_file <- if (!is.null(.mojor_state$source_file))
                    .mojor_state$source_file else "kernel.R"
                  mojo_lines <- c(
                    mojo_lines, paste0(
                      "    ", check_fn, "(", arg, ", ", len_var, ", \"",
                      arg, "\", \"", source_file, "\", 1)"
                  )
                )
                }
            }
            if (!is.null(assume_aligned)) {
                mojo_lines <- c(mojo_lines, paste0("    var ", arg, "_aligned = True"))
            }
        }
    }

 # Helper: resolve length() calls in an R expression to Mojo
 # length variables
    resolve_length_in_expr <- function(expr) {
        try_scalar_range <- function(ast) {
            tryCatch(
                .mojor_scalar_range_expr_to_mojo(
                    ast,
                    loop_arg_specs,
                    loop_args,
                    len_var_map,
                    n_source_name,
                    scalar_inits = scalar_inits,
                    nrow_var_map = nrow_var_map,
                    ncol_var_map = ncol_var_map,
                    dim_var_map = dim_var_map,
                    allow_scalar_cast = TRUE
                ),
                error = function(e) NULL
            )
        }
 # sum(mask) local-length patterns in strict kernels should not emit a raw
 # Mojo `sum(...)` call; map to known length vars / output length.
        if (is.call(expr) &&
            identical(expr[[1]], as.name("sum")) &&
            length(expr) >= 2 &&
            is.name(expr[[2]])) {
            mask_name <- as.character(expr[[2]])
            mask_spec <- arg_specs[[mask_name]]
            if (!is.null(mask_spec) && .mojor_is_logical_mask_type(mask_spec)) {
                if (!is.null(out_len_source) &&
                    identical(out_len_source$kind, "mask_true") &&
                    identical(out_len_source$name, mask_name)) {
                    return("n_i")
                }
                lv <- .mojor_len_var_for(
                    list(kind = "array", name = mask_name),
                    n_source_name, len_var_map
                )
                if (is.null(lv)) {
                    lv <- .mojor_len_var_name(mask_name)
                }
                return(lv)
            }
        }
 # Simple length(param) call
        if (is.call(expr) &&
            identical(expr[[1]], as.name("length")) &&
            length(expr) ==
                2 && is.name(expr[[2]])) {
            arg_name <- as.character(expr[[2]])
            lv <- .mojor_len_var_for(
                list(kind = "array", name = arg_name),
                n_source_name, len_var_map
            )
            if (is.null(lv))
                lv <- .mojor_len_var_name(arg_name)
            return(lv)
        }
 # Numeric literal
        if (is.numeric(expr)) {
            return(as.character(as.integer(expr)))
        }
 # Name that is a known scalar arg
        if (is.name(expr)) {
            return(as.character(expr))
        }
 # Compound expression: recursively resolve sub-expressions
        if (is.call(expr)) {
            range_expr <- try_scalar_range(expr)
            if (!is.null(range_expr)) {
                return(range_expr)
            }
            op <- as.character(expr[[1]])
            if (op %in% c("+", "-", "*", "/", "%%", "%/%")) {
 # Map R operators to Mojo operators
                mojo_op <- if (op == "%/%")
                  "//" else if (op == "%%")
                  "%" else op
                left <- resolve_length_in_expr(expr[[2]])
                if (length(expr) ==
                  3) {
                  right <- resolve_length_in_expr(expr[[3]])
                  return(paste0("(", left, " ", mojo_op, " ", right, ")"))
                } else {
 # Unary minus
                  return(paste0("(-", left, ")"))
                }
            }
        }
        range_expr <- try_scalar_range(expr)
        if (!is.null(range_expr)) {
            return(range_expr)
        }
 # Fallback: use general expression emitter
        return(
            .mojor_expr_to_mojo(
                expr, character(0),
                list(), in_cond = FALSE
            )
        )
    }

 # Step 5.1: Emit dimension variables for local matrix/array
 # allocations
    if (length(.mojor_state$current_local_matrix_dims) >
        0) {
        for (nm in names(.mojor_state$current_local_matrix_dims)) {
            dims <- .mojor_state$current_local_matrix_dims[[nm]]
            nrow_var <- paste0("nrow_", nm)
            ncol_var <- paste0("ncol_", nm)
            nrow_expr <- resolve_length_in_expr(dims$nrow)
            ncol_expr <- resolve_length_in_expr(dims$ncol)
            mojo_lines <- c(mojo_lines, paste0("    var ", nrow_var, " = Int(", nrow_expr, ")"))
            mojo_lines <- c(mojo_lines, paste0("    var ", ncol_var, " = Int(", ncol_expr, ")"))
 # Populate the nrow/ncol var maps for slice emitter
            if (is.null(.mojor_state$current_nrow_var_map)) {
                .mojor_state$current_nrow_var_map <- list()
            }
            .mojor_state$current_nrow_var_map[[nm]] <- nrow_var
            if (is.null(nrow_var_map)) {
                nrow_var_map <- list()
            }
            nrow_var_map[[nm]] <- nrow_var
            if (is.null(.mojor_state$current_ncol_var_map)) {
                .mojor_state$current_ncol_var_map <- list()
            }
            .mojor_state$current_ncol_var_map[[nm]] <- ncol_var
            if (is.null(ncol_var_map)) {
                ncol_var_map <- list()
            }
            ncol_var_map[[nm]] <- ncol_var
        }
    }

 # Step 5.1 Stage B: Emit length variables for local vector
 # allocations
    if (length(.mojor_state$current_local_vector_lengths) >
        0) {
        for (nm in names(.mojor_state$current_local_vector_lengths)) {
            vec_info <- .mojor_state$current_local_vector_lengths[[nm]]
            len_var <- paste0("length_", nm)
            vec_len_expr <- vec_info$length_alloc
            if (is.null(vec_len_expr)) {
                vec_len_expr <- vec_info$length
            }
            if (is.null(vec_len_expr)) {
                vec_len_expr <- 0L
            }

 # Resolve length() calls within the expression to Mojo
 # length variables
            resolved_expr <- resolve_length_in_expr(vec_len_expr)
            mojo_lines <- c(
                mojo_lines, paste0("    var ", len_var, " = Int(", resolved_expr, ")")
            )

 # Populate the length var map for bounds
 # checking/assignment emission
            if (is.null(.mojor_state$current_length_var_map)) {
                .mojor_state$current_length_var_map <- list()
            }
            .mojor_state$current_length_var_map[[nm]] <- len_var
 # Also add to len_var_map for bounds checking
            if (is.null(len_var_map))
                len_var_map <- list()
            len_var_map[[nm]] <- len_var

 # Emit hoisted alloc for local vectors (except the output buffer, which is wrapper-owned)
            should_hoist_local <- identical(vec_info$scope, "loop_body") ||
                identical(vec_info$origin, "subset")
            if (!identical(nm, out_name) && isTRUE(should_hoist_local)) {
                alloc_type <- if (vec_info$type %in% c("i32[]", "lgl[]")) "Int32" else "Float64"
                fill_val <- if (alloc_type == "Int32") "Int32(0)" else "0.0"
                init_var <- paste0("__mojor_init_", nm, "_j")
                mojo_lines <- c(
                    mojo_lines,
                    paste0("    var ", nm, " = alloc[", alloc_type, "](", len_var, ")"),
                    paste0("    for ", init_var, " in range(0, ", len_var, "):"),
                    paste0("      ", nm, "[", init_var, "] = ", fill_val)
                )
 # Register in len_var_map for emission
                if (is.null(.mojor_state$current_len_var_map)) {
                    .mojor_state$current_len_var_map <- list()
                }
                .mojor_state$current_len_var_map[[nm]] <- len_var
            }
        }
    }
    # Emit local chr vars as pre-computed Int32 position arrays
    local_chr_info <- .mojor_state$current_chr_index_local_vars
    if (!is.null(local_chr_info) && length(local_chr_info$vars) > 0) {
        for (nm in names(local_chr_info$vars)) {
            chr_strs <- local_chr_info$vars[[nm]]
            tgt <- local_chr_info$target_info[[nm]]
            positions <- NULL
            if (!is.null(tgt)) {
                dn_map <- .mojor_state$matrix_dimnames
                if (!is.null(dn_map) && !is.null(dn_map[[tgt$target]])) {
                    dn <- dn_map[[tgt$target]]
                    dim_names <- if (!is.null(dn$dim_names)) dn$dim_names
                                 else list(dn$row_names, dn$col_names)
                    if (tgt$dim >= 1L && tgt$dim <= length(dim_names)) {
                        axis_names <- dim_names[[tgt$dim]]
                        if (!is.null(axis_names)) {
                            positions <- match(chr_strs, axis_names)
                            bad <- which(is.na(positions))
                            if (length(bad) > 0L) {
                                stop(sprintf(
                                    "mojor_transpile: local chr var '%s' value '%s' not found in dimnames for '%s' (dim %d)",
                                    nm, chr_strs[[bad[[1L]]]], tgt$target, tgt$dim
                                ))
                            }
                        }
                    }
                }
            }
            if (!is.null(positions)) {
                chr_len_var <- paste0("n_", nm, "_i")
                mojo_lines <- c(
                    mojo_lines,
                    paste0("    var ", chr_len_var, " = Int(", length(positions), ")"),
                    paste0("    var ", nm, " = alloc[Int32](", length(positions), ")")
                )
                for (pi in seq_along(positions)) {
                    mojo_lines <- c(mojo_lines,
                        paste0("    ", nm, "[", pi - 1L, "] = Int32(", positions[[pi]], ")"))
                }
                if (is.null(len_var_map)) len_var_map <- list()
                len_var_map[[nm]] <- chr_len_var
                if (is.null(.mojor_state$current_len_var_map)) {
                    .mojor_state$current_len_var_map <- list()
                }
                .mojor_state$current_len_var_map[[nm]] <- chr_len_var
            }
        }
    }
    if (length(.mojor_state$current_local_array_dims) >
        0) {
        for (nm in names(.mojor_state$current_local_array_dims)) {
            dims <- .mojor_state$current_local_array_dims[[nm]]
            dim_var <- paste0("dim_", nm)
 # Emit dim array <U+2014> resolve length(name) to Mojo
 # length vars
            dim_exprs <- vapply(
                seq_along(dims$dim),
                function(i) {
                  paste0(
                    "Int(", resolve_length_in_expr(dims$dim[[i]]),
                    ")"
                )
                }, character(1)
            )
            mojo_lines <- c(
                mojo_lines, paste0(
                  "    var ", dim_var, " = [", paste(dim_exprs, collapse = ", "),
                  "]"
              )
            )
 # Populate the dim var map
            if (is.null(.mojor_state$current_dim_var_map)) {
                .mojor_state$current_dim_var_map <- list()
            }
            .mojor_state$current_dim_var_map[[nm]] <- dim_var
            if (is.null(dim_var_map)) {
                dim_var_map <- list()
            }
            dim_var_map[[nm]] <- dim_var
        }
    }
    if (isTRUE(.mojor_state$needs_mojo_random)) {
        mojo_lines <- c(
            mojo_lines, "    var __mojor_rng_state: MutU64Ptr = __mojor_rng_state_ptr.bitcast[UInt64]()"
        )
        if (isTRUE(.mojor_state$needs_rng_tables)) {
            mojo_lines <- c(
                mojo_lines, "    var __mojor_rng_ki = materialize[_KI_DOUBLE]()",
                "    var __mojor_rng_wi = materialize[_WI_DOUBLE]()", "    var __mojor_rng_fi = materialize[_FI_DOUBLE]()"
            )
        }
    }
    if (out_kind == "vector") {
        out_ty <- .mojor_mojo_type(out_type)
        if (is.null(out_ty))
            out_ty <- "Float64"
        if (isTRUE(out_alias_input)) {
            copy_len_var <- if (!is.null(len_var_map) && out_name %in% names(len_var_map)) {
                len_var_map[[out_name]]
            } else {
                "n_i"
            }
            mojo_lines <- c(
                mojo_lines, paste0(
                    "    var ", out_name, ": Mut", out_ty, "Ptr = ", out_ptr_param,
                    ".bitcast[", out_ty, "]()"
                ),
                "    var __mojor_alias_copy_i = 0",
                paste0("    while __mojor_alias_copy_i < Int(", copy_len_var, "):"),
                paste0("        ", out_name, "[__mojor_alias_copy_i] = ", out_name, "_in[__mojor_alias_copy_i]"),
                "        __mojor_alias_copy_i += 1"
            )
        } else {
            mojo_lines <- c(
                mojo_lines, paste0(
                    "    var ", out_name, ": Mut", out_ty, "Ptr = ", out_ptr_param,
                    ".bitcast[", out_ty, "]()"
                )
            )
        }
        if (!is.null(out_alias_source) && nzchar(out_alias_source)) {
            copy_len_var <- if (!is.null(len_var_map) && out_alias_source %in% names(len_var_map)) {
                len_var_map[[out_alias_source]]
            } else {
                "n_i"
            }
            mojo_lines <- c(
                mojo_lines,
                "    var __mojor_alias_copy_i = 0",
                paste0("    while __mojor_alias_copy_i < Int(", copy_len_var, "):"),
                paste0("        ", out_name, "[__mojor_alias_copy_i] = ", out_alias_source, "[__mojor_alias_copy_i]"),
                "        __mojor_alias_copy_i += 1"
            )
        }
 # Stage B: Memory safety - validate output pointer +
 # length
        if (isTRUE(.mojor_state$memory_check)) {
            out_check_fn <- switch(
                out_type, f64 = , `f64[]` = "mojor_check_ptr_f64_mut",
                f32 = , `f32[]` = "mojor_check_ptr_f32_mut", i32 = , `i32[]` = "mojor_check_ptr_i32_mut",
                lgl = , `lgl[]` = "mojor_check_ptr_i32_mut", NULL
            )
            if (!is.null(out_check_fn)) {
                source_file <- if (!is.null(.mojor_state$source_file))
                  .mojor_state$source_file else "kernel.R"
                mojo_lines <- c(
                  mojo_lines, paste0(
                    "    ", out_check_fn, "(", out_name, ", n_i, \"", out_name,
                    "\", \"", source_file, "\", 1)"
                )
              )
            }
        }
        if (!is.null(assume_aligned)) {
            mojo_lines <- c(mojo_lines, paste0("    var ", out_name, "_aligned = True"))
        }
    } else {
        out_ty <- .mojor_mojo_type(scalar_type)
        if (is.list(scalar_init_value) &&
            identical(scalar_init_value$kind, "expr") &&
            !is.null(scalar_init_value$expr)) {
            expr_ast <- scalar_init_value$expr
            if (.mojor_is_logical(scalar_type)) {
                cond_mojo <- .mojor_expr_to_mojo(
                  expr_ast, character(0),
                  local_types, in_cond = TRUE
              )
                init_val <- paste0("(1 if ", cond_mojo, " else 0)")
            } else {
                init_val <- NULL
                if (scalar_type %in% c("i32", "Int32", "Int")) {
                    init_val <- tryCatch(
                        .mojor_scalar_range_expr_to_mojo(
                            expr_ast,
                            loop_arg_specs,
                            loop_args,
                            len_var_map,
                            n_source_name,
                            scalar_inits = scalar_inits,
                            nrow_var_map = nrow_var_map,
                            ncol_var_map = ncol_var_map,
                            dim_var_map = dim_var_map,
                            allow_scalar_cast = TRUE
                        ),
                        error = function(e) NULL
                    )
                }
                if (is.null(init_val)) {
                    init_val <- .mojor_expr_to_mojo(
                        expr_ast, character(0),
                        local_types,
                        in_cond = FALSE,
                        index_context = scalar_type %in% c("i32", "Int32", "Int")
                    )
                }
            }
        } else {
            init_val <- if (!is.null(scalar_init_value))
                as.character(scalar_init_value) else "0"
        }
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", scalar_init, "_out: Mut", out_ty, "Ptr = ",
                scalar_init, "_ptr.bitcast[", out_ty, "]()"
            )
        )
        mojo_lines <- c(
            mojo_lines, paste0("    var ", scalar_init, ": ", out_ty, " = ", init_val)
        )
    }
    scalar_decl_names <- names(scalar_inits)
    if (!is.null(scalar_init) &&
        out_kind == "scalar") {
        scalar_decl_names <- setdiff(scalar_decl_names, scalar_init)
    }
    if (length(scalar_decl_names) >
        0) {
        for (nm in scalar_decl_names) {
            spec <- scalar_inits[[nm]]$type
            ty <- .mojor_mojo_type(spec)
            init_val <- scalar_inits[[nm]]$value
            if (is.list(init_val) &&
                identical(init_val$kind, "expr") &&
                !is.null(init_val$expr)) {
                if (spec %in% c("lgl", "bool")) {
                  cond_mojo <- .mojor_expr_to_mojo(
                    init_val$expr, character(0),
                    local_types, in_cond = TRUE
                )
                  init_val <- paste0("(1 if ", cond_mojo, " else 0)")
                } else {
                  expr_ast <- init_val$expr
                  init_val <- NULL
                  if (spec %in% c("i32", "Int32", "Int")) {
                    init_val <- tryCatch(
                      .mojor_scalar_range_expr_to_mojo(
                        expr_ast,
                        loop_arg_specs,
                        loop_args,
                        len_var_map,
                        n_source_name,
                        scalar_inits = scalar_inits,
                        nrow_var_map = nrow_var_map,
                        ncol_var_map = ncol_var_map,
                        dim_var_map = dim_var_map,
                        allow_scalar_cast = TRUE
                      ),
                      error = function(e) NULL
                    )
                  }
                  if (is.null(init_val)) {
                    init_val <- .mojor_expr_to_mojo(
                      expr_ast, character(0),
                      local_types,
                      in_cond = FALSE,
                      index_context = spec %in% c("i32", "Int32", "Int")
                    )
                  }
                }
            } else if (spec %in% c("lgl", "bool")) {
                init_val <- if (isTRUE(init_val))
                  1L else 0L
            }
            mojo_lines <- c(
                mojo_lines, paste0("    var ", nm, ": ", ty, " = ", as.character(init_val))
            )
        }
    }

    prev_n_source_name <- .mojor_state$current_n_source_name
    prev_len_var_map <- .mojor_state$current_len_var_map
    prev_nrow_var_map <- .mojor_state$current_nrow_var_map
    prev_dim_var_map <- .mojor_state$current_dim_var_map
    prev_ndim_var_map <- .mojor_state$current_ndim_var_map
    prev_ncol_var_map <- .mojor_state$current_ncol_var_map
    prev_local_matrix_dims <- .mojor_state$current_local_matrix_dims
    prev_local_array_dims <- .mojor_state$current_local_array_dims
    prev_local_vector_lengths <- .mojor_state$current_local_vector_lengths
    prev_out_nrow_var <- .mojor_state$current_out_nrow_var
    prev_out_ncol_var <- .mojor_state$current_out_ncol_var
    prev_out_dim_var <- .mojor_state$current_out_dim_var
    prev_out_ndim_var <- .mojor_state$current_out_ndim_var
    prev_out_name <- .mojor_state$current_out_name
 # Step 8.11: Save matrix/array output info
    prev_out_is_matrix <- .mojor_state$current_out_is_matrix
    prev_out_is_array <- .mojor_state$current_out_is_array
    prev_out_nrow_expr <- .mojor_state$current_out_nrow_expr
    prev_out_ncol_expr <- .mojor_state$current_out_ncol_expr
    prev_out_dim_exprs <- .mojor_state$current_out_dim_exprs
    prev_len_hint <- .mojor_state$current_len_hint
    prev_tensor_map <- .mojor_state$current_tensor_map
    prev_tensor_ranks <- .mojor_state$current_tensor_ranks
    .mojor_state$current_n_source_name <- n_source_name
    .mojor_state$current_len_var_map <- len_var_map
    .mojor_state$current_nrow_var_map <- nrow_var_map
    .mojor_state$current_dim_var_map <- dim_var_map
    .mojor_state$current_ndim_var_map <- ndim_var_map
    .mojor_state$current_ncol_var_map <- ncol_var_map
    .mojor_state$current_local_matrix_dims <- if (is.null(prev_local_matrix_dims))
        list() else prev_local_matrix_dims
    .mojor_state$current_local_array_dims <- if (is.null(prev_local_array_dims))
        list() else prev_local_array_dims
    .mojor_state$current_local_vector_lengths <- if (is.null(prev_local_vector_lengths))
        list() else prev_local_vector_lengths
    .mojor_state$current_out_nrow_var <- if (isTRUE(out_matrix))
        .mojor_out_nrow_var_name() else NULL
    .mojor_state$current_out_ncol_var <- if (isTRUE(out_matrix))
        .mojor_out_ncol_var_name() else NULL
    .mojor_state$current_out_dim_var <- if (isTRUE(out_array) ||
        isTRUE(broadcast_nd))
        .mojor_out_dim_var_name() else NULL
    .mojor_state$current_out_ndim_var <- if (isTRUE(out_array) ||
        isTRUE(broadcast_nd))
        .mojor_out_ndim_var_name() else NULL
    .mojor_state$current_out_name <- out_name
 # Step 8.11: Store matrix/array output info for IR emission
    .mojor_state$current_out_is_matrix <- isTRUE(out_matrix)
    .mojor_state$current_out_is_array <- isTRUE(out_array)
    .mojor_state$current_out_nrow_expr <- if (isTRUE(out_matrix))
        out_nrow_expr else NULL
    .mojor_state$current_out_ncol_expr <- if (isTRUE(out_matrix))
        out_ncol_expr else NULL
    .mojor_state$current_out_dim_exprs <- if (isTRUE(out_array))
        out_dim_exprs else NULL
    .mojor_state$current_len_hint <- list()
    .mojor_state$current_iter_map <- list()
    .mojor_state$current_tensor_map <- if (length(tensor_ranks) >
        0) {
        as.list(
            setNames(
                paste0("__mojor_tensor_", names(tensor_ranks)),
                names(tensor_ranks)
            )
        )
    } else {
        list()
    }
    .mojor_state$current_tensor_ranks <- tensor_ranks
    on.exit(
        {
            .mojor_state$current_n_source_name <- prev_n_source_name
            .mojor_state$current_len_var_map <- prev_len_var_map
            .mojor_state$current_nrow_var_map <- prev_nrow_var_map
            .mojor_state$current_dim_var_map <- prev_dim_var_map
            .mojor_state$current_ndim_var_map <- prev_ndim_var_map
            .mojor_state$current_ncol_var_map <- prev_ncol_var_map
            .mojor_state$current_local_matrix_dims <- prev_local_matrix_dims
            .mojor_state$current_local_array_dims <- prev_local_array_dims
            .mojor_state$current_local_vector_lengths <- prev_local_vector_lengths
            .mojor_state$current_out_nrow_var <- prev_out_nrow_var
            .mojor_state$current_out_ncol_var <- prev_out_ncol_var
            .mojor_state$current_out_dim_var <- prev_out_dim_var
            .mojor_state$current_out_ndim_var <- prev_out_ndim_var
            .mojor_state$current_out_name <- prev_out_name
 # Step 8.11: Restore matrix/array output info
            .mojor_state$current_out_is_matrix <- prev_out_is_matrix
            .mojor_state$current_out_is_array <- prev_out_is_array
            .mojor_state$current_out_nrow_expr <- prev_out_nrow_expr
            .mojor_state$current_out_ncol_expr <- prev_out_ncol_expr
            .mojor_state$current_out_dim_exprs <- prev_out_dim_exprs
            .mojor_state$current_len_hint <- prev_len_hint
            .mojor_state$current_tensor_map <- prev_tensor_map
            .mojor_state$current_tensor_ranks <- prev_tensor_ranks
        }, add = TRUE
    )

    if (length(tensor_ranks) >
        0) {
        tensor_setup_lines <- character(0)
        for (nm in names(tensor_ranks)) {
            rank <- tensor_ranks[[nm]]
            if (rank < 2)
                next
            layout_name <- paste0(nm, "_layout")
            tensor_name <- paste0("__mojor_tensor_", nm)
            dims_exprs <- NULL
            mut_flag <- "False"
            ty <- NULL
            is_output_tensor_target <- !is.null(out_name) && nm == out_name
            uses_output_dims <- is_output_tensor_target &&
                (isTRUE(out_matrix) || isTRUE(out_array) || isTRUE(broadcast_nd))
            if (uses_output_dims) {
 # Step 8.11: Skip output matrices in IR mode (IR
 # emits its own wrapper)
                if (TRUE && rank == 2 && isTRUE(out_matrix)) {
                  next
                }
                mut_flag <- "True"
                ty <- .mojor_mojo_type(out_type)
                if (isTRUE(out_matrix) &&
                  rank == 2 && !is.null(.mojor_state$current_out_nrow_var)) {
                  dims_exprs <- c(.mojor_state$current_out_nrow_var, .mojor_state$current_out_ncol_var)
                } else if ((isTRUE(out_array) ||
                  isTRUE(broadcast_nd)) &&
                  !is.null(.mojor_state$current_out_dim_var)) {
                  dims_exprs <- vapply(
                    seq_len(rank),
                    function(i) {
                      paste0(
                        "Int(", .mojor_state$current_out_dim_var, "[",
                        i - 1L, "])"
                    )
                    }, character(1)
                )
                }
            } else {
                if (is_output_tensor_target) {
                    mut_flag <- "True"
                }
                spec <- arg_specs[[nm]]
                if (!is.null(spec) &&
                  .mojor_is_array(spec)) {
                  ty <- .mojor_mojo_type(spec)
                }
                if (!is.null(dim_var_map) &&
                  !is.null(dim_var_map[[nm]]) &&
                  rank >= 2) {
                  dim_var <- dim_var_map[[nm]]
                  dims_exprs <- vapply(
                    seq_len(rank),
                    function(i) {
                      paste0("Int(", dim_var, "[", i - 1L, "])")
                    }, character(1)
                )
                } else if (rank == 2 && !is.null(nrow_var_map) &&
                  !is.null(nrow_var_map[[nm]])) {
                  nrow_var <- nrow_var_map[[nm]]
                  ncol_var <- paste0("ncol_", nm, "_i")
                  len_expr <- NULL
                  if (!is.null(len_var_map) &&
                    !is.null(len_var_map[[nm]])) {
                    len_expr <- len_var_map[[nm]]
                  } else if (!is.null(n_source_name) &&
                    identical(nm, n_source_name)) {
                    len_expr <- "n_i"
                  } else {
                    len_expr <- "n_i"
                  }
                  tensor_setup_lines <- c(
                    tensor_setup_lines, paste0("    var ", ncol_var, ": Int = 0"),
                    paste0("    if ", nrow_var, " > 0:"),
                    paste0(
                      "        ", ncol_var, " = Int(", len_expr, " / ",
                      nrow_var, ")"
                  )
                )
                  .mojor_state$current_ncol_var_map[[nm]] <- ncol_var
                  dims_exprs <- c(nrow_var, ncol_var)
                }
            }
            var_dtype <- NULL
            if (nm == out_name && !is.null(out_type)) {
                var_dtype <- .mojor_mojo_dtype(out_type)
            } else if (!is.null(arg_specs[[nm]])) {
                var_dtype <- .mojor_mojo_dtype(arg_specs[[nm]])
            } else if (!is.null(ty)) {
                if (identical(ty, "Float64"))
                  var_dtype <- "DType.float64"
                if (identical(ty, "Float32"))
                  var_dtype <- "DType.float32"
                if (identical(ty, "Int32"))
                  var_dtype <- "DType.int32"
            }
            if (is.null(dims_exprs) ||
                is.null(var_dtype))
                next
            index_list <- paste0(
                "IndexList[", rank, "](", paste(dims_exprs, collapse = ", "),
                ")"
            )
            tensor_setup_lines <- c(
                tensor_setup_lines, paste0(
                  "    var ", layout_name, " = ", layout_runtime(
                    paste0("_MOJOR_LAYOUT_", nm),
                    index_list
                )
              ),
                paste0(
                  "    var ", tensor_name, " = LayoutTensor[mut=", mut_flag,
                  ", ", var_dtype, ", _MOJOR_LAYOUT_", nm, ", MutAnyOrigin](",
                  nm, ", ", layout_name, ")"
              )
            )
 # Track tensor mutability for IR emission (needed for
 # .item() extraction)
            if (is.null(.mojor_state$current_tensor_mut_map)) {
                .mojor_state$current_tensor_mut_map <- list()
            }
            .mojor_state$current_tensor_mut_map[[nm]] <- mut_flag
        }
        if (length(tensor_setup_lines) >
            0) {
            mojo_lines <- c(mojo_lines, tensor_setup_lines)
        }
    }

    if (length(pre_loop_out_assigns) >
        0 && !is.null(out_name)) {
        if (isTRUE(.mojor_state$current_out_is_matrix) &&
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
    }

    if (length(pre_loop_out_assigns) >
        0 || length(post_loop_out_assigns) >
        0) {
        layout_ctx <- .mojor_ir_layout_ctx(
            n_var = "n_i", nrow_var = .mojor_state$current_out_nrow_var,
            ncol_var = .mojor_state$current_out_ncol_var, dim_var_map = .mojor_state$current_dim_var_map,
            ndim_var_map = .mojor_state$current_ndim_var_map, nrow_var_map = .mojor_state$current_nrow_var_map,
            ncol_var_map = .mojor_state$current_ncol_var_map, len_var_map = .mojor_state$current_len_var_map,
            tensor_map = .mojor_state$current_tensor_map, index_base = .mojor_state$current_index_base,
            array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
            fusion_allow_broadcast_nd_identity = isTRUE(.mojor_state$options$fusion_allow_broadcast_nd_identity)
        )
        layout_ctx$gpu_jit_mode <- gpu_jit_mode
            layout_ctx$n_source_name <- .mojor_state$current_n_source_name
            layout_ctx$type_env <- local_types
            layout_ctx$reduction_mode <- reduction  # Step 25: Pass reduction mode to lowering
            layout_ctx$out_name <- out_name
            layout_ctx$chr_index_hashed_args <- .mojor_state$current_chr_index_hashed_args
            top_level_type_env <- local_types
        strict_ir <- isTRUE(.mojor_state$options$ir_only)
        emit_top_level_stmt <- function(stmt_expr, phase_name = "top-level") {
            ir_build_res <- tryCatch(
                .mojor_ir_build_stmt(stmt_expr),
                error = function(e) e
            )
            if (inherits(ir_build_res, "error")) {
                if (isTRUE(strict_ir)) {
                  stop(
                    "mojor_transpile: strict IR build failed: ", conditionMessage(ir_build_res),
                    call. = FALSE
                )
                }
                stop(ir_build_res)
            }
            ir <- ir_build_res
            ir <- .mojor_ir_normalize(ir)
            ir <- .mojor_ir_fold_type_stmt(ir, top_level_type_env)
            ir <- .mojor_ir_lower_dim_stmt(ir, dim_map)
            layout_ctx$len_var_map <- .mojor_state$current_len_var_map
            layout_ctx$type_env <- top_level_type_env
            .mojor_ir_verify(
                ir, ctx = list(
                  ir_only = isTRUE(.mojor_state$options$ir_only),
                  type_env = top_level_type_env
              )
            )
            ir <- .mojor_ir_optimize(ir, opt_level = opt_level)
            ir <- .mojor_ir_lower(ir, ctx = layout_ctx)
            schedule$type_env <- top_level_type_env
            ir <- .mojor_ir_schedule(ir, schedule = schedule)
            emit_res <- .mojor_ir_service_type_and_emit_stmt(
                ir = ir, local_types = top_level_type_env, out_name = out_name,
                na_mode = na_mode, bounds_check = isTRUE(.mojor_state$options$index_bounds),
                scalar_name = scalar_init, schedule = schedule, loop_var = NULL,
                unroll = NULL, indent = "    ", zero_based_vars = character(0)
            )
            typed_stmt <- emit_res$ir_typed
            if (!is.null(typed_stmt) && identical(typed_stmt$kind, "assign") &&
              !is.null(typed_stmt$lhs) && identical(typed_stmt$lhs$kind, "var")) {
                lhs_name <- typed_stmt$lhs$name
                rhs_type <- .mojor_ir_infer_type(typed_stmt$rhs, top_level_type_env)
                if (!is.null(lhs_name) && nzchar(lhs_name) &&
                  !is.null(rhs_type) && rhs_type != "unknown") {
                    top_level_type_env[[lhs_name]] <<- rhs_type
                }
            }
            emit_res$mojo_lines
        }
        for (s in pre_loop_out_assigns) {
            stmt_lines <- emit_top_level_stmt(s, phase_name = "pre-loop")
            if (is.null(stmt_lines) ||
                length(stmt_lines) ==
                  0 || any(!nzchar(stmt_lines))) {
                .mojor_err(
                  "IR failed to emit pre-loop statement", s, "simplify the statement or move it inside the loop"
              )
            }
            mojo_lines <- c(mojo_lines, stmt_lines)
        }
    }

    .mojor_emit_transpile_loop_codegen(environment())

    if (length(post_loop_out_assigns) >
        0) {
        for (s in post_loop_out_assigns) {
            stmt_lines <- emit_top_level_stmt(s, phase_name = "post-loop")
            if (is.null(stmt_lines) ||
                length(stmt_lines) ==
                  0 || any(!nzchar(stmt_lines))) {
                .mojor_err(
                  "IR failed to emit post-loop statement", s, "simplify the statement"
              )
            }
            mojo_lines <- c(mojo_lines, stmt_lines)
        }
    }

    .mojor_finalize_transpile_pipeline(environment())
}
