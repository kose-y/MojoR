# =============================================================================
# IR Statement Loop Emit
# =============================================================================

.mojor_dtype_to_scalar_ty <- function(dtype) {
    if (dtype %in% c("DType.float32", "Float32")) "Float32"
    else if (dtype %in% c("DType.int32", "Int32")) "Int32"
    else "Float64"
}

.mojor_ir_loop_emit <- function(
    node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid",
    bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL,
    unroll = NULL, schedule = NULL
) {
 # Step 4.4: Added loop_var parameter for guard optimization
 # Step 5.1: Added type_env parameter for logical array support
 # Step 8.12: Added unroll parameter for loop unrolling
 # optimization
    if (is.null(node) ||
        node$kind != "loop") {
        return(NULL)
    }
    if (is.null(zero_based_vars))
        zero_based_vars <- character(0)

 # Elementwise loop emission (IR-marked)
    if (!is.null(node$metadata) &&
        !is.null(node$metadata$elementwise)) {
        ew_lines <- .mojor_ir_elementwise_emit(
            node, node$metadata$elementwise, indent, zero_based_vars, out_name,
            na_guard, bounds_check, loop_var, scalar_name, type_env, schedule
        )
        if (!is.null(ew_lines) &&
            length(ew_lines) >
                0) {
            return(ew_lines)
        }
    }

 # Try to emit the range expression
    range_info <- .mojor_ir_simple_range_emit(
        node$range,
        type_env,
        zero_based_vars = zero_based_vars
    )
    if (is.null(range_info))
        {
            return(NULL)
        }  # Fallback for complex ranges

 # PR-B5 Step 2: Scan for cumulative operations and prepare
 # accumulators PR-B5 Step 4: Also reset pending updates and node
 # map for expression-level cumulative ops Reset accumulator state
 # for this loop
    .mojor_state$current_cumul_acc_counter <- 0
    .mojor_state$current_cumul_accs <- list()
    .mojor_state$current_cumul_updates <- list()
    .mojor_state$current_cumul_node_map <- list()

 # Emit loop header
    loop_header <- paste0(indent, "for ", node$var, " in ", range_info$range, ":")

 # Determine if loop variable should be added to zero_based_vars
 # For R-style 1-based loops (range starting at 1), the loop
 # variable is NOT zero-based So we don't add it to
 # zero_based_vars
    inner_zero_based <- zero_based_vars
 # If the range emitter indicates zero-based (e.g., range(0, n) or
 # explicit 0-start), add loop var to zero_based_vars to skip
 # index normalization
    if (isTRUE(range_info$loop_var_is_zero_based)) {
        inner_zero_based <- c(inner_zero_based, node$var)
    }

 # Step 4.4/5.1: Pass loop variables for nested statements (outer
 # + current)
    inner_loop_vars <- node$var
    if (!is.null(loop_var))
        inner_loop_vars <- c(loop_var, node$var)

    hoisted_guard_lines <- character(0)
    hoisted_bounds_cache <- NULL
    hoisted_na_cache <- NULL
    can_hoist_guards <- isTRUE(range_info$is_simple_1_based) &&
        !is.null(range_info$start) &&
        !is.null(range_info$end) &&
        is.character(range_info$range) &&
        !grepl(" if ", range_info$range, fixed = TRUE)
    if (can_hoist_guards) {
        hoisted <- .mojor_ir_loop_hoist_guards(
            node, indent = paste0(indent, "    "),
            zero_based_vars = inner_zero_based, na_guard = na_guard, bounds_check = bounds_check,
            type_env = type_env
        )
        if (length(hoisted$lines) >
            0) {
            nonempty_cond <- paste0(
                "(Int(", range_info$start, ") <= Int(", range_info$end,
                "))"
            )
            hoisted_guard_lines <- c(
                paste0(indent, "if ", nonempty_cond, ":"),
                hoisted$lines
            )
            hoisted_bounds_cache <- hoisted$bounds_cache
            hoisted_na_cache <- hoisted$na_cache
        }
    }

 # Emit loop body
    body_indent <- paste0(indent, "    ")

 # Add trace instrumentation if trace mode is enabled
    trace_lines <- character(0)
    if (isTRUE(.mojor_state$trace_mode)) {
 # Add trace statement showing loop iteration
        trace_lines <- c(
            trace_lines, paste0(
                body_indent, "mojor_trace_value_int(\"", node$var, "\", ",
                node$var, ")"
            )
        )
    }

 # Step 5: Increment line estimate for loop body
 # Estimate: loop header + body statement = approximately 2-3
 # lines in R
    if (!is.null(.mojor_state$current_line_estimate)) {
        .mojor_state$current_line_estimate <- .mojor_state$current_line_estimate +
            2
    }

    red_info <- if (!is.null(node$reduce))
        node$reduce else node$metadata$reduction

    body_lines <- .mojor_ir_block_emit(
        node$body, body_indent, inner_zero_based, out_name, na_guard, bounds_check,
        inner_loop_vars, scalar_name, type_env, schedule, bounds_guard_cache = hoisted_bounds_cache,
        na_guard_cache = hoisted_na_cache
    )
    if (is.null(body_lines))
        {
            return(NULL)
        }  # Fallback if body can't be emitted
    if (length(body_lines) ==
        0) {
        fallback_line <- NULL
        if (!is.null(red_info) &&
            !is.null(red_info$kind) &&
            !is.null(red_info$acc) &&
            !is.null(red_info$rhs)) {
            rhs_str <- .mojor_ir_expr_emit(
                red_info$rhs, inner_zero_based, type_env, loop_vars = inner_loop_vars
            )
            if (!is.null(rhs_str) &&
                rhs_str != "") {
                if (red_info$kind %in% c("sum", "product")) {
                  op <- if (red_info$kind == "sum")
                    "+" else "*"
                  fallback_line <- paste0(
                    body_indent, red_info$acc, " = ", red_info$acc, " ",
                    op, " ", rhs_str
                )
                } else if (red_info$kind %in% c("min", "max")) {
                  fallback_line <- paste0(
                    body_indent, red_info$acc, " = ", red_info$kind, "(",
                    red_info$acc, ", ", rhs_str, ")"
                )
                }
            }
        }
        if (is.null(fallback_line))
            fallback_line <- paste0(body_indent, "pass")
        body_lines <- fallback_line
    }

 # Prepend trace lines to body
    if (length(trace_lines) >
        0) {
        body_lines <- c(trace_lines, body_lines)
    }

    comment_lines <- character(0)
    if (!is.null(red_info)) {
        red <- red_info
        rhs_str <- .mojor_ir_expr_emit(red$rhs, inner_zero_based, type_env, loop_vars = inner_loop_vars)
        if (is.null(rhs_str) ||
            rhs_str == "")
            rhs_str <- "<expr>"
        if (red$kind %in% c("sum", "product")) {
            op <- if (red$kind == "sum")
                "+" else "*"
            comment_lines <- c(
                comment_lines, paste0(
                  indent, "# IR reduction: ", red$kind, " ", red$acc, " <- ",
                  red$acc, " ", op, " ", rhs_str
              )
            )
        } else if (red$kind %in% c("min", "max")) {
            comment_lines <- c(
                comment_lines, paste0(
                  indent, "# IR reduction: ", red$kind, " ", red$acc, " <- ",
                  red$kind, "(", red$acc, ", ", rhs_str, ")"
              )
            )
        }
    }

    effective_schedule <- .mojor_ir_schedule_merge(node$schedule, schedule)
    reduction_mode <- effective_schedule$reduction
    reduction_sched_ok <- is.character(reduction_mode) &&
        length(reduction_mode) == 1L &&
        !is.na(reduction_mode) &&
        reduction_mode %in% c("tree", "simd")
    start_one <- FALSE
    if (!is.null(range_info$start) &&
        is.character(range_info$start) &&
        length(range_info$start) ==
            1) {
        start_norm <- gsub("\\s+", "", range_info$start)
        start_one <- identical(start_norm, "1") ||
            identical(start_norm, "Int(1)")
    }
    if (!is.null(red_info) &&
        isTRUE(reduction_sched_ok) &&
        identical(na_guard, "unsafe") &&
        !isTRUE(bounds_check) &&
        isTRUE(range_info$is_simple_1_based) &&
        isTRUE(start_one) &&
        !is.null(range_info$end)) {
 # Schedule inner-loop reductions only for simple safe
 # patterns.
        red <- red_info
        if (!is.null(red$kind) &&
            red$kind %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
            red_arg <- .mojor_ir_reduction_rhs_array_var(red$rhs, node$var)
            if (!is.null(red_arg)) {
                is_which <- red$kind %in% c("which.min", "which.max")
                red_node <- .mojor_ir_scalar_reduce(
                  op = red$kind, acc = red$acc, arg = red_arg, associative = !is_which,
                  commutative = !is_which, na_rm = FALSE
              )
                red_ctx <- list(n_var = "__mojor_red_n", type_env = type_env)
                scheduled <- if (identical(reduction_mode, "tree")) {
                  .mojor_ir_schedule_tree_reduce(red_node, red_ctx)
                } else {
                  .mojor_ir_schedule_simd_reduce(red_node, red_ctx)
                }
                if (!is.null(scheduled)) {
                  scheduled_lines <- .mojor_ir_scheduled_reduce_emit(
                    scheduled, indent = indent, scalar_name = scalar_name,
                    type_env = type_env
                )
                  if (!is.null(scheduled_lines) &&
                    length(scheduled_lines) >
                      0) {
                    acc_mojo_ty <- .mojor_type_env_to_mojo_type(red$acc, type_env)
                    sched_mojo_ty <- if (!is.null(scheduled$dtype)) {
                      .mojor_dtype_to_scalar_ty(scheduled$dtype)
                    } else {
                      NULL
                    }
                    type_mismatch <- !is.null(acc_mojo_ty) &&
                      !is.null(sched_mojo_ty) &&
                      !identical(acc_mojo_ty, sched_mojo_ty)
                    if (isTRUE(type_mismatch)) {
                      scheduled_lines <- NULL
                    }
                    if (!is.null(scheduled_lines) &&
                      length(scheduled_lines) >
                        0) {
                    n_line <- paste0(
                      indent, "var __mojor_red_n = Int(", range_info$end,
                      ")"
                  )
                    return(
                      c(
                        comment_lines, hoisted_guard_lines,
                        n_line, scheduled_lines
                      )
                    )
                    }
                  }
                }
            }
        }
    }
    if (isTRUE(effective_schedule$vectorize)) {
        vec_lines <- NULL
        if (!isTRUE(bounds_check) &&
            !is.null(out_name) &&
            !is.null(type_env) &&
            !is.null(range_info$end) &&
            isTRUE(range_info$is_simple_1_based) &&
            is.null(node$reduce) &&
            is.null(node$metadata$reduction) &&
            !isTRUE(.mojor_state$current_out_is_matrix) &&
            !isTRUE(.mojor_state$current_out_is_array)) {
            out_type <- type_env[[out_name]]
            if (!is.null(out_type) &&
                out_type %in% c("f64[]", "f32[]")) {
                if (!is.null(node$body) &&
                  node$body$kind == "block" && length(node$body$stmts) ==
                  1) {
                  stmt <- node$body$stmts[[1]]
                  if (!is.null(stmt) &&
                    stmt$kind == "assign" && !.mojor_ir_needs_na_guard(stmt$rhs, na_guard)) {
                    lhs <- stmt$lhs
                    if (!is.null(lhs) &&
                      lhs$kind == "index" && !is.null(lhs$base) &&
                      lhs$base$kind == "var" && identical(lhs$base$name, out_name) &&
                      !is.null(lhs$indices) &&
                      length(lhs$indices) ==
                        1) {
                      idx <- lhs$indices[[1]]
                      if (!is.null(idx$kind) &&
                        idx$kind == "scalar_index")
                        idx <- idx$expr
                      if (!is.null(idx) &&
                        idx$kind == "var" && identical(idx$name, node$var)) {
                        vec <- .mojor_ir_expr_emit_vec_chunks(stmt$rhs, node$var, type_env)
                        if (!is.null(vec) &&
                          !is.null(vec$expr)) {
                          arrays_ok <- TRUE
                          if (length(vec$arrays) >
                            0) {
                            for (arr in vec$arrays) {
                              if (is.null(type_env[[arr]]) ||
                                type_env[[arr]] != out_type) {
                                arrays_ok <- FALSE
                                break
                              }
                            }
                          }
                          if (arrays_ok) {
                            width_var <- if (out_type == "f32[]")
                              "_MOJOR_ALIGN_F32" else "_MOJOR_ALIGN_F64"
                            simd_end_var <- "__mojor_simd_end"
                            simd_i <- "__mojor_simd_i"
                            vec_lines <- c(
                              paste0(
                                indent, "var ", simd_end_var, " = ", range_info$end,
                                " - (", range_info$end, " % ", width_var,
                                ")"
                            ),
                              paste0(
                                indent, "for ", simd_i, " in range(0, ",
                                simd_end_var, ", ", width_var, "):"
                            )
                          )
                            if (length(vec$arrays) >
                              0) {
                              for (arr in vec$arrays) {
                                vec_lines <- c(
                                  vec_lines, paste0(
                                    indent, "    var ", arr, "_chunk = ",
                                    arr, ".load[width=", width_var, ", alignment=_MOJOR_ALIGN](",
                                    simd_i, ")"
                                )
                              )
                              }
                            }
                            vec_lines <- c(
                              vec_lines, paste0(indent, "    var out_chunk = ", vec$expr),
                              paste0(
                                indent, "    ", out_name, ".store[width=",
                                width_var, ", alignment=_MOJOR_ALIGN](",
                                simd_i, ", out_chunk)"
                            ),
                              paste0(
                                indent, "for ", node$var, " in range(",
                                simd_end_var, " + 1, ", range_info$end,
                                " + 1):"
                            )
                          )
                            vec_lines <- c(vec_lines, body_lines)
                          }
                        }
                      }
                    }
                  }
                }
            }
        }
        if (!is.null(vec_lines)) {
            return(c(comment_lines, hoisted_guard_lines, vec_lines))
        }
    }
    if (is.null(unroll) &&
        !is.null(effective_schedule$unroll)) {
        unroll <- effective_schedule$unroll
    }

 # Step 8.12: Loop unrolling optimization Cumulative operations
 # carry loop dependencies via accumulator state. The unroll path
 # currently rewrites loop vars textually and is not safe for
 # cumulative updates; keep cumulative loops on the scalar path.
    cumul_accs <- .mojor_state$current_cumul_accs
    has_cumulative_accs <- !is.null(cumul_accs) &&
        length(cumul_accs) >
            0
    unroll_factor <- if (!is.null(unroll))
        as.integer(unroll) else 1
    if (unroll_factor > 1 && isTRUE(range_info$is_simple_1_based) &&
        !has_cumulative_accs) {
 # Only unroll simple 1-based integer ranges. Guard empty
 # ranges explicitly to avoid negative-span modulo behavior.
        lines <- c(comment_lines, hoisted_guard_lines)
        unroll_if_indent <- paste0(indent, "    ")
        unroll_body_indent <- paste0(unroll_if_indent, "    ")

        start_int <- paste0("Int(", range_info$start, ")")
        end_int <- paste0("Int(", range_info$end, ")")
        lines <- c(lines, paste0(indent, "if ", start_int, " <= ", end_int, ":"))
        lines <- c(
            lines, paste0(
                unroll_if_indent, "var _mojor_unroll_span = (", end_int,
                " - ", start_int, " + 1)"
            )
        )
        lines <- c(
            lines, paste0(
                unroll_if_indent, "var _mojor_unroll_main_count = _mojor_unroll_span - (_mojor_unroll_span % ",
                unroll_factor, ")"
            )
        )
        lines <- c(
            lines, paste0(
                unroll_if_indent, "var _mojor_unroll_tail_start = ", start_int,
                " + _mojor_unroll_main_count"
            )
        )
        lines <- c(
            lines, paste0(
                unroll_if_indent, "for _mojor_i in range(", start_int,
                ", _mojor_unroll_tail_start, ", unroll_factor, "):"
            )
        )

 # Emit unrolled iterations.
        for (offset in 0:(unroll_factor - 1)) {
            offset_expr <- if (offset == 0)
                "_mojor_i" else paste0("_mojor_i + ", offset)

            unroll_body <- .mojor_ir_block_emit(
                node$body, unroll_body_indent, inner_zero_based, out_name,
                na_guard, bounds_check, inner_loop_vars, scalar_name, type_env,
                schedule, bounds_guard_cache = hoisted_bounds_cache, na_guard_cache = hoisted_na_cache
            )
            if (is.null(unroll_body)) {
                return(NULL)
            }

 # NOTE (Issue 8): gsub-based substitution is brittle;
 # IR-level substitution is the long-term fix. Keep this
 # path constrained to optional unrolling.
            loop_var_pattern <- paste0("\\b", node$var, "\\b")
            unroll_body_substituted <- gsub(
                loop_var_pattern, paste0("(", offset_expr, ")"),
                unroll_body
            )

 # Keep each unrolled lane in its own scope block so temp
 # declarations emitted by statement lowering do not
 # collide across lanes.
            lane_guard <- paste0("__mojor_unroll_lane_guard_", offset)
            lines <- c(lines, paste0(unroll_body_indent, "var ", lane_guard, " = True"))
            lines <- c(lines, paste0(unroll_body_indent, "if ", lane_guard, ":"))
            lines <- c(lines, paste0("    ", unroll_body_substituted))
        }

 # Remainder loop.
        lines <- c(
            lines, paste0(
                unroll_if_indent, "for ", node$var, " in range(_mojor_unroll_tail_start, ",
                end_int, " + 1):"
            )
        )
        remainder_body <- .mojor_ir_block_emit(
            node$body, unroll_body_indent, inner_zero_based, out_name,
            na_guard, bounds_check, inner_loop_vars, scalar_name, type_env,
            schedule, bounds_guard_cache = hoisted_bounds_cache, na_guard_cache = hoisted_na_cache
        )
        if (is.null(remainder_body)) {
            return(NULL)
        }
        lines <- c(lines, remainder_body)

        return(lines)
    }

 # PR-B5 Step 2: Emit pre-loop accumulator initialization for
 # cumulative ops
    acc_init_lines <- character(0)
    if (has_cumulative_accs) {
        for (acc_info in cumul_accs) {
            acc_init_lines <- c(
                acc_init_lines, paste0(indent, "var ", acc_info$var, " = ", acc_info$init)
            )
        }
    }

 # Combine header and body
    c(
        comment_lines, hoisted_guard_lines, acc_init_lines, loop_header,
        body_lines
    )
}

.mojor_ir_scheduled_reduce_emit <- function(node, indent = "    ", scalar_name = NULL, type_env = NULL) {
    if (is.null(node) ||
        node$kind != "scheduled_reduce") {
        return(NULL)
    }

    mode <- node$mode
    op <- node$op
    acc <- node$acc
    arg <- node$arg
    n_var <- if (!is.null(node$n_var))
        node$n_var else "n_i"

    combine_expr <- function(lhs, rhs) {
        if (op %in% c("min", "max")) {
            return(paste0(op, "(", lhs, ", ", rhs, ")"))
        }
        if (op == "sum") {
            return(paste0("(", lhs, " + ", rhs, ")"))
        }
        if (op == "product") {
            return(paste0("(", lhs, " * ", rhs, ")"))
        }
        NULL
    }

    infer_dtype <- function(mode) {
        if (!is.null(node$dtype))
            return(node$dtype)
        arg_type <- if (!is.null(type_env))
            type_env[[arg]] else "f64[]"
        if (identical(mode, "tree")) {
            if (grepl("^f32", arg_type)) "Float32"
            else if (grepl("^i32", arg_type)) "Int32"
            else "Float64"
        } else {
            if (grepl("^f32", arg_type)) "DType.float32"
            else if (grepl("^i32", arg_type)) "DType.int32"
            else "DType.float64"
        }
    }

    get_value_cast <- function() {
        if (!is.null(node$value_cast) &&
            is.character(node$value_cast) &&
            length(node$value_cast) == 1) {
            node$value_cast
        } else {
            NULL
        }
    }

    which_labels <- function() {
        want_max <- identical(op, "which.max")
        list(
            want_max = want_max,
            op_label = if (want_max) "which.max" else "which.min",
            pair_label = if (want_max) "argmax" else "argmin"
        )
    }

    reduce_empty_val <- function(dtype) {
        if (!is.null(node$empty_val)) {
            node$empty_val
        } else if (!is.null(node$nan_val)) {
            node$nan_val
        } else if (op == "sum") {
            if (dtype %in% c("Float32", "DType.float32"))
                "Float32(0.0)" else if (dtype %in% c("Int32", "DType.int32"))
                "Int32(0)" else "0.0"
        } else if (op == "product") {
            if (dtype %in% c("Float32", "DType.float32"))
                "Float32(1.0)" else if (dtype %in% c("Int32", "DType.int32"))
                "Int32(1)" else "1.0"
        } else if (dtype %in% c("Int32", "DType.int32")) {
            "Int32(0)"
        } else if (dtype %in% c("Float32", "DType.float32")) {
            "_MOJOR_NAN_F32"
        } else {
            "_MOJOR_NAN"
        }
    }

    if (mode == "tree") {
        dtype <- infer_dtype("tree")
        if (op %in% c("which.min", "which.max")) {
            wl <- which_labels()
            want_max <- wl$want_max
            op_label <- wl$op_label
            pair_label <- wl$pair_label
            pair_cmp <- if (want_max)
                "__mojor_right_v > __mojor_left_v" else "__mojor_right_v < __mojor_left_v"
            rec_cmp_right <- if (want_max)
                "__mojor_rv > __mojor_lv" else "__mojor_rv < __mojor_lv"
            rec_cmp_left <- if (want_max)
                "__mojor_rv < __mojor_lv" else "__mojor_rv > __mojor_lv"
            empty_val <- if (!is.null(node$empty_val))
                node$empty_val else "Int32(0)"
            value_cast <- get_value_cast()
            read_val_expr <- function(idx_expr) {
                raw_expr <- paste0(arg, "[", idx_expr, "]")
                if (!is.null(value_cast))
                  paste0(value_cast, "(", raw_expr, ")") else raw_expr
            }
            code <- c(
                paste0(
                  "# Tree arg-reduction for ", op_label, "(", arg, ") (tie-stable: first index)"
              ),
                paste0("var temp_val = alloc[", dtype, "](", n_var, ")"),
                paste0("var temp_idx = alloc[Int32](", n_var, ")"),
                paste0("if ", n_var, " == Int(0):"),
                paste0("    ", acc, " = ", empty_val),
                "else:", "", paste0("# Phase 1: Pairwise ", pair_label, " reduction"),
                paste0("    var n_pairs = ", n_var, " // 2"),
                "    for __mojor_tr_i in range(n_pairs):", "        var __mojor_left_i = 2*__mojor_tr_i",
                "        var __mojor_right_i = __mojor_left_i + 1", paste0(
                  "        var __mojor_left_v: ", dtype, " = ", read_val_expr("__mojor_left_i")
              ),
                paste0(
                  "        var __mojor_right_v: ", dtype, " = ", read_val_expr("__mojor_right_i")
              ),
                paste0("        if ", pair_cmp, ":"),
                "            temp_val[__mojor_tr_i] = __mojor_right_v",
                "            temp_idx[__mojor_tr_i] = Int32(__mojor_right_i + 1)",
                "        else:", "            temp_val[__mojor_tr_i] = __mojor_left_v",
                "            temp_idx[__mojor_tr_i] = Int32(__mojor_left_i + 1)",
                "", "# Handle odd length", paste0("    if ", n_var, " % 2 == 1:"),
                paste0("        temp_val[n_pairs] = ", read_val_expr(paste0(n_var, " - 1"))),
                paste0("        temp_idx[n_pairs] = Int32(", n_var, ")"),
                "        n_pairs += 1", "", "# Phase 2+: Recursive pairwise (stable ties by lower index)",
                "    while n_pairs > 1:", "        var next_pairs = n_pairs // 2",
                "        for __mojor_tr_j in range(next_pairs):", "            var __mojor_l = 2*__mojor_tr_j",
                "            var __mojor_r = __mojor_l + 1", paste0("            var __mojor_lv: ", dtype, " = temp_val[__mojor_l]"),
                paste0("            var __mojor_rv: ", dtype, " = temp_val[__mojor_r]"),
                "            var __mojor_li: Int32 = temp_idx[__mojor_l]",
                "            var __mojor_ri: Int32 = temp_idx[__mojor_r]",
                paste0("            if ", rec_cmp_right, ":"),
                "                temp_val[__mojor_tr_j] = __mojor_rv",
                "                temp_idx[__mojor_tr_j] = __mojor_ri",
                paste0("            elif ", rec_cmp_left, ":"),
                "                temp_val[__mojor_tr_j] = __mojor_lv",
                "                temp_idx[__mojor_tr_j] = __mojor_li",
                "            else:", "                temp_val[__mojor_tr_j] = __mojor_lv",
                "                temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)",
                "        if n_pairs % 2 == 1:", "            temp_val[next_pairs] = temp_val[n_pairs - 1]",
                "            temp_idx[next_pairs] = temp_idx[n_pairs - 1]",
                "            next_pairs += 1", "        n_pairs = next_pairs",
                "", paste0("    ", acc, " = temp_idx[0]"),
                "temp_idx.free()", "temp_val.free()"
            )
            return(paste0(indent, code))
        }
        empty_val <- reduce_empty_val(dtype)
        pair_expr <- combine_expr(
            paste0(arg, "[2*__mojor_tr_i]"),
            paste0(arg, "[2*__mojor_tr_i + 1]")
        )
        rec_expr <- combine_expr("temp[2*__mojor_tr_j]", "temp[2*__mojor_tr_j + 1]")
        if (is.null(pair_expr) ||
            is.null(rec_expr)) {
            return(NULL)
        }
        code <- c(
            paste0("# Tree reduction for ", op, "(", arg, ")"),
            paste0("var temp = alloc[", dtype, "](", n_var, ")"),
            paste0("if ", n_var, " == Int(0):"),
            paste0("    ", acc, " = ", empty_val),
            "else:", "", "# Phase 1: Pairwise reduction", paste0("    var n_pairs = ", n_var, " // 2"),
            "    for __mojor_tr_i in range(n_pairs):", paste0("        temp[__mojor_tr_i] = ", pair_expr),
            "", "# Handle odd length", paste0("    if ", n_var, " % 2 == 1:"),
            paste0("        temp[n_pairs] = ", arg, "[", n_var, " - 1]"),
            "        n_pairs += 1", "", "# Phase 2+: Recursive pairwise",
            "    while n_pairs > 1:", "        var next_pairs = n_pairs // 2",
            "        for __mojor_tr_j in range(next_pairs):", paste0("            temp[__mojor_tr_j] = ", rec_expr),
            "        if n_pairs % 2 == 1:", "            temp[next_pairs] = temp[n_pairs - 1]",
            "            next_pairs += 1", "        n_pairs = next_pairs",
            "", paste0("    ", acc, " = temp[0]"),
            "temp.free()"
        )
        return(paste0(indent, code))
    }

    if (mode == "simd") {
        dtype <- infer_dtype("simd")

        if (op %in% c("which.min", "which.max")) {
            value_cast <- get_value_cast()
            scalar_ty <- .mojor_dtype_to_scalar_ty(dtype)
            wl <- which_labels()
            want_max <- wl$want_max
            op_label <- wl$op_label
            lane_cmp <- if (want_max)
                "__mojor_lane_v > __mojor_chunk_best_v" else "__mojor_lane_v < __mojor_chunk_best_v"
            chunk_cmp <- if (want_max)
                "__mojor_chunk_best_v > __mojor_best_v" else "__mojor_chunk_best_v < __mojor_best_v"
            rem_cmp <- if (want_max)
                "__mojor_rem_v > __mojor_best_v" else "__mojor_rem_v < __mojor_best_v"
            read_expr <- function(expr) {
                if (is.null(value_cast))
                  expr else paste0(value_cast, "(", expr, ")")
            }
            code <- c(
                paste0(
                  "# SIMD arg-reduction for ", op_label, "(", arg, ") (tie-stable: first index)"
              ),
                "from sys.info import simd_width_of", paste0("comptime simd_width = simd_width_of[", dtype, "]()"),
                paste0("if ", n_var, " == Int(0):"),
                paste0("    ", acc, " = Int32(0)"),
                "else:", paste0(
                  "    var __mojor_best_v: ", scalar_ty, " = ", read_expr(paste0(arg, "[0]"))
              ),
                "    var __mojor_best_idx: Int32 = Int32(1)", paste0("    var n_chunks = ", n_var, " // simd_width"),
                "    for __mojor_sr_i in range(n_chunks):", "        var __mojor_chunk_base = __mojor_sr_i * simd_width",
                paste0(
                  "        var vec = ", arg, ".load[width=simd_width](__mojor_chunk_base)"
              ),
                "        var __mojor_chunk_best_lane = Int(0)", paste0(
                  "        var __mojor_chunk_best_v: ", scalar_ty, " = ",
                  read_expr("vec[0]")
              ),
                "        for __mojor_sr_lane in range(1, simd_width):",
                paste0(
                  "            var __mojor_lane_v: ", scalar_ty, " = ",
                  read_expr("vec[__mojor_sr_lane]")
              ),
                paste0("            if ", lane_cmp, ":"),
                "                __mojor_chunk_best_v = __mojor_lane_v",
                "                __mojor_chunk_best_lane = __mojor_sr_lane",
                "            elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:",
                "                __mojor_chunk_best_lane = __mojor_sr_lane",
                "        var __mojor_chunk_best_idx = Int32(__mojor_chunk_base + __mojor_chunk_best_lane + 1)",
                paste0("        if ", chunk_cmp, ":"),
                "            __mojor_best_v = __mojor_chunk_best_v", "            __mojor_best_idx = __mojor_chunk_best_idx",
                "        elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:",
                "            __mojor_best_idx = __mojor_chunk_best_idx",
                paste0(
                  "    for __mojor_sr_rem in range(n_chunks * simd_width, ",
                  n_var, "):"
              ),
                paste0(
                  "        var __mojor_rem_v: ", scalar_ty, " = ", read_expr(paste0(arg, "[__mojor_sr_rem]"))
              ),
                "        var __mojor_rem_idx = Int32(__mojor_sr_rem + 1)",
                paste0("        if ", rem_cmp, ":"),
                "            __mojor_best_v = __mojor_rem_v", "            __mojor_best_idx = __mojor_rem_idx",
                "        elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:",
                "            __mojor_best_idx = __mojor_rem_idx", paste0("    ", acc, " = __mojor_best_idx")
            )
            return(paste0(indent, code))
        }

        init_val <- if (!is.null(node$init_val)) {
            node$init_val
        } else if (op == "sum") {
            if (dtype == "DType.int32")
                "Int32(0)" else "0.0"
        } else if (op == "product") {
            if (dtype == "DType.int32")
                "Int32(1)" else "1.0"
        } else if (dtype == "DType.int32") {
            if (op == "min")
                "Int32(2147483647)" else "Int32(-2147483648)"
        } else if (dtype == "DType.float32") {
            if (op == "min")
                "_MOJOR_INF_F32" else "_MOJOR_NINF_F32"
        } else {
            if (op == "min")
                "_MOJOR_INF" else "_MOJOR_NINF"
        }
        empty_val <- reduce_empty_val(dtype)
        vec_reduce <- combine_expr("vec_acc", "vec")
        lane_reduce <- combine_expr(acc, "vec_acc[__mojor_sr_lane]")
        rem_reduce <- combine_expr(acc, paste0(arg, "[__mojor_sr_rem]"))
        if (is.null(vec_reduce) ||
            is.null(lane_reduce) ||
            is.null(rem_reduce)) {
            return(NULL)
        }
        code <- c(
            paste0("# SIMD reduction for ", op, "(", arg, ")"),
            "from sys.info import simd_width_of", paste0("comptime simd_width = simd_width_of[", dtype, "]()"),
            paste0(
                "var vec_acc = SIMD[", dtype, ", simd_width](", init_val,
                ")"
            ),
            paste0("if ", n_var, " == Int(0):"),
            paste0("    ", acc, " = ", empty_val),
            "else:", "", "# Phase 1: SIMD accumulation", paste0("    var n_chunks = ", n_var, " // simd_width"),
            "    for __mojor_sr_i in range(n_chunks):", paste0(
                "        var vec = ", arg, ".load[width=simd_width](__mojor_sr_i * simd_width)"
            ),
            paste0("        vec_acc = ", vec_reduce),
            "", "# Phase 2: Horizontal reduction", paste0("    ", acc, " = vec_acc[0]"),
            "    for __mojor_sr_lane in range(1, simd_width):", paste0("        ", acc, " = ", lane_reduce),
            "", "# Phase 3: Handle remainder", paste0(
                "    for __mojor_sr_rem in range(n_chunks * simd_width, ",
                n_var, "):"
            ),
            paste0("        ", acc, " = ", rem_reduce)
        )
        return(paste0(indent, code))
    }

    NULL
}
