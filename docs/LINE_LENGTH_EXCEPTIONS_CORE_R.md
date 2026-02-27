# Core R Line-Length Exceptions (120)

Generated: 2026-02-18

These are explicit exceptions after broad formatting churn in scoped core trees.

## packages/mojor/R
packages/mojor/R/build/build_expression.R:500: " if (__mojor_start < 1) __mojor_start = 1;", " if (__mojor_stop < __mojor_start || __mojor_start > __mojor_len || __mojor_len <= 0) {",
packages/mojor/R/build/build_expression.R:504: " const char* __mojor_raw = CHAR(__mojor_ch);", " SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_raw + __mojor_from, __mojor_n));",
packages/mojor/R/build/build_expression.R:562: " if (__mojor_sep_len > 0) memcpy(__mojor_buf + __mojor_xn, __mojor_sep, (size_t)__mojor_sep_len);",
packages/mojor/R/build/build_expression.R:564: " __mojor_buf[__mojor_total] = '\\0';", " SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_buf, __mojor_total));",
packages/mojor/R/build/build_expression.R:608: " __mojor_off += __mojor_sep_len;", " }", " memcpy(__mojor_buf + __mojor_off, __mojor_ys, (size_t)__mojor_yn);",
packages/mojor/R/build/build_expression.R:609: " __mojor_off += __mojor_yn;", " if (__mojor_i + 1 < __mojor_out_len && __mojor_collapse_len > 0) {",
packages/mojor/R/build/build_expression.R:612: " }", " __mojor_buf[__mojor_total] = '\\0';", " SEXP __mojor_out = PROTECT(Rf_ScalarString(Rf_mkCharLen(__mojor_buf, __mojor_total)));",
packages/mojor/R/build/build_expression.R:867: " if (TYPEOF(%s_dim_sexp) != INTSXP || LENGTH(%s_dim_sexp) != 2) error(\"%s: expected matrix (2D array)\");",
packages/mojor/R/build/build_expression.R:1179: if (identical(trans$return_type, "Int32")) " return Rf_ScalarInteger(result);" else " return Rf_ScalarReal(result);",
packages/mojor/R/ir/verify.R:164: "IR verify [%s]: FUN named function '%s' is unsupported for arity %d; use an anonymous function or one of: %s",
packages/mojor/R/ir/verify.R:1515: stop("IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'; otherwise assign the result to a variable")
packages/mojor/R/ir/verify.R:1360: if (!is.numeric(node$expected_arity) || length(node$expected_arity) != 1 || is.na(node$expected_arity) || node$expected_arity < 0) {
packages/mojor/R/ir/verify.R:1428: if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
packages/mojor/R/transpile/helpers_core.R:656:.mojor_phase_runtime_regex <- function(op, pattern, x, replacement = NULL, fixed = FALSE, perl = TRUE, context = "mojor_build") {
packages/mojor/R/transpile/helpers_core.R:853:.mojor_signal_error <- function(message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL, severity = "error") {
packages/mojor/R/transpile/helpers_core.R:870:.mojor_add_diag <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:1166:mojor_diagnostics_add <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:1168: .mojor_add_diag(severity, message, file = file, line = line, col = col, end_line = end_line, end_col = end_col, expr = expr)
packages/mojor/R/transpile/helpers_core.R:1514: stop("Indexing must use a loop variable (optionally +/- integer literal, scalar variable, or multiplied/divided by integer)")
packages/mojor/R/transpile/helpers_core.R:1522: .mojor_warn(paste0(inner_op, "() lowered to recycling"), expr[[2]], "length.out/times ignored; uses output length")
packages/mojor/R/transpile/helpers_core.R:1530: if (length(inner_parts) >= pos && (is.null(inner_nms) || is.null(inner_nms[[pos]]) || inner_nms[[pos]] == "")) {
packages/mojor/R/transpile/helpers_core.R:1539: if (is.null(len_expr)) .mojor_err("rep_len() length.out must be an integer literal or scalar arg", expr[[2]])
packages/mojor/R/transpile/helpers_core.R:1757:.mojor_elementwise_plan <- function(li, out_name, out_kind, out_type, elementwise_target, elementwise_cpu, non_unit_range,
packages/mojor/R/transpile/helpers_core.R:1952: if (is.null(spec) || (gpu_buf_dtype == "f32" && spec != "f32[]") || (gpu_buf_dtype == "f64" && spec != "f64[]")) {
packages/mojor/R/transpile/helpers_core.R:1954: gpu_buf_reason <- if (gpu_buf_dtype == "f64") "gpu buffers require f64[] inputs" else "gpu buffers require f32[] inputs"
packages/mojor/R/transpile/helpers_core.R:2036:.mojor_expr_to_mojo <- function(expr, loop_vars, types, in_cond = FALSE, zero_based_vars = NULL, idx_override = NULL, idx_zero_based = FALSE, suppress_len_checks = FALSE, suppress_read_helpers = FALSE) {
packages/mojor/R/transpile/helpers_core.R:2322: return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
packages/mojor/R/transpile/helpers_core.R:2347: return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
packages/mojor/R/transpile/helpers_core.R:2609:.mojor_ifelse_assign_vector <- function(lhs_var, idx, cond, yes, no, loop_vars, types, target_spec, indent, target_expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:2695:.mojor_ifelse_assign_scalar <- function(lhs_var, cond, yes, no, loop_vars, types, target_spec, indent, break_on_na = TRUE) {
packages/mojor/R/transpile/helpers_core.R:2780: .mojor_warn("ifelse() expression lowered to a temporary", expr, "prefer explicit loop assignment for best performance")
packages/mojor/R/transpile/helpers_core.R:2931: .mojor_err("unsupported any()/all() expression", expr, "use array expressions with &, |, comparisons, or is.na/is.nan/is.finite/is.infinite")
packages/mojor/R/transpile/helpers_core.R:3100: lines <- c(lines, sprintf(" if (%s > __mojor_bc_ndim) __mojor_bc_ndim = %s;", .mojor_ndim_param_name(a), .mojor_ndim_param_name(a)))
packages/mojor/R/transpile/helpers_core.R:3140: sprintf(" if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");", a, a, a, a),
packages/mojor/R/transpile/helpers_core.R:3141: sprintf(" %s %s_val = (TYPEOF(%s) == REALSXP) ? (%s) REAL(%s)[0] : (%s) INTEGER(%s)[0];", c_type, a, a, c_type, a, c_type, a)
packages/mojor/R/transpile/loop_codegen_units.R:55: if (had_skip_when_tiled) ctx$skip_when_tiled <- prev_skip_when_tiled else rm(list = "skip_when_tiled", envir = ctx, inherits = FALSE)
packages/mojor/R/transpile/loop_codegen_units.R:56: if (had_tiling_applied) ctx$tiling_applied <- prev_tiling_applied else rm(list = "tiling_applied", envir = ctx, inherits = FALSE)
packages/mojor/R/transpile/loop_codegen_units.R:309: loop_body_expr <- .mojor_build_transpile_loop_body_expr(li, short_ir_fallback = short_ir_fallback, scalar_init = scalar_init)
packages/mojor/R/ir/ssa_backend.R:168: "SSA backend invariant: expected phase order to_ssa -> memory_canonicalize -> annotate_effects_resources -> typing -> backend_cfg"
packages/mojor/R/ir/ssa_backend.R:1154: args = args, index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds),
packages/mojor/R/ir/ssa_backend.R:2007: fusion <- .mojor_ssa_fusion_candidate_analysis(ssa_recanonical, recovery = loop_recovery, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2008: ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(ssa_recanonical, analysis = fusion, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2025: ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(fusion_rewrite_result$ssa, analysis = fusion, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2486: formatted_selected = if (!is.null(selected)) .mojor_ir_ssa_backend_selected_format(selected, verify = FALSE) else NULL,
packages/mojor/R/ir/ssa_backend.R:2487: formatted_codegen = if (!is.null(codegen)) .mojor_ir_ssa_backend_codegen_format(codegen, target = backend_target, verify = FALSE) else NULL
packages/mojor/R/build/build.R:352: na_mode = build_ctx$na_mode, semantics = build_ctx$semantics, opt_level = if (is.null(build_ctx$opt_level)) "" else as.character(build_ctx$opt_level),
packages/mojor/R/build/build.R:1345: c_lines, " union { int i; float f; } __mojor_f32_bits;", " #define MOJOR_BITS_TO_F32(bits) (__mojor_f32_bits.i = (bits), __mojor_f32_bits.f)"
packages/mojor/R/build/build.R:1370: " if (IS_S4_OBJECT(%s)) { SEXP __mc_%s = R_do_slot(%s, install(\"Data\")); if (__mc_%s == R_NilValue || LENGTH(__mc_%s) == 0) error(\"%s: zero-length float32 array (memory_check)\"); }",
packages/mojor/R/build/build.R:1388: " if (TYPEOF(%s) == REALSXP && LENGTH(%s) > 0 && ISNAN(REAL(%s)[0])) error(\"%s: scalar is NA/NaN (memory_check)\");",
packages/mojor/R/build/build.R:1395: " if (TYPEOF(%s) == INTSXP && LENGTH(%s) > 0 && INTEGER(%s)[0] == NA_INTEGER) error(\"%s: scalar is NA (memory_check)\");",
packages/mojor/R/build/build.R:1402: " if (TYPEOF(%s) == LGLSXP && LENGTH(%s) > 0 && LOGICAL(%s)[0] == NA_LOGICAL) error(\"%s: scalar is NA (memory_check)\");",
packages/mojor/R/build/build.R:1489: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) __mojor_%s_cast[__mojor_i_%s] = (double) INTEGER(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1508: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) REAL(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1512: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) INTEGER(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1535: " if (TYPEOF(%s_data) != INTSXP || LENGTH(%s_data) != 1) error(\"%s: expected float32 scalar data\");",
packages/mojor/R/build/build.R:1546: " if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");",
packages/mojor/R/build/build.R:1581: " if ((__mojor_seq_by > 0.0 && __mojor_seq_to < __mojor_seq_from) || (__mojor_seq_by < 0.0 && __mojor_seq_to > __mojor_seq_from)) error(\"seq(): wrong sign in 'by' argument\");"
packages/mojor/R/build/build.R:1846: c_lines, if (a %in% nrow_arrays) NULL else sprintf(" SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
packages/mojor/R/build/build.R:1871: c_lines, if (a %in% nrow_arrays) NULL else sprintf(" SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
packages/mojor/R/build/build.R:2126: c_lines, " if (__mojor_out_nrow <= 0 || __mojor_out_ncol <= 0) error(\"output matrix dims must be positive\");"
packages/mojor/R/build/build.R:2155: "; di++) {", " if (", .mojor_out_dim_param_name(), "_ptr[di] <= 0) error(\"output array dims must be positive\");",
packages/mojor/R/build/build.R:2311: c_lines, " SEXP dim = PROTECT(allocVector(INTSXP, 2));", paste0(" INTEGER(dim)[0] = ", .mojor_out_nrow_param_name(), ";"),
packages/mojor/R/build/build.R:3072: max_bytes = .mojor_state$options$jit_cache_max_bytes, max_entries = .mojor_state$options$jit_cache_max_entries,
packages/mojor/R/build/build.R:3350: max_entries = .mojor_state$options$jit_cache_max_entries, max_age_days = .mojor_state$options$jit_cache_max_age_days,
packages/mojor/R/transpile/entry_phases.R:282: elementwise_gpu_layouttensor <- .mojor_assert_scalar_logical_or_null(elementwise_gpu_layouttensor, "elementwise_gpu_layouttensor")
packages/mojor/R/transpile/entry_phases.R:425: helper_lines, "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float64:",
packages/mojor/R/transpile/entry_phases.R:441: helper_lines, "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float32:",
packages/mojor/R/transpile/entry_phases.R:457: helper_lines, "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
packages/mojor/R/transpile/entry_phases.R:473: helper_lines, "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
packages/mojor/R/transpile/entry_phases.R:495: gpu_helper_lines, "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF64Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float64:",
packages/mojor/R/transpile/entry_phases.R:516: " return _MOJOR_NAN", "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF32Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float32:",
packages/mojor/R/transpile/entry_phases.R:542: gpu_helper_lines, "fn _mojor_gpu_matmul_into(out: MutF64Ptr, a: ImmutF64Ptr, b: ImmutF64Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF64Ptr:",
packages/mojor/R/transpile/entry_phases.R:556: "fn _mojor_gpu_matmul_into(out: MutF32Ptr, a: ImmutF32Ptr, b: ImmutF32Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF32Ptr:",
packages/mojor/R/transpile/entry_phases.R:570: "fn _mojor_gpu_matmul[a_t: AnyType, b_t: AnyType](a: a_t, b: b_t, transpose_a: Bool, transpose_b: Bool) raises -> a_t:",
packages/mojor/R/transpile/entry_phases.R:652: opt_level_fallback = phase$ssa_backend_opt_fallback, opt_level_fallback_reason = phase$ssa_backend_opt_fallback_reason
packages/mojor/R/transpile/entry_phases.R:668: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/transpile/entry_phases.R:705: ssa_backend_opt_fallback = ssa_backend_opt_fallback, ssa_backend_opt_fallback_reason = ssa_backend_opt_fallback_reason
packages/mojor/R/core/gpu.R:887: "args <- commandArgs(trailingOnly = TRUE)", "if (length(args) < 2L) stop('probe runner requires request/result paths')",
packages/mojor/R/core/gpu.R:888: "req <- readRDS(args[[1L]])", "out_path <- args[[2L]]", "res <- list(available = FALSE, reason = 'probe runner did not execute')",
packages/mojor/R/core/gpu.R:889: "tryCatch({", " source(req$mojor_script)", " if (is.character(req$mojor_lib_path) && length(req$mojor_lib_path) == 1L && nzchar(req$mojor_lib_path)) {",
packages/mojor/R/core/gpu.R:892: " if (!isTRUE(mojor_is_loaded()) || !isTRUE(mojor_has_gpu())) stop('GPU not available for f64 probe subprocess', call. = FALSE)",
packages/mojor/R/core/gpu.R:903: " out <- tryCatch({", " .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode, op_class = op_class)",
packages/mojor/R/core/gpu.R:908: " stop('unsupported probe kind: ', kind)", " }", " if (!is.list(out) || is.null(out$available)) stop('probe subprocess produced invalid result')",
packages/mojor/R/core/gpu.R:4139: gather_out <- .mojor_gpu_try_gather_linearized(x, idx_vals = idx_vals, missing_flags = missing_flags, drop = drop)
packages/mojor/R/core/core.R:774: effective_ir_only <- .mojor_resolve_effective_ir_only(primary = build_args, secondary = user_extra, context = "mojor_fn")
packages/mojor/R/core/core.R:1991: current_index_base = NULL, current_array_layout = NULL, current_ir_only = isTRUE(.mojor_options_defaults()$ir_only),
packages/mojor/R/transpile/expression_only.R:503: if (identical(op_name, "paste")) "paste(x, y, sep = \"...\", collapse = \"...\"|NULL)" else "paste0(x, y, collapse = \"...\"|NULL)",
packages/mojor/R/transpile/expression_only.R:539: stop("mojor_transpile: ", op_label, "() collapse must be a scalar character literal or NULL in expression-only mode")
packages/mojor/R/transpile/expression_only.R:582: "() currently supports only ", op_name, "(pattern, x, fixed = TRUE|FALSE, perl = TRUE|FALSE) in expression-only mode"
packages/mojor/R/transpile/expression_only.R:699: stop("mojor_transpile: ", op_name, "() named FUN is unsupported for arity ", expected_arity, " in expression-only mode")
packages/mojor/R/transpile/expression_only.R:707: stop("mojor_transpile: ", op_name, "() requires FUN to be an anonymous function or supported named function in expression-only mode")
packages/mojor/R/transpile/expression_only.R:879: stop("mojor_transpile: mapply() requires all vector arguments to have the same element type in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1002: stop("mojor_transpile: ", op_label, "() ", label, " must be a direct character vector argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1022: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1030: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1095: if (!(is.null(collapse_value) || (is.character(collapse_value) && length(collapse_value) == 1 && !is.na(collapse_value)))) {
packages/mojor/R/transpile/expression_only.R:1130: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct character vector argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1138: stop("mojor_transpile: ", op_label, "() ", arg_label, " must have type chr[] in expression-only mode; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1150: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1157: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1169: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a numeric/integer/logical matrix argument; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1183: stop("mojor_transpile: ", op_label, "() currently supports only f64[], i32[], lgl[] vector arguments; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1256: if ((op %in% c("sub", "gsub")) && identical(replacement_info$kind, "const") && has_backref(replacement_info$value)) {
packages/mojor/R/transpile/expression_only.R:1262: if (!is.null(replacement_info) && identical(replacement_info$kind, "arg")) kernel_args <- c(kernel_args, replacement_info$name)
packages/mojor/R/transpile/expression_only.R:1473: paste0(" _ = mojor_match_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
packages/mojor/R/transpile/expression_only.R:1479: paste0(" _ = mojor_in_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
packages/mojor/R/transpile/expression_only.R:2246: stop("mojor_transpile: quantile() probs must be a function parameter or supported compile-time numeric vector expression in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1604: paste0(" _ = mojor_quantile_", type_suffix, "(", x_name, ", n_", x_name, ", ", probs_name, ", n_", probs_name, ", ", type_str, ", ", na_rm_str, ", out)"),
packages/mojor/R/transpile/expression_only.R:1672: paste0(" return mojor_mad_", type_suffix, "(", x_name, ", n_", x_name, ", center, ", na_rm_str, ", 1.4826, ", type_str, ")")
packages/mojor/R/transpile/expression_only.R:1722: stop("mojor_transpile: ", op_label, "() supports at most ", max_positional, " positional arguments in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2660: stop("mojor_transpile: ", op_label, "() weighted sampling currently requires prob to be a direct f64[] argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2668: stop("mojor_transpile: ", op_label, "() weighted sampling requires prob argument '", nm, "' to be typed f64[] in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1793: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1809: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1825: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2660: stop("mojor_transpile: ", op_label, "() weighted sampling currently requires prob to be a direct f64[] argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2668: stop("mojor_transpile: ", op_label, "() weighted sampling requires prob argument '", nm, "' to be typed f64[] in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2156: stop("mojor_transpile: expression-only mode currently only supports standalone reductions (sum, mean, min, max, prod, sd, var), matrix operations (%*%, crossprod, tcrossprod), covariance/correlation (cov, cor), set/match primitives (unique, duplicated, anyDuplicated, match, %in%), quantile/robust stats (median, quantile, IQR, mad), higher-order functions (vapply, sapply, lapply, mapply) under strict inline subset, sampling (sample.int, sample) under strict scalar-arg subset, native string subset (nchar, nzchar, substr, substring, paste, paste0) under strict chr[] direct-arg constraints, /9.3 regex-table subset (grepl, grep, sub, gsub, row, col, expand.grid) under strict direct-arg constraints, and scalar expressions.")
packages/mojor/R/transpile/expression_only.R:2370: stop("mojor_transpile: ", operation, " left-hand argument '", lhs_name, "' must be a matrix type (f64[,]), got: ", lhs_type)
packages/mojor/R/transpile/expression_only.R:2376: stop("mojor_transpile: ", operation, " right-hand argument must be a simple variable name, got: ", deparse(rhs_expr))
packages/mojor/R/transpile/expression_only.R:2386: stop("mojor_transpile: ", operation, " right-hand argument '", rhs_name, "' must be a matrix type (f64[,]), got: ", rhs_type)
packages/mojor/R/transpile/expression_only.R:2571: stop("mojor_transpile: ", operation, "() with single argument requires a matrix, got vector. Use var(x) for variance or provide two vector arguments.")
packages/mojor/R/transpile/helpers_analysis.R:1041: .mojor_state$current_local_vector_lengths[[out_name]] <- list(length = resolved_len, type = out_type)
packages/mojor/R/transpile/helpers_analysis.R:2109: "sum()/prod()/mean()/var()/sd()/min()/max()/which.min()/which.max() reductions require a typed array argument",
packages/mojor/R/transpile/helpers_analysis.R:2150: "sum()/prod()/mean()/var()/sd()/min()/max()/which.min()/which.max() reductions require an array variable",
packages/mojor/R/transpile/helpers_analysis.R:2217: "which.min()/which.max() expression reductions could not be lowered to element-wise selector access",
packages/mojor/R/transpile/helpers_analysis.R:3500: "mojor_transpile: matrix(dimnames=...) must be a list of character literals like list(c(\"a\", \"b\"), c(\"c\", \"d\"))"
packages/mojor/R/transpile/helpers_analysis.R:3918: "mojor_transpile: unsupported length expression (use length(name), scalar name, literal, +, -, *, %/%, %%, min/max)"
packages/mojor/R/transpile/helpers_analysis.R:4023: "mojor_transpile: unsupported float expression (use literals, scalar names, length(name), +, -, *, /, %/%, %%, min/max)"
packages/mojor/R/transpile/prelude_helpers.R:100: .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- build_iter_expr_entry(loop_infos[[1]], loop_var)
packages/mojor/R/transpile/prelude_helpers.R:102: .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- list(source = loop_infos[[1]]$iter_source, index = loop_var)
packages/mojor/R/ir/ir.R:361: indent, "fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/ir.R:384: indent, "fn _mojor_elementwise_", dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/stmt_assign_emit.R:668: base_name, mask_name, rhs_mask_var, rhs_mask_name, if (!is.null(type_env)) names(type_env) else character(0),
packages/mojor/R/ir/type_guards.R:77: layout_kind <- .mojor_state$`%||%`(.mojor_state$current_array_layout, .mojor_state$options$array_layout)
packages/mojor/R/core/reductions_jit.R:535: elementwise_target = elementwise_target, elementwise_size = if (is.null(elementwise_size)) "" else as.character(elementwise_size),
packages/mojor/R/ir/ssa_semantic.R:93: "^condbr\\s+(%[A-Za-z][A-Za-z0-9_]*)\\s*\\?\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*:\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*$",
packages/mojor/R/transpile/gpu_emit.R:146: " var ", .mojor_out_dim_var_name(), "_stage = ctx.enqueue_create_host_buffer[DType.int32](",
packages/mojor/R/transpile/gpu_emit.R:347: " var ", a, "_dev = LayoutTensor[mut=False, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/gpu_emit.R:355: " var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/gpu_emit.R:358: " @parameter", " @always_inline", " fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/transpile/transpile.R:279: pre_loop_out_assigns <- .mojor_collect_pre_loop_out_assignments(blocks = blocks, out_name = out_name, scalar_inits = scalar_inits)
packages/mojor/R/transpile/transpile.R:555: loop_infos[[i]]$iter_expr_transform <- info_i$iter_expr_transform # Preserve division/multiplication transforms
packages/mojor/R/transpile/transpile.R:1930: debug_imports <- "from debug_helpers import mojor_assert, mojor_check_bounds, mojor_check_bounds_2d, mojor_trace, mojor_trace_value, mojor_trace_value_int, mojor_trace_enter, mojor_trace_exit"
packages/mojor/R/transpile/transpile.R:1934: debug_imports, ", mojor_check_ptr_f64, mojor_check_ptr_f64_mut, mojor_check_ptr_f32, mojor_check_ptr_f32_mut, mojor_check_ptr_i32, mojor_check_ptr_i32_mut, mojor_check_length"
packages/mojor/R/transpile/transpile.R:1984: "comptime MutBufF32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1985: "comptime MutBufF64Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1992: " var buf: MutBufF64Ptr", " var len: Int", "comptime MutGpuBufF32Ptr = UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1999: " return total <= MOJOR_GPU_MAX_BYTES", "fn _mojor_set_status(ptr: MutOpaqueAny, value: Int32) -> None:",
packages/mojor/R/transpile/transpile.R:2013: mojo_lines, "from nn.argmaxmin import argmax, argmin", paste0("comptime _MOJOR_ARGEXT_LAYOUT = ", layout_ctor(1, "0"))
packages/mojor/R/transpile/transpile.R:2034: "from math import exp, log, log1p, sqrt", paste0("from rng_helpers import ", paste(rng_helper_syms, collapse = ", "))
packages/mojor/R/transpile/transpile.R:2038: rng_imports, "from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE, _ZIGGURAT_NOR_R, _ZIGGURAT_NOR_INV_R"
packages/mojor/R/transpile/transpile.R:2065: "from na_helpers import _mojor_lt_f64_na, _mojor_gt_f64_na, _mojor_le_f64_na, _mojor_ge_f64_na, _mojor_eq_f64_na, _mojor_ne_f64_na"
packages/mojor/R/transpile/transpile.R:2071: "from na_helpers import _mojor_lt_f32_na, _mojor_gt_f32_na, _mojor_le_f32_na, _mojor_ge_f32_na, _mojor_eq_f32_na, _mojor_ne_f32_na"
packages/mojor/R/transpile/transpile.R:2090: "from set_match_helpers import mojor_any_duplicated_f64, mojor_any_duplicated_f32, mojor_any_duplicated_i32"
packages/mojor/R/transpile/transpile.R:2248: mojo_lines, "fn _mojor_bcast_index(out_idx: Int, out_dim: ImmutInt32Ptr, out_ndim: Int, arg_dim: ImmutInt32Ptr, arg_ndim: Int) -> Int:",
packages/mojor/R/transpile/transpile.R:2647: " var __mojor_rng_wi = materialize[_WI_DOUBLE]()", " var __mojor_rng_fi = materialize[_FI_DOUBLE]()"
packages/mojor/R/transpile/transpile.R:3019: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/ir/op_schema.R:718: loop_id = loop_id, kind = p$kind, anchor_block = bn, continue_dest = list(loop_id = loop_id, role = "Continue", block = bn),
packages/mojor/R/transpile/helpers_codegen.R:1180: "unsupported loop sequence", seq_expr, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
packages/mojor/R/transpile/helpers_codegen.R:1185: "unsupported loop sequence", seq_expr, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
packages/mojor/R/transpile/helpers_codegen.R:2234: n_var = "n_i", out_name = out_name, out_type = out_type, out_matrix = isTRUE(.mojor_state$current_out_is_matrix),
packages/mojor/R/transpile/helpers_codegen.R:2818: "var _mojor_out_tensor = LayoutTensor[mut=True, DType.int64, _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](_mojor_out_ptr, _mojor_out_layout)",
packages/mojor/R/transpile/helpers_codegen.R:3053: kernel_sig_parts <- .mojor_append_gpu_kernel_broadcast_sig(kernel_sig_generic, kernel_sig_wrapper, kernel_call, dim_arrays)
packages/mojor/R/transpile/helpers_codegen.R:3182: " var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/helpers_codegen.R:3185: " @parameter", " @always_inline", " fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/transpile/helpers_codegen.R:3205: " fn _mojor_elementwise_", gpu_dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/fusion.R:139: order <- if (!is.null(step) && step$kind == "const" && (as.character(step$value) == "1" || as.numeric(step$value) > 0)) "ascending" else "descending"
packages/mojor/R/ir/ssa_verify.R:306: severity = "warning", block = "", message = "post_recanonicalize boundary checks disabled because loop recovery status is Unknown",
packages/mojor/R/ir/nodes.R:102:.mojor_ir_scalar_reduce <- function(op, acc, arg, init = NULL, axis = 0L, associative = TRUE, commutative = TRUE, na_rm = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:123:.mojor_ir_scheduled_reduce <- function(mode, op, acc, arg, n_var = "n_i", dtype = NULL, init_val = NULL, empty_val = NULL, nan_val = NULL, value_cast = NULL, src = NULL) {
packages/mojor/R/ir/nodes.R:367:.mojor_ir_regex_grep <- function(pattern, x, value = FALSE, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:368: list(kind = "regex_grep", pattern = pattern, x = x, value = value, ignore_case = ignore_case, perl = perl, fixed = fixed, src = src)
packages/mojor/R/ir/nodes.R:371:.mojor_ir_regex_sub <- function(pattern, replacement, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, global = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:372: list(kind = "regex_sub", pattern = pattern, replacement = replacement, x = x, ignore_case = ignore_case, perl = perl, fixed = fixed, global = global, src = src)
packages/mojor/R/ir/nodes.R:789: paste0("(", mean_val, " + ", sd_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
packages/mojor/R/ir/nodes.R:794: paste0("(_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", shape_val, ") / ", rate_val, ")")
packages/mojor/R/ir/nodes.R:804: paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], "))")
packages/mojor/R/ir/nodes.R:809: paste0("exp(", meanlog_val, " + ", sdlog_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
packages/mojor/R/ir/nodes.R:816: paste0("(_random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi) / sqrt(_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df_val, ") / ", df_val, "))")
packages/mojor/R/ir/nodes.R:821: paste0("((_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df1_val, ") / ", df1_val, ") / (_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df2_val, ") / ", df2_val, "))")
packages/mojor/R/ir/nodes.R:824: paste0("_random_beta(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], ", ", params[[2]], ")")
packages/mojor/R/ir/nodes.R:846: paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, (_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", size_val, ") / (", prob_val, " / (1.0 - ", prob_val, ")))))")
packages/mojor/R/ir/nodes.R:849: paste0("Float64(_random_hypergeometric(__mojor_rng_state, Int(", params[[1]], "), Int(", params[[2]], "), Int(", params[[3]], ")))")
packages/mojor/R/ir/nodes.R:1331: slice_index = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$start), .mojor_ir_expr_resources(node$end))),
packages/mojor/R/ir/nodes.R:1338: rep_len = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$x), .mojor_ir_expr_resources(node$length_out))),
packages/mojor/R/ir/nodes.R:2093:.mojor_ir_collect_loop_invariants <- function(stmt, loop_var, subexprs = list(), allow_reads = FALSE, written_resource_ids = character(0)) {
packages/mojor/R/ir/nodes.R:3005: lhs_base <- if (!is.null(stmt$lhs$base)) stmt$lhs$base else if (!is.null(stmt$lhs$var)) .mojor_ir_var(stmt$lhs$var) else NULL
packages/mojor/R/ir/nodes.R:3342: fused$metadata$elementwise$fused_count <- if (is.null(fused$metadata$elementwise$fused_count)) 2L else fused$metadata$elementwise$fused_count + 1L
packages/mojor/R/ir/nodes.R:3730: init_val <- if (op == "min") "_MOJOR_INF_F32" else if (op == "max") "_MOJOR_NINF_F32" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
packages/mojor/R/ir/nodes.R:3733: init_val <- if (op == "min") "_MOJOR_INF" else if (op == "max") "_MOJOR_NINF" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
packages/mojor/R/ir/nodes.R:3739: init_val <- if (op == "sum") "Int32(0)" else if (op == "product") "Int32(1)" else if (isTRUE(is_which)) "Int32(0)" else NULL
packages/mojor/R/ir/nodes.R:3949: if (op %in% c("c", "rep", "rep_len", "rep.int", "seq", "seq.int", "t", "cbind", "rbind", "diag", "cumsum", "cumprod", "cummax", "cummin", "mean", "var", "sd")) {
packages/mojor/R/ir/nodes.R:4014: part_lens <- vapply(parts, .mojor_ir_ctor_len_expr, character(1), type_env = type_env, len_var_map = len_var_map, n_source_name = n_source_name)
packages/mojor/R/transpile/ir_helpers.R:77: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/ir/loop_recanonicalize.R:157: loop_id = loop_id, role = "Continue", block = if (!is.null(li$header)) as.character(li$header) else NULL
packages/mojor/R/ir/loop_recanonicalize.R:159: break_dest = if (!is.null(li$break_dest)) li$break_dest else list(loop_id = loop_id, role = "Break", block = NULL)
packages/mojor/R/ir/expr_emit.R:1:.mojor_ir_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
packages/mojor/R/ir/expr_emit.R:42: if (length(requested_num) != 1 || !is.finite(requested_num) || requested_num != floor(requested_num) || requested_num < 1) {
packages/mojor/R/ir/expr_emit.R:156: "_mojor_sample_pick_index(__mojor_rng_state, Int(", n_str, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
packages/mojor/R/ir/expr_emit.R:188: "_mojor_sample_pick_index(__mojor_rng_state, Int(", x_len, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
packages/mojor/R/ir/expr_emit.R:548: args[i] <- .mojor_ir_expr_emit(node$args[[i]], zero_based_vars, type_env, loop_vars, index_context = index_context)
packages/mojor/R/ir/expr_emit.R:600: return(if (is_float) paste0("((", args[[1]], " == ", inf_pos, ") or (", args[[1]], " == ", inf_neg, "))") else "False")
packages/mojor/R/ir/expr_emit.R:603: return(if (is_float) paste0("((", args[[1]], " != ", inf_pos, ") and (", args[[1]], " != ", inf_neg, ") and (", args[[1]], " == ", args[[1]], "))") else "True")
packages/mojor/R/ir/expr_emit.R:754: if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
packages/mojor/R/ir/expr_emit.R:1030: return(paste0("(", from_str, " if (", length_out_str, " == 1) else (", from_str, " + ", pos0, " * (", to_str, " - ", from_str, ") / Float64((", length_out_str, " - 1))))"))
packages/mojor/R/ir/expr_emit.R:1714: table_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[table_name]])) len_var_map[[table_name]] else "n_i"
packages/mojor/R/ir/expr_emit.R:1759: probs_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[probs_name]])) len_var_map[[probs_name]] else "n_i"
packages/mojor/R/ir/expr_emit.R:1761: na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, zero_based_vars, type_env, loop_vars) else "False"
packages/mojor/R/ir/expr_emit.R:1765: return(paste0("mojor_quantile_", type_suffix, "(", x_name, ", ", x_len, ", ", probs_name, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", out)"))
packages/mojor/R/ir/expr_emit.R:1946:.mojor_ir_rng_vec_scalar_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
packages/mojor/R/ir/stmt_loop_emit.R:616: " var __mojor_r = __mojor_l + 1", paste0(" var __mojor_lv: ", dtype, " = temp_val[__mojor_l]"),
packages/mojor/R/ir/stmt_loop_emit.R:736: " for __mojor_sr_i in range(n_chunks):", " var __mojor_chunk_base = __mojor_sr_i * simd_width",
packages/mojor/R/ir/stmt_loop_emit.R:752: " elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:",
packages/mojor/R/ir/stmt_loop_emit.R:756: " __mojor_best_v = __mojor_chunk_best_v", " __mojor_best_idx = __mojor_chunk_best_idx",
packages/mojor/R/ir/stmt_emit.R:1:.mojor_ir_stmt_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid", bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL, unroll = NULL, schedule = NULL, bounds_guard_cache = NULL, na_guard_cache = NULL) {
packages/mojor/R/ir/stmt_emit.R:123: out_lines <- c(paste0(indent, "var ", lhs_name, ": Array[Float64, 1] = Array[Float64, 1](", length(values), ")"))
packages/mojor/R/ir/stmt_emit.R:296: read_expr <- paste0(read_helper_for_spec(src_spec), "(", src_name, ", ", idx_expr, ", Int(", src_len, "))")
packages/mojor/R/ir/stmt_emit.R:370: arr_dims <- if (!is.null(local_array_dims) && !is.null(local_array_dims[[lhs_name]])) local_array_dims[[lhs_name]]$dim else NULL
packages/mojor/R/ir/stmt_emit.R:558: if (node$rhs$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$src)) {
packages/mojor/R/ir/stmt_emit.R:563: if (node$rhs$expr$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$expr$src)) {
packages/mojor/R/ir/stmt_emit.R:572: ctor_expr <- if (is.list(ctor_expr_node) && !is.null(ctor_expr_node$kind)) ctor_expr_node else ctor_expr_node$src
packages/mojor/R/ir/stmt_emit.R:669: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:670: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:699: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:756: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:763: return(.mojor_ir_loop_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, unroll, schedule))
packages/mojor/R/ir/stmt_emit.R:767: return(.mojor_ir_while_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:792: return(.mojor_ir_repeat_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:795: return(.mojor_ir_if_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:798: return(.mojor_ir_block_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:819:.mojor_ir_rng_vec_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:966: nrow_var <- if (!is.null(nrow_map) && !is.null(nrow_map[[x_name]])) nrow_map[[x_name]] else paste0("nrow_", x_name, "_i")
packages/mojor/R/ir/stmt_emit.R:967: ncol_var <- if (!is.null(ncol_map) && !is.null(ncol_map[[x_name]])) ncol_map[[x_name]] else paste0("ncol_", x_name, "_i")
packages/mojor/R/ir/stmt_emit.R:969: len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")
packages/mojor/R/ir/stmt_emit.R:1085: len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")
packages/mojor/R/ir/stmt_emit.R:1115:.mojor_ir_apply_assign_emit <- function(node, out_name, kernel_out_name = NULL, type_env = NULL, indent = " ", zero_based_vars = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1175:.mojor_ir_apply_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1246: paste0(indent, " var ", val_var, " = ", meta$read_helper, "(", meta$x_read_name, ", ", idx_var, ", Int(", meta$len_var, "))")
packages/mojor/R/ir/stmt_emit.R:1427: idx_one_based_str <- .mojor_ir_expr_emit(rhs_index$indices[[1]], zero_based_vars, type_env, loop_var, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:1491: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:1492: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:1519: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1523: lines <- c(lines, paste0(indent, " var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
packages/mojor/R/ir/stmt_emit.R:1527: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1534: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1537: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1544: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1553: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1589: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1593: lines <- c(lines, paste0(indent, " var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
packages/mojor/R/ir/stmt_emit.R:1597: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1604: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1607: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1610: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1614: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1617: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1623: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1628: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1640:.mojor_ir_sample_int_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1800: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:1808: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:1850:.mojor_ir_sample_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:2024: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:2032: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:2242: loop_var <- .mojor_unique_loop_var("__mojor_hof_i", c(out_name, arg_names, names(type_env), if (is.null(zero_based_vars)) character(0) else zero_based_vars))
packages/mojor/R/ir/stmt_emit.R:2267:.mojor_ir_phase_string_assign_emit <- function(node, out_name, type_env = NULL, indent = " ", zero_based_vars = NULL) {
packages/mojor/R/ir/stmt_emit.R:2367: assign_target <- if (!is.null(out_type) && identical(out_type, "chr[]")) paste0(out_name, "[Int(0)]") else out_name
packages/mojor/R/ir/stmt_emit.R:2414: start_str <- .mojor_ir_expr_emit(node$start, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:2415: stop_str <- .mojor_ir_expr_emit(node$stop, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:2458:.mojor_ir_phase_set_match_call <- function(kind, type_suffix, x_ptr, x_len, out_name = NULL, table_ptr = NULL, table_len = NULL, indent = "") {
packages/mojor/R/ir/stmt_emit.R:2468: if (!(kind %in% c("match", "in")) || is.null(out_name) || !nzchar(out_name) || is.null(table_ptr) || is.null(table_len)) {
packages/mojor/R/ir/stmt_emit.R:2471: paste0(indent, "mojor_", kind, "_", type_suffix, "(", x_ptr, ", ", x_len, ", ", table_ptr, ", ", table_len, ", ", out_name, ")")
packages/mojor/R/ir/stmt_emit.R:2545: na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, NULL, type_env, loop_vars = NULL) else "False"
packages/mojor/R/ir/stmt_emit.R:2549: return(paste0(indent, "mojor_quantile_", type_suffix, "(", x_ptr, ", ", x_len, ", ", probs_ptr, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", ", out_name, ")"))
packages/mojor/R/ir/ssa.R:843: kind = "ssa_stmt", id = cond_id, op = "loop_cond", args = c(iter_param, end_val$value, step_val$value),
packages/mojor/R/ir/ssa.R:1943: header = header, nodes = nodes, continue_block = if (!is.null(m$continue_dest$block)) as.character(m$continue_dest$block) else "",
packages/mojor/R/ir/ssa.R:4161: target = term$then, args = if (is.null(term$then_args)) character(0) else as.character(term$then_args)
packages/mojor/R/ir/ssa.R:4164: target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
packages/mojor/R/ir/ssa.R:4283: target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)

## packages/mojor/R
packages/mojor/R/build/build_expression.R:500: " if (__mojor_start < 1) __mojor_start = 1;", " if (__mojor_stop < __mojor_start || __mojor_start > __mojor_len || __mojor_len <= 0) {",
packages/mojor/R/build/build_expression.R:504: " const char* __mojor_raw = CHAR(__mojor_ch);", " SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_raw + __mojor_from, __mojor_n));",
packages/mojor/R/build/build_expression.R:562: " if (__mojor_sep_len > 0) memcpy(__mojor_buf + __mojor_xn, __mojor_sep, (size_t)__mojor_sep_len);",
packages/mojor/R/build/build_expression.R:564: " __mojor_buf[__mojor_total] = '\\0';", " SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_buf, __mojor_total));",
packages/mojor/R/build/build_expression.R:608: " __mojor_off += __mojor_sep_len;", " }", " memcpy(__mojor_buf + __mojor_off, __mojor_ys, (size_t)__mojor_yn);",
packages/mojor/R/build/build_expression.R:609: " __mojor_off += __mojor_yn;", " if (__mojor_i + 1 < __mojor_out_len && __mojor_collapse_len > 0) {",
packages/mojor/R/build/build_expression.R:612: " }", " __mojor_buf[__mojor_total] = '\\0';", " SEXP __mojor_out = PROTECT(Rf_ScalarString(Rf_mkCharLen(__mojor_buf, __mojor_total)));",
packages/mojor/R/build/build_expression.R:867: " if (TYPEOF(%s_dim_sexp) != INTSXP || LENGTH(%s_dim_sexp) != 2) error(\"%s: expected matrix (2D array)\");",
packages/mojor/R/build/build_expression.R:1179: if (identical(trans$return_type, "Int32")) " return Rf_ScalarInteger(result);" else " return Rf_ScalarReal(result);",
packages/mojor/R/build/build.R:352: na_mode = build_ctx$na_mode, semantics = build_ctx$semantics, opt_level = if (is.null(build_ctx$opt_level)) "" else as.character(build_ctx$opt_level),
packages/mojor/R/build/build.R:1345: c_lines, " union { int i; float f; } __mojor_f32_bits;", " #define MOJOR_BITS_TO_F32(bits) (__mojor_f32_bits.i = (bits), __mojor_f32_bits.f)"
packages/mojor/R/build/build.R:1370: " if (IS_S4_OBJECT(%s)) { SEXP __mc_%s = R_do_slot(%s, install(\"Data\")); if (__mc_%s == R_NilValue || LENGTH(__mc_%s) == 0) error(\"%s: zero-length float32 array (memory_check)\"); }",
packages/mojor/R/build/build.R:1388: " if (TYPEOF(%s) == REALSXP && LENGTH(%s) > 0 && ISNAN(REAL(%s)[0])) error(\"%s: scalar is NA/NaN (memory_check)\");",
packages/mojor/R/build/build.R:1395: " if (TYPEOF(%s) == INTSXP && LENGTH(%s) > 0 && INTEGER(%s)[0] == NA_INTEGER) error(\"%s: scalar is NA (memory_check)\");",
packages/mojor/R/build/build.R:1402: " if (TYPEOF(%s) == LGLSXP && LENGTH(%s) > 0 && LOGICAL(%s)[0] == NA_LOGICAL) error(\"%s: scalar is NA (memory_check)\");",
packages/mojor/R/build/build.R:1489: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) __mojor_%s_cast[__mojor_i_%s] = (double) INTEGER(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1508: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) REAL(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1512: " for (int __mojor_i_%s = 0; __mojor_i_%s < __mojor_len_%s_cast; __mojor_i_%s++) %s_ptr[__mojor_i_%s] = (float) INTEGER(%s)[__mojor_i_%s];",
packages/mojor/R/build/build.R:1535: " if (TYPEOF(%s_data) != INTSXP || LENGTH(%s_data) != 1) error(\"%s: expected float32 scalar data\");",
packages/mojor/R/build/build.R:1546: " if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");",
packages/mojor/R/build/build.R:1581: " if ((__mojor_seq_by > 0.0 && __mojor_seq_to < __mojor_seq_from) || (__mojor_seq_by < 0.0 && __mojor_seq_to > __mojor_seq_from)) error(\"seq(): wrong sign in 'by' argument\");"
packages/mojor/R/build/build.R:1846: c_lines, if (a %in% nrow_arrays) NULL else sprintf(" SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
packages/mojor/R/build/build.R:1871: c_lines, if (a %in% nrow_arrays) NULL else sprintf(" SEXP dim_%s = getAttrib(%s, R_DimSymbol);", a, a),
packages/mojor/R/build/build.R:2126: c_lines, " if (__mojor_out_nrow <= 0 || __mojor_out_ncol <= 0) error(\"output matrix dims must be positive\");"
packages/mojor/R/build/build.R:2155: "; di++) {", " if (", .mojor_out_dim_param_name(), "_ptr[di] <= 0) error(\"output array dims must be positive\");",
packages/mojor/R/build/build.R:2311: c_lines, " SEXP dim = PROTECT(allocVector(INTSXP, 2));", paste0(" INTEGER(dim)[0] = ", .mojor_out_nrow_param_name(), ";"),
packages/mojor/R/build/build.R:3072: max_bytes = .mojor_state$options$jit_cache_max_bytes, max_entries = .mojor_state$options$jit_cache_max_entries,
packages/mojor/R/build/build.R:3350: max_entries = .mojor_state$options$jit_cache_max_entries, max_age_days = .mojor_state$options$jit_cache_max_age_days,
packages/mojor/R/ir/verify.R:164: "IR verify [%s]: FUN named function '%s' is unsupported for arity %d; use an anonymous function or one of: %s",
packages/mojor/R/ir/verify.R:1515: stop("IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'; otherwise assign the result to a variable")
packages/mojor/R/ir/verify.R:1360: if (!is.numeric(node$expected_arity) || length(node$expected_arity) != 1 || is.na(node$expected_arity) || node$expected_arity < 0) {
packages/mojor/R/ir/verify.R:1428: if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
packages/mojor/R/transpile/loop_codegen_units.R:55: if (had_skip_when_tiled) ctx$skip_when_tiled <- prev_skip_when_tiled else rm(list = "skip_when_tiled", envir = ctx, inherits = FALSE)
packages/mojor/R/transpile/loop_codegen_units.R:56: if (had_tiling_applied) ctx$tiling_applied <- prev_tiling_applied else rm(list = "tiling_applied", envir = ctx, inherits = FALSE)
packages/mojor/R/transpile/loop_codegen_units.R:309: loop_body_expr <- .mojor_build_transpile_loop_body_expr(li, short_ir_fallback = short_ir_fallback, scalar_init = scalar_init)
packages/mojor/R/transpile/helpers_core.R:656:.mojor_phase_runtime_regex <- function(op, pattern, x, replacement = NULL, fixed = FALSE, perl = TRUE, context = "mojor_build") {
packages/mojor/R/transpile/helpers_core.R:853:.mojor_signal_error <- function(message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL, severity = "error") {
packages/mojor/R/transpile/helpers_core.R:870:.mojor_add_diag <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:1166:mojor_diagnostics_add <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:1168: .mojor_add_diag(severity, message, file = file, line = line, col = col, end_line = end_line, end_col = end_col, expr = expr)
packages/mojor/R/transpile/helpers_core.R:1514: stop("Indexing must use a loop variable (optionally +/- integer literal, scalar variable, or multiplied/divided by integer)")
packages/mojor/R/transpile/helpers_core.R:1522: .mojor_warn(paste0(inner_op, "() lowered to recycling"), expr[[2]], "length.out/times ignored; uses output length")
packages/mojor/R/transpile/helpers_core.R:1530: if (length(inner_parts) >= pos && (is.null(inner_nms) || is.null(inner_nms[[pos]]) || inner_nms[[pos]] == "")) {
packages/mojor/R/transpile/helpers_core.R:1539: if (is.null(len_expr)) .mojor_err("rep_len() length.out must be an integer literal or scalar arg", expr[[2]])
packages/mojor/R/transpile/helpers_core.R:1757:.mojor_elementwise_plan <- function(li, out_name, out_kind, out_type, elementwise_target, elementwise_cpu, non_unit_range,
packages/mojor/R/transpile/helpers_core.R:1952: if (is.null(spec) || (gpu_buf_dtype == "f32" && spec != "f32[]") || (gpu_buf_dtype == "f64" && spec != "f64[]")) {
packages/mojor/R/transpile/helpers_core.R:1954: gpu_buf_reason <- if (gpu_buf_dtype == "f64") "gpu buffers require f64[] inputs" else "gpu buffers require f32[] inputs"
packages/mojor/R/transpile/helpers_core.R:2036:.mojor_expr_to_mojo <- function(expr, loop_vars, types, in_cond = FALSE, zero_based_vars = NULL, idx_override = NULL, idx_zero_based = FALSE, suppress_len_checks = FALSE, suppress_read_helpers = FALSE) {
packages/mojor/R/transpile/helpers_core.R:2322: return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
packages/mojor/R/transpile/helpers_core.R:2347: return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
packages/mojor/R/transpile/helpers_core.R:2609:.mojor_ifelse_assign_vector <- function(lhs_var, idx, cond, yes, no, loop_vars, types, target_spec, indent, target_expr = NULL) {
packages/mojor/R/transpile/helpers_core.R:2695:.mojor_ifelse_assign_scalar <- function(lhs_var, cond, yes, no, loop_vars, types, target_spec, indent, break_on_na = TRUE) {
packages/mojor/R/transpile/helpers_core.R:2780: .mojor_warn("ifelse() expression lowered to a temporary", expr, "prefer explicit loop assignment for best performance")
packages/mojor/R/transpile/helpers_core.R:2931: .mojor_err("unsupported any()/all() expression", expr, "use array expressions with &, |, comparisons, or is.na/is.nan/is.finite/is.infinite")
packages/mojor/R/transpile/helpers_core.R:3100: lines <- c(lines, sprintf(" if (%s > __mojor_bc_ndim) __mojor_bc_ndim = %s;", .mojor_ndim_param_name(a), .mojor_ndim_param_name(a)))
packages/mojor/R/transpile/helpers_core.R:3140: sprintf(" if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");", a, a, a, a),
packages/mojor/R/transpile/helpers_core.R:3141: sprintf(" %s %s_val = (TYPEOF(%s) == REALSXP) ? (%s) REAL(%s)[0] : (%s) INTEGER(%s)[0];", c_type, a, a, c_type, a, c_type, a)
packages/mojor/R/transpile/entry_phases.R:282: elementwise_gpu_layouttensor <- .mojor_assert_scalar_logical_or_null(elementwise_gpu_layouttensor, "elementwise_gpu_layouttensor")
packages/mojor/R/transpile/entry_phases.R:425: helper_lines, "fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float64:",
packages/mojor/R/transpile/entry_phases.R:441: helper_lines, "fn _mojor_read_f32(ptr: ImmutF32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Float32:",
packages/mojor/R/transpile/entry_phases.R:457: helper_lines, "fn _mojor_read_i32(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
packages/mojor/R/transpile/entry_phases.R:473: helper_lines, "fn _mojor_read_lgl(ptr: ImmutInt32Ptr, idx: Int, upper: Int, var_name: StringLiteral, file: StringLiteral, line: Int) raises -> Int32:",
packages/mojor/R/transpile/entry_phases.R:495: gpu_helper_lines, "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF64Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float64:",
packages/mojor/R/transpile/entry_phases.R:516: " return _MOJOR_NAN", "fn _mojor_gpu_reduce[dims_t: AnyType](value: ImmutF32Ptr, op: String, dims: dims_t, keepdims: Bool, dims_default: Bool, n_i: Int) -> Float32:",
packages/mojor/R/transpile/entry_phases.R:542: gpu_helper_lines, "fn _mojor_gpu_matmul_into(out: MutF64Ptr, a: ImmutF64Ptr, b: ImmutF64Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF64Ptr:",
packages/mojor/R/transpile/entry_phases.R:556: "fn _mojor_gpu_matmul_into(out: MutF32Ptr, a: ImmutF32Ptr, b: ImmutF32Ptr, transpose_a: Bool, transpose_b: Bool, a_nrow: Int, a_ncol: Int, b_nrow: Int, b_ncol: Int, out_nrow: Int, out_ncol: Int) -> MutF32Ptr:",
packages/mojor/R/transpile/entry_phases.R:570: "fn _mojor_gpu_matmul[a_t: AnyType, b_t: AnyType](a: a_t, b: b_t, transpose_a: Bool, transpose_b: Bool) raises -> a_t:",
packages/mojor/R/transpile/entry_phases.R:652: opt_level_fallback = phase$ssa_backend_opt_fallback, opt_level_fallback_reason = phase$ssa_backend_opt_fallback_reason
packages/mojor/R/transpile/entry_phases.R:668: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/transpile/entry_phases.R:705: ssa_backend_opt_fallback = ssa_backend_opt_fallback, ssa_backend_opt_fallback_reason = ssa_backend_opt_fallback_reason
packages/mojor/R/transpile/prelude_helpers.R:100: .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- build_iter_expr_entry(loop_infos[[1]], loop_var)
packages/mojor/R/transpile/prelude_helpers.R:102: .mojor_state$current_iter_map[[loop_infos[[1]]$iter_value]] <- list(source = loop_infos[[1]]$iter_source, index = loop_var)
packages/mojor/R/core/gpu.R:887: "args <- commandArgs(trailingOnly = TRUE)", "if (length(args) < 2L) stop('probe runner requires request/result paths')",
packages/mojor/R/core/gpu.R:888: "req <- readRDS(args[[1L]])", "out_path <- args[[2L]]", "res <- list(available = FALSE, reason = 'probe runner did not execute')",
packages/mojor/R/core/gpu.R:889: "tryCatch({", " source(req$mojor_script)", " if (is.character(req$mojor_lib_path) && length(req$mojor_lib_path) == 1L && nzchar(req$mojor_lib_path)) {",
packages/mojor/R/core/gpu.R:892: " if (!isTRUE(mojor_is_loaded()) || !isTRUE(mojor_has_gpu())) stop('GPU not available for f64 probe subprocess', call. = FALSE)",
packages/mojor/R/core/gpu.R:903: " out <- tryCatch({", " .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode, op_class = op_class)",
packages/mojor/R/core/gpu.R:908: " stop('unsupported probe kind: ', kind)", " }", " if (!is.list(out) || is.null(out$available)) stop('probe subprocess produced invalid result')",
packages/mojor/R/core/gpu.R:4139: gather_out <- .mojor_gpu_try_gather_linearized(x, idx_vals = idx_vals, missing_flags = missing_flags, drop = drop)
packages/mojor/R/transpile/ir_helpers.R:77: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/core/reductions_jit.R:535: elementwise_target = elementwise_target, elementwise_size = if (is.null(elementwise_size)) "" else as.character(elementwise_size),
packages/mojor/R/ir/op_schema.R:718: loop_id = loop_id, kind = p$kind, anchor_block = bn, continue_dest = list(loop_id = loop_id, role = "Continue", block = bn),
packages/mojor/R/transpile/helpers_codegen.R:1180: "unsupported loop sequence", seq_expr, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
packages/mojor/R/transpile/helpers_codegen.R:1185: "unsupported loop sequence", seq_expr, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
packages/mojor/R/transpile/helpers_codegen.R:2234: n_var = "n_i", out_name = out_name, out_type = out_type, out_matrix = isTRUE(.mojor_state$current_out_is_matrix),
packages/mojor/R/transpile/helpers_codegen.R:2818: "var _mojor_out_tensor = LayoutTensor[mut=True, DType.int64, _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](_mojor_out_ptr, _mojor_out_layout)",
packages/mojor/R/transpile/helpers_codegen.R:3053: kernel_sig_parts <- .mojor_append_gpu_kernel_broadcast_sig(kernel_sig_generic, kernel_sig_wrapper, kernel_call, dim_arrays)
packages/mojor/R/transpile/helpers_codegen.R:3182: " var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/helpers_codegen.R:3185: " @parameter", " @always_inline", " fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/transpile/helpers_codegen.R:3205: " fn _mojor_elementwise_", gpu_dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/ssa_verify.R:306: severity = "warning", block = "", message = "post_recanonicalize boundary checks disabled because loop recovery status is Unknown",
packages/mojor/R/core/core.R:774: effective_ir_only <- .mojor_resolve_effective_ir_only(primary = build_args, secondary = user_extra, context = "mojor_fn")
packages/mojor/R/core/core.R:1991: current_index_base = NULL, current_array_layout = NULL, current_ir_only = isTRUE(.mojor_options_defaults()$ir_only),
packages/mojor/R/transpile/expression_only.R:503: if (identical(op_name, "paste")) "paste(x, y, sep = \"...\", collapse = \"...\"|NULL)" else "paste0(x, y, collapse = \"...\"|NULL)",
packages/mojor/R/transpile/expression_only.R:539: stop("mojor_transpile: ", op_label, "() collapse must be a scalar character literal or NULL in expression-only mode")
packages/mojor/R/transpile/expression_only.R:582: "() currently supports only ", op_name, "(pattern, x, fixed = TRUE|FALSE, perl = TRUE|FALSE) in expression-only mode"
packages/mojor/R/transpile/expression_only.R:699: stop("mojor_transpile: ", op_name, "() named FUN is unsupported for arity ", expected_arity, " in expression-only mode")
packages/mojor/R/transpile/expression_only.R:707: stop("mojor_transpile: ", op_name, "() requires FUN to be an anonymous function or supported named function in expression-only mode")
packages/mojor/R/transpile/expression_only.R:879: stop("mojor_transpile: mapply() requires all vector arguments to have the same element type in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1002: stop("mojor_transpile: ", op_label, "() ", label, " must be a direct character vector argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1022: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1030: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1095: if (!(is.null(collapse_value) || (is.character(collapse_value) && length(collapse_value) == 1 && !is.na(collapse_value)))) {
packages/mojor/R/transpile/expression_only.R:1130: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct character vector argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1138: stop("mojor_transpile: ", op_label, "() ", arg_label, " must have type chr[] in expression-only mode; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1150: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1157: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1169: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a numeric/integer/logical matrix argument; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1183: stop("mojor_transpile: ", op_label, "() currently supports only f64[], i32[], lgl[] vector arguments; got: ", spec)
packages/mojor/R/transpile/expression_only.R:1256: if ((op %in% c("sub", "gsub")) && identical(replacement_info$kind, "const") && has_backref(replacement_info$value)) {
packages/mojor/R/transpile/expression_only.R:1262: if (!is.null(replacement_info) && identical(replacement_info$kind, "arg")) kernel_args <- c(kernel_args, replacement_info$name)
packages/mojor/R/transpile/expression_only.R:1473: paste0(" _ = mojor_match_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
packages/mojor/R/transpile/expression_only.R:1479: paste0(" _ = mojor_in_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
packages/mojor/R/transpile/expression_only.R:2246: stop("mojor_transpile: quantile() probs must be a function parameter or supported compile-time numeric vector expression in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1604: paste0(" _ = mojor_quantile_", type_suffix, "(", x_name, ", n_", x_name, ", ", probs_name, ", n_", probs_name, ", ", type_str, ", ", na_rm_str, ", out)"),
packages/mojor/R/transpile/expression_only.R:1672: paste0(" return mojor_mad_", type_suffix, "(", x_name, ", n_", x_name, ", center, ", na_rm_str, ", 1.4826, ", type_str, ")")
packages/mojor/R/transpile/expression_only.R:1722: stop("mojor_transpile: ", op_label, "() supports at most ", max_positional, " positional arguments in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2660: stop("mojor_transpile: ", op_label, "() weighted sampling currently requires prob to be a direct f64[] argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2668: stop("mojor_transpile: ", op_label, "() weighted sampling requires prob argument '", nm, "' to be typed f64[] in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1793: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1809: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:1825: stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2660: stop("mojor_transpile: ", op_label, "() weighted sampling currently requires prob to be a direct f64[] argument in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2668: stop("mojor_transpile: ", op_label, "() weighted sampling requires prob argument '", nm, "' to be typed f64[] in expression-only mode")
packages/mojor/R/transpile/expression_only.R:2156: stop("mojor_transpile: expression-only mode currently only supports standalone reductions (sum, mean, min, max, prod, sd, var), matrix operations (%*%, crossprod, tcrossprod), covariance/correlation (cov, cor), set/match primitives (unique, duplicated, anyDuplicated, match, %in%), quantile/robust stats (median, quantile, IQR, mad), higher-order functions (vapply, sapply, lapply, mapply) under strict inline subset, sampling (sample.int, sample) under strict scalar-arg subset, native string subset (nchar, nzchar, substr, substring, paste, paste0) under strict chr[] direct-arg constraints, /9.3 regex-table subset (grepl, grep, sub, gsub, row, col, expand.grid) under strict direct-arg constraints, and scalar expressions.")
packages/mojor/R/transpile/expression_only.R:2370: stop("mojor_transpile: ", operation, " left-hand argument '", lhs_name, "' must be a matrix type (f64[,]), got: ", lhs_type)
packages/mojor/R/transpile/expression_only.R:2376: stop("mojor_transpile: ", operation, " right-hand argument must be a simple variable name, got: ", deparse(rhs_expr))
packages/mojor/R/transpile/expression_only.R:2386: stop("mojor_transpile: ", operation, " right-hand argument '", rhs_name, "' must be a matrix type (f64[,]), got: ", rhs_type)
packages/mojor/R/transpile/expression_only.R:2571: stop("mojor_transpile: ", operation, "() with single argument requires a matrix, got vector. Use var(x) for variance or provide two vector arguments.")
packages/mojor/R/ir/ssa.R:843: kind = "ssa_stmt", id = cond_id, op = "loop_cond", args = c(iter_param, end_val$value, step_val$value),
packages/mojor/R/ir/ssa.R:1943: header = header, nodes = nodes, continue_block = if (!is.null(m$continue_dest$block)) as.character(m$continue_dest$block) else "",
packages/mojor/R/ir/ssa.R:4161: target = term$then, args = if (is.null(term$then_args)) character(0) else as.character(term$then_args)
packages/mojor/R/ir/ssa.R:4164: target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
packages/mojor/R/ir/ssa.R:4283: target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
packages/mojor/R/transpile/helpers_analysis.R:1041: .mojor_state$current_local_vector_lengths[[out_name]] <- list(length = resolved_len, type = out_type)
packages/mojor/R/transpile/helpers_analysis.R:2109: "sum()/prod()/mean()/var()/sd()/min()/max()/which.min()/which.max() reductions require a typed array argument",
packages/mojor/R/transpile/helpers_analysis.R:2150: "sum()/prod()/mean()/var()/sd()/min()/max()/which.min()/which.max() reductions require an array variable",
packages/mojor/R/transpile/helpers_analysis.R:2217: "which.min()/which.max() expression reductions could not be lowered to element-wise selector access",
packages/mojor/R/transpile/helpers_analysis.R:3500: "mojor_transpile: matrix(dimnames=...) must be a list of character literals like list(c(\"a\", \"b\"), c(\"c\", \"d\"))"
packages/mojor/R/transpile/helpers_analysis.R:3918: "mojor_transpile: unsupported length expression (use length(name), scalar name, literal, +, -, *, %/%, %%, min/max)"
packages/mojor/R/transpile/helpers_analysis.R:4023: "mojor_transpile: unsupported float expression (use literals, scalar names, length(name), +, -, *, /, %/%, %%, min/max)"
packages/mojor/R/transpile/gpu_emit.R:146: " var ", .mojor_out_dim_var_name(), "_stage = ctx.enqueue_create_host_buffer[DType.int32](",
packages/mojor/R/transpile/gpu_emit.R:347: " var ", a, "_dev = LayoutTensor[mut=False, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/gpu_emit.R:355: " var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
packages/mojor/R/transpile/gpu_emit.R:358: " @parameter", " @always_inline", " fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/loop_recanonicalize.R:157: loop_id = loop_id, role = "Continue", block = if (!is.null(li$header)) as.character(li$header) else NULL
packages/mojor/R/ir/loop_recanonicalize.R:159: break_dest = if (!is.null(li$break_dest)) li$break_dest else list(loop_id = loop_id, role = "Break", block = NULL)
packages/mojor/R/ir/stmt_loop_emit.R:616: " var __mojor_r = __mojor_l + 1", paste0(" var __mojor_lv: ", dtype, " = temp_val[__mojor_l]"),
packages/mojor/R/ir/stmt_loop_emit.R:736: " for __mojor_sr_i in range(n_chunks):", " var __mojor_chunk_base = __mojor_sr_i * simd_width",
packages/mojor/R/ir/stmt_loop_emit.R:752: " elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:",
packages/mojor/R/ir/stmt_loop_emit.R:756: " __mojor_best_v = __mojor_chunk_best_v", " __mojor_best_idx = __mojor_chunk_best_idx",
packages/mojor/R/ir/ssa_semantic.R:93: "^condbr\\s+(%[A-Za-z][A-Za-z0-9_]*)\\s*\\?\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*:\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*$",
packages/mojor/R/ir/stmt_assign_emit.R:668: base_name, mask_name, rhs_mask_var, rhs_mask_name, if (!is.null(type_env)) names(type_env) else character(0),
packages/mojor/R/ir/type_guards.R:77: layout_kind <- .mojor_state$`%||%`(.mojor_state$current_array_layout, .mojor_state$options$array_layout)
packages/mojor/R/ir/fusion.R:139: order <- if (!is.null(step) && step$kind == "const" && (as.character(step$value) == "1" || as.numeric(step$value) > 0)) "ascending" else "descending"
packages/mojor/R/transpile/transpile.R:279: pre_loop_out_assigns <- .mojor_collect_pre_loop_out_assignments(blocks = blocks, out_name = out_name, scalar_inits = scalar_inits)
packages/mojor/R/transpile/transpile.R:555: loop_infos[[i]]$iter_expr_transform <- info_i$iter_expr_transform # Preserve division/multiplication transforms
packages/mojor/R/transpile/transpile.R:1930: debug_imports <- "from debug_helpers import mojor_assert, mojor_check_bounds, mojor_check_bounds_2d, mojor_trace, mojor_trace_value, mojor_trace_value_int, mojor_trace_enter, mojor_trace_exit"
packages/mojor/R/transpile/transpile.R:1934: debug_imports, ", mojor_check_ptr_f64, mojor_check_ptr_f64_mut, mojor_check_ptr_f32, mojor_check_ptr_f32_mut, mojor_check_ptr_i32, mojor_check_ptr_i32_mut, mojor_check_length"
packages/mojor/R/transpile/transpile.R:1984: "comptime MutBufF32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1985: "comptime MutBufF64Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1992: " var buf: MutBufF64Ptr", " var len: Int", "comptime MutGpuBufF32Ptr = UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin]",
packages/mojor/R/transpile/transpile.R:1999: " return total <= MOJOR_GPU_MAX_BYTES", "fn _mojor_set_status(ptr: MutOpaqueAny, value: Int32) -> None:",
packages/mojor/R/transpile/transpile.R:2013: mojo_lines, "from nn.argmaxmin import argmax, argmin", paste0("comptime _MOJOR_ARGEXT_LAYOUT = ", layout_ctor(1, "0"))
packages/mojor/R/transpile/transpile.R:2034: "from math import exp, log, log1p, sqrt", paste0("from rng_helpers import ", paste(rng_helper_syms, collapse = ", "))
packages/mojor/R/transpile/transpile.R:2038: rng_imports, "from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE, _ZIGGURAT_NOR_R, _ZIGGURAT_NOR_INV_R"
packages/mojor/R/transpile/transpile.R:2065: "from na_helpers import _mojor_lt_f64_na, _mojor_gt_f64_na, _mojor_le_f64_na, _mojor_ge_f64_na, _mojor_eq_f64_na, _mojor_ne_f64_na"
packages/mojor/R/transpile/transpile.R:2071: "from na_helpers import _mojor_lt_f32_na, _mojor_gt_f32_na, _mojor_le_f32_na, _mojor_ge_f32_na, _mojor_eq_f32_na, _mojor_ne_f32_na"
packages/mojor/R/transpile/transpile.R:2090: "from set_match_helpers import mojor_any_duplicated_f64, mojor_any_duplicated_f32, mojor_any_duplicated_i32"
packages/mojor/R/transpile/transpile.R:2248: mojo_lines, "fn _mojor_bcast_index(out_idx: Int, out_dim: ImmutInt32Ptr, out_ndim: Int, arg_dim: ImmutInt32Ptr, arg_ndim: Int) -> Int:",
packages/mojor/R/transpile/transpile.R:2647: " var __mojor_rng_wi = materialize[_WI_DOUBLE]()", " var __mojor_rng_fi = materialize[_FI_DOUBLE]()"
packages/mojor/R/transpile/transpile.R:3019: array_layout = .mojor_state$current_array_layout, fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
packages/mojor/R/ir/ir.R:361: indent, "fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/ir.R:384: indent, "fn _mojor_elementwise_", dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
packages/mojor/R/ir/expr_emit.R:1:.mojor_ir_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
packages/mojor/R/ir/expr_emit.R:42: if (length(requested_num) != 1 || !is.finite(requested_num) || requested_num != floor(requested_num) || requested_num < 1) {
packages/mojor/R/ir/expr_emit.R:156: "_mojor_sample_pick_index(__mojor_rng_state, Int(", n_str, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
packages/mojor/R/ir/expr_emit.R:188: "_mojor_sample_pick_index(__mojor_rng_state, Int(", x_len, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
packages/mojor/R/ir/expr_emit.R:548: args[i] <- .mojor_ir_expr_emit(node$args[[i]], zero_based_vars, type_env, loop_vars, index_context = index_context)
packages/mojor/R/ir/expr_emit.R:600: return(if (is_float) paste0("((", args[[1]], " == ", inf_pos, ") or (", args[[1]], " == ", inf_neg, "))") else "False")
packages/mojor/R/ir/expr_emit.R:603: return(if (is_float) paste0("((", args[[1]], " != ", inf_pos, ") and (", args[[1]], " != ", inf_neg, ") and (", args[[1]], " == ", args[[1]], "))") else "True")
packages/mojor/R/ir/expr_emit.R:754: if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
packages/mojor/R/ir/expr_emit.R:1030: return(paste0("(", from_str, " if (", length_out_str, " == 1) else (", from_str, " + ", pos0, " * (", to_str, " - ", from_str, ") / Float64((", length_out_str, " - 1))))"))
packages/mojor/R/ir/expr_emit.R:1714: table_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[table_name]])) len_var_map[[table_name]] else "n_i"
packages/mojor/R/ir/expr_emit.R:1759: probs_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[probs_name]])) len_var_map[[probs_name]] else "n_i"
packages/mojor/R/ir/expr_emit.R:1761: na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, zero_based_vars, type_env, loop_vars) else "False"
packages/mojor/R/ir/expr_emit.R:1765: return(paste0("mojor_quantile_", type_suffix, "(", x_name, ", ", x_len, ", ", probs_name, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", out)"))
packages/mojor/R/ir/expr_emit.R:1946:.mojor_ir_rng_vec_scalar_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
packages/mojor/R/ir/ssa_backend.R:168: "SSA backend invariant: expected phase order to_ssa -> memory_canonicalize -> annotate_effects_resources -> typing -> backend_cfg"
packages/mojor/R/ir/ssa_backend.R:1154: args = args, index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds),
packages/mojor/R/ir/ssa_backend.R:2007: fusion <- .mojor_ssa_fusion_candidate_analysis(ssa_recanonical, recovery = loop_recovery, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2008: ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(ssa_recanonical, analysis = fusion, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2025: ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(fusion_rewrite_result$ssa, analysis = fusion, policy = fusion_policy)
packages/mojor/R/ir/ssa_backend.R:2486: formatted_selected = if (!is.null(selected)) .mojor_ir_ssa_backend_selected_format(selected, verify = FALSE) else NULL,
packages/mojor/R/ir/ssa_backend.R:2487: formatted_codegen = if (!is.null(codegen)) .mojor_ir_ssa_backend_codegen_format(codegen, target = backend_target, verify = FALSE) else NULL
packages/mojor/R/ir/stmt_emit.R:1:.mojor_ir_stmt_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid", bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL, unroll = NULL, schedule = NULL, bounds_guard_cache = NULL, na_guard_cache = NULL) {
packages/mojor/R/ir/stmt_emit.R:123: out_lines <- c(paste0(indent, "var ", lhs_name, ": Array[Float64, 1] = Array[Float64, 1](", length(values), ")"))
packages/mojor/R/ir/stmt_emit.R:296: read_expr <- paste0(read_helper_for_spec(src_spec), "(", src_name, ", ", idx_expr, ", Int(", src_len, "))")
packages/mojor/R/ir/stmt_emit.R:370: arr_dims <- if (!is.null(local_array_dims) && !is.null(local_array_dims[[lhs_name]])) local_array_dims[[lhs_name]]$dim else NULL
packages/mojor/R/ir/stmt_emit.R:558: if (node$rhs$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$src)) {
packages/mojor/R/ir/stmt_emit.R:563: if (node$rhs$expr$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$expr$src)) {
packages/mojor/R/ir/stmt_emit.R:572: ctor_expr <- if (is.list(ctor_expr_node) && !is.null(ctor_expr_node$kind)) ctor_expr_node else ctor_expr_node$src
packages/mojor/R/ir/stmt_emit.R:669: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:670: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:699: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:756: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:763: return(.mojor_ir_loop_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, unroll, schedule))
packages/mojor/R/ir/stmt_emit.R:767: return(.mojor_ir_while_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:792: return(.mojor_ir_repeat_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:795: return(.mojor_ir_if_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:798: return(.mojor_ir_block_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
packages/mojor/R/ir/stmt_emit.R:819:.mojor_ir_rng_vec_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:966: nrow_var <- if (!is.null(nrow_map) && !is.null(nrow_map[[x_name]])) nrow_map[[x_name]] else paste0("nrow_", x_name, "_i")
packages/mojor/R/ir/stmt_emit.R:967: ncol_var <- if (!is.null(ncol_map) && !is.null(ncol_map[[x_name]])) ncol_map[[x_name]] else paste0("ncol_", x_name, "_i")
packages/mojor/R/ir/stmt_emit.R:969: len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")
packages/mojor/R/ir/stmt_emit.R:1085: len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")
packages/mojor/R/ir/stmt_emit.R:1115:.mojor_ir_apply_assign_emit <- function(node, out_name, kernel_out_name = NULL, type_env = NULL, indent = " ", zero_based_vars = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1175:.mojor_ir_apply_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1246: paste0(indent, " var ", val_var, " = ", meta$read_helper, "(", meta$x_read_name, ", ", idx_var, ", Int(", meta$len_var, "))")
packages/mojor/R/ir/stmt_emit.R:1427: idx_one_based_str <- .mojor_ir_expr_emit(rhs_index$indices[[1]], zero_based_vars, type_env, loop_var, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:1491: bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
packages/mojor/R/ir/stmt_emit.R:1492: na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
packages/mojor/R/ir/stmt_emit.R:1519: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1523: lines <- c(lines, paste0(indent, " var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
packages/mojor/R/ir/stmt_emit.R:1527: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1534: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1537: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1544: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1553: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1589: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1593: lines <- c(lines, paste0(indent, " var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
packages/mojor/R/ir/stmt_emit.R:1597: lines <- c(lines, paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
packages/mojor/R/ir/stmt_emit.R:1604: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1607: lines <- c(lines, paste0(indent, " ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1610: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1614: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1617: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1623: lines <- c(lines, paste0(indent, " var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
packages/mojor/R/ir/stmt_emit.R:1628: lines <- c(lines, paste0(indent, " ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
packages/mojor/R/ir/stmt_emit.R:1640:.mojor_ir_sample_int_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:1800: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:1808: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:1850:.mojor_ir_sample_emit <- function(node, indent = " ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
packages/mojor/R/ir/stmt_emit.R:2024: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:2032: paste0(indent, " var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
packages/mojor/R/ir/stmt_emit.R:2242: loop_var <- .mojor_unique_loop_var("__mojor_hof_i", c(out_name, arg_names, names(type_env), if (is.null(zero_based_vars)) character(0) else zero_based_vars))
packages/mojor/R/ir/stmt_emit.R:2267:.mojor_ir_phase_string_assign_emit <- function(node, out_name, type_env = NULL, indent = " ", zero_based_vars = NULL) {
packages/mojor/R/ir/stmt_emit.R:2367: assign_target <- if (!is.null(out_type) && identical(out_type, "chr[]")) paste0(out_name, "[Int(0)]") else out_name
packages/mojor/R/ir/stmt_emit.R:2414: start_str <- .mojor_ir_expr_emit(node$start, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:2415: stop_str <- .mojor_ir_expr_emit(node$stop, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
packages/mojor/R/ir/stmt_emit.R:2458:.mojor_ir_phase_set_match_call <- function(kind, type_suffix, x_ptr, x_len, out_name = NULL, table_ptr = NULL, table_len = NULL, indent = "") {
packages/mojor/R/ir/stmt_emit.R:2468: if (!(kind %in% c("match", "in")) || is.null(out_name) || !nzchar(out_name) || is.null(table_ptr) || is.null(table_len)) {
packages/mojor/R/ir/stmt_emit.R:2471: paste0(indent, "mojor_", kind, "_", type_suffix, "(", x_ptr, ", ", x_len, ", ", table_ptr, ", ", table_len, ", ", out_name, ")")
packages/mojor/R/ir/stmt_emit.R:2545: na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, NULL, type_env, loop_vars = NULL) else "False"
packages/mojor/R/ir/stmt_emit.R:2549: return(paste0(indent, "mojor_quantile_", type_suffix, "(", x_ptr, ", ", x_len, ", ", probs_ptr, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", ", out_name, ")"))
packages/mojor/R/ir/nodes.R:102:.mojor_ir_scalar_reduce <- function(op, acc, arg, init = NULL, axis = 0L, associative = TRUE, commutative = TRUE, na_rm = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:123:.mojor_ir_scheduled_reduce <- function(mode, op, acc, arg, n_var = "n_i", dtype = NULL, init_val = NULL, empty_val = NULL, nan_val = NULL, value_cast = NULL, src = NULL) {
packages/mojor/R/ir/nodes.R:367:.mojor_ir_regex_grep <- function(pattern, x, value = FALSE, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:368: list(kind = "regex_grep", pattern = pattern, x = x, value = value, ignore_case = ignore_case, perl = perl, fixed = fixed, src = src)
packages/mojor/R/ir/nodes.R:371:.mojor_ir_regex_sub <- function(pattern, replacement, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, global = FALSE, src = NULL) {
packages/mojor/R/ir/nodes.R:372: list(kind = "regex_sub", pattern = pattern, replacement = replacement, x = x, ignore_case = ignore_case, perl = perl, fixed = fixed, global = global, src = src)
packages/mojor/R/ir/nodes.R:789: paste0("(", mean_val, " + ", sd_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
packages/mojor/R/ir/nodes.R:794: paste0("(_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", shape_val, ") / ", rate_val, ")")
packages/mojor/R/ir/nodes.R:804: paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], "))")
packages/mojor/R/ir/nodes.R:809: paste0("exp(", meanlog_val, " + ", sdlog_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
packages/mojor/R/ir/nodes.R:816: paste0("(_random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi) / sqrt(_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df_val, ") / ", df_val, "))")
packages/mojor/R/ir/nodes.R:821: paste0("((_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df1_val, ") / ", df1_val, ") / (_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df2_val, ") / ", df2_val, "))")
packages/mojor/R/ir/nodes.R:824: paste0("_random_beta(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], ", ", params[[2]], ")")
packages/mojor/R/ir/nodes.R:846: paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, (_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", size_val, ") / (", prob_val, " / (1.0 - ", prob_val, ")))))")
packages/mojor/R/ir/nodes.R:849: paste0("Float64(_random_hypergeometric(__mojor_rng_state, Int(", params[[1]], "), Int(", params[[2]], "), Int(", params[[3]], ")))")
packages/mojor/R/ir/nodes.R:1331: slice_index = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$start), .mojor_ir_expr_resources(node$end))),
packages/mojor/R/ir/nodes.R:1338: rep_len = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$x), .mojor_ir_expr_resources(node$length_out))),
packages/mojor/R/ir/nodes.R:2093:.mojor_ir_collect_loop_invariants <- function(stmt, loop_var, subexprs = list(), allow_reads = FALSE, written_resource_ids = character(0)) {
packages/mojor/R/ir/nodes.R:3005: lhs_base <- if (!is.null(stmt$lhs$base)) stmt$lhs$base else if (!is.null(stmt$lhs$var)) .mojor_ir_var(stmt$lhs$var) else NULL
packages/mojor/R/ir/nodes.R:3342: fused$metadata$elementwise$fused_count <- if (is.null(fused$metadata$elementwise$fused_count)) 2L else fused$metadata$elementwise$fused_count + 1L
packages/mojor/R/ir/nodes.R:3730: init_val <- if (op == "min") "_MOJOR_INF_F32" else if (op == "max") "_MOJOR_NINF_F32" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
packages/mojor/R/ir/nodes.R:3733: init_val <- if (op == "min") "_MOJOR_INF" else if (op == "max") "_MOJOR_NINF" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
packages/mojor/R/ir/nodes.R:3739: init_val <- if (op == "sum") "Int32(0)" else if (op == "product") "Int32(1)" else if (isTRUE(is_which)) "Int32(0)" else NULL
packages/mojor/R/ir/nodes.R:3949: if (op %in% c("c", "rep", "rep_len", "rep.int", "seq", "seq.int", "t", "cbind", "rbind", "diag", "cumsum", "cumprod", "cummax", "cummin", "mean", "var", "sd")) {
packages/mojor/R/ir/nodes.R:4014: part_lens <- vapply(parts, .mojor_ir_ctor_len_expr, character(1), type_env = type_env, len_var_map = len_var_map, n_source_name = n_source_name)
