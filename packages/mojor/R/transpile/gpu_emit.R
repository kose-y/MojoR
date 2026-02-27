# Transpile GPU elementwise buffer emission helper.

.mojor_emit_transpile_gpu_buffer_block <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(".mojor_emit_transpile_gpu_buffer_block: ctx must be an environment")
    }
    evalq(
        {
            if (!is.null(elementwise_gpu_buf_info)) {
                gpu <- elementwise_gpu_buf_info
                gpu_name <- gpu$name
                gpu_dtype_tag <- if (identical(gpu$buf_dtype, "f64"))
                  "float64" else "float32"
                use_broadcast_nd <- isTRUE(gpu$broadcast_nd)
                gpu_dim_arrays <- if (use_broadcast_nd && !is.null(gpu$dim_arrays))
                  gpu$dim_arrays else character(0)
                matrix2d_mode <- identical(gpu$index_mode, "matrix2d")
                buf_ptr_type <- if (identical(gpu$buf_dtype, "f64"))
                  "MutBufF64Ptr" else "MutBufF32Ptr"
                buf_null <- if (identical(gpu$buf_dtype, "f64"))
                  "NULL_BUF_F64" else "NULL_BUF_F32"
                mut_ptr_type <- if (identical(gpu$buf_dtype, "f64"))
                  "MutF64Ptr" else "MutF32Ptr"
                immut_ptr_type <- if (identical(gpu$buf_dtype, "f64"))
                  "ImmutF64Ptr" else "ImmutF32Ptr"
                scalar_type <- if (identical(gpu$buf_dtype, "f64"))
                  "Float64" else "Float32"
                bytes_per <- if (!is.null(gpu$bytes_per))
                  gpu$bytes_per else if (identical(gpu$buf_dtype, "f64"))
                  8L else 4L
                gpu_sig <- c(
                  "ctxp: MutCtxPtr", paste0("out_bufp: ", buf_ptr_type),
                  "n: Int32"
              )
                for (a in gpu$args) {
                  spec <- arg_specs[[a]]
                  if (.mojor_is_array(spec)) {
                    gpu_sig <- c(gpu_sig, paste0(a, "_bufp: ", buf_ptr_type))
                  } else {
                    gpu_sig <- c(gpu_sig, paste0(a, ": ", .mojor_mojo_type(spec)))
                  }
                }
                if (isTRUE(matrix2d_mode)) {
                  gpu_sig <- c(gpu_sig, "__mojor_m2_nrow: Int32", "__mojor_m2_ncol: Int32")
                }
                if (use_broadcast_nd && length(gpu_dim_arrays) >
                  0) {
                  gpu_sig <- c(
                    gpu_sig, paste0(.mojor_out_dim_param_name(), "_ptr: ImmutOpaqueAny"),
                    paste0(.mojor_out_ndim_param_name(), ": Int32")
                )
                  for (a in gpu_dim_arrays) {
                    gpu_sig <- c(
                      gpu_sig, paste0(
                        .mojor_dim_param_name(a),
                        "_ptr: ImmutOpaqueAny"
                    ),
                      paste0(
                        .mojor_ndim_param_name(a),
                        ": Int32"
                    )
                  )
                  }
                }
                gpu_sig <- c(gpu_sig, "status_ptr: MutOpaqueAny")
                gpu_lines <- c(
                  "", paste0("@export(\"", gpu_name, "\", ABI=\"C\")"),
                  paste0(
                    "fn ", gpu_name, "(", paste(gpu_sig, collapse = ", "),
                    ") -> Int32:"
                ),
                  paste0(
                    "    if ctxp == NULL_CTX or out_bufp == ", buf_null,
                    ":"
                ),
                  "        _mojor_set_status(status_ptr, -1)", "        return 0",
                  "    @parameter", "    if not has_accelerator():", "        _mojor_set_status(status_ptr, -1)",
                  "        return 0", "    var n_i = Int(n)", "    if n_i <= 0:",
                  "        _mojor_set_status(status_ptr, 0)", "        return 0"
              )
                for (a in gpu$array_args) {
                  gpu_lines <- c(
                    gpu_lines, paste0("    if ", a, "_bufp == ", buf_null, ":"),
                    "        _mojor_set_status(status_ptr, -1)", "        return 0"
                )
                }
                if (!is.null(gpu$elementwise_size)) {
                  gpu_lines <- c(
                    gpu_lines, paste0("    if n_i != ", gpu$elementwise_size, ":"),
                    "        _mojor_set_status(status_ptr, -3)", "        return 0"
                )
                }
                if (isTRUE(matrix2d_mode)) {
                  gpu_lines <- c(
                    gpu_lines,
                    "    if Int(__mojor_m2_nrow) <= 0 or Int(__mojor_m2_ncol) <= 0:",
                    "        _mojor_set_status(status_ptr, -3)",
                    "        return 0"
                  )
                }
                if (use_broadcast_nd && length(gpu_dim_arrays) >
                  0) {
                  gpu_lines <- c(
                    gpu_lines, paste0(
                      "    var ", .mojor_out_dim_var_name(), "_host: ImmutInt32Ptr = ",
                      .mojor_out_dim_param_name(), "_ptr.bitcast[Int32]()"
                  ),
                    paste0(
                      "    var ", .mojor_out_ndim_var_name(), " = Int(",
                      .mojor_out_ndim_param_name(), ")"
                  ),
                    paste0("    if ", .mojor_out_ndim_var_name(), " < 0:"),
                    "        _mojor_set_status(status_ptr, -3)", "        return 0"
                )
                  for (a in gpu_dim_arrays) {
                    gpu_lines <- c(
                      gpu_lines, paste0(
                        "    var ", .mojor_dim_var_name(a),
                        "_host: ImmutInt32Ptr = ", .mojor_dim_param_name(a),
                        "_ptr.bitcast[Int32]()"
                    ),
                      paste0(
                        "    var ", .mojor_ndim_var_name(a),
                        " = Int(", .mojor_ndim_param_name(a),
                        ")"
                    ),
                      paste0(
                        "    if ", .mojor_ndim_var_name(a),
                        " < 0:"
                    ),
                      "        _mojor_set_status(status_ptr, -3)", "        return 0"
                  )
                  }
                }
                gpu_lines <- c(
                  gpu_lines, paste0(
                    "    if not _mojor_gpu_limit_ok(n_i, 1, ", bytes_per,
                    "):"
                ),
                  "        _mojor_set_status(status_ptr, -8)", "        return 0",
                  "    try:", "        var ctx = ctxp[0]"
              )
                if (length(gpu$scalar_f64) >
                  0 && gpu_dtype_tag == "float32") {
                  for (sn in gpu$scalar_f64) {
                    gpu_lines <- c(
                      gpu_lines, paste0(
                        "        var ", sn, "_f32: Float32 = Float32(",
                        sn, ")"
                    )
                  )
                  }
                }
                if (use_broadcast_nd && length(gpu_dim_arrays) >
                  0) {
                  gpu_lines <- c(
                    gpu_lines, paste0(
                      "        var ", .mojor_out_dim_var_name(), "_stage = ctx.enqueue_create_host_buffer[DType.int32](",
                      .mojor_out_ndim_var_name(), ")"
                  ),
                    paste0(
                      "        for __mojor_di_out in range(", .mojor_out_ndim_var_name(),
                      "):"
                  ),
                    paste0(
                      "            ", .mojor_out_dim_var_name(), "_stage[__mojor_di_out] = ",
                      .mojor_out_dim_var_name(), "_host[__mojor_di_out]"
                  ),
                    paste0(
                      "        var ", .mojor_out_dim_var_name(), " = ctx.enqueue_create_buffer[DType.int32](",
                      .mojor_out_ndim_var_name(), ")"
                  ),
                    paste0(
                      "        ctx.enqueue_copy(dst_buf=", .mojor_out_dim_var_name(),
                      ", src_buf=", .mojor_out_dim_var_name(), "_stage)"
                  )
                )
                  for (a in gpu_dim_arrays) {
                    gpu_lines <- c(
                      gpu_lines, paste0(
                        "        var ", .mojor_dim_var_name(a),
                        "_stage = ctx.enqueue_create_host_buffer[DType.int32](",
                        .mojor_ndim_var_name(a),
                        ")"
                    ),
                      paste0(
                        "        for __mojor_di_", a, " in range(", .mojor_ndim_var_name(a),
                        "):"
                    ),
                      paste0(
                        "            ", .mojor_dim_var_name(a),
                        "_stage[__mojor_di_", a, "] = ", .mojor_dim_var_name(a),
                        "_host[__mojor_di_", a, "]"
                    ),
                      paste0(
                        "        var ", .mojor_dim_var_name(a),
                        " = ctx.enqueue_create_buffer[DType.int32](", .mojor_ndim_var_name(a),
                        ")"
                    ),
                      paste0(
                        "        ctx.enqueue_copy(dst_buf=", .mojor_dim_var_name(a),
                        ", src_buf=", .mojor_dim_var_name(a),
                        "_stage)"
                    )
                  )
                  }
                }
                indent_lines <- function(lines, indent) paste0(indent, lines)
                use_layouttensor <- isTRUE(elementwise_gpu_layouttensor) &&
                  !isTRUE(gpu$broadcast_nd) &&
                  !identical(gpu$index_mode, "matrix2d")
                matrix2d_use_coords <- isTRUE(matrix2d_mode) &&
                  (isTRUE(gpu$matrix2d_uses_control_flow) || isTRUE(gpu$matrix2d_uses_neighbors))
                kernel_sig_generic <- c(
                  "out_ptr: UnsafePointer[mut=True, type=Scalar[dtype], origin=MutAnyOrigin]",
                  "n: Int32"
              )
                kernel_sig_wrapper <- c(
                  paste0("out_ptr: ", mut_ptr_type),
                  "n: Int32"
              )
                kernel_call <- c(if (use_layouttensor) paste0(gpu$out_name, "_ptr") else "out_bufp[0]", "n")
                for (a in gpu$array_args) {
                  kernel_sig_generic <- c(
                    kernel_sig_generic, paste0(
                      a, ": UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin]"
                  )
                )
                  kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(a, ": ", immut_ptr_type))
                  kernel_call <- c(kernel_call, if (use_layouttensor) paste0(a, "_ptr") else paste0(a, "_bufp[0]"))
                }
                if (isTRUE(matrix2d_mode)) {
                  kernel_sig_generic <- c(
                    kernel_sig_generic,
                    "__mojor_m2_nrow: Int32",
                    "__mojor_m2_ncol: Int32"
                  )
                  kernel_sig_wrapper <- c(
                    kernel_sig_wrapper,
                    "__mojor_m2_nrow: Int32",
                    "__mojor_m2_ncol: Int32"
                  )
                  kernel_call <- c(kernel_call, "__mojor_m2_nrow", "__mojor_m2_ncol")
                }
                if (length(gpu$scalar_f32) >
                  0 && gpu_dtype_tag == "float32") {
                  for (sn in gpu$scalar_f32) {
                    kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, ": Scalar[dtype]"))
                    kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, ": ", scalar_type))
                    kernel_call <- c(kernel_call, sn)
                  }
                }
                if (length(gpu$scalar_f64) >
                  0) {
                  for (sn in gpu$scalar_f64) {
                    if (gpu_dtype_tag == "float32") {
                      kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, "_f32: Scalar[dtype]"))
                      kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, "_f32: Float32"))
                      kernel_call <- c(kernel_call, paste0(sn, "_f32"))
                    } else {
                      kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, ": Scalar[dtype]"))
                      kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, ": Float64"))
                      kernel_call <- c(kernel_call, sn)
                    }
                  }
                }
                if (use_broadcast_nd && length(gpu_dim_arrays) >
                  0) {
                  kernel_sig_generic <- c(
                    kernel_sig_generic, paste0(.mojor_out_dim_var_name(), ": ImmutInt32Ptr"),
                    paste0(.mojor_out_ndim_var_name(), ": Int")
                )
                  kernel_sig_wrapper <- c(
                    kernel_sig_wrapper, paste0(.mojor_out_dim_var_name(), ": ImmutInt32Ptr"),
                    paste0(.mojor_out_ndim_var_name(), ": Int")
                )
                  kernel_call <- c(kernel_call, .mojor_out_dim_var_name(), .mojor_out_ndim_var_name())
                  for (a in gpu_dim_arrays) {
                    kernel_sig_generic <- c(
                      kernel_sig_generic, paste0(
                        .mojor_dim_var_name(a),
                        ": ImmutInt32Ptr"
                    ),
                      paste0(
                        .mojor_ndim_var_name(a),
                        ": Int"
                    )
                  )
                    kernel_sig_wrapper <- c(
                      kernel_sig_wrapper, paste0(
                        .mojor_dim_var_name(a),
                        ": ImmutInt32Ptr"
                    ),
                      paste0(
                        .mojor_ndim_var_name(a),
                        ": Int"
                    )
                  )
                    kernel_call <- c(
                      kernel_call, .mojor_dim_var_name(a),
                      .mojor_ndim_var_name(a)
                  )
                  }
                }
                gpu_ew_scalar <- gpu$ew_scalar
                kernel_sig_wrapper_names <- sub(":.*$", "", kernel_sig_wrapper)
                loop_var <- if (!is.null(gpu$linear_loop_var) &&
                  nzchar(gpu$linear_loop_var)) gpu$linear_loop_var else gpu$loop_var
                raw_kernel_defs <- c(
                  "comptime block_size = 256", "var grid_size = (n_i + block_size - 1) // block_size",
                  "@parameter", paste0(
                    "fn _mojor_ew_kernel[dtype: DType](", paste(kernel_sig_generic, collapse = ", "),
                    ") -> None:"
                ),
                  "    var tid = block_idx.x * block_dim.x + thread_idx.x",
                  "    if tid < UInt(n):", paste0("        var ", loop_var, " = Int(tid)")
              )
                if (isTRUE(matrix2d_mode)) {
                  raw_kernel_defs <- c(
                    raw_kernel_defs,
                    "        var __mojor_m2_nrow_i: Int = Int(__mojor_m2_nrow)",
                    "        var __mojor_m2_ncol_i: Int = Int(__mojor_m2_ncol)"
                  )
                  if (isTRUE(matrix2d_use_coords)) {
                    raw_kernel_defs <- c(
                      raw_kernel_defs,
                      paste0("        var __mojor_m2_i: Int = (", loop_var, " % __mojor_m2_nrow_i) + 1"),
                      paste0("        var __mojor_m2_j: Int = (", loop_var, " // __mojor_m2_nrow_i) + 1")
                    )
                  }
                }
                if (use_broadcast_nd && length(gpu_dim_arrays) >
                  0) {
                  for (a in gpu_dim_arrays) {
                    len_var <- .mojor_len_var_name(a)
                    dim_var <- .mojor_dim_var_name(a)
                    ndim_var <- .mojor_ndim_var_name(a)
                    idx_var <- paste0("__mojor_di_", a)
                    raw_kernel_defs <- c(
                      raw_kernel_defs, paste0("        var ", len_var, ": Int = 1"),
                      paste0(
                        "        for ", idx_var, " in range(", ndim_var,
                        "):"
                    ),
                      paste0(
                        "            ", len_var, " *= Int(", dim_var, "[",
                        idx_var, "])"
                    )
                  )
                  }
                }
                for (expr in gpu_ew_scalar) {
                  raw_kernel_defs <- c(raw_kernel_defs, paste0("        out_ptr[", loop_var, "] = ", expr))
                }
                raw_kernel_defs <- c(
                  raw_kernel_defs, "@parameter", paste0(
                    "fn _mojor_ew_kernel_", gpu_dtype_tag, "(", paste(kernel_sig_wrapper, collapse = ", "),
                    ") -> None:"
                ),
                  paste0(
                    "    _mojor_ew_kernel[DType.", gpu_dtype_tag, "](",
                    paste(kernel_sig_wrapper_names, collapse = ", "),
                    ")"
                )
              )
                raw_kernel_call <- c(
                  paste0(
                    "ctx.enqueue_function[_mojor_ew_kernel_", gpu_dtype_tag,
                    ", _mojor_ew_kernel_", gpu_dtype_tag, "]("
                ),
                  paste0(
                    "    ", paste(kernel_call, collapse = ", "),
                    ","
                ),
                  "    grid_dim=grid_size,", "    block_dim=block_size,",
                  ")", "ctx.synchronize()", "_mojor_set_status(status_ptr, 1)",
                  "return 1"
              )
                gpu_lines <- c(gpu_lines, indent_lines(raw_kernel_defs, "        "))
                if (use_layouttensor) {
                  gpu_lines <- c(
                    gpu_lines, paste0("        comptime _MOJOR_EW_DTYPE = DType.", gpu_dtype_tag),
                    "        comptime _MOJOR_EW_LAYOUT = Layout.row_major(1)",
                    "        var layout = RuntimeLayout[_MOJOR_EW_LAYOUT].row_major(IndexList[1](n_i))"
                )
                  for (a in gpu$array_args) {
                    gpu_lines <- c(
                      gpu_lines, paste0("        var ", a, "_ptr = ", a, "_bufp[0].unsafe_ptr()"),
                      paste0(
                        "        var ", a, "_dev = LayoutTensor[mut=False, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
                        a, "_ptr, layout)"
                    )
                  )
                  }
                  gpu_lines <- c(
                    gpu_lines, paste0("        var ", gpu$out_name, "_ptr = out_bufp[0].unsafe_ptr()"),
                    paste0(
                      "        var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
                      gpu$out_name, "_ptr, layout)"
                  ),
                    "        @parameter", "        @always_inline", "        fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
                )
                  for (a in gpu$array_args) {
                    gpu_lines <- c(
                      gpu_lines, paste0(
                        "            var ", a, "_chunk = ", a, "_dev.load[simd_width](indices)"
                    )
                  )
                  }
                  for (expr in gpu$ew_specs) {
                    gpu_lines <- c(
                      gpu_lines, paste0(
                        "            ", gpu$out_name, "_dev.store[simd_width](indices, ",
                        expr, ")"
                    )
                  )
                  }
                  gpu_lines <- c(
                    gpu_lines, "        @parameter", "        @always_inline",
                    paste0(
                      "        fn _mojor_elementwise_", gpu_dtype_tag,
                      "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
                  ),
                    paste0(
                      "            _mojor_elementwise[DType.", gpu_dtype_tag,
                      ", simd_width, rank, alignment](indices)"
                  ),
                    "        comptime _MOJOR_EW_SIMD = simd_width_of[_MOJOR_EW_DTYPE, target=get_gpu_target()]()",
                    "        if (n_i % Int(_MOJOR_EW_SIMD)) != 0:"
                )
                  gpu_lines <- c(gpu_lines, indent_lines(raw_kernel_call, "            "))
                  gpu_lines <- c(
                    gpu_lines, paste0(
                      "        elementwise[_mojor_elementwise_", gpu_dtype_tag,
                      ", _MOJOR_EW_SIMD, target=\"gpu\"](n_i, ctx)"
                  ),
                    "        ctx.synchronize()", "        _mojor_set_status(status_ptr, 1)",
                    "        return 1", "    except:", "        _mojor_set_status(status_ptr, -2)",
                    "        return 0"
                )
                } else {
                  gpu_lines <- c(
                    gpu_lines, indent_lines(raw_kernel_call, "        "),
                    "    except:", "        _mojor_set_status(status_ptr, -2)",
                    "        return 0"
                )
                }
                mojo_lines <- c(mojo_lines, gpu_lines)
                elementwise_gpu_buf_emitted <- TRUE
            }
        }, envir = ctx
    )
}
