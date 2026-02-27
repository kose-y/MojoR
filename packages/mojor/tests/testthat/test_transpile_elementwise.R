extract_ew_kernel <- function(mojo) {
  start <- regexpr("fn _mojor_elementwise\\[", mojo)
  if (start < 0) return(NULL)
  sub <- substr(mojo, start, nchar(mojo))
  end_rel <- regexpr("fn _mojor_elementwise_[A-Za-z0-9]+", sub)
  if (end_rel < 0) return(sub)
  substr(sub, 1, end_rel - 1)
}

test_that("elementwise transpile emits elementwise call", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", elementwise = TRUE, elementwise_cpu = TRUE)
  expect_true(grepl("elementwise\\[", trans$mojo))
  expect_true(isTRUE(trans$elementwise$emitted))
  kernel <- extract_ew_kernel(trans$mojo)
  expect_true(is.character(kernel))
  expect_true(grepl("var i = Int\\(indices\\[0\\]\\)", kernel))
  expect_true(grepl("out\\[Int\\(i\\)\\]", kernel))
  expect_true(grepl("Int\\(i\\)", kernel))
  expect_false(grepl("i - 1", kernel, fixed = TRUE))
})

test_that("elementwise supports multi-statement loops", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
      out[i] <- x[i] + 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", elementwise = TRUE, elementwise_cpu = TRUE)
  expect_true(isTRUE(trans$elementwise$emitted))
  kernel <- extract_ew_kernel(trans$mojo)
  expect_true(is.character(kernel))
  expect_true(grepl("out\\[Int\\(i\\)\\]", kernel))
  expect_false(grepl("i - 1", kernel, fixed = TRUE))
})

test_that("elementwise GPU target emits GPU elementwise scaffolding", {  skip("GPU tests disabled - CPU only")
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f32[]", elementwise = TRUE, elementwise_target = "gpu", elementwise_size = 8L)
  expect_true(grepl("elementwise\\[", trans$mojo))
  expect_true(grepl("target=\\\"gpu\\\"", trans$mojo))
  expect_true(grepl("LayoutTensor", trans$mojo))
  expect_true(isTRUE(trans$elementwise$emitted))
  expect_equal(trans$elementwise$target, "gpu")
})

test_that("gpu elementwise requires elementwise_size", {  skip("GPU tests disabled - CPU only")
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f32[]", elementwise = TRUE, elementwise_target = "gpu")
  expect_false(isTRUE(trans$elementwise$emitted))
  expect_match(trans$elementwise$reason, "elementwise_size")
})

test_that("gpu broadcast_nd transpile stages dim metadata through device buffers", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 8L,
    broadcast = "broadcast_nd",
    name = "t_gpu_bcast_codegen_check"
  )
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_true(grepl("enqueue_create_buffer\\[DType.int32\\]", trans$mojo))
  expect_true(grepl("ctx.enqueue_copy\\(dst_buf=dim_out_ptr, src_buf=dim_out_ptr_stage\\)", trans$mojo))
  expect_true(grepl("ctx.enqueue_copy\\(dst_buf=dim_x_ptr, src_buf=dim_x_ptr_stage\\)", trans$mojo))
  expect_true(grepl("ctx.enqueue_copy\\(dst_buf=dim_y_ptr, src_buf=dim_y_ptr_stage\\)", trans$mojo))
  expect_true(grepl("fn t_gpu_bcast_codegen_check_gpu_buf_f64\\(", trans$mojo))
  expect_true(grepl("__mojor_out_dim_ptr: ImmutOpaqueAny, __mojor_out_ndim: Int32", trans$mojo, fixed = TRUE))
  expect_true(grepl("__mojor_dim_x_ptr: ImmutOpaqueAny, __mojor_ndim_x: Int32", trans$mojo, fixed = TRUE))
  expect_true(grepl("__mojor_dim_y_ptr: ImmutOpaqueAny, __mojor_ndim_y: Int32", trans$mojo, fixed = TRUE))
  expect_true(grepl("fn _mojor_bcast_index\\(out_idx: Int, out_dim: ImmutInt32Ptr", trans$mojo))
  expect_true(grepl("ctx.enqueue_function\\[_mojor_ew_kernel_float64, _mojor_ew_kernel_float64\\]\\(", trans$mojo))
  expect_true(grepl("out_bufp\\[0\\], n, x_bufp\\[0\\], y_bufp\\[0\\], dim_out_ptr, ndim_out_i, dim_x_ptr, ndim_x_i, dim_y_ptr, ndim_y_i,", trans$mojo))
  expect_false(grepl("__mojor_out_dim_0", trans$mojo, fixed = TRUE))
})

test_that("gpu elementwise transpile emits boundary wrapper and raw kernel scaffolding", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 2
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 8L,
    name = "t_gpu_boundary_scaffold"
  )
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_true(grepl("@export\\(\"t_gpu_boundary_scaffold_gpu_buf_f64\", ABI=\"C\"\\)", trans$mojo))
  expect_true(grepl("fn t_gpu_boundary_scaffold_gpu_buf_f64\\(ctxp: MutCtxPtr, out_bufp: MutBufF64Ptr, n: Int32, x_bufp: MutBufF64Ptr, status_ptr: MutOpaqueAny\\) -> Int32:", trans$mojo))
  expect_true(grepl("if ctxp == NULL_CTX or out_bufp == NULL_BUF_F64:", trans$mojo, fixed = TRUE))
  expect_true(grepl("if n_i <= 0:", trans$mojo, fixed = TRUE))
  expect_true(grepl("try:", trans$mojo, fixed = TRUE))
  expect_true(grepl("except:", trans$mojo, fixed = TRUE))
  expect_true(grepl("fn _mojor_ew_kernel\\[dtype: DType\\]\\(", trans$mojo))
  expect_true(grepl("fn _mojor_ew_kernel_float64\\(", trans$mojo))
  expect_true(grepl("ctx.enqueue_function\\[_mojor_ew_kernel_float64, _mojor_ew_kernel_float64\\]\\(", trans$mojo))
})

test_that("gpu elementwise transpile accepts linearized matrix array+array loops", {  f <- function(x, y) {
  out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  for (i in seq_along(x)) {
    out[i] <- x[i] + y[i]
  }
  out
}
  trans <- mojor_transpile(
    f,
    x = "f64[,]",
    y = "f64[,]",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L,
    name = "t_gpu_matrix_linearized_add"
  )
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_true(grepl("fn t_gpu_matrix_linearized_add_gpu_buf_f64\\(", trans$mojo))
  expect_true(
    grepl("out_ptr\\[i\\] = \\(x\\[Int\\(i\\)\\] \\+ y\\[Int\\(i\\)\\]\\)", trans$mojo) ||
      grepl("out_dev\\.store\\[simd_width\\]\\(indices, \\(x_chunk \\+ y_chunk\\)\\)", trans$mojo)
  )
})

test_that("gpu elementwise transpile accepts linearized matrix scalar+array loops", {  f <- function(x, bias) {
  out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  for (i in seq_along(x)) {
    out[i] <- x[i] + bias
  }
  out
}
  trans <- mojor_transpile(
    f,
    x = "f64[,]",
    bias = "f64",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L,
    name = "t_gpu_matrix_linearized_bias"
  )
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_true(grepl("fn t_gpu_matrix_linearized_bias_gpu_buf_f64\\(", trans$mojo))
  expect_true(
    grepl("out_ptr\\[i\\] = \\(x\\[Int\\(i\\)\\] \\+ bias\\)", trans$mojo) ||
      grepl("out_dev\\.store\\[simd_width\\]\\(indices, \\(x_chunk \\+ bias\\)\\)", trans$mojo)
  )
})

test_that("gpu elementwise end-to-end correctness (small)", {  skip("GPU tests disabled - CPU only")
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!identical(Sys.getenv("MOJOR_TEST_GPU_EW"), "1")) {
    skip("set MOJOR_TEST_GPU_EW=1 to run")
  }
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  n <- 64L
  build <- mojor_build(
    f,
    x = "f32[]",
    name = "ew_gpu_test",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = n,
    load = TRUE,
    cache = TRUE
  )
  x <- float::fl(runif(n))
  out <- build$func(x)
  out_num <- if (float::is.float(out)) float::dbl(out) else as.numeric(out)
  ref <- float::dbl(x) * 2 + 1
  expect_length(out_num, length(ref))
  expect_true(max(abs(out_num - ref)) < 1e-4)
})

test_that("elementwise allows in-place rhs references to output", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- out[i] + x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", elementwise = TRUE, elementwise_cpu = TRUE)
  expect_true(isTRUE(trans$elementwise$emitted))
  expect_true(isTRUE(trans$elementwise$inplace))
})
