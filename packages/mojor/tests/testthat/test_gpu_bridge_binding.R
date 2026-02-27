.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("gpu strict bridge binding smoke", {  if (!identical(Sys.getenv("MOJOR_TEST_GPU_STRICT_BIND"), "1")) {
    skip("set MOJOR_TEST_GPU_STRICT_BIND=1 to run")
  }
  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }

  Sys.unsetenv("MOJOR_LIB_PATH")
  st <- tryCatch(get(".mojor_state", inherits = TRUE), error = function(e) NULL)
  if (is.environment(st)) {
    st$bridge_pkg <- NULL
    st$bridge_path <- NULL
    st$gpu_ctx <- NULL
  }

  expect_true(mojor_has_gpu())

  x <- matrix(1:12, nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f32")
  cols <- gpu_reduce(gx, "sum", dims = 1L, keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(cols), colSums(x))

  mojor_gpu_array_free(cols)
  mojor_gpu_array_free(gx)
})
