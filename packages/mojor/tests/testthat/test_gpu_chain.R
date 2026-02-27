.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("gpu chain returns float32 with attrs and matches CPU", {  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  x <- float::fl(c(-1, 0, 1, 2))
  out <- mojor_gpu_chain(
    x,
    iters = 2L,
    op = "sigmoid",
    scale = 1.1,
    bias = 0.05,
    post_scale = 0.9,
    post_bias = -0.02,
    post_iters = 1L
  )

  expect_true(float::is.float(out))
  expect_length(out, length(x))
  expect_true(!is.null(attr(out, "gpu_status")))
  expect_equal(attr(out, "gpu_op"), "sigmoid")
  expect_equal(attr(out, "gpu_iters"), 2L)
  expect_equal(attr(out, "gpu_post_iters"), 1L)

  ref <- x
  for (i in seq_len(2L)) {
    ref <- float::fl((1 / (1 + exp(-as.double(ref)))) * 1.1 + 0.05)
  }
  ref <- float::fl(float::dbl(ref) * 0.9 - 0.02)
  diff <- max(abs(float::dbl(out) - float::dbl(ref)))
  expect_true(diff < 1e-4)
})

test_that("gpu chain sum returns numeric close to CPU", {  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  x <- float::fl(c(-1, 0, 1, 2))
  out <- mojor_sigmoid_affine_f32_gpu_chain_sum(
    x,
    iters = 2L,
    scale = 1.1,
    bias = 0.05,
    post_scale = 0.9,
    post_bias = -0.02,
    post_iters = 1L
  )

  ref <- x
  for (i in seq_len(2L)) {
    ref <- float::fl((1 / (1 + exp(-as.double(ref)))) * 1.1 + 0.05)
  }
  ref <- float::fl(float::dbl(ref) * 0.9 - 0.02)
  ref_sum <- sum(float::dbl(ref))
  expect_true(abs(as.numeric(out) - ref_sum) < 1e-3)
})
