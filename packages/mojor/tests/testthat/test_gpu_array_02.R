# Split from test_gpu_array.R (chunk 02).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray like constructors validate arguments", {  .skip_if_no_gpu_backend(require_float = TRUE)

  src <- .mojor_gpu_src_matrix()
  .mojor_expect_call_errors(list(
    list(fn = gpu_full_like, args = list(src, c(1, 2)), pattern = "non-NA numeric scalar"),
    list(fn = gpu_zeros_like, args = list(NULL), pattern = "x must not be NULL"),
    list(fn = gpu_empty_like, args = list(NULL), pattern = "x must not be NULL"),
    list(fn = gpu_empty_like, args = list(numeric(0)), pattern = "positive length"),
    list(fn = gpu_rand_like, args = list(NULL), pattern = "x must not be NULL"),
    list(fn = gpu_rand_like, args = list(numeric(0)), pattern = "positive length"),
    list(fn = gpu_rand_like, args = list(src, min = NA_real_), pattern = "min must be a non-NA numeric scalar"),
    list(fn = gpu_rand_like, args = list(src, max = NA_real_), pattern = "max must be a non-NA numeric scalar"),
    list(fn = gpu_rand_like, args = list(src, min = 2, max = 1), pattern = "max must be >= min"),
    list(fn = gpu_randn_like, args = list(NULL), pattern = "x must not be NULL"),
    list(fn = gpu_randn_like, args = list(numeric(0)), pattern = "positive length"),
    list(fn = gpu_randn_like, args = list(src, mean = NA_real_), pattern = "mean must be a non-NA numeric scalar"),
    list(fn = gpu_randn_like, args = list(src, sd = NA_real_), pattern = "sd must be a non-NA numeric scalar"),
    list(fn = gpu_randn_like, args = list(src, sd = -1), pattern = "sd must be >= 0"),
    list(fn = gpu_randi_like, args = list(NULL), pattern = "x must not be NULL"),
    list(fn = gpu_randi_like, args = list(numeric(0)), pattern = "positive length"),
    list(fn = gpu_randi_like, args = list(src, low = NA_real_), pattern = "low must be a finite integer scalar"),
    list(fn = gpu_randi_like, args = list(src, high = NA_real_), pattern = "high must be a finite integer scalar"),
    list(fn = gpu_randi_like, args = list(src, low = 1.5, high = 4), pattern = "low must be a finite integer scalar"),
    list(fn = gpu_randi_like, args = list(src, low = 2, high = 2), pattern = "high must be > low"),
    list(fn = gpu_randi_like, args = list(src, low = 3, high = 2), pattern = "high must be > low")
  ))
})

test_that("mojor_gpu_session wrappers work", {  .skip_if_no_gpu_backend(require_float = TRUE, require_mojo = FALSE)

  sess <- mojor_gpu_session(float::fl(runif(2048)))
  expect_true(inherits(sess, "mojor_gpu_session"))
  expect_true(sess$n > 0)
  expect_true(!is.null(attr(sess, "gpu_status")))

  sess2 <- mojor_gpu_session_run(sess, iters = 2L, scale = 1.1, bias = 0.05)
  expect_true(inherits(sess2, "mojor_gpu_session"))
  expect_true(sess2$n == sess$n)

  total <- mojor_gpu_session_sum(sess, iters = 2L, scale = 1.1, bias = 0.05, post_scale = 0.9, post_bias = -0.02, post_iters = 1L)
  expect_true(is.numeric(total))

  sess_free <- mojor_gpu_session_free(sess)
  expect_true(sess_free$n == 0L)
})

test_that("mojor_gpu_meminfo returns bytes", {  .skip_if_no_gpu_backend()
  info <- mojor_gpu_meminfo()
  expect_true(is.list(info))
  expect_true(all(c("free_bytes", "total_bytes", "status") %in% names(info)))
  if (is.numeric(info$status) && info$status > 0) {
    expect_true(is.numeric(info$free_bytes))
    expect_true(is.numeric(info$total_bytes))
    expect_true(info$total_bytes >= info$free_bytes)
  }
})

test_that("gpu array linear path avoids host read", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()
  x <- runif(1024)
  buf <- mojor_gpu_array(x, dtype = "f32")
  out <- mojor_gpu_linear(buf, scale = 2, bias = 1)
  expect_true(inherits(out, "mojor_gpu_array"))
  out_host <- mojor_gpu_array_read(out)
  expect_true(max(abs(out_host - (x * 2 + 1))) < 1e-5)
  .mojor_gpu_array_free_all(out, buf)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray basic ops use GPU kernels", {  .skip_if_no_gpu_backend(require_float = TRUE)

  n <- 256L
  x <- runif(n)
  y <- runif(n)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")
  out <- gx + gy
  expect_true(inherits(out, "GPUArray"))
  out_host <- mojor_gpu_array_read(out)
  expect_true(max(abs(out_host - (x + y))) < 1e-5)
  .mojor_gpu_array_free_all(out, gy, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu matmul runs on device", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m <- 4L
  k <- 5L
  n <- 3L
  a <- matrix(runif(m * k), nrow = m)
  b <- matrix(runif(k * n), nrow = k)
  ga <- GPUArray(a, dtype = "f32")
  gb <- GPUArray(b, dtype = "f32")
  gc <- gpu_matmul(ga, gb)
  expect_true(inherits(gc, "GPUArray"))
  host <- mojor_gpu_array_read(gc)
  expect_equal(dim(host), c(m, n))
  expect_true(max(abs(host - (a %*% b))) < 1e-4)
  .mojor_gpu_array_free_all(gc, gb, ga)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu matmul respects transpose flags", {  .skip_if_no_gpu_backend(require_float = TRUE)

  a_t <- matrix(runif(20L), nrow = 5L, ncol = 4L)
  b_t <- matrix(runif(15L), nrow = 3L, ncol = 5L)
  a_nt <- t(a_t)
  b_nt <- t(b_t)

  ga_t <- GPUArray(a_t, dtype = "f32")
  gb_t <- GPUArray(b_t, dtype = "f32")
  ga_nt <- GPUArray(a_nt, dtype = "f32")
  gb_nt <- GPUArray(b_nt, dtype = "f32")

  out_tt <- gpu_matmul(ga_t, gb_t, transpose_a = TRUE, transpose_b = TRUE)
  expect_equal(mojor_gpu_array_read(out_tt), t(a_t) %*% t(b_t), tolerance = 1e-4)

  out_tn <- gpu_matmul(ga_t, gb_nt, transpose_a = TRUE, transpose_b = FALSE)
  expect_equal(mojor_gpu_array_read(out_tn), t(a_t) %*% b_nt, tolerance = 1e-4)

  out_nt <- gpu_matmul(ga_nt, gb_t, transpose_a = FALSE, transpose_b = TRUE)
  expect_equal(mojor_gpu_array_read(out_nt), a_nt %*% t(b_t), tolerance = 1e-4)

  .mojor_gpu_array_free_all(out_nt, out_tn, out_tt, gb_nt, ga_nt, gb_t, ga_t)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu matmul can write into existing array", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m <- 4L
  k <- 5L
  n <- 3L
  a <- matrix(runif(m * k), nrow = m)
  b <- matrix(runif(k * n), nrow = k)
  ga <- GPUArray(a, dtype = "f32")
  gb <- GPUArray(b, dtype = "f32")
  out <- GPUArray(n = m * n, dtype = "f32")
  out$dim <- c(m, n)
  out$strides <- .mojor_dim_strides(out$dim)
  out2 <- gpu_matmul_into(out, ga, gb)
  expect_true(identical(.mojor_gpu_array_handle(out2), .mojor_gpu_array_handle(out)))
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), c(m, n))
  expect_true(max(abs(host - (a %*% b))) < 1e-4)
  .mojor_gpu_array_free_all(out, gb, ga)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("i32 scatter capability probe is stable", {  .skip_if_no_gpu_backend()

  cap1 <- .mojor_gpu_i32_scatter_capable(api = "metal", force_refresh = TRUE)
  cap2 <- .mojor_gpu_i32_scatter_capable(api = "metal")
  expect_type(cap1, "logical")
  expect_length(cap1, 1L)
  expect_true(identical(cap1, cap2))

  old_opt <- options(mojor.gpu.i32_scatter_on_metal = TRUE)
  on.exit(options(old_opt), add = TRUE)
  cap3 <- .mojor_gpu_i32_scatter_capable(api = "metal")
  expect_type(cap3, "logical")
  expect_length(cap3, 1L)
  expect_true(identical(cap1, cap3))
})

test_that("f64 matmul capability probe is stable", {  skip_if_no_gpu_f64()

  cap_mm_1 <- .mojor_gpu_f64_matmul_capable(mode = "matmul")
  cap_mm_2 <- .mojor_gpu_f64_matmul_capable(mode = "matmul")
  expect_type(cap_mm_1, "logical")
  expect_length(cap_mm_1, 1L)
  expect_true(identical(cap_mm_1, cap_mm_2))

  cap_gemv_1 <- .mojor_gpu_f64_matmul_capable(mode = "gemv")
  cap_gemv_2 <- .mojor_gpu_f64_matmul_capable(mode = "gemv")
  expect_type(cap_gemv_1, "logical")
  expect_length(cap_gemv_1, 1L)
  expect_true(identical(cap_gemv_1, cap_gemv_2))

  cap_gevm_1 <- .mojor_gpu_f64_matmul_capable(mode = "gevm")
  cap_gevm_2 <- .mojor_gpu_f64_matmul_capable(mode = "gevm")
  expect_type(cap_gevm_1, "logical")
  expect_length(cap_gevm_1, 1L)
  expect_true(identical(cap_gevm_1, cap_gevm_2))
})

test_that("f64 capability cache invalidates on context epoch change", {  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_probe_reduce <- get(".mojor_gpu_probe_f64_reduce", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    assign(".mojor_gpu_probe_f64_reduce", old_probe_reduce, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  epoch <- 100L
  probes_mm <- 0L
  probes_reduce <- 0L
  fake_ctx <- structure(list(token = 1L), class = "mojor_gpu_ctx")

  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() as.integer(epoch), envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      probes_mm <<- probes_mm + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )
  assign(
    ".mojor_gpu_probe_f64_reduce",
    function(ctx, dims_mode = c("scalar", "dims"), op_class = c("value", "arg")) {
      dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
      op_class <- match.arg(op_class, c("value", "arg"))
      probes_reduce <<- probes_reduce + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  expect_true(.mojor_gpu_f64_matmul_capable())
  expect_true(.mojor_gpu_f64_matmul_capable())
  expect_equal(probes_mm, 1L)

  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar"))
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar"))
  expect_equal(probes_reduce, 1L)

  epoch <- epoch + 1L
  expect_true(.mojor_gpu_f64_matmul_capable())
  expect_equal(probes_mm, 2L)
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar"))
  expect_equal(probes_reduce, 2L)
})

test_that("f64 capability cache is keyed by matmul mode and reduce op class", {  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_probe_reduce <- get(".mojor_gpu_probe_f64_reduce", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    assign(".mojor_gpu_probe_f64_reduce", old_probe_reduce, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  matmul_modes <- character()
  reduce_keys <- character()
  fake_ctx <- structure(list(token = 3L), class = "mojor_gpu_ctx")

  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 200L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      matmul_modes <<- c(matmul_modes, mode)
      list(available = !identical(mode, "gevm"), reason = NULL)
    },
    envir = cap_env
  )
  assign(
    ".mojor_gpu_probe_f64_reduce",
    function(ctx, dims_mode = c("scalar", "dims"), op_class = c("value", "arg")) {
      dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
      op_class <- match.arg(op_class, c("value", "arg"))
      reduce_keys <<- c(reduce_keys, paste0(dims_mode, ":", op_class))
      list(available = identical(op_class, "value"), reason = NULL)
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()

  expect_true(.mojor_gpu_f64_matmul_capable(mode = "matmul"))
  expect_true(.mojor_gpu_f64_matmul_capable(mode = "matmul"))
  expect_true(.mojor_gpu_f64_matmul_capable(mode = "gemv"))
  expect_true(.mojor_gpu_f64_matmul_capable(mode = "gemv"))
  expect_false(.mojor_gpu_f64_matmul_capable(mode = "gevm"))
  expect_false(.mojor_gpu_f64_matmul_capable(mode = "gevm"))
  expect_equal(matmul_modes, c("matmul", "gemv", "gevm"))

  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "value"))
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "value"))
  expect_false(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "arg"))
  expect_false(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "arg"))
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "value"))
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "value"))
  expect_false(.mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "arg"))
  expect_false(.mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "arg"))
  expect_equal(reduce_keys, c("scalar:value", "scalar:arg", "dims:value", "dims:arg"))
})

test_that("f64 force-refresh capability probe uses subprocess isolation", {  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_probe_subprocess <- get(".mojor_gpu_probe_run_subprocess", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  old_probe_mode <- Sys.getenv("MOJOR_GPU_F64_PROBE_MODE", unset = NA_character_)
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    assign(".mojor_gpu_probe_run_subprocess", old_probe_subprocess, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
    if (is.na(old_probe_mode)) {
      Sys.unsetenv("MOJOR_GPU_F64_PROBE_MODE")
    } else {
      Sys.setenv(MOJOR_GPU_F64_PROBE_MODE = old_probe_mode)
    }
  }, add = TRUE)

  inprocess_calls <- 0L
  subprocess_calls <- 0L
  fake_ctx <- structure(list(token = 9L), class = "mojor_gpu_ctx")

  Sys.unsetenv("MOJOR_GPU_F64_PROBE_MODE")
  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 310L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      inprocess_calls <<- inprocess_calls + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )
  assign(
    ".mojor_gpu_probe_run_subprocess",
    function(kind = c("matmul", "reduce"), timeout_sec = 45L, ...) {
      kind <- match.arg(kind, c("matmul", "reduce"))
      subprocess_calls <<- subprocess_calls + 1L
      list(available = identical(kind, "matmul"), reason = NULL)
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  expect_true(.mojor_gpu_f64_matmul_capable(force_refresh = TRUE, mode = "matmul"))
  expect_equal(subprocess_calls, 1L)
  expect_equal(inprocess_calls, 0L)
  expect_true(.mojor_gpu_f64_matmul_capable(mode = "matmul"))
  expect_equal(subprocess_calls, 1L)
})

test_that("f64 probe mode env override can force subprocess path", {  cap_env <- environment(.mojor_gpu_f64_reduce_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_reduce <- get(".mojor_gpu_probe_f64_reduce", envir = cap_env, inherits = TRUE)
  old_probe_subprocess <- get(".mojor_gpu_probe_run_subprocess", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  old_probe_mode <- Sys.getenv("MOJOR_GPU_F64_PROBE_MODE", unset = NA_character_)
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_reduce", old_probe_reduce, envir = cap_env)
    assign(".mojor_gpu_probe_run_subprocess", old_probe_subprocess, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
    if (is.na(old_probe_mode)) {
      Sys.unsetenv("MOJOR_GPU_F64_PROBE_MODE")
    } else {
      Sys.setenv(MOJOR_GPU_F64_PROBE_MODE = old_probe_mode)
    }
  }, add = TRUE)

  inprocess_calls <- 0L
  subprocess_calls <- 0L
  fake_ctx <- structure(list(token = 10L), class = "mojor_gpu_ctx")

  Sys.setenv(MOJOR_GPU_F64_PROBE_MODE = "subprocess")
  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 311L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_reduce",
    function(ctx, dims_mode = c("scalar", "dims"), op_class = c("value", "arg")) {
      dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
      op_class <- match.arg(op_class, c("value", "arg"))
      inprocess_calls <<- inprocess_calls + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )
  assign(
    ".mojor_gpu_probe_run_subprocess",
    function(kind = c("matmul", "reduce"), timeout_sec = 45L, ...) {
      kind <- match.arg(kind, c("matmul", "reduce"))
      subprocess_calls <<- subprocess_calls + 1L
      list(available = identical(kind, "reduce"), reason = NULL)
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "value"))
  expect_equal(subprocess_calls, 1L)
  expect_equal(inprocess_calls, 0L)
  expect_true(.mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "value"))
  expect_equal(subprocess_calls, 1L)
})

test_that("f64 probe mode env override can force inprocess on force-refresh", {  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_probe_subprocess <- get(".mojor_gpu_probe_run_subprocess", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  old_probe_mode <- Sys.getenv("MOJOR_GPU_F64_PROBE_MODE", unset = NA_character_)
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    assign(".mojor_gpu_probe_run_subprocess", old_probe_subprocess, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
    if (is.na(old_probe_mode)) {
      Sys.unsetenv("MOJOR_GPU_F64_PROBE_MODE")
    } else {
      Sys.setenv(MOJOR_GPU_F64_PROBE_MODE = old_probe_mode)
    }
  }, add = TRUE)

  inprocess_calls <- 0L
  subprocess_calls <- 0L
  fake_ctx <- structure(list(token = 11L), class = "mojor_gpu_ctx")

  Sys.setenv(MOJOR_GPU_F64_PROBE_MODE = "inprocess")
  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 312L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      inprocess_calls <<- inprocess_calls + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )
  assign(
    ".mojor_gpu_probe_run_subprocess",
    function(kind = c("matmul", "reduce"), timeout_sec = 45L, ...) {
      kind <- match.arg(kind, c("matmul", "reduce"))
      subprocess_calls <<- subprocess_calls + 1L
      list(available = TRUE, reason = NULL)
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  expect_true(.mojor_gpu_f64_matmul_capable(force_refresh = TRUE, mode = "matmul"))
  expect_equal(inprocess_calls, 1L)
  expect_equal(subprocess_calls, 0L)
})

test_that("f64 probe result normalization assigns deterministic reason codes", {
  invalid <- .mojor_gpu_probe_result_normalize(list(), "matmul")
  expect_false(invalid$available)
  expect_identical(invalid$code, "invalid_result_shape")
  expect_match(
    invalid$reason,
    "f64 matmul probe returned invalid result shape",
    fixed = TRUE
  )

  missing_script <- .mojor_gpu_probe_result_normalize(
    list(
      available = FALSE,
      reason = "f64 probe subprocess unavailable: mojor.R not found"
    ),
    "matmul"
  )
  expect_identical(missing_script$code, "subprocess_script_missing")

  ctx_missing <- .mojor_gpu_probe_result_normalize(
    list(
      available = FALSE,
      reason = "f64 reduce probe requires GPU context"
    ),
    "reduce"
  )
  expect_identical(ctx_missing$code, "probe_context_missing")

  passthrough <- .mojor_gpu_probe_result_normalize(
    list(available = FALSE, reason = "custom probe failure", code = "custom_code"),
    "reduce"
  )
  expect_identical(passthrough$code, "custom_code")
})

test_that("f64 capability cache stores reason code metadata", {
  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  fake_ctx <- structure(list(token = 12L), class = "mojor_gpu_ctx")

  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 313L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      list(available = FALSE, reason = "f64 matmul probe requires GPU context")
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  expect_false(.mojor_gpu_f64_matmul_capable(mode = "matmul"))

  cached <- .mojor_state$gpu_capability_cache[["f64_matmul_matmul"]]
  expect_true(is.list(cached))
  expect_identical(cached$reason, "f64 matmul probe requires GPU context")
  expect_identical(cached$reason_code, "probe_context_missing")
})

test_that("gpu capability cache diagnostics expose memoization stats and entries", {
  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_loaded <- get("mojor_is_loaded", envir = cap_env, inherits = TRUE)
  old_has_gpu <- get("mojor_has_gpu", envir = cap_env, inherits = TRUE)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_probe_mm <- get(".mojor_gpu_probe_f64_matmul", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  old_stats <- .mojor_state$gpu_capability_cache_stats
  on.exit({
    assign("mojor_is_loaded", old_loaded, envir = cap_env)
    assign("mojor_has_gpu", old_has_gpu, envir = cap_env)
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    assign(".mojor_gpu_probe_f64_matmul", old_probe_mm, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
    .mojor_state$gpu_capability_cache_stats <- old_stats
  }, add = TRUE)

  fake_ctx <- structure(list(token = 19L), class = "mojor_gpu_ctx")
  assign("mojor_is_loaded", function() TRUE, envir = cap_env)
  assign("mojor_has_gpu", function() TRUE, envir = cap_env)
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 401L, envir = cap_env)
  assign(
    ".mojor_gpu_probe_f64_matmul",
    function(ctx, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      list(
        available = FALSE,
        reason = sprintf("mock capability probe blocked (%s)", mode),
        code = "mock_probe_blocked"
      )
    },
    envir = cap_env
  )

  .mojor_state$gpu_capability_cache <- list()
  .mojor_state$gpu_capability_cache_stats <- NULL
  expect_false(.mojor_gpu_f64_matmul_capable(mode = "matmul"))
  expect_false(.mojor_gpu_f64_matmul_capable(mode = "matmul"))

  diag <- .mojor_gpu_capability_cache_diag(include_entries = TRUE)
  expect_true(is.list(diag))
  expect_true(is.list(diag$stats))
  expect_true(diag$stats$lookups >= 1L)
  expect_true(diag$stats$hits >= 1L)
  expect_true(diag$stats$stores >= 1L)
  expect_true(any(diag$entries$key == "f64_matmul_matmul"))
  expect_true(any(diag$entries$reason_code == "mock_probe_blocked"))

  .mojor_gpu_capability_cache_diag(reset = TRUE)
  reset_diag <- .mojor_gpu_capability_cache_diag(include_entries = FALSE)
  expect_identical(reset_diag$stats$lookups, 0L)
  expect_identical(reset_diag$stats$hits, 0L)
  expect_identical(reset_diag$stats$misses, 0L)
  expect_identical(reset_diag$stats$stores, 0L)
})

test_that("gpu kernel cache diagnostics track negative cache hits", {
  old_cache <- .mojor_state$gpu_kernel_cache
  old_stats <- .mojor_state$gpu_kernel_cache_stats
  on.exit({
    .mojor_state$gpu_kernel_cache <- old_cache
    .mojor_state$gpu_kernel_cache_stats <- old_stats
  }, add = TRUE)

  .mojor_state$gpu_kernel_cache <- list()
  .mojor_state$gpu_kernel_cache_stats <- NULL

  .mojor_gpu_kernel_cache_set("mock_positive", list(name = "kernel"))
  .mojor_gpu_kernel_cache_set("mock_negative", FALSE)
  expect_true(is.list(.mojor_gpu_kernel_cache_get("mock_positive")))
  expect_true(identical(.mojor_gpu_kernel_cache_get("mock_negative"), FALSE))
  expect_null(.mojor_gpu_kernel_cache_get("mock_missing"))

  diag <- .mojor_gpu_kernel_cache_diag(include_entries = TRUE)
  expect_true(diag$stats$lookups >= 3L)
  expect_true(diag$stats$hits >= 1L)
  expect_true(diag$stats$negative_hits >= 1L)
  expect_true(diag$stats$misses >= 1L)
  expect_true(diag$stats$stores >= 2L)
  expect_true(any(diag$entries$key == "mock_positive" & diag$entries$status == "positive"))
  expect_true(any(diag$entries$key == "mock_negative" & diag$entries$status == "negative"))

  bundle <- .mojor_gpu_cache_diag(include_entries = FALSE, reset = TRUE)
  expect_true(is.list(bundle))
  post <- .mojor_gpu_kernel_cache_diag(include_entries = FALSE)
  expect_identical(post$stats$lookups, 0L)
  expect_identical(post$stats$negative_hits, 0L)
})

test_that("f64 probe diagnostics helpers consume cache deterministically", {
  cap_env <- environment(.mojor_gpu_f64_matmul_capable)
  old_ctx_get <- get(".mojor_gpu_ctx_get", envir = cap_env, inherits = TRUE)
  old_ctx_epoch_get <- get(".mojor_gpu_ctx_epoch_get", envir = cap_env, inherits = TRUE)
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign(".mojor_gpu_ctx_get", old_ctx_get, envir = cap_env)
    assign(".mojor_gpu_ctx_epoch_get", old_ctx_epoch_get, envir = cap_env)
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  fake_ctx <- structure(list(token = 13L), class = "mojor_gpu_ctx")
  assign(".mojor_gpu_ctx_get", function() fake_ctx, envir = cap_env)
  assign(".mojor_gpu_ctx_epoch_get", function() 314L, envir = cap_env)

  .mojor_state$gpu_capability_cache <- list(
    f64_matmul_matmul = list(
      ctx = fake_ctx, ctx_epoch = 314L, available = FALSE,
      reason = "f64 probe subprocess unavailable: mojor.R not found",
      reason_code = "subprocess_script_missing"
    ),
    f64_reduce_scalar_value = list(
      ctx = fake_ctx, ctx_epoch = 314L, available = FALSE,
      reason = "f64 reduce probe requires GPU context",
      reason_code = "probe_context_missing"
    )
  )

  mm_diag <- .mojor_gpu_f64_matmul_probe_diag("matmul", ctx = fake_ctx, ctx_epoch = 314L)
  expect_identical(mm_diag$reason, "f64 probe subprocess unavailable: mojor.R not found")
  expect_identical(mm_diag$code, "subprocess_script_missing")

  red_diag <- .mojor_gpu_f64_reduce_probe_diag("scalar", "value", ctx = fake_ctx, ctx_epoch = 314L)
  expect_identical(red_diag$reason, "f64 reduce probe requires GPU context")
  expect_identical(red_diag$code, "probe_context_missing")

  .mojor_state$gpu_capability_cache <- list()
  mm_default <- .mojor_gpu_f64_matmul_probe_diag("gemv", ctx = fake_ctx, ctx_epoch = 314L)
  expect_match(mm_default$reason, "f64 matmul probe unavailable \\(gemv\\)")
  expect_identical(mm_default$code, "probe_unavailable")
})

