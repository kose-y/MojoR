library(testthat)

.mojor_truthy_env <- function(name) {
  val <- tolower(Sys.getenv(name, unset = ""))
  val %in% c("1", "true", "yes", "y", "on")
}

skip_if_gpu_jit_preview_stress_not_opt_in <- function() {
  if (!.mojor_truthy_env("MOJOR_TEST_GPU_JIT_AUTO_MATRIX2D")) {
    skip("set MOJOR_TEST_GPU_JIT_AUTO_MATRIX2D=1 to run gpu_jit preview stress runtime tests")
  }
}

if (!.mojor_truthy_env("MOJOR_TEST_RUN_GPU_JIT_PREVIEW_RUNTIME")) {
  skip("set MOJOR_TEST_RUN_GPU_JIT_PREVIEW_RUNTIME=1 to run gpu_jit preview runtime file")
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("runtime: unified preview gpu_sum/gpu_mean parity", {  skip_if_no_mojo()

  f_sum <- function(x) {
    out <- 0
    out <- gpu_sum(x)
    out
  }
  f_mean <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }

  built_sum <- mojor_build(
    f_sum,
    x = "f64[]",
    name = "t_gpu_jit_preview_sum_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_mean <- mojor_build(
    f_mean,
    x = "f64[]",
    name = "t_gpu_jit_preview_mean_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1.5, -2.0, 3.25, 8.0)
  expect_equal(as.numeric(built_sum$func(x)), as.numeric(sum(x)), tolerance = 1e-8)
  expect_equal(as.numeric(built_mean$func(x)), as.numeric(mean(x)), tolerance = 1e-8)
  expect_identical(built_sum$trans$gpu_jit$route, "loop")
  expect_identical(built_mean$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_reduce nested expression parity", {  skip_if_no_mojo()

  f_nested <- function(x) {
    out <- 0
    out <- 1 + gpu_sum(x)
    out
  }

  built <- mojor_build(
    f_nested,
    x = "f64[]",
    name = "t_gpu_jit_preview_nested_sum_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  x <- c(-1.0, 2.0, 4.0)
  expect_equal(as.numeric(built$func(x)), as.numeric(1 + sum(x)), tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_sum/gpu_mean honor NA forbid policy", {  skip_if_no_mojo()

  f_sum <- function(x) {
    out <- 0
    out <- gpu_sum(x)
    out
  }
  f_mean <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }

  built_sum <- mojor_build(
    f_sum,
    x = "f64[]",
    name = "t_gpu_jit_preview_sum_na_forbid_rt",
    gpu_jit_mode = "unified_preview",
    na_guard = "forbid",
    cache = FALSE,
    load = TRUE
  )
  built_mean <- mojor_build(
    f_mean,
    x = "f64[]",
    name = "t_gpu_jit_preview_mean_na_forbid_rt",
    gpu_jit_mode = "unified_preview",
    na_guard = "forbid",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1.0, NA_real_, 3.0)
  expect_error(built_sum$func(x), "NA")
  expect_error(built_mean$func(x), "NA")
  expect_identical(built_sum$trans$gpu_jit$route, "loop")
  expect_identical(built_mean$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul assign subset parity", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    out <- matrix(0, 2, 2)
    out <- gpu_matmul(A, B)
    out
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_preview_matmul_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B, tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul return subset parity", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    out <- matrix(0, 2, 2)
    return(gpu_matmul(A, B))
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_preview_matmul_return_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B, tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul return parity without predeclared output", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    return(gpu_matmul(A, B))
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_preview_matmul_return_no_out_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B, tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul handles zero-row rectangular output", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    return(gpu_matmul(A, B))
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_preview_matmul_zero_row_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(numeric(0), nrow = 0, ncol = 2)
  B <- matrix(as.double(c(1, 2, 3, 4, 5, 6)), nrow = 2, ncol = 3)
  expect_error(built$func(A, B), "output matrix dims must be positive")
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul honors NA forbid policy", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    return(gpu_matmul(A, B))
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_preview_matmul_na_forbid_rt",
    gpu_jit_mode = "unified_preview",
    na_guard = "forbid",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, NA_real_, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_error(built$func(A, B), "NA")
})

test_that("runtime: gpu_jit auto gpu_sum/gpu_mean parity routes loop", {  skip_if_no_mojo()

  f_sum <- function(x) {
    out <- 0
    out <- gpu_sum(x)
    out
  }
  f_mean <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }

  built_sum <- mojor_build(
    f_sum,
    x = "f64[]",
    name = "t_gpu_jit_auto_sum_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )
  built_mean <- mojor_build(
    f_mean,
    x = "f64[]",
    name = "t_gpu_jit_auto_mean_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1.5, -2.0, 3.25, 8.0)
  expect_equal(as.numeric(built_sum$func(x)), as.numeric(sum(x)), tolerance = 1e-8)
  expect_equal(as.numeric(built_mean$func(x)), as.numeric(mean(x)), tolerance = 1e-8)
  expect_identical(built_sum$trans$gpu_jit$route, "loop")
  expect_identical(built_mean$trans$gpu_jit$route, "loop")
})

test_that("runtime: gpu_jit auto gpu_matmul return parity without predeclared output", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    return(gpu_matmul(A, B))
  }

  built <- mojor_build(
    f_mm,
    A = "f64[,]",
    B = "f64[,]",
    name = "t_gpu_jit_auto_matmul_return_no_out_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B, tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: gpu_jit auto promotes simple elementwise loop parity", {  skip_if_no_mojo()

  f_ew <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }

  built <- mojor_build(
    f_ew,
    x = "f64[]",
    name = "t_gpu_jit_auto_elementwise_loop_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1.5, -2.0, 3.25, 8.0)
  expect_equal(as.numeric(built$func(x)), as.numeric(x * 2 + 1), tolerance = 1e-8)
  expect_identical(built$trans$gpu_jit$route, "loop")
  expect_true(isTRUE(built$trans$elementwise$gpu_buf_emitted))
})

test_that("runtime: gpu_jit auto unified elementwise honors NA forbid policy", {  skip_if_no_mojo()

  f_ew <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }

  built <- mojor_build(
    f_ew,
    x = "f64[]",
    name = "t_gpu_jit_auto_elementwise_na_forbid_rt",
    gpu_jit_mode = "auto",
    na_guard = "forbid",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1.5, NA_real_, 3.25)
  expect_error(built$func(x), "NA")
  expect_identical(built$trans$gpu_jit$route, "loop")
  expect_true(isTRUE(built$trans$elementwise$gpu_buf_emitted))
})

test_that("runtime: unified preview gpu_sum/gpu_mean parity for i32 lane", {  skip_if_no_mojo()

  f_sum <- function(x) {
    out <- 0L
    out <- gpu_sum(x)
    out
  }
  f_mean <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }

  built_sum <- mojor_build(
    f_sum,
    x = "i32[]",
    name = "t_gpu_jit_preview_sum_i32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_mean <- mojor_build(
    f_mean,
    x = "i32[]",
    name = "t_gpu_jit_preview_mean_i32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  x <- c(1L, -2L, 3L, 8L)
  expect_equal(as.integer(built_sum$func(x)), as.integer(sum(x)))
  expect_equal(as.numeric(built_mean$func(x)), as.numeric(mean(x)), tolerance = 1e-8)
  expect_identical(built_sum$trans$gpu_jit$route, "loop")
  expect_identical(built_mean$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_sum/gpu_mean parity for f32 lane", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f_sum <- function(x) {
    out <- as.single(0)
    out <- gpu_sum(x)
    out
  }
  f_mean <- function(x) {
    out <- as.single(0)
    out <- gpu_mean(x)
    out
  }

  built_sum <- mojor_build(
    f_sum,
    x = "f32[]",
    name = "t_gpu_jit_preview_sum_f32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_mean <- mojor_build(
    f_mean,
    x = "f32[]",
    name = "t_gpu_jit_preview_mean_f32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  x <- float::fl(c(1.5, -2.0, 3.25, 8.0))
  expect_equal(
    as.numeric(float::dbl(built_sum$func(x))),
    as.numeric(sum(float::dbl(x))),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(float::dbl(built_mean$func(x))),
    as.numeric(mean(float::dbl(x))),
    tolerance = 1e-6
  )
  expect_identical(built_sum$trans$gpu_jit$route, "loop")
  expect_identical(built_mean$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul assign subset parity for i32 lane", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    out <- matrix(0L, 2, 2)
    out <- gpu_matmul(A, B)
    out
  }

  built <- mojor_build(
    f_mm,
    A = "i32[,]",
    B = "i32[,]",
    name = "t_gpu_jit_preview_matmul_i32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(c(1L, 2L, 3L, 4L), nrow = 2, ncol = 2)
  B <- matrix(c(2L, 0L, 1L, 5L), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul assign subset parity for f32 lane", {  skip_if_no_mojo()

  f_mm <- function(A, B) {
    out <- matrix(0.0, 2, 2)
    out <- gpu_matmul(A, B)
    out
  }

  built <- mojor_build(
    f_mm,
    A = "f32[,]",
    B = "f32[,]",
    name = "t_gpu_jit_preview_matmul_f32_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2, ncol = 2)
  B <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2, ncol = 2)
  expect_equal(built$func(A, B), A %*% B, tolerance = 1e-6)
  expect_identical(built$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul parity covers transpose variants for i32 lane", {  skip_if_no_mojo()

  f_base <- function(A, B) {
    out <- matrix(0L, 3, 4)
    out <- gpu_matmul(A, B)
    out
  }
  f_ta <- function(A, B) {
    out <- matrix(0L, 3, 4)
    out <- gpu_matmul(A, B, transpose_a = TRUE)
    out
  }
  f_tb <- function(A, B) {
    out <- matrix(0L, 3, 4)
    out <- gpu_matmul(A, B, transpose_b = TRUE)
    out
  }

  built_base <- mojor_build(
    f_base,
    A = "i32[,]",
    B = "i32[,]",
    name = "t_gpu_jit_preview_matmul_i32_rect_base_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_ta <- mojor_build(
    f_ta,
    A = "i32[,]",
    B = "i32[,]",
    name = "t_gpu_jit_preview_matmul_i32_rect_ta_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_tb <- mojor_build(
    f_tb,
    A = "i32[,]",
    B = "i32[,]",
    name = "t_gpu_jit_preview_matmul_i32_rect_tb_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A_base <- matrix(as.integer(c(1, 4, 7, 2, 5, 8)), nrow = 3, ncol = 2)
  B_base <- matrix(as.integer(c(1, 2, 3, 4, 5, 6, 7, 8)), nrow = 2, ncol = 4)
  expect_equal(built_base$func(A_base, B_base), A_base %*% B_base)

  A_ta <- matrix(as.integer(c(1, 2, 3, 4, 5, 6)), nrow = 2, ncol = 3)
  B_ta <- matrix(as.integer(c(2, 4, 6, 8, 1, 3, 5, 7)), nrow = 2, ncol = 4)
  expect_equal(built_ta$func(A_ta, B_ta), t(A_ta) %*% B_ta)

  A_tb <- matrix(as.integer(c(3, 6, 9, 4, 7, 10)), nrow = 3, ncol = 2)
  B_tb <- matrix(as.integer(c(1, 2, 3, 4, 5, 6, 7, 8)), nrow = 4, ncol = 2)
  expect_equal(built_tb$func(A_tb, B_tb), A_tb %*% t(B_tb))

  expect_identical(built_base$trans$gpu_jit$route, "loop")
  expect_identical(built_ta$trans$gpu_jit$route, "loop")
  expect_identical(built_tb$trans$gpu_jit$route, "loop")
})

test_that("runtime: unified preview gpu_matmul parity covers transpose variants for f32 lane", {  skip_if_no_mojo()
  skip_if_gpu_jit_preview_stress_not_opt_in()

  f_base <- function(A, B) {
    out <- matrix(0.0, 3, 4)
    out <- gpu_matmul(A, B)
    out
  }
  f_ta <- function(A, B) {
    out <- matrix(0.0, 3, 4)
    out <- gpu_matmul(A, B, transpose_a = TRUE)
    out
  }
  f_tb <- function(A, B) {
    out <- matrix(0.0, 3, 4)
    out <- gpu_matmul(A, B, transpose_b = TRUE)
    out
  }

  built_base <- mojor_build(
    f_base,
    A = "f32[,]",
    B = "f32[,]",
    name = "t_gpu_jit_preview_matmul_f32_rect_base_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_ta <- mojor_build(
    f_ta,
    A = "f32[,]",
    B = "f32[,]",
    name = "t_gpu_jit_preview_matmul_f32_rect_ta_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )
  built_tb <- mojor_build(
    f_tb,
    A = "f32[,]",
    B = "f32[,]",
    name = "t_gpu_jit_preview_matmul_f32_rect_tb_rt",
    gpu_jit_mode = "unified_preview",
    cache = FALSE,
    load = TRUE
  )

  A_base <- matrix(as.double(c(1, 4, 7, 2, 5, 8)), nrow = 3, ncol = 2)
  B_base <- matrix(as.double(c(1, 2, 3, 4, 5, 6, 7, 8)), nrow = 2, ncol = 4)
  expect_equal(built_base$func(A_base, B_base), A_base %*% B_base, tolerance = 1e-6)

  A_ta <- matrix(as.double(c(1, 2, 3, 4, 5, 6)), nrow = 2, ncol = 3)
  B_ta <- matrix(as.double(c(2, 4, 6, 8, 1, 3, 5, 7)), nrow = 2, ncol = 4)
  expect_equal(built_ta$func(A_ta, B_ta), t(A_ta) %*% B_ta, tolerance = 1e-6)

  A_tb <- matrix(as.double(c(3, 6, 9, 4, 7, 10)), nrow = 3, ncol = 2)
  B_tb <- matrix(as.double(c(1, 2, 3, 4, 5, 6, 7, 8)), nrow = 4, ncol = 2)
  expect_equal(built_tb$func(A_tb, B_tb), A_tb %*% t(B_tb), tolerance = 1e-6)

  expect_identical(built_base$trans$gpu_jit$route, "loop")
  expect_identical(built_ta$trans$gpu_jit$route, "loop")
  expect_identical(built_tb$trans$gpu_jit$route, "loop")
})

test_that("runtime: gpu_jit auto promotes canonical matrix2d elementwise loop", {  skip_if_no_mojo()
  skip_if_gpu_jit_preview_stress_not_opt_in()

  f_ew2d <- function(x, y) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j] * 2 + y[i, j]
      }
    }
    out
  }

  built <- mojor_build(
    f_ew2d,
    x = "f32[,]",
    y = "f32[,]",
    name = "t_gpu_jit_auto_elementwise_matrix2d_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )

  x <- matrix(as.double(runif(12)), nrow = 3, ncol = 4)
  y <- matrix(as.double(runif(12)), nrow = 3, ncol = 4)
  expect_equal(built$func(x, y), x * 2 + y, tolerance = 1e-6)
  expect_identical(built$trans$gpu_jit$route, "loop")
  expect_true(isTRUE(built$trans$elementwise$gpu_buf_emitted))
  expect_identical(built$trans$elementwise$gpu_buf_index_mode, "matrix2d")
})

test_that("runtime: gpu_jit auto promotes guarded matrix2d control-flow neighbor loop", {  skip_if_no_mojo()
  skip_if_gpu_jit_preview_stress_not_opt_in()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f_ew2d_guard <- function(x, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if (i > 1 && i < nrow(x) && j > 1 && j < ncol(x)) {
          out[i, j] <- x[i - 1, j + 1] + b
        }
      }
    }
    out
  }

  built <- mojor_build(
    f_ew2d_guard,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_jit_auto_elementwise_matrix2d_guard_neighbor_rt",
    gpu_jit_mode = "auto",
    cache = FALSE,
    load = TRUE
  )

  x <- matrix(as.double(runif(20)), nrow = 4, ncol = 5)
  b <- float::fl(0.5)
  b_host <- as.numeric(float::dbl(b))
  expected <- x
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (i > 1 && i < nrow(x) && j > 1 && j < ncol(x)) {
        expected[i, j] <- x[i - 1, j + 1] + b_host
      }
    }
  }
  expect_equal(built$func(x, b), expected, tolerance = 1e-6)
  expect_identical(built$trans$gpu_jit$route, "loop")
  expect_true(isTRUE(built$trans$elementwise$gpu_buf_emitted))
  expect_identical(built$trans$elementwise$gpu_buf_index_mode, "matrix2d")
})

.mojor_with_mock_binding_local <- function(name, value, expr) {
  target_env <- environment(mojor_guvectorize)
  if (is.null(target_env)) {
    target_env <- .GlobalEnv
  }
  has_old <- exists(name, envir = target_env, inherits = FALSE)
  if (has_old) {
    old <- get(name, envir = target_env, inherits = FALSE)
  }
  assign(name, value, envir = target_env)
  on.exit({
    if (has_old) {
      assign(name, old, envir = target_env)
    } else if (exists(name, envir = target_env, inherits = FALSE)) {
      rm(list = name, envir = target_env)
    }
  }, add = TRUE)
  force(expr)
}

test_that("runtime: guvectorize gpu multi-output uses strict elementwise lane per output", {
  captured <- list()
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    captured[[length(captured) + 1L]] <<- list(...)
    list(
      func = function(...) 1,
      trans = list(types = list(x = "f64[]", y = "f64[]"))
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) list(sum = x + y, diff = x - y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_multi",
      load = TRUE,
      cache = FALSE
    )
    expect_true(is.function(out))
  })
  expect_length(captured, 2L)
  for (k in seq_along(captured)) {
    expect_true(isTRUE(captured[[k]]$elementwise))
    expect_identical(captured[[k]]$elementwise_target, "gpu")
    expect_identical(captured[[k]]$object_mode, "off")
  }
})

test_that("runtime: guvectorize gpu rank-n strict elementwise lane uses gpu elementwise metadata", {
  captured <- list()
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    dots <- list(...)
    captured[[length(captured) + 1L]] <<- dots
    list(
      func = dots$fn,
      trans = list(types = list(x = "f64[]", y = "f64[]"))
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_rankn_strict",
      load = TRUE,
      cache = FALSE
    )
    expect_true(is.function(out))
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")
    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    y <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(out(x, y), x + y)
  })
  expect_length(captured, 1L)
  expect_true(isTRUE(captured[[1L]]$elementwise))
  expect_identical(captured[[1L]]$elementwise_target, "gpu")
  expect_identical(captured[[1L]]$object_mode, "off")
  expect_identical(captured[[1L]]$x, "f64[]")
  expect_identical(captured[[1L]]$y, "f64[]")
})

test_that("runtime: guvectorize gpu rank-n strict indexed lane uses gpu elementwise metadata", {
  captured <- list()
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    dots <- list(...)
    captured[[length(captured) + 1L]] <<- dots
    list(
      func = dots$fn,
      trans = list(types = list(x = "f64[]"))
    )
  }, {
    out <- mojor_guvectorize(
      function(x) x[i1, i2, i3] + 1,
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_rankn_indexed",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")
    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(out(x), x + 1)
  })
  expect_length(captured, 1L)
  expect_true(isTRUE(captured[[1L]]$elementwise))
  expect_identical(captured[[1L]]$elementwise_target, "gpu")
  expect_identical(captured[[1L]]$object_mode, "off")
  expect_identical(captured[[1L]]$x, "f64[]")
})

test_that("runtime: guvectorize rank-n strict non-indexed lane reports compiled_batch engine metadata", {
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = list(x = "f64[]", y = "f64[]"))
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_compiled_batch",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_core_dims_batch_engine"), "compiled_batch")
    x <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    y <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    v1 <- out(x, y)
    expect_equal(v1, x + y)
    expect_equal(out(x, y), x + y)
  })
})

test_that("runtime: guvectorize compiled_batch and forced r_loop produce identical values", {
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    compiled <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_compiled_vs_rloop_compiled",
      load = TRUE,
      cache = FALSE
    )
    forced_rloop <- mojor_guvectorize(
      function(x, y) x[i1, i2, i3] + y[i1, i2, i3],
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      target = "gpu",
      name = "t_gpu_jit_preview_guvectorize_compiled_vs_rloop_rloop",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(compiled, "mojor_core_dims_batch_engine"), "compiled_batch")
    expect_identical(attr(forced_rloop, "mojor_core_dims_batch_engine"), "r_loop")
    x <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    y <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    expect_equal(compiled(x, y), forced_rloop(x, y))
    expect_equal(compiled(x, y), forced_rloop(x, y))
  })
})

test_that("runtime: guvectorize gpu multi-output compile failures stay explicit", {
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    dots <- list(...)
    if (identical(dots$name, "t_gpu_jit_preview_guvectorize_multi_fail_out2")) {
      stop("forced compile failure")
    }
    list(
      func = function(...) 1,
      trans = list(types = list(x = "f64[]", y = "f64[]"))
    )
  }, {
    expect_error(
      mojor_guvectorize(
        function(x, y) list(sum = x + y, diff = x - y),
        signature = list(x = "f64[]", y = "f64[]"),
        core_dims = "(n),(n)->(n),(n)",
        target = "gpu",
        name = "t_gpu_jit_preview_guvectorize_multi_fail",
        load = TRUE,
        cache = FALSE
      ),
      "gpu target core kernel #2 failed strict compilation for core_dims multi-output"
    )
  })
})

test_that("runtime: guvectorize gpu rank-n single-output compile failures stay explicit", {
  .mojor_with_mock_binding_local("mojor_build", function(...) {
    stop("forced compile failure")
  }, {
    expect_error(
      mojor_guvectorize(
        function(x) x,
        signature = list(x = "f64[3d]"),
        core_dims = "(a,b,c)->(a,b,c)",
        target = "gpu",
        name = "t_gpu_jit_preview_guvectorize_rankn_fail",
        load = TRUE,
        cache = FALSE
      ),
      "gpu target core kernel failed strict compilation for core_dims rank-n"
    )
  })
})
