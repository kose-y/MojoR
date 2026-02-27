library(testthat)

test_that("fused sequential loops produce correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (i in seq_along(x)) {
      out[i] <- out[i] + 1
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_fusion_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4, 5))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# variance/sd reduction runtime tests
# ============================================================================

test_that("mojor_var_f64 produces correct results with Welford", {  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  result <- mojor_var_f64(x, sd_mode = "welford")
  expect_equal(result, var(x))
})

test_that("mojor_var_f64 produces correct results with two-pass", {  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  result <- mojor_var_f64(x, sd_mode = "two_pass")
  expect_equal(result, var(x))
})

test_that("mojor_sd_f64 produces correct results with Welford", {  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  result <- mojor_sd_f64(x, sd_mode = "welford")
  expect_equal(result, sd(x))
})

test_that("mojor_sd_f64 produces correct results with two-pass", {  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  result <- mojor_sd_f64(x, sd_mode = "two_pass")
  expect_equal(result, sd(x))
})

# ============================================================================
# stable pairwise sum reduction runtime tests
# ============================================================================

test_that("mojor_sum_f64_pairwise produces correct results", {  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  result <- mojor_sum_f64_pairwise(x)
  expect_equal(result, sum(x))
})

test_that("mojor_sum_f64_pairwise handles large arrays correctly", {  skip_if_no_mojo()
  x <- as.double(1:1000)
  result <- mojor_sum_f64_pairwise(x)
  expect_equal(result, sum(x))
})

# ============================================================================
# elementwise math functions runtime tests
# NOTE: Currently skipped due to Mojo stdlib API changes - the transpiler
# imports all math functions upfront (including pow which no longer exists).
# These tests work conceptually but need transpiler fix to selectively import.
# ============================================================================

test_that("trigonometric functions produce correct results", {  skip_if_no_mojo()
  f_sin <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- sin(x[i])
    }
    out
  }
  f_cos <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- cos(x[i])
    }
    out
  }
  f_tan <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- tan(x[i])
    }
    out
  }
  x <- as.double(c(0, pi/6, pi/4, pi/3, pi/2))
  built_sin <- mojor_build(f_sin, x = "f64[]", name = "t_sin_rt", cache = FALSE, load = TRUE)
  built_cos <- mojor_build(f_cos, x = "f64[]", name = "t_cos_rt", cache = FALSE, load = TRUE)
  built_tan <- mojor_build(f_tan, x = "f64[]", name = "t_tan_rt", cache = FALSE, load = TRUE)
  expect_equal(built_sin$func(x), f_sin(x), tolerance = 1e-14)
  expect_equal(built_cos$func(x), f_cos(x), tolerance = 1e-14)
  expect_equal(built_tan$func(x), f_tan(x), tolerance = 1e-14)
})

test_that("logarithm and exponential functions produce correct results", {  skip_if_no_mojo()
  f_log <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- log(x[i])
    }
    out
  }
  f_log1p <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- log1p(x[i])
    }
    out
  }
  f_exp <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- exp(x[i])
    }
    out
  }
  f_expm1 <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- expm1(x[i])
    }
    out
  }
  x <- as.double(c(0.1, 0.5, 1, 2, 5))
  built_log <- mojor_build(f_log, x = "f64[]", name = "t_log_rt", cache = FALSE, load = TRUE)
  built_log1p <- mojor_build(f_log1p, x = "f64[]", name = "t_log1p_rt", cache = FALSE, load = TRUE)
  built_exp <- mojor_build(f_exp, x = "f64[]", name = "t_exp_rt", cache = FALSE, load = TRUE)
  built_expm1 <- mojor_build(f_expm1, x = "f64[]", name = "t_expm1_rt", cache = FALSE, load = TRUE)
  expect_equal(built_log$func(x), f_log(x), tolerance = 1e-9)
  expect_equal(built_log1p$func(x), f_log1p(x), tolerance = 1e-9)
  expect_equal(built_exp$func(x), f_exp(x), tolerance = 1e-9)
  expect_equal(built_expm1$func(x), f_expm1(x), tolerance = 1e-9)
})

test_that("rounding functions produce correct results", {  skip_if_no_mojo()
  f_floor <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- floor(x[i])
    }
    out
  }
  f_ceiling <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ceiling(x[i])
    }
    out
  }
  f_trunc <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- trunc(x[i])
    }
    out
  }
  f_round <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- round(x[i])
    }
    out
  }
  x <- as.double(c(-2.7, -1.5, -0.3, 0.3, 1.5, 2.7))
  built_floor <- mojor_build(f_floor, x = "f64[]", name = "t_floor_rt", cache = FALSE, load = TRUE)
  built_ceil <- mojor_build(f_ceiling, x = "f64[]", name = "t_ceil_rt", cache = FALSE, load = TRUE)
  built_trunc <- mojor_build(f_trunc, x = "f64[]", name = "t_trunc_rt", cache = FALSE, load = TRUE)
  built_round <- mojor_build(f_round, x = "f64[]", name = "t_round_rt", cache = FALSE, load = TRUE)
  expect_equal(built_floor$func(x), f_floor(x))
  expect_equal(built_ceil$func(x), f_ceiling(x))
  expect_equal(built_trunc$func(x), f_trunc(x))
  expect_equal(built_round$func(x), f_round(x))
})

# ============================================================================
# bounds check elimination runtime tests
# ============================================================================

test_that("bounds_check = FALSE produces correct results for safe indexing", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2  # Safe: i is always in bounds
    }
    out
  }
  # With bounds checks
  built_checked <- mojor_build(f, x = "f64[]", name = "t_bounds_checked",
                               bounds_check = TRUE, cache = FALSE, load = TRUE)
  # Without bounds checks (faster but user must ensure safety)
  built_unchecked <- mojor_build(f, x = "f64[]", name = "t_bounds_unchecked",
                                 bounds_check = FALSE, cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4, 5))
  expect_equal(built_checked$func(x), f(x))
  expect_equal(built_unchecked$func(x), f(x))
})
