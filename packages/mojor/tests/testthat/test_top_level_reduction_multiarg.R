library(testthat)

test_that("top-level sum/prod/min/max accept multi-argument array forms", {
  f_sum <- function(x, y) sum(x, y)
  f_prod <- function(x, y) prod(x, y)
  f_min <- function(x, y) min(x, y)
  f_max <- function(x, y) max(x, y)

  expect_error(mojor_transpile(f_sum, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_prod, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_min, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_max, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)

  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4))
  y <- as.double(c(5, 6, 7, 8))

  b_sum <- mojor_build(f_sum, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  b_prod <- mojor_build(f_prod, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  b_min <- mojor_build(f_min, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  b_max <- mojor_build(f_max, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)

  expect_equal(as.double(b_sum$func(x, y)), as.double(f_sum(x, y)))
  expect_equal(as.double(b_prod$func(x, y)), as.double(f_prod(x, y)))
  expect_equal(as.double(b_min$func(x, y)), as.double(f_min(x, y)))
  expect_equal(as.double(b_max$func(x, y)), as.double(f_max(x, y)))
})

test_that("top-level reduction expression forms over arrays transpile", {
  f <- function(x, y) sum(x + y)

  expect_error(mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)

  skip_if_no_mojo()
  x <- as.double(c(1, 2, 3, 4))
  y <- as.double(c(5, 6, 7, 8))
  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  expect_equal(as.double(built$func(x, y)), as.double(f(x, y)))
})

test_that("top-level mean/var/sd remain single-argument only", {
  f_mean <- function(x, y) mean(x, y)
  f_var <- function(x, y) var(x, y)
  f_sd <- function(x, y) sd(x, y)

  expect_error(
    mojor_transpile(f_mean, x = "f64[]", y = "f64[]", ir_only = TRUE),
    "mean\\(\\)/var\\(\\)/sd\\(\\) reductions only support a single array argument"
  )
  expect_error(
    mojor_transpile(f_var, x = "f64[]", y = "f64[]", ir_only = TRUE),
    "mean\\(\\)/var\\(\\)/sd\\(\\) reductions only support a single array argument"
  )
  expect_error(
    mojor_transpile(f_sd, x = "f64[]", y = "f64[]", ir_only = TRUE),
    "mean\\(\\)/var\\(\\)/sd\\(\\) reductions only support a single array argument"
  )
})

test_that("top-level which.min/which.max accept expression forms over arrays", {
  f_which_min <- function(x, y) which.min(x + y)
  f_which_max <- function(x, y) which.max(x + y)

  expect_error(mojor_transpile(f_which_min, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_which_max, x = "f64[]", y = "f64[]", ir_only = TRUE), NA)

  skip_if_no_mojo()
  x <- as.double(c(1, 8, 3, 4, 5))
  y <- as.double(c(6, 1, 7, 2, 0))

  b_min <- mojor_build(f_which_min, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  b_max <- mojor_build(f_which_max, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)

  expect_equal(as.integer(b_min$func(x, y)), as.integer(f_which_min(x, y)))
  expect_equal(as.integer(b_max$func(x, y)), as.integer(f_which_max(x, y)))
})
