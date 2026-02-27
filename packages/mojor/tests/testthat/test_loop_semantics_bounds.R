library(testthat)

# ---- Bounds Check Elimination Tests ----

test_that("bounds_check parameter is validated", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  
  # Valid values
  expect_error(mojor_transpile(f, x = "f64[]", bounds_check = TRUE), NA)
  expect_error(mojor_transpile(f, x = "f64[]", bounds_check = FALSE), NA)
  
  # Invalid values
  expect_error(mojor_transpile(f, x = "f64[]", bounds_check = "yes"), "bounds_check must be TRUE or FALSE")
  expect_error(mojor_transpile(f, x = "f64[]", bounds_check = c(TRUE, FALSE)), "bounds_check must be TRUE or FALSE")
})

test_that("bounds_check parameter is returned in transpile output", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", bounds_check = FALSE)
  expect_false(trans$bounds_check)
  
  trans_true <- mojor_transpile(f, x = "f64[]", bounds_check = TRUE)
  expect_true(trans_true$bounds_check)
})

test_that("bounds_check=FALSE eliminates runtime bounds checks", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]  # Safe indexed read/write lane
    }
    out
  }
  
  # With bounds_check=FALSE, should still work correctly
  # (bounds are provably safe from loop structure)
  built <- mojor_build(f, x = "f64[]", name = "t_nobc", bounds_check = FALSE, cache = FALSE, load = TRUE)
  
  x <- as.double(1:10)
  expect_equal(built$func(x), x)
})

test_that("bounds_check parameter affects code generation", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  
  # Test that bounds_check parameter is passed through correctly
  built_true <- mojor_build(f, x = "f64[]", name = "t_bc_on", bounds_check = TRUE, cache = FALSE, load = TRUE)
  built_false <- mojor_build(f, x = "f64[]", name = "t_bc_off", bounds_check = FALSE, cache = FALSE, load = TRUE)
  
  x <- as.double(1:10)
  expect_equal(built_true$func(x), f(x))
  expect_equal(built_false$func(x), f(x))
})

test_that("bounds_check=TRUE errors on out-of-bounds reads", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  
  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    offset = "i32",
    name = "t_bounds_oob_error",
    bounds_check = TRUE,
    cache = FALSE,
    load = TRUE
  )
  
  expect_error(
    built$func(c(10.0, 20.0, 30.0), 1L),
    "Index out of bounds"
  )
})

test_that("bounds_check=FALSE preserves legacy sentinel behavior for out-of-bounds reads", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  
  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    offset = "i32",
    name = "t_bounds_oob_legacy",
    bounds_check = FALSE,
    cache = FALSE,
    load = TRUE
  )
  
  out <- built$func(c(10.0, 20.0, 30.0), 1L)
  expect_true(is.nan(out[3]))
})

test_that("mojor_options(index_bounds=TRUE) enables strict out-of-bounds errors", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)

  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32",
    name = "t_bounds_oob_optin",
    cache = FALSE,
    load = TRUE
  )

  expect_error(
    built$func(c(10.0, 20.0, 30.0), 10L),
    "Index out of bounds"
  )
})

test_that("bounds_check=TRUE errors on out-of-bounds scalar writes", {
  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  f <- function(idx) {
    out <- numeric(3)
    out[1] <- 1.0
    out[2] <- 2.0
    out[3] <- 3.0
    out[idx] <- 99.0
    out
  }

  built <- mojor_build(
    f,
    idx = "i32",
    name = "t_bounds_oob_write_scalar",
    bounds_check = TRUE,
    cache = FALSE,
    load = TRUE
  )

  expect_error(built$func(0L), "Index out of bounds")
  expect_error(built$func(4L), "Index out of bounds")
  expect_equal(built$func(2L), c(1.0, 99.0, 3.0))
})

test_that("bounds_check=TRUE errors on out-of-bounds slice writes", {
  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  f <- function(end_idx) {
    out <- numeric(3)
    out[1] <- 1.0
    out[2] <- 2.0
    out[3] <- 3.0
    out[1:end_idx] <- 9.0
    out
  }

  built <- mojor_build(
    f,
    end_idx = "i32",
    name = "t_bounds_oob_write_slice",
    bounds_check = TRUE,
    cache = FALSE,
    load = TRUE
  )

  expect_error(built$func(5L), "Index out of bounds")
  expect_equal(built$func(2L), c(9.0, 9.0, 3.0))
})

test_that("bounds_check=TRUE errors on out-of-bounds ND reads (strict + non-strict)", {
  skip_if_no_mojo()

  f <- function(arr, k, n) {
    out <- numeric(n)
    dim_k <- dim(arr)[k]
    for (i in seq_len(n)) {
      if (i <= dim_k) {
        out[i] <- arr[i, 1, 1]
      }
    }
    out
  }

  arr <- array(as.double(1:24), dim = c(2, 3, 4))
  modes <- list(strict = TRUE, non_strict = FALSE)
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    built <- mojor_build(
      f,
      arr = "f64[3d]",
      k = "i32",
      n = "i32",
      name = paste0("t_bounds_oob_read_nd_", mode_name),
      bounds_check = TRUE,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(arr, 1L, 2L), f(arr, 1L, 2L))
    expect_error(built$func(arr, 2L, 3L), "Index out of bounds")
  }
})

test_that("bounds_check=TRUE errors on out-of-bounds ND writes (strict + non-strict)", {
  skip_if_no_mojo()

  f <- function(arr, n) {
    out <- arr
    for (i in seq_len(n)) {
      out[i, 1, 1] <- 99.0
    }
    out
  }

  arr <- array(as.double(1:24), dim = c(2, 3, 4))
  modes <- list(strict = TRUE, non_strict = FALSE)
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    built <- mojor_build(
      f,
      arr = "f64[3d]",
      n = "i32",
      name = paste0("t_bounds_oob_write_nd_", mode_name),
      bounds_check = TRUE,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(arr, 2L)[, 1, 1], c(99.0, 99.0))
    expect_error(built$func(arr, 3L), "Index out of bounds")
  }
})

test_that("bounds_check=TRUE errors on out-of-bounds ND exclusion reads", {
  skip_if_no_mojo()

  f <- function(mat, col_idx) {
    out <- numeric(1)
    out[1] <- mat[-1, col_idx]
    out
  }

  mat <- matrix(as.double(1:9), nrow = 3, ncol = 3, byrow = TRUE)
  modes <- list(strict = TRUE, non_strict = FALSE)
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    built <- mojor_build(
      f,
      mat = "f64[,]",
      col_idx = "i32",
      name = paste0("t_bounds_oob_read_nd_excl_", mode_name),
      bounds_check = TRUE,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(mat, 2L), c(mat[-1, 2][1]))
    expect_error(built$func(mat, 5L), "Index out of bounds")
  }
})
test_that("bounds_check=TRUE errors on out-of-bounds ND exclusion writes", {
  skip_if_no_mojo()

  f <- function(col_idx) {
    out <- matrix(0, nrow = 3, ncol = 3)
    out[1, 1] <- 1
    out[1, 2] <- 2
    out[1, 3] <- 3
    out[2, 1] <- 4
    out[2, 2] <- 5
    out[2, 3] <- 6
    out[3, 1] <- 7
    out[3, 2] <- 8
    out[3, 3] <- 9
    out[-1, col_idx] <- 99.0
    out
  }

  built <- mojor_build(
    f,
    col_idx = "i32",
    name = "t_bounds_oob_write_nd_excl",
    bounds_check = TRUE,
    cache = FALSE,
    load = TRUE
  )

  baseline <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  in_bounds <- baseline
  in_bounds[-1, 2] <- 99.0

  expect_equal(built$func(2L), in_bounds)
  expect_error(built$func(5L), "Index out of bounds")
})
