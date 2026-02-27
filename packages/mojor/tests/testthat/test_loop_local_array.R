library(testthat)

# Tests for local array allocation inside loop bodies
# The allocation is hoisted to pre-loop scope by the analysis pass;
# the in-loop constructor emits a zero-fill re-initialization.

test_that("numeric(3) inside loop body transpiles", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      temp <- numeric(3)
      temp[1] <- x[i]
      temp[2] <- x[i] * 2.0
      out[i] <- temp[1] + temp[2]
    }
    out
  }
  result <- mojor_transpile(f, x = "f64[]", name = "t_loop_local_arr")
  expect_true(!is.null(result$mojo) && nzchar(result$mojo))
  # Should have temp allocation (hoisted)
  expect_true(grepl("alloc\\[Float64\\]", result$mojo))
  # Should have re-init loop inside the outer loop
  expect_true(grepl("__mojor_reinit_i", result$mojo))
  # Should have indexed writes to temp
  expect_true(grepl("temp\\[", result$mojo))
})

test_that("integer(2) inside loop body transpiles", {
  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      buf <- integer(2)
      buf[1] <- as.integer(x[i])
      buf[2] <- as.integer(x[i] + 1L)
      out[i] <- buf[1] + buf[2]
    }
    out
  }
  result <- mojor_transpile(f, x = "i32[]", name = "t_loop_local_int")
  expect_true(!is.null(result$mojo) && nzchar(result$mojo))
  expect_true(grepl("alloc\\[Int32\\]", result$mojo))
})

test_that("dynamic-size numeric(n) inside loop body transpiles", {
  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      temp <- numeric(n)
      temp[1] <- x[i]
      out[i] <- temp[1]
    }
    out
  }
  result <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_loop_local_dyn")
  expect_true(!is.null(result$mojo) && nzchar(result$mojo))
  expect_true(grepl("alloc\\[Float64\\]", result$mojo))
})
