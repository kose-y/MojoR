library(testthat)

test_that("slice write lowers to inner loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice_write")
  expect_mojor_any_match(trans$mojo, c("__mojor_j", "__mojor_i1"), fixed = TRUE)
  expect_mojor_any_match(trans$mojo, c("for __mojor_j", "for __mojor_i1"), fixed = TRUE)
  expect_mojor_any_match(
    trans$mojo,
    c("out[Int(__mojor_j)]", "out[__mojor_j", "out[__mojor_i1"),
    fixed = TRUE
  )
})

test_that("logical index assignment lowers to masked inner loop", {  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", name = "t_logical_write")
  expect_mojor_any_match(trans$mojo, c("for __mojor_j", "for __mojor_mask_j"), fixed = TRUE)
  expect_mojor_any_match(
    trans$mojo,
    c("mask[__mojor_j", "_mojor_read_lgl(mask, __mojor_mask_j"),
    fixed = TRUE
  )
  expect_true(grepl("continue", trans$mojo))
})

# --- RHS slice expression tests ---

test_that("matched 1D slice copy: out[1:n] <- x[1:n]", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice_copy")
  expect_true(nzchar(trans$mojo))
  # Inner loop variable should appear
  expect_mojor_any_match(trans$mojo, c("__mojor_j", "__mojor_i1"), fixed = TRUE)
  # Both LHS and RHS should use the loop var for indexed access
  expect_true(grepl("out\\[", trans$mojo))
  expect_true(grepl("x\\[", trans$mojo) || grepl("_mojor_read_f64\\(x", trans$mojo))
})

test_that("binop with RHS slices: out[1:n] <- x[1:n] + y[1:n]", {
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n] + y[1:n]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32",
                           name = "t_slice_binop")
  expect_true(nzchar(trans$mojo))
  # Should have inner loop
  expect_mojor_any_match(trans$mojo, c("__mojor_j", "__mojor_i1"), fixed = TRUE)
  # Should reference both x and y in the inner loop
  expect_true(grepl("x\\[", trans$mojo) || grepl("_mojor_read_f64\\(x", trans$mojo))
  expect_true(grepl("y\\[", trans$mojo) || grepl("_mojor_read_f64\\(y", trans$mojo))
})

test_that("slice times scalar: out[1:n] <- x[1:n] * 2.0", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n] * 2.0
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice_scalar")
  expect_true(nzchar(trans$mojo))
  expect_mojor_any_match(trans$mojo, c("__mojor_j", "__mojor_i1"), fixed = TRUE)
  # Should have multiplication by 2.0
  expect_true(grepl("2\\.0", trans$mojo))
})

test_that("self-referencing slice accumulation: out[1:n] <- x[1:n] + out[1:n]", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n] + out[1:n]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice_accum")
  expect_true(nzchar(trans$mojo))
  expect_mojor_any_match(trans$mojo, c("__mojor_j", "__mojor_i1"), fixed = TRUE)
})
