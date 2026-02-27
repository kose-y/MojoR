library(testthat)

test_that("slice syntax lowers to loop-var indexing", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[1:n]
    }
    out
  }
  expect_mojor_error_or_result(
    run = function() mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice"),
    error_patterns = c(
      "top-level loop not supported by IR",
      "code generation failed for loop",
      "Loop not supported",
      "strict IR emission produced empty output",
      "strict IR emission failed",
      "IR statement emission failed"
    ),
    on_success = function(trans) {
      expect_mojor_any_match(trans$mojo, "_mojor_read_f64", fixed = TRUE)
      expect_mojor_any_match(
        trans$mojo,
        c(
          "Int\\(\\(i - 1\\)\\)",
          "Int\\(\\(\\(_mojor_i\\) - 1\\)\\)",
          "for __mojor_i1 in range"
        )
      )
    }
  )
})

test_that("offset indexing uses bounds helpers", {  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i - 1] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_offset")
  expect_mojor_any_match(trans$mojo, c("_mojor_oob", "_mojor_read_f64"), fixed = TRUE)
  expect_true(grepl("i - 1", trans$mojo, fixed = TRUE))
  expect_mojor_any_match(
    trans$mojo,
    c("\\(\\(i - 1\\) - 1\\)", "\\(Float64\\(i\\) - 1\\) - 1", "if \\(i - 1\\) == 0")
  )
})

test_that("nd slice assignment uses zero-based inner loop indices", {  f <- function(x) {
    out <- array(0, dim = c(2, 2, 2))
    for (i in seq_along(x)) {
      out[1:2, 1, 2] <- x
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_nd_slice_index")
  expect_true(grepl("for __mojor_i1 in range\\(0,", trans$mojo))
  expect_true(grepl("__mojor_tensor_out\\[__mojor_i1", trans$mojo))
  expect_true(
    grepl("1 - 1", trans$mojo, fixed = TRUE) ||
      grepl("__mojor_tensor_out\\[__mojor_i1, Int\\(0\\), Int\\(1\\)\\]", trans$mojo)
  )
})

test_that("range slice lowering does not leak __pos_vec_sel__ markers", {  f <- function(x) {
    out <- array(0, dim = c(2, 2, 2))
    for (i in seq_along(x)) {
      out[1:2, 1, 2] <- x[1:2]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_nd_slice_marker_free")
  expect_false(grepl("__pos_vec_sel__", trans$mojo, fixed = TRUE))
})

test_that("array output ND writes avoid matrix-only out dim vars", {  f <- function(x) {
    out <- array(0, dim = c(2, 2, 2))
    for (i in seq_along(x)) {
      out[1:2, 1, 2] <- x
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_nd_dim_vars")
  expect_false(grepl("nrow_out_i", trans$mojo, fixed = TRUE))
  expect_false(grepl("ncol_out_i", trans$mojo, fixed = TRUE))
})

test_that("selector index expressions receive explicit selector lengths", {  f <- function(vals, mask, rows_a, rows_b) {
    out <- matrix(0, 2, 2)
    for (i in seq_along(vals)) {
      out[ifelse(mask, rows_a, rows_b), 1] <- vals[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    vals = "f64[]",
    mask = "lgl[]",
    rows_a = "i32[]",
    rows_b = "i32[]",
    name = "t_selector_mask_len"
  )
  expect_true(grepl("__mojor_len_mask", trans$mojo, fixed = TRUE))
  expect_true(grepl("n_mask_i", trans$mojo, fixed = TRUE))
  expect_true(grepl("_mojor_read_i32(mask", trans$mojo, fixed = TRUE))
})

test_that("slice assignment scalarizes RHS array vars in writes", {  f <- function(vals, n) {
    out <- matrix(0, n, 2)
    for (i in seq_len(n)) {
      out[i, ] <- vals
    }
    out
  }
  trans <- mojor_transpile(f, vals = "f64[]", n = "i32", name = "t_rhs_scalarize")
  expect_true(grepl("_mojor_read_f64(vals", trans$mojo, fixed = TRUE))
  expect_false(grepl("= vals\\b", trans$mojo))
})
