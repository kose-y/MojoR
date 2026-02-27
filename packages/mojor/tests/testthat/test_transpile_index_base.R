library(testthat)

test_that("supports non-1-based loop starts (e.g., 2:n)", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 2:n) {
      out[i] <- x[i]
  }
  out
}
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_start_two", assume_aligned = 32L, simd_mode = "explicit")
  expect_true(grepl("range\\(2,", trans$mojo))
  expect_true(grepl("if", trans$mojo, fixed = TRUE))
  expect_false(trans$simd$emitted)
  expect_equal(trans$simd$reason, "non-1-based range")
})

test_that("index_base = zero_based keeps loop induction indexing stable", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", index_base = "zero_based", ir_only = TRUE)
  expect_true(grepl("(i - 1)", trans$mojo, fixed = TRUE))
})
