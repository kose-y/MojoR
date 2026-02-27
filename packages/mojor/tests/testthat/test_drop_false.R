library(testthat)

# Tests for drop = FALSE parameter in indexing

test_that("drop = FALSE is parsed correctly in matrix indexing", {  skip_if_no_mojo()
  
  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j, drop = FALSE]
      }
    }
    out
  }
  
  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_drop_basic",
                       cache = FALSE, load = TRUE)
  
  n <- 5L
  a <- matrix(as.double(1:25), n, n)
  expect_equal(built$func(a, n), f(a, n))
})

test_that("drop = TRUE is also parsed (default behavior)", {  skip_if_no_mojo()
  
  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j, drop = TRUE]
      }
    }
    out
  }
  
  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_drop_true",
                       cache = FALSE, load = TRUE)
  
  n <- 5L
  a <- matrix(as.double(1:25), n, n)
  expect_equal(built$func(a, n), f(a, n))
})

test_that("drop = FALSE transpiles without error", {  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j, drop = FALSE]
      }
    }
    out
  }
  
  # Just verify it transpiles without error
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", name = "t_drop_trans"), NA)
})
