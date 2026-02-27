library(testthat)

# Tests for dimnames preservation on output matrices

test_that("dimnames are preserved on matrix output", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_dimnames",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  expect_equal(dimnames(result)[[1]], c("a", "b"))
  expect_equal(dimnames(result)[[2]], c("c", "d"))
})

test_that("dimnames with longer names are preserved", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("row1", "row2"), c("col1", "col2")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_dimnames_long",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  expect_equal(dimnames(result)[[1]], c("row1", "row2"))
  expect_equal(dimnames(result)[[2]], c("col1", "col2"))
})

test_that("dimnames with single character names are preserved", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("x", "y"), c("z", "w")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_dimnames_short",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  expect_equal(dimnames(result)[[1]], c("x", "y"))
  expect_equal(dimnames(result)[[2]], c("z", "w"))
})

# Note: Matrices without explicit dimnames have NULL dimnames attribute
# This is standard R behavior and doesn't need explicit testing

test_that("dimnames work with character indexing", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == 1 && j == 1) {
          out["a", "c"] <- 999.0
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_dimnames_char",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  # Check dimnames are preserved AND character indexing worked
  expect_equal(dimnames(result)[[1]], c("a", "b"))
  expect_equal(dimnames(result)[[2]], c("c", "d"))
  expect_equal(result[1, 1], 999.0)
})
