library(testthat)

# Tests for mixed indices support (vector indexing)

test_that("row subsetting with vector indices works", {  skip_if_no_mojo()

  f <- function(x, rows, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_along(rows)) {
      for (j in seq_len(nc)) {
        out[i, j] <- x[rows[i], j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", rows = "i32[]", nr = "i32", nc = "i32",
                       name = "t_mixed_rows", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  rows <- c(1L, 3L, 5L)
  result <- built$func(x, rows, length(rows), ncol(x))
  expected <- f(x, rows, length(rows), ncol(x))
  
  expect_equal(result, expected)
})

test_that("column subsetting with vector indices works", {  skip_if_no_mojo()

  f <- function(x, cols, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_len(nr)) {
      for (j in seq_along(cols)) {
        out[i, j] <- x[i, cols[j]]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", cols = "i32[]", nr = "i32", nc = "i32",
                       name = "t_mixed_cols", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  cols <- c(2L, 4L)
  result <- built$func(x, cols, nrow(x), length(cols))
  expected <- f(x, cols, nrow(x), length(cols))
  
  expect_equal(result, expected)
})

test_that("row and column subsetting with vector indices works", {  skip_if_no_mojo()

  f <- function(x, rows, cols, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_along(rows)) {
      for (j in seq_along(cols)) {
        out[i, j] <- x[rows[i], cols[j]]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", rows = "i32[]", cols = "i32[]", 
                       nr = "i32", nc = "i32",
                       name = "t_mixed_both", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  rows <- c(1L, 3L)
  cols <- c(2L, 4L)
  result <- built$func(x, rows, cols, length(rows), length(cols))
  expected <- f(x, rows, cols, length(rows), length(cols))
  
  expect_equal(result, expected)
})

test_that("reordering rows with vector indices works", {  skip_if_no_mojo()

  f <- function(x, rows, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_along(rows)) {
      for (j in seq_len(nc)) {
        out[i, j] <- x[rows[i], j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", rows = "i32[]", nr = "i32", nc = "i32",
                       name = "t_mixed_reorder", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  rows <- c(5L, 3L, 1L)  # Reverse order
  result <- built$func(x, rows, length(rows), ncol(x))
  expected <- f(x, rows, length(rows), ncol(x))
  
  expect_equal(result, expected)
})

test_that("duplicate indices in vector indices works", {  skip_if_no_mojo()

  f <- function(x, rows, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_along(rows)) {
      for (j in seq_len(nc)) {
        out[i, j] <- x[rows[i], j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", rows = "i32[]", nr = "i32", nc = "i32",
                       name = "t_mixed_dup", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  rows <- c(1L, 1L, 2L, 2L)  # Duplicates
  result <- built$func(x, rows, length(rows), ncol(x))
  expected <- f(x, rows, length(rows), ncol(x))
  
  expect_equal(result, expected)
})

test_that("single row extraction with vector indices works", {  skip_if_no_mojo()

  f <- function(x, rows, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_along(rows)) {
      for (j in seq_len(nc)) {
        out[i, j] <- x[rows[i], j]
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", rows = "i32[]", nr = "i32", nc = "i32",
                       name = "t_mixed_single", cache = FALSE, load = TRUE)
  
  x <- matrix(as.double(1:20), nrow = 5, ncol = 4)
  rows <- c(3L)  # Single row
  result <- built$func(x, rows, length(rows), ncol(x))
  expected <- f(x, rows, length(rows), ncol(x))
  
  expect_equal(result, expected)
  expect_equal(nrow(result), 1)
})
