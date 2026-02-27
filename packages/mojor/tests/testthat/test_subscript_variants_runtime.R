# PR-B2 Phase 2: Subscript Assignment Variants - Runtime Tests

source("helper-mojo.R")

test_that("matrix column assignment executes correctly", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in seq_len(n)) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_mat_col_rt")
  
  x <- c(1.0, 2.0, 3.0)
  result <- built$func(x, 3L)
  
  # Check dimensions
  expect_equal(dim(result), c(2, 3))
  
  # Check values: column j should be [x[j], x[j]*2]
  expect_equal(result[, 1], c(1.0, 2.0))
  expect_equal(result[, 2], c(2.0, 4.0))
  expect_equal(result[, 3], c(3.0, 6.0))
})

test_that("matrix column assignment with scalar executes correctly", {  skip_if_no_mojo()
  
  f <- function(n) {
    mat <- matrix(0, 3, n)
    for (j in seq_len(n)) {
      mat[, j] <- 5.0
    }
    mat
  }
  
  built <- mojor_build(f, n = "i32", name = "t_mat_col_scalar_rt")
  
  result <- built$func(3L)
  
  expect_equal(dim(result), c(3, 3))
  expect_true(all(result == 5.0))
})

test_that("3D array slice assignment executes correctly", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    arr <- array(0, dim = c(2, 2, n))
    for (k in seq_len(n)) {
      arr[, , k] <- c(x[k], x[k]*2, x[k]*3, x[k]*4)
    }
    arr
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_arr_3d_rt")
  
  x <- c(1.0, 2.0)
  result <- built$func(x, 2L)
  
  expect_equal(dim(result), c(2, 2, 2))
  
  # After ND fix: c(v1,v2,v3,v4) correctly fills [v1,v3; v2,v4] (column-major)
  # Slice 1: c(1,2,3,4) fills [1,3; 2,4]
  expect_equal(result[, , 1], matrix(c(1, 2, 3, 4), 2, 2))
  # Slice 2: c(2,4,6,8) fills [2,6; 4,8]
  expect_equal(result[, , 2], matrix(c(2, 4, 6, 8), 2, 2))
})

test_that("matrix assignment with offset index executes correctly", {  skip_if_no_mojo()
  
  f <- function(x, offset, n) {
    mat <- matrix(0, n, 2)
    for (i in seq_len(n - offset)) {
      mat[i + offset, ] <- c(x[i], x[i] * 2)
    }
    mat
  }
  
  built <- mojor_build(f, x = "f64[]", offset = "i32", n = "i32", 
                       name = "t_mat_offset_rt")
  
  x <- c(10.0, 20.0, 30.0)
  result <- built$func(x, 1L, 4L)
  
  expect_equal(dim(result), c(4, 2))
  
  # Row 1 should be zeros (offset=1 means start at row 2)
  expect_equal(result[1, ], c(0, 0))
  
  # Rows 2-4 should have values
  expect_equal(result[2, ], c(10.0, 20.0))
  expect_equal(result[3, ], c(20.0, 40.0))
  expect_equal(result[4, ], c(30.0, 60.0))
})

test_that("3D array element-wise assignment works correctly", {  skip_if_no_mojo()
  
  # Workaround for c() limitation: use explicit nested loops
  f <- function(x, n) {
    arr <- array(0, dim = c(2, 2, n))
    for (k in seq_len(n)) {
      for (i in seq_len(2)) {
        for (j in seq_len(2)) {
          arr[i, j, k] <- x[k] * (i + (j-1)*2)
        }
      }
    }
    arr
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_arr_3d_explicit_rt")
  
  x <- c(1.0, 2.0)
  result <- built$func(x, 2L)
  
  expect_equal(dim(result), c(2, 2, 2))
  # Slice 1: [1,2; 3,4] * 1
  expect_equal(result[, , 1], matrix(c(1, 2, 3, 4), 2, 2))
  # Slice 2: [1,2; 3,4] * 2
  expect_equal(result[, , 2], matrix(c(2, 4, 6, 8), 2, 2))
})

test_that("column assignment matches pure R output", {  skip_if_no_mojo()
  
  f_compiled <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in seq_len(n)) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }
  
  built <- mojor_build(f_compiled, x = "f64[]", n = "i32", name = "t_col_parity_rt")
  
  x <- runif(20)
  result <- built$func(x, 20L)
  expected <- f_compiled(x, 20L)
  
  expect_equal(result, expected, tolerance = 1e-10)
})
