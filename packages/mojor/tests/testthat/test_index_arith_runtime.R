# PR-B2: Index Arithmetic Runtime Verification
# Execution tests for new index patterns

test_that("index with scalar offset executes correctly", {  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", offset = "i32", name = "t_idx_offset_rt")
  
  # offset=0: should give same array back
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  result <- built$func(x, 0L)
  expect_equal(result, x)
  
  # offset=1: should shift read positions
  # R indices: i+1 where i=1,2,3,4,5 gives 2,3,4,5,6
  # x[2]=20, x[3]=30, x[4]=40, x[5]=50, x[6]=NaN (out of bounds)
  result2 <- built$func(x, 1L)
  expect_equal(result2[1:4], c(20.0, 30.0, 40.0, 50.0))
  # Last element is out of bounds -> NaN in unsafe mode
  expect_true(is.nan(result2[5]))
})

test_that("index with scalar subtraction executes correctly", {  f <- function(x, stride) {
    out <- numeric(length(x) - stride)
    for (i in seq_len(length(x) - stride)) {
      out[i] <- x[i + stride]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", stride = "i32", name = "t_idx_stride_rt")
  
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  # stride=2: loop runs i=1,2,3 reads x[3],x[4],x[5] = 3,4,5
  result <- built$func(x, 2L)
  expect_equal(result, c(3.0, 4.0, 5.0))
})

test_that("index with multiplication executes correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i * 2]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_idx_mul_rt")
  
  # x: 10, 20, 30, 40, 50, 60
  # n=3: loop i=1,2,3 reads x[2],x[4],x[6] = 20,40,60
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  result <- built$func(x, 3L)
  expect_equal(result, c(20.0, 40.0, 60.0))
})

test_that("index with division executes correctly", {  f <- function(x) {
    out <- numeric(3)
    for (i in seq_len(3)) {
      out[i] <- x[i / 2 + 1]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", name = "t_idx_div_rt")
  
  # R: integer division i/2 for i=1,2,3 gives 0,1,1
  # i/2 + 1 gives 1,2,2
  # reads x[1],x[2],x[2]
  x <- c(100.0, 200.0, 300.0, 400.0)
  result <- built$func(x)
  expect_equal(result, c(100.0, 200.0, 200.0))
})

test_that("scalar offset in reverse order executes correctly", {  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[offset + i]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", offset = "i32", name = "t_idx_offset_rev_rt")
  
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  # offset=1: reads x[2],x[3],x[4],x[5],x[6] = 20,30,40,50,NaN
  result <- built$func(x, 1L)
  expect_equal(result[1:4], c(20.0, 30.0, 40.0, 50.0))
  expect_true(is.nan(result[5]))
})

test_that("index arithmetic with bounds_check works", {  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  
  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", offset = "i32", 
                       name = "t_idx_bounds_rt", bounds_check = TRUE)
  
  x <- c(10.0, 20.0, 30.0)
  result <- built$func(x, 0L)
  expect_equal(result, x)
})

test_that("index arithmetic matches pure R output for in-bounds elements", {  f_compiled <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  f_pure_r <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      idx <- i + offset
      if (idx >= 1 && idx <= length(x)) {
        out[i] <- x[idx]
      } else {
        out[i] <- NaN  # Match unsafe mode behavior
      }
    }
    out
  }
  
  built <- mojor_build(f_compiled, x = "f64[]", offset = "i32",
                       name = "t_idx_parity_rt", na_mode = "unsafe")
  
  x <- runif(100)
  offset <- 5L
  
  compiled_result <- built$func(x, offset)
  r_result <- f_pure_r(x, offset)
  
  expect_equal(compiled_result, r_result, tolerance = 1e-10)
})
