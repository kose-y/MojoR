library(testthat)

# Tests for count/histogram reductions

test_that("histogram count with integer(length(arg)) works", {  skip_if_no_mojo()
  
  f <- function(x, bins) {
    counts <- integer(length(bins))
    for (i in seq_along(x)) {
      for (j in seq_along(bins)) {
        if (x[i] <= bins[j]) {
          counts[j] <- counts[j] + 1L
          break
        }
      }
    }
    counts
  }
  
  built <- mojor_build(f, x = "f64[]", bins = "f64[]", name = "t_hist_basic", 
                       cache = FALSE, load = TRUE)
  
  x <- c(1.0, 5.0, 10.0, 15.0, 20.0)
  bins <- c(5.0, 10.0, 15.0)
  
  result <- built$func(x, bins)
  expected <- f(x, bins)
  
  expect_equal(result, expected)
  expect_equal(length(result), length(bins))
})

test_that("histogram count with integer(scalar_arg) works", {  skip_if_no_mojo()
  
  f <- function(x, breaks, nbins) {
    counts <- integer(nbins)
    for (i in seq_along(x)) {
      for (j in seq_len(nbins)) {
        if (x[i] >= breaks[j] && x[i] < breaks[j + 1]) {
          counts[j] <- counts[j] + 1L
          break
        }
      }
    }
    counts
  }
  
  built <- mojor_build(f, x = "f64[]", breaks = "f64[]", nbins = "i32",
                       name = "t_hist_scalar", cache = FALSE, load = TRUE)
  
  x <- c(1, 2, 3, 5, 6, 7, 10, 11, 12)
  breaks <- c(0, 5, 10, 15)
  nbins <- length(breaks) - 1L
  
  result <- built$func(x, breaks, nbins)
  expected <- f(x, breaks, nbins)
  
  expect_equal(result, expected)
  expect_equal(length(result), nbins)
})

test_that("histogram with empty bins", {  skip_if_no_mojo()
  
  f <- function(x, bins) {
    counts <- integer(length(bins))
    for (i in seq_along(x)) {
      for (j in seq_along(bins)) {
        if (x[i] <= bins[j]) {
          counts[j] <- counts[j] + 1L
          break
        }
      }
    }
    counts
  }
  
  built <- mojor_build(f, x = "f64[]", bins = "f64[]", name = "t_hist_empty", 
                       cache = FALSE, load = TRUE)
  
  x <- numeric(0)
  bins <- c(5.0, 10.0, 15.0)
  
  result <- built$func(x, bins)
  expected <- f(x, bins)
  
  expect_equal(result, c(0L, 0L, 0L))
})

test_that("out_len_source is captured for length(arg)", {  f <- function(x, bins) {
    counts <- integer(length(bins))
    for (i in seq_along(x)) {
      for (j in seq_along(bins)) {
        if (x[i] <= bins[j]) {
          counts[j] <- counts[j] + 1L
          break
        }
      }
    }
    counts
  }
  
  trans <- mojor_transpile(f, x = "f64[]", bins = "f64[]", name = "t_hist_meta1")
  
  expect_equal(trans$out_len_source$kind, "array")
  expect_equal(trans$out_len_source$name, "bins")
})

test_that("out_len_source is captured for scalar arg", {  f <- function(x, nbins) {
    counts <- integer(nbins)
    for (i in seq_along(x)) {
      for (j in seq_len(nbins)) {
        counts[j] <- counts[j] + 1L
      }
    }
    counts
  }
  
  trans <- mojor_transpile(f, x = "f64[]", nbins = "i32", name = "t_hist_meta2")
  
  expect_equal(trans$out_len_source$kind, "scalar")
  expect_equal(trans$out_len_source$name, "nbins")
})
