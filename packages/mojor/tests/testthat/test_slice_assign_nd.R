library(testthat)

test_that("matrix slice assignment recycles rhs", {  f <- function(x) {
    out <- matrix(0, nrow = 3, ncol = 4)
    for (i in seq_along(x)) {
      out[1:2, 2:4] <- x
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    name = "t_slice_matrix",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(c(1, 2, 3))
  res <- built$func(x)
  expected <- matrix(0, nrow = 3, ncol = 4)
  expected[1:2, 2:4] <- x
  expect_equal(dim(res), dim(expected))
  expect_equal(as.vector(res), as.vector(expected))
})

test_that("array slice assignment recycles rhs", {  f <- function(x) {
    out <- array(0, dim = c(2, 2, 2))
    for (i in seq_along(x)) {
      out[1:2, 1, 2] <- x
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    name = "t_slice_array",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(c(5, 6))
  res <- built$func(x)
  expected <- array(0, dim = c(2, 2, 2))
  expected[1:2, 1, 2] <- x
  expect_equal(dim(res), dim(expected))
  expect_equal(as.vector(res), as.vector(expected))
})
