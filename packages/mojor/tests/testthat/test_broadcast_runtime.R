library(testthat)

broadcast_apply <- function(x, y, op) {
  dx <- dim(x)
  dy <- dim(y)
  if (is.null(dx)) dx <- length(x)
  if (is.null(dy)) dy <- length(y)
  dx <- as.integer(dx)
  dy <- as.integer(dy)
  ndim <- max(length(dx), length(dy))
  dx <- c(rep.int(1L, ndim - length(dx)), dx)
  dy <- c(rep.int(1L, ndim - length(dy)), dy)
  out_dims <- pmax(dx, dy)
  if (any(dx != 1L & dx != out_dims)) stop("dx not broadcastable")
  if (any(dy != 1L & dy != out_dims)) stop("dy not broadcastable")
  out_len <- prod(out_dims)
  out <- array(0, dim = out_dims)
  stride_x <- c(1L, cumprod(dx[-length(dx)]))
  stride_y <- c(1L, cumprod(dy[-length(dy)]))
  for (i in seq_len(out_len)) {
    idx <- i - 1L
    coords <- integer(ndim)
    for (k in seq_len(ndim)) {
      coords[k] <- idx %% out_dims[k]
      idx <- idx %/% out_dims[k]
    }
    cx <- ifelse(dx == 1L, 0L, coords)
    cy <- ifelse(dy == 1L, 0L, coords)
    ix <- sum(cx * stride_x) + 1L
    iy <- sum(cy * stride_y) + 1L
    out[i] <- op(x[ix], y[iy])
  }
  out
}

test_that("broadcast scalar supports length-1 inputs", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "scalar",
    name = "t_bcast_scalar_rt",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(10)
  expect_equal(built$func(x, y), x + y[1])
})

test_that("broadcast recycle supports mixed lengths", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "recycle",
    name = "t_bcast_recycle_mixed",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:12)
  y <- as.double(c(10, 20, 30))
  expect_equal(built$func(x, y), x + rep(y, length.out = length(x)))
})

test_that("broadcast recycle supports multiple args", {  skip_if_no_mojo()
  
  f <- function(x, y, z) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i] - z[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    z = "f64[]",
    broadcast = "recycle",
    name = "t_bcast_recycle_multi",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:12)
  y <- as.double(c(10, 20, 30))
  z <- as.double(c(1, 2, 3, 4))
  expected <- x + rep(y, length.out = length(x)) - rep(z, length.out = length(x))
  expect_equal(built$func(x, y, z), expected)
})

test_that("broadcast scalar rejects non-scalar mismatches", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "scalar",
    name = "t_bcast_scalar_err",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(1:2)
  expect_error(built$func(x, y), "length mismatch")
})

test_that("broadcast recycle errors on non-multiple lengths", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "recycle",
    name = "t_bcast_recycle_err",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(1:2)
  expect_error(built$func(x, y), "length mismatch")
})

test_that("broadcast_nd_warn matches broadcast_nd behavior", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "broadcast_nd_warn",
    name = "t_bcast_nd_warn",
    cache = FALSE,
    load = TRUE
  )
  
  x <- array(as.double(1:6), dim = c(2, 3, 1))
  y <- array(as.double(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)), dim = c(1, 3, 4))
  expected <- broadcast_apply(x, y, `+`)
  res <- built$func(x, y)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})

test_that("broadcast_nd_warn errors on incompatible dims", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "broadcast_nd_warn",
    name = "t_bcast_nd_warn_bad",
    cache = FALSE,
    load = TRUE
  )
  
  x <- array(as.double(1:6), dim = c(2, 3))
  y <- array(as.double(1:4), dim = c(4, 1))
  expect_error(built$func(x, y), "broadcast: incompatible dims")
})
