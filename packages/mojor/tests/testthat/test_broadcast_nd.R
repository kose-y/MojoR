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

broadcast_apply3 <- function(x, y, z) {
  dx <- dim(x)
  dy <- dim(y)
  dz <- dim(z)
  if (is.null(dx)) dx <- length(x)
  if (is.null(dy)) dy <- length(y)
  if (is.null(dz)) dz <- length(z)
  dx <- as.integer(dx)
  dy <- as.integer(dy)
  dz <- as.integer(dz)
  ndim <- max(length(dx), length(dy), length(dz))
  dx <- c(rep.int(1L, ndim - length(dx)), dx)
  dy <- c(rep.int(1L, ndim - length(dy)), dy)
  dz <- c(rep.int(1L, ndim - length(dz)), dz)
  out_dims <- pmax(dx, dy, dz)
  if (any(dx != 1L & dx != out_dims)) stop("dx not broadcastable")
  if (any(dy != 1L & dy != out_dims)) stop("dy not broadcastable")
  if (any(dz != 1L & dz != out_dims)) stop("dz not broadcastable")
  out_len <- prod(out_dims)
  out <- array(0, dim = out_dims)
  stride_x <- c(1L, cumprod(dx[-length(dx)]))
  stride_y <- c(1L, cumprod(dy[-length(dy)]))
  stride_z <- c(1L, cumprod(dz[-length(dz)]))
  for (i in seq_len(out_len)) {
    idx <- i - 1L
    coords <- integer(ndim)
    for (k in seq_len(ndim)) {
      coords[k] <- idx %% out_dims[k]
      idx <- idx %/% out_dims[k]
    }
    cx <- ifelse(dx == 1L, 0L, coords)
    cy <- ifelse(dy == 1L, 0L, coords)
    cz <- ifelse(dz == 1L, 0L, coords)
    ix <- sum(cx * stride_x) + 1L
    iy <- sum(cy * stride_y) + 1L
    iz <- sum(cz * stride_z) + 1L
    out[i] <- x[ix] + y[iy] + z[iz]
  }
  out
}

test_that("broadcast_nd applies N-dim broadcasting", {  f <- function(x, y) {
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
    broadcast = "broadcast_nd",
    name = "t_bcast_nd",
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

test_that("broadcast_nd supports multi-arg differing ranks", {  f <- function(x, y, z) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i] + z[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    z = "f64[]",
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_3",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.double(1:6), dim = c(2, 3, 1))
  y <- array(as.double(c(10, 20, 30)), dim = c(1, 3, 1))
  z <- array(as.double(c(100, 200, 300, 400)), dim = c(1, 1, 4))
  expected <- broadcast_apply3(x, y, z)
  res <- built$func(x, y, z)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})

test_that("broadcast_nd errors on incompatible dims", {  f <- function(x, y) {
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
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_bad",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.double(1:6), dim = c(2, 3))
  y <- array(as.double(1:4), dim = c(4, 1))
  expect_error(built$func(x, y), "broadcast: incompatible dims")
})

test_that("broadcast_nd handles zero-length dims", {  f <- function(x, y) {
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
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_zero",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.double(numeric(0)), dim = c(0, 3))
  y <- array(as.double(c(10, 20, 30)), dim = c(1, 3))
  res <- built$func(x, y)
  expect_equal(dim(res), c(0L, 3L))
  expect_equal(length(res), 0L)
})

test_that("broadcast_nd supports scalar and vector mixed ranks", {  f <- function(x, y) {
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
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_scalar_vec",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.double(1), dim = c(1, 1))
  y <- array(as.double(1:6), dim = c(2, 3))
  expected <- broadcast_apply(x, y, `+`)
  res <- built$func(x, y)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})

test_that("broadcast_nd supports vector vs matrix ranks", {  f <- function(x, y) {
    out <- numeric(length(y))
    for (i in seq_along(y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_vec_mat",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:3)
  y <- array(as.double(1:6), dim = c(2, 3))
  expected <- broadcast_apply(x, y, `+`)
  res <- built$func(x, y)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})

test_that("broadcast_nd supports mixed f32/f64 with explicit cast", {  skip_if_not_installed("float")
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.double(x[i]) + y[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f32[]",
    y = "f64[]",
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_f32_f64",
    cache = FALSE,
    load = TRUE
  )
  x <- float::fl(as.double(c(1, 2, 3, 4)))
  y <- array(as.double(c(10, 20, 30, 40)), dim = c(1, 4))
  x_num <- float::dbl(x)
  expected <- broadcast_apply(x_num, y, `+`)
  res <- built$func(x, y)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})

test_that("broadcast_nd supports mixed i32/f64 with explicit cast", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.double(x[i]) * 2.0 + y[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "i32[]",
    y = "f64[]",
    broadcast = "broadcast_nd",
    name = "t_bcast_nd_i32_f64",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.integer(c(1, 2, 3, 4)), dim = c(2, 2, 1))
  y <- array(as.double(c(10, 20)), dim = c(1, 2, 1))
  x_num <- array(as.double(x), dim = dim(x))
  expected <- broadcast_apply(x_num, y, function(a, b) a * 2 + b)
  res <- built$func(x, y)
  expect_equal(dim(res), dim(expected))
  expect_equal(res, expected)
})
