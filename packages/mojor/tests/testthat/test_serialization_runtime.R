# Runtime tests for Phase 6.2: Serialization
# These tests require Mojo compiler + backend to be available.

test_that("mojor_save + mojor_load_kernel round-trip produces correct results", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2.0
    out
  }

  built <- mojor_build(f, x = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  expect_true(file.exists(path))

  loaded <- mojor_load_kernel(path)
  expect_true(loaded$success)
  expect_is(loaded$func, "function")

  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  result_orig <- built$func(x)
  result_loaded <- loaded$func(x)
  expect_equal(result_loaded, result_orig)
  expect_equal(result_loaded, x * 2.0)
})

test_that("mojor_save with embed_source=TRUE round-trips correctly", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1.0
    out
  }

  built <- mojor_build(f, x = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path, embed_source = TRUE)

  info <- mojor_serialize(path)
  expect_true(info$has_mojo_source)

  loaded <- mojor_load_kernel(path)
  result <- loaded$func(c(10.0, 20.0))
  expect_equal(result, c(11.0, 21.0))
})

test_that("mojor_info returns correct metadata for saved kernel", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 3.0
    out
  }

  built <- mojor_build(f, x = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)

  info <- mojor_serialize(path)
  expect_equal(info$version, "1.0.0")
  expect_equal(info$format, "rds")
  expect_equal(info$kernel, built$kernel)
  expect_equal(info$cache_key, built$cache_key)
  expect_false(info$has_mojo_source)
  expect_true(nzchar(info$build_timestamp))
})

test_that("mojor_check_compatibility works on saved kernel", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2.0
    out
  }

  built <- mojor_build(f, x = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  serial <- readRDS(path)
  expect_true(mojor_check_compatibility(serial))
})

test_that("round-trip with scalar argument", {  skip_if_no_mojo()

  f <- function(x, alpha) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * alpha
    out
  }

  built <- mojor_build(f, x = "f64[]", alpha = "f64")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  loaded <- mojor_load_kernel(path)

  x <- c(1.0, 2.0, 3.0)
  expect_equal(loaded$func(x, 10.0), c(10.0, 20.0, 30.0))
  expect_equal(loaded$func(x, 10.0), built$func(x, 10.0))
})

test_that("round-trip with two array arguments", {  skip_if_no_mojo()

  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  loaded <- mojor_load_kernel(path)

  x <- c(1.0, 2.0, 3.0)
  y <- c(10.0, 20.0, 30.0)
  expect_equal(loaded$func(x, y), c(11.0, 22.0, 33.0))
})

test_that("round-trip with integer types", {  skip_if_no_mojo()

  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1L
    out
  }

  built <- mojor_build(f, x = "i32[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  loaded <- mojor_load_kernel(path)

  result <- loaded$func(c(1L, 2L, 3L))
  expect_equal(result, c(2L, 3L, 4L))
})

test_that("loaded kernel produces same result as original on larger input", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * x[i] + 1.0
    out
  }

  built <- mojor_build(f, x = "f64[]")
  path <- tempfile(fileext = ".mojor")
  on.exit(unlink(path), add = TRUE)

  mojor_save(built, path)
  loaded <- mojor_load_kernel(path)

  x <- seq(1.0, 100.0, by = 1.0)
  expect_equal(loaded$func(x), built$func(x))
  expect_equal(loaded$func(x), x * x + 1.0)
})
