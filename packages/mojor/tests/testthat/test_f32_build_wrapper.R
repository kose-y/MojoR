library(testthat)

if (!mojor_is_loaded()) {
  try(mojor_load(file.path(getwd(), "prototype", "build")), silent = TRUE)
}
if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}
if (!requireNamespace("float", quietly = TRUE)) {
  skip("float package not installed")
}

test_that("mojor_build handles f32[] inputs and length checks", {  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  x <- float::fl(c(1, 2, 3))
  y <- float::fl(c(4, 5, 6))
  build <- mojor_build(f, x = "f32[]", y = "f32[]", name = "t_f32_build_add", cache = FALSE, load = TRUE)
  res <- build$func(x, y)
  expect_equal(float::dbl(res), float::dbl(x) + float::dbl(y))

  y_short <- float::fl(c(1, 2))
  expect_error(build$func(x, y_short), "length mismatch")
})

test_that("mojor_build recycles f32[] with broadcast=recycle", {  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  x <- float::fl(c(1, 2, 3, 4))
  y <- float::fl(c(10, 20))
  build <- mojor_build(f, x = "f32[]", y = "f32[]", name = "t_f32_build_recycle", cache = FALSE, load = TRUE, broadcast = "recycle")
  res <- build$func(x, y)
  expect_equal(float::dbl(res), float::dbl(x) + rep(float::dbl(y), length.out = length(x)))
})

test_that("f32[] wrappers do not emit unused f32 scalar bitcast helper", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_len(length(x))) {
      out[i] <- is.nan(x[i])
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f32[]",
    name = "t_f32_array_wrapper_no_scalar_helper",
    cache = FALSE,
    load = FALSE,
    na_mode = "unsafe"
  )

  expect_wrapper_lacks(
    built,
    c("__mojor_f32_bits", "MOJOR_BITS_TO_F32", "__mojor_out_len = LENGTH(result)")
  )
})

test_that("f32 scalar wrappers emit bitcast helper when required", {  f <- function(x, n) {
    out <- float::float32(n)
    for (i in seq_len(n)) {
      out[i] <- x
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f32",
    n = "i32",
    name = "t_f32_scalar_wrapper_has_helper",
    cache = FALSE,
    load = FALSE,
    na_mode = "unsafe"
  )

  expect_wrapper_has(built, c("__mojor_f32_bits", "MOJOR_BITS_TO_F32"))
})
