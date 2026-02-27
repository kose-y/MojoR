# Runtime tests for Phase 6.1 Stage C: Error Recovery
# These tests require Mojo compiler + backend to be available.

test_that("error_mode='stop' returns raw result (default behavior)", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2.0
    out
  }

  built <- mojor_build(f, x = "f64[]", error_mode = "stop")
  result <- built$func(c(1.0, 2.0, 3.0))
  # In stop mode, result is raw (not wrapped in mojor_error_result)
  expect_equal(result, c(2.0, 4.0, 6.0))
  expect_false(inherits(result, "mojor_error_result"))
})

test_that("error_mode='partial' wraps successful result", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2.0
    out
  }

  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(c(1.0, 2.0, 3.0))
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result_status(result), "ok")
  expect_equal(result_data(result), c(2.0, 4.0, 6.0))
  expect_equal(result$retries_used, 0)
})

test_that("error_mode='retry' wraps successful result", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1.0
    out
  }

  built <- mojor_build(f, x = "f64[]", error_mode = "retry")
  result <- built$func(c(10.0, 20.0, 30.0))
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result_status(result), "ok")
  expect_equal(result_data(result), c(11.0, 21.0, 31.0))
})

test_that("error_mode='partial' catches NA error gracefully", {  skip_if_no_mojo()

  f <- function(x, alpha) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * alpha
    out
  }

  built <- mojor_build(f, x = "f64[]", alpha = "f64", error_mode = "partial")
  # NA_real_ triggers the na_mode='forbid' guard, caught by error recovery
  result <- built$func(c(1.0, 2.0), NA_real_)
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result_status(result), "partial")
  expect_false(is.null(result_error(result)))
})

test_that("error_mode='stop' propagates NA error", {  skip_if_no_mojo()

  f <- function(x, alpha) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * alpha
    out
  }

  built <- mojor_build(f, x = "f64[]", alpha = "f64", error_mode = "stop")
  expect_error(built$func(c(1.0, 2.0), NA_real_), "NA")
})

test_that("error_mode='partial' with memory_check catches NULL", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2.0
    out
  }

  built <- mojor_build(f, x = "f64[]", memory_check = TRUE, error_mode = "partial")
  result <- built$func(NULL)
  expect_s3_class(result, "mojor_error_result")
  expect_true(result_status(result) %in% c("partial", "failed"))
  expect_false(is.null(result_error(result)))
})

test_that("error_mode='partial' produces same data as stop mode", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * x[i]
    out
  }

  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  built_stop <- mojor_build(f, x = "f64[]", error_mode = "stop")
  built_partial <- mojor_build(f, x = "f64[]", error_mode = "partial")

  result_stop <- built_stop$func(x)
  result_partial <- built_partial$func(x)

  expect_equal(result_data(result_partial), result_stop)
})

test_that("result accessor functions work on runtime results", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1.0
    out
  }

  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(c(5.0, 10.0))

  expect_true(result_is_success(result))
  expect_false(result_is_partial(result))
  expect_false(result_is_failed(result))
  expect_null(result_error(result))
  expect_equal(result_data(result), c(6.0, 11.0))
})
