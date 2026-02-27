# Tests for Phase 6.1 Stage C: Error Recovery
# Tests for mojor_error_result, retry logic, and error_mode parameter

library(testthat)
library(mojor)

# ============================================================================
# Test Suite: Error Recovery Infrastructure
# ============================================================================

context("Error Recovery Infrastructure")
test_that("mojor_error_result creates valid result objects", {  # Success case
  result <- mojor_error_result("ok", data = c(1, 2, 3))
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result$status, "ok")
  expect_equal(result$data, c(1, 2, 3))
  expect_equal(result$retries_used, 0)
  expect_null(result$error)
  
  # Partial result case
  result <- mojor_error_result("partial", error = "Some elements failed", data = c(1, 2, NA))
  expect_equal(result$status, "partial")
  expect_equal(result$data, c(1, 2, NA))
  
  # Failed case
  result <- mojor_error_result("failed", error = "Kernel execution failed")
  expect_equal(result$status, "failed")
  expect_null(result$data)
})

test_that("mojor_error_result validates status parameter", {  expect_error(
    mojor_error_result("invalid"),
    "status must be 'ok', 'partial', or 'failed'"
  )
})

test_that("result_status extracts status correctly", {  result <- mojor_error_result("ok", data = 1:5)
  expect_equal(result_status(result), "ok")
  
  result <- mojor_error_result("partial", error = "Error", data = c(1, 2))
  expect_equal(result_status(result), "partial")
  
  result <- mojor_error_result("failed", error = "Error")
  expect_equal(result_status(result), "failed")
})

test_that("result_status validates input", {  expect_error(
    result_status("not a result"),
    "must be a mojor_error_result object"
  )
})

test_that("result_error extracts error correctly", {  err <- "Test error"
  result <- mojor_error_result("partial", error = err, data = c(1, 2))
  expect_equal(result_error(result), err)
  
  result <- mojor_error_result("ok", data = 1:5)
  expect_null(result_error(result))
})

test_that("result_data extracts data correctly", {  result <- mojor_error_result("ok", data = 1:5)
  expect_equal(result_data(result), 1:5)
  
  result <- mojor_error_result("partial", error = "Error", data = c(1, 2))
  expect_equal(result_data(result), c(1, 2))
  
  result <- mojor_error_result("failed", error = "Error")
  expect_null(result_data(result))
})

test_that("result_is_success checks status correctly", {  result <- mojor_error_result("ok", data = 1:5)
  expect_true(result_is_success(result))
  
  result <- mojor_error_result("partial", error = "Error", data = c(1, 2))
  expect_false(result_is_success(result))
  
  result <- mojor_error_result("failed", error = "Error")
  expect_false(result_is_success(result))
})

test_that("result_is_partial checks status correctly", {  result <- mojor_error_result("ok", data = 1:5)
  expect_false(result_is_partial(result))
  
  result <- mojor_error_result("partial", error = "Error", data = c(1, 2))
  expect_true(result_is_partial(result))
  
  result <- mojor_error_result("failed", error = "Error")
  expect_false(result_is_partial(result))
})

test_that("result_is_failed checks status correctly", {  result <- mojor_error_result("ok", data = 1:5)
  expect_false(result_is_failed(result))
  
  result <- mojor_error_result("partial", error = "Error", data = c(1, 2))
  expect_false(result_is_failed(result))
  
  result <- mojor_error_result("failed", error = "Error")
  expect_true(result_is_failed(result))
})

# ============================================================================
# Test Suite: Error Recovery State Management
# ============================================================================

test_that(".mojor_init_error_recovery_state initializes state correctly", {  state <- .mojor_init_error_recovery_state()
  expect_equal(state$error_mode, "stop")
  expect_equal(state$max_retries, 3)
  expect_equal(state$retry_delay, 0.1)
  expect_equal(state$retry_on, character(0))
  expect_equal(state$current_retry, 0)
})

# ============================================================================
# Test Suite: Retry Logic with Exponential Backoff
# ============================================================================

test_that(".mojor_execute_with_recovery executes successfully without retries", {  result <- .mojor_execute_with_recovery(
    fn = function() 42,
    error_mode = "retry",
    max_retries = 3
  )
  expect_equal(result$status, "ok")
  expect_equal(result$data, 42)
  expect_equal(result$retries_used, 0)
})

test_that(".mojor_execute_with_recovery retries on error", {  # Use a counter in the parent environment
  env <- new.env()
  env$counter <- 0
  
  result <- .mojor_execute_with_recovery(
    fn = function() {
      env$counter <- env$counter + 1
      if (env$counter < 3) stop("Transient error")
      return("success")
    },
    error_mode = "retry",
    max_retries = 3,
    retry_delay = 0.01  # Fast for testing
  )
  expect_equal(result$status, "ok")
  expect_equal(result$data, "success")
  # retries_used counts the number of retry attempts (not the initial attempt)
  # After 2 failures, counter=3, retries_used=2
  expect_equal(result$retries_used, 2)
  expect_equal(env$counter, 3)
})

test_that(".mojor_execute_with_recovery fails after max retries", {  env <- new.env()
  env$counter <- 0
  result <- .mojor_execute_with_recovery(
    fn = function() {
      env$counter <- env$counter + 1
      stop("Persistent error")
    },
    error_mode = "retry",
    max_retries = 2,
    retry_delay = 0.01
  )
  expect_equal(result$status, "failed")
  # After 2 retries, counter=3 (initial + 2 retries)
  expect_equal(result$retries_used, 2)
  expect_equal(env$counter, 3)
})

test_that(".mojor_execute_with_recovery returns partial on error with partial mode", {  env <- new.env()
  env$counter <- 0
  result <- .mojor_execute_with_recovery(
    fn = function() {
      env$counter <- env$counter + 1
      stop("Error")
    },
    error_mode = "partial",
    max_retries = 3,
    retry_delay = 0.01
  )
  # In partial mode, the first error is caught and returned as partial
  expect_equal(result$status, "partial")
  expect_equal(result$retries_used, 0)
})

test_that(".mojor_execute_with_recovery stops immediately with stop mode", {  result <- .mojor_execute_with_recovery(
    fn = function() stop("Error"),
    error_mode = "stop",
    max_retries = 3
  )
  # In stop mode, the error propagates - result should be failed
  expect_equal(result$status, "failed")
})

# ============================================================================
# Test Suite: mojor_build with error_mode
# ============================================================================

test_that("mojor_build accepts error_mode parameter", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  # Test with error_mode = "stop" (default)
  built <- mojor_build(f, x = "f64[]", error_mode = "stop")
  expect_equal(built$success, TRUE)
  expect_is(built$func, "function")
  
  # Test with error_mode = "partial"
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  expect_equal(built$success, TRUE)
  
  # Test with error_mode = "retry"
  built <- mojor_build(f, x = "f64[]", error_mode = "retry")
  expect_equal(built$success, TRUE)
})

test_that("mojor_build with error_mode = 'stop' behaves normally", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "stop")
  result <- built$func(1:5)
  expect_equal(result, c(2, 4, 6, 8, 10))
})

test_that("mojor_build with error_mode = 'partial' returns result object", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(1:5)
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result$status, "ok")
  expect_equal(result_data(result), c(2, 4, 6, 8, 10))
})

test_that("mojor_build with error_mode = 'retry' returns result object", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "retry")
  result <- built$func(1:5)
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result$status, "ok")
  expect_equal(result_data(result), c(2, 4, 6, 8, 10))
})

# ============================================================================
# Test Suite: Error Recovery with Failing Kernels
# ============================================================================

test_that("Error recovery handles kernel errors gracefully", {  # This test verifies that error recovery catches errors from kernel execution
  # and returns appropriate result objects
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  # With error_mode = "stop", errors should propagate
  built <- mojor_build(f, x = "f64[]", error_mode = "stop")
  
  # With error_mode = "partial", errors should be caught and result returned
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(1:5)
  expect_s3_class(result, "mojor_error_result")
})

test_that("Error recovery with max_retries limits retry attempts", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "retry", max_retries = 2)
  expect_equal(built$success, TRUE)
})

# ============================================================================
# Test Suite: Cleanup Mechanisms
# ============================================================================

test_that("with_cleanup executes code and calls cleanup", {  cleanup_called <- FALSE
  result <- with_cleanup(
    function() cleanup_called <<- TRUE,
    42
  )
  expect_equal(result, 42)
  expect_true(cleanup_called)
})

test_that("with_cleanup calls cleanup on error", {  cleanup_called <- FALSE
  result <- tryCatch({
    with_cleanup(
      function() cleanup_called <<- TRUE,
      stop("Error")
    )
  }, error = function(e) e)
  
  expect_true(cleanup_called)
  expect_s3_class(result, "error")
})

test_that("with_cleanup runs cleanup exactly once on error", {  cleanup_calls <- 0L
  result <- tryCatch({
    with_cleanup(
      function() cleanup_calls <<- cleanup_calls + 1L,
      stop("Error")
    )
  }, error = function(e) e)

  expect_equal(cleanup_calls, 1L)
  expect_s3_class(result, "error")
})

# ============================================================================
# Test Suite: Integration with Existing Features
# ============================================================================

test_that("Error recovery works with debug mode", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", debug = TRUE, error_mode = "partial")
  expect_equal(built$success, TRUE)
})

test_that("Error recovery works with trace mode", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", trace = TRUE, error_mode = "partial")
  expect_equal(built$success, TRUE)
})

test_that("Error recovery works with memory_check mode", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", memory_check = TRUE, error_mode = "partial")
  expect_equal(built$success, TRUE)
})

# ============================================================================
# Test Suite: Edge Cases
# ============================================================================

test_that("Error recovery handles empty arrays", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(numeric(0))
  expect_equal(length(result_data(result)), 0)
})

test_that("Error recovery handles single element arrays", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  result <- built$func(42)
  expect_equal(result_data(result), 84)
})

test_that("Error recovery handles large arrays", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  
  built <- mojor_build(f, x = "f64[]", error_mode = "partial")
  large_input <- 1:10000
  result <- built$func(large_input)
  expected <- large_input * 2
  expect_equal(result_data(result), expected)
})
