# Error Recovery Infrastructure (Stage C)
# Provides graceful error handling with retry mechanisms and partial results

# ============================================================================
# Result Wrapper Class
# ============================================================================

#' Create an error recovery result object
#'
#' @param status Character: "ok", "partial", or "failed"
#' @param error Optional error object or message
#' @param data The computed data (full or partial)
#' @param retries_used Number of retries used (0 if no retry)
#'
#' @return A list with class "mojor_error_result"
#' @noRd
#'
#' @examples
#' # Success case
#' result <- mojor_error_result("ok", data = c(1, 2, 3))
#'
#' # Partial result case
#' result <- mojor_error_result("partial", error = "Some elements failed", data = c(1, 2, NA))
#'
#' # Failed case
#' result <- mojor_error_result("failed", error = "Kernel execution failed")
mojor_error_result <- function(status = "ok", error = NULL, data = NULL, retries_used = 0) {
  if (!status %in% c("ok", "partial", "failed")) {
    stop("mojor_error_result: status must be 'ok', 'partial', or 'failed'")
  }

  structure(
    list(
      status = status,
      error = error,
      data = data,
      retries_used = retries_used
    ),
    class = "mojor_error_result"
  )
}

#' Print method for mojor_error_result
#'
#' @param x The result object
#' @param ... Additional arguments (unused)
#' @noRd
print.mojor_error_result <- function(x, ...) {
  cat("<mojor_error_result>\n")
  cat("  status: ", x$status, "\n", sep = "")
  cat("  retries_used: ", x$retries_used, "\n", sep = "")

  if (!is.null(x$error)) {
    cat("  error: ", conditionMessage(x$error), "\n", sep = "")
  }

  if (!is.null(x$data)) {
    cat("  data: ", deparse(substitute(x$data)), "\n", sep = "")
  }
}

#' Extract status from mojor_error_result
#'
#' @param x The result object
#' @noRd
#' @examples
#' result <- mojor_error_result("ok", data = 1:5)
#' result_status(result)
result_status <- function(x) {
  if (!inherits(x, "mojor_error_result")) {
    stop("result_status: x must be a mojor_error_result object")
  }
  x$status
}

#' Extract error from mojor_error_result
#'
#' @param x The result object
#' @noRd
#' @examples
#' result <- mojor_error_result("partial", error = "Some elements failed")
#' result_error(result)
result_error <- function(x) {
  if (!inherits(x, "mojor_error_result")) {
    stop("result_error: x must be a mojor_error_result object")
  }
  x$error
}

#' Extract data from mojor_error_result
#'
#' @param x The result object
#' @noRd
#' @examples
#' result <- mojor_error_result("ok", data = 1:5)
#' result_data(result)
result_data <- function(x) {
  if (!inherits(x, "mojor_error_result")) {
    stop("result_data: x must be a mojor_error_result object")
  }
  x$data
}

# ============================================================================
# Error Recovery State Management
# ============================================================================

# Error recovery options stored in .mojor_state
# - error_mode: "stop" (default), "partial", "retry"
# - max_retries: Maximum number of retries (default: 3)
# - retry_delay: Base delay for exponential backoff (default: 0.1 seconds)
# - retry_on: Character vector of error conditions to retry on

.mojor_init_error_recovery_state <- function() {
  list(
    error_mode = "stop",
    max_retries = 3,
    retry_delay = 0.1,
    retry_on = character(0),
    current_retry = 0
  )
}

# ============================================================================
# Retry Logic with Exponential Backoff
# ============================================================================

#' Execute a function with error recovery
#'
#' @param fn Function to execute
#' @param error_mode One of "stop", "partial", "retry"
#' @param max_retries Maximum number of retries
#' @param retry_delay Base delay for exponential backoff
#' @param retry_on Character vector of error conditions to retry on
#' @param ... Arguments to pass to fn
#'
#' @return A mojor_error_result object
.mojor_execute_with_recovery <- function(fn, error_mode = "stop", max_retries = 3,
                                         retry_delay = 0.1, retry_on = character(0), ...) {
  retries_used <- 0
  last_error <- NULL

  while (TRUE) {
    result <- tryCatch(
      {
        list(success = TRUE, data = fn(...))
      },
      error = function(e) {
        last_error <<- e

 # Check if we should retry
        should_retry <- error_mode == "retry"

 # Check if error matches retry_on patterns
        if (length(retry_on) > 0) {
          err_msg <- conditionMessage(e)
          should_retry <- should_retry && any(grepl(retry_on, err_msg, ignore.case = TRUE))
        }

 # Check if we have retries left
        should_retry <- should_retry && (retries_used < max_retries)

        if (!should_retry) {
 # Return partial/failed result based on error_mode
          status <- if (error_mode == "partial") "partial" else "failed"
          return(mojor_error_result(status, error = e, retries_used = retries_used))
        }

 # Retry with exponential backoff
        retries_used <<- retries_used + 1
        delay <- retry_delay * (2^(retries_used - 1))
        Sys.sleep(delay)

 # Return a special marker to indicate retry
        return(list(success = FALSE))
      }
    )

 # Check if result is already a mojor_error_result (error handler returned early)
    if (inherits(result, "mojor_error_result")) {
      return(result)
    }

 # Check if we got a success result
    if (!is.null(result) && is.list(result) && result$success) {
      return(mojor_error_result("ok", data = result$data, retries_used = retries_used))
    }

 # If result is NULL or success=FALSE, we should retry or continue
 # The error handler already handled the retry logic
  }

 # Should not reach here, but just in case
  mojor_error_result("failed", error = last_error, retries_used = retries_used)
}

#' Execute a function with error recovery using state
#'
#' @param fn Function to execute
#' @param state The error recovery state list
#' @param ... Arguments to pass to fn
#'
#' @return A mojor_error_result object
.mojor_execute_with_state <- function(fn, state, ...) {
  .mojor_execute_with_recovery(
    fn = fn,
    error_mode = state$error_mode,
    max_retries = state$max_retries,
    retry_delay = state$retry_delay,
    retry_on = state$retry_on,
    ...
  )
}

# ============================================================================
# Cleanup Mechanisms
# ============================================================================

#' RAII-style cleanup for resource management
#'
#' @param cleanup_fn Function to call for cleanup
#' @param code Code to execute with cleanup
#'
#' @return Result of code execution
with_cleanup <- function(cleanup_fn, code) {
  on.exit(
    {
      tryCatch(cleanup_fn(), error = function(e) {
        warning("Cleanup failed: ", conditionMessage(e))
      })
    },
    add = TRUE
  )
  code
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Check if a result indicates success
#'
#' @param result A mojor_error_result object
#' @noRd
#' @examples
#' result <- mojor_error_result("ok", data = 1:5)
#' result_is_success(result)
result_is_success <- function(result) {
  if (!inherits(result, "mojor_error_result")) {
    stop("result_is_success: result must be a mojor_error_result object")
  }
  result$status == "ok"
}

#' Check if a result indicates partial success
#'
#' @param result A mojor_error_result object
#' @noRd
#' @examples
#' result <- mojor_error_result("partial", error = "Some elements failed", data = c(1, 2, NA))
#' result_is_partial(result)
result_is_partial <- function(result) {
  if (!inherits(result, "mojor_error_result")) {
    stop("result_is_partial: result must be a mojor_error_result object")
  }
  result$status == "partial"
}

#' Check if a result indicates failure
#'
#' @param result A mojor_error_result object
#' @noRd
#' @examples
#' result <- mojor_error_result("failed", error = "Kernel execution failed")
#' result_is_failed(result)
result_is_failed <- function(result) {
  if (!inherits(result, "mojor_error_result")) {
    stop("result_is_failed: result must be a mojor_error_result object")
  }
  result$status == "failed"
}
