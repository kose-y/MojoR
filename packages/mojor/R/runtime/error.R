# =============================================================================
# MojoR Error Handling
# =============================================================================
# Centralized error handling for MojoR
#
# Organization:
# 1. Error Classes
# 2. Error Reporting Functions
# 3. Assertion Functions
# 4. Error Context Helpers
# =============================================================================

# =============================================================================
# Section 1: Error Classes
# =============================================================================

setOldClass(c("mojor_error", "error", "condition"))

# =============================================================================
# Section 2: Error Reporting Functions
# =============================================================================

#' Internal error with context
#'
#' @param code Error code (integer)
#' @param message Error message (string)
#' @param context Additional context (list)
#' @param hint Optional hint for users
#' @param file Optional source file
#' @param line Optional source line
#' @param col Optional source column
#'
#' @noRd
.mojor_error <- function(code, message, context = NULL, hint = NULL, file = NULL, line = NULL, col = NULL) {
  msg <- message
  if (!is.null(context) && length(context) > 0) {
    ctx_str <- paste(vapply(context, function(x) {
      if (is.character(x) && length(x) == 1) {
        return(x)
      }
      paste(deparse(x), collapse = " ")
    }, character(1)), collapse = "; ")
    msg <- paste0(msg, " (", ctx_str, ")")
  }
  if (!is.null(hint)) {
    msg <- paste0(msg, " (hint: ", hint, ")")
  }

  if (!is.null(file) && !is.null(line)) {
    loc <- paste0(file, ":", line)
    if (!is.null(col)) {
      loc <- paste0(loc, ":", col)
    }
    msg <- paste0(msg, " [", loc, "]")
  }

  cond <- structure(
    list(
      message = msg,
      code = code,
      context = context,
      hint = hint,
      file = file,
      line = line,
      col = col
    ),
    class = c("mojor_error", "error", "condition")
  )
  stop(cond)
}

#' User-facing error with context
#'
#' @param message Error message (string)
#' @param ... Additional context as named arguments
#' @param hint Optional hint for users
#' @param file Optional source file
#' @param line Optional source line
#' @param col Optional source column
#'
#' @noRd
.mojor_stop <- function(message, ..., hint = NULL, file = NULL, line = NULL, col = NULL) {
  context <- list(...)
  .mojor_error(MOJOR_ERR_INVALID_INPUT, message, context, hint, file, line, col)
}

#' Warning with context
#'
#' @param message Warning message (string)
#' @param ... Additional context as named arguments
#' @param hint Optional hint for users
#' @param file Optional source file
#' @param line Optional source line
#' @param col Optional source column
#'
#' @noRd
.mojor_msg_warn <- function(message, ..., hint = NULL, file = NULL, line = NULL, col = NULL) {
  msg <- message
  if (length(list(...)) > 0) {
    ctx_str <- paste(vapply(list(...), function(x) {
      if (is.character(x) && length(x) == 1) {
        return(x)
      }
      paste(deparse(x), collapse = " ")
    }, character(1)), collapse = "; ")
    msg <- paste0(msg, " (", ctx_str, ")")
  }
  if (!is.null(hint)) {
    msg <- paste0(msg, " (hint: ", hint, ")")
  }

  if (!is.null(file) && !is.null(line)) {
    loc <- paste0(file, ":", line)
    if (!is.null(col)) {
      loc <- paste0(loc, ":", col)
    }
    msg <- paste0(msg, " [", loc, "]")
  }

  warning(msg, call. = FALSE)
}

#' Info message with context
#'
#' @param message Info message (string)
#' @param ... Additional context as named arguments
#' @param hint Optional hint for users
#'
#' @noRd
.mojor_msg_info <- function(message, ..., hint = NULL) {
  msg <- message
  if (length(list(...)) > 0) {
    ctx_str <- paste(vapply(list(...), function(x) {
      if (is.character(x) && length(x) == 1) {
        return(x)
      }
      paste(deparse(x), collapse = " ")
    }, character(1)), collapse = "; ")
    msg <- paste0(msg, " (", ctx_str, ")")
  }
  if (!is.null(hint)) {
    msg <- paste0(msg, " (hint: ", hint, ")")
  }
  message(msg)
}

# =============================================================================
# Section 3: Assertion Functions
# =============================================================================

#' Assert condition, throw error if false
#'
#' @param condition Condition to check
#' @param message Error message if assertion fails
#' @param ... Additional context as named arguments
#'
#' @noRd
.mojor_assert <- function(condition, message, ...) {
  if (!isTRUE(condition)) {
    .mojor_stop(message, ...)
  }
}

#' Assert condition, throw error if false (with hint)
#'
#' @param condition Condition to check
#' @param message Error message if assertion fails
#' @param hint Hint for fixing the issue
#' @param ... Additional context as named arguments
#'
#' @noRd
.mojor_assert_hint <- function(condition, message, hint, ...) {
  if (!isTRUE(condition)) {
    .mojor_stop(message, hint = hint, ...)
  }
}

#' Assert type, throw error if wrong
#'
#' @param x Object to check
#' @param type Expected type
#' @param name Object name for error message
#'
#' @noRd
.mojor_assert_type <- function(x, type, name = NULL) {
  if (is.null(name)) name <- deparse(substitute(x))
  if (!is(x, type)) {
    .mojor_stop(
      paste0("Expected type '", type, "' for '", name, "', got '", class(x)[1], "'"),
      actual_type = class(x)[1],
      expected_type = type
    )
  }
}

#' Assert non-null, throw error if null
#'
#' @param x Object to check
#' @param name Object name for error message
#'
#' @noRd
.mojor_assert_not_null <- function(x, name = NULL) {
  if (is.null(x)) {
    if (is.null(name)) name <- "object"
    .mojor_stop(paste0("'", name, "' must not be NULL"))
  }
}

#' Assert in range, throw error if out of range
#'
#' @param x Value to check
#' @param min Minimum value (inclusive)
#' @param max Maximum value (inclusive)
#' @param name Value name for error message
#'
#' @noRd
.mojor_assert_range <- function(x, min, max, name = NULL) {
  if (is.null(name)) name <- deparse(substitute(x))
  if (x < min || x > max) {
    .mojor_stop(
      paste0("'", name, "' must be between ", min, " and ", max, ", got ", x),
      value = x,
      min = min,
      max = max
    )
  }
}

#' Assert in set, throw error if not in set
#'
#' @param x Value to check
#' @param set Allowed values
#' @param name Value name for error message
#'
#' @noRd
.mojor_assert_in_set <- function(x, set, name = NULL) {
  if (is.null(name)) name <- deparse(substitute(x))
  if (!x %in% set) {
    .mojor_stop(
      paste0("'", name, "' must be one of: ", paste(set, collapse = ", "), ", got '", x, "'"),
      value = x,
      allowed = set
    )
  }
}

# =============================================================================
# Section 4: Error Context Helpers
# =============================================================================

#' Get error context from state
#'
#' @noRd
.mojor_get_error_context <- function() {
  ctx <- list()

  if (!is.null(.mojor_state$current_srcref)) {
    sr <- .mojor_state$current_srcref
    if (inherits(sr, "srcref") && length(sr) >= 4) {
      ctx$file <- attr(sr, "srcfile")$filename
      ctx$line <- sr[[1]]
      ctx$col <- sr[[2]]
    }
  }

  if (!is.null(.mojor_state$current_function_name)) {
    ctx$func <- .mojor_state$current_function_name
  }

  ctx
}

#' Format error context for display
#'
#' @param ctx Error context (list)
#'
#' @noRd
.mojor_format_error_context <- function(ctx) {
  if (length(ctx) == 0) {
    return(NULL)
  }

  parts <- character(0)
  if (!is.null(ctx$file)) {
    parts <- c(parts, paste0("file: ", ctx$file))
  }
  if (!is.null(ctx$line)) {
    parts <- c(parts, paste0("line: ", ctx$line))
  }
  if (!is.null(ctx$col)) {
    parts <- c(parts, paste0("col: ", ctx$col))
  }
  if (!is.null(ctx$func)) {
    parts <- c(parts, paste0("function: ", ctx$func))
  }

  if (length(parts) == 0) {
    return(NULL)
  }
  paste(parts, collapse = ", ")
}
