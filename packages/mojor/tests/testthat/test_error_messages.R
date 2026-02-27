library(testthat)

# Phase 5.7: Error Message Quality Tests
# Verify that error messages are clear, specific, and actionable

test_that("Loop errors provide specific reasons not generic 'extend IR support'", {  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in sample(n, 1)) {
      out[i] <- x[i]
    }
    out
  }

  err <- tryCatch(
    mojor_transpile(f, x = "f64[]", n = "i32"),
    error = function(e) e
  )

  # Extract message safely
  msg <- if (inherits(err, "condition")) {
    conditionMessage(err)
  } else if (is.character(err)) {
    err
  } else {
    ""
  }

  # Should NOT use generic "top-level loop not supported by IR" message
  expect_false(
    grepl("top-level loop not supported by IR", msg, fixed = TRUE)
  )

  # Should provide a specific reason (even if not seq-specific)
  expect_match(
    msg,
    "Loop not supported:|unsupported|failed",
    ignore.case = TRUE
  )
})

test_that("Loop diagnostics call out sampled iterators explicitly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in sample.int(n, size = n, replace = FALSE)) {
      out[i] <- x[i]
    }
    out
  }

  err <- tryCatch(
    mojor_transpile(f, x = "f64[]", n = "i32"),
    error = function(e) e
  )

  msg <- if (inherits(err, "condition")) {
    conditionMessage(err)
  } else if (is.character(err)) {
    err
  } else {
    ""
  }

  expect_match(msg, "sampled iterator|sample\\(\\.\\.\\.\\)|sample\\.int", ignore.case = TRUE)
})

test_that("Error messages include actionable hints", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in sample(n, 1)) {
      out[i] <- x[i]
    }
    out
  }

  err <- tryCatch(
    mojor_transpile(f, x = "f64[]", n = "i32"),
    error = function(e) e
  )

  # Extract message safely
  msg <- if (inherits(err, "condition")) {
    conditionMessage(err)
  } else if (is.character(err)) {
    err
  } else {
    ""
  }

  # Error message should have a hint
  expect_match(
    msg,
    "hint:|\\(check |\\(use |\\(see |\\(replace ",
    ignore.case = TRUE
  )
})

test_that("Error messages reference documentation", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in sample(n, 1)) {
      out[i] <- x[i]
    }
    out
  }

  err <- tryCatch(
    mojor_transpile(f, x = "f64[]", n = "i32"),
    error = function(e) e
  )

  # Extract message safely
  msg <- if (inherits(err, "condition")) {
    conditionMessage(err)
  } else if (is.character(err)) {
    err
  } else {
    ""
  }

  # Error should reference documentation
  expect_match(
    msg,
    "WORKAROUNDS|TROUBLESHOOTING|SUBSET|\\.md",
    ignore.case = TRUE
  )
})

test_that("Error diagnostics don't use 'extend IR support' for any errors", {  # The new diagnostic system should provide specific messages for all errors
  
  # Test various error patterns
  patterns <- list(
    # Pattern 1: seq() issue
    function(x, n) {
      out <- numeric(n)
      for (i in sample(n, 1)) out[i] <- x[i]
      out
    }
  )

  for (pattern_fn in patterns) {
    err <- tryCatch(
      mojor_transpile(pattern_fn, x = "f64[]", n = "i32"),
      error = function(e) e
    )

    if (inherits(err, "condition")) {
      msg <- conditionMessage(err)
      
      # Should NOT use the generic message
      expect_false(
        grepl("extend IR support", msg, fixed = TRUE),
        info = "Error should not use generic 'extend IR support' message"
      )
    }
  }
})

test_that("Error messages are under 300 characters for main text", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in sample(n, 1)) {
      out[i] <- x[i]
    }
    out
  }

  err <- tryCatch(
    mojor_transpile(f, x = "f64[]", n = "i32"),
    error = function(e) e
  )

  msg <- if (inherits(err, "condition")) {
    conditionMessage(err)
  } else {
    ""
  }

  # Extract main message (before pipes and excessive details)
  main_msg <- sub(" \\| see.*", "", msg)
  main_msg <- trimws(main_msg)

  # Main message should be concise but informative
  expect_true(nchar(main_msg) > 10)
  expect_true(nchar(main_msg) < 300)
})
