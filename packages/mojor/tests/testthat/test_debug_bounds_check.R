source("helper-mojo.R")

test_that("debug mode emits bounds checks in generated Mojo", {  # Simple kernel that accesses arrays
  f <- function(x, y) {
    n <- length(x)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + y[i]
    }
    out
  }

  # Transpile with debug mode
  trans_debug <- mojor_transpile(f, x = "f64[]", y = "f64[]", debug = TRUE)

  # Check that debug helpers are imported
  expect_true(any(grepl("from debug_helpers import", trans_debug$mojo, fixed = TRUE)),
              info = "Debug helpers should be imported")

  # Check that _mojor_read_f64 is called with debug parameters (6 args instead of 3)
  # In debug mode: _mojor_read_f64(ptr, idx, len, var_name, file, line)
  # In production: _mojor_read_f64(ptr, idx, len)
  # Look for actual call sites (lines containing the function call in kernel code)
  call_pattern <- '_mojor_read_f64\\([^,]+,\\s*[^,]+,\\s*[^,]+,\\s*"[^"]+",\\s*"[^"]+",\\s*\\d+\\)'
  has_debug_params <- any(grepl(call_pattern, trans_debug$mojo))
  expect_true(has_debug_params,
              info = "Debug read helpers should include var_name, file, and line parameters")

  # Verify the helper function signature includes raises
  helper_def <- grep("fn _mojor_read_f64\\(", trans_debug$mojo, value = TRUE)
  if (length(helper_def) > 0) {
    expect_true(any(grepl("raises", helper_def)),
                info = "Debug helper should have 'raises' in signature")
  }
})

test_that("production mode has no debug overhead", {  f <- function(x, y) {
    n <- length(x)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + y[i]
    }
    out
  }

  # Transpile without debug mode
  trans_prod <- mojor_transpile(f, x = "f64[]", y = "f64[]", debug = FALSE)

  # Check that debug helpers are NOT imported
  expect_false(any(grepl("from debug_helpers import", trans_prod$mojo, fixed = TRUE)),
               info = "Debug helpers should NOT be imported in production mode")

  # Check that _mojor_read_f64 is called with only 3 args (production version)
  read_calls <- grep("_mojor_read_f64\\(", trans_prod$mojo, value = TRUE)

  # Production calls should NOT have the extra string literal parameters
  has_debug_params <- any(grepl('_mojor_read_f64\\([^)]+,\\s*"[^"]+",\\s*"[^"]+",\\s*\\d+\\)', read_calls))
  expect_false(has_debug_params,
               info = "Production read helpers should NOT include debug parameters")

  # Verify the helper function signature does NOT include raises
  helper_def <- grep("fn _mojor_read_f64\\(", trans_prod$mojo, value = TRUE)
  if (length(helper_def) > 0) {
    expect_false(any(grepl("raises", helper_def)),
                 info = "Production helper should NOT have 'raises' in signature")
  }
})

test_that("debug mode helper uses mojor_check_bounds", {  f <- function(x) {
    n <- length(x)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)

  # Find the helper function definition
  helper_start <- which(grepl("fn _mojor_read_f64\\(", trans_debug$mojo))

  if (length(helper_start) > 0) {
    # Get a few lines after the function definition
    helper_lines <- trans_debug$mojo[helper_start:(helper_start + 5)]
    helper_text <- paste(helper_lines, collapse = "\n")

    # Should call mojor_check_bounds
    expect_true(grepl("mojor_check_bounds", helper_text, fixed = TRUE),
                info = "Debug helper should call mojor_check_bounds")
  }
})
