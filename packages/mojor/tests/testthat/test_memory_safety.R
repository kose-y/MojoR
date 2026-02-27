# Tests for Phase 6.1 Stage B: Memory Safety Audit

test_that("memory_check=TRUE emits mojor_check_ptr calls in Mojo", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", memory_check = TRUE)
  expect_true(trans$memory_check)
  # Should import memory safety helpers
  expect_match(trans$mojo, "mojor_check_ptr_f64", fixed = TRUE)
  expect_match(trans$mojo, "mojor_check_ptr_f64_mut", fixed = TRUE)
  # Should have raises in signature (implied by debug=TRUE)
  expect_match(trans$mojo, "raises", fixed = TRUE)
})

test_that("memory_check=FALSE does not emit check_ptr calls", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", memory_check = FALSE)
  expect_false(isTRUE(trans$memory_check))
  expect_false(grepl("mojor_check_ptr", trans$mojo, fixed = TRUE))
})

test_that("memory_check implies debug mode", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  # memory_check=TRUE without explicit debug=TRUE should still get debug helpers
  trans <- mojor_transpile(f, x = "f64[]", memory_check = TRUE)
  expect_match(trans$mojo, "from debug_helpers import", fixed = TRUE)
  expect_match(trans$mojo, "raises", fixed = TRUE)
})

test_that("memory_check with i32[] emits check_ptr_i32", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1L
    out
  }
  trans <- mojor_transpile(f, x = "i32[]", memory_check = TRUE)
  expect_match(trans$mojo, "mojor_check_ptr_i32", fixed = TRUE)
})

test_that("memory_check with multiple args emits checks for each", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]",
                           memory_check = TRUE)
  mojo <- trans$mojo
  # Both x and y should have check_ptr calls
  x_checks <- gregexpr("mojor_check_ptr_f64\\(x,", mojo)[[1]]
  y_checks <- gregexpr("mojor_check_ptr_f64\\(y,", mojo)[[1]]
  expect_true(length(x_checks) >= 1 && x_checks[1] > 0)
  expect_true(length(y_checks) >= 1 && y_checks[1] > 0)
})

test_that("mojor_memory_stats returns correct estimates", {  # Simulate a built kernel object with minimal fields
  built <- list(
    types = list(x = "f64[]", y = "f64[]"),
    out_type = "f64",
    out_kind = "vector"
  )
  stats <- mojor_memory_stats(built, n = 1000L)
  expect_equal(stats$n, 1000L)
  # Two f64[] inputs: 2 * 1000 * 8 = 16000
  expect_equal(stats$input_bytes, 16000L)
  # f64 vector output: 1000 * 8 = 8000
  expect_equal(stats$output_bytes, 8000L)
  expect_equal(stats$total_bytes, 24000L)
  expect_true(!is.null(stats$details$inputs$x))
  expect_true(!is.null(stats$details$inputs$y))
})

test_that("mojor_memory_stats handles scalar args", {  built <- list(
    types = list(x = "f64[]", alpha = "f64"),
    out_type = "f64",
    out_kind = "vector"
  )
  stats <- mojor_memory_stats(built, n = 500L)
  # x: 500*8=4000, alpha: 8 (scalar)
  expect_equal(stats$input_bytes, 4008L)
  # output: 500*8=4000
  expect_equal(stats$output_bytes, 4000L)
})

test_that("mojor_memory_stats accepts built$trans layout", {  built <- list(
    trans = list(
      types = list(x = "f64[]", alpha = "f64"),
      out_type = "f64",
      out_kind = "vector"
    )
  )
  stats <- mojor_memory_stats(built, n = 500L)
  expect_equal(stats$input_bytes, 4008L)
  expect_equal(stats$output_bytes, 4000L)
})

test_that("mojor_memory_stats handles scalar reduction output", {  built <- list(
    types = list(x = "f64[]"),
    out_type = "f64",
    out_kind = "scalar"
  )
  stats <- mojor_memory_stats(built, n = 1000L)
  expect_equal(stats$input_bytes, 8000L)
  expect_equal(stats$output_bytes, 8L)  # single scalar
})

test_that("mojor_memory_stats handles i32 and f32 types", {  built <- list(
    types = list(x = "i32[]", y = "f32[]"),
    out_type = "f64",
    out_kind = "vector"
  )
  stats <- mojor_memory_stats(built, n = 100L)
  # x: 100*4=400, y: 100*4=400
  expect_equal(stats$input_bytes, 800L)
  # output f64: 100*8=800
  expect_equal(stats$output_bytes, 800L)
})

test_that("mojor_memory_stats errors on invalid input", {  expect_error(mojor_memory_stats(NULL), "expected a built kernel")
  expect_error(mojor_memory_stats(list()), "expected a built kernel")
})
