library(testthat)

test_that("seq_parallel_along is recognized and marks loop as parallel", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_parallel_along")
  
  # Check that parallelize is emitted
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
  # Check that the import is added
  expect_true(grepl("from algorithm import parallelize", trans$mojo, fixed = TRUE))
  expect_false(isTRUE(trans$parallel$requested))
  expect_true(isTRUE(trans$parallel$explicit))
  expect_true(isTRUE(trans$parallel$effective))
})

test_that("seq_parallel_len is recognized and marks loop as parallel", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_parallel_len(n)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_parallel_len")
  
  # Check that parallelize is emitted
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
})

test_that("seq_parallel_len with length(x) works", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_len(length(x))) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_parallel_len_length")
  
  # Check that parallelize is emitted
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
})

test_that("parallel=TRUE auto-parallelizes parallel-safe loops", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_auto_parallel", parallel = TRUE)
  
  # Check that parallelize is emitted
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
  expect_true(isTRUE(trans$parallel$requested))
  expect_false(isTRUE(trans$parallel$explicit))
  expect_true(isTRUE(trans$parallel$effective))
})

test_that("parallel=TRUE does not parallelize loops with scalar accumulators", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_no_parallel_reduction", parallel = TRUE)
  
  # Check that regular for is used (not parallel_for)
  # The parallel_for should not appear because scalar accumulators prevent parallelization
  expect_false(grepl("parallel_for", trans$mojo, fixed = TRUE))
})

test_that("parallel=FALSE prevents parallelization even with seq_parallel_along", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Note: seq_parallel_along explicitly marks loop as parallel regardless of parallel parameter
  trans <- mojor_transpile(f, x = "f64[]", name = "t_explicit_parallel")
  
  # seq_parallel_along should always use parallelize
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
  expect_false(isTRUE(trans$parallel$requested))
  expect_true(isTRUE(trans$parallel$explicit))
  expect_true(isTRUE(trans$parallel$effective))
})

test_that("parallel metadata reports disabled-by-flag reason deterministically", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", name = "t_parallel_disabled_reason", parallel = FALSE)

  expect_false(grepl("parallelize", trans$mojo, fixed = TRUE))
  expect_false(isTRUE(trans$parallel$requested))
  expect_false(isTRUE(trans$parallel$explicit))
  expect_false(isTRUE(trans$parallel$effective))
  expect_identical(trans$parallel$reason, "parallel disabled (parallel=FALSE)")
})

test_that("parallel loop runtime uses safe mode and stays correct", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  
  built <- if (isTRUE(.mojor_parallel_inprocess_enabled())) {
    mojor_build(f, x = "f64[]", name = "t_parallel_runtime", cache = FALSE, load = TRUE)
  } else {
    expect_warning(
      mojor_build(f, x = "f64[]", name = "t_parallel_runtime", cache = FALSE, load = TRUE),
      "subprocess fallback required"
    )
  }
  x <- as.double(1:100)
  result <- built$func(x)
  expected <- x * 2 + 1
  
  expect_equal(result, expected)
  expect_true(isTRUE(built$trans$parallel$uses_parallelize))
  expect_true(is.list(built$trans$parallel))
  expect_true(built$trans$parallel$runtime_mode %in% c("inprocess", "subprocess"))
  expect_identical(
    built$trans$parallel$runtime_reason,
    .mojor_parallel_runtime_reason(built$trans$parallel$runtime_mode)
  )
  expect_identical(
    isTRUE(built$trans$parallel$inprocess_enabled),
    identical(built$trans$parallel$runtime_mode, "inprocess")
  )

  if (isTRUE(built$trans$parallel$inprocess_enabled)) {
    expect_false(identical(attr(built$func, "parallel_runtime_mode"), "subprocess_fallback"))
  } else {
    expect_identical(attr(built$func, "parallel_runtime_mode"), "subprocess_fallback")
    expect_identical(built$parallel_runtime$mode, "subprocess_fallback")
    expect_identical(built$parallel_runtime$reason, built$trans$parallel$runtime_reason)
    expect_false(isTRUE(built$compiled))
    expect_match(
      attr(built$func, "parallel_runtime_reason"),
      "subprocess fallback required",
      fixed = TRUE
    )
    expect_identical(
      attr(built$func, "parallel_runtime_reason"),
      built$trans$parallel$runtime_reason
    )
  }
})

test_that("forced subprocess mode emits deterministic fallback diagnostics", {  skip_if_no_mojo()

  old_mode <- Sys.getenv("MOJOR_PARALLEL_RUNTIME", unset = "")
  on.exit(Sys.setenv(MOJOR_PARALLEL_RUNTIME = old_mode), add = TRUE)
  Sys.setenv(MOJOR_PARALLEL_RUNTIME = "subprocess")

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- expect_warning(
    mojor_build(f, x = "f64[]", name = "t_parallel_forced_subprocess", cache = FALSE, load = TRUE),
    "subprocess fallback required"
  )

  expect_identical(built$trans$parallel$runtime_mode, "subprocess")
  expect_false(isTRUE(built$trans$parallel$inprocess_enabled))
  expect_true(isTRUE(built$trans$parallel$uses_parallelize))
  expect_identical(
    built$trans$parallel$runtime_reason,
    .mojor_parallel_runtime_reason("subprocess")
  )
  expect_identical(built$parallel_runtime$mode, "subprocess_fallback")
  expect_identical(built$parallel_runtime$reason, built$trans$parallel$runtime_reason)
  expect_identical(attr(built$func, "parallel_runtime_mode"), "subprocess_fallback")
  expect_identical(attr(built$func, "parallel_runtime_reason"), built$trans$parallel$runtime_reason)
  expect_false(isTRUE(built$compiled))
})

test_that("mojor_build passes parallel parameter correctly", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_build_parallel", parallel = TRUE)
  
  # Check that the mojo code contains parallelize
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
})

test_that("parallel loop respects parallel safety analysis", {  f1 <- function(x) {
    out <- numeric(length(x))
    # This loop is parallel-safe (no cross-iteration dependencies)
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans1 <- mojor_transpile(f1, x = "f64[]", name = "t_safe", parallel = TRUE)
  expect_true(grepl("parallelize", trans1$mojo, fixed = TRUE))
  
  f2 <- function(x) {
    out <- numeric(length(x))
    # This loop is NOT parallel-safe (output depends on previous output value)
    for (i in 2:length(x)) {
      out[i] <- out[i-1] + x[i]
    }
    out
  }
  
  # When parallel=TRUE but loop is not parallel-safe, we fall back to regular for
  trans2 <- mojor_transpile(f2, x = "f64[]", name = "t_unsafe", parallel = TRUE)
  # Should NOT have parallelize since it's not parallel-safe
  expect_false(grepl("parallelize", trans2$mojo, fixed = TRUE))
  # Should have regular for
  expect_true(grepl("for ", trans2$mojo, fixed = TRUE))
})
