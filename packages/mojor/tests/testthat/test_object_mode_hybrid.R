test_that("object_mode hybrid accelerates eligible steps and reuses cache", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  foo_local <- function(v) v + 1
  f <- function(x) {
    y <- x * 2
    s <- sum(y)
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out + s
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  expect_true(isTRUE(built$success))
  expect_true(isTRUE(built$object_mode))
  expect_equal(built$object_mode_kind, "hybrid")
  expect_true(is.data.frame(built$object_mode_entry_summary))
  expect_true(all(c("reason", "count") %in% names(built$object_mode_entry_summary)))
  expect_identical(as.integer(built$object_mode_entry_summary$count[[1L]]), 1L)
  expect_true(is.list(built$object_plan_stats))
  expect_true(built$object_plan_stats$steps_accelerable >= 1)
  expect_true(is.environment(built$object_runtime_state))

  x <- c(-1, 0, 1, 2)
  out1 <- built$func(x)
  expect_equal(out1, f(x), tolerance = 1e-12)

  compiled_after_first <- built$object_runtime_state$stats$steps_compiled
  expect_true(compiled_after_first >= 1)

  out2 <- built$func(x)
  expect_equal(out2, f(x), tolerance = 1e-12)
  expect_equal(built$object_runtime_state$stats$steps_compiled, compiled_after_first)
})

test_that("object_mode hybrid deopts unsupported accelerated candidates", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  foo_local <- function(v) v + 1
  f <- function(x) {
    y <- foo_local(x) # Not compilable by transpiler; should deopt this step.
    s <- sum(y)
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out + s
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  x <- c(1, 2, 3)
  out <- built$func(x)
  expect_equal(out, f(x), tolerance = 1e-12)
  expect_true(built$object_runtime_state$stats$steps_deopted >= 1)
  expect_true(is.list(built$object_runtime_state$deopt_log))
  expect_true(length(built$object_runtime_state$deopt_log) >= 1)
  entry <- built$object_runtime_state$deopt_log[[1]]
  expect_true(all(c("step_id", "step_kind", "stage", "reason", "signature") %in% names(entry)))
  expect_true(is.list(built$object_runtime_state$deopt_reason_counts))
  expect_true(length(built$object_runtime_state$deopt_reason_counts) >= 1)
  expect_true(!is.null(built$object_runtime_state$stats$deopt_cache_hits))
  expect_true(is.data.frame(built$object_mode_entry_summary))
  expect_true(all(c("reason", "count") %in% names(built$object_mode_entry_summary)))
  expect_identical(as.integer(built$object_mode_entry_summary$count[[1L]]), 1L)

  out2 <- built$func(x)
  expect_equal(out2, f(x), tolerance = 1e-12)
  expect_true(built$object_runtime_state$stats$deopt_cache_hits >= 1)
})

test_that("object_mode hybrid supports top-level early-return guards without full R deopt", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  foo_local <- function(v) v + 1
  f <- function(x) {
    if (length(x) == 0) return(numeric(0))
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  expect_true(isTRUE(built$object_mode))
  expect_equal(built$object_mode_kind, "hybrid")
  expect_false(grepl("full R", built$object_mode_reason, fixed = TRUE))
  expect_true(is.environment(built$object_runtime_state))
  expect_equal(built$func(c(-1, 0, 1)), f(c(-1, 0, 1)), tolerance = 1e-12)
  expect_equal(built$func(numeric(0)), f(numeric(0)))
})

test_that("object_mode hybrid supports top-level if/else terminal returns without full R deopt", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  foo_local <- function(v) v + 1
  f <- function(x) {
    if (length(x) == 0) {
      return(numeric(0))
    } else {
      return(foo_local(x))
    }
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  expect_true(isTRUE(built$object_mode))
  expect_equal(built$object_mode_kind, "hybrid")
  expect_false(grepl("full R", built$object_mode_reason, fixed = TRUE))
  expect_true(is.environment(built$object_runtime_state))
  expect_equal(built$func(c(-1, 0, 1)), f(c(-1, 0, 1)), tolerance = 1e-12)
  expect_equal(built$func(numeric(0)), f(numeric(0)))
})

test_that("object_mode hybrid supports top-level if/else branch assignment before terminal return", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  foo_local <- function(v) v + 1
  f <- function(x) {
    if (length(x) == 0) {
      s <- 0
    } else {
      s <- 1
    }
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i]) + s
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  expect_true(isTRUE(built$object_mode))
  expect_equal(built$object_mode_kind, "hybrid")
  expect_false(grepl("full R", built$object_mode_reason, fixed = TRUE))
  expect_true(is.environment(built$object_runtime_state))
  expect_equal(built$func(c(-1, 0, 1)), f(c(-1, 0, 1)), tolerance = 1e-12)
  expect_equal(built$func(numeric(0)), f(numeric(0)))
})

test_that("object_mode hybrid supports nested return control flow without full R deopt", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  foo_local <- function(v) v + 1
  f <- function(x) {
    if (length(x) == 0) {
      if (sum(x) == 0) return(numeric(0))
    }
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "hybrid")
  expect_true(isTRUE(built$object_mode))
  expect_equal(built$object_mode_kind, "hybrid")
  expect_false(grepl("full R", built$object_mode_reason, fixed = TRUE))
  expect_true(is.environment(built$object_runtime_state))
  expect_equal(built$func(c(-1, 0, 1)), f(c(-1, 0, 1)), tolerance = 1e-12)
  expect_equal(built$func(numeric(0)), f(numeric(0)))
})
