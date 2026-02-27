test_that("object_mode fallback runs unsupported loop in R", {  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out
  }

  expect_error(
    mojor_build(f, x = "f64[]", object_mode = "off"),
    "Loop not supported|unsupported|strict mode \\(ir_only=TRUE\\) forbids object-mode fallback"
  )

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  built <- mojor_build(f, x = "f64[]", object_mode = "fallback")
  expect_true(isTRUE(built$success))
  expect_true(isTRUE(built$object_mode))
  expect_false(isTRUE(built$compiled))
  expect_true(is.data.frame(built$object_mode_entry_summary))
  expect_true(all(c("reason", "count") %in% names(built$object_mode_entry_summary)))
  expect_identical(as.integer(built$object_mode_entry_summary$count[[1L]]), 1L)
  expect_true(is.character(built$object_mode_entry_message))
  expect_true(nzchar(built$object_mode_entry_message[[1L]]))
  expect_null(built$gpu_func)
  expect_null(built$gpu_func_raw)

  x <- c(-1, 0, 1, 2)
  expect_equal(built$func(x), f(x), tolerance = 1e-12)
})

test_that("object_mode fallback cooperates with error_mode wrappers", {  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out
  }

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  built <- mojor_build(f, x = "f64[]", object_mode = "fallback", error_mode = "partial")
  result <- built$func(c(-1, 0, 1))
  expect_s3_class(result, "mojor_error_result")
  expect_equal(result_status(result), "ok")
  expect_equal(result_data(result), f(c(-1, 0, 1)), tolerance = 1e-12)
})

test_that("object_mode fallback does not mask invalid type specs", {  f <- function(x) x

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  expect_error(
    mojor_build(f, x = "not_a_type", object_mode = "fallback"),
    "unsupported scalar type|unsupported type spec"
  )
})

test_that("object_mode fallback is forbidden in strict ir_only mode", {  f <- function(x) x + 1
  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)
  expect_error(
    mojor_build(f, x = "f64[]", object_mode = "fallback"),
    "object_mode must be 'off' when ir_only=TRUE"
  )
})
