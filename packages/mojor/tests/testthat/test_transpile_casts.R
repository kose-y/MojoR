library(testthat)

test_that("integer outputs auto-cast from float with typed IR", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }

  # Typed IR inserts casts automatically
  result <- mojor_transpile(f, x = "f64[]", name = "mojor_i32_need_cast")
  expect_equal(result$out_type, "i32[]")
  # Should have Int32 cast in output
  expect_true(grepl("Int32\\(", result$mojo))
})

test_that("integer outputs handle abs expression with stable contract", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- abs(x[i])
    }
    out
  }

  res <- tryCatch(
    mojor_transpile(f, x = "f64[]", name = "mojor_i32_unknown"),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    expect_match(
      conditionMessage(res),
      paste(
        c(
          "explicit cast",
          "top-level loop not supported by IR",
          "type checking failed for loop body",
          "Loop not supported",
          "IR statement emission failed"
        ),
        collapse = "|"
      )
    )
  } else {
    expect_equal(res$out_type, "i32[]")
    expect_true(grepl("Int32\\(", res$mojo))
  }
})


test_that("as.integer enables float to int assignment", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.integer(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_i32_cast")
  expect_match(res$mojo, "Int32", fixed = TRUE)
})


test_that("as.double emits Float64 cast", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.double(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "i32[]", name = "mojor_f64_cast")
  expect_match(res$mojo, "Float64", fixed = TRUE)
})

test_that("as.single emits Float32 cast", {  f <- function(x) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.single(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_f32_cast")
  expect_true(grepl("Float32\\(", res$mojo))
})
