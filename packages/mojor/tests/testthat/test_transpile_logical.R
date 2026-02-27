library(testthat)


test_that("logical output is supported", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] > 0
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_logical_out")
  expect_equal(res$out_type, "lgl[]")
  expect_mojor_any_match(res$mojo, "Int32\\(")
})


test_that("logical inputs can be used as conditions", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i]) {
        out[i] <- 1
      } else {
        out[i] <- 0
      }
    }
    out
  }

  res <- mojor_transpile(f, x = "lgl[]", name = "mojor_logical_cond")
  expect_true(grepl("if.*_mojor_read_lgl.*x", res$mojo))
})


test_that("reject numeric assignment into logical output", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }

  expect_mojor_error_or_result(
    run = function() mojor_transpile(f, x = "f64[]", name = "mojor_bad_logical"),
    error_patterns = c(
      "Logical outputs must be assigned",
      "top-level loop not supported by IR",
      "IR statement emission failed",
      "strict IR emission failed"
    ),
    on_success = function(res) {
      expect_equal(res$out_type, "lgl[]")
      expect_mojor_any_match(res$mojo, "Int32\\(")
    }
  )
})
