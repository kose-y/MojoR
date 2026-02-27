library(testthat)

test_that("ifelse lowers in loop assignment", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, x[i], -x[i])
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse")
  expect_true(grepl("> (Float64\\(0\\)|0)", trans$mojo))
  # ifelse lowers to inline ternary (Python-style): val if cond else other
  expect_true(grepl("if.*else", trans$mojo))
})

test_that("ifelse supports logical outputs with boolean branches", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, TRUE, FALSE)
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse_lgl")
  expect_mojor_any_match(trans$mojo, "Int32\\(")
  expect_mojor_any_match(trans$mojo, "if.*else")
})

test_that("ifelse with numeric branches for logical output errors or emits", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, 1, 0)
    }
    out
  }
  expect_mojor_error_or_result(
    run = function() mojor_transpile(f, x = "f64[]", name = "t_ifelse_lgl_err"),
    error_patterns = ".+",
    on_success = function(res) {
      expect_mojor_any_match(res$mojo, "Int32\\(")
      expect_mojor_any_match(res$mojo, "if.*else")
    }
  )
})

test_that("ifelse with float branches for integer output casts", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, x[i], 0)
    }
    out
  }
  # IR path casts float result to Int32 for integer output
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse_i_err")
  expect_true(grepl("Int32", trans$mojo))
})

test_that("ifelse can be nested inside larger expressions", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, x[i], 0) + y[i]
    }
    out
  }
  expect_warning(
    trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_ifelse_nested"),
    "ifelse\\(\\) expression lowered"
  )
  # IR emits inline ternary, not a var
  expect_true(grepl("if.*else", trans$mojo))
  expect_true(grepl("out\\[.*\\].*\\+", trans$mojo))
})

test_that("ifelse is supported inside any/all", {  f <- function(x) {
    any(ifelse(x > 0, TRUE, FALSE))
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse_any")
  expect_true(grepl("&&", trans$mojo) || grepl("if", trans$mojo))
})

test_that("ifelse can be used in if condition", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (ifelse(x[i] > 0, TRUE, FALSE)) out[i] <- x[i] else out[i] <- 0
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse_if")
  expect_true(grepl("if ", trans$mojo))
})


test_that("if branches with mixed numeric types auto-cast with typed IR", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) out[i] <- x[i] else out[i] <- 1L
    }
    out
  }
  # Typed IR inserts casts automatically
  result <- mojor_transpile(f, x = "f64[]", name = "t_if_mixed")
  expect_equal(result$out_type, "f64[]")
  # Should successfully transpile with auto-cast
  expect_true(nchar(result$mojo) > 0)
})


test_that("ifelse in any/all with numeric branches is coerced", {  f <- function(x) {
    any(ifelse(x > 0, x, 0))
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ifelse_any_num")
  expect_true(grepl("!= (Float64\\(0\\)|0)", trans$mojo))
})

test_that("if/else with multi-statement bodies transpile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        out[i] <- x[i]
        out[i] <- out[i] + 1
      } else {
        out[i] <- -x[i]
        out[i] <- out[i] - 1
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_if_multi")
  expect_true(grepl("if ", trans$mojo))
  expect_true(grepl("else:", trans$mojo))
})

test_that("else-if chains with multi-statement bodies transpile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 1) {
        out[i] <- x[i]
        out[i] <- out[i] + 1
      } else if (x[i] > 0) {
        out[i] <- x[i] * 2
        out[i] <- out[i] + 2
      } else {
        out[i] <- -x[i]
        out[i] <- out[i] - 1
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_if_chain")
  expect_true(grepl("if ", trans$mojo))
  expect_true(grepl("else:", trans$mojo))
})
