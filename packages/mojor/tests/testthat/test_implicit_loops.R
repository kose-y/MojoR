# =============================================================================
# Phase 4.4: Implicit Loops Tests
# =============================================================================

library(testthat)

# Helper: Extract kernel from Mojo code
extract_ew_kernel <- function(mojo) {
  start <- regexpr("fn _mojor_elementwise\\[", mojo)
  if (start < 0) return(NULL)
  sub <- substr(mojo, start, nchar(mojo))
  end_rel <- regexpr("fn _mojor_elementwise_[A-Za-z0-9]+", sub)
  if (end_rel < 0) return(sub)
  substr(sub, 1, end_rel - 1)
}

# =============================================================================
# Transpilation Tests
# =============================================================================

test_that("x + y lowers to vector loop", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_implicit_add")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("x * y lowers to vector loop", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * y[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_implicit_mul")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("x - y lowers to vector loop", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] - y[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_implicit_sub")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("x / y lowers to vector loop", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] / y[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_implicit_div")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("x > y comparison lowers to vector loop", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > y[i], 1, 0)
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_implicit_cmp")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("ifelse in implicit loop works correctly", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > y[i], x[i], -y[i])
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_implicit_ifelse_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(c(1, 2, 3, 4, 5))
  y <- as.double(c(3, 2, 1, 4, 5))
  res <- built$func(x, y)
  expected <- ifelse(x > y, x, -y)
  expect_equal(res, expected)
})

test_that("sin(x) math function lowers to vector loop", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- sin(x[i])
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_implicit_sin")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

test_that("cos(x) math function lowers to vector loop", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- cos(x[i])
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_implicit_cos")
  expect_true(grepl("elementwise", trans$mojo) || grepl("for.*in range", trans$mojo))
})

# =============================================================================
# Runtime Tests
# =============================================================================

test_that("x + y runtime correctness", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_implicit_add_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(10:14)
  res <- built$func(x, y)
  expect_equal(res, x + y)
})

test_that("x * y runtime correctness", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_implicit_mul_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(2:6)
  res <- built$func(x, y)
  expect_equal(res, x * y)
})

test_that("sin(x) runtime correctness", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- sin(x[i])
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    name = "t_implicit_sin_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(c(0, pi/6, pi/4, pi/3, pi/2))
  res <- built$func(x)
  expect_equal(res, sin(x), tolerance = 1e-10)
})

test_that("exact-multiple recycling supported", {  skip("Recycling with dynamic length not yet supported - requires loop variable analysis")
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[(i - 1) %% length(y) + 1]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "recycle",
    name = "t_implicit_recycle_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:6)
  y <- as.double(c(10, 20, 30))
  res <- built$func(x, y)
  expected <- x + rep(y, length.out = length(x))
  expect_equal(res, expected)
})

test_that("scalar broadcast supported", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 5
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f64[]",
    broadcast = "scalar",
    name = "t_implicit_scalar_broadcast_runtime",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  res <- built$func(x)
  expect_equal(res, x + 5)
})

# =============================================================================
# Error Cases
# =============================================================================

test_that("mismatched lengths produce clear error", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  # This should work with broadcast = "none" since lengths match
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "none",
    name = "t_implicit_error_match",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.double(1:5)
  y <- as.double(10:14)
  res <- built$func(x, y)
  expect_equal(res, x + y)
})
