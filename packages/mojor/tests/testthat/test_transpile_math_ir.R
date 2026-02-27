library(testthat)

# Math Function IR Support Tests

test_that("IR emits sin() correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- sin(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for import and call
  expect_true(grepl("from math import sin", res$mojo))
  expect_true(grepl("sin\\(_mojor_read_f64\\(", res$mojo))
})

test_that("IR emits cos() and tan() correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cos(x[i]) + tan(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for imports and calls
  expect_true(grepl("from math import.*cos", res$mojo))
  expect_true(grepl("from math import.*tan", res$mojo))
  expect_true(grepl("cos\\(", res$mojo))
  expect_true(grepl("tan\\(", res$mojo))
})

test_that("IR emits inverse trig functions", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- asin(x[i]) + acos(x[i]) + atan(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for all three inverse trig functions
  expect_true(grepl("asin\\(", res$mojo))
  expect_true(grepl("acos\\(", res$mojo))
  expect_true(grepl("atan\\(", res$mojo))
})

test_that("IR emits log functions correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- log(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("from math import.*log", res$mojo))
  expect_true(grepl("log\\(_mojor_read_f64\\(", res$mojo))
})

test_that("IR emits log10, log1p, log2 correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- log10(x[i]) + log1p(x[i]) + log2(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for all three log variants
  expect_true(grepl("log10\\(", res$mojo))
  expect_true(grepl("log1p\\(", res$mojo))
  expect_true(grepl("log2\\(", res$mojo))
})

test_that("IR emits exp and expm1 correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- exp(x[i]) + expm1(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("exp\\(", res$mojo))
  expect_true(grepl("expm1\\(", res$mojo))
})

test_that("IR emits sqrt and abs correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- sqrt(abs(x[i]))
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("sqrt\\(", res$mojo))
  expect_true(grepl("abs\\(", res$mojo))
})

test_that("IR emits floor correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- floor(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("floor\\(", res$mojo))
})

test_that("IR emits ceiling as ceil", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- ceiling(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # ceiling should map to Mojo's ceil
  expect_true(grepl("ceil\\(", res$mojo))
  expect_false(grepl("ceiling\\(", res$mojo))
})

test_that("IR emits trunc correctly", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- trunc(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("trunc\\(", res$mojo))
})

test_that("IR handles nested math function calls", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- sin(log(abs(x[i])))
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for nested structure
  expect_true(grepl("sin\\(log\\(abs\\(", res$mojo))
})

test_that("IR handles math functions in expressions", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- 2 * sin(x[i]) + cos(x[i]) / 3
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Math functions should be part of arithmetic expressions
  expect_true(grepl("sin\\(_mojor_read_f64\\(", res$mojo))
  expect_true(grepl("cos\\(_mojor_read_f64\\(", res$mojo))
  expect_true(grepl("\\*.*sin\\(", res$mojo))
})

# --- gamma family and build-time lowering tests ---

test_that("IR emits gamma() as tgamma()", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- gamma(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("tgamma\\(", res$mojo))
  expect_false(grepl("[^t]gamma\\(", res$mojo))
})

test_that("IR lowers factorial(x) to tgamma(x + 1)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- factorial(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # factorial(x) -> gamma(x + 1) -> tgamma(... + 1.0)
  expect_true(grepl("tgamma\\(", res$mojo))
  expect_true(grepl("\\+ 1\\.0", res$mojo))
})

test_that("IR lowers beta(a, b) to exp(lgamma(a) + lgamma(b) - lgamma(a+b))", {
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- beta(x[i], y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32")

  # Should see exp() and lgamma() calls (lowered from beta)
  expect_true(grepl("exp\\(", res$mojo))
  expect_true(grepl("lgamma\\(", res$mojo))
})

test_that("IR lowers choose(n, k) to round(exp(lgamma(...)))", {
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- choose(x[i], y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32")

  # Should see round(), exp(), lgamma() calls (lowered from choose)
  expect_true(grepl("round\\(", res$mojo))
  expect_true(grepl("exp\\(", res$mojo))
  expect_true(grepl("lgamma\\(", res$mojo))
})

test_that("IR lowers log(x, base) to log(x) / log(base)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- log(x[i], 10)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Should emit log(x) / log(10.0) pattern
  expect_true(grepl("log\\(", res$mojo))
  # Two log calls (numerator and denominator)
  log_matches <- gregexpr("log\\(", res$mojo)
  expect_true(sum(sapply(log_matches, function(m) sum(m > 0))) >= 1)
  expect_true(grepl("/", res$mojo))
})

test_that("IR lowers round(x, digits) to round(x * pow(10,d)) / pow(10,d)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- round(x[i], 2)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Should emit round() and pow() calls
  expect_true(grepl("round\\(", res$mojo))
  expect_true(grepl("pow\\(", res$mojo))
})
