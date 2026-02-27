library(testthat)

# Matrix Output IR Runtime Tests
#
# Tests that matrix output code actually compiles and produces correct results.
# These tests verify runtime behavior, not just transpilation correctness.

skip_if_no_mojo <- function() {
  if (!nzchar(Sys.which("mojo"))) {
    skip("Mojo not available")
  }
}

test_that("matrix output 1D indexing runtime - simple fill", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:5) {
      out[i] <- x[i]
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output 2D indexing runtime - simple fill", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:2) {
      for (j in 1:2) {
        out[i, j] <- x[i]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output 2D indexing runtime - addition", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i] + x[j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output 2D indexing runtime - multiplication", {  skip_if_no_mojo()

  f <- function(x, y, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i] * y[j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:5)
  y <- as.double(2:6)
  n <- 3L

  result <- built$func(x, y, n)
  expected <- f(x, y, n)

  expect_equal(result, expected)
})

test_that("matrix output 2D indexing runtime - diagonal", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:4) {
      out[i, i] <- x[i]
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 4L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output with scalar constant RHS", {  skip_if_no_mojo()

  f <- function(n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- 42.0
      }
    }
    out
  }

  built <- mojor_build(f, n = "i32", ir_only = TRUE)
  n <- 3L

  result <- built$func(n)
  expected <- f(n)

  expect_equal(result, expected)
})

test_that("matrix output with math functions in RHS", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- sin(x[i]) + cos(x[j])
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(seq(0, 2*pi, length.out = 10))
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("matrix output with conditional assignment", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:4) {
      for (j in 1:4) {
        if (i <= j) {
          out[i, j] <- x[i]
        }
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 4L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output i32 dtype", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0L, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- as.integer(x[i] + x[j])
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output with index arithmetic", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:2) {
      for (j in 1:2) {
        out[i, j] <- x[i + j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output mixed 1D and 2D indexing", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    # Fill first row with 1D indexing
    for (i in 1:3) {
      out[i] <- x[i]
    }
    # Fill rest with 2D indexing
    for (i in 2:3) {
      for (j in 1:3) {
        out[i, j] <- x[i] + x[j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output with non-square dimensions", {  skip_if_no_mojo()

  f <- function(x, nrow_val, ncol_val) {
    out <- matrix(0, nrow_val, ncol_val)
    for (i in 1:2) {
      for (j in 1:3) {
        out[i, j] <- x[i] * x[j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", nrow_val = "i32", ncol_val = "i32",
                       ir_only = TRUE)
  x <- as.double(1:10)
  nrow_val <- 2L
  ncol_val <- 3L

  result <- built$func(x, nrow_val, ncol_val)
  expected <- f(x, nrow_val, ncol_val)

  expect_equal(result, expected)
})

test_that("matrix output accumulation pattern", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (k in 1:5) {
      for (i in 1:3) {
        for (j in 1:3) {
          out[i, j] <- out[i, j] + x[k]
        }
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})

test_that("matrix output with complex expression RHS", {  skip_if_no_mojo()

  f <- function(x, y, z, n) {
    out <- matrix(0, n, n)
    for (i in 1:2) {
      for (j in 1:2) {
        out[i, j] <- x[i] + y[j] * z[i + j]
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", z = "f64[]", n = "i32",
                       ir_only = TRUE)
  x <- as.double(1:5)
  y <- as.double(2:6)
  z <- as.double(3:7)
  n <- 3L

  result <- built$func(x, y, z, n)
  expected <- f(x, y, z, n)

  expect_equal(result, expected)
})

test_that("matrix output with type cast in assignment", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- as.double(i + j)
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", ir_only = TRUE)
  x <- as.double(1:10)
  n <- 3L

  result <- built$func(x, n)
  expected <- f(x, n)

  expect_equal(result, expected)
})
