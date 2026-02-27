library(testthat)

# Phase 5.1 Stage B: Local Vector Allocation Support
# Tests for element assignment with locally-allocated vectors that become outputs
#
# Stage B now supports both output vectors and intermediate local vector allocations.

test_that("Local numeric vector (as output) transpiles", {  f <- function(N) {
    vec <- numeric(N)
    for (i in 1:N) {
      vec[i] <- i * 2.0
    }
    vec
  }

  result <- mojor_transpile(f, N = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))

  # Vector identified as output, so becomes output parameter (no length_vec needed)
  expect_match(result$mojo, "vec_ptr", fixed = TRUE)
})

test_that("Local integer vector (as output) transpiles", {  f <- function(N) {
    vec <- integer(N)
    for (i in 1:N) {
      vec[i] <- i * 2L
    }
    vec
  }

  result <- mojor_transpile(f, N = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))

  expect_match(result$mojo, "vec_ptr", fixed = TRUE)
})

test_that("Local numeric vector with element assignment works", {  skip_if_no_mojo()

  f <- function(N) {
    vec <- numeric(N)
    for (i in 1:N) {
      vec[i] <- i * 2.0
    }
    vec
  }

  built <- mojor_build(f, N = "i32", name = "test_vec_b_elem", cache = FALSE, load = TRUE)
  result <- built$func(5L)

  expected <- numeric(5)
  for (i in 1:5) {
    expected[i] <- i * 2.0
  }

  expect_equal(result, expected)
})

test_that("Local integer vector with element assignment works", {  skip_if_no_mojo()

  f <- function(N) {
    vec <- integer(N)
    for (i in 1:N) {
      vec[i] <- i * 2L
    }
    vec
  }

  built <- mojor_build(f, N = "i32", name = "test_ivec_b_elem", cache = FALSE, load = TRUE)
  result <- built$func(5L)

  expected <- integer(5)
  for (i in 1:5) {
    expected[i] <- i * 2L
  }

  expect_equal(result, expected)
})

test_that("Local vector with dynamic length from parameter works", {  skip_if_no_mojo()

  f <- function(N) {
    result <- numeric(N)
    for (i in 1:N) {
      result[i] <- i * i
    }
    result
  }

  built <- mojor_build(f, N = "i32", name = "test_vec_b_dynamic", cache = FALSE, load = TRUE)
  result_compiled <- built$func(10L)

  expected <- numeric(10)
  for (i in 1:10) {
    expected[i] <- i * i
  }

  expect_equal(result_compiled, expected)
})

test_that("Local vector with simple arithmetic length works", {  skip_if_no_mojo()

  f <- function(N) {
    vec <- numeric(N)
    for (i in 1:N) {
      vec[i] <- i + 0.5
    }
    vec
  }

  built <- mojor_build(f, N = "i32", name = "test_vec_b_simple2", cache = FALSE, load = TRUE)
  result <- built$func(5L)

  expected <- numeric(5)
  for (i in 1:5) {
    expected[i] <- i + 0.5
  }

  expect_equal(result, expected)
})

test_that("Local logical vector allocation works", {  skip_if_no_mojo()

  f <- function(N) {
    vec <- logical(N)
    for (i in 1:N) {
      vec[i] <- (i %% 2L) == 1L
    }
    vec
  }

  built <- mojor_build(f, N = "i32", name = "test_logical_b_vec", cache = FALSE, load = TRUE)
  result <- built$func(5L)

  expected <- logical(5)
  for (i in 1:5) {
    expected[i] <- (i %% 2L) == 1L
  }

  expect_equal(result, expected)
})

test_that("Local vector as intermediate transpiles", {  f <- function(N) {
    squares <- numeric(N)
    for (i in 1:N) {
      squares[i] <- i * i
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- squares[i] + 1.0
    }
    out
  }

  result <- mojor_transpile(f, N = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "var squares = alloc[Float64]", fixed = TRUE)
})

test_that("Local vector as intermediate runtime parity", {  skip_if_no_mojo()

  f <- function(N) {
    squares <- numeric(N)
    for (i in 1:N) {
      squares[i] <- i * i
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- squares[i] + 1.0
    }
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_vec_b_intermediate", cache = FALSE, load = TRUE)
  expect_equal(built$func(6L), f(6L))
})

# Tests for unsupported patterns (skipped with clear explanations)

test_that("Multiple local vectors transpile", {  f <- function(N) {
    vec1 <- numeric(N)
    vec2 <- numeric(N)
    for (i in 1:N) {
      vec1[i] <- i * 2.0
      vec2[i] <- i * 3.0
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- vec1[i] + vec2[i]
    }
    out
  }

  result <- mojor_transpile(f, N = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "var vec1 = alloc[Float64]", fixed = TRUE)
  expect_match(result$mojo, "var vec2 = alloc[Float64]", fixed = TRUE)
})

test_that("Multiple local vectors runtime parity", {  skip_if_no_mojo()

  f <- function(N) {
    vec1 <- numeric(N)
    vec2 <- numeric(N)
    for (i in 1:N) {
      vec1[i] <- i * 2.0
      vec2[i] <- i * 3.0
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- vec1[i] + vec2[i]
    }
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_vec_b_multi_intermediate", cache = FALSE, load = TRUE)
  expect_equal(built$func(7L), f(7L))
})

test_that("Local vector accumulation with dependent loop ranges", {  f <- function(N) {
    sums <- numeric(N)
    for (i in 1:N) {
      sum <- 0.0
      for (j in 1:i) {  # j range depends on i
        sum <- sum + j
      }
      sums[i] <- sum
    }
    sums
  }

  result <- mojor_transpile(f, N = "i32")
  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for j in", fixed = TRUE)

  skip_if_no_mojo()
  built <- mojor_build(f, N = "i32", name = "test_vec_b_dep_loop", cache = FALSE, load = TRUE)
  expect_equal(built$func(8L), f(8L))
})
