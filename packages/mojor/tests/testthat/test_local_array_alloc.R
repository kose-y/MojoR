library(testthat)

# Phase 5.1 Stage C: Local Array Allocation Support
# Tests for N-dimensional array allocations that become outputs
#
# KEY LIMITATION: Stage C only supports arrays that are returned as function outputs.
# True local allocations (intermediate buffers) are not yet supported.

test_that("Local 3D array (as output) transpiles", {  f <- function(N, M, K) {
    arr <- array(0, dim = c(N, M, K))
    for (i in 1:N) {
      for (j in 1:M) {
        for (k in 1:K) {
          arr[i, j, k] <- i + j + k
        }
      }
    }
    arr
  }

  result <- mojor_transpile(f, N = "i32", M = "i32", K = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))

  # Array identified as output, so becomes output parameter
  expect_match(result$mojo, "arr_ptr", fixed = TRUE)
  # Dimension tracking via dim_out_ptr
  expect_match(result$mojo, "dim_out_ptr", fixed = TRUE)
})

test_that("Local 3D array with element assignment works", {  skip_if_no_mojo()

  f <- function(N, M, K) {
    arr <- array(0, dim = c(N, M, K))
    for (i in 1:N) {
      for (j in 1:M) {
        for (k in 1:K) {
          arr[i, j, k] <- i * 100 + j * 10 + k
        }
      }
    }
    arr
  }

  built <- mojor_build(f, N = "i32", M = "i32", K = "i32",
                       name = "test_arr_c_3d", cache = FALSE, load = TRUE)
  result <- built$func(2L, 3L, 2L)

  expected <- array(0, dim = c(2, 3, 2))
  for (i in 1:2) {
    for (j in 1:3) {
      for (k in 1:2) {
        expected[i, j, k] <- i * 100 + j * 10 + k
      }
    }
  }

  expect_equal(result, expected)
})

test_that("Local 3D numeric array works", {  skip_if_no_mojo()

  f <- function(X, Y, Z) {
    arr <- array(0.0, dim = c(X, Y, Z))
    for (i in 1:X) {
      for (j in 1:Y) {
        for (k in 1:Z) {
          arr[i, j, k] <- i + j + k + 0.5
        }
      }
    }
    arr
  }

  built <- mojor_build(f, X = "i32", Y = "i32", Z = "i32",
                       name = "test_arr_c_numeric", cache = FALSE, load = TRUE)
  result <- built$func(2L, 2L, 2L)

  expected <- array(0.0, dim = c(2, 2, 2))
  for (i in 1:2) {
    for (j in 1:2) {
      for (k in 1:2) {
        expected[i, j, k] <- i + j + k + 0.5
      }
    }
  }

  expect_equal(result, expected)
})

test_that("Local 4D array works", {  skip_if_no_mojo()

  f <- function(N) {
    arr <- array(0, dim = c(N, N, N, N))
    for (i in 1:N) {
      for (j in 1:N) {
        for (k in 1:N) {
          for (m in 1:N) {
            arr[i, j, k, m] <- i + j + k + m
          }
        }
      }
    }
    arr
  }

  built <- mojor_build(f, N = "i32", name = "test_arr_c_4d", cache = FALSE, load = TRUE)
  result <- built$func(2L)

  expected <- array(0, dim = c(2, 2, 2, 2))
  for (i in 1:2) {
    for (j in 1:2) {
      for (k in 1:2) {
        for (m in 1:2) {
          expected[i, j, k, m] <- i + j + k + m
        }
      }
    }
  }

  expect_equal(result, expected)
})

test_that("Local 3D integer array works", {  skip_if_no_mojo()

  f <- function(A, B, C) {
    arr <- array(0L, dim = c(A, B, C))
    for (i in 1:A) {
      for (j in 1:B) {
        for (k in 1:C) {
          arr[i, j, k] <- i * j * k
        }
      }
    }
    arr
  }

  built <- mojor_build(f, A = "i32", B = "i32", C = "i32",
                       name = "test_arr_c_int", cache = FALSE, load = TRUE)
  result <- built$func(2L, 2L, 3L)

  expected <- array(0L, dim = c(2, 2, 3))
  for (i in 1:2) {
    for (j in 1:2) {
      for (k in 1:3) {
        expected[i, j, k] <- i * j * k
      }
    }
  }

  expect_equal(result, expected)
})

test_that("Local 3D array with simple arithmetic works", {  skip_if_no_mojo()

  f <- function(D) {
    arr <- array(0.0, dim = c(D, D, D))
    for (i in 1:D) {
      for (j in 1:D) {
        for (k in 1:D) {
          arr[i, j, k] <- i * j + k
        }
      }
    }
    arr
  }

  built <- mojor_build(f, D = "i32", name = "test_arr_c_simple", cache = FALSE, load = TRUE)
  result <- built$func(3L)

  expected <- array(0.0, dim = c(3, 3, 3))
  for (i in 1:3) {
    for (j in 1:3) {
      for (k in 1:3) {
        expected[i, j, k] <- i * j + k
      }
    }
  }

  expect_equal(result, expected)
})

test_that("Local 3D array transpilation check", {  # Verify code generation patterns
  f <- function(P, Q, R) {
    arr <- array(0, dim = c(P, Q, R))
    for (i in 1:P) {
      for (j in 1:Q) {
        for (k in 1:R) {
          arr[i, j, k] <- i + j + k
        }
      }
    }
    arr
  }

  trans <- mojor_transpile(f, P = "i32", Q = "i32", R = "i32")

  # Check for output parameter
  expect_match(trans$mojo, "arr_ptr", fixed = TRUE)
  # Check for LayoutTensor usage
  expect_match(trans$mojo, "LayoutTensor", fixed = TRUE)
  # Check for dimension pointer
  expect_match(trans$mojo, "dim_out_ptr", fixed = TRUE)
})

# Tests for unsupported patterns (skipped with clear explanations)

test_that("Multiple local arrays (not as output) are supported", {  skip_if_no_mojo()

  f <- function(N) {
    arr1 <- array(0, dim = c(N, N, 2))
    arr2 <- array(0, dim = c(N, N, 2))

    for (i in 1:N) {
      for (j in 1:N) {
        arr1[i, j, 1] <- i + j
        arr2[i, j, 1] <- i * j
      }
    }

    result <- array(0, dim = c(N, N, 2))
    for (i in 1:N) {
      for (j in 1:N) {
        for (k in 1:2) {
          result[i, j, k] <- arr1[i, j, k] + arr2[i, j, k]
        }
      }
    }
    result
  }

  built <- mojor_build(f, N = "i32", name = "test_local_array_multi", cache = FALSE, load = TRUE)
  expect_equal(built$func(3L), f(3L))
})

test_that("Local array as intermediate buffer is supported", {  skip_if_no_mojo()

  f <- function(N) {
    temp <- array(0, dim = c(N, N, 2))
    for (i in 1:N) {
      for (j in 1:N) {
        temp[i, j, 1] <- i * j
        temp[i, j, 2] <- i + j
      }
    }

    # Compute scalar from temp
    sum <- 0.0
    for (i in 1:N) {
      for (j in 1:N) {
        sum <- sum + temp[i, j, 1] + temp[i, j, 2]
      }
    }

    out <- array(0.0, dim = c(1, 1, 1))
    out[1, 1, 1] <- sum
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_local_array_intermediate_buffer", cache = FALSE, load = TRUE)
  expect_equal(built$func(4L), f(4L))
})
