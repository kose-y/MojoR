library(testthat)

# Phase 5.1: Local Matrix/Array Allocation Support
# Tests for matrix slice assignment with locally-allocated matrices

test_that("Local matrix allocation with slice assignment transpiles", {  f <- function(N) {
    mat <- matrix(0, nrow = N, ncol = 2)
    for (i in 1:N) {
      mat[i, ] <- c(1.0, 2.0)
    }
    mat
  }

  result <- mojor_transpile(f, N = "i32")

  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))

  # Check that dimension variables are emitted
  expect_match(result$mojo, "nrow_mat", fixed = TRUE)
  expect_match(result$mojo, "ncol_mat", fixed = TRUE)
})

test_that("Gibbs sampler pattern with local matrix works", {  skip_if_no_mojo()

  gibbs <- function(N, thin) {
    mat <- matrix(0, nrow = N, ncol = 2)
    x <- y <- 0.0
    for (i in 1:N) {
      for (j in 1:thin) {
        x <- x + 1.0
        y <- y + 2.0
      }
      mat[i, ] <- c(x, y)
    }
    mat
  }

  # R reference
  gibbs_r <- function(N, thin) {
    mat <- matrix(0, nrow = N, ncol = 2)
    x <- y <- 0.0
    for (i in 1:N) {
      for (j in 1:thin) {
        x <- x + 1.0
        y <- y + 2.0
      }
      mat[i, ] <- c(x, y)
    }
    mat
  }

  built <- mojor_build(gibbs, N = "i32", thin = "i32", name = "test_gibbs", cache = FALSE, load = TRUE)
  result_compiled <- built$func(5L, 3L)
  result_r <- gibbs_r(5L, 3L)

  expect_equal(result_compiled, result_r)
})

test_that("Local matrix with dynamic dimensions", {  skip_if_no_mojo()

  f <- function(nr, nc) {
    mat <- matrix(0, nrow = nr, ncol = nc)
    for (i in 1:nr) {
      for (j in 1:nc) {
        mat[i, j] <- i * 10 + j
      }
    }
    mat
  }

  built <- mojor_build(f, nr = "i32", nc = "i32", name = "test_dynamic_mat", cache = FALSE, load = TRUE)
  result <- built$func(3L, 4L)

  expected <- matrix(0, 3, 4)
  for (i in 1:3) {
    for (j in 1:4) {
      expected[i, j] <- i * 10 + j
    }
  }

  expect_equal(result, expected)
})

test_that("Local matrix as intermediate pre-loop buffer transpiles", {  f <- function(N) {
    mat <- matrix(0, nrow = N, ncol = 2)
    for (i in 1:N) {
      mat[i, 1] <- i
      mat[i, 2] <- i * 2
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- mat[i, 1] + mat[i, 2]
    }
    out
  }

  result <- mojor_transpile(f, N = "i32")
  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "var mat = alloc[Float64]", fixed = TRUE)
})

test_that("Local matrix as intermediate pre-loop buffer runtime parity", {  skip_if_no_mojo()

  f <- function(N) {
    mat <- matrix(0, nrow = N, ncol = 2)
    for (i in 1:N) {
      mat[i, 1] <- i
      mat[i, 2] <- i * 2
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- mat[i, 1] + mat[i, 2]
    }
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_local_matrix_intermediate", cache = FALSE, load = TRUE)
  expect_equal(built$func(6L), f(6L))
})

test_that("Local array as intermediate pre-loop buffer transpiles", {  f <- function(N) {
    arr <- array(0, dim = c(N, 2, 1))
    for (i in 1:N) {
      arr[i, 1, 1] <- i
      arr[i, 2, 1] <- i * 2
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- arr[i, 1, 1] + arr[i, 2, 1]
    }
    out
  }

  result <- mojor_transpile(f, N = "i32")
  expect_true(!is.null(result))
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "var arr = alloc[Float64]", fixed = TRUE)
})

test_that("Local array as intermediate pre-loop buffer runtime parity", {  skip_if_no_mojo()

  f <- function(N) {
    arr <- array(0, dim = c(N, 2, 1))
    for (i in 1:N) {
      arr[i, 1, 1] <- i
      arr[i, 2, 1] <- i * 2
    }
    out <- numeric(N)
    for (i in 1:N) {
      out[i] <- arr[i, 1, 1] + arr[i, 2, 1]
    }
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_local_array_intermediate", cache = FALSE, load = TRUE)
  expect_equal(built$func(6L), f(6L))
})

test_that("Local matrix with row assignment", {  skip_if_no_mojo()

  f <- function(N) {
    mat <- matrix(0, N, 3)
    for (i in 1:N) {
      mat[i, ] <- c(i, i*2, i*3)
    }
    mat
  }

  built <- mojor_build(f, N = "i32", name = "test_row_assign", cache = FALSE, load = TRUE)
  result <- built$func(5L)

  expected <- matrix(0, 5, 3)
  for (i in 1:5) {
    expected[i, ] <- c(i, i*2, i*3)
  }

  expect_equal(result, expected)
})

test_that("Local matrix with column assignment", {  skip_if_no_mojo()

  f <- function(N) {
    mat <- matrix(0, 3, N)
    for (j in 1:N) {
      mat[, j] <- c(j, j*10, j*100)
    }
    mat
  }

  built <- mojor_build(f, N = "i32", name = "test_col_assign", cache = FALSE, load = TRUE)
  result <- built$func(4L)

  expected <- matrix(0, 3, 4)
  for (j in 1:4) {
    expected[, j] <- c(j, j*10, j*100)
  }

  expect_equal(result, expected)
})

test_that("Multiple local matrices in same function", {  skip_if_no_mojo()
  f <- function(N) {
    mat1 <- matrix(0, N, 2)
    mat2 <- matrix(0, N, 2)

    for (i in 1:N) {
      mat1[i, ] <- c(i, i*2)
      mat2[i, ] <- c(i*10, i*20)
    }

    # Return sum of both matrices
    out <- matrix(0, N, 2)
    for (i in 1:N) {
      for (j in 1:2) {
        out[i, j] <- mat1[i, j] + mat2[i, j]
      }
    }
    out
  }

  built <- mojor_build(f, N = "i32", name = "test_multi_mat", cache = FALSE, load = TRUE)
  result <- built$func(3L)

  expected <- matrix(0, 3, 2)
  for (i in 1:3) {
    expected[i, ] <- c(i + i*10, i*2 + i*20)
  }

  expect_equal(result, expected)
})

test_that("Local matrix with scalar accumulation", {  skip_if_no_mojo()

  # Pattern: accumulate scalars, then store in matrix
  f <- function(N, K) {
    results <- matrix(0, N, 2)
    for (i in 1:N) {
      sum <- 0.0
      prod <- 1.0
      for (k in 1:K) {
        sum <- sum + k
        prod <- prod * 1.1
      }
      results[i, ] <- c(sum, prod)
    }
    results
  }

  built <- mojor_build(f, N = "i32", K = "i32", name = "test_scalar_accum", cache = FALSE, load = TRUE)
  result <- built$func(4L, 3L)

  # Reference
  f_r <- function(N, K) {
    results <- matrix(0, N, 2)
    for (i in 1:N) {
      sum <- 0.0
      prod <- 1.0
      for (k in 1:K) {
        sum <- sum + k
        prod <- prod * 1.1
      }
      results[i, ] <- c(sum, prod)
    }
    results
  }

  expected <- f_r(4L, 3L)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("Local matrix dimensions are correctly tracked", {  # Verify that dimension tracking happens during transpilation
  f <- function(M, N) {
    mat <- matrix(0, M, N)
    for (i in 1:M) {
      mat[i, ] <- rep(i, N)
    }
    mat
  }

  trans <- mojor_transpile(f, M = "i32", N = "i32")

  # Check that dimension variables are emitted with correct names
  expect_match(trans$mojo, "var nrow_mat = Int\\(M\\)", fixed = FALSE)
  expect_match(trans$mojo, "var ncol_mat = Int\\(N\\)", fixed = FALSE)
})

test_that("Local matrix with constant dimensions", {  skip_if_no_mojo()

  f <- function(N) {
    # Matrix with one dimension constant
    mat <- matrix(0, N, 5)  # ncol is constant
    for (i in 1:N) {
      for (j in 1:5) {
        mat[i, j] <- i + j
      }
    }
    mat
  }

  built <- mojor_build(f, N = "i32", name = "test_const_dim", cache = FALSE, load = TRUE)
  result <- built$func(3L)

  expected <- matrix(0, 3, 5)
  for (i in 1:3) {
    for (j in 1:5) {
      expected[i, j] <- i + j
    }
  }

  expect_equal(result, expected)
})

test_that("Local matrix output as return value", {  skip_if_no_mojo()
  f <- function(rows, cols) {
    mat <- matrix(0, rows, cols)
    for (i in 1:rows) {
      for (j in 1:cols) {
        mat[i, j] <- i * 100 + j
      }
    }
    mat  # Return the local matrix
  }

  built <- mojor_build(f, rows = "i32", cols = "i32", name = "test_return_local", cache = FALSE, load = TRUE)
  result <- built$func(2L, 3L)

  expected <- matrix(c(101, 201, 102, 202, 103, 203), nrow = 2, ncol = 3, byrow = FALSE)
  expect_equal(result, expected)
})

test_that("Local matrix constructor accepts byrow=TRUE", {
  f <- function(x) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE)
    out <- numeric(length(x))
    for (j in seq_len(length(x))) {
      out[j] <- mat[j]
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(trans))
  expect_true(!is.null(trans$mojo))
})

test_that("Local matrix constructor with byrow=TRUE preserves row-wise fill", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE)
    out <- numeric(length(x))
    for (j in seq_len(length(x))) {
      out[j] <- mat[j]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    name = "test_local_matrix_byrow_true",
    cache = FALSE,
    load = TRUE
  )
  x <- c(1, 2, 3, 4, 5, 6)
  expected <- as.vector(matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE))
  expect_equal(built$func(x), expected)
})

test_that("Direct-return local matrix constructor writes to output buffer", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(x, nrow = 2L, ncol = 3L)
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    name = "test_local_matrix_direct_return",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(built$func(x), matrix(x, nrow = 2L, ncol = 3L))
})

test_that("Direct-return local matrix constructor honors byrow=TRUE", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE)
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    name = "test_local_matrix_direct_return_byrow",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(built$func(x), matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE))
})

test_that("Direct-return local matrix constructor accepts scalar byrow flag", {  skip_if_no_mojo()
  f <- function(x, byrow_flag) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = byrow_flag)
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    byrow_flag = "lgl",
    ir_only = TRUE,
    name = "test_local_matrix_direct_return_byrow_flag",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(built$func(x, TRUE), matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE))
  expect_equal(built$func(x, FALSE), matrix(x, nrow = 2L, ncol = 3L, byrow = FALSE))
})

test_that("Direct-return local matrix constructor accepts computed scalar byrow expression", {  skip_if_no_mojo()
  f <- function(x, byrow_flag) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = ((byrow_flag + 1L) %% 2L))
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    byrow_flag = "i32",
    ir_only = TRUE,
    name = "test_local_matrix_direct_return_byrow_expr",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(built$func(x, 0L), matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE))
  expect_equal(built$func(x, 1L), matrix(x, nrow = 2L, ncol = 3L, byrow = FALSE))
})

test_that("Non-direct local matrix constructor accepts computed scalar byrow expression", {  skip_if_no_mojo()
  f <- function(x, byrow_flag) {
    mat <- matrix(x, nrow = 2L, ncol = 3L, byrow = ((byrow_flag + 1L) %% 2L))
    out <- numeric(length(x))
    for (j in seq_len(length(x))) {
      out[j] <- mat[j]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    byrow_flag = "i32",
    ir_only = TRUE,
    name = "test_local_matrix_non_direct_byrow_expr",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(
    built$func(x, 0L),
    as.vector(matrix(x, nrow = 2L, ncol = 3L, byrow = TRUE))
  )
  expect_equal(
    built$func(x, 1L),
    as.vector(matrix(x, nrow = 2L, ncol = 3L, byrow = FALSE))
  )
})

test_that("Strict local matrix constructor supports inline literal c() data", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
    mat[1, 1] <- x[1]
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    name = "test_local_matrix_inline_c_literal",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(9)
  expect_equal(built$func(x), f(x))
})

test_that("Strict local array constructor supports inline literal c() data", {  skip_if_no_mojo()
  f <- function(x) {
    arr <- array(c(1, 2, 3, 4, 5, 6), dim = c(2L, 3L, 1L))
    arr[1, 1, 1] <- x[1]
    arr
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    name = "test_local_array_inline_c_literal",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(9)
  expect_equal(built$func(x), f(x))
})

test_that("Strict local matrix constructor supports computed helper data expressions", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(log1p(x), nrow = 2L, ncol = 3L)
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    name = "test_local_matrix_computed_helper_data",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  expect_equal(built$func(x), f(x))
})

test_that("Strict local array constructor supports computed arithmetic data expressions", {  skip_if_no_mojo()
  f <- function(x, shift) {
    arr <- array((x * 2.0) + shift, dim = c(2L, 3L, 1L))
    arr
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    shift = "f64",
    ir_only = TRUE,
    name = "test_local_array_computed_expr_data",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:6)
  shift <- 0.5
  expect_equal(built$func(x, shift), f(x, shift))
})
