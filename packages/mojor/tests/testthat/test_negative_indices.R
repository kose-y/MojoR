library(testthat)

# This file mixes many transpile/runtime negative-index lanes and can stress
# wrapper lifecycle under high parallel sweeps; keep reset mode conservative.
Sys.setenv(MOJOR_TEST_RESET_MODE = "hard", MOJOR_TEST_FILE_RESET_MODE = "hard")

# Tests for R-style negative indexing (element exclusion)
# In R, x[-k] excludes the k-th element, returning a shorter vector.
# MojoR supports scalar negative indices in all contexts:
#   - Whole-vector assignment: out <- x[-k] (copy-skip loop, output length n-1)
#   - Loop body read: out[i] <- x[-k] (first element of exclusion result)
#   - Write-side: out[-k] <- val (assign to all positions except k-th)

# --- Transpile tests: whole-vector exclusion ---

test_that("out <- x[-1] transpiles as R-style exclusion (drop first)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i] * 2.0
    }
    out <- x[-1]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  # Should emit exclusion loop with skip condition
  expect_true(grepl("!=", res$mojo))
})

test_that("out <- x[-3] transpiles as exclusion (drop third element)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-3]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  expect_true(grepl("!= 2", res$mojo))  # 3-1 = 2 (0-based)
})

# --- Transpile tests: loop body read (first element of exclusion) ---

test_that("out[i] <- x[-1] in loop body transpiles (first element of exclusion)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-1]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  # x[-1] in scalar context: first element of exclusion = x[1] (0-based)
  # Emitted as ternary: x[1 if (0) == 0 else 0]
  expect_true(grepl("1 if.*== 0 else 0", res$mojo))
})

test_that("out[i] <- x[-3] in loop body transpiles (first element of exclusion)", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-3]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  # x[-3] in scalar context: k_0based=2, first element = x[0]
  # Emitted as ternary: x[1 if (2) == 0 else 0]
  expect_true(grepl("1 if.*== 0 else 0", res$mojo))
})

# --- Transpile tests: write-side exclusion ---

test_that("out[-1] <- val transpiles as write-side exclusion", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[-1] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  # Should emit skip-write loop with != 0 (0-based for k=1)
  expect_true(grepl("!= 0", res$mojo))
})

test_that("out[-3] <- val transpiles as write-side exclusion", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[-3] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  # Should emit skip-write loop with != 2 (0-based for k=3)
  expect_true(grepl("!= 2", res$mojo))
})

# --- Transpile tests: dynamic scalar exclusion ---

test_that("out <- x[-k] dynamic exclusion transpiles", {
  f <- function(x, k) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out <- x[-k]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit exclusion loop with dynamic skip condition
  expect_true(grepl("Int\\(k\\) - 1", res$mojo) || grepl("!=", res$mojo))
})

test_that("out[i] <- x[-k] dynamic exclusion in loop body transpiles", {
  f <- function(x, k, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-k]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k = "i32", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dynamic ternary: 1 if (Int(k) - 1) == 0 else 0
  expect_true(grepl("Int\\(k\\) - 1", res$mojo))
})

test_that("out[-k] <- val dynamic write-side exclusion transpiles", {
  f <- function(x, k, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[-k] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k = "i32", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit exclusion write loop with dynamic skip condition
  expect_true(grepl("Int\\(k\\) - 1", res$mojo) || grepl("!=", res$mojo))
})

# --- Transpile tests: N-d scalar negative in loop body ---

test_that("out[i] <- mat[-1, i] N-d negative row index transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[-1, i]
    }
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should have ternary for the excluded row dimension
  expect_true(grepl("1 if.*== 0 else 0", res$mojo))
})

test_that("out[i] <- mat[i, -1] N-d negative col index transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[i, -1]
    }
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should have ternary for the excluded col dimension
  expect_true(grepl("1 if.*== 0 else 0", res$mojo))
})

# --- Transpile tests: vector exclusion ---

test_that("out <- x[-c(1,2)] vector exclusion transpiles", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-c(1, 2)]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit loop with != conditions for excluded elements
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))  # 1-1=0 (0-based)
  expect_true(grepl("!=\\s*\\(?1\\)?", res$mojo))  # 2-1=1 (0-based)
  expect_true(grepl("and", res$mojo))     # Combined condition
})

test_that("out <- x[-(1:3)] range exclusion transpiles", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-(1:3)]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit range skip condition
  expect_true(grepl("<\\s*\\(?0\\)?\\s*or|>=\\s*\\(?3\\)?", res$mojo))
})

test_that("out[-c(1,2)] <- val vector exclusion write transpiles", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[-c(1, 2)] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))
  expect_true(grepl("!=\\s*\\(?1\\)?", res$mojo))
})

test_that("out[i] <- x[-c(1,2)] vector exclusion in loop body transpiles", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-c(1, 2)]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # First non-excluded element: {0,1} excluded → first ok is 2 (0-based)
  # Emitted as _mojor_read_f64(x, Int(2), ...) or x[2]
  expect_true(grepl("Int\\(2\\)|\\[2\\]", res$mojo))
})

# --- Transpile tests: dynamic vector exclusion ---

test_that("out <- x[-c(k1, k2)] dynamic vector exclusion transpiles", {
  f <- function(x, k1, k2) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out <- x[-c(k1, k2)]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k1 = "i32", k2 = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit dynamic exclusion: != (Int(k1) - 1) and != (Int(k2) - 1)
  expect_true(grepl("Int\\(k1\\) - 1", res$mojo))
  expect_true(grepl("Int\\(k2\\) - 1", res$mojo))
  expect_true(grepl("and", res$mojo))
})

test_that("out[-c(k1, k2)] <- val dynamic vector exclusion write transpiles", {
  f <- function(x, k1, k2, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[-c(k1, k2)] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k1 = "i32", k2 = "i32", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("Int\\(k1\\) - 1", res$mojo))
  expect_true(grepl("Int\\(k2\\) - 1", res$mojo))
})

test_that("out <- x[-c(1, k)] mixed literal/dynamic exclusion transpiles", {
  f <- function(x, k) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out <- x[-c(1, k)]
    out
  }

  res <- mojor_transpile(f, x = "f64[]", k = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # First element literal (0), second dynamic
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))
  expect_true(grepl("Int\\(k\\) - 1", res$mojo))
})

# --- Transpile tests: N-d whole-vector exclusion ---

test_that("out <- mat[-1, ] N-d row exclusion transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[1, i]
    }
    out <- mat[-1, ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit nested loops with skip on row dimension
  expect_true(grepl("__mojor_nd_1", res$mojo))  # row loop var
  expect_true(grepl("__mojor_nd_2", res$mojo))  # col loop var
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))  # skip row 0 (0-based)
})

test_that("out <- mat[, -1] N-d col exclusion transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[i, 1]
    }
    out <- mat[, -1]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit nested loops with skip on col dimension
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))  # skip col 0 (0-based)
})

test_that("out <- mat[-3, ] N-d row exclusion with non-first row transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[1, i]
    }
    out <- mat[-3, ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("!=\\s*\\(?2\\)?", res$mojo))  # skip row 2 (0-based for R row 3)
})

# --- Transpile tests: Higher-rank N-d exclusion ---

test_that("out <- arr[-1, , ] 3D row exclusion transpiles", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, 1, i]
    }
    out <- arr[-1, , ]
    out
  }

  res <- mojor_transpile(f, arr = "f64[3d]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("__mojor_nd_3", res$mojo))
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))
})

test_that("out <- arr[-1, , -2] multi-exclusion transpiles", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, 1, i]
    }
    out <- arr[-1, , -2]
    out
  }

  res <- mojor_transpile(f, arr = "f64[3d]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_3", res$mojo))
  # Two skip conditions: dim1 != 0, dim3 != 1
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
  expect_true(grepl("__mojor_nd_3 != \\(1\\)", res$mojo))
})

test_that("out <- mat[-1, -2] all-exclusion transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[1, i]
    }
    out <- mat[-1, -2]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
  expect_true(grepl("__mojor_nd_2 != \\(1\\)", res$mojo))
})

test_that("out[-1, , ] <- scalar 3D write exclusion transpiles", {
  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i]
        }
      }
    }
    out[-1, , ] <- 99.0
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_3", res$mojo))
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))
  expect_true(grepl("99\\.0", res$mojo))
})

test_that("out[-1, , -2] <- scalar multi-exclusion write transpiles", {
  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i]
        }
      }
    }
    out[-1, , -2] <- 99.0
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
  expect_true(grepl("__mojor_nd_3 != \\(1\\)", res$mojo))
  expect_true(grepl("99\\.0", res$mojo))
})

# --- Runtime correctness tests (require Mojo toolchain) ---

test_that("out <- x[-1] runtime: drops first element", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-1]
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # x[-1] in R = c(20, 30, 40, 50)
  expect_equal(result, c(20, 30, 40, 50))
  expect_equal(length(result), 4L)
})

test_that("out <- x[-3] runtime: drops third element", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-3]
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # x[-3] in R = c(10, 20, 40, 50)
  expect_equal(result, c(10, 20, 40, 50))
  expect_equal(length(result), 4L)
})

test_that("out <- x[-5] runtime: drops last element", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out <- x[-5]
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # x[-5] in R = c(10, 20, 30, 40)
  expect_equal(result, c(10, 20, 30, 40))
  expect_equal(length(result), 4L)
})

test_that("out[i] <- x[-1] runtime: fills with first element of x[-1]", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-1]
    }
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # x[-1] in R = c(20,30,40,50), first element = 20
  # All positions filled with 20
  expect_equal(result, rep(20.0, 5))
})

test_that("out[i] <- x[-3] runtime: fills with first element of x[-3]", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[-3]
    }
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # x[-3] in R = c(10,20,40,50), first element = 10
  # All positions filled with 10
  expect_equal(result, rep(10.0, 5))
})

test_that("out[-3] <- scalar runtime: writes to all except 3rd", {
  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out[-3] <- 99.0
    out
  }

  built <- mojor_build(
    f, x = "f64[]", n = "i32",
    cache = FALSE, load = TRUE
  )

  x <- as.double(c(10, 20, 30, 40, 50))
  result <- built$func(x, 5L)
  # out[-3] <- 99 in R: all except 3rd element become 99
  expect_equal(result, c(99, 99, 30, 99, 99))
})

# --- Transpile tests: N-d write-side exclusion ---

test_that("out[-1, ] <- scalar N-d row exclusion write transpiles", {
  f <- function(x, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i]
      }
    }
    out[-1, ] <- 99.0
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should emit nested loops with skip on row dimension
  expect_true(grepl("__mojor_nd_1", res$mojo))  # row loop var
  expect_true(grepl("__mojor_nd_2", res$mojo))  # col loop var
  expect_true(grepl("!=\\s*\\(?0\\)?", res$mojo))  # skip row 0 (0-based)
  expect_true(grepl("99\\.0", res$mojo))
})

test_that("out[, -2] <- scalar N-d col exclusion write transpiles", {
  f <- function(x, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i]
      }
    }
    out[, -2] <- 77.0
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("!=\\s*\\(?1\\)?", res$mojo))  # skip col 1 (0-based for R col 2)
  expect_true(grepl("77\\.0", res$mojo))
})

test_that("positive indices still work correctly", {
  skip_if_no_mojo()

  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(mojor_transpile(f, x = "f64[]", n = "i32"), NA)

  built <- mojor_build(f, x = "f64[]", n = "i32",
                       cache = FALSE, load = TRUE)

  x <- as.double(1:5)
  result <- built$func(x, 5L)
  expect_equal(result, x)
})

# --- Transpile tests: Mixed positive/negative N-d subscripts ---

test_that("out <- arr[-1, 2, ] mixed 3D exclusion+scalar transpiles", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, 1, i]
    }
    out <- arr[-1, 2, ]
    out
  }

  res <- mojor_transpile(f, arr = "f64[3d]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should have loop vars for dim 1 and dim 3 (not dim 2 — it's scalar)
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_3", res$mojo))
  # Dim 2 should NOT get a loop variable, it's a fixed index
  expect_false(grepl("__mojor_nd_2", res$mojo))
  # Skip condition on dim 1 (exclude row 0, 0-based)
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
})

test_that("out <- mat[-1, 2] mixed 2D exclusion+scalar transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[-1, 2]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1 gets a loop with exclusion, dim 2 is scalar
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_false(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
})

test_that("out <- mat[2, -1] mixed 2D scalar+exclusion transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[2, -1]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1 is scalar (no loop), dim 2 gets loop with exclusion
  expect_false(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("__mojor_nd_2 != \\(0\\)", res$mojo))
})

test_that("out <- arr[1, -2, 3] mixed 3D scalar+exclusion+scalar transpiles", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, i, 1]
    }
    out <- arr[1, -2, 3]
    out
  }

  res <- mojor_transpile(f, arr = "f64[3d]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Only dim 2 gets a loop
  expect_false(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_false(grepl("__mojor_nd_3", res$mojo))
  # Skip row 1 (R index 2 → 0-based 1)
  expect_true(grepl("__mojor_nd_2 != \\(1\\)", res$mojo))
})

# --- Write-side mixed subscripts ---

test_that("out[-1, 2] <- val mixed 2D exclusion+scalar write transpiles", {
  f <- function(mat, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    out[-1, 2] <- 99.0
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Should have exclusion loop on dim 1, scalar on dim 2
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_false(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("99\\.0", res$mojo))
})

test_that("out[2, -1] <- val mixed 2D scalar+exclusion write transpiles", {
  f <- function(mat, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    out[2, -1] <- 77.0
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1 scalar (no loop), dim 2 with exclusion
  expect_false(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  expect_true(grepl("77\\.0", res$mojo))
})

# --- Transpile tests: Vector selection in N-d subscripts ---

test_that("out <- mat[c(1,3), ] positive vector selection read transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[c(1,3), ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1: vector selection loop (sel var), Dim 2: full range loop
  expect_true(grepl("__mojor_sel_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
  # Ternary chain for c(1,3) → 0-based {0, 2}
  expect_true(grepl("0 if __mojor_sel_1 == 0 else", res$mojo))
})

test_that("out <- mat[, c(2,4)] column vector selection read transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[, c(2,4)]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1: full range, Dim 2: vector selection
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_sel_2", res$mojo))
})

test_that("out <- mat[1:3, ] range selection read transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[1:3, ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Range selection: for __mojor_nd_1 in range(0, 3)
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("range\\(0, 3\\)", res$mojo))
})

test_that("out <- arr[c(1,3), -2, ] mixed vec_sel + exclusion + missing transpiles", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, 1, i]
    }
    out <- arr[c(1,3), -2, ]
    out
  }

  res <- mojor_transpile(f, arr = "f64[3d]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1: vector selection, Dim 2: exclusion (skip row 1), Dim 3: full range
  expect_true(grepl("__mojor_sel_1", res$mojo))
  expect_true(grepl("__mojor_nd_2 != \\(1\\)", res$mojo))
  expect_true(grepl("__mojor_nd_3", res$mojo))
})

test_that("out <- mat[-c(1,2), ] neg vector exclusion in N-d transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[-c(1,2), ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1: neg vector exclusion (skip 0 and 1), Dim 2: full range
  expect_true(grepl("__mojor_nd_1 != \\(0\\)", res$mojo))
  expect_true(grepl("__mojor_nd_1 != \\(1\\)", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
})

test_that("out <- mat[-(1:3), ] neg range exclusion in N-d transpiles", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out <- mat[-(1:3), ]
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  # Dim 1: neg range exclusion, Dim 2: full range
  expect_true(grepl("__mojor_nd_1", res$mojo))
  expect_true(grepl("__mojor_nd_2", res$mojo))
})

# --- Write-side vector selection ---

test_that("out[c(1,3), ] <- val vector selection write transpiles", {
  f <- function(mat, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    out[c(1,3), ] <- 99.0
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_sel_1", res$mojo))
  expect_true(grepl("99\\.0", res$mojo))
})

test_that("out[c(1,3), -2] <- val mixed vec_sel + exclusion write transpiles", {
  f <- function(mat, n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    out[c(1,3), -2] <- 77.0
    out
  }

  res <- mojor_transpile(f, mat = "f64[,]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_true(grepl("__mojor_sel_1", res$mojo))
  expect_true(grepl("__mojor_nd_2 != \\(1\\)", res$mojo))
  expect_true(grepl("77\\.0", res$mojo))
})
