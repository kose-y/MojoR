library(testthat)

# Tests for IR missing index handling
#
# These tests verify that the IR builder correctly handles missing indices
# in subscript expressions (e.g., mat[i, ], mat[, j]) without triggering
# R's "argument is missing" errors.

# =============================================================================
# IR Building Tests - Missing Index Detection
# =============================================================================

test_that("IR builder handles missing column index", {  # Expression: mat[i, ]
  expr <- quote(mat[i, ])

  # Build subscript IR node
  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 2)

  # First index should be scalar_index wrapping i
  expect_equal(ir$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$indices[[1]]$expr$name, "i")

  # Second index should be missing_index
  expect_equal(ir$indices[[2]]$kind, "missing_index")
})

test_that("IR builder handles missing row index", {  # Expression: mat[, j]
  expr <- quote(mat[, j])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 2)

  # First index should be missing_index
  expect_equal(ir$indices[[1]]$kind, "missing_index")

  # Second index should be scalar_index wrapping j
  expect_equal(ir$indices[[2]]$kind, "scalar_index")
  expect_equal(ir$indices[[2]]$expr$name, "j")
})

test_that("IR builder handles both indices missing", {  # Expression: mat[, ]
  expr <- quote(mat[, ])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 2)

  # Both indices should be missing_index
  expect_equal(ir$indices[[1]]$kind, "missing_index")
  expect_equal(ir$indices[[2]]$kind, "missing_index")
})

test_that("IR builder handles normal (non-missing) 2D index", {  # Expression: mat[i, j]
  expr <- quote(mat[i, j])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 2)

  # Both indices should be scalar_index wrappers
  expect_equal(ir$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$indices[[1]]$expr$name, "i")
  expect_equal(ir$indices[[2]]$kind, "scalar_index")
  expect_equal(ir$indices[[2]]$expr$name, "j")
})

test_that("IR builder handles missing index with named args", {  # Expression: mat[i, , drop = FALSE]
  expr <- quote(mat[i, , drop = FALSE])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")

  # Should have 2 indices (drop is filtered out as named arg)
  expect_equal(length(ir$indices), 2)
  expect_equal(ir$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$indices[[2]]$kind, "missing_index")
})

test_that("IR builder keeps named index args", {  # Expression: mat[rows = i, cols = j]
  expr <- quote(mat[rows = i, cols = j])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 2)
  expect_equal(ir$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$indices[[1]]$expr$name, "i")
  expect_equal(ir$indices[[2]]$kind, "scalar_index")
  expect_equal(ir$indices[[2]]$expr$name, "j")
})

# =============================================================================
# IR Statement Building Tests
# =============================================================================

test_that("IR builds assignment with missing index on LHS", {  # Statement: mat[i, ] <- rhs
  stmt <- quote(mat[i, ] <- c(x, y))

  ir <- .mojor_ir_build_stmt(stmt)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(length(ir$lhs$indices), 2)

  # First index is scalar_index(i), second is missing
  expect_equal(ir$lhs$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$lhs$indices[[2]]$kind, "missing_index")
})

test_that("IR builds assignment with named index args on LHS", {  # Statement: mat[rows = i, cols = j] <- rhs
  stmt <- quote(mat[rows = i, cols = j] <- x)

  ir <- .mojor_ir_build_stmt(stmt)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(length(ir$lhs$indices), 2)
  expect_equal(ir$lhs$indices[[1]]$expr$name, "i")
  expect_equal(ir$lhs$indices[[2]]$expr$name, "j")
})

test_that("IR builds assignment with missing index and c() constructor", {  # Statement: mat[i, ] <- c(a[i], b[i])
  stmt <- quote(mat[i, ] <- c(a[i], b[i]))

  ir <- .mojor_ir_build_stmt(stmt)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "assign")

  # LHS should be subscript with missing index
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(ir$lhs$indices[[2]]$kind, "missing_index")

  # RHS should be c() constructor
  expect_equal(ir$rhs$kind, "c")
  expect_equal(length(ir$rhs$parts), 2)
})

# =============================================================================
# Integration Tests - Full Transpilation
# =============================================================================

test_that("missing index in full function transpiles without error", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  # Should transpile without "argument is missing" error
  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
})

test_that("missing row index transpiles", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in 1:n) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
})

test_that("missing index with complex RHS transpiles", {  f <- function(x, y, z, n) {
    mat <- matrix(0, n, 3)
    for (i in 1:n) {
      mat[i, ] <- c(x[i] + y[i], y[i] * z[i], z[i] - x[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", z = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("IR builder handles single missing index (vector case)", {  # Expression: vec[] (which is valid R but unusual)
  expr <- quote(vec[])

  ir <- .mojor_ir_build_subscript(expr)

  # Should build successfully
  expect_true(!is.null(ir))
  expect_equal(ir$kind, "subscript")
  expect_equal(length(ir$indices), 1)
  expect_equal(ir$indices[[1]]$kind, "missing_index")
})

test_that("IR builder preserves index order with missing indices", {  # Expression: arr[i, , k]
  expr <- quote(arr[i, , k])

  ir <- .mojor_ir_build_subscript(expr)

  expect_true(!is.null(ir))
  expect_equal(length(ir$indices), 3)

  # First index: scalar_index(i)
  expect_equal(ir$indices[[1]]$kind, "scalar_index")
  expect_equal(ir$indices[[1]]$expr$name, "i")

  # Second index: missing
  expect_equal(ir$indices[[2]]$kind, "missing_index")

  # Third index: scalar_index(k)
  expect_equal(ir$indices[[3]]$kind, "scalar_index")
  expect_equal(ir$indices[[3]]$expr$name, "k")
})

# =============================================================================
# Boundary Condition Tests (c() constructor)
# =============================================================================

test_that("c() constructor IR node has correct part count", {  # Expression: c(x, y)
  expr <- quote(c(x, y))

  ir <- .mojor_ir_expr_build(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "c")
  expect_equal(length(ir$parts), 2)
})

test_that("c() constructor with three parts", {  # Expression: c(a, b, c)
  expr <- quote(c(a, b, c))

  ir <- .mojor_ir_expr_build(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "c")
  expect_equal(length(ir$parts), 3)
})

test_that("c() constructor with scalar and indexed elements", {  # Expression: c(42.0, x[i])
  expr <- quote(c(42.0, x[i]))

  ir <- .mojor_ir_expr_build(expr)

  expect_true(!is.null(ir))
  expect_equal(ir$kind, "c")
  expect_equal(length(ir$parts), 2)

  # First part is constant
  expect_equal(ir$parts[[1]]$kind, "const")
  expect_equal(ir$parts[[1]]$value, "42.0")

  # Second part is index
  expect_equal(ir$parts[[2]]$kind, "index")
})
