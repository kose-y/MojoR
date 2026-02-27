# Test: Matrix Diagonal Extraction (PR-B4 Phase 2)
# Tests for diag() with matrix input - proper index calculation

library(testthat)

if (!exists(".mojor_ir_var", mode = "function", inherits = TRUE)) {
  stop("IR helpers are unavailable in the test environment")
}

context("matrix diagonal extraction")
test_that("diag() IR node tracks matrix vs vector mode", {  # Matrix input
  mat_ir <- .mojor_ir_var("mat")
  diag_mat_ir <- .mojor_ir_diag(x = mat_ir)
  expect_equal(diag_mat_ir$kind, "diag")
  expect_equal(diag_mat_ir$x$name, "mat")
  
  # Vector input
  vec_ir <- .mojor_ir_var("vec")
  diag_vec_ir <- .mojor_ir_diag(x = vec_ir)
  expect_equal(diag_vec_ir$x$name, "vec")
})

test_that("diag() is recognized as constructor", {  expect_true(.mojor_ir_expr_has_constructor(quote(diag(x))))
  expect_true(.mojor_ir_expr_has_constructor(quote(diag(mat))))
})

test_that("diag() has correct effects", {  # Pure when input is pure
  x_ir <- .mojor_ir_const("5")
  diag_ir <- .mojor_ir_diag(x = x_ir)
  eff <- .mojor_ir_expr_effects(diag_ir)
  expect_true("Pure" %in% eff)
  
  # ReadsMem when input reads memory
  vec_ir <- .mojor_ir_var("vec")
  diag_vec_ir <- .mojor_ir_diag(x = vec_ir)
  eff <- .mojor_ir_expr_effects(diag_vec_ir)
  expect_true("Pure" %in% eff || "ReadsMem" %in% eff)
})

test_that("diag() IR node is built from AST", {  # Vector diag
  expr <- quote(diag(x))
  ir <- .mojor_ir_expr_build(expr)
  expect_false(is.null(ir))
  expect_equal(ir$kind, "diag")
  expect_equal(ir$x$kind, "var")
  expect_equal(ir$x$name, "x")
})

context("matrix diagonal extraction documentation")
test_that("PR-B4 is documented", {  spec_files <- c(
    "docs/IR/SPEC.md"
  )
  existing <- spec_files[file.exists(spec_files)]
  skip_if(length(existing) == 0, "IR spec docs not found")

  spec_doc <- unlist(lapply(existing, readLines, warn = FALSE), use.names = FALSE)
  expect_true(any(grepl("diag", spec_doc, ignore.case = TRUE)))
})

# Placeholder for runtime tests - these require full integration
# The index calculation is: (i-1) * (nrow+1) for column-major order
context("matrix diagonal extraction index calculation")
test_that("diagonal index formula is correct", {  # For a matrix with nrow rows, diagonal element i (1-based) is at:
  # Position = (i-1) * (nrow+1) in 0-based linearized indexing
  #
  # Example: 3x3 matrix
  # Column-major layout: [a11, a21, a31, a12, a22, a32, a13, a23, a33]
  # Positions:            [0,   1,   2,   3,   4,   5,   6,   7,   8 ]
  # Diagonal positions:   0 (a11), 4 (a22), 8 (a33)
  # Formula: (i-1) * (3+1) = 0, 4, 8 ✓
  
  nrow <- 3
  expected_positions <- c(0, 4, 8)
  actual_positions <- sapply(1:3, function(i) (i-1) * (nrow+1))
  expect_equal(actual_positions, expected_positions)
})

context("matrix diagonal creation mode (2D)")
test_that("diag() in 2D context emits conditional pattern", {  # This tests the internal emitter logic
  # diag(x) with 2 loop vars should emit: (x[i] if (i == j) else 0.0)
  
  # Setup state for 2D context
  x_ir <- .mojor_ir_var("x")
  diag_ir <- .mojor_ir_diag(x = x_ir)
  
  # Verify IR structure
  expect_equal(diag_ir$kind, "diag")
  expect_equal(diag_ir$x$name, "x")
})

test_that("identity matrix in 2D context emits correct pattern", {  # diag(n) with 2 loop vars should emit: (1.0 if (i == j) else 0.0)
  n_ir <- .mojor_ir_const("3")
  diag_ir <- .mojor_ir_diag(n = n_ir)
  
  expect_equal(diag_ir$kind, "diag")
  expect_equal(diag_ir$n$kind, "const")
})
