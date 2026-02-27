# Test matrix multiplication transpilation (PR-B6.1)

test_that("matmul operation is detected", {  f <- function(A, B) A %*% B
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$operation, "matmul")
})

test_that("crossprod(A) is detected", {  f <- function(A) crossprod(A)
  trans <- mojor_transpile(f, A = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$operation, "crossprod")
})

test_that("crossprod(A, B) is detected", {  f <- function(A, B) crossprod(A, B)
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$operation, "crossprod")
})

test_that("tcrossprod(A) is detected", {  f <- function(A) tcrossprod(A)
  trans <- mojor_transpile(f, A = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$operation, "tcrossprod")
})

test_that("tcrossprod(A, B) is detected", {  f <- function(A, B) tcrossprod(A, B)
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$operation, "tcrossprod")
})

test_that("matmul generates valid Mojo code", {  f <- function(A, B) A %*% B
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  # Check for key matmul components
  expect_true(grepl("matrix operation", trans$mojo))
  expect_true(grepl("PR-B6", trans$mojo))

  # Check for the triple nested loop structure
  expect_true(grepl("for i in range\\(m\\)", trans$mojo))
  expect_true(grepl("for j in range\\(p\\)", trans$mojo))
  expect_true(grepl("for k in range\\(n\\)", trans$mojo))

  # Check for matrix multiplication logic
  expect_true(grepl("sum \\+= a_val \\* b_val", trans$mojo))
})

test_that("crossprod(A) generates valid Mojo code", {  f <- function(A) crossprod(A)
  trans <- mojor_transpile(f, A = "f64[,]")

  # Check for crossprod-specific indexing pattern (column-major)
  expect_true(grepl("A\\[k \\+ i \\* m_dim\\]", trans$mojo))
  expect_true(grepl("A\\[k \\+ j \\* m_dim\\]", trans$mojo))

  # Check for loop structure
  expect_true(grepl("for i in range\\(p\\)", trans$mojo))
  expect_true(grepl("for j in range\\(p\\)", trans$mojo))
  expect_true(grepl("for k in range\\(m_dim\\)", trans$mojo))
})

test_that("crossprod(A, B) generates valid Mojo code", {  f <- function(A, B) crossprod(A, B)
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  # Check for crossprod-specific indexing pattern (column-major)
  expect_true(grepl("A\\[k \\+ i \\* m_dim\\]", trans$mojo))
  expect_true(grepl("B\\[k \\+ j \\* m_dim\\]", trans$mojo))

  # Check for loop structure (crossprod(A,B) uses output dimensions)
  expect_true(grepl("for i in range\\(m\\)", trans$mojo))
  expect_true(grepl("for j in range\\(p\\)", trans$mojo))
  expect_true(grepl("for k in range\\(m_dim\\)", trans$mojo))
})

test_that("tcrossprod(A) generates valid Mojo code", {  f <- function(A) tcrossprod(A)
  trans <- mojor_transpile(f, A = "f64[,]")

  # Check for tcrossprod-specific indexing pattern (column-major)
  expect_true(grepl("A\\[i \\+ k \\* m_dim\\]", trans$mojo))
  expect_true(grepl("A\\[j \\+ k \\* m_dim\\]", trans$mojo))

  # Check for loop structure
  expect_true(grepl("for i in range\\(m\\)", trans$mojo))
  expect_true(grepl("for j in range\\(m\\)", trans$mojo))
  expect_true(grepl("for k in range\\(n\\)", trans$mojo))
})

test_that("tcrossprod(A, B) generates valid Mojo code", {  f <- function(A, B) tcrossprod(A, B)
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  # Check for tcrossprod-specific indexing pattern (column-major)
  expect_true(grepl("A\\[i \\+ k \\* m_dim\\]", trans$mojo))
  expect_true(grepl("B\\[j \\+ k \\* p_dim\\]", trans$mojo))

  # Check for loop structure
  expect_true(grepl("for i in range\\(m\\)", trans$mojo))
  expect_true(grepl("for j in range\\(p\\)", trans$mojo))
  expect_true(grepl("for k in range\\(n\\)", trans$mojo))
})

test_that("matrix operations use correct indexing patterns", {  f <- function(A, B) A %*% B
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  # Check for proper column-major indexing patterns (i + j * dim)
  # For matmul, we expect patterns like A[i + k * m] and B[k + j * n]
  expect_true(grepl("\\[.*\\+.*\\*.*\\]", trans$mojo))
})

test_that("matrix operations include dimension variables", {  f <- function(A, B) A %*% B
  trans <- mojor_transpile(f, A = "f64[,]", B = "f64[,]")

  # Check for dimension extraction
  expect_true(grepl("var m =", trans$mojo))
  expect_true(grepl("var n =", trans$mojo))
  expect_true(grepl("var p =", trans$mojo))
})
