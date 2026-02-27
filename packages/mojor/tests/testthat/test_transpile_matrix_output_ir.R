library(testthat)

# Matrix Output IR Transpilation Tests
#
# Tests that matrix output assignments transpile correctly in IR-only mode.
# Covers both 1D and 2D indexing patterns with LayoutTensor wrapper emission.

test_that("matrix output 1D indexing transpiles with LayoutTensor wrapper", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:10) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check LayoutTensor wrapper emission
  expect_mojor_any_match(result$mojo, "var out_layout = RuntimeLayout\\[(2|_MOJOR_MATRIX_LAYOUT)\\]\\.col_major")
  expect_mojor_any_match(result$mojo, "var out_tensor = LayoutTensor\\[mut=True, DType\\.float64, (2|_MOJOR_MATRIX_LAYOUT), MutAnyOrigin\\]")

  # Check indexing uses tensor
  expect_match(result$mojo, "out\\[Int\\(")
})

test_that("matrix output 2D indexing transpiles with tensor subscripting", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i] + x[j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check LayoutTensor wrapper emission
  expect_mojor_any_match(result$mojo, "var out_layout = RuntimeLayout\\[(2|_MOJOR_MATRIX_LAYOUT)\\]\\.col_major")
  expect_mojor_any_match(result$mojo, "var out_tensor = LayoutTensor")

  # Check 2D tensor indexing is used (not plain array indexing)
  expect_match(result$mojo, "out_tensor\\[.*,.*\\]")
})

test_that("matrix output 2D indexing with scalar RHS", {  f <- function(n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- 42.0
      }
    }
    out
  }

  result <- mojor_transpile(f, n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "= 42")
})

test_that("matrix output 2D indexing with expression RHS", {  f <- function(x, y, n) {
    out <- matrix(0, n, n)
    for (i in 1:5) {
      for (j in 1:5) {
        out[i, j] <- x[i] * y[j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "\\*")
})

test_that("matrix output with nested loops and multiple assignments", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i]
        out[j, i] <- x[j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have multiple tensor assignments
  tensor_assigns <- gregexpr("out_tensor\\[", result$mojo)[[1]]
  expect_true(length(tensor_assigns) >= 2)
})

test_that("matrix output with conditional assignment", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:5) {
      for (j in 1:5) {
        if (i <= j) {
          out[i, j] <- x[i]
        }
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "if ")
})

test_that("matrix output with arithmetic on indices", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i + j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "\\+")
})

test_that("matrix output LayoutTensor includes nrow/ncol vars", {  f <- function(x, nrow_val, ncol_val) {
    out <- matrix(0, nrow_val, ncol_val)
    for (i in 1:5) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", nrow_val = "i32", ncol_val = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check that dimension vars appear in layout construction
  # Note: vars get converted to *_i form (e.g., nrow_out_i, ncol_out_i)
  expect_match(result$mojo, "IndexList\\[2\\]\\(Int\\([^)]+\\), Int\\([^)]+\\)\\)")
})

test_that("matrix output with single element assignment", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    out[2, 2] <- x[1]
    out
  }

  res <- try(mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    expect_match(as.character(res), "only supports|strict IR emission produced empty output")
  } else {
    expect_true(!is.null(res$mojo))
  }
})

test_that("matrix output with different dtypes - i32", {  f <- function(x, n) {
    out <- matrix(0L, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- as.integer(x[i])
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check dtype is int32
  expect_match(result$mojo, "DType\\.int32")
  expect_match(result$mojo, "out_tensor\\[")
})

test_that("matrix output 1D and 2D indexing can coexist", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:5) {
      out[i] <- x[i]
    }
    for (i in 1:2) {
      for (j in 1:2) {
        out[i, j] <- x[i] + x[j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have both 1D and 2D indexing
  expect_match(result$mojo, "out\\[Int\\(")
  expect_match(result$mojo, "out_tensor\\[")
})

test_that("matrix output with complex expression RHS", {  f <- function(x, y, z, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i] + y[j] * z[i + j]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", z = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "\\+")
  expect_match(result$mojo, "\\*")
})

test_that("matrix output IR structure contains tensor metadata", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", emit_ir = TRUE, ir_only = TRUE)
  expect_true(!is.null(result$ir))

  # Check IR structure (not a deep validation, just basic sanity)
  ir_str <- capture.output(mojor_ir_print(result$ir))
  expect_true(length(ir_str) > 0)
})

test_that("matrix output with type cast in assignment", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- as.double(i + j)
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "Float64\\(")
})

test_that("matrix output with math function in RHS", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- sin(x[i]) + cos(x[j])
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
  expect_match(result$mojo, "sin")
  expect_match(result$mojo, "cos")
})

test_that("matrix output assigns to diagonal", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:5) {
      out[i, i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
})

test_that("matrix output with ifelse in RHS", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- ifelse(i == j, x[i], 0.0)
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out_tensor\\[")
})

test_that("matrix output LayoutTensor import is present", {  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in 1:3) {
      for (j in 1:3) {
        out[i, j] <- x[i]
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check for LayoutTensor-related imports
  expect_match(result$mojo, "from layout import")
})
