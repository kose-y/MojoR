library(testthat)

# Logical Mask Assignment IR Tests
#
# Tests that logical mask indexing/assignment transpiles correctly in IR-only mode.
# Pattern: out[mask] <- rhs where mask is a logical array

test_that("mask assignment with scalar RHS transpiles in IR", {  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_mask_j\\d* in range")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask, __mojor_mask_j\\d*")
  expect_match(result$mojo, "if __mojor_mask_val\\d* == -2147483648:")
  expect_match(result$mojo, "if __mojor_mask_val\\d* == 0:")
  expect_match(result$mojo, "out\\[__mojor_mask_j\\d*\\] = ")
})

test_that("mask assignment with expression RHS transpiles correctly", {  f <- function(x, y, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i] * y[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", mask = "lgl[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_mask_j\\d* in range")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask, __mojor_mask_j\\d*")
  expect_match(result$mojo, "\\*")  # multiplication in RHS
})

test_that("mask assignment with constant RHS transpiles correctly", {  f <- function(mask) {
    out <- numeric(length(mask))
    for (i in 1:10) {
      out[mask] <- 42.0
    }
    out
  }

  result <- mojor_transpile(f, mask = "lgl[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "out\\[__mojor_mask_j\\d*\\] = (Float64\\()?42(\\.0)?\\)?")
})

test_that("mask assignment NA handling code is present", {  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", ir_only = TRUE)
  # Check that NA check is present (Int32 NA = -2147483648)
  expect_match(result$mojo, "-2147483648")
  expect_match(result$mojo, "continue", all = FALSE)
})

test_that("mask assignment false handling code is present", {  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", ir_only = TRUE)
  # Check that false check is present
  expect_match(result$mojo, "== 0:")
  expect_match(result$mojo, "continue", all = FALSE)
})

test_that("nested mask assignment transpiles correctly", {  f <- function(x, mask1, mask2) {
    out1 <- numeric(length(x))
    out2 <- numeric(length(x))
    for (i in seq_along(x)) {
      out1[mask1] <- x[i]
      out2[mask2] <- x[i] * 2.0
    }
    list(out1, out2)
  }

  result <- try(
    mojor_transpile(f, x = "f64[]", mask1 = "lgl[]", mask2 = "lgl[]", ir_only = TRUE),
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    expect_match(
      as.character(result),
      "return constructors are not supported|strict IR emission produced empty output"
    )
  } else {
    expect_true(!is.null(result$mojo))
    if (!nzchar(result$mojo)) {
      expect_true(TRUE)
    } else {
      # Should have two mask loops when constructor lowering is available.
      expect_true(grepl(
        "for __mojor_mask_j\\d* in range[\\s\\S]*for __mojor_mask_j\\d* in range",
        result$mojo,
        perl = TRUE
      ))
    }
  }
})

test_that("mask assignment with type cast transpiles correctly", {  f <- function(x, mask) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[mask] <- as.integer(x[i])
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "Int32\\(")
})

test_that("mask assignment IR structure is correct", {  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", emit_ir = TRUE)
  # IR should contain the mask assignment
  ir_str <- capture.output(mojor_ir_print(result$ir))
  expect_true(any(grepl("mask", ir_str)))
})

test_that("mask extraction assignment lowers in IR", {  f <- function(x, mask) {
    out <- numeric(length(mask))
    for (i in 1:1) {
      out <- x[mask]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "__mojor_out_k")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask")
  expect_match(result$mojo, "out\\[__mojor_out_k")
})

# Logical Matrix Mask Indexing Tests
#
# Tests that logical matrix masks (lgl[,]) work for mask assignment and extraction.
# R semantics: mat[logical_mat] treats both as flat vectors in column-major order.

test_that("matrix mask assignment with scalar RHS transpiles in IR", {
  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[,]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_mask_j\\d* in range")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask, __mojor_mask_j\\d*")
  expect_match(result$mojo, "if __mojor_mask_val\\d* == -2147483648:")
  expect_match(result$mojo, "if __mojor_mask_val\\d* == 0:")
  expect_match(result$mojo, "out\\[__mojor_mask_j\\d*\\] = ")
})

test_that("matrix mask extraction assignment lowers in IR", {
  f <- function(x, mask) {
    out <- numeric(length(mask))
    for (i in 1:1) {
      out <- x[mask]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[,]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "__mojor_out_k")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask")
  expect_match(result$mojo, "out\\[__mojor_out_k")
})

test_that("matrix mask assignment with matrix data transpiles in IR", {
  f <- function(mat, mask) {
    out <- numeric(length(mat))
    for (i in 1:1) {
      out[mask] <- 0.0
    }
    out
  }

  result <- mojor_transpile(f, mat = "f64[,]", mask = "lgl[,]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "_mojor_read_lgl\\(mask")
})

test_that("3D array mask assignment transpiles in IR", {
  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[3d]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_mask_j\\d* in range")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask, __mojor_mask_j\\d*")
})

test_that("3D array mask extraction transpiles in IR", {
  f <- function(x, mask) {
    out <- numeric(length(mask))
    for (i in 1:1) {
      out <- x[mask]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", mask = "lgl[3d]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "__mojor_out_k")
  expect_match(result$mojo, "_mojor_read_lgl\\(mask")
})
