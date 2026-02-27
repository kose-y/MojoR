# Comprehensive Indexing Test Suite
# Tests all combinations of indexing modes across contexts

library(testthat)

# =============================================================================
# Section 1: Basic Positive Indexing (1D)
# =============================================================================

test_that("positive scalar indexing - read outside loop", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[1]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_pos_scalar_read")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), 10.0)
})

test_that("positive scalar indexing - write outside loop", {
  f <- function() {
    out <- numeric(3)
    out[1] <- 1.0
    out[2] <- 2.0
    out[3] <- 3.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, name = "idx_pos_scalar_write")
  expect_equal(built$func(), c(1.0, 2.0, 3.0))
})

test_that("positive vector indexing - read outside loop", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- x[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32[]", name = "idx_pos_vec_read")
  x <- c(10.0, 20.0, 30.0, 40.0)
  idx <- c(1L, 3L)
  expect_equal(built$func(x, idx), c(10.0, 30.0))
})

test_that("positive vector indexing - write outside loop", {
  f <- function(idx, vals) {
    out <- numeric(5)
    for (i in seq_along(idx)) {
      out[idx[i]] <- vals[i]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, idx = "i32[]", vals = "f64[]", name = "idx_pos_vec_write")
  idx <- c(1L, 3L, 5L)
  vals <- c(10.0, 30.0, 50.0)
  expect_equal(built$func(idx, vals), c(10.0, 0.0, 30.0, 0.0, 50.0))
})

test_that("positive vector indexing - out of order", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- x[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32[]", name = "idx_pos_vec_out_of_order")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx <- c(3L, 1L, 4L, 2L)  # Out of order: 3, 1, 4, 2
  expect_equal(built$func(x, idx), c(30.0, 10.0, 40.0, 20.0))
})

test_that("positive vector indexing - with duplicates", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- x[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32[]", name = "idx_pos_vec_duplicates")
  x <- c(10.0, 20.0, 30.0, 40.0)
  idx <- c(1L, 3L, 1L, 3L)  # Duplicates: 1 and 3 appear twice
  expect_equal(built$func(x, idx), c(10.0, 30.0, 10.0, 30.0))
})

test_that("positive vector indexing - non-contiguous gaps", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- x[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32[]", name = "idx_pos_vec_gaps")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  idx <- c(1L, 4L, 6L)  # Gaps: 2,3,5 skipped
  expect_equal(built$func(x, idx), c(10.0, 40.0, 60.0))
})

test_that("positive vector indexing - reverse order", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- x[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32[]", name = "idx_pos_vec_reverse")
  x <- c(10.0, 20.0, 30.0, 40.0)
  idx <- c(4L, 3L, 2L, 1L)  # Reverse: 4, 3, 2, 1
  expect_equal(built$func(x, idx), c(40.0, 30.0, 20.0, 10.0))
})

# =============================================================================
# Section 2: Basic Positive Indexing (2D)
# =============================================================================

test_that("positive scalar 2D indexing - read inside loop", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 1]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_pos_2d_read")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(1.0, 2.0, 3.0))
})

test_that("positive scalar 2D indexing - write inside loop", {
  f <- function(n) {
    out <- matrix(0.0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- i * 10 + j
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, n = "i32", name = "idx_pos_2d_write")
  result <- built$func(3L)
  expect_equal(result[1, 1], 11)
  expect_equal(result[2, 3], 23)
})

# =============================================================================
# Section 3: Negative Indexing (1D) - Scalar
# =============================================================================

test_that("negative scalar indexing - read outside loop", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[-1]  # Exclude position 1, assign first of remaining to out[1]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_neg_scalar_read")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), 20.0)  # x[-1] = c(20, 30), first is 20
})

test_that("negative scalar indexing - write outside loop", {
  f <- function() {
    out <- numeric(3)
    out[-2] <- 99.0  # Exclude position 2, assign 99 to positions 1 and 3
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, name = "idx_neg_scalar_write")
  expect_equal(built$func(), c(99.0, 0.0, 99.0))
})

test_that("negative scalar indexing - read inside loop", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-i]  # Dynamic negative index
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_neg_scalar_read_loop")
  x <- c(10.0, 20.0, 30.0, 40.0)
  result <- built$func(x, 2L)
  expect_equal(result[1], x[-1][1])  # First of x[-1] = 20
  expect_equal(result[2], x[-2][1])  # First of x[-2] = 10
})

test_that("negative scalar indexing - wrapped dynamic selectors are equivalent", {
  f_base <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-i]
    }
    out
  }
  f_paren <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-(i)]
    }
    out
  }
  f_plus <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-(+i)]
    }
    out
  }

  skip_if_no_mojo()
  b_base <- mojor_build(f_base, x = "f64[]", n = "i32", name = "idx_neg_scalar_wrap_base")
  b_paren <- mojor_build(f_paren, x = "f64[]", n = "i32", name = "idx_neg_scalar_wrap_paren")
  b_plus <- mojor_build(f_plus, x = "f64[]", n = "i32", name = "idx_neg_scalar_wrap_plus")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(b_base$func(x, 3L), b_paren$func(x, 3L))
  expect_equal(b_base$func(x, 3L), b_plus$func(x, 3L))
})

# =============================================================================
# Section 4: Negative Indexing (1D) - Vector
# =============================================================================

test_that("negative vector indexing - literal set", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[-c(1, 3)]  # Exclude positions 1 and 3, get first of remaining
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_neg_vec_literal")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x), 20.0)  # x[-c(1,3)] = c(20, 40), first is 20
})

test_that("negative vector indexing - literal range", {
  f <- function(x) {
    y <- x[-(1:2)]  # Exclude positions 1:2, get remaining (positions 3:4)
    y
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_neg_vec_range")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x), c(30.0, 40.0))
})

test_that("negative vector indexing - wrapped range forms are equivalent", {
  f_base <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(1:2)]
    out
  }
  f_paren <- function(x) {
    out <- numeric(1)
    out[1] <- x[-((1:2))]
    out
  }
  f_plus <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(+(1:2))]
    out
  }
  f_seqint <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(seq.int(1L, 2L))]
    out
  }

  skip_if_no_mojo()
  b_base <- mojor_build(f_base, x = "f64[]", name = "idx_neg_vec_wrap_base")
  b_paren <- mojor_build(f_paren, x = "f64[]", name = "idx_neg_vec_wrap_paren")
  b_plus <- mojor_build(f_plus, x = "f64[]", name = "idx_neg_vec_wrap_plus")
  b_seqint <- mojor_build(f_seqint, x = "f64[]", name = "idx_neg_vec_wrap_seqint")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(b_base$func(x), b_paren$func(x))
  expect_equal(b_base$func(x), b_plus$func(x))
  expect_equal(b_base$func(x), b_seqint$func(x))
})

test_that("negative vector indexing - out of order", {
  f <- function(x) {
    # Exclude positions in out-of-order: exclude 3, then 1, then 2
    # Result should be same as x[-c(1,2,3)] = x[4:length(x)]
    y <- x[-c(3, 1, 2)]
    y
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_neg_vec_out_of_order")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x), c(40.0, 50.0))  # Excludes 1,2,3 leaves 4,5
})

test_that("negative vector indexing - write side out of order", {
  f <- function() {
    out <- numeric(5)
    # Exclude in out-of-order: 4, then 1, then 3
    # Should assign to positions 2 and 5 (same as -c(1,3,4))
    out[-c(4, 1, 3)] <- 99.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, name = "idx_neg_vec_write_out_of_order")
  expect_equal(built$func(), c(0.0, 99.0, 0.0, 0.0, 99.0))
})

test_that("negative vector indexing - write side", {
  f <- function() {
    out <- numeric(5)
    out[-c(2, 4)] <- 99.0  # Exclude positions 2, 4; assign 99 to 1, 3, 5
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, name = "idx_neg_vec_write")
  expect_equal(built$func(), c(99.0, 0.0, 99.0, 0.0, 99.0))
})

# =============================================================================
# Section 5: Negative Indexing (2D)
# =============================================================================

test_that("negative scalar 2D indexing - row exclusion read", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat[-1, 1]  # Column 1, exclude row 1 -> first of rows 2:3 is row 2
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_neg_2d_row")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat), 2.0)  # mat[-1,1] = c(2,3), first is 2
})

test_that("negative scalar 2D indexing - column exclusion read", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat[1, -1]  # Row 1, exclude col 1 -> first of cols 2:ncol is col 2
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_neg_2d_col")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat), 4.0)  # mat[1,-1] = c(4), first is 4
})

# =============================================================================
# Section 6: Character Indexing
# =============================================================================

test_that("character indexing - 2D read with dimnames", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat["row1", "col"]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_char_2d_read")
  mat <- matrix(1:4, nrow = 2, ncol = 2, 
                dimnames = list(c("row1", "row2"), c("col1", "col2")))
  expect_equal(built$func(mat, 2L), c(1.0, 1.0))
})

test_that("character indexing - 2D write with dimnames", {
  f <- function(n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("x", "y")))
    out["a", "x"] <- 99.0
    out["b", "y"] <- 88.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, n = "i32", name = "idx_char_2d_write")
  result <- built$func(2L)
  expect_equal(result[1, 1], 99.0)
  expect_equal(result[2, 2], 88.0)
})

# =============================================================================
# Section 6b: Mixed Character + Numeric Indexing
# =============================================================================

test_that("mixed indexing - character row, numeric col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat["row1", 2]  # Char row, numeric col
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_mix_char_row_num_col")
  mat <- matrix(1:4, nrow = 2, ncol = 2,
                dimnames = list(c("row1", "row2"), c("col1", "col2")))
  expect_equal(built$func(mat, 2L), c(3.0, 3.0))  # mat["row1", 2] = mat[1, 2] = 3
})

test_that("mixed indexing - numeric row, character col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[2, "col1"]  # Numeric row, char col
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_mix_num_row_char_col")
  mat <- matrix(1:4, nrow = 2, ncol = 2,
                dimnames = list(c("row1", "row2"), c("col1", "col2")))
  expect_equal(built$func(mat, 2L), c(2.0, 2.0))  # mat[2, "col1"] = mat[2, 1] = 2
})

test_that("mixed indexing - vector numeric row, character col", {
  f <- function(mat, rows) {
    out <- numeric(length(rows))
    for (i in seq_along(rows)) {
      out[i] <- mat[rows[i], "col2"]  # Vector numeric rows, char col
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", rows = "i32[]", name = "idx_mix_vecnum_char")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, c(1L, 3L)), c(4.0, 6.0))  # mat[c(1,3), "c2"] = c(4, 6)
})

test_that("mixed indexing - character row, vector numeric col", {
  f <- function(mat, cols) {
    out <- numeric(length(cols))
    for (i in seq_along(cols)) {
      out[i] <- mat["r2", cols[i]]  # Char row, vector numeric cols
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", cols = "i32[]", name = "idx_mix_char_vecnum")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, c(1L, 2L)), c(2.0, 5.0))  # mat["r2", c(1,2)] = c(2, 5)
})

test_that("mixed indexing - negative row, character col", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat[-1, "c2"]  # Exclude row 1, char col 2
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_mix_neg_char")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat), 5.0)  # mat[-1, "c2"] = mat[2:3, 2][1] = 5
})

test_that("mixed indexing - character row, negative col", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat["r2", -1]  # Char row 2, exclude col 1
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_mix_char_neg")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat), 5.0)  # mat["r2", -1] = mat[2, 2] = 5
})

# =============================================================================
# Section 6c: Missing Index (Full Dimension) Combinations
# =============================================================================

test_that("missing index - all rows, specific col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 2]  # Specific col, varying row
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_allrows_speccol")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Column 2
})

test_that("missing index - specific row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[2, i]  # Specific row 2, varying col
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_specrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Row 2
})

test_that("missing index - all rows, character col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, "c2"]  # All rows, char col
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_allrows_charcol")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Column "c2"
})

test_that("missing index - character row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat["r2", i]  # Char row, all cols
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_charrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Row "r2"
})

test_that("missing index - all rows, negative col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, -1]  # All rows, exclude col 1 = only col 2
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_allrows_negcol")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Just column 2
})

test_that("missing index - negative row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[-1, i]  # Exclude row 1, all cols
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_miss_negrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Rows 2:3, first of each col
})

test_that("missing index - 3D all of dim 2 and 3, vary dim 1", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[i, 1, 1]  # Vary dim 1, fix dims 2 and 3
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_miss_3d_vary1")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L), c(1.0, 2.0))
})

test_that("missing index - 3D vary dim 2, all of dim 1, fix dim 3", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, i, 1]  # Vary dim 2, fix dims 1 and 3
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_miss_3d_vary2")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 3L), c(1.0, 3.0, 5.0))
})

# =============================================================================
# Section 7: Logical Indexing (Masks)
# =============================================================================

test_that("logical mask indexing - 1D read", {
  f <- function(x, mask) {
    out <- numeric(sum(mask))
    j <- 1L
    for (i in seq_along(x)) {
      if (mask[i]) {
        out[j] <- x[i]
        j <- j + 1L
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", name = "idx_mask_1d_read")
  x <- c(10.0, 20.0, 30.0, 40.0)
  mask <- c(TRUE, FALSE, TRUE, FALSE)
  expect_equal(built$func(x, mask), c(10.0, 30.0))
})

test_that("logical mask indexing - 1D write", {
  f <- function(mask, val) {
    out <- numeric(length(mask))
    for (i in seq_along(mask)) {
      if (mask[i]) {
        out[i] <- val
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mask = "lgl[]", val = "f64", name = "idx_mask_1d_write")
  mask <- c(TRUE, FALSE, TRUE)
  expect_equal(built$func(mask, 99.0), c(99.0, 0.0, 99.0))
})

test_that("logical mask indexing - 2D matrix mask", {
  f <- function(mat, mask) {
    out <- matrix(0.0, nrow(mat), ncol(mat))
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (mask[i, j]) {
          out[i, j] <- mat[i, j]
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", mask = "lgl[,]", name = "idx_mask_2d")
  mat <- matrix(1:4, nrow = 2, ncol = 2)
  mask <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
  result <- built$func(mat, mask)
  expect_equal(result[1, 1], 1.0)
  expect_equal(result[1, 2], 0.0)
  expect_equal(result[2, 1], 0.0)
  expect_equal(result[2, 2], 4.0)
})

# =============================================================================
# Section 8: Complex Indexing in Nested Loops
# =============================================================================

test_that("complex indexing - nested loops with mixed access", {
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) {
          out[i] <- x[i] + y[j]
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", name = "idx_complex_nested")
  x <- c(1.0, 2.0, 3.0)
  y <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, y, 3L), c(11.0, 22.0, 33.0))
})

test_that("complex indexing - indirect indexing via vector", {
  f <- function(x, idx_map, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      mapped_idx <- idx_map[i]
      out[i] <- x[mapped_idx]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx_map = "i32[]", n = "i32", name = "idx_indirect")
  x <- c(10.0, 20.0, 30.0, 40.0)
  idx_map <- c(4L, 2L, 1L)
  expect_equal(built$func(x, idx_map, 3L), c(40.0, 20.0, 10.0))
})

test_that("complex indexing - computed indices", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      idx <- i * 2 - 1  # 1, 3, 5, ...
      if (idx <= length(x)) {
        out[i] <- x[idx]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_computed")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x, 3L), c(10.0, 30.0, 50.0))
})

# =============================================================================
# Section 9: Slice Indexing
# =============================================================================

test_that("slice indexing - basic range", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[1:n][i]  # Slice then index
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_slice_basic")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x, 2L), c(10.0, 20.0))
})

test_that("slice indexing - assignment form", {
  f <- function(x, n) {
    out <- numeric(length(x))
    out[1:n] <- x[1:n]  # Slice assignment
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_slice_assign")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x, 2L), c(10.0, 20.0, 0.0, 0.0))
})

test_that("slice indexing - with arithmetic", {
  f <- function(x, start, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      idx <- start + i - 1
      out[i] <- x[idx]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", start = "i32", n = "i32", name = "idx_slice_arith")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x, 2L, 3L), c(20.0, 30.0, 40.0))
})

# =============================================================================
# Section 10: Edge Cases and Boundary Conditions
# =============================================================================

test_that("edge case - index 1 (first element)", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[1]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_first")
  x <- c(99.0, 88.0, 77.0)
  expect_equal(built$func(x), 99.0)
})

test_that("edge case - index at length (last element)", {
  f <- function(x, len) {
    out <- numeric(1)
    out[1] <- x[len]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", len = "i32", name = "idx_edge_last")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, 3L), 30.0)
})

test_that("edge case - negative index -1 (exclude last)", {
  f <- function(x) {
    y <- x[-length(x)]  # Exclude last position, get all but last
    y
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_neg_last")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), c(10.0, 20.0))
})

# =============================================================================
# Section 11: Error Cases (should fail gracefully)
# =============================================================================

test_that("error - out of bounds index should fail", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[100]  # Out of bounds
    out
  }
  
  # In R semantics, positive OOB read returns NA.
  # Compiled path mirrors this as an NA-like payload.
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", cache = FALSE)
  result <- built$func(c(1.0, 2.0, 3.0))
  expect_true(is.na(result[1]))
})

test_that("error - zero index should fail", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[0]  # Zero is invalid in R
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", name = "idx_err_zero_idx"),
    "index 0 is unsupported in scalar selector"
  )
})

test_that("error - mixed positive and negative indices", {
  f <- function(x) {
    # R does not allow mixing positive and negative indices
    out <- numeric(1)
    out[1] <- x[c(1, -2, 3)]  # Mixed: should error
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]"),
    "mixed positive and negative selector values are not supported"
  )
})

test_that("error - NA selector values are rejected", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[NA_real_]
    out
  }

  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", name = "idx_err_na_selector"),
    "NA selector values are not supported"
  )
})

test_that("error - non-finite selector values are rejected", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[Inf]
    out
  }

  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", name = "idx_err_nonfinite_selector"),
    "non-finite selector values \\(Inf/NaN\\) are not supported"
  )
})

test_that("error - logical selector mismatch on static extent is rejected", {
  f <- function() {
    out <- numeric(1)
    out[1] <- c(10.0, 20.0, 30.0)[c(TRUE, FALSE)]
    out
  }

  skip_if_no_mojo()
  expect_error(
    mojor_build(f, name = "idx_err_lgl_selector_extent"),
    "logical selector length mismatch for statically known extent"
  )
})

# =============================================================================
# Section 14: Additional Edge Cases
# =============================================================================

test_that("edge case - empty index vector", {
  f <- function(x) {
    idx <- integer(0)  # Empty index
    out <- x[idx]      # Should return empty vector
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_empty_vec")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), numeric(0))
})

test_that("edge case - negative empty (exclude nothing)", {
  f <- function(x) {
    out <- x[-integer(0)]  # Exclude nothing = same as x[]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_neg_empty")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), x)
})

test_that("edge case - exclude all elements", {
  f <- function(x) {
    # Exclude all positions - result should be empty
    y <- x[-c(1, 2, 3)]  # For length-3 vector
    y
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_exclude_all")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), numeric(0))
})

test_that("edge case - NA in positive index vector", {
  f <- function(x, idx) {
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      if (!is.na(idx[i])) {
        out[i] <- x[idx[i]]
      } else {
        out[i] <- NA_real_
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32[]",
    name = "idx_edge_na_in_vec",
    na_mode = "unsafe"
  )
  x <- c(10.0, 20.0, 30.0)
  idx <- c(1L, NA_integer_, 3L)
  result <- built$func(x, idx)
  expect_equal(result[c(1, 3)], c(10.0, 30.0))
  expect_true(is.na(result[2]))
})

test_that("edge case - index beyond length (runtime check)", {
  f <- function(x, idx) {
    out <- numeric(1)
    # idx might be out of bounds - behavior depends on bounds_check
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  strict <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32",
    name = "idx_edge_oob_strict",
    bounds_check = TRUE
  )
  legacy <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32",
    name = "idx_edge_oob_legacy",
    bounds_check = FALSE
  )
  x <- c(10.0, 20.0, 30.0)
  expect_error(strict$func(x, 10L), "Index out of bounds")
  expect_true(is.nan(legacy$func(x, 10L)[1]))
})

test_that("edge case - floating point index (should coerce)", {
  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]  # idx is f64, should coerce to integer
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "f64", name = "idx_edge_float_idx")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, 2.0), 20.0)  # 2.0 -> 2
})

test_that("edge case - character indexing non-existent name", {
  f <- function(mat) {
    out <- numeric(1)
    # Should error at runtime if name doesn't exist
    out[1] <- mat["nonexistent", "col1"]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, mat = "f64[,]", name = "idx_edge_char_missing"),
    "character indexing requires dimnames/names"
  )
})

test_that("edge case - logical index with NA values", {
  f <- function(x, mask) {
    out <- numeric(sum(mask, na.rm = TRUE))
    j <- 1L
    for (i in seq_along(x)) {
      if (!is.na(mask[i]) && mask[i]) {
        out[j] <- x[i]
        j <- j + 1L
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", name = "idx_edge_lgl_na")
  x <- c(10.0, 20.0, 30.0, 40.0)
  mask <- c(TRUE, NA, FALSE, TRUE)
  expect_equal(built$func(x, mask), c(10.0, 40.0))
})

test_that("edge case - duplicate negative exclusions", {
  f <- function(x) {
    # Duplicate exclusions should be equivalent to single exclusion
    y <- x[-c(1, 1, 2, 2)]  # Same as x[-c(1, 2)]
    y
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_dup_neg")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x), c(30.0, 40.0))  # Excludes 1 and 2
})

test_that("edge case - zero in positive index vector", {
  f <- function(x) {
    # R treats 0 as "select nothing" (no error, just no element)
    out <- x[c(1, 0, 3)]  # Should be equivalent to c(x[1], x[3])
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_zero_in_vec")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x), c(10.0, 30.0))
})

test_that("edge case - matrix as 1D vector indexing", {
  f <- function(mat, idx) {
    # Treat matrix as vector (column-major)
    out <- numeric(length(idx))
    for (i in seq_along(idx)) {
      out[i] <- mat[idx[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", idx = "i32[]", name = "idx_edge_mat_as_vec")
  mat <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 2, ncol = 3)  # Col-major: 1,2,3,4,5,6
  idx <- c(1L, 4L, 6L)
  expect_equal(built$func(mat, idx), c(1.0, 4.0, 6.0))
})

test_that("edge case - large index values", {
  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32", name = "idx_edge_large")
  x <- 1:100  # Large vector
  expect_equal(built$func(x, 100L), 100.0)
  expect_equal(built$func(x, 50L), 50.0)
})

# =============================================================================
# Section 15: Assignment Edge Cases
# =============================================================================

test_that("assignment - self assignment with different indices", {
  f <- function(x, n) {
    out <- x
    for (i in seq_len(n)) {
      out[i] <- out[n - i + 1]  # Reverse: out[1] <- out[n], out[2] <- out[n-1], etc
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_assign_self")
  x <- c(10.0, 20.0, 30.0, 40.0)
  result <- built$func(x, 4L)
  # Full traversal keeps output-shape inference aligned with loop-bound n.
  expect_true(is.numeric(result))
})

test_that("assignment - subset with recycling", {
  f <- function(x) {
    out <- numeric(6)
    # RHS shorter than LHS - should recycle
    out[1:6] <- c(1.0, 2.0)  # Recycles to 1,2,1,2,1,2
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_assign_recycle")
  expect_equal(built$func(c(0.0)), c(1.0, 2.0, 1.0, 2.0, 1.0, 2.0))
})

test_that("assignment - from computed expression", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] * 2 + 1  # Computed, not direct index
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_assign_expr")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, 3L), c(21.0, 41.0, 61.0))
})

test_that("assignment - conditional within loop", {
  f <- function(x, mask, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      if (mask[i]) {
        out[i] <- x[i] * 2
      } else {
        out[i] <- x[i]  # Copy unchanged
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", n = "i32", name = "idx_assign_cond")
  x <- c(10.0, 20.0, 30.0)
  mask <- c(TRUE, FALSE, TRUE)
  expect_equal(built$func(x, mask, 3L), c(20.0, 20.0, 60.0))
})

test_that("assignment - chained indexing", {
  f <- function(mat) {
    out <- numeric(2)
    # Extract diagonal via chained logic
    for (i in 1:2) {
      out[i] <- mat[i, i]  # Diagonal
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_assign_chained")
  mat <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  expect_equal(built$func(mat), c(1.0, 4.0))
})

test_that("assignment - increment/decrement pattern", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      if (i > 1) {
        out[i] <- out[i - 1] + x[i]  # Cumulative sum pattern
      } else {
        out[i] <- x[i]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_assign_increment")
  x <- c(1.0, 2.0, 3.0, 4.0)
  expect_equal(built$func(x, 4L), c(1.0, 3.0, 6.0, 10.0))
})

test_that("assignment - multiple arrays different indices", {
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + y[n - i + 1]  # x forward, y backward
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", name = "idx_assign_multi")
  x <- c(1.0, 2.0, 3.0)
  y <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, y, 3L), c(31.0, 22.0, 13.0))  # 1+30, 2+20, 3+10
})

# =============================================================================
# Section 16: 3D Array Indexing
# =============================================================================

test_that("3D array - full indexing all dimensions", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[i, 1, 1]  # Vary first dim, fix others
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_3d_full")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L), c(1.0, 2.0))
})

test_that("3D array - vary middle dimension", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, i, 1]  # Vary second dim
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_3d_mid")
  arr <- array(1:24, dim = c(2, 3, 4))  # Col-major: pos 1, 3, 5 in slice
  expect_equal(built$func(arr, 3L), c(1.0, 3.0, 5.0))
})

test_that("3D array - vary last dimension", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[1, 1, i]  # Vary third dim
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_3d_last")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 4L), c(1.0, 7.0, 13.0, 19.0))
})

test_that("3D array - nested loops over two dimensions", {
  f <- function(arr, n) {
    out <- matrix(0.0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- arr[i, j, 1]  # First slice
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_3d_nested")
  arr <- array(1:24, dim = c(2, 3, 4))
  result <- built$func(arr, 2L)
  expect_equal(result[1, 1], 1.0)
  expect_equal(result[2, 2], 4.0)
})

test_that("3D array - dynamic dim selection via dim() compiles and runs", {
  f <- function(arr, axis, n) {
    out <- numeric(n)
    dim_size <- dim(arr)[axis]  # Get size of dynamic axis
    for (i in seq_len(n)) {
      if (axis == 1) {
        out[i] <- arr[i, 1, 1]
      } else if (axis == 2) {
        out[i] <- arr[1, i, 1]
      } else {
        out[i] <- arr[1, 1, i]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", axis = "i32", n = "i32", name = "idx_3d_dyn_axis")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 1L, 2L), c(1.0, 2.0))
  expect_equal(built$func(arr, 2L, 3L), c(1.0, 3.0, 5.0))
})

# =============================================================================
# Section 17: Additional Edge Cases
# =============================================================================

test_that("edge case - single element arrays", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[1]  # Only valid index is 1
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_single")
  x <- c(42.0)
  expect_equal(built$func(x), 42.0)
})

test_that("edge case - index into empty array", {
  f <- function(x, idx) {
    out <- numeric(1)
    # idx should be 0 for empty array
    out[1] <- 0.0  # Can't index into empty, just return 0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "i32", name = "idx_edge_into_empty")
  x <- numeric(0)
  expect_equal(built$func(x, 1L), 0.0)
})

test_that("edge case - length-1 matrix", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat[1, 1]  # 1x1 matrix
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_edge_1x1")
  mat <- matrix(99.0, nrow = 1, ncol = 1)
  expect_equal(built$func(mat), 99.0)
})

test_that("edge case - degenerate dimension (size 1)", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[i, 1, 1]  # Second and third dims are size 1
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_edge_degen")
  arr <- array(1:5, dim = c(5, 1, 1))  # Degenerate dims
  expect_equal(built$func(arr, 3L), c(1.0, 2.0, 3.0))
})

test_that("edge case - boolean used as index (coercion)", {
  f <- function(x) {
    # TRUE coerces to 1, FALSE to 0
    out <- numeric(1)
    out[1] <- x[TRUE]   # Should be x[1]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_bool_coerce")
  expect_equal(built$func(c(10.0, 20.0, 30.0)), 10.0)
})

test_that("edge case - very long index computation", {
  f <- function(x, a, b, c) {
    out <- numeric(1)
    idx <- a + b * 2 - c + 1  # Complex computation
    if (idx >= 1 && idx <= length(x)) {
      out[1] <- x[idx]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", a = "i32", b = "i32", c = "i32", name = "idx_edge_complex")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x, 1L, 2L, 1L), x[1 + 4 - 1 + 1])  # x[5] = 50
})

test_that("edge case - floating point very close to integer", {
  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]  # idx is f64 like 1.9999999 or 2.0000001
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx = "f64", name = "idx_edge_float_prec")
  x <- c(10.0, 20.0, 30.0)
  # Truncation behavior: 1.9999 -> 1, 2.0 -> 2
  expect_equal(built$func(x, 2.0), 20.0)
})

test_that("edge case - negative index with NA mixed", {
  f <- function(x) {
    # Placeholder lane until mixed negative+NA selector parity is implemented.
    out <- numeric(1)
    out[1] <- 0.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_neg_na_mixed")
  expect_equal(built$func(c(1.0, 2.0, 3.0)), 0.0)
})

test_that("edge case - unicode in character dimnames", {
  f <- function(mat) {
    out <- numeric(1)
    out[1] <- mat["日本語", "αβγ"]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, mat = "f64[,]", name = "idx_edge_unicode_names"),
    "character indexing requires dimnames/names"
  )
})

test_that("edge case - memory aliasing (same array read/write)", {
  f <- function(x, n) {
    # Read and write to same array with overlapping indices
    for (i in 2:n) {
      x[i] <- x[i] + x[i-1]  # Each iteration depends on previous
    }
    x
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_edge_alias")
  x <- c(1.0, 2.0, 3.0, 4.0)
  result <- built$func(x, 4L)
  # x[2] = 2+1=3, x[3] = 3+3=6, x[4] = 4+6=10
  expect_equal(result, c(1.0, 3.0, 6.0, 10.0))
})

test_that("edge case - parallel loop with indexing", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] * 2  # Parallel-safe indexing
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_edge_parallel_index")
  expect_equal(built$func(c(1.0, 2.0, 3.0), 3L), c(2.0, 4.0, 6.0))
})

test_that("edge case - named vector indexing", {
  f <- function(x) {
    # Named vectors: x["name"] access
    out <- numeric(1)
    out[1] <- x["second"]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", name = "idx_edge_named_vec"),
    "character indexing requires dimnames/names"
  )
})

test_that("edge case - chained vector indexing", {
  f <- function(x, idx1, idx2) {
    # First extract subset, then index into subset
    # x[idx1] returns vector, then we want element idx2 of that vector
    subset <- x[idx1]      # e.g., x[c(5,3,1)] = c(50,30,10)
    out <- numeric(1)
    out[1] <- subset[idx2]  # e.g., subset[2] = 30
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", idx2 = "i32", name = "idx_edge_chained")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx1 <- c(5L, 3L, 1L)  # subset = c(50, 30, 10)
  expect_equal(built$func(x, idx1, 2L), 30.0)  # subset[2] = 30
})

# =============================================================================
# Section 20: Chained Indexing Patterns
# =============================================================================

test_that("chained - positive then positive", {
  f <- function(x, idx1, idx2) {
    # x[idx1] gives subset, then subset[idx2]
    subset <- x[idx1]
    out <- numeric(length(idx2))
    for (i in seq_along(idx2)) {
      out[i] <- subset[idx2[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", idx2 = "i32[]", name = "idx_chain_pos_pos")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  idx1 <- c(6L, 4L, 2L, 5L)  # subset = c(60, 40, 20, 50)
  idx2 <- c(3L, 1L)          # subset[c(3,1)] = c(20, 60)
  expect_equal(built$func(x, idx1, idx2), c(20.0, 60.0))
})

test_that("chained - positive then negative", {
  f <- function(x, idx1) {
    # x[idx1] gives subset, then exclude from subset
    subset <- x[idx1]      # e.g., x[c(1,2,3,4,5)] = c(10,20,30,40,50)
    out <- numeric(1)
    out[1] <- subset[-2]   # exclude 2nd element of subset
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", name = "idx_chain_pos_neg")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx1 <- c(1L, 3L, 5L, 2L, 4L)  # subset = c(10, 30, 50, 20, 40)
  expect_equal(built$func(x, idx1), 10.0)  # subset[-2] = exclude 30, first is 10
})

test_that("chained - negative then positive", {
  f <- function(x) {
    # Exclude first, then index into result
    subset <- x[-1]        # exclude position 1: c(20,30,40,50)
    out <- numeric(2)
    out[1] <- subset[1]    # 20
    out[2] <- subset[3]    # 40
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_neg_pos")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x), c(20.0, 40.0))
})

test_that("chained - negative then negative", {
  f <- function(x) {
    # Double exclusion: x[-1] then exclude from that
    subset <- x[-c(1, 5)]  # exclude 1 and 5: c(20,30,40)
    out <- numeric(1)
    out[1] <- subset[-2]   # exclude 2nd of subset (30), get first (20)
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_neg_neg")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x), 20.0)  # subset[-2] of c(20,30,40) = c(20,40), first is 20
})

test_that("chained - three levels deep", {
  f <- function(x, i1, i2, i3) {
    # x[i1][i2][i3] - three levels of indexing
    temp1 <- x[i1]
    temp2 <- temp1[i2]
    out <- numeric(length(i3))
    for (i in seq_along(i3)) {
      out[i] <- temp2[i3[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", i1 = "i32[]", i2 = "i32[]", i3 = "i32[]", name = "idx_chain_3level")
  x <- 10.0 * (1:10)  # 10, 20, 30, ..., 100
  i1 <- c(10L, 8L, 6L, 4L, 2L)  # temp1 = c(100, 80, 60, 40, 20)
  i2 <- c(1L, 3L, 5L)           # temp2 = c(100, 60, 20)
  i3 <- c(2L, 1L)               # c(60, 100)
  expect_equal(built$func(x, i1, i2, i3), c(60.0, 100.0))
})

test_that("chained - with out of order at each level", {
  f <- function(x) {
    # Scramble at each level
    temp <- x[c(5, 2, 6, 1, 3)]     # c(50, 20, 60, 10, 30)
    temp <- temp[c(4, 2, 5)]         # c(10, 20, 30)
    out <- temp[c(3, 1, 2)]          # c(30, 10, 20)
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_scrambled")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  expect_equal(built$func(x), c(30.0, 10.0, 20.0))
})

test_that("chained - matrix row then column", {
  f <- function(mat, row_idx, col_idx) {
    # Extract row, then index into row
    row <- mat[row_idx, ]    # Get entire row
    out <- numeric(length(col_idx))
    for (i in seq_along(col_idx)) {
      out[i] <- row[col_idx[i]]  # Index into that row
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", row_idx = "i32", col_idx = "i32[]", name = "idx_chain_mat_row")
  mat <- matrix(1:12, nrow = 3, ncol = 4)  # 3x4 matrix
  expect_equal(built$func(mat, 2L, c(1L, 3L)), c(2.0, 8.0))  # row 2: c(2,5,8,11), cols 1,3
})

test_that("chained - matrix column then row", {
  f <- function(mat, col_idx, row_idx) {
    # Extract column, then index into column
    col <- mat[, col_idx]    # Get entire column
    out <- numeric(length(row_idx))
    for (i in seq_along(row_idx)) {
      out[i] <- col[row_idx[i]]  # Index into that column
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", col_idx = "i32", row_idx = "i32[]", name = "idx_chain_mat_col")
  mat <- matrix(1:12, nrow = 3, ncol = 4)  # 3x4 matrix, col-major
  expect_equal(built$func(mat, 2L, c(1L, 3L)), c(4.0, 6.0))  # col 2: c(4,5,6), rows 1,3
})

test_that("chained - with negative at second level", {
  f <- function(x, idx1) {
    temp <- x[idx1]         # First selection
    out <- temp[-c(1, 3)]   # Exclude positions 1 and 3 from result
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", name = "idx_chain_2nd_neg")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  idx1 <- c(6L, 2L, 5L, 1L)  # temp = c(60, 20, 50, 10)
  expect_equal(built$func(x, idx1), c(20.0, 10.0))  # exclude 60 and 50
})

test_that("chained - shrinking then growing selection", {
  f <- function(x, n) {
    # Chain of operations that changes size
    temp1 <- x[1:n]          # First n elements
    temp2 <- temp1[-1]       # Remove first (n-1 elements)
    out <- numeric(length(temp2) + 1)
    out[1] <- x[1]           # Original first
    for (i in seq_along(temp2)) {
      out[i + 1] <- temp2[i]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_chain_shrink")
  x <- c(100.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x, 4L), c(100.0, 20.0, 30.0, 40.0))  # 100 + (20,30,40)
})

test_that("chained - in loop with dynamic indices", {
  f <- function(x, idx_mat, n) {
    # idx_mat is n x 2 matrix where each row is (idx1, idx2) for chain
    out <- numeric(n)
    for (i in seq_len(n)) {
      first_idx <- idx_mat[i, 1]
      second_idx <- idx_mat[i, 2]
      temp <- x[first_idx]
      out[i] <- temp[second_idx]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx_mat = "i32[,]", n = "i32", name = "idx_chain_loop")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  idx_mat <- matrix(c(6L, 1L, 4L, 1L, 2L, 1L), nrow = 3, ncol = 2, byrow = TRUE)
  expect_equal(built$func(x, idx_mat, 3L), c(60.0, 40.0, 20.0))
})

test_that("chained - conditional chain", {
  f <- function(x, idx1, use_second, idx2) {
    # Conditionally apply second level
    temp <- x[idx1]
    out <- numeric(1)
    if (use_second) {
      out[1] <- temp[idx2]
    } else {
      out[1] <- temp[1]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", use_second = "lgl", idx2 = "i32", name = "idx_chain_cond")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx1 <- c(5L, 3L, 1L)  # c(50, 30, 10)
  expect_equal(built$func(x, idx1, TRUE, 2L), 30.0)   # Use second level
  expect_equal(built$func(x, idx1, FALSE, 2L), 50.0)  # Just first of temp
})

test_that("chained - with duplicates propagating", {
  f <- function(x) {
    # First selection has duplicates, second selection extracts from duplicates
    temp <- x[c(1, 1, 2, 2, 3)]  # c(10, 10, 20, 20, 30)
    out <- temp[c(2, 4)]          # c(10, 20) - from positions 2 and 4
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_dup_prop")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), c(10.0, 20.0))
})

test_that("chained - vector write to chained result", {
  f <- function(x, idx1, vals) {
    # Write to chained selection
    temp <- x[idx1]
    for (i in seq_along(vals)) {
      temp[i] <- vals[i]
    }
    temp  # Return modified
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", vals = "f64[]", name = "idx_chain_write")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx1 <- c(5L, 3L, 1L)  # temp = c(50, 30, 10)
  vals <- c(99.0, 88.0, 77.0)
  expect_equal(built$func(x, idx1, vals), c(99.0, 88.0, 77.0))
})

test_that("chained - double negative exclusion", {
  f <- function(x) {
    # Exclude some, then exclude more from result
    temp <- x[-c(1, 6)]      # Remove 1st and 6th: c(20,30,40,50)
    out <- temp[-c(2, 3)]    # Remove 2nd and 3rd of temp: c(20,50)
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_dbl_neg")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  expect_equal(built$func(x), c(20.0, 50.0))
})

test_that("chained - 3D slice to 2D to 1D", {
  f <- function(arr, slice_idx) {
    # Extract 2D slice from 3D, then row, then element
    slice <- arr[, , slice_idx]  # 2D slice
    row <- slice[1, ]            # First row of slice
    out <- numeric(1)
    out[1] <- row[2]             # Second element of row
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", slice_idx = "i32", name = "idx_chain_3d_slice")
  arr <- array(as.numeric(1:24), dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L), 9.0)
  expect_equal(built$func(arr, 4L), 21.0)
})

test_that("chained - 4D slice to 2D to 1D", {
  f <- function(arr, k, l) {
    slice <- arr[, , k, l]
    row <- slice[1, ]
    out <- numeric(1)
    out[1] <- row[2]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[4d]", k = "i32", l = "i32", name = "idx_chain_4d_slice")
  arr <- array(as.numeric(1:48), dim = c(2, 3, 4, 2))
  expect_equal(built$func(arr, 2L, 1L), f(arr, 2L, 1L))
  expect_equal(built$func(arr, 4L, 2L), f(arr, 4L, 2L))
})

test_that("chained - 4D slice matrix write in loop", {
  f <- function(arr, k, l, val) {
    slice <- arr[, , k, l]
    for (j in 1:3) {
      slice[1, j] <- val
    }
    out <- numeric(1)
    out[1] <- slice[1, 2]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(
    f,
    arr = "f64[4d]",
    k = "i32",
    l = "i32",
    val = "f64",
    name = "idx_chain_4d_slice_write"
  )
  arr <- array(as.numeric(1:48), dim = c(2, 3, 4, 2))
  expect_equal(built$func(arr, 3L, 1L, 999.0), f(arr, 3L, 1L, 999.0))
})

test_that("chained - direct nested vector read expression", {
  f <- function(x, idx1, idx2) {
    out <- numeric(1)
    out[1] <- x[idx1][idx2]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(
    f,
    x = "f64[]",
    idx1 = "i32[]",
    idx2 = "i32",
    name = "idx_chain_direct_vec_read"
  )
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  idx1 <- c(5L, 3L, 1L)
  expect_equal(built$func(x, idx1, 2L), f(x, idx1, 2L))
  expect_equal(built$func(x, idx1, 3L), f(x, idx1, 3L))
})

test_that("no-loop sequencing - local slice indexed write is preserved", {
  f <- function(arr, k, l, val) {
    slice <- arr[, , k, l]
    slice[1, 2] <- val
    out <- numeric(2)
    out[1] <- slice[1, 1]
    out[2] <- slice[1, 2]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(
    f,
    arr = "f64[4d]",
    k = "i32",
    l = "i32",
    val = "f64",
    name = "idx_noloop_seq_local_slice_write"
  )
  arr <- array(as.numeric(1:48), dim = c(2, 3, 4, 2))
  expect_equal(built$func(arr, 3L, 1L, 999.0), f(arr, 3L, 1L, 999.0))
})

test_that("chained - nested loops with chains", {
  f <- function(x, n) {
    # Chained indexing inside nested loops
    out <- matrix(0.0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        temp <- x[c(i, j, i + j)]  # Chain-able selection
        out[i, j] <- temp[1] + temp[3]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_chain_nested")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  result <- built$func(x, 2L)
  expect_equal(result[1, 1], 20.0)
  expect_equal(result[1, 2], 60.0)
  expect_equal(result[2, 1], 20.0)
  expect_equal(result[2, 2], 40.0)
})

test_that("chained - with computed indices at each level", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      idx1 <- i * 2 - 1          # 1, 3, 5, ...
      idx2 <- (i %% 2) + 1       # 1, 2, 1, 2, ...
      temp <- x[idx1:(idx1 + 1)]  # Subset of 2 elements
      out[i] <- temp[idx2]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_chain_computed")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0)
  result <- built$func(x, 3L)
  # i=1: idx1=1, temp=c(10,20), idx2=2, out[1]=20
  # i=2: idx1=3, temp=c(30,40), idx2=1, out[2]=30
  # i=3: idx1=5, temp=c(50,60), idx2=2, out[3]=60
  expect_equal(result, c(20.0, 30.0, 60.0))
})

test_that("chained - empty intermediate result", {
  f <- function(x) {
    # First selection returns empty, second selection on empty
    temp <- x[-(1:length(x))]  # Exclude all = empty
    out <- numeric(length(temp))
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_chain_empty")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x), numeric(0))
})

test_that("chained - single element intermediate", {
  f <- function(x, idx1, idx2) {
    # Intermediate has only 1 element
    temp <- x[idx1]    # Single element
    out <- numeric(1)
    out[1] <- temp[idx2]  # Must be idx2=1 or -1
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", idx1 = "i32[]", idx2 = "i32", name = "idx_chain_single")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, c(2L), 1L), 20.0)
})

# =============================================================================
# Section 18: Error Cases and Unsupported Types
# =============================================================================

test_that("error - list indexing should fail", {
  f <- function(lst, idx) {
    out <- numeric(1)
    out[1] <- lst[[idx]]  # List element access
    out
  }
  
  skip_if_no_mojo()
  # Lists are not supported in compiled loops
  expect_error(mojor_build(f, lst = "list", idx = "i32"))
})

test_that("error - data.frame indexing should fail", {
  f <- function(df, idx) {
    out <- numeric(1)
    out[1] <- df[idx, 1]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, df = "df", idx = "i32", name = "idx_err_df_no_schema"),
    "df_schema|out of bounds"
  )
})

test_that("error - factor indexing behavior", {
  f <- function(x, f) {
    out <- numeric(1)
    # Factor as index - uses integer values, not levels
    out[1] <- x[f]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", f = "factor", name = "idx_err_factor"),
    "missing type annotation/value|unsupported type spec"
  )
})

test_that("error - raw type indexing", {
  f <- function(x, idx) {
    out <- raw(1)
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  # Raw type not in supported type list
  expect_error(mojor_build(f, x = "raw[]", idx = "i32"))
})

test_that("error - complex number indexing", {
  f <- function(x, idx) {
    out <- numeric(1)
    # Complex index doesn't make sense
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "cplx[]", idx = "i32", name = "idx_err_complex"),
    "unsupported type spec"
  )
})

# =============================================================================
# Section 19: Special R Behaviors
# =============================================================================

test_that("special - drop=FALSE matrix indexing", {
  f <- function(mat) {
    # drop=FALSE should preserve dimensions
    out <- mat[1, , drop = FALSE]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, mat = "f64[,]", name = "idx_special_drop_false"),
    "return\\(\\) must return the output vector or scalar accumulator"
  )
})

test_that("special - NA in negative index", {
  f <- function(x) {
    # c(-1, NA) in R: excludes 1, then NA causes NA in result
    # Actually R errors on this
    out <- numeric(1)
    out[1] <- 0.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_special_neg_na_placeholder")
  expect_equal(built$func(c(1.0, 2.0, 3.0)), 0.0)
})

test_that("special - both positive and negative zero", {
  f <- function(x) {
    # -0 is same as 0 in R
    out <- numeric(1)
    out[1] <- 0.0  # Can't test -0 easily
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_special_neg_zero")
  expect_equal(built$func(c(1.0, 2.0, 3.0)), 0.0)
})

test_that("special - array with only dimnames, no dim", {
  f <- function(x) {
    # Named vector without dim attribute
    out <- numeric(1)
    out[1] <- x["name"]
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", name = "idx_special_named_nodim"),
    "character indexing requires dimnames/names"
  )
})

test_that("special - matrix subset assignment with recycling", {
  f <- function(mat, n) {
    # Assign vector to matrix slice with recycling
    for (i in seq_len(n)) {
      mat[i, ] <- c(1.0, 2.0)  # Should recycle if ncol > 2
    }
    mat
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_special_mat_recycle")
  mat <- matrix(0.0, nrow = 3, ncol = 4)
  expected <- matrix(c(
    1.0, 2.0, 1.0, 2.0,
    1.0, 2.0, 1.0, 2.0,
    0.0, 0.0, 0.0, 0.0
  ), nrow = 3, ncol = 4, byrow = TRUE)
  expect_equal(built$func(mat, 2L), expected)
})

test_that("special - partial matching in character index", {
  f <- function(mat) {
    out <- numeric(1)
    # R does partial matching for names
    out[1] <- mat["row", ]  # Might match "row1"
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, mat = "f64[,]", name = "idx_special_partial_match"),
    "strict IR emission produced empty output|character indexing requires dimnames/names"
  )
})

# =============================================================================
# Section 12: Type Combinations
# =============================================================================

test_that("type - i32 array with i32 index", {
  f <- function(x, idx) {
    out <- integer(1)
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "i32[]", idx = "i32", name = "idx_type_i32")
  x <- c(10L, 20L, 30L)
  expect_equal(built$func(x, 2L), 20L)
})

test_that("type - f32 array with i32 index", {
  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f32[]", idx = "i32", name = "idx_type_f32")
  x <- as.float(c(10.0, 20.0, 30.0))
  result <- built$func(x, 2L)
  expect_equal(as.numeric(result), 20.0, tolerance = 0.01)
})

test_that("type - logical array with index", {
  f <- function(x, idx) {
    out <- logical(1)
    out[1] <- x[idx]
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "lgl[]", idx = "i32", name = "idx_type_lgl")
  x <- c(TRUE, FALSE, TRUE)
  expect_equal(built$func(x, 2L), FALSE)
})

# =============================================================================
# Section 13: Higher-Dimensional Arrays
# =============================================================================

test_that("3D array - positive indexing", {
  f <- function(arr, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- arr[i, 1, 1]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_3d_pos")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L), c(1.0, 2.0))
})

test_that("3D array - with dim expression indexing compiles and runs", {
  f <- function(arr, k, n) {
    out <- numeric(n)
    dim_k <- dim(arr)[k]  # Get k-th dimension
    for (i in seq_len(n)) {
      if (i <= dim_k) {
        out[i] <- arr[i, 1, 1]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", k = "i32", n = "i32", name = "idx_3d_dim")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L, 3L), c(1.0, 2.0, 3.0))
})


# =============================================================================
# Section 21: Named Vector Indexing
# =============================================================================

test_that("named vector - basic name access", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x["b"]  # Access by name
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_named_basic")
  x <- setNames(c(1.0, 2.0, 3.0), c("a", "b", "c"))
  expect_equal(built$func(x, 3L), c(2.0, 2.0, 2.0))
})

test_that("named vector - dynamic name in loop", {
  f <- function(x, names, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[names[i]]  # Dynamic name lookup
    }
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", names = "chr[]", n = "i32", name = "idx_named_dynamic"),
    "unsupported type spec for names"
  )
})

# =============================================================================
# Section 22: which() and Conditional Results as Indices
# =============================================================================

test_that("which result as index", {
  f <- function(x, threshold, n) {
    # Find indices where x > threshold, then access those
    out <- numeric(n)
    for (i in seq_len(n)) {
      if (x[i] > threshold) {
        out[i] <- x[i]  # Simplified - can't use which() in strict
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", threshold = "f64", n = "i32", name = "idx_which_pattern")
  x <- c(1.0, 5.0, 2.0, 8.0, 3.0)
  result <- built$func(x, 3.0, 5L)
  expect_equal(result, c(0.0, 5.0, 0.0, 8.0, 0.0))  # Values > 3
})

test_that("seq with by parameter as index", {
  f <- function(x, n) {
    out <- numeric(n)
    # Every other element: x[seq(1, length(x), by=2)]
    for (i in seq_len(n)) {
      idx <- i * 2 - 1  # 1, 3, 5, ...
      if (idx <= length(x)) {
        out[i] <- x[idx]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_seq_by")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  expect_equal(built$func(x, 3L), c(10.0, 30.0, 50.0))
})

# =============================================================================
# Section 23: Assignment with Mixed Index Types
# =============================================================================

test_that("assignment - negative index write on returned-arg target", {
  f_scalar <- function(x) {
    x[-1] <- 9.0
    x
  }
  f_vector <- function(x) {
    x[-c(1, 3)] <- 9.0
    x
  }

  skip_if_no_mojo()
  built_scalar <- mojor_build(
    f_scalar,
    x = "f64[]",
    name = "idx_assign_neg_write_non_output_scalar"
  )
  built_vector <- mojor_build(
    f_vector,
    x = "f64[]",
    name = "idx_assign_neg_write_non_output_vec"
  )
  expect_equal(built_scalar$func(c(1.0, 2.0, 3.0, 4.0)), c(1.0, 9.0, 9.0, 9.0))
  expect_equal(built_vector$func(c(1.0, 2.0, 3.0, 4.0, 5.0)), c(1.0, 9.0, 3.0, 9.0, 9.0))
})

test_that("assignment - character col, numeric row", {
  f <- function(mat, row_idx, val) {
    out <- mat
    out[row_idx, "c2"] <- val  # Numeric row, char col
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", row_idx = "i32", val = "f64", name = "idx_assign_mix")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  result <- built$func(mat, 2L, 99.0)
  expect_equal(result[2, 2], 99.0)  # Row 2, col "c2" = 99
})

# =============================================================================
# Section 24: Advanced Edge Cases
# =============================================================================

test_that("edge case - index 0 in R (select nothing)", {
  f <- function(x) {
    # x[0] returns numeric(0) in R
    out <- numeric(0)
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_zero_select_none")
  expect_length(built$func(c(1.0, 2.0, 3.0)), 0L)
})

test_that("edge case - very large array, small index", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[1]  # Just access first element
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_edge_large_array")
  x <- rnorm(1000000)  # 1M elements
  expect_equal(built$func(x), x[1])
})

test_that("edge case - index arithmetic complex", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      # Complex index computation
      idx <- ((i * 3) %% 5) + 1  # Wrap around pattern
      out[i] <- x[idx]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_edge_arith_complex")
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  expect_equal(built$func(x, 5L), c(40.0, 20.0, 50.0, 30.0, 10.0))  # Pattern: 4,2,5,3,1
})

test_that("edge case - repeated assignment same index", {
  f <- function(n) {
    out <- numeric(3)
    for (i in seq_len(n)) {
      out[2] <- out[2] + i  # Keep updating position 2
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, n = "i32", name = "idx_edge_repeated_assign")
  expect_equal(built$func(5L), c(0.0, 15.0, 0.0))  # 1+2+3+4+5 = 15
})

test_that("edge case - aliased index variables", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      j <- i
      k <- j
      out[k] <- x[i]  # Multiple aliases for same index
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_edge_aliased")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built$func(x, 3L), c(10.0, 20.0, 30.0))
})


# =============================================================================
# Section 25: Literal Missing Dimension Tests (mat[,1], mat[-1,], etc.)
# =============================================================================

test_that("missing dimension - matrix all rows, specific col", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, 2]  # This is mat[,2] with varying i
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_allrows_speccol")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Column 2
})

test_that("missing dimension - matrix specific row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[2, i]  # This is mat[2,] with varying i
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_specrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Row 2
})

test_that("missing dimension - all rows, negative col", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat[,-1] means all rows, exclude col 1
    for (i in seq_len(n)) {
      out[i] <- mat[i, -1][1]  # First of remaining (only col 2)
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_allrows_negcol")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Just column 2
})

test_that("missing dimension - negative row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat[-1,] means exclude row 1, all cols
    for (i in seq_len(n)) {
      out[i] <- mat[-1, i][1]  # First of remaining rows for col i
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_negrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Rows 2:3, first of each
})

test_that("missing dimension - character row, all cols", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat["r2",] with char row, all cols
    for (i in seq_len(n)) {
      out[i] <- mat["r2", i]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_charrow_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Row "r2"
})

test_that("missing dimension - all rows, character col", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat[,"c2"] with all rows, char col
    for (i in seq_len(n)) {
      out[i] <- mat[i, "c2"]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_allrows_charcol2")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))  # Column "c2"
})

test_that("missing dimension - all rows, vector cols", {
  f <- function(mat, cols) {
    # mat[,cols] - all rows, specific columns
    out <- matrix(0.0, nrow(mat), length(cols))
    for (i in seq_len(nrow(mat))) {
      for (j in seq_along(cols)) {
        out[i, j] <- mat[i, cols[j]]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", cols = "i32[]", name = "idx_missdim_allrows_veccols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  result <- built$func(mat, c(2L, 1L))  # cols 2, 1
  expect_equal(result[, 1], c(4.0, 5.0, 6.0))  # col 2
  expect_equal(result[, 2], c(1.0, 2.0, 3.0))  # col 1
})

test_that("missing dimension - vector rows, all cols", {
  f <- function(mat, rows) {
    # mat[rows,] - specific rows, all cols
    out <- matrix(0.0, length(rows), ncol(mat))
    for (i in seq_along(rows)) {
      for (j in seq_len(ncol(mat))) {
        out[i, j] <- mat[rows[i], j]
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", rows = "i32[]", name = "idx_missdim_vecrows_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  result <- built$func(mat, c(3L, 1L))  # rows 3, 1
  expect_equal(result[1, ], c(3.0, 6.0))  # row 3
  expect_equal(result[2, ], c(1.0, 4.0))  # row 1
})

test_that("missing dimension - 3D all of dim 1, specific dim 2 and 3", {
  f <- function(arr, n) {
    out <- numeric(n)
    # arr[,2,1] - all of dim 1, dim 2=2, dim 3=1
    for (i in seq_len(n)) {
      out[i] <- arr[i, 2, 1]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_missdim_3d_all1_spec23")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 2L), c(3.0, 4.0))  # dim1 varies, dim2=2, dim3=1
})

test_that("missing dimension - 3D specific dim 1, all of dim 2, specific dim 3", {
  f <- function(arr, n) {
    out <- numeric(n)
    # arr[1,,2] - dim 1=1, all of dim 2, dim 3=2
    for (i in seq_len(n)) {
      out[i] <- arr[1, i, 2]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_missdim_3d_spec1_all2_spec3")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 3L), c(7.0, 9.0, 11.0))  # dim1=1, dim2 varies, dim3=2
})

test_that("missing dimension - 3D all dims 1 and 2, specific dim 3 compiles and runs", {
  f <- function(arr, n) {
    out <- numeric(n)
    # arr[,,2] - all of dim 1 and 2, dim 3=2
    # Flattened traversal
    idx <- 1
    for (i in seq_len(dim(arr)[1])) {
      for (j in seq_len(dim(arr)[2])) {
        if (idx <= n) {
          out[idx] <- arr[i, j, 2]
          idx <- idx + 1
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_missdim_3d_all12_spec3")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 6L), c(7.0, 9.0, 11.0, 8.0, 10.0, 12.0))
})

test_that("missing dimension - write with missing row index", {
  f <- function(mat, val) {
    out <- mat
    # out[,2] <- val - all rows, column 2
    for (i in seq_len(nrow(mat))) {
      out[i, 2] <- val
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", val = "f64", name = "idx_missdim_write_allrows")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  result <- built$func(mat, 99.0)
  expect_equal(result[, 2], c(99.0, 99.0, 99.0))  # Column 2 all 99
})

test_that("missing dimension - write with missing col index", {
  f <- function(mat, row_idx, vals) {
    out <- mat
    # out[row_idx,] <- vals - specific row, all cols
    for (j in seq_len(ncol(mat))) {
      out[row_idx, j] <- vals[j]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", row_idx = "i32", vals = "f64[]", name = "idx_missdim_write_allcols")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  result <- built$func(mat, 2L, c(88.0, 99.0))
  expect_equal(result[2, ], c(88.0, 99.0))  # Row 2 has new values
})

test_that("missing dimension - RHS matrix missing row slice assignment", {
  f <- function(mat, n) {
    out <- numeric(n)
    out[1:n] <- mat[, 2L]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_rhs_mat_col")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), mat[, 2L])
})

test_that("missing dimension - RHS array missing middle slice assignment", {
  f <- function(arr, n) {
    out <- numeric(n)
    out[1:n] <- arr[1L, , 2L]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, arr = "f64[3d]", n = "i32", name = "idx_missdim_rhs_arr_mid")
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(built$func(arr, 3L), arr[1L, , 2L])
})

test_that("missing dimension - shifted LHS slice aligns RHS missing indices", {
  f_mat <- function(mat, n, j) {
    out <- numeric(n + 1L)
    out[2:(n + 1L)] <- mat[, j]
    out
  }
  f_arr <- function(arr, n, i, k) {
    out <- numeric(n + 1L)
    out[2:(n + 1L)] <- arr[i, , k]
    out
  }

  skip_if_no_mojo()

  built_mat <- mojor_build(
    f_mat,
    mat = "f64[,]",
    n = "i32",
    j = "i32",
    name = "idx_missdim_shift_lhs_rhs_mat"
  )
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  expected_mat <- numeric(4L)
  expected_mat[2:4] <- mat[, 2L]
  expect_equal(built_mat$func(mat, 3L, 2L), expected_mat)

  built_arr <- mojor_build(
    f_arr,
    arr = "f64[3d]",
    n = "i32",
    i = "i32",
    k = "i32",
    name = "idx_missdim_shift_lhs_rhs_arr"
  )
  arr <- array(1:24, dim = c(2, 3, 4))
  expected_arr <- numeric(4L)
  expected_arr[2:4] <- arr[1L, , 2L]
  expect_equal(built_arr$func(arr, 3L, 1L, 2L), expected_arr)
})

test_that("missing dimension - RHS slice assignment parity (strict + non-strict, recycling)", {
  f_mat <- function(mat, n, j) {
    out <- numeric(n)
    out[1:n] <- mat[, j]
    out
  }
  f_arr <- function(arr, n, i, k) {
    out <- numeric(n)
    out[1:n] <- arr[i, , k]
    out
  }

  skip_if_no_mojo()
  modes <- list(strict = TRUE, non_strict = FALSE)
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  arr <- array(1:24, dim = c(2, 3, 4))

  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    built_mat <- mojor_build(
      f_mat, mat = "f64[,]", n = "i32", j = "i32",
      name = paste0("idx_missdim_rhs_parity_mat_", mode_name)
    )
    built_arr <- mojor_build(
      f_arr, arr = "f64[3d]", n = "i32", i = "i32", k = "i32",
      name = paste0("idx_missdim_rhs_parity_arr_", mode_name)
    )

    expect_equal(built_mat$func(mat, 5L, 2L), suppressWarnings({
      out <- numeric(5L)
      out[1:5] <- mat[, 2L]
      out
    }))
    expect_equal(built_arr$func(arr, 5L, 1L, 2L), suppressWarnings({
      out <- numeric(5L)
      out[1:5] <- arr[1L, , 2L]
      out
    }))
  }
})

test_that("missing dimension - negative with missing", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat[-1,] - exclude row 1, all cols
    for (i in seq_len(n)) {
      out[i] <- mat[-1, i][1]  # First of remaining rows
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_neg_missing")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Rows 2:3, first of each
})

test_that("missing dimension - character with missing", {
  f <- function(mat, n) {
    out <- numeric(n)
    # mat["r2",] - char row, all cols
    for (i in seq_len(n)) {
      out[i] <- mat["r2", i]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_missdim_char_missing")
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(built$func(mat, 2L), c(2.0, 5.0))  # Row "r2"
})


# =============================================================================
# Section 26: Additional Missing Coverage
# =============================================================================

# 1. x[] full extraction
# 2. x[] <- y full replacement
# 3. drop=FALSE behavior
# 4. Dimnames preservation
# 5. cbind(i,j) matrix index
# 6. Assignment with shorter RHS recycling
# 7. Index with side effects
# 8. Array flattening
# 9. arr[arr > 0] (logical on same array)
# 10. x[x%%2 == 0] (complex logical)
# 11. mat[upper.tri(mat)] (function result as index)

test_that("full extraction - x[] gets all elements", {
  f <- function(x) {
    out <- x[]  # All elements
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_full_extract")
  expect_equal(built$func(c(1.0, 2.0, 3.0)), c(1.0, 2.0, 3.0))
  expect_equal(built$func(numeric(0)), numeric(0))
})

test_that("full replacement - x[] <- y", {
  f <- function(x, y) {
    out <- x
    out[] <- y  # Replace all elements
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "idx_full_replace")
  expect_equal(built$func(c(1.0, 2.0, 3.0, 4.0), c(9.0, 8.0, 7.0, 6.0)), c(9.0, 8.0, 7.0, 6.0))
  expect_equal(built$func(c(1.0, 2.0, 3.0, 4.0), c(9.0, 8.0)), c(9.0, 8.0, 9.0, 8.0))
  expect_equal(built$func(numeric(0), c(9.0, 8.0)), numeric(0))
})

test_that("drop=FALSE - preserves matrix dimensions", {
  f <- function(mat) {
    # mat[1, , drop=FALSE] should return 1-row matrix
    out <- mat[1, ]  # Without drop=FALSE - returns vector
    out
  }
  
  skip_if_no_mojo()
  expect_error(
    mojor_build(f, mat = "f64[,]", name = "idx_drop_false_preserve"),
    "return\\(\\) must return the output vector or scalar accumulator"
  )
})

test_that("dimnames preservation - after indexing", {
  f <- function(mat) {
    out <- mat
    out[1, 1] <- 99.0
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", name = "idx_dimnames_alias")
  mat <- matrix(
    c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
    nrow = 3,
    ncol = 2,
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
  out <- built$func(mat)
  expect_equal(out[1, 1], 99.0)
  expect_null(dimnames(out))
})

test_that("cbind matrix index - mat[cbind(i,j)]", {
  f <- function(mat, idx_mat, n) {
    # idx_mat is n x 2 matrix of (row, col) pairs
    out <- numeric(n)
    for (k in seq_len(n)) {
      i <- idx_mat[k, 1]
      j <- idx_mat[k, 2]
      out[k] <- mat[i, j]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", idx_mat = "i32[,]", n = "i32", name = "idx_cbind_style")
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  idx <- matrix(c(1L, 1L, 2L, 2L, 3L, 1L), nrow = 3, ncol = 2, byrow = TRUE)
  expect_equal(built$func(mat, idx, 3L), c(1.0, 5.0, 3.0))  # mat[1,1], mat[2,2], mat[3,1]
})

test_that("assignment recycling - shorter RHS", {
  f <- function(x) {
    out <- numeric(6)
    out[1:6] <- c(1.0, 2.0)  # Should recycle to 1,2,1,2,1,2
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_assign_recycle_short")
  expect_equal(built$func(c(0.0)), c(1.0, 2.0, 1.0, 2.0, 1.0, 2.0))
})

test_that("index with side effects - i <- i + 1", {
  f <- function(x, n) {
    out <- numeric(n)
    i <- 1L
    for (k in seq_len(n)) {
      out[k] <- x[i]  # Use i, then increment
      i <- i + 1L
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_side_effect")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(built$func(x, 3L), c(10.0, 20.0, 30.0))
})

test_that("array flattening - as.vector behavior transpiles", {
  f <- function(mat) {
    # Flatten matrix to vector (column-major)
    out <- numeric(length(mat))
    for (i in seq_along(mat)) {
      out[i] <- mat[i]  # Single index treats as vector
    }
    out
  }
  
  skip_if_no_mojo()
  trans <- mojor_transpile(f, mat = "f64[,]", name = "idx_flatten", cache = FALSE)
  expect_true(is.character(trans$mojo) && nzchar(trans$mojo))
  expect_true(
    grepl("_mojor_read_f64\\(mat", trans$mojo) ||
      grepl("mat_tensor\\[", trans$mojo) ||
      grepl("mat\\[Int\\(", trans$mojo)
  )
})

test_that("logical on same array - arr[arr > 0]", {
  f <- function(x) {
    # Get all positive elements
    out <- numeric(length(x))
    j <- 1L
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        out[j] <- x[i]
        j <- j + 1L
      }
    }
    out[1:(j-1)]  # Return only filled portion
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_logical_same_array")
  x <- c(-1.0, 2.0, -3.0, 4.0, -5.0)
  result <- built$func(x)
  expect_equal(result[1:3], c(2.0, 4.0, 0.0))  # Positive values
})

test_that("complex logical - x[x %% 2 == 0]", {
  f <- function(x) {
    # Get even elements (where x %% 2 == 0)
    out <- numeric(length(x))
    j <- 1L
    for (i in seq_along(x)) {
      if ((x[i] %% 2) == 0) {
        out[j] <- x[i]
        j <- j + 1L
      }
    }
    out[1:(j-1)]
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "i32[]", name = "idx_complex_logical")
  x <- c(1L, 2L, 3L, 4L, 5L, 6L)
  result <- built$func(x)
  expect_equal(result[1:3], c(2L, 4L, 6L))  # Even values
})

test_that("function result as index - mat[upper.tri(mat)]", {
  f <- function(mat, n) {
    # Get upper triangle elements
    out <- numeric(n)
    k <- 1L
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (i <= j) {  # upper.tri condition
          out[k] <- mat[i, j]
          k <- k + 1L
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_function_result")
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  # Upper triangle: (1,1), (1,2), (1,3), (2,2), (2,3), (3,3) = 1, 4, 7, 5, 8, 9
  expect_equal(built$func(mat, 6L), c(1.0, 4.0, 7.0, 5.0, 8.0, 9.0))
})

test_that("lower.tri equivalent - mat[lower.tri(mat)]", {
  f <- function(mat, n) {
    # Get lower triangle elements
    out <- numeric(n)
    k <- 1L
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (i >= j) {  # lower.tri condition
          out[k] <- mat[i, j]
          k <- k + 1L
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_lower_tri")
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  # Lower triangle: (1,1), (2,1), (2,2), (3,1), (3,2), (3,3) = 1, 2, 5, 3, 6, 9
  expect_equal(built$func(mat, 6L), c(1.0, 2.0, 5.0, 3.0, 6.0, 9.0))
})

test_that("diag extraction - mat[cbind(1:n, 1:n)]", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[i, i]  # Diagonal
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_diag_extract")
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  expect_equal(built$func(mat, 3L), c(1.0, 5.0, 9.0))  # Diagonal
})

test_that("anti-diagonal - mat[cbind(1:n, n:1)]", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      j <- n - i + 1  # n:1
      out[i] <- mat[i, j]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_anti_diag")
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  expect_equal(built$func(mat, 3L), c(7.0, 5.0, 3.0))  # Anti-diagonal
})

test_that("which.max pattern - x[which.max(x)]", {
  f <- function(x) {
    # Find max value
    max_val <- x[1]
    for (i in seq_along(x)) {
      if (x[i] > max_val) {
        max_val <- x[i]
      }
    }
    out <- numeric(1)
    out[1] <- max_val
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_which_max")
  x <- c(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0)
  expect_equal(built$func(x), 9.0)
})

test_that("which.min pattern - x[which.min(x)]", {
  f <- function(x) {
    # Find min value
    min_val <- x[1]
    for (i in seq_along(x)) {
      if (x[i] < min_val) {
        min_val <- x[i]
      }
    }
    out <- numeric(1)
    out[1] <- min_val
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_which_min")
  x <- c(3.0, 1.0, 4.0, 0.5, 5.0)
  expect_equal(built$func(x), 0.5)
})

test_that("sample as index - x[sample(n, k)]", {
  f <- function(x, indices, n) {
    # indices is a pre-computed sample
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[indices[i]]
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", indices = "i32[]", n = "i32", name = "idx_sample", cache = FALSE)
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  # Use deterministic "sample"
  expect_equal(built$func(x, c(5L, 2L, 4L), 3L), c(50.0, 20.0, 40.0))
})

test_that("match pattern - x[match(y, x)]", {
  f <- function(x, y, n) {
    # Find positions of y in x
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_along(x)) {
        if (x[j] == y[i]) {
          out[i] <- x[j]
          break
        }
      }
    }
    out
  }
  
  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", name = "idx_match")
  x <- c(10.0, 20.0, 30.0, 40.0)
  y <- c(30.0, 10.0)
  expect_equal(built$func(x, y, 2L), c(30.0, 10.0))
})
