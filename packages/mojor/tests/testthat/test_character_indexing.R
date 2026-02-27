library(testthat)

# Tests for character indexing with dimnames

test_that("dimnames are parsed in matrix creation", {  f <- function(x, n) {
    out <- matrix(0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  expect_error(mojor_transpile(f, x = "f64[]", n = "i32", name = "t_dimnames"), NA)
})

test_that("computed matrix dimnames expressions are parsed in strict lanes", {  f <- function(x) {
    out <- matrix(
      0.0,
      2,
      2,
      dimnames = list(paste0("r", 1:2), LETTERS[1:2])
    )
    for (i in 1:2) {
      for (j in 1:2) {
        out[i, j] <- x[i, j]
      }
    }
    out["r1", "A"] <- out["r2", "B"]
    out
  }

  expect_error(
    mojor_transpile(f, x = "f64[,]", name = "t_dimnames_matrix_computed"),
    NA
  )
})

test_that("computed array dimnames expressions are parsed in strict lanes", {  f <- function(x) {
    out <- array(
      0.0,
      dim = c(2, 2, 2),
      dimnames = list(
        paste0("r", 1:2),
        paste0("c", seq_len(2)),
        c("k1", paste0("k", 2))
      )
    )
    for (i in 1:2) {
      for (j in 1:2) {
        for (k in 1:2) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out["r1", "c2", "k1"] <- out["r2", "c1", "k2"]
    out
  }

  expect_error(
    mojor_transpile(f, x = "f64[]", name = "t_dimnames_array_computed"),
    NA
  )
})

test_that("character indexing works for write in loops", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == 1 && j == 1) {
          out["a", "c"] <- 999.0
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_write",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  expect_equal(result[1, 1], 999.0)
  expect_equal(result[2, 1], x[2, 1])
  expect_equal(result[1, 2], x[1, 2])
  expect_equal(result[2, 2], x[2, 2])
})

test_that("character indexing works for read in loops", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == 2 && j == 2) {
          # Copy from [1,1] to [2,2] using character names
          out["b", "d"] <- out["a", "c"]
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_read",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  # out["a", "c"] is x[1,1] = 1.0
  # out["b", "d"] should become 1.0 (copied from [1,1])
  expect_equal(result[1, 1], x[1, 1])
  expect_equal(result[2, 2], x[1, 1])  # copied from [1,1]
})

test_that("character indexing works with named index args", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out[rows = "a", cols = "c"] <- out[rows = "b", cols = "d"]
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_named_args",
                       cache = FALSE, load = TRUE)

  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)

  expect_equal(result[1, 1], x[2, 2])
  expect_equal(result[2, 2], x[2, 2])
})

test_that("array dimnames are parsed in array creation", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- array(
      0.0,
      dim = c(n, n, 2),
      dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
    )
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in 1:2) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_array_dimnames",
                       cache = FALSE, load = TRUE)

  x <- array(as.double(1:8), dim = c(2, 2, 2))
  got <- built$func(x, 2L)
  ref <- f(x, 2L)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("ND character indexing works for read/write", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- array(
      0.0,
      dim = c(n, n, 2),
      dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
    )
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in 1:2) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out["r1", "c2", "k1"] <- out["r2", "c1", "k2"]
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_nd",
                       cache = FALSE, load = TRUE)

  x <- array(as.double(1:8), dim = c(2, 2, 2))
  got <- built$func(x, 2L)
  ref <- f(x, 2L)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("ND character indexing works with named index args", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- array(
      0.0,
      dim = c(n, n, 2),
      dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
    )
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in 1:2) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out[rows = "r1", cols = "c2", deps = "k1"] <- out[rows = "r2", cols = "c1", deps = "k2"]
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_nd_named",
                       cache = FALSE, load = TRUE)

  x <- array(as.double(1:8), dim = c(2, 2, 2))
  got <- built$func(x, 2L)
  ref <- f(x, 2L)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("character vector dimname indexing works for direct submatrix assignment", {  skip_if_no_mojo()
  f <- function(m) {
    mat <- matrix(
      0.0,
      2,
      2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
    mat[c("r1", "r2"), c("c2", "c1")] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_submatrix"
  )

  m <- as.double(c(10, 20, 30, 40))
  got <- built$func(m)
  ref <- f(m)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("computed literal character vector indices work with named args", {  skip_if_no_mojo()
  f <- function(m) {
    mat <- matrix(
      0.0,
      2,
      2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
    mat[rows = c("r1", paste0("r", 2)), cols = c("c2", "c1")] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_submatrix_named"
  )

  m <- as.double(c(7, 8, 9, 10))
  got <- built$func(m)
  ref <- f(m)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("runtime character vector index vars work for submatrix assignment", {  skip_if_no_mojo()
  f <- function(m) {
    mat <- matrix(
      0.0,
      2,
      2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
    rows <- c("r1", "r2")
    cols <- c("c2", "c1")
    mat[rows, cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_runtime_vars"
  )

  m <- as.double(c(11, 12, 13, 14))
  got <- built$func(m)
  ref <- f(m)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("function chr[] index args work for submatrix assignment", {  skip_if_no_mojo()
  f <- function(m, rows, cols) {
    mat <- matrix(
      0.0,
      2,
      2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
    mat[rows, cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    rows = "chr[]",
    cols = "chr[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_arg_vars"
  )

  m <- as.double(c(21, 22, 23, 24))
  rows <- c("r1", "r2")
  cols <- c("c2", "c1")
  got <- built$func(m, rows, cols)
  ref <- f(m, rows, cols)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("literal empty character dimname index vectors are supported", {  skip_if_no_mojo()
  f <- function(x) {
    mat <- matrix(
      0.0,
      2,
      2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
    mat[character(0), "c1"] <- 7.0
    mat
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_empty_literal"
  )

  x <- as.double(c(1, 2))
  got <- built$func(x)
  ref <- f(x)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("ND character vector dimname indexing works for direct assignment", {  skip_if_no_mojo()
  f <- function(v) {
    arr <- array(
      0.0,
      dim = c(2, 2, 2),
      dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
    )
    arr[c("r1", "r2"), "c1", c("k2", "k1")] <- v
    arr
  }

  built <- mojor_build(
    f,
    v = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_char_vec_nd_assign"
  )

  v <- as.double(c(3, 4, 5, 6))
  got <- built$func(v)
  ref <- f(v)
  expect_equal(unname(got), unname(ref))
  expect_equal(dim(got), dim(ref))
})

test_that("ND character indexing errors for unknown names", {  f <- function(n) {
  out <- array(
    0.0,
    dim = c(n, n, 2),
    dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
  )
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      for (k in 1:2) {
        out[i, j, k] <- 0.0
      }
    }
  }
  out["missing", "c2", "k1"] <- 1.0
  out
}

  expect_error(
    mojor_transpile(f, n = "i32", name = "t_char_nd_err"),
    "not found in dimnames"
  )
})

test_that("character indexing with different row and column names", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("row1", "row2"), c("col1", "col2")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == 1 && j == 1) {
          out["row1", "col1"] <- 111.0
        } else if (i == 2 && j == 2) {
          out["row2", "col2"] <- 222.0
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_char_names",
                       cache = FALSE, load = TRUE)
  
  x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2)
  result <- built$func(x, 2L)
  
  expect_equal(result[1, 1], 111.0)
  expect_equal(result[2, 2], 222.0)
})

test_that("character indexing errors for unknown names", {  f <- function(x, n) {
    out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out["unknown", "c"] <- 999.0  # invalid row name
      }
    }
    out
  }

  expect_error(
    mojor_transpile(f, x = "f64[]", n = "i32", name = "t_char_err"),
    "not found in dimnames"
  )
})

test_that("character indexing requires dimnames", {  # This test checks that the transpilation succeeds (the check happens at runtime
  # in the generated code, not at transpile time, because the assignment might
  # not be reached if it's inside a conditional)
  f <- function(x, n) {
    out <- matrix(0.0, n, n)  # no dimnames
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  # Should transpile fine without dimnames
  expect_error(mojor_transpile(f, x = "f64[]", n = "i32", name = "t_char_nodn"), NA)
})

# ── Feature A: 1D named vector character indexing ──────────────────────────────

test_that("named vector 1D character indexing transpiles", {
  f <- function(n) {
    x <- c(a = 1.0, b = 2.0, c = 3.0)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x["b"]
    }
    out
  }
  expect_error(mojor_transpile(f, n = "i32", name = "t_named_vec_1d"), NA)
})

test_that("named vector 1D multi-select character indexing transpiles", {
  f <- function(n) {
    x <- c(a = 1.0, b = 2.0, c = 3.0)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x["a"] + x["c"]
    }
    out
  }
  expect_error(mojor_transpile(f, n = "i32", name = "t_named_vec_multi"), NA)
})

test_that("named vector 1D character indexing errors for unknown name", {
  f <- function(n) {
    x <- c(a = 1.0, b = 2.0)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x["z"]
    }
    out
  }
  expect_error(mojor_transpile(f, n = "i32", name = "t_named_vec_err"), "not found in dimnames")
})

test_that("named integer vector 1D character indexing transpiles", {
  f <- function(n) {
    x <- c(a = 1L, b = 2L, c = 3L)
    out <- integer(n)
    for (i in seq_len(n)) {
      out[i] <- x["a"]
    }
    out
  }
  expect_error(mojor_transpile(f, n = "i32", name = "t_named_ivec"), NA)
})

test_that("named vector 1D character indexing runs correctly", {
  skip_if_no_mojo()
  f <- function(n) {
    x <- c(a = 10L, b = 20L, c = 30L)
    out <- integer(n)
    for (i in seq_len(n)) {
      out[i] <- x["b"]
    }
    out
  }
  built <- mojor_build(f, n = "i32", name = "t_named_vec_run",
                       cache = FALSE, load = TRUE)
  got <- built$func(3L)
  expect_equal(got, c(20L, 20L, 20L))
})

test_that("named vector 1D multi-name character indexing runs correctly", {
  skip_if_no_mojo()
  f <- function(n) {
    x <- c(a = 10L, b = 20L, c = 30L)
    out <- integer(n)
    for (i in seq_len(n)) {
      out[i] <- x["a"] + x["c"]
    }
    out
  }
  built <- mojor_build(f, n = "i32", name = "t_named_vec_multi_run",
                       cache = FALSE, load = TRUE)
  got <- built$func(2L)
  expect_equal(got, c(40L, 40L))
})

# ========================================================================
# Feature B: Local character variable element-wise indexing in loops
# ========================================================================

test_that("local chr var element-wise loop indexing transpiles", {  skip_if_no_mojo()
  f <- function(n) {
    mat <- matrix(c(10.0, 20.0, 30.0, 40.0), 2, 2,
      dimnames = list(c("r1", "r2"), c("c1", "c2")))
    rows <- c("r1", "r2")
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[rows[i], "c1"]
    }
    out
  }
  expect_error(mojor_transpile(f, n = "i32"), NA)
})

test_that("local chr var element-wise loop indexing runs correctly", {  skip_if_no_mojo()
  f <- function(n) {
    mat <- matrix(c(10.0, 20.0, 30.0, 40.0), 2, 2,
      dimnames = list(c("r1", "r2"), c("c1", "c2")))
    rows <- c("r1", "r2")
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[rows[i], "c1"]
    }
    out
  }
  built <- mojor_build(f, n = "i32",
    name = "t_local_chr_loop", cache = FALSE, load = TRUE)
  got <- built$func(2L)
  # mat column 1 = c(10, 20), rows = c("r1", "r2") -> positions 1, 2
  expect_equal(got, c(10, 20))
})

test_that("local chr var errors for unknown dimname value", {  skip_if_no_mojo()
  f <- function(n) {
    mat <- matrix(c(10.0, 20.0, 30.0, 40.0), 2, 2,
      dimnames = list(c("r1", "r2"), c("c1", "c2")))
    rows <- c("r1", "bad")
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- mat[rows[i], "c1"]
    }
    out
  }
  expect_error(
    mojor_transpile(f, n = "i32"),
    "not found in dimnames"
  )
})
