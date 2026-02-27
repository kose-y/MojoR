library(testthat)

# Tests for loop tiling (blocking) optimization

test_that("tile parameter is validated", {  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] * 2
      }
    }
    out
  }
  
  # Valid tile values
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = 32), NA)
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = c(64, 64)), NA)
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = c(32, 64)), NA)
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = c(32, 64, 16)), NA)
  
  # Invalid tile values
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = 0), "tile values must be positive integers")
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = -1), "tile values must be positive integers")
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = c(32, 0)), "tile values must be positive integers")
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = c(32, 32, 32, 32)), "tile must be a numeric vector of length 1, 2, or 3")
  expect_error(mojor_transpile(f, a = "f64[]", n = "i32", tile = "auto"), "tile must be a numeric vector of length 1, 2, or 3")
})

test_that("tile parameter is returned in transpile output", {  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] * 2
      }
    }
    out
  }
  
  trans <- mojor_transpile(f, a = "f64[]", n = "i32", tile = c(64, 64))
  expect_equal(trans$tile, c(64, 64))
  
  trans_single <- mojor_transpile(f, a = "f64[]", n = "i32", tile = 32)
  expect_equal(trans_single$tile, c(32, 32))  # Single value expanded to square

  trans_3d <- mojor_transpile(f, a = "f64[]", n = "i32", tile = c(64, 32, 16))
  expect_equal(trans_3d$tile, c(64, 32, 16))
  
  trans_default <- mojor_transpile(f, a = "f64[]", n = "i32")
  expect_null(trans_default$tile)
})

test_that("tiling generates tiled loop code", {  skip_if_no_mojo()

  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] * 2
      }
    }
    out
  }
  
  # Build with tiling
  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_tile_32", 
                       tile = 32, cache = FALSE, load = TRUE)
  
  # Verify correctness
  n <- 10L
  a <- matrix(as.double(1:100), n, n)
  expect_equal(built$func(a, n), f(a, n))
  
  # Test with larger matrix that exceeds tile size
  n2 <- 100L
  a2 <- matrix(as.double(1:10000), n2, n2)
  expect_equal(built$func(a2, n2), f(a2, n2))
})

test_that("tiling works with different tile sizes", {  skip_if_no_mojo()

  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] + 1
      }
    }
    out
  }
  
  # Test various tile sizes
  for (tile_size in c(8, 16, 32, 64)) {
    built <- mojor_build(f, a = "f64[]", n = "i32", 
                         name = paste0("t_tile_", tile_size), 
                         tile = tile_size, cache = FALSE, load = TRUE)
    
    n <- 50L
    a <- matrix(as.double(1:2500), n, n)
    expect_equal(built$func(a, n), f(a, n), 
                 info = paste("Failed for tile size", tile_size))
  }
})

test_that("tiling with rectangular tiles", {  skip_if_no_mojo()

  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] * 2 + 1
      }
    }
    out
  }
  
  # Rectangular tiles
  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_tile_rect",
                       tile = c(16, 64), cache = FALSE, load = TRUE)
  
  n <- 100L
  a <- matrix(as.double(1:10000), n, n)
  expect_equal(built$func(a, n), f(a, n))
})

test_that("tiling works with rectangular matrices", {  skip_if_no_mojo()

  f <- function(a, n, m) {
    out <- matrix(0, n, m)
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        out[i, j] <- a[i, j] + i - j
      }
    }
    out
  }
  
  built <- mojor_build(f, a = "f64[]", n = "i32", m = "i32",
                       name = "t_tile_rect_dims", tile = c(16, 32),
                       cache = FALSE, load = TRUE)
  
  n <- 17L
  m <- 29L
  a <- matrix(as.double(seq_len(n * m)), n, m)
  expect_equal(built$func(a, n, m), f(a, n, m))
})

test_that("rectangular tile sizes are mapped by loop depth in codegen", {  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j] + i - j
      }
    }
    out
  }

  trans <- mojor_transpile(
    f,
    a = "f64[]",
    n = "i32",
    tile = c(16, 64),
    na_mode = "unsafe",
    bounds_check = FALSE
  )
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "for _mojor_tile_i in range\\(1, Int\\(n\\) \\+ 1, 16\\):", perl = TRUE)
  expect_match(code, "for _mojor_tile_j in range\\(1, Int\\(n\\) \\+ 1, 64\\):", perl = TRUE)
})

test_that("tiling supports non-matrix single loops", {  skip_if_no_mojo()
  
  # Vector output - tiling should not be applied
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", tile = 32)
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "for _mojor_tile_i in range\\(", perl = TRUE)
  expect_match(code, "for i in range\\(_mojor_tile_i, ", perl = TRUE)

  built <- mojor_build(f, x = "f64[]", name = "t_tile_vec",
                       tile = 32, cache = FALSE, load = TRUE)
  
  x <- as.double(1:100)
  expect_equal(built$func(x), f(x))
})

test_that("tiling supports single flattened loops", {  skip_if_no_mojo()

  # Single loop with matrix input but not nested
  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n * n)) {
      out[i] <- a[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, a = "f64[]", n = "i32", tile = 32)
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "for _mojor_tile_i in range\\(", perl = TRUE)
  expect_match(code, "for i in range\\(_mojor_tile_i, ", perl = TRUE)

  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_tile_single",
                       tile = 32, cache = FALSE, load = TRUE)
  
  n <- 10L
  a <- matrix(as.double(1:100), n, n)
  expect_equal(built$func(a, n), f(a, n))
})

test_that("tile parameter passes through mojor_build to transpile", {  f <- function(a, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- a[i, j]
      }
    }
    out
  }
  
  built <- mojor_build(f, a = "f64[]", n = "i32", name = "t_tile_pass",
                       tile = c(32, 32), cache = FALSE, load = FALSE)
  
  expect_equal(built$trans$tile, c(32, 32))
})

test_that("3D tile vectors are respected for 3 nested loops", {  skip_if_no_mojo()

  f <- function(x, n, m, p) {
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        for (k in seq_len(p)) out[((i - 1) * m + (j - 1)) * p + k] <- x[((i - 1) * m + (j - 1)) * p + k] + 1
      }
    }
    out
  }

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    m = "i32",
    p = "i32",
    tile = c(4, 3, 2),
    na_mode = "unsafe",
    bounds_check = FALSE
  )
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "for _mojor_tile_i in range\\(1, Int\\(n\\) \\+ 1, 4\\):", perl = TRUE)
  expect_match(code, "for _mojor_tile_j in range\\(1, Int\\(m\\) \\+ 1, 3\\):", perl = TRUE)
  expect_match(code, "for _mojor_tile_k in range\\(1, Int\\(p\\) \\+ 1, 2\\):", perl = TRUE)

  built <- mojor_build(
    f,
    x = "f64[]",
    n = "i32",
    m = "i32",
    p = "i32",
    name = "t_tile_3d_depth",
    tile = c(4, 3, 2),
    na_mode = "unsafe",
    bounds_check = FALSE,
    cache = FALSE,
    load = TRUE
  )

  n <- 3L
  m <- 4L
  p <- 5L
  x <- as.double(seq_len(n * m * p))
  expect_equal(built$func(x, n, m, p), f(x, n, m, p))
})
