library(testthat)

golden_path_reduction <- function(name) {
  .mojor_test_golden_path(name)
}

read_golden_reduction <- function(name) {
  .mojor_test_read_golden(name)
}

expect_golden_reduction <- function(actual, name) {
  expect_mojor_golden(actual, name)
}

extract_reduction_chunk_reduction <- function(mojo, marker) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep(marker, lines, fixed = TRUE)
  end <- grep("^\\s*[A-Za-z_][A-Za-z0-9_]*_out\\[0\\] = [A-Za-z_][A-Za-z0-9_]*\\s*$", lines)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[[length(end)]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

extract_nested_reduction_chunk_reduction <- function(mojo, marker) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep(marker, lines, fixed = TRUE)
  end <- grep("^\\s*out\\[Int\\(\\(j - 1\\)\\)\\] = .+\\s*$", lines, perl = TRUE)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[end >= start]
  if (length(end) == 0) return(NULL)
  end <- end[[1]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

test_that("golden file: simd reduction i32 which.max chunk", {  f <- function(x) {
    which.max(x)
  }
  mojo <- mojor_transpile(
    f,
    x = "i32[]",
    reduction = "simd",
    na_mode = "forbid",
    ir_only = TRUE
  )$mojo
  chunk <- extract_reduction_chunk_reduction(
    mojo,
    "# SIMD arg-reduction for which.max(x) (tie-stable: first index)"
  )
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden_reduction(chunk, "simd_reduction_which_max_i32.mojo")
})

test_that("golden file: nested tree reduction product chunk", {  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 1
      for (i in seq_along(x)) acc <- acc * x[i]
      out[j] <- acc
    }
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    g = "i32",
    reduction = "tree",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_nested_reduction_chunk_reduction(mojo, "# Tree reduction for product(x)")
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden_reduction(chunk, "nested_tree_reduction_product_vector.mojo")
})

test_that("golden file: nested simd reduction product chunk", {  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 1
      for (i in seq_along(x)) acc <- acc * x[i]
      out[j] <- acc
    }
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    g = "i32",
    reduction = "simd",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_nested_reduction_chunk_reduction(mojo, "# SIMD reduction for product(x)")
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden_reduction(chunk, "nested_simd_reduction_product_vector.mojo")
})
