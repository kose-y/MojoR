library(testthat)

tier8_golden_path <- function(name) {
  .mojor_test_golden_path(name)
}

read_tier8_golden <- function(name) {
  .mojor_test_read_golden(name)
}

expect_tier8_golden <- function(actual, name) {
  expect_mojor_golden(actual, name)
}

extract_tier8_hof_loop_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("^\\s*for\\s+[A-Za-z_][A-Za-z0-9_]*\\s+in\\s+range\\((Int\\()?n_[A-Za-z0-9_]+\\)?\\):\\s*$", lines)
  end <- grep("^\\s*return Int32\\(n_[A-Za-z0-9_]+\\)\\s*$", lines)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[[length(end)]]
  if (end < start) return(NULL)
  chunk <- paste(lines[start:end], collapse = "\n")
  chunk <- gsub("__mojor_hof_i[0-9]*", "i", chunk)
  gsub("range\\(Int\\((n_[A-Za-z0-9_]+)\\)\\)", "range(\\1)", chunk)
}

extract_tier8_hof_mapply_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("^\\s*if n_[A-Za-z0-9_]+ != n_[A-Za-z0-9_]+:\\s*$", lines)
  end <- grep("^\\s*return Int32\\(n_[A-Za-z0-9_]+\\)\\s*$", lines)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[[length(end)]]
  if (end < start) return(NULL)
  chunk <- paste(lines[start:end], collapse = "\n")
  chunk <- gsub("__mojor_hof_i[0-9]*", "i", chunk)
  gsub("range\\(Int\\((n_[A-Za-z0-9_]+)\\)\\)", "range(\\1)", chunk)
}

test_that("golden file: compiled subset vapply loop chunk", {  trans <- mojor_transpile(
    function(x) vapply(x, function(v) v * 2, FUN.VALUE = numeric(1)),
    x = "f64[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_vapply_golden"
  )
  chunk <- extract_tier8_hof_loop_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_vapply_loop.mojo")
})

test_that("golden file: compiled subset lapply loop chunk", {  trans <- mojor_transpile(
    function(x) lapply(x, function(v) v - 1),
    x = "f64[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_lapply_golden"
  )
  chunk <- extract_tier8_hof_loop_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_lapply_loop.mojo")
})

test_that("golden file: compiled subset mapply chunk", {  trans <- mojor_transpile(
    function(x, y) mapply(function(a, b) a + b, x, y),
    x = "f64[]",
    y = "f64[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_mapply_golden"
  )
  chunk <- extract_tier8_hof_mapply_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_mapply2_loop.mojo")
})

test_that("golden file: compiled subset mapply 3-arg chunk", {  trans <- mojor_transpile(
    function(x, y, z) mapply(function(a, b, c) a + b - c, x, y, z),
    x = "f64[]",
    y = "f64[]",
    z = "f64[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_mapply3_golden"
  )
  chunk <- extract_tier8_hof_mapply_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_mapply3_loop.mojo")
})

test_that("golden file: compiled subset vapply i32 loop chunk", {  trans <- mojor_transpile(
    function(x) vapply(x, function(v) v + 1L, FUN.VALUE = integer(1)),
    x = "i32[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_vapply_i32_golden"
  )
  chunk <- extract_tier8_hof_loop_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_vapply_i32_loop.mojo")
})

test_that("golden file: compiled subset sapply lgl loop chunk", {  trans <- mojor_transpile(
    function(x) sapply(x, function(v) !v),
    x = "lgl[]",
    object_mode = "off",
    ir_only = TRUE,
    name = "t_tier8_sapply_lgl_golden"
  )
  chunk <- extract_tier8_hof_loop_chunk(trans$mojo)
  expect_false(is.null(chunk))
  expect_tier8_golden(chunk, "hof_sapply_lgl_loop.mojo")
})
