library(testthat)

# ---- Reduction Pattern Tests ----

test_that("reduction parameter is validated", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  
  # Valid values
  expect_error(mojor_transpile(f, x = "f64[]", reduction = "auto"), NA)
  expect_error(mojor_transpile(f, x = "f64[]", reduction = "linear"), NA)
  expect_error(mojor_transpile(f, x = "f64[]", reduction = "tree"), NA)
  expect_error(mojor_transpile(f, x = "f64[]", reduction = "simd"), NA)
  
  # Invalid value
  expect_error(mojor_transpile(f, x = "f64[]", reduction = "parallel"), "should be one of")
})

test_that("reduction parameter is returned in transpile output", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  
  trans <- mojor_transpile(f, x = "f64[]", reduction = "tree")
  expect_equal(trans$reduction, "tree")
  
  trans_default <- mojor_transpile(f, x = "f64[]")
  expect_equal(trans_default$reduction, "auto")
})

test_that("sum reduction works with different reduction modes", {  skip_if_no_mojo()
  
  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  
  # Test all reduction modes on non-multiple length
  for (mode in c("auto", "linear", "tree", "simd")) {
    built <- mojor_build(f, x = "f64[]", name = paste0("t_red_sum_", mode), reduction = mode, cache = FALSE, load = TRUE)
    x <- as.double(1:103)
    x_na <- as.double(c(1, NA_real_, 2))
    expect_equal(built$func(x), sum(x), info = paste("reduction mode:", mode))
    expect_error(built$func(x_na), "NA/NaN not supported", info = paste("sum NA forbid:", mode))
  }
})

test_that("f32 sum reduction works with different reduction modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f <- function(x) {
    acc <- 0.0
    for (i in seq_along(x)) {
      acc <- acc + as.double(x[i])
    }
    acc
  }
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built <- mojor_build(f, x = "f32[]", name = paste0("t_red_sum_f32_", mode), reduction = mode, cache = FALSE, load = TRUE)
    x <- float::fl(as.double(1:103))
    x_na <- float::fl(c(1, NA_real_, 2))
    expect_equal(built$func(x), sum(float::dbl(x)), tolerance = 1e-6, info = paste("reduction mode:", mode))
    expect_error(built$func(x_na), "NA/NaN not supported", info = paste("f32 sum NA forbid:", mode))
  }
})

test_that("min/max reduction works with different modes", {  skip_if_no_mojo()
  
  # Use built-in min/max which are recognized by transpiler
  f_min <- function(x) {
    min(x)
  }
  
  f_max <- function(x) {
    max(x)
  }
  
  x <- as.double(c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13))
  x_na <- as.double(c(3, NA_real_, 1))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_min, x = "f64[]", name = paste0("t_red_min_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_max, x = "f64[]", name = paste0("t_red_max_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(built_min$func(x), min(x), info = paste("min reduction mode:", mode))
    expect_equal(built_max$func(x), max(x), info = paste("max reduction mode:", mode))
    expect_error(built_min$func(x_na), "NA/NaN not supported", info = paste("min NA forbid:", mode))
    expect_error(built_max$func(x_na), "NA/NaN not supported", info = paste("max NA forbid:", mode))
  }
})

test_that("f32 min/max reduction works with different modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f_min <- function(x) {
    min(x)
  }
  
  f_max <- function(x) {
    max(x)
  }
  
  vals <- c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13, -7, 10, 12, -1, 14, 15, 25, 16)
  x <- float::fl(as.double(vals))
  x_na <- float::fl(c(3, NA_real_, 1))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_min, x = "f32[]", name = paste0("t_red_min_f32_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_max, x = "f32[]", name = paste0("t_red_max_f32_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(as.double(built_min$func(x)), min(float::dbl(x)), tolerance = 1e-6, info = paste("f32 min reduction mode:", mode))
    expect_equal(as.double(built_max$func(x)), max(float::dbl(x)), tolerance = 1e-6, info = paste("f32 max reduction mode:", mode))
    expect_error(built_min$func(x_na), "NA/NaN not supported", info = paste("f32 min NA forbid:", mode))
    expect_error(built_max$func(x_na), "NA/NaN not supported", info = paste("f32 max NA forbid:", mode))
  }
})

test_that("which.min/which.max reductions work with different modes", {  skip_if_no_mojo()
  
  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  x <- as.double(c(5, 1, 7, 3, 9, 2, 8, 4, 6, 0, 10))
  x_na <- as.double(c(5, NA_real_, 7))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f64[]", name = paste0("t_red_which_min_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f64[]", name = paste0("t_red_which_max_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(as.integer(built_min$func(x)), which.min(x), info = paste("which.min reduction mode:", mode))
    expect_equal(as.integer(built_max$func(x)), which.max(x), info = paste("which.max reduction mode:", mode))
    expect_error(built_min$func(x_na), "NA/NaN not supported", info = paste("which.min NA forbid:", mode))
    expect_error(built_max$func(x_na), "NA/NaN not supported", info = paste("which.max NA forbid:", mode))
  }
})

test_that("which.min/which.max tie-break to first occurrence across modes", {  skip_if_no_mojo()
  
  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  x_min_dup <- as.double(c(3, 1, 2, 1, 4))
  x_max_dup <- as.double(c(5, 2, 5, 1, 5))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f64[]", name = paste0("t_red_which_min_tie_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f64[]", name = paste0("t_red_which_max_tie_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(as.integer(built_min$func(x_min_dup)), which.min(x_min_dup), info = paste("which.min tie:", mode))
    expect_equal(as.integer(built_max$func(x_max_dup)), which.max(x_max_dup), info = paste("which.max tie:", mode))
  }
})

test_that("f32 which.min/which.max reductions work with different modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  vals <- c(5, 1, 7, 3, 9, 2, 8, 4, 6, 0, 10, -1, 12)
  x <- float::fl(as.double(vals))
  x_na <- float::fl(c(5, NA_real_, 7))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f32[]", name = paste0("t_red_which_min_f32_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f32[]", name = paste0("t_red_which_max_f32_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(as.integer(built_min$func(x)), which.min(float::dbl(x)), info = paste("f32 which.min reduction mode:", mode))
    expect_equal(as.integer(built_max$func(x)), which.max(float::dbl(x)), info = paste("f32 which.max reduction mode:", mode))
    expect_error(built_min$func(x_na), "NA/NaN not supported", info = paste("f32 which.min NA forbid:", mode))
    expect_error(built_max$func(x_na), "NA/NaN not supported", info = paste("f32 which.max NA forbid:", mode))
  }
})

test_that("f32 which.min/which.max tie-break to first occurrence across modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  x_min_dup <- float::fl(as.double(c(3, 1, 2, 1, 4)))
  x_max_dup <- float::fl(as.double(c(5, 2, 5, 1, 5)))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f32[]", name = paste0("t_red_which_min_f32_tie_", mode), reduction = mode, cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f32[]", name = paste0("t_red_which_max_f32_tie_", mode), reduction = mode, cache = FALSE, load = TRUE)
    expect_equal(as.integer(built_min$func(x_min_dup)), which.min(float::dbl(x_min_dup)), info = paste("f32 which.min tie:", mode))
    expect_equal(as.integer(built_max$func(x_max_dup)), which.max(float::dbl(x_max_dup)), info = paste("f32 which.max tie:", mode))
  }
})

test_that("na_mode='unsafe' allows NA for min/max reductions across modes", {  skip_if_no_mojo()

  f_min <- function(x) {
    min(x)
  }
  
  f_max <- function(x) {
    max(x)
  }
  
  x_na <- as.double(c(3, NA_real_, 1))
  x_empty <- numeric(0)
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_min, x = "f64[]", name = paste0("t_red_min_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_max, x = "f64[]", name = paste0("t_red_max_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built_min$func(x_na))
    expect_silent(built_max$func(x_na))
    expect_warning(min_empty <- built_min$func(x_empty), "no non-missing arguments to min; returning Inf", fixed = TRUE, info = paste("min empty warning:", mode))
    expect_warning(max_empty <- built_max$func(x_empty), "no non-missing arguments to max; returning -Inf", fixed = TRUE, info = paste("max empty warning:", mode))
    expect_identical(min_empty, Inf, info = paste("min empty result:", mode))
    expect_identical(max_empty, -Inf, info = paste("max empty result:", mode))
  }
})

test_that("na_mode='unsafe' allows NA for which.min/which.max reductions across modes", {  skip_if_no_mojo()

  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  x_na <- as.double(c(5, NA_real_, 7))
  x_empty <- numeric(0)
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f64[]", name = paste0("t_red_which_min_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f64[]", name = paste0("t_red_which_max_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built_min$func(x_na))
    expect_silent(built_max$func(x_na))
    if (mode != "auto") {
      expect_warning(min_empty <- built_min$func(x_empty), NA, info = paste("which.min empty:", mode))
      expect_warning(max_empty <- built_max$func(x_empty), NA, info = paste("which.max empty:", mode))
      expect_identical(min_empty, integer(0), info = paste("which.min empty result:", mode))
      expect_identical(max_empty, integer(0), info = paste("which.max empty result:", mode))
    }
  }
})

test_that("na_mode='unsafe' allows NA for sum reductions across modes", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  
  x_na <- as.double(c(1, NA_real_, 2))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built <- mojor_build(f, x = "f64[]", name = paste0("t_red_sum_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built$func(x_na))
  }
})

test_that("na_mode='unsafe' allows NA for f32 sum reductions across modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f <- function(x) {
    acc <- 0.0
    for (i in seq_along(x)) {
      acc <- acc + as.double(x[i])
    }
    acc
  }
  
  x_na <- float::fl(c(1, NA_real_, 2))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built <- mojor_build(f, x = "f32[]", name = paste0("t_red_sum_f32_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built$func(x_na))
  }
})

test_that("na_mode='unsafe' allows NA for f32 min/max reductions across modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f_min <- function(x) {
    min(x)
  }
  
  f_max <- function(x) {
    max(x)
  }
  
  x_na <- float::fl(c(3, NA_real_, 1))
  x_empty <- float::fl(numeric(0))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_min, x = "f32[]", name = paste0("t_red_min_f32_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_max, x = "f32[]", name = paste0("t_red_max_f32_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built_min$func(x_na))
    expect_silent(built_max$func(x_na))
    expect_warning(min_empty <- built_min$func(x_empty), "no non-missing arguments to min; returning Inf", fixed = TRUE, info = paste("f32 min empty warning:", mode))
    expect_warning(max_empty <- built_max$func(x_empty), "no non-missing arguments to max; returning -Inf", fixed = TRUE, info = paste("f32 max empty warning:", mode))
    expect_identical(as.double(min_empty), Inf, info = paste("f32 min empty result:", mode))
    expect_identical(as.double(max_empty), -Inf, info = paste("f32 max empty result:", mode))
  }
})

test_that("na_mode='unsafe' allows NA for f32 which.min/which.max reductions across modes", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f_which_min <- function(x) {
    which.min(x)
  }
  
  f_which_max <- function(x) {
    which.max(x)
  }
  
  x_na <- float::fl(c(5, NA_real_, 7))
  x_empty <- float::fl(numeric(0))
  
  for (mode in c("auto", "linear", "tree", "simd")) {
    built_min <- mojor_build(f_which_min, x = "f32[]", name = paste0("t_red_which_min_f32_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    built_max <- mojor_build(f_which_max, x = "f32[]", name = paste0("t_red_which_max_f32_unsafe_", mode), reduction = mode, na_mode = "unsafe", cache = FALSE, load = TRUE)
    expect_silent(built_min$func(x_na))
    expect_silent(built_max$func(x_na))
    if (mode != "auto") {
      expect_warning(min_empty <- built_min$func(x_empty), NA, info = paste("f32 which.min empty:", mode))
      expect_warning(max_empty <- built_max$func(x_empty), NA, info = paste("f32 which.max empty:", mode))
      expect_identical(min_empty, integer(0), info = paste("f32 which.min empty result:", mode))
      expect_identical(max_empty, integer(0), info = paste("f32 which.max empty result:", mode))
    }
  }
})
