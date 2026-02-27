library(testthat)

test_that("SIMD min/max reduction (f64) uses current SIMD API and runs", {  skip_if_no_mojo()

  f_min <- function(x) {
    min(x)
  }
  f_max <- function(x) {
    max(x)
  }

  trans <- mojor_transpile(f_min, x = "f64[]", reduction = "simd")
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# SIMD reduction for min(x)", fixed = TRUE)
  expect_match(code, "from sys.info import simd_width_of", fixed = TRUE)
  expect_match(code, "comptime simd_width = simd_width_of[DType.float64]()", fixed = TRUE)
  expect_match(code, "load[width=simd_width]", fixed = TRUE)
  expect_false(grepl("simdwidthof", code, fixed = TRUE))
  expect_false(grepl("for __mojor_ri in range\\(2, n_i \\+ 1\\):", code))

  built_min <- mojor_build(
    f_min,
    x = "f64[]",
    name = "t_red_sched_simd_min_f64",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_max,
    x = "f64[]",
    name = "t_red_sched_simd_max_f64",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13))
  x_na <- as.double(c(3, NA_real_, 1))
  x_empty <- as.double(numeric(0))

  expect_equal(built_min$func(x), min(x))
  expect_equal(built_max$func(x), max(x))
  expect_identical(suppressWarnings(built_min$func(x_empty)), Inf)
  expect_identical(suppressWarnings(built_max$func(x_empty)), -Inf)
  expect_error(built_min$func(x_na), "NA/NaN not supported")
  expect_error(built_max$func(x_na), "NA/NaN not supported")
})

test_that("SIMD min/max reduction (f32) uses current SIMD API and runs", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f_min <- function(x) {
    min(x)
  }
  f_max <- function(x) {
    max(x)
  }

  trans <- mojor_transpile(f_min, x = "f32[]", reduction = "simd")
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# SIMD reduction for min(x)", fixed = TRUE)
  expect_match(code, "from sys.info import simd_width_of", fixed = TRUE)
  expect_match(code, "comptime simd_width = simd_width_of[DType.float32]()", fixed = TRUE)
  expect_match(code, "load[width=simd_width]", fixed = TRUE)
  expect_false(grepl("simdwidthof", code, fixed = TRUE))
  expect_false(grepl("for __mojor_ri in range\\(2, n_i \\+ 1\\):", code))

  built_min <- mojor_build(
    f_min,
    x = "f32[]",
    name = "t_red_sched_simd_min_f32",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_max,
    x = "f32[]",
    name = "t_red_sched_simd_max_f32",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x <- float::fl(as.double(c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13, -7, 25)))
  x_na <- float::fl(c(3, NA_real_, 1))
  x_empty <- float::fl(as.double(numeric(0)))

  expect_equal(as.double(built_min$func(x)), min(float::dbl(x)), tolerance = 1e-6)
  expect_equal(as.double(built_max$func(x)), max(float::dbl(x)), tolerance = 1e-6)
  expect_identical(suppressWarnings(as.double(built_min$func(x_empty))), Inf)
  expect_identical(suppressWarnings(as.double(built_max$func(x_empty))), -Inf)
  expect_error(built_min$func(x_na), "NA/NaN not supported")
  expect_error(built_max$func(x_na), "NA/NaN not supported")

  built_min_unsafe <- mojor_build(
    f_min,
    x = "f32[]",
    name = "t_red_sched_simd_min_f32_unsafe",
    reduction = "simd",
    na_mode = "unsafe",
    cache = FALSE,
    load = TRUE
  )
  built_max_unsafe <- mojor_build(
    f_max,
    x = "f32[]",
    name = "t_red_sched_simd_max_f32_unsafe",
    reduction = "simd",
    na_mode = "unsafe",
    cache = FALSE,
    load = TRUE
  )

  expect_true(is.finite(built_min_unsafe$func(x_na)))
  expect_true(is.finite(built_max_unsafe$func(x_na)))
})

test_that("Tree min/max reduction (f64) runs and handles empty input", {  skip_if_no_mojo()

  f_min <- function(x) {
    min(x)
  }
  f_max <- function(x) {
    max(x)
  }

  trans <- mojor_transpile(f_min, x = "f64[]", reduction = "tree")
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree reduction for min(x)", fixed = TRUE)
  expect_match(code, "if n_i == Int(0):", fixed = TRUE)
  expect_match(code, "acc = _MOJOR_NAN", fixed = TRUE)

  built_min <- mojor_build(
    f_min,
    x = "f64[]",
    name = "t_red_sched_tree_min_f64",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_max,
    x = "f64[]",
    name = "t_red_sched_tree_max_f64",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13))
  x_na <- as.double(c(3, NA_real_, 1))
  x_empty <- as.double(numeric(0))

  expect_equal(built_min$func(x), min(x))
  expect_equal(built_max$func(x), max(x))
  expect_identical(suppressWarnings(built_min$func(x_empty)), Inf)
  expect_identical(suppressWarnings(built_max$func(x_empty)), -Inf)
  expect_error(built_min$func(x_na), "NA/NaN not supported")
  expect_error(built_max$func(x_na), "NA/NaN not supported")
})

test_that("Tree which.max reduction is tie-stable and handles empty input", {  skip_if_no_mojo()

  f_which_max <- function(x) {
    which.max(x)
  }

  trans <- mojor_transpile(f_which_max, x = "f64[]", reduction = "tree")
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)

  built <- mojor_build(
    f_which_max,
    x = "f64[]",
    name = "t_red_sched_tree_which_max_f64",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(5, 2, 5, 1, 5, 3, 5))
  x2 <- as.double(c(9, 1, 7, 9, 4, 9, 2, 8))
  x_na <- as.double(c(5, NA_real_, 7))
  x_empty <- as.double(numeric(0))

  expect_equal(as.integer(built$func(x)), which.max(x))
  expect_equal(as.integer(built$func(x2)), which.max(x2))
  expect_identical(as.integer(built$func(x_empty)), which.max(x_empty))
  expect_error(built$func(x_na), "NA/NaN not supported")
})

test_that("Tree which.min reduction is tie-stable and handles empty input", {  skip_if_no_mojo()

  f_which_min <- function(x) {
    which.min(x)
  }

  trans <- mojor_transpile(f_which_min, x = "f64[]", reduction = "tree")
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)

  built <- mojor_build(
    f_which_min,
    x = "f64[]",
    name = "t_red_sched_tree_which_min_f64",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(3, 1, 4, 1, 5, 1, 2))
  x2 <- as.double(c(9, 7, 2, 5, 2, 8, 2, 6))
  x_na <- as.double(c(5, NA_real_, 7))
  x_empty <- as.double(numeric(0))

  expect_equal(as.integer(built$func(x)), which.min(x))
  expect_equal(as.integer(built$func(x2)), which.min(x2))
  expect_identical(as.integer(built$func(x_empty)), which.min(x_empty))
  expect_error(built$func(x_na), "NA/NaN not supported")
})

test_that("Tree/SIMD sum/product reductions (f64) run and handle empty input", {  skip_if_no_mojo()

  f_sum <- function(x) {
    sum(x)
  }
  f_prod <- function(x) {
    prod(x)
  }

  for (mode in c("tree", "simd")) {
    trans_sum <- mojor_transpile(f_sum, x = "f64[]", reduction = mode)
    trans_prod <- mojor_transpile(f_prod, x = "f64[]", reduction = mode)
    code_sum <- paste(trans_sum$mojo, collapse = "\n")
    code_prod <- paste(trans_prod$mojo, collapse = "\n")
    marker_prefix <- if (mode == "simd") "SIMD" else "Tree"
    expect_match(code_sum, paste0("# ", marker_prefix, " reduction for sum(x)"), fixed = TRUE)
    expect_match(code_prod, paste0("# ", marker_prefix, " reduction for product(x)"), fixed = TRUE)

    built_sum <- mojor_build(
      f_sum,
      x = "f64[]",
      name = paste0("t_red_sched_", mode, "_sum_f64"),
      reduction = mode,
      cache = FALSE,
      load = TRUE
    )
    built_prod <- mojor_build(
      f_prod,
      x = "f64[]",
      name = paste0("t_red_sched_", mode, "_prod_f64"),
      reduction = mode,
      cache = FALSE,
      load = TRUE
    )

    x <- as.double(c(3, 1, 4, 1, 5, 9, 2, 6, 7, 8, 0, 11, 13))
    x_na <- as.double(c(3, NA_real_, 1))
    x_empty <- as.double(numeric(0))

    expect_equal(built_sum$func(x), sum(x))
    expect_equal(built_prod$func(x), prod(x))
    expect_equal(built_sum$func(x_empty), 0.0)
    expect_equal(built_prod$func(x_empty), 1.0)
    expect_error(built_sum$func(x_na), "NA/NaN not supported")
    expect_error(built_prod$func(x_na), "NA/NaN not supported")
  }
})

test_that("SIMD reduction mode schedules i32 sum/product and runs", {  skip_if_no_mojo()

  f_sum <- function(x) {
    sum(x)
  }
  f_prod <- function(x) {
    prod(x)
  }

  trans_sum <- mojor_transpile(f_sum, x = "i32[]", reduction = "simd")
  trans_prod <- mojor_transpile(f_prod, x = "i32[]", reduction = "simd")
  code_sum <- paste(trans_sum$mojo, collapse = "\n")
  code_prod <- paste(trans_prod$mojo, collapse = "\n")
  expect_match(code_sum, "# SIMD reduction for sum(x)", fixed = TRUE)
  expect_match(code_prod, "# SIMD reduction for product(x)", fixed = TRUE)
  expect_match(code_sum, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_prod, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)

  built_sum <- mojor_build(
    f_sum,
    x = "i32[]",
    name = "t_red_sched_simd_sum_i32",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )
  built_prod <- mojor_build(
    f_prod,
    x = "i32[]",
    name = "t_red_sched_simd_prod_i32",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(2L, 3L, 4L, 5L))
  x_empty <- as.integer(integer(0))

  expect_equal(built_sum$func(x), sum(x))
  expect_equal(built_prod$func(x), prod(x))
  expect_equal(built_sum$func(x_empty), 0L)
  expect_equal(built_prod$func(x_empty), 1L)
})

.mojor_test_run_simd_which_case <- function(cfg) {
  f_which_min <- function(x) {
    which.min(x)
  }
  f_which_max <- function(x) {
    which.max(x)
  }
  trans_min <- mojor_transpile(f_which_min, x = cfg$x_type, reduction = "simd")
  trans_max <- mojor_transpile(f_which_max, x = cfg$x_type, reduction = "simd")
  code_min <- paste(trans_min$mojo, collapse = "\n")
  code_max <- paste(trans_max$mojo, collapse = "\n")
  expect_match(code_min, "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max, "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min, cfg$simd_width_sig, fixed = TRUE)
  expect_match(code_max, cfg$simd_width_sig, fixed = TRUE)
  if (isTRUE(cfg$expect_no_has_value)) {
    expect_false(grepl("var has_value = False", code_min, fixed = TRUE))
    expect_false(grepl("var has_value = False", code_max, fixed = TRUE))
  }

  built_min <- mojor_build(
    f_which_min,
    x = cfg$x_type,
    name = paste0("t_red_sched_simd_which_min_", cfg$name_suffix),
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_which_max,
    x = cfg$x_type,
    name = paste0("t_red_sched_simd_which_max_", cfg$name_suffix),
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x_host <- cfg$to_host(cfg$x)
  x2_host <- cfg$to_host(cfg$x2)
  x_empty_host <- cfg$to_host(cfg$x_empty)
  expect_identical(built_min$func(cfg$x), which.min(x_host))
  expect_identical(built_min$func(cfg$x2), which.min(x2_host))
  expect_identical(built_min$func(cfg$x_empty), which.min(x_empty_host))
  expect_identical(built_max$func(cfg$x), which.max(x_host))
  expect_identical(built_max$func(cfg$x2), which.max(x2_host))
  expect_identical(built_max$func(cfg$x_empty), which.max(x_empty_host))
}

for (case in list(
  list(
    name = "SIMD reduction mode schedules which.min/which.max and runs",
    x_type = "f64[]",
    name_suffix = "f64",
    simd_width_sig = "comptime simd_width = simd_width_of[DType.float64]()",
    expect_no_has_value = TRUE,
    x = as.double(c(5, 1, 4, 1, 9, 3)),
    x2 = as.double(c(1, 7, 7, 2, 7)),
    x_empty = as.double(numeric(0)),
    to_host = function(v) v,
    needs_float = FALSE
  ),
  list(
    name = "SIMD which.min/which.max reductions run for i32[]",
    x_type = "i32[]",
    name_suffix = "i32",
    simd_width_sig = "comptime simd_width = simd_width_of[DType.int32]()",
    expect_no_has_value = FALSE,
    x = as.integer(c(5L, 1L, 4L, 1L, 9L, 3L)),
    x2 = as.integer(c(1L, 7L, 7L, 2L, 7L)),
    x_empty = integer(0),
    to_host = function(v) v,
    needs_float = FALSE
  ),
  list(
    name = "SIMD which.min/which.max reductions run for f32[]",
    x_type = "f32[]",
    name_suffix = "f32",
    simd_width_sig = "comptime simd_width = simd_width_of[DType.float32]()",
    expect_no_has_value = FALSE,
    x = NULL,
    x2 = NULL,
    x_empty = NULL,
    to_host = function(v) float::dbl(v),
    needs_float = TRUE
  )
)) {
  local({
    cfg <- case
    test_that(cfg$name, {
      skip_if_no_mojo()
      if (isTRUE(cfg$needs_float)) {
        if (!requireNamespace("float", quietly = TRUE)) {
          skip("float package not installed")
        }
        cfg$x <- float::fl(as.double(c(5, 1, 4, 1, 9, 3)))
        cfg$x2 <- float::fl(as.double(c(1, 7, 7, 2, 7)))
        cfg$x_empty <- float::fl(as.double(numeric(0)))
      }
      .mojor_test_run_simd_which_case(cfg)
    })
  })
}

test_that("Tree/SIMD which.min/which.max are tie-stable on long inputs and reject NA/NaN", {  skip_if_no_mojo()

  f_which_min <- function(x) {
    which.min(x)
  }
  f_which_max <- function(x) {
    which.max(x)
  }

  x_tie_min <- as.double(c(
    7, -9, 5, 4, 3, -9, 2, 1, 0, -9, 8, 9, 10, 11, 12, -9, 13, 14, 15,
    16, 17, 18, -9, 19, 20, 21, 22, 23, 24, 25, 26, -9, 27, 28, 29, 30, -9
  ))
  x_tie_max <- as.double(c(
    99, 3, 4, 5, 6, 7, 8, 9, 10, 11, 99, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 99, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99
  ))
  x_na <- as.double(c(5, NA_real_, 4, 5, 3))
  x_nan <- as.double(c(5, NaN, 4, 5, 3))

  for (mode in c("tree", "simd")) {
    built_min <- mojor_build(
      f_which_min,
      x = "f64[]",
      name = paste0("t_red_sched_", mode, "_which_min_tie_nan_f64"),
      reduction = mode,
      cache = FALSE,
      load = TRUE
    )
    built_max <- mojor_build(
      f_which_max,
      x = "f64[]",
      name = paste0("t_red_sched_", mode, "_which_max_tie_nan_f64"),
      reduction = mode,
      cache = FALSE,
      load = TRUE
    )

    expect_identical(as.integer(built_min$func(x_tie_min)), which.min(x_tie_min))
    expect_identical(as.integer(built_min$func(x_tie_min)), 2L)
    expect_identical(as.integer(built_max$func(x_tie_max)), which.max(x_tie_max))
    expect_identical(as.integer(built_max$func(x_tie_max)), 1L)
    expect_error(built_min$func(x_na), "NA/NaN not supported")
    expect_error(built_max$func(x_na), "NA/NaN not supported")
    expect_error(built_min$func(x_nan), "NA/NaN not supported")
    expect_error(built_max$func(x_nan), "NA/NaN not supported")
  }
})

test_that("Tree sum/product reductions run for lgl[] with integer outputs", {  skip_if_no_mojo()

  f_sum <- function(x) {
    sum(x)
  }
  f_prod <- function(x) {
    prod(x)
  }

  built_sum <- mojor_build(
    f_sum,
    x = "lgl[]",
    name = "t_red_sched_tree_sum_lgl",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_prod <- mojor_build(
    f_prod,
    x = "lgl[]",
    name = "t_red_sched_tree_prod_lgl",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )

  x <- c(TRUE, FALSE, TRUE, FALSE)
  x2 <- c(TRUE, TRUE, TRUE, TRUE)
  x_empty <- logical(0)

  expect_identical(built_sum$func(x), as.integer(sum(x)))
  expect_identical(built_sum$func(x2), as.integer(sum(x2)))
  expect_identical(built_sum$func(x_empty), as.integer(sum(x_empty)))
  expect_identical(built_prod$func(x), as.integer(prod(x)))
  expect_identical(built_prod$func(x2), as.integer(prod(x2)))
  expect_identical(built_prod$func(x_empty), as.integer(prod(x_empty)))
})

test_that("Logical scheduled reductions reject NA inputs in forbid mode", {  skip_if_no_mojo()

  f_sum <- function(x) {
    sum(x)
  }
  f_prod <- function(x) {
    prod(x)
  }
  f_which_min <- function(x) {
    which.min(x)
  }
  f_which_max <- function(x) {
    which.max(x)
  }

  built_sum_tree <- mojor_build(
    f_sum,
    x = "lgl[]",
    name = "t_red_sched_tree_sum_lgl_na_guard",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_prod_tree <- mojor_build(
    f_prod,
    x = "lgl[]",
    name = "t_red_sched_tree_prod_lgl_na_guard",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_wmin_tree <- mojor_build(
    f_which_min,
    x = "lgl[]",
    name = "t_red_sched_tree_which_min_lgl_na_guard",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_wmax_tree <- mojor_build(
    f_which_max,
    x = "lgl[]",
    name = "t_red_sched_tree_which_max_lgl_na_guard",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_sum_simd_req <- mojor_build(
    f_sum,
    x = "lgl[]",
    name = "t_red_sched_simdreq_sum_lgl_na_guard",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x_na <- c(TRUE, NA, FALSE)
  expect_error(built_sum_tree$func(x_na), "NA/NaN not supported")
  expect_error(built_prod_tree$func(x_na), "NA/NaN not supported")
  expect_error(built_wmin_tree$func(x_na), "NA/NaN not supported")
  expect_error(built_wmax_tree$func(x_na), "NA/NaN not supported")
  expect_error(built_sum_simd_req$func(x_na), "NA/NaN not supported")
})

test_that("Logical mean/var/sd reductions reject NA inputs in forbid mode", {  skip_if_no_mojo()

  f_mean <- function(x) {
    mean(x)
  }
  f_var <- function(x) {
    var(x)
  }
  f_sd <- function(x) {
    sd(x)
  }

  built_mean <- mojor_build(
    f_mean,
    x = "lgl[]",
    name = "t_red_mean_lgl_na_guard",
    cache = FALSE,
    load = TRUE
  )
  built_var <- mojor_build(
    f_var,
    x = "lgl[]",
    name = "t_red_var_lgl_na_guard",
    cache = FALSE,
    load = TRUE
  )
  built_sd <- mojor_build(
    f_sd,
    x = "lgl[]",
    name = "t_red_sd_lgl_na_guard",
    cache = FALSE,
    load = TRUE
  )

  x_na <- c(TRUE, NA, FALSE, TRUE)
  expect_error(built_mean$func(x_na), "NA/NaN not supported")
  expect_error(built_var$func(x_na), "NA/NaN not supported")
  expect_error(built_sd$func(x_na), "NA/NaN not supported")
})

test_that("Tree which.min/which.max reductions run for lgl[] (non-NA + empty)", {  skip_if_no_mojo()

  f_which_min <- function(x) {
    which.min(x)
  }
  f_which_max <- function(x) {
    which.max(x)
  }

  trans_min <- mojor_transpile(f_which_min, x = "lgl[]", reduction = "tree")
  trans_max <- mojor_transpile(f_which_max, x = "lgl[]", reduction = "tree")
  code_min <- paste(trans_min$mojo, collapse = "\n")
  code_max <- paste(trans_max$mojo, collapse = "\n")
  expect_match(code_min, "# Tree arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max, "# Tree arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min, "Int32(x[__mojor_left_i])", fixed = TRUE)
  expect_match(code_max, "Int32(x[__mojor_left_i])", fixed = TRUE)

  built_min <- mojor_build(
    f_which_min,
    x = "lgl[]",
    name = "t_red_sched_tree_which_min_lgl",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_which_max,
    x = "lgl[]",
    name = "t_red_sched_tree_which_max_lgl",
    reduction = "tree",
    cache = FALSE,
    load = TRUE
  )

  x <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  x2 <- c(FALSE, TRUE, FALSE, TRUE, TRUE)
  x_empty <- logical(0)

  expect_identical(built_min$func(x), which.min(x))
  expect_identical(built_min$func(x2), which.min(x2))
  expect_identical(built_min$func(x_empty), which.min(x_empty))
  expect_identical(built_max$func(x), which.max(x))
  expect_identical(built_max$func(x2), which.max(x2))
  expect_identical(built_max$func(x_empty), which.max(x_empty))
})

test_that("nested non-scalar sum reductions run with tree/simd scheduling (f64)", {  skip_if_no_mojo()

  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 0
      for (i in seq_along(x)) acc <- acc + x[i]
      out[j] <- acc
    }
    out
  }

  x <- as.double(c(3, 1, 4, 1, 5, 9))
  g <- 4L
  expected <- rep(sum(x), g)

  for (mode in c("tree", "simd")) {
    trans <- mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = mode,
      na_mode = "unsafe",
      bounds_check = FALSE
    )
    code <- paste(trans$mojo, collapse = "\n")
    marker <- if (mode == "simd") "# SIMD reduction for sum(x)" else "# Tree reduction for sum(x)"
    expect_match(code, marker, fixed = TRUE)
    expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code))

    built <- mojor_build(
      f,
      x = "f64[]",
      g = "i32",
      name = paste0("t_nested_red_sched_", mode, "_sum_f64"),
      reduction = mode,
      na_mode = "unsafe",
      bounds_check = FALSE,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(x, g), expected)
  }
})

test_that("nested non-scalar SIMD sum reductions run for i32", {  skip_if_no_mojo()

  f <- function(x, g) {
    out <- integer(g)
    for (j in seq_len(g)) {
      acc <- 0L
      for (i in seq_along(x)) acc <- acc + x[i]
      out[j] <- acc
    }
    out
  }

  trans <- mojor_transpile(
    f,
    x = "i32[]",
    g = "i32",
    reduction = "simd",
    na_mode = "unsafe",
    bounds_check = FALSE
  )
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# SIMD reduction for sum(x)", fixed = TRUE)
  expect_match(code, "simd_width_of[DType.int32]()", fixed = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code))

  x <- as.integer(c(2L, 3L, 5L))
  g <- 5L
  expected <- rep(sum(x), g)

  built <- mojor_build(
    f,
    x = "i32[]",
    g = "i32",
    name = "t_nested_red_sched_simd_sum_i32",
    reduction = "simd",
    na_mode = "unsafe",
    bounds_check = FALSE,
    cache = FALSE,
    load = TRUE
  )
  expect_equal(built$func(x, g), expected)
})

test_that("nested non-scalar product reductions run with tree/simd scheduling (f64)", {  skip_if_no_mojo()

  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 1
      for (i in seq_along(x)) acc <- acc * x[i]
      out[j] <- acc
    }
    out
  }

  x <- as.double(c(2, 3, 5, 7))
  g <- 3L
  expected <- rep(prod(x), g)

  for (mode in c("tree", "simd")) {
    trans <- mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = mode,
      na_mode = "unsafe",
      bounds_check = FALSE
    )
    code <- paste(trans$mojo, collapse = "\n")
    marker <- if (mode == "simd") "# SIMD reduction for product(x)" else "# Tree reduction for product(x)"
    expect_match(code, marker, fixed = TRUE)
    expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code))

    built <- mojor_build(
      f,
      x = "f64[]",
      g = "i32",
      name = paste0("t_nested_red_sched_", mode, "_prod_f64"),
      reduction = mode,
      na_mode = "unsafe",
      bounds_check = FALSE,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(x, g), expected)
  }
})

test_that("SIMD i32 which.min/which.max reductions stay tie-stable on long inputs", {  skip_if_no_mojo()

  f_which_min <- function(x) {
    which.min(x)
  }
  f_which_max <- function(x) {
    which.max(x)
  }

  trans_min <- mojor_transpile(f_which_min, x = "i32[]", reduction = "simd")
  trans_max <- mojor_transpile(f_which_max, x = "i32[]", reduction = "simd")
  code_min <- paste(trans_min$mojo, collapse = "\n")
  code_max <- paste(trans_max$mojo, collapse = "\n")
  expect_match(code_min, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_max, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_min, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_max, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)

  built_min <- mojor_build(
    f_which_min,
    x = "i32[]",
    name = "t_red_sched_simd_which_min_i32_tie_long",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )
  built_max <- mojor_build(
    f_which_max,
    x = "i32[]",
    name = "t_red_sched_simd_which_max_i32_tie_long",
    reduction = "simd",
    cache = FALSE,
    load = TRUE
  )

  x_tie_min <- as.integer(c(
    7L, -9L, 5L, 4L, 3L, -9L, 2L, 1L, 0L, -9L, 8L, 9L, 10L, 11L, 12L, -9L,
    13L, 14L, 15L, 16L, 17L, 18L, -9L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, -9L
  ))
  x_tie_max <- as.integer(c(
    99L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 99L, 12L, 13L, 14L, 15L, 16L,
    17L, 18L, 19L, 20L, 99L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 99L
  ))
  x_empty <- integer(0)

  expect_identical(as.integer(built_min$func(x_tie_min)), which.min(x_tie_min))
  expect_identical(as.integer(built_min$func(x_tie_min)), 2L)
  expect_identical(as.integer(built_max$func(x_tie_max)), which.max(x_tie_max))
  expect_identical(as.integer(built_max$func(x_tie_max)), 1L)
  expect_identical(built_min$func(x_empty), which.min(x_empty))
  expect_identical(built_max$func(x_empty), which.max(x_empty))
})
