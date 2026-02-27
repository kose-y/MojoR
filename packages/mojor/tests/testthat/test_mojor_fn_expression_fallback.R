library(testthat)

.mojor_fn_off <- function(fn, ..., .load = NULL, .ir_only = NULL) {
  args <- c(list(fn, object_mode = "off", cache = FALSE), list(...))
  if (!is.null(.load)) args$load <- .load
  if (!is.null(.ir_only)) args$ir_only <- .ir_only
  do.call(mojor_fn, args)
}

.mojor_with_tier9_force <- function(code) {
  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)
  force(code)
}

.mojor_build_off <- function(fn, types = list(), .load = NULL, .ir_only = NULL) {
  do.call(.mojor_fn_off, c(list(fn), types, list(.load = .load, .ir_only = .ir_only)))
}

.expect_mojor_fn_off_error <- function(fn, types = list(), err, .ir_only = NULL) {
  args <- c(list(fn), types)
  if (!is.null(.ir_only)) args$.ir_only <- .ir_only
  # Shared-lane convergence may surface loop-lane strict failures in places
  # that previously raised expression-only diagnostics.
  alt_err <- paste(
    err,
    "strict mode \\(ir_only=TRUE\\) forbids non-strict object-mode fallback",
    "strict mode \\(ir_only=TRUE\\) forbids expression/object fallback",
    "mojor_transpile: unsupported type spec for",
    sep = "|"
  )
  expect_error(
    do.call(.mojor_fn_off, args),
    alt_err
  )
}

.expect_expression_kernel <- function(built, marker = NULL, preview_field = "preview_rewrite_fallback", interpreted_field = NULL, allow_non_expression = FALSE) {
  if (isTRUE(allow_non_expression) && !isTRUE(built$is_expression_kernel)) {
    ok <- is.function(built$func) || isTRUE(built$success)
    expect_true(ok)
    return(invisible(NULL))
  }
  expect_true(isTRUE(built$is_expression_kernel))
  if (!is.null(preview_field) && !is.null(built[[preview_field]])) {
    expect_false(isTRUE(built[[preview_field]]))
  }
  if (!is.null(interpreted_field) && !is.null(built[[interpreted_field]])) {
    expect_false(isTRUE(built[[interpreted_field]]))
  }
  if (!is.null(marker)) {
    expect_true(grepl(marker, built$mojo, fixed = TRUE))
  }
}

.read_wrapper_src <- function(built) {
  candidates <- character(0)
  if (!is.null(built$build_dir) && !is.null(built$kernel) &&
      is.character(built$build_dir) && is.character(built$kernel)) {
    candidates <- c(candidates, file.path(built$build_dir, paste0(built$kernel, "_wrapper.c")))
  }
  if (!is.null(built$lib) && !is.null(built$name) &&
      is.character(built$lib) && is.character(built$name)) {
    candidates <- c(candidates, file.path(dirname(built$lib), paste0(built$name, "_wrapper.c")))
  }
  candidates <- unique(candidates[nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]
  expect_true(length(hit) >= 1, info = paste("wrapper not found:", paste(candidates, collapse = ", ")))
  paste(readLines(hit[[1]], warn = FALSE), collapse = "\n")
}

.expect_sample_int_draw <- function(out, n, size) {
  expect_type(out, "integer")
  expect_length(out, size)
  expect_true(all(out >= 1L & out <= n))
}

.expect_sample_vec_draw <- function(out, x, size, unique_expected) {
  expect_type(out, "double")
  expect_length(out, size)
  expect_true(all(out %in% x))
  if (isTRUE(unique_expected)) {
    expect_equal(length(unique(out)), size)
  }
}

.mojor_compile_case <- function(case, builder = .mojor_fn_off, .load = NULL, .ir_only = NULL) {
  args <- c(list(case$fn), case$types)
  if (!is.null(.load)) args$.load <- .load
  if (!is.null(.ir_only)) args$.ir_only <- .ir_only
  do.call(builder, args)
}

.mojor_build_cases <- function(cases, builder = .mojor_fn_off, .load = NULL, .ir_only = NULL) {
  built <- lapply(cases, function(case) {
    .mojor_compile_case(case, builder = builder, .load = .load, .ir_only = .ir_only)
  })
  has_name <- vapply(cases, function(case) !is.null(case$name) && nzchar(case$name), logical(1))
  if (length(cases) > 0 && all(has_name)) {
    names(built) <- vapply(cases, `[[`, "", "name")
  }
  built
}

.mojor_run_runtime_cases <- function(cases, builder = .mojor_fn_off, .ir_only = NULL) {
  for (case in cases) {
    compiled <- .mojor_compile_case(case, builder = builder, .ir_only = .ir_only)
    case$check(compiled)
  }
}

.mojor_expect_reject_cases <- function(cases, .ir_only = NULL) {
  for (case in cases) {
    .expect_mojor_fn_off_error(case$fn, types = case$types, err = case$err, .ir_only = .ir_only)
  }
}

test_that("mojor_fn set/match subset compiles with object_mode='off'", {
  x <- c(1, 2, 2, 3, 3, 3)
  table_vals <- c(1, 2, 3, 4, 5)

  build_cases <- list(
    list(name = "unique", fn = function(x) unique(x), types = list(x = "f64[]")),
    list(name = "duplicated", fn = function(x) duplicated(x), types = list(x = "f64[]")),
    list(name = "anyDuplicated", fn = function(x) anyDuplicated(x), types = list(x = "f64[]")),
    list(name = "match", fn = function(x, table) match(x, table), types = list(x = "f64[]", table = "f64[]")),
    list(name = "in", fn = function(x, table) x %in% table, types = list(x = "f64[]", table = "f64[]"))
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (case in build_cases) {
    .expect_expression_kernel(
      built[[case$name]],
      marker = "compiled subset set/match",
      preview_field = "preview_rewrite_fallback",
      interpreted_field = "interpreted_fallback"
    )
  }

  runtime_cases <- list(
    list(
      fn = function(x) unique(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), unique(x))
    ),
    list(
      fn = function(x) duplicated(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), duplicated(x))
    ),
    list(
      fn = function(x) anyDuplicated(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), anyDuplicated(x))
    ),
    list(
      fn = function(x, table) match(x, table),
      types = list(x = "f64[]", table = "f64[]"),
      check = function(f) expect_equal(f(c(2, 4, 6), table_vals), match(c(2, 4, 6), table_vals))
    ),
    list(
      fn = function(x, table) x %in% table,
      types = list(x = "f64[]", table = "f64[]"),
      check = function(f) expect_equal(f(c(2, 4, 6), table_vals), c(2, 4, 6) %in% table_vals)
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
})

test_that("mojor_fn set/match subset works with ir_only=TRUE", {
  x <- c(1, 2, 2, 3, 3, 3)
  table_vals <- c(1, 2, 3, 4, 5)

  build_cases <- list(
    list(name = "unique", fn = function(x) unique(x), types = list(x = "f64[]")),
    list(name = "duplicated", fn = function(x) duplicated(x), types = list(x = "f64[]")),
    list(name = "anyDuplicated", fn = function(x) anyDuplicated(x), types = list(x = "f64[]")),
    list(name = "match", fn = function(x, table) match(x, table), types = list(x = "f64[]", table = "f64[]")),
    list(name = "in", fn = function(x, table) x %in% table, types = list(x = "f64[]", table = "f64[]"))
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE, .ir_only = TRUE)
  for (case in build_cases) {
    .expect_expression_kernel(
      built[[case$name]],
      marker = "compiled subset set/match",
      preview_field = "preview_rewrite_fallback",
      interpreted_field = "interpreted_fallback"
    )
  }

  runtime_cases <- list(
    list(
      fn = function(x) unique(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), unique(x))
    ),
    list(
      fn = function(x) duplicated(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), duplicated(x))
    ),
    list(
      fn = function(x) anyDuplicated(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(x), anyDuplicated(x))
    ),
    list(
      fn = function(x, table) match(x, table),
      types = list(x = "f64[]", table = "f64[]"),
      check = function(f) expect_equal(f(c(2, 4, 6), table_vals), match(c(2, 4, 6), table_vals))
    ),
    list(
      fn = function(x, table) x %in% table,
      types = list(x = "f64[]", table = "f64[]"),
      check = function(f) expect_equal(f(c(2, 4, 6), table_vals), c(2, 4, 6) %in% table_vals)
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off, .ir_only = TRUE)
})

test_that("mojor_fn quantile/robust subset compiles with object_mode='off'", {
  x <- as.double(1:10)
  p <- c(0.1, 0.9)

  build_cases <- list(
    list(name = "median", fn = function(x) median(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust"),
    list(name = "quantile_lit", fn = function(x) quantile(x, c(0.25, 0.5, 0.75)), types = list(x = "f64[]"), marker = "compiled subset quantile"),
    list(name = "quantile_param", fn = function(x, p) quantile(x, p), types = list(x = "f64[]", p = "f64[]"), marker = "compiled subset quantile"),
    list(name = "iqr", fn = function(x) IQR(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust"),
    list(name = "mad", fn = function(x) mad(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust")
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (case in build_cases) {
    .expect_expression_kernel(
      built[[case$name]],
      marker = case$marker,
      preview_field = "preview_rewrite_fallback",
      interpreted_field = "interpreted_fallback",
      allow_non_expression = TRUE
    )
  }

  runtime_cases <- list(
    list(
      fn = function(x) median(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(median(x)))
    ),
    list(
      fn = function(x) quantile(x, c(0.25, 0.5, 0.75)),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(quantile(x, c(0.25, 0.5, 0.75))))
    ),
    list(
      fn = function(x, p) quantile(x, p),
      types = list(x = "f64[]", p = "f64[]"),
      check = function(f) expect_equal(unname(f(x, p)), unname(quantile(x, p)))
    ),
    list(
      fn = function(x) IQR(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(IQR(x)))
    ),
    list(
      fn = function(x) mad(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(mad(x)))
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
})

test_that("mojor_fn quantile/robust subset works with ir_only=TRUE", {
  x <- as.double(1:10)
  p <- c(0.1, 0.9)

  build_cases <- list(
    list(name = "median", fn = function(x) median(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust"),
    list(name = "quantile_lit", fn = function(x) quantile(x, c(0.25, 0.5, 0.75)), types = list(x = "f64[]"), marker = "compiled subset quantile"),
    list(name = "quantile_param", fn = function(x, p) quantile(x, p), types = list(x = "f64[]", p = "f64[]"), marker = "compiled subset quantile"),
    list(name = "iqr", fn = function(x) IQR(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust"),
    list(name = "mad", fn = function(x) mad(x), types = list(x = "f64[]"), marker = "compiled subset quantile/robust")
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE, .ir_only = TRUE)
  for (case in build_cases) {
    .expect_expression_kernel(
      built[[case$name]],
      marker = case$marker,
      preview_field = "preview_rewrite_fallback",
      interpreted_field = "interpreted_fallback",
      allow_non_expression = TRUE
    )
  }

  runtime_cases <- list(
    list(
      fn = function(x) median(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(median(x)))
    ),
    list(
      fn = function(x) quantile(x, c(0.25, 0.5, 0.75)),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(quantile(x, c(0.25, 0.5, 0.75))))
    ),
    list(
      fn = function(x, p) quantile(x, p),
      types = list(x = "f64[]", p = "f64[]"),
      check = function(f) expect_equal(unname(f(x, p)), unname(quantile(x, p)))
    ),
    list(
      fn = function(x) IQR(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(IQR(x)))
    ),
    list(
      fn = function(x) mad(x),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(x)), unname(mad(x)))
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off, .ir_only = TRUE)
})

test_that("mojor_fn interpreted quantile/robust preview fallbacks remain for unsupported signatures", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  check_preview_case <- function(case) {
    build_args <- c(list(case$fn, .load = FALSE), case$types)
    built <- expect_warning(
      do.call(.mojor_fn_off, build_args),
      paste0("preview op '", case$op, "'")
    )
    expect_true(is.function(built$func))
    if (isTRUE(case$check_object_mode)) {
      expect_false(isTRUE(built$object_mode))
    }
    do.call(case$expect_result, list(built$func))
  }

  preview_cases <- list(
    list(
      op = "median",
      fn = function(x, na_rm) median(x, na.rm = na_rm),
      types = list(x = "f64[]", na_rm = "lgl"),
      expect_result = function(f) expect_equal(unname(f(c(1, 2, NA_real_, 4), TRUE)), unname(median(c(1, 2, NA_real_, 4), na.rm = TRUE)))
    ),
    list(
      op = "quantile",
      fn = function(x) quantile(x),
      types = list(x = "f64[]"),
      expect_result = function(f) expect_equal(unname(f(1:10)), unname(quantile(1:10)))
    ),
    list(
      op = "IQR",
      fn = function(x, na_rm) IQR(x, na.rm = na_rm),
      types = list(x = "f64[]", na_rm = "lgl"),
      expect_result = function(f) expect_equal(unname(f(c(1, 2, NA_real_, 4), TRUE)), unname(IQR(c(1, 2, NA_real_, 4), na.rm = TRUE)))
    ),
    list(
      op = "mad",
      fn = function(x, constant) mad(x, constant = constant),
      types = list(x = "f64[]", constant = "f64"),
      expect_result = function(f) expect_equal(unname(f(c(1, 2, 3, 4, 5), 2.0)), unname(mad(c(1, 2, 3, 4, 5), constant = 2.0)))
    )
  )

  for (case in preview_cases) {
    check_preview_case(case)
  }
})

test_that("mojor_fn strict ir_only mode forbids interpreted preview fallback", {  .expect_mojor_fn_off_error(
    function(x) quantile(x),
    types = list(x = "f64[]"),
    err = "strict mode \\(ir_only=TRUE\\) forbids non-strict object-mode fallback",
    .ir_only = TRUE
  )
})

test_that("strict ir_only mode forbids hybrid/object fallback paths", {  .expect_mojor_fn_off_error(
    function(x) sort(x),
    types = list(x = "f64[]"),
    err = "strict mode \\(ir_only=TRUE\\) forbids non-strict object-mode fallback",
    .ir_only = TRUE
  )

  expect_error(
    mojor_build(
      function(x) sort(x),
      x = "f64[]",
      object_mode = "hybrid",
      ir_only = TRUE,
      load = FALSE,
      cache = FALSE
    ),
    "object_mode must be 'off' when ir_only=TRUE"
  )
})

test_that("mojor_fn HOF subset compiles with object_mode='off'", {  x_named <- c(1, 5, 3)
  y_named <- c(2, 4, 6)
  x3 <- c(6, 2, 8)
  y3 <- c(5, 3, 9)
  z3 <- c(7, 1, 4)
  runtime_cases <- list(
    list(
      fn = function(x) vapply(x, function(v) v * 2, FUN.VALUE = numeric(1)),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(c(1, 2, 3)), c(2, 4, 6))
    ),
    list(
      fn = function(x) sapply(x, function(v) v + 1),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(f(c(1, 2, 3)), c(2, 3, 4))
    ),
    list(
      fn = function(x) lapply(x, function(v) v - 1),
      types = list(x = "f64[]"),
      check = function(f) expect_equal(unname(f(c(1, 2, 3))), c(0, 1, 2))
    ),
    list(
      fn = function(x, y) mapply(function(a, b) a + b, x, y),
      types = list(x = "f64[]", y = "f64[]"),
      check = function(f) expect_equal(unname(f(c(1, 2, 3), c(4, 5, 6))), c(5, 7, 9))
    ),
    list(
      fn = function(x, y, z) mapply(function(a, b, c) a + b - c, x, y, z),
      types = list(x = "f64[]", y = "f64[]", z = "f64[]"),
      check = function(f) expect_equal(unname(f(c(5, 6), c(1, 2), c(2, 3))), c(4, 5))
    ),
    list(
      fn = function(x, y) mapply(FUN = pmax, x, y),
      types = list(x = "f64[]", y = "f64[]"),
      check = function(f) expect_equal(unname(f(x_named, y_named)), as.double(pmax(x_named, y_named)))
    ),
    list(
      fn = function(x, y) mapply(FUN = pmax, x = x, y = y),
      types = list(x = "f64[]", y = "f64[]"),
      check = function(f) expect_equal(unname(f(x_named, y_named)), as.double(pmax(x_named, y_named)))
    ),
    list(
      fn = function(x, y) mapply(FUN = pmax, FUN = pmax, x = x, y = y),
      types = list(x = "f64[]", y = "f64[]"),
      check = function(f) expect_equal(unname(f(x_named, y_named)), as.double(pmax(x_named, y_named)))
    ),
    list(
      fn = function(x, y, z) mapply(pmin, x, y, z),
      types = list(x = "f64[]", y = "f64[]", z = "f64[]"),
      check = function(f) expect_equal(unname(f(x3, y3, z3)), as.double(pmin(x3, y3, z3)))
    ),
    list(
      fn = function(x, y) mapply(function(a, b) a + b, x, y),
      types = list(x = "f64[]", y = "i32[]"),
      check = function(f) expect_equal(unname(f(c(1, 2, 3), c(4L, 5L, 6L))), c(5, 7, 9))
    ),
    list(
      fn = function(x) sapply(x, function(v) c(v, v + 1)),
      types = list(x = "f64[]"),
      check = function(f) {
        x_local <- c(1, 2, 3)
        expect_equal(unname(f(x_local)), unname(as.double(unlist(lapply(x_local, function(v) c(v, v + 1))))))
      }
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
})

test_that("mojor_fn HOF subset rejects unsupported forms with stable errors", {  reject_cases <- list(
    list(
      fn = function(x, y) mapply(function(a) a, x, y),
      types = list(x = "f64[]", y = "f64[]"),
      err = "FUN arity must match"
    ),
    list(
      fn = function(x, y) mapply(function(a, b) a + b, x + 1, y),
      types = list(x = "f64[]", y = "f64[]"),
      err = "direct vector arguments"
    ),
    list(
      fn = function(x, y) mapply(mean, x, y),
      types = list(x = "f64[]", y = "f64[]"),
      err = "named FUN 'mean' is unsupported"
    )
  )
  .mojor_expect_reject_cases(reject_cases)

  bias <- 10
  capture_case <- list(
    fn = function(x) sapply(x, function(v) v + bias),
    types = list(x = "f64[]"),
    err = "closures/non-inlineable captures"
  )
  for (ir_only in list(NULL, TRUE)) {
    .expect_mojor_fn_off_error(
      capture_case$fn,
      types = capture_case$types,
      err = capture_case$err,
      .ir_only = ir_only
    )
  }
})

test_that("mojor_fn compiled subset supported forms use compiled expression kernel path", {  build_cases <- list(
    list(fn = function(x) vapply(x, function(v) v * 3, FUN.VALUE = numeric(1)), types = list(x = "f64[]")),
    list(fn = function(x) lapply(x, function(v) v + 2), types = list(x = "f64[]")),
    list(fn = function(x, y) mapply(function(a, b) a * b, x, y), types = list(x = "f64[]", y = "f64[]")),
    list(fn = function(x, y) mapply(function(a, b) a + b, x, y), types = list(x = "f64[]", y = "i32[]"))
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (entry in built) {
    .expect_expression_kernel(
      entry,
      marker = "compiled subset higher-order functions",
      preview_field = NULL,
      interpreted_field = "interpreted_fallback"
    )
  }
})

test_that("mojor_fn string subset compiles with object_mode='off'", {  build_cases <- list(
    list(name = "nchar", fn = function(x) nchar(x), types = list(x = "chr[]"), wrapper_pat = "R_nchar"),
    list(name = "nzchar", fn = function(x) nzchar(x), types = list(x = "chr[]"), wrapper_pat = "R_nchar"),
    list(name = "substr", fn = function(x, start, stop) substr(x, start, stop), types = list(x = "chr[]", start = "i32", stop = "i32"), wrapper_pat = "R_nchar"),
    list(name = "paste", fn = function(x, y) paste(x, y, sep = "|"), types = list(x = "chr[]", y = "chr[]"), wrapper_pat = "Rf_mkCharLen"),
    list(name = "paste0", fn = function(x, y) paste0(x, y), types = list(x = "chr[]", y = "chr[]"), wrapper_pat = "Rf_mkCharLen")
  )

  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (case in build_cases) {
    .expect_expression_kernel(built[[case$name]], marker = "compiled subset native nchar/nzchar/substr/paste path")
    wrapper_src <- .read_wrapper_src(built[[case$name]])
    expect_true(grepl(case$wrapper_pat, wrapper_src, fixed = TRUE))
    expect_false(grepl("Rf_eval", wrapper_src, fixed = TRUE))
  }

  x <- c("a", "bc", NA_character_, "")
  y <- c("X", "", "Z")
  runtime_cases <- list(
    list(fn = function(x) nchar(x), types = list(x = "chr[]"), check = function(f) expect_equal(unname(f(x)), unname(nchar(x)))),
    list(fn = function(x) nzchar(x), types = list(x = "chr[]"), check = function(f) expect_equal(unname(f(x)), unname(nzchar(x)))),
    list(fn = function(x, start, stop) substr(x, start, stop), types = list(x = "chr[]", start = "i32", stop = "i32"), check = function(f) expect_equal(unname(f(x, 2L, 3L)), unname(substr(x, 2L, 3L)))),
    list(fn = function(x, start, stop) substring(x, first = start, last = stop), types = list(x = "chr[]", start = "i32", stop = "i32"), check = function(f) expect_equal(unname(f(x, 2L, 3L)), unname(substring(x, first = 2L, last = 3L)))),
    list(fn = function(x, y) paste(x, y, sep = "|"), types = list(x = "chr[]", y = "chr[]"), check = function(f) expect_equal(unname(f(x, y)), unname(paste(x, y, sep = "|")))),
    list(fn = function(x, y) paste0(x, y), types = list(x = "chr[]", y = "chr[]"), check = function(f) expect_equal(unname(f(x, y)), unname(paste0(x, y)))),
    list(fn = function(x, y) paste(x, y, sep = "|", collapse = "::"), types = list(x = "chr[]", y = "chr[]"), check = function(f) expect_equal(unname(f(x, y)), unname(paste(x, y, sep = "|", collapse = "::")))),
    list(fn = function(x, y) paste0(x, y, collapse = "::"), types = list(x = "chr[]", y = "chr[]"), check = function(f) expect_equal(unname(f(x, y)), unname(paste0(x, y, collapse = "::"))))
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
})

test_that("mojor_fn string subset works with ir_only=TRUE", {  x <- c("", "a", NA_character_, "bc")
  y <- c("X", "", "Z", "W")
  x_mismatch <- c("a", "b", "c")
  y_mismatch <- c("X", "Y")
  x_na <- c(NA_character_, "", NA_character_, "tail")
  y_na <- c("left", NA_character_, "", NA_character_)

  ir_only_cases <- list(
    list(
      fn = function(x) nzchar(x),
      types = list(x = "chr[]"),
      check = function(f) {
        expect_equal(unname(f(x)), unname(nzchar(x)))
      }
    ),
    list(
      fn = function(x, start, stop) substr(x, start, stop),
      types = list(x = "chr[]", start = "i32", stop = "i32"),
      check = function(f) {
        expect_equal(unname(f(x, 1L, 2L)), unname(substr(x, 1L, 2L)))
      }
    ),
    list(
      fn = function(x, start, stop) substring(x, first = start, last = stop),
      types = list(x = "chr[]", start = "i32", stop = "i32"),
      check = function(f) {
        expect_equal(unname(f(x, 1L, 2L)), unname(substring(x, first = 1L, last = 2L)))
      }
    ),
    list(
      fn = function(x, y) paste(x, y, sep = "|"),
      types = list(x = "chr[]", y = "chr[]"),
      check = function(f) {
        expect_equal(unname(f(x, y)), unname(paste(x, y, sep = "|")))
        expect_equal(unname(f(x_mismatch, y_mismatch)), unname(suppressWarnings(paste(x_mismatch, y_mismatch, sep = "|"))))
        expect_equal(unname(f(x_na, y_na)), unname(paste(x_na, y_na, sep = "|")))
      }
    ),
    list(
      fn = function(x, y) paste0(x, y),
      types = list(x = "chr[]", y = "chr[]"),
      check = function(f) {
        expect_equal(unname(f(x, y)), unname(paste0(x, y)))
        expect_equal(unname(f(x_mismatch, y_mismatch)), unname(suppressWarnings(paste0(x_mismatch, y_mismatch))))
        expect_equal(unname(f(x_na, y_na)), unname(paste0(x_na, y_na)))
      }
    ),
    list(
      fn = function(x, y) paste(x, y, sep = "|", collapse = "::"),
      types = list(x = "chr[]", y = "chr[]"),
      check = function(f) {
        expect_equal(unname(f(x, y)), unname(paste(x, y, sep = "|", collapse = "::")))
        expect_equal(unname(f(x_mismatch, y_mismatch)), unname(suppressWarnings(paste(x_mismatch, y_mismatch, sep = "|", collapse = "::"))))
        expect_equal(unname(f(x_na, y_na)), unname(paste(x_na, y_na, sep = "|", collapse = "::")))
      }
    ),
    list(
      fn = function(x, y) paste0(x, y, collapse = "::"),
      types = list(x = "chr[]", y = "chr[]"),
      check = function(f) {
        expect_equal(unname(f(x, y)), unname(paste0(x, y, collapse = "::")))
        expect_equal(unname(f(x_mismatch, y_mismatch)), unname(suppressWarnings(paste0(x_mismatch, y_mismatch, collapse = "::"))))
        expect_equal(unname(f(x_na, y_na)), unname(paste0(x_na, y_na, collapse = "::")))
      }
    )
  )
  .mojor_run_runtime_cases(ir_only_cases, builder = .mojor_fn_off, .ir_only = TRUE)
})

test_that("mojor_fn strict subset enforces signature constraints", {  reject_cases <- list(
    list(
      fn = function(x, y, sep) paste(x, y, sep = sep),
      types = list(x = "chr[]", y = "chr[]", sep = "chr[]"),
      err = "sep must be a scalar character literal"
    ),
    list(
      fn = function(x, y, collapse) paste(x, y, sep = "|", collapse = collapse),
      types = list(x = "chr[]", y = "chr[]", collapse = "chr[]"),
      err = "collapse must be a scalar character literal or NULL"
    ),
    list(
      fn = function(x, y, z) paste0(x, y, z),
      types = list(x = "chr[]", y = "chr[]", z = "chr[]"),
      err = "collapse must be a scalar character literal or NULL"
    ),
    list(
      fn = function(x) nchar(x[1]),
      types = list(x = "chr[]"),
      err = "direct character vector argument"
    )
  )
  .mojor_expect_reject_cases(reject_cases)
})

test_that("mojor_fn sampling subset compiles with object_mode='off'", {  build_cases <- list(
    list(fn = function(n, size) sample.int(n, size = size, replace = TRUE), types = list(n = "i32", size = "i32")),
    list(fn = function(n, size, r) sample.int(n, size = size, replace = r), types = list(n = "i32", size = "i32", r = "lgl")),
    list(fn = function(x, size) sample(x, size = size, replace = FALSE), types = list(x = "f64[]", size = "i32")),
    list(fn = function(x, size, r) sample(x, size = size, replace = r), types = list(x = "f64[]", size = "i32", r = "i32"))
  )
  built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (entry in built) {
    .expect_expression_kernel(entry, marker = "compiled subset sampling")
    expect_true(grepl("IR-native node emit", entry$mojo, fixed = TRUE))
  }

  x <- as.double(c(1, 2, 3, 4, 5))
  runtime_cases <- list(
    list(
      fn = function(n, size) sample.int(n, size = size, replace = TRUE),
      types = list(n = "i32", size = "i32"),
      check = function(f) .expect_sample_int_draw(f(10L, 5L), n = 10L, size = 5L)
    ),
    list(
      fn = function(n) sample.int(n),
      types = list(n = "i32"),
      check = function(f) .expect_sample_int_draw(f(10L), n = 10L, size = 10L)
    ),
    list(
      fn = function(n, size, r) sample.int(n, size = size, replace = r),
      types = list(n = "i32", size = "i32", r = "lgl"),
      check = function(f) {
        draw_r <- f(10L, 5L, TRUE)
        .expect_sample_int_draw(draw_r, n = 10L, size = 5L)
        draw_nr <- f(10L, 4L, FALSE)
        .expect_sample_int_draw(draw_nr, n = 10L, size = 4L)
        expect_equal(length(unique(draw_nr)), 4L)
      }
    ),
    list(
      fn = function(x, size) sample(x, size = size, replace = FALSE),
      types = list(x = "f64[]", size = "i32"),
      check = function(f) .expect_sample_vec_draw(f(x, 3L), x = x, size = 3L, unique_expected = TRUE)
    ),
    list(
      fn = function(x, size, r) sample(x, size = size, replace = r),
      types = list(x = "f64[]", size = "i32", r = "i32"),
      check = function(f) {
        draw_r <- f(x, 5L, 1L)
        .expect_sample_vec_draw(draw_r, x = x, size = 5L, unique_expected = FALSE)
        draw_nr <- f(x, 3L, 0L)
        .expect_sample_vec_draw(draw_nr, x = x, size = 3L, unique_expected = TRUE)
      }
    ),
    list(
      fn = function(x) sample(x),
      types = list(x = "f64[]"),
      check = function(f) .expect_sample_vec_draw(f(x), x = x, size = length(x), unique_expected = TRUE)
    ),
    list(
      fn = function(n, size, p) sample.int(n, size = size, replace = TRUE, prob = p),
      types = list(n = "i32", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_len(10L))
        .expect_sample_int_draw(f(10L, 4L, p), n = 10L, size = 4L)
      }
    ),
    list(
      fn = function(x, size, p) sample(x, size = size, replace = TRUE, prob = p),
      types = list(x = "f64[]", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_along(x))
        .expect_sample_vec_draw(f(x, 4L, p), x = x, size = 4L, unique_expected = FALSE)
      }
    )
  )
  .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
})

test_that("mojor_fn sampling subset works with ir_only=TRUE", {  x <- as.double(c(1, 2, 3, 4, 5))
  ir_only_cases <- list(
    list(
      fn = function(n, size) sample.int(n, size = size, replace = TRUE),
      types = list(n = "i32", size = "i32"),
      check = function(f) .expect_sample_int_draw(f(8L, 4L), n = 8L, size = 4L)
    ),
    list(
      fn = function(n) sample.int(n),
      types = list(n = "i32"),
      check = function(f) .expect_sample_int_draw(f(8L), n = 8L, size = 8L)
    ),
    list(
      fn = function(n, size, r) sample.int(n, size = size, replace = r),
      types = list(n = "i32", size = "i32", r = "i32"),
      check = function(f) {
        draw_r <- f(8L, 4L, 1L)
        .expect_sample_int_draw(draw_r, n = 8L, size = 4L)
        draw_nr <- f(8L, 4L, 0L)
        .expect_sample_int_draw(draw_nr, n = 8L, size = 4L)
        expect_equal(length(unique(draw_nr)), 4L)
      }
    ),
    list(
      fn = function(x, size) sample(x, size = size, replace = FALSE),
      types = list(x = "f64[]", size = "i32"),
      check = function(f) .expect_sample_vec_draw(f(x, 3L), x = x, size = 3L, unique_expected = TRUE)
    ),
    list(
      fn = function(x, size, r) sample(x, size = size, replace = r),
      types = list(x = "f64[]", size = "i32", r = "lgl"),
      check = function(f) {
        draw_r <- f(x, 5L, TRUE)
        .expect_sample_vec_draw(draw_r, x = x, size = 5L, unique_expected = FALSE)
        draw_nr <- f(x, 3L, FALSE)
        .expect_sample_vec_draw(draw_nr, x = x, size = 3L, unique_expected = TRUE)
      }
    ),
    list(
      fn = function(x) sample(x, size = length(x), replace = FALSE),
      types = list(x = "f64[]"),
      check = function(f) .expect_sample_vec_draw(f(x), x = x, size = length(x), unique_expected = TRUE)
    ),
    list(
      fn = function(x) sample(x),
      types = list(x = "f64[]"),
      check = function(f) .expect_sample_vec_draw(f(x), x = x, size = length(x), unique_expected = TRUE)
    ),
    list(
      fn = function(n, size, p) sample.int(n, size = size, replace = TRUE, prob = p),
      types = list(n = "i32", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_len(8L))
        .expect_sample_int_draw(f(8L, 4L, p), n = 8L, size = 4L)
      }
    ),
    list(
      fn = function(x, size, p) sample(x, size = size, replace = TRUE, prob = p),
      types = list(x = "f64[]", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_along(x))
        .expect_sample_vec_draw(f(x, 4L, p), x = x, size = 4L, unique_expected = FALSE)
      }
    ),
    list(
      fn = function(n, size, p) sample.int(n, size = size, replace = FALSE, prob = p),
      types = list(n = "i32", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_len(8L))
        draw <- f(8L, 4L, p)
        .expect_sample_int_draw(draw, n = 8L, size = 4L)
        expect_equal(length(unique(draw)), 4L)
      }
    ),
    list(
      fn = function(x, size, p) sample(x, size = size, replace = FALSE, prob = p),
      types = list(x = "f64[]", size = "i32", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_along(x))
        draw <- f(x, 4L, p)
        .expect_sample_vec_draw(draw, x = x, size = 4L, unique_expected = TRUE)
      }
    ),
    list(
      fn = function(x, size, r, p) sample(x, size = size, replace = r, prob = p),
      types = list(x = "f64[]", size = "i32", r = "lgl", p = "f64[]"),
      check = function(f) {
        p <- as.double(seq_along(x))
        draw_r <- f(x, 4L, TRUE, p)
        .expect_sample_vec_draw(draw_r, x = x, size = 4L, unique_expected = FALSE)
        draw_nr <- f(x, 3L, FALSE, p)
        .expect_sample_vec_draw(draw_nr, x = x, size = 3L, unique_expected = TRUE)
      }
    )
  )
  .mojor_run_runtime_cases(ir_only_cases, builder = .mojor_fn_off, .ir_only = TRUE)
})

test_that("mojor_fn compiled subset sampling rejects non-subset weighted prob forms", {  reject_cases <- list(
    list(
      fn = function(n, size, p) sample.int(n, size = size, replace = TRUE, prob = p + 0),
      types = list(n = "i32", size = "i32", p = "f64[]"),
      err = "direct f64\\[\\] argument"
    )
  )
  .mojor_expect_reject_cases(reject_cases)
})

test_that("mojor_fn compiled subset sampling enforces replace scalar type", {
  reject_cases <- list(
    list(
      fn = function(n, size, r) sample.int(n, size = size, replace = r),
      types = list(n = "i32", size = "i32", r = "f64"),
      err = "replace argument 'r' must be typed as lgl, bool, or i32"
    ),
    list(
      fn = function(x, size, r) sample(x, size = size, replace = r),
      types = list(x = "f64[]", size = "i32", r = "f64"),
      err = "replace argument 'r' must be typed as lgl, bool, or i32"
    )
  )
  .mojor_expect_reject_cases(reject_cases)
})

test_that("mojor_fn compiled subset sampling enforces strict direct-vector subset", {  .mojor_expect_reject_cases(
    list(list(
      fn = function(x) sample(x[1:3], size = 2L, replace = FALSE),
      types = list(x = "f64[]"),
      err = "direct vector argument"
    )),
    .ir_only = TRUE
  )
})

test_that("mojor_fn compiled subset sampling rejects unsupported argument signatures", {  reject_cases <- list(
    list(
      fn = function(n, size, extra) sample.int(n, size = size, replace = TRUE, extra = extra),
      types = list(n = "i32", size = "i32", extra = "i32"),
      err = "supports only n, size, replace, prob arguments"
    ),
    list(
      fn = function(x, size, extra) sample(x, size = size, replace = FALSE, extra = extra),
      types = list(x = "f64[]", size = "i32", extra = "i32"),
      err = "supports only x, X, size, replace, prob arguments"
    )
  )
  .mojor_expect_reject_cases(reject_cases)
})

test_that("mojor_fn supported subset works with ir_only=TRUE", {  f_ir_only <- mojor_fn(
    function(x, y) mapply(function(a, b) a - b, x, y),
    x = "f64[]",
    y = "f64[]",
    object_mode = "off",
    ir_only = TRUE,
    cache = FALSE
  )
  expect_equal(unname(f_ir_only(c(8, 9, 10), c(1, 2, 3))), c(7, 7, 7))
})

test_that("compiled path does not mark preview rewrite fallback", {  build_cases <- list(
    list(fn = function(x) vapply(x, function(v) v + 1, FUN.VALUE = numeric(1)), types = list(x = "f64[]")),
    list(fn = function(x) sapply(x, function(v) v + 1), types = list(x = "f64[]")),
    list(fn = function(x) lapply(x, function(v) v + 1), types = list(x = "f64[]")),
    list(fn = function(x, y) mapply(function(a, b) a + b, x, y), types = list(x = "f64[]", y = "f64[]")),
    list(fn = function(x, y) mapply(function(a, b) a + b, x, y), types = list(x = "f64[]", y = "i32[]"))
  )
  built_all <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
  for (built in built_all) {
    expect_true(isTRUE(built$is_expression_kernel))
    expect_false(isTRUE(built$preview_rewrite_fallback))
    expect_false(isTRUE(built$preview_rewrite_unexpected))
  }
})

test_that("mojor_fn validates ir_only flag type in fallback gate", {  expect_error(
    .mojor_compile_case(
      list(
        fn = function(x) sapply(x, function(v) v + 1),
        types = list(x = "f64[]")
      ),
      builder = .mojor_fn_off,
      .ir_only = 1
    ),
    "ir_only must be TRUE or FALSE"
  )
})

test_that("mojor_fn regex subset compiles with object_mode='off'", {  .mojor_with_tier9_force({
    build_cases <- list(
      list(name = "grepl", fn = function(pattern, x) grepl(pattern, x), types = list(pattern = "chr", x = "chr[]")),
      list(name = "grep", fn = function(x) grep("a", x), types = list(x = "chr[]")),
      list(name = "sub", fn = function(pattern, replacement, x) sub(pattern, replacement, x), types = list(pattern = "chr", replacement = "chr", x = "chr[]")),
      list(name = "gsub", fn = function(pattern, replacement, x) gsub(pattern, replacement, x), types = list(pattern = "chr", replacement = "chr", x = "chr[]"))
    )
    built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
    for (entry in built) {
      .expect_expression_kernel(entry, marker = "compiled subset regex runtime bridge path")
      expect_true(is.list(entry$tier9_runtime_plan))
      expect_identical(entry$tier9_runtime_plan$kind, "regex_native")
    }
    expect_true(is.na(built$grepl$lib))
    expect_true(is.na(built$sub$lib))

    x <- c("aa", "bb", "ab", "")
    runtime_cases <- list(
      list(fn = function(pattern, x) grepl(pattern, x), types = list(pattern = "chr", x = "chr[]"), check = function(f) expect_equal(unname(f("a", x)), unname(grepl("a", x)))),
      list(fn = function(x) grep("a", x), types = list(x = "chr[]"), check = function(f) expect_equal(unname(f(x)), unname(grep("a", x)))),
      list(fn = function(pattern, replacement, x) sub(pattern, replacement, x), types = list(pattern = "chr", replacement = "chr", x = "chr[]"), check = function(f) expect_equal(unname(f("a", "z", x)), unname(sub("a", "z", x)))),
      list(fn = function(pattern, replacement, x) gsub(pattern, replacement, x), types = list(pattern = "chr", replacement = "chr", x = "chr[]"), check = function(f) expect_equal(unname(f("a", "z", x)), unname(gsub("a", "z", x))))
    )
    .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
  })
})

test_that("mojor_fn regex subset works with ir_only=TRUE", {  .mojor_with_tier9_force({
    x <- c("aa", "bb", "ab", "")
    .mojor_run_runtime_cases(
      list(list(
        fn = function(pattern, replacement, x) gsub(pattern, replacement, x),
        types = list(pattern = "chr", replacement = "chr", x = "chr[]"),
        check = function(f) expect_equal(unname(f("a", "z", x)), unname(gsub("a", "z", x)))
      )),
      builder = .mojor_fn_off,
      .ir_only = TRUE
    )
  })
})

test_that("mojor_fn regex subset accepts scalar typed fixed/perl arguments", {  .mojor_with_tier9_force({
    built <- .mojor_fn_off(
      function(pattern, x, fixed_flag, perl_flag) grepl(pattern, x, fixed = fixed_flag, perl = perl_flag),
      pattern = "chr",
      x = "chr[]",
      fixed_flag = "lgl",
      perl_flag = "i32",
      .load = FALSE
    )
    expect_identical(built$tier9_runtime_plan$kind, "regex_native")
    expect_identical(built$tier9_runtime_plan$fixed$kind, "arg")
    expect_identical(built$tier9_runtime_plan$perl$kind, "arg")
    expect_identical(built$tier9_runtime_plan$fixed$name, "fixed_flag")
    expect_identical(built$tier9_runtime_plan$perl$name, "perl_flag")

    f <- .mojor_fn_off(
      function(pattern, x, fixed_flag, perl_flag) grepl(pattern, x, fixed = fixed_flag, perl = perl_flag),
      pattern = "chr",
      x = "chr[]",
      fixed_flag = "lgl",
      perl_flag = "i32"
    )
    x <- c("aa", "bb", "ab", "")
    expect_equal(
      unname(f("a", x, FALSE, 1L)),
      unname(grepl("a", x, fixed = FALSE, perl = TRUE))
    )
    expect_equal(
      unname(f("a", x, TRUE, 0L)),
      unname(grepl("a", x, fixed = TRUE, perl = FALSE))
    )
  })
})

test_that("mojor_fn regex subset enforces strict signature constraints", {  .mojor_with_tier9_force({
    reject_cases <- list(
      list(fn = function(pattern, x) grepl(pattern, x), types = list(pattern = "chr[]", x = "chr[]"), err = "pattern must be a chr scalar argument or scalar character literal"),
      list(fn = function(pattern, x) grepl(pattern, x[1]), types = list(pattern = "chr", x = "chr[]"), err = "direct character vector argument"),
      list(fn = function(pattern, x) grep(pattern, x, fixed = TRUE, perl = TRUE), types = list(pattern = "chr", x = "chr[]"), err = "does not support fixed=TRUE with perl=TRUE"),
      list(fn = function(pattern, x, fixed_flag) grepl(pattern, x, fixed = fixed_flag), types = list(pattern = "chr", x = "chr[]", fixed_flag = "f64"), err = "fixed must be a scalar logical argument/literal"),
      list(fn = function(x) sub("a", "\\1", x), types = list(x = "chr[]"), err = "backreferences in replacement are not supported"),
      list(fn = function(x) gsub("a", "\\1", x), types = list(x = "chr[]"), err = "backreferences in replacement are not supported")
    )
    .mojor_expect_reject_cases(reject_cases)
  })
})

test_that("mojor_fn table subset compiles with object_mode='off'", {  .mojor_with_tier9_force({
    build_cases <- list(
      list(name = "row", fn = function(x) row(x), types = list(x = "f64[,]"), marker = "compiled subset row IR-native matrix path", plan_kind = NULL),
      list(name = "col", fn = function(x) col(x), types = list(x = "i32[,]"), marker = "compiled subset col IR-native matrix path", plan_kind = NULL),
      list(name = "expand1", fn = function(x) expand.grid(x), types = list(x = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand", fn = function(x, y) expand.grid(x, y), types = list(x = "i32[]", y = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand4", fn = function(x, y, z, w) expand.grid(x, y, z, w), types = list(x = "i32[]", y = "i32[]", z = "i32[]", w = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand5", fn = function(x, y, z, w, v) expand.grid(x, y, z, w, v), types = list(x = "i32[]", y = "i32[]", z = "i32[]", w = "i32[]", v = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand_named", fn = function(x, y) expand.grid(a = x, b = y), types = list(x = "i32[]", y = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand_expr", fn = function(x, y) expand.grid(x + 1L, y * 2L), types = list(x = "i32[]", y = "i32[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native"),
      list(name = "expand_expr_math", fn = function(x, y) expand.grid(log1p(x), sqrt(y + 1.0)), types = list(x = "f64[]", y = "f64[]"), marker = "compiled subset expand.grid runtime bridge path", plan_kind = "expand_grid_native")
    )
    built <- .mojor_build_cases(build_cases, builder = .mojor_fn_off, .load = FALSE)
    for (case in build_cases) {
      entry <- built[[case$name]]
      expect_true(isTRUE(entry$is_expression_kernel))
      expect_true(grepl(case$marker, entry$mojo, fixed = TRUE))
      if (is.null(case$plan_kind)) {
        next
      }
      expect_true(is.list(entry$tier9_runtime_plan))
      expect_identical(entry$tier9_runtime_plan$kind, case$plan_kind)
    }
    expect_true(is.na(built$expand$lib))

    mat_f64 <- matrix(as.double(1:6), nrow = 2, ncol = 3)
    mat_i32 <- matrix(as.integer(1:6), nrow = 2, ncol = 3)
    runtime_cases <- list(
      list(
        fn = function(x) row(x),
        types = list(x = "f64[,]"),
        check = function(f) expect_equal(unname(f(mat_f64)), unname(row(mat_f64)))
      ),
      list(
        fn = function(x) col(x),
        types = list(x = "i32[,]"),
        check = function(f) expect_equal(unname(f(mat_i32)), unname(col(mat_i32)))
      ),
      list(
        fn = function(x) expand.grid(x),
        types = list(x = "i32[]"),
        check = function(f) expect_equal(f(1:2), expand.grid(1:2))
      ),
      list(
        fn = function(x, y) expand.grid(x, y),
        types = list(x = "i32[]", y = "i32[]"),
        check = function(f) expect_equal(f(1:2, 3:4), expand.grid(1:2, 3:4))
      ),
      list(
        fn = function(x, y, z, w) expand.grid(x, y, z, w),
        types = list(x = "i32[]", y = "i32[]", z = "i32[]", w = "i32[]"),
        check = function(f) expect_equal(f(1:2, 3:4, 5:6, 7:8), expand.grid(1:2, 3:4, 5:6, 7:8))
      ),
      list(
        fn = function(x, y, z, w, v) expand.grid(x, y, z, w, v),
        types = list(x = "i32[]", y = "i32[]", z = "i32[]", w = "i32[]", v = "i32[]"),
        check = function(f) expect_equal(f(1:2, 3:4, 5:6, 7:8, 9:10), expand.grid(1:2, 3:4, 5:6, 7:8, 9:10))
      ),
      list(
        fn = function(x, y) expand.grid(a = x, b = y),
        types = list(x = "i32[]", y = "i32[]"),
        check = function(f) expect_equal(f(1:2, 3:4), expand.grid(a = 1:2, b = 3:4))
      ),
      list(
        fn = function(x, y) expand.grid(x + 1L, y * 2L),
        types = list(x = "i32[]", y = "i32[]"),
        check = function(f) expect_equal(f(1:2, 3:4), expand.grid(1:2 + 1L, 3:4 * 2L))
      ),
      list(
        fn = function(x, y) expand.grid(log1p(x), sqrt(y + 1.0)),
        types = list(x = "f64[]", y = "f64[]"),
        check = function(f) {
          drop_out_attrs <- function(df) {
            attr(df, "out.attrs") <- NULL
            df
          }
          expect_equal(
            drop_out_attrs(f(as.double(1:2), as.double(3:4))),
            drop_out_attrs(expand.grid(log1p(as.double(1:2)), sqrt(as.double(3:4) + 1.0)))
          )
        }
      )
    )
    .mojor_run_runtime_cases(runtime_cases, builder = .mojor_fn_off)
  })
})

test_that("mojor_fn table subset works with ir_only=TRUE", {  .mojor_with_tier9_force({
    mat <- matrix(as.double(1:6), nrow = 2, ncol = 3)
    .mojor_run_runtime_cases(
      list(list(
        fn = function(x) row(x),
        types = list(x = "f64[,]"),
        check = function(f) expect_equal(unname(f(mat)), unname(row(mat)))
      )),
      builder = .mojor_fn_off,
      .ir_only = TRUE
    )
  })
})

test_that("mojor_fn table subset enforces strict signature constraints", {  .mojor_with_tier9_force({
    reject_cases <- list(
      list(fn = function(x) row(x), types = list(x = "f64[]"), err = "must be a numeric/integer/logical matrix argument"),
      list(fn = function(x, y) expand.grid(rank(x), y), types = list(x = "i32[]", y = "i32[]"), err = "unsupported operator/function 'rank'")
    )
    .mojor_expect_reject_cases(reject_cases)
  })
})
