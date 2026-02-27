library(testthat)

test_that("mojor_jit cache options include parallel and tuning dimensions", {  body_text <- paste(deparse(body(mojor_jit)), collapse = "\n")
  expect_true(grepl("parallel\\s*=\\s*isTRUE\\(parallel\\)", body_text))
  expect_true(grepl("semantics\\s*=\\s*semantics", body_text))
  expect_true(grepl("fast_math = if (is.null(fast_math))", body_text, fixed = TRUE))
  expect_true(grepl("bounds_check = if (is.null(bounds_check))", body_text, fixed = TRUE))
})

test_that(".mojor_signature_key differentiates target/strictness/parallel/tuning options", {  types <- list(x = "f64[]")
  opts_base <- list(
    elementwise = TRUE,
    elementwise_target = "cpu",
    elementwise_size = "",
    elementwise_cpu = "",
    broadcast = "none",
    parallel = FALSE,
    object_mode = "off",
    semantics = "r",
    fast_math = "auto",
    bounds_check = "auto"
  )
  opts_gpu <- opts_base
  opts_gpu$elementwise_target <- "gpu"
  opts_parallel <- opts_base
  opts_parallel$parallel <- TRUE
  opts_hybrid <- opts_base
  opts_hybrid$object_mode <- "hybrid"
  opts_raw <- opts_base
  opts_raw$semantics <- "raw"
  opts_fast_math <- opts_base
  opts_fast_math$fast_math <- "TRUE"
  opts_no_bounds <- opts_base
  opts_no_bounds$bounds_check <- "FALSE"

  key_base <- .mojor_signature_key(types, opts_base)
  key_gpu <- .mojor_signature_key(types, opts_gpu)
  key_parallel <- .mojor_signature_key(types, opts_parallel)
  key_hybrid <- .mojor_signature_key(types, opts_hybrid)
  key_raw <- .mojor_signature_key(types, opts_raw)
  key_fast_math <- .mojor_signature_key(types, opts_fast_math)
  key_no_bounds <- .mojor_signature_key(types, opts_no_bounds)

  expect_false(identical(key_base, key_gpu))
  expect_false(identical(key_base, key_parallel))
  expect_false(identical(key_base, key_hybrid))
  expect_false(identical(key_base, key_raw))
  expect_false(identical(key_base, key_fast_math))
  expect_false(identical(key_base, key_no_bounds))
})

test_that(".mojor_jit_fn_hash is stable across environment churn", {  env <- new.env(parent = baseenv())
  f <- with(env, function(x) x + 1)
  h1 <- .mojor_jit_fn_hash(f)
  env$tmp <- runif(1)
  env$tmp2 <- rnorm(2)
  h2 <- .mojor_jit_fn_hash(f)
  expect_identical(h1, h2)
})

test_that("mojor_njit and equivalent mojor_jit share a single disk-cache entry", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }

  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_njit_parity_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(12))
  nj <- mojor_njit(f, name = "cache_njit_parity", disk_cache = TRUE, cache_dir = cache_dir, parallel = TRUE)
  jj <- mojor_jit(f, name = "cache_jit_parity", disk_cache = TRUE, cache_dir = cache_dir, parallel = TRUE, object_mode = "off")

  expect_equal(nj(x), f(x), tolerance = 1e-12)
  expect_equal(jj(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 1L)
  signatures <- vapply(index, function(entry) entry$signature, character(1))
  expect_true(all(grepl("parallel=TRUE", signatures, fixed = TRUE)))
  expect_true(all(grepl("object_mode=off", signatures, fixed = TRUE)))
})

test_that("mojor_jit disk cache separates parallel and non-parallel signatures", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    acc <- 0.0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
      out[i] <- acc
    }
    out
  }

  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_parallel_sep_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(16))
  jit_seq <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir, parallel = FALSE)
  jit_par <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir, parallel = TRUE)

  expect_equal(jit_seq(x), f(x), tolerance = 1e-12)
  expect_equal(jit_par(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 2L)

  signatures <- vapply(index, function(entry) entry$signature, character(1))
  expect_true(any(grepl("parallel=FALSE", signatures, fixed = TRUE)))
  expect_true(any(grepl("parallel=TRUE", signatures, fixed = TRUE)))
})

test_that("mojor_vectorize and equivalent mojor_jit share a single disk-cache entry", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2 + 3
    out
  }

  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_vectorize_parity_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(10))
  vj <- mojor_vectorize(f, name = "cache_vectorize_parity", disk_cache = TRUE, cache_dir = cache_dir, target = "cpu")
  jj <- mojor_jit(
    f,
    name = "cache_jit_vectorize_parity",
    disk_cache = TRUE,
    cache_dir = cache_dir,
    elementwise = TRUE,
    elementwise_target = "cpu",
    object_mode = "off"
  )

  expect_equal(vj(x), f(x), tolerance = 1e-12)
  expect_equal(jj(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 1L)
  signatures <- vapply(index, function(entry) entry$signature, character(1))
  expect_true(all(grepl("elementwise=TRUE", signatures, fixed = TRUE)))
  expect_true(all(grepl("elementwise_target=cpu", signatures, fixed = TRUE)))
  expect_true(all(grepl("object_mode=off", signatures, fixed = TRUE)))
})

test_that("mojor_jit disk cache separates strictness by object_mode", {  skip_if_no_mojo()
  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  mojor_options(ir_only = FALSE)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 5
    out
  }

  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_object_mode_sep_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(9))
  jit_off <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir, object_mode = "off")
  jit_fallback <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir, object_mode = "fallback")

  expect_equal(jit_off(x), f(x), tolerance = 1e-12)
  expect_equal(jit_fallback(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 2L)
  signatures <- vapply(index, function(entry) entry$signature, character(1))
  expect_true(any(grepl("object_mode=off", signatures, fixed = TRUE)))
  expect_true(any(grepl("object_mode=fallback", signatures, fixed = TRUE)))
})

test_that("mojor_jit disk cache separates semantics/fast_math/bounds_check options", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2 + 1
    out
  }

  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_tuning_sep_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(11))
  jit_default <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  jit_tuned <- mojor_jit(
    f,
    disk_cache = TRUE,
    cache_dir = cache_dir,
    semantics = "raw",
    fast_math = TRUE,
    bounds_check = FALSE
  )

  expect_equal(jit_default(x), f(x), tolerance = 1e-12)
  expect_equal(suppressWarnings(jit_tuned(x)), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 2L)
  signatures <- vapply(index, function(entry) entry$signature, character(1))
  expect_true(any(grepl("semantics=r", signatures, fixed = TRUE)))
  expect_true(any(grepl("semantics=raw", signatures, fixed = TRUE)))
  expect_true(any(grepl("fast_math=auto", signatures, fixed = TRUE)))
  expect_true(any(grepl("fast_math=TRUE", signatures, fixed = TRUE)))
  expect_true(any(grepl("bounds_check=auto", signatures, fixed = TRUE)))
  expect_true(any(grepl("bounds_check=FALSE", signatures, fixed = TRUE)))
})

test_that("mojor_jit cache entries include compatibility metadata", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_compat_meta_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  jit <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  x <- as.double(seq_len(8))
  expect_equal(jit(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 1L)
  entry <- index[[1]]
  expect_true(is.list(entry$compat))
  expect_true("cache_schema_version" %in% names(entry$compat))
  expect_true("transpile_version" %in% names(entry$compat))
  expect_true("api_surface_version" %in% names(entry$compat))
  expect_true("r_version" %in% names(entry$compat))
  expect_true("platform" %in% names(entry$compat))
  expect_true("bridge_pkg" %in% names(entry$compat))
  expect_true("bridge_path_hash" %in% names(entry$compat))
})

test_that("mojor_jit skips incompatible disk cache entries and recompiles", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 3
    out
  }
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_cache_compat_skip_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(9))
  jit1 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit1(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  expect_length(index, 1L)
  key <- names(index)[[1]]
  entry <- index[[key]]
  entry$compat$transpile_version <- "bad_transpile_version_for_test"
  index[[key]] <- entry
  saveRDS(index, file.path(cache_dir, "jit_index.rds"))

  jit2 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit2(x), f(x), tolerance = 1e-12)
  info <- mojor_jit_info(jit2)
  expect_identical(info$stats$cache_hits_disk, 0L)
  expect_identical(info$stats$cache_misses_compile, 1L)
  expect_identical(info$stats$cache_incompatible_skips, 1L)
  expect_identical(info$signatures[[1]]$last_event, "compiled")
})

test_that("mojor_cache_info reports compatibility summary and incompatibility reasons", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 7
    out
  }
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_cache_info_compat_summary_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(5))
  jit <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit(x), f(x), tolerance = 1e-12)

  info_ok <- mojor_cache_info(cache_dir = cache_dir)
  expect_true(is.list(info_ok$jit_compat_summary))
  expect_identical(info_ok$jit_compat_summary$total_index_entries, 1L)
  expect_identical(info_ok$jit_compat_summary$stamped_entries, 1L)
  expect_identical(info_ok$jit_compat_summary$compatible_entries, 1L)
  expect_identical(info_ok$jit_compat_summary$incompatible_entries, 0L)

  index <- .mojor_jit_cache_read(cache_dir)
  key <- names(index)[[1]]
  index[[key]]$compat$platform <- "bad_platform_for_test"
  saveRDS(index, file.path(cache_dir, "jit_index.rds"))

  info_bad <- mojor_cache_info(cache_dir = cache_dir, include_entries = FALSE)
  expect_identical(info_bad$jit_compat_summary$total_index_entries, 1L)
  expect_identical(info_bad$jit_compat_summary$stamped_entries, 1L)
  expect_identical(info_bad$jit_compat_summary$incompatible_entries, 1L)
  expect_identical(info_bad$jit_compat_summary$compatible_entries, 0L)
  expect_identical(nrow(info_bad$entries), 0L)
  expect_identical(nrow(info_bad$jit_entries), 0L)
  expect_true(any(grepl(
    "compat mismatch: platform",
    info_bad$jit_compat_summary$incompatible_reasons$reason,
    fixed = TRUE
  )))
})

test_that("mojor_cache_info counts legacy compatibility entries", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 4
    out
  }
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_cache_info_legacy_summary_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x <- as.double(seq_len(6))
  jit <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit(x), f(x), tolerance = 1e-12)

  index <- .mojor_jit_cache_read(cache_dir)
  key <- names(index)[[1]]
  index[[key]]$compat <- NULL
  saveRDS(index, file.path(cache_dir, "jit_index.rds"))

  info <- mojor_cache_info(cache_dir = cache_dir)
  expect_identical(info$jit_compat_summary$total_index_entries, 1L)
  expect_identical(info$jit_compat_summary$legacy_entries, 1L)
  expect_identical(info$jit_compat_summary$stamped_entries, 0L)
  expect_identical(info$jit_compat_summary$incompatible_entries, 0L)
})
