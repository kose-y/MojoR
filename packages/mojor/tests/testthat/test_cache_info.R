library(testthat)


test_that("mojor_cache_info returns structure", {  info <- mojor_cache_info()
  expect_true(is.list(info))
  expect_true("path" %in% names(info))
  expect_true("entries" %in% names(info))
  expect_true("total_bytes" %in% names(info))
  expect_true("jit_entries" %in% names(info))
  expect_true("jit_total_bytes" %in% names(info))
  expect_true("jit_policy" %in% names(info))
  expect_true("jit_compat_summary" %in% names(info))
  expect_true(is.list(info$jit_compat_summary))
})

test_that("mojor_cache_info summary mode omits heavy entry materialization", {  cache_dir <- file.path(
  tempdir(),
  paste0("mojor_cache_info_summary_", Sys.getpid(), "_", sample.int(1e6, 1))
)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(cache_dir, "dummy_key"), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1:16), file.path(cache_dir, "dummy_key", "blob.bin"))

  info <- mojor_cache_info(cache_dir = cache_dir, include_entries = FALSE)
  expect_true(is.list(info))
  expect_true("entries" %in% names(info))
  expect_true("jit_entries" %in% names(info))
  expect_true("jit_compat_summary" %in% names(info))
  expect_identical(nrow(info$entries), 0L)
  expect_identical(nrow(info$jit_entries), 0L)
  expect_true(info$total_bytes > 0)
  expect_true(is.list(info$jit_compat_summary))
  expect_true(all(c(
    "total_index_entries",
    "stamped_entries",
    "legacy_entries",
    "compatible_entries",
    "incompatible_entries",
    "incompatible_reasons"
  ) %in% names(info$jit_compat_summary)))
})

test_that("mojor_cache_info validates include_entries", {
  expect_error(
    mojor_cache_info(include_entries = "yes"),
    "include_entries must be TRUE or FALSE"
  )
})

test_that("mojor_cache_evict and mojor_jit_clear_cache return", {  cache_dir <- file.path(tempdir(), paste0("mojor_cache_test_", Sys.getpid()))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  res <- mojor_cache_evict(cache_dir = cache_dir, max_entries = 0, dry_run = TRUE)
  expect_true(is.list(res))
  expect_true("jit_removed" %in% names(res))
  expect_true("jit_remaining" %in% names(res))
  cleared <- mojor_jit_clear_cache(cache_dir = cache_dir, remove_builds = FALSE)
  expect_true(isTRUE(cleared))
})

test_that("mojor_cache_prune respects policy defaults", {  cache_dir <- file.path(tempdir(), paste0("mojor_cache_prune_", Sys.getpid()))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  old <- mojor_options()
  mojor_options(
    jit_cache_max_bytes = 1,
    jit_cache_max_entries = 1,
    jit_cache_max_age_days = 0.0001
  )
  res <- mojor_cache_prune(cache_dir = cache_dir, dry_run = TRUE)
  do.call(mojor_options, old)
  expect_true(is.list(res))
  expect_true("remaining_bytes" %in% names(res))
})

test_that("mojor_cache_cli --help prints usage", {
  script <- NA_character_
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "tools", "mojor_cache_cli.R")
    if (file.exists(cand)) {
      script <- normalizePath(cand, mustWork = TRUE)
      break
    }
    cur <- dirname(cur)
  }
  if (!is.character(script) || length(script) != 1L || !nzchar(script) || !file.exists(script)) {
    skip("tools/mojor_cache_cli.R not available in this test layout")
  }
  rscript_bin <- if (nzchar(Sys.getenv("R_HOME", unset = ""))) {
    file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  } else {
    Sys.which("Rscript")
  }
  out <- suppressWarnings(system2(rscript_bin, c(script, "--help"), stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("Usage:|USAGE:", out)))
})

test_that("mojor_cache_print returns info invisibly", {  info <- mojor_cache_print()
  expect_true(is.list(info))
  expect_true("total_bytes" %in% names(info))
})

test_that("mojor_cache_print formats bytes", {  out <- capture.output(mojor_cache_print())
  expect_true(any(grepl("cache_bytes:", out)))
  expect_true(any(grepl("jit_bytes:", out)))
})
