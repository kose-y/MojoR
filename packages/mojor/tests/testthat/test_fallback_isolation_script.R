library(testthat)

.fallback_script_path <- function() {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", "check_fallback_isolation.sh")
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  stop("could not locate scripts/check_fallback_isolation.sh from test root")
}

.fallback_script_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

.fallback_count_scope <- function(pattern_file, scope_dir) {
  raw_patterns <- readLines(pattern_file, warn = FALSE)
  norm_patterns <- trimws(raw_patterns)
  norm_patterns <- norm_patterns[nzchar(norm_patterns)]
  norm_patterns <- norm_patterns[!grepl("^#", norm_patterns)]
  if (length(norm_patterns) == 0L) stop("no active patterns in ", pattern_file)
  tmp_patterns <- tempfile("fallback_patterns_norm_", fileext = ".txt")
  on.exit(unlink(tmp_patterns), add = TRUE)
  writeLines(norm_patterns, tmp_patterns)

  out <- suppressWarnings(system2(
    "rg",
    c("-n", "-S", "-f", tmp_patterns, scope_dir),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  if (identical(status, 1L)) return(0L)
  if (!identical(status, 0L)) stop("failed to count fallback behavior hits for ", scope_dir)
  length(out)
}

.fallback_write_baseline <- function(path, pkg_hits) {
  writeLines(
    c(
      "scope,max_hits",
      paste0("packages/mojor/R,", as.integer(pkg_hits))
    ),
    path
  )
}

.fallback_active_patterns <- function(pattern_file) {
  raw_patterns <- readLines(pattern_file, warn = FALSE)
  norm_patterns <- trimws(raw_patterns)
  norm_patterns <- norm_patterns[nzchar(norm_patterns)]
  norm_patterns[!grepl("^#", norm_patterns)]
}

test_that("fallback behavior catalog keeps intentional family floor at three anchors", {
  script <- .fallback_script_path()
  repo <- dirname(dirname(script))
  patterns <- file.path(repo, "scripts", "fallback_behavior_patterns.txt")
  baseline <- file.path(repo, "docs", "BASELINES", "FALLBACK_HOTSPOTS_BASELINE.csv")
  expect_true(file.exists(patterns))
  expect_true(file.exists(baseline))

  active <- .fallback_active_patterns(patterns)
  expect_identical(length(active), 3L)
  expect_true(any(grepl("^mojor_build: object_mode fallback enabled:$", active)))
  expect_true(any(grepl("^using subprocess fallback wrapper$", active)))
  expect_true(any(grepl("^gpu_func_raw: broadcast_nd GPU launch failed; falling back to CPU host execution$", active)))

  base_tbl <- read.csv(baseline, stringsAsFactors = FALSE)
  pkg_cap <- as.integer(base_tbl$max_hits[base_tbl$scope == "packages/mojor/R"])
  expect_identical(pkg_cap, 3L)
})

test_that("fallback isolation script passes with matching behavior baseline", {
  script <- .fallback_script_path()
  if (!nzchar(Sys.which("rg"))) skip("rg not available")
  repo <- dirname(dirname(script))
  patterns <- file.path(repo, "scripts", "fallback_behavior_patterns.txt")
  expect_true(file.exists(patterns))

  pkg_hits <- .fallback_count_scope(patterns, file.path(repo, "packages", "mojor", "R"))
  baseline <- tempfile("fallback_base_", fileext = ".csv")
  on.exit(unlink(baseline), add = TRUE)
  .fallback_write_baseline(baseline, pkg_hits)

  out <- system2(
    "bash",
    c(script, "--baseline", baseline, "--patterns", patterns),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.fallback_script_status(out), 0L)
  expect_true(any(grepl("Fallback isolation check: clean", out, fixed = TRUE)))
})

test_that("fallback isolation script fails on growth and allow-growth overrides", {
  script <- .fallback_script_path()
  if (!nzchar(Sys.which("rg"))) skip("rg not available")
  repo <- dirname(dirname(script))
  patterns <- file.path(repo, "scripts", "fallback_behavior_patterns.txt")

  pkg_hits <- .fallback_count_scope(patterns, file.path(repo, "packages", "mojor", "R"))
  if (pkg_hits == 0L) skip("no behavior fallback hits to validate growth failure")

  baseline <- tempfile("fallback_growth_", fileext = ".csv")
  on.exit(unlink(baseline), add = TRUE)
  .fallback_write_baseline(baseline, max(pkg_hits - 1L, 0L))

  out_fail <- suppressWarnings(system2(
    "bash",
    c(script, "--baseline", baseline, "--patterns", patterns),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.fallback_script_status(out_fail), 1L)
  expect_true(any(grepl("Fallback hotspot growth detected.", out_fail, fixed = TRUE)))

  out_allow <- system2(
    "bash",
    c(script, "--baseline", baseline, "--patterns", patterns, "--allow-growth"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.fallback_script_status(out_allow), 0L)
  expect_true(any(grepl("continuing (--allow-growth)", out_allow, fixed = TRUE)))
})

test_that("fallback isolation script returns input errors for malformed inputs", {
  script <- .fallback_script_path()
  repo <- dirname(dirname(script))
  patterns <- file.path(repo, "scripts", "fallback_behavior_patterns.txt")

  baseline_missing_scope <- tempfile("fallback_missing_scope_", fileext = ".csv")
  on.exit(unlink(baseline_missing_scope), add = TRUE)
  writeLines(
    c(
      "scope,max_hits",
      "legacy/scope,0"
    ),
    baseline_missing_scope
  )

  out_scope <- suppressWarnings(system2(
    "bash",
    c(script, "--baseline", baseline_missing_scope, "--patterns", patterns),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.fallback_script_status(out_scope), 2L)
  expect_true(any(grepl("missing from baseline", out_scope, fixed = TRUE)))

  out_missing_patterns <- suppressWarnings(system2(
    "bash",
    c(script, "--patterns", file.path(tempdir(), "does_not_exist_patterns.txt")),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.fallback_script_status(out_missing_patterns), 2L)
  expect_true(any(grepl("patterns file not found", out_missing_patterns, fixed = TRUE)))

  empty_patterns <- tempfile("fallback_patterns_empty_", fileext = ".txt")
  on.exit(unlink(empty_patterns), add = TRUE)
  writeLines(c("# comment", "   ", "\t"), empty_patterns)
  out_empty_patterns <- suppressWarnings(system2(
    "bash",
    c(script, "--patterns", empty_patterns),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.fallback_script_status(out_empty_patterns), 2L)
  expect_true(any(grepl("patterns file has no active patterns", out_empty_patterns, fixed = TRUE)))
})

test_that("fallback isolation verbose output lists behavior hotspots", {
  script <- .fallback_script_path()
  if (!nzchar(Sys.which("rg"))) skip("rg not available")
  repo <- dirname(dirname(script))
  patterns <- file.path(repo, "scripts", "fallback_behavior_patterns.txt")

  pkg_hits <- .fallback_count_scope(patterns, file.path(repo, "packages", "mojor", "R"))
  baseline <- tempfile("fallback_verbose_", fileext = ".csv")
  on.exit(unlink(baseline), add = TRUE)
  .fallback_write_baseline(baseline, pkg_hits)

  out <- system2(
    "bash",
    c(script, "--baseline", baseline, "--patterns", patterns, "--verbose"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.fallback_script_status(out), 0L)
  expect_true(any(grepl("Top fallback behavior hotspots:", out, fixed = TRUE)))
})
