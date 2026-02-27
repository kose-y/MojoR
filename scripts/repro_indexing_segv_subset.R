#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(name, default = NULL) {
  key <- paste0("--", name, "=")
  hit <- args[startsWith(args, key)]
  if (length(hit) == 0) return(default)
  sub(key, "", hit[[1]], fixed = TRUE)
}

as_int_or_empty <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  y <- suppressWarnings(as.integer(x))
  if (is.na(y)) stop("Expected integer value, got: ", x)
  as.character(y)
}

parse_targets <- function(x) {
  if (is.null(x) || !nzchar(x)) return(integer(0))
  tokens <- strsplit(x, ",", fixed = TRUE)[[1]]
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  out <- integer(0)
  for (t in tokens) {
    if (grepl("^[0-9]+[:\\-][0-9]+$", t)) {
      parts <- strsplit(t, "[:\\-]")[[1]]
      a <- suppressWarnings(as.integer(parts[[1]]))
      b <- suppressWarnings(as.integer(parts[[2]]))
      if (!is.na(a) && !is.na(b)) {
        out <- c(out, seq.int(a, b))
      }
    } else {
      v <- suppressWarnings(as.integer(t))
      if (!is.na(v)) out <- c(out, v)
    }
  }
  sort(unique(out))
}

file_path <- get_opt("file", "packages/mojor/tests/testthat/test_indexing_comprehensive.R")
context_end <- suppressWarnings(as.integer(get_opt("context_end", "128")))
repeats <- suppressWarnings(as.integer(get_opt("repeats", "4")))
if (is.na(context_end) || context_end < 0) context_end <- 128L
if (is.na(repeats) || repeats < 1) repeats <- 4L
trace_tests <- tolower(get_opt("trace_tests", "false")) %in% c("1", "true", "yes", "y", "on")
diag_index <- tolower(get_opt("diag_index", "false")) %in% c("1", "true", "yes", "y", "on")
reset_mode <- tolower(get_opt("reset_mode", "hard"))
file_reset_mode <- tolower(get_opt("file_reset_mode", reset_mode))
keep_backend <- tolower(get_opt("keep_backend", "true")) %in% c("1", "true", "yes", "y", "on")
verbose <- tolower(get_opt("verbose", "false")) %in% c("1", "true", "yes", "y", "on")
clear_cache_each_run <- tolower(get_opt("clear_cache_each_run", "false")) %in% c("1", "true", "yes", "y", "on")
reporter <- tolower(get_opt("reporter", "summary"))
stress_opt_level <- as_int_or_empty(get_opt("stress_opt_level", ""))
disable_r_jit <- tolower(get_opt("disable_r_jit", "false")) %in% c("1", "true", "yes", "y", "on")
per_test_reset <- tolower(get_opt("per_test_reset", "true")) %in% c("1", "true", "yes", "y", "on")
if (!(reset_mode %in% c("soft", "hard"))) stop("--reset_mode must be one of: soft, hard")
if (!(file_reset_mode %in% c("off", "none", "soft", "hard"))) stop("--file_reset_mode must be one of: off, none, soft, hard")
if (!(reporter %in% c("summary", "check", "minimal"))) stop("--reporter must be one of: summary, check, minimal")

reporter_ctor <- switch(
  reporter,
  summary = "testthat::SummaryReporter$new()",
  check = "testthat::CheckReporter$new()",
  minimal = "testthat::MinimalReporter$new()"
)

# High-probability interaction window discovered during bisect probing.
target_ids <- parse_targets(get_opt("targets", ""))
if (length(target_ids) == 0L) target_ids <- c(129:148, 160)

exprs <- parse(file_path)
get_desc <- function(e) {
  if (is.call(e) && identical(as.character(e[[1]]), "test_that") && length(e) >= 2 && is.character(e[[2]]) && length(e[[2]]) == 1) {
    return(as.character(e[[2]]))
  }
  NA_character_
}

descs <- vapply(exprs, get_desc, character(1))
test_expr_idx <- which(!is.na(descs))
if (length(test_expr_idx) == 0L) stop("No test_that blocks found")
max_test <- length(test_expr_idx)
if (context_end > max_test) stop("--context_end out of range: ", context_end, " > ", max_test)
bad_targets <- target_ids[target_ids < 1L | target_ids > max_test]
if (length(bad_targets) > 0L) stop("--targets out of range: ", paste(bad_targets, collapse = ","))
preamble_idx <- if (min(test_expr_idx) > 1L) seq_len(min(test_expr_idx) - 1L) else integer(0)

test_dir <- dirname(file_path)
repo_root <- normalizePath(file.path(test_dir, "..", "..", "..", ".."), winslash = "/", mustWork = FALSE)
cache_cli <- file.path(repo_root, "tools", "mojor_cache_cli.R")
render_expr <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = "\n")

run_once <- function(rep_i) {
  if (isTRUE(clear_cache_each_run) && file.exists(cache_cli)) {
    try(
      system2("Rscript", c(cache_cli, "clear", "--remove-builds"), stdout = TRUE, stderr = TRUE),
      silent = TRUE
    )
  }
  test_ids <- unique(c(seq_len(context_end), target_ids))
  subset_expr_idx <- c(preamble_idx, test_expr_idx[test_ids])

  subset_file <- file.path(test_dir, sprintf(".tmp_repro_idx_segv_%02d.R", rep_i))
  runner_file <- file.path(test_dir, sprintf(".tmp_repro_idx_segv_runner_%02d.R", rep_i))
  on.exit({
    if (file.exists(subset_file)) unlink(subset_file)
    if (file.exists(runner_file)) unlink(runner_file)
  }, add = TRUE)

  lines <- unlist(lapply(exprs[subset_expr_idx], function(e) c(render_expr(e), "")), use.names = FALSE)
  writeLines(lines, subset_file, useBytes = TRUE)
  subset_norm <- normalizePath(subset_file, winslash = "/", mustWork = TRUE)
  runner_lines <- c(
    "library(testthat)",
    sprintf("Sys.setenv(MOJOR_TEST_RESET_MODE=%s)", dQuote(reset_mode)),
    sprintf("Sys.setenv(MOJOR_TEST_FILE_RESET_MODE=%s)", dQuote(file_reset_mode)),
    sprintf("Sys.setenv(MOJOR_TEST_KEEP_BACKEND=%s)", dQuote(if (isTRUE(keep_backend)) "true" else "false")),
    sprintf("Sys.setenv(MOJOR_TEST_TRACE_TESTS=%s)", dQuote(if (isTRUE(trace_tests)) "1" else "0")),
    sprintf("Sys.setenv(MOJOR_TEST_DIAG_INDEX=%s)", dQuote(if (isTRUE(diag_index)) "1" else "0")),
    sprintf("Sys.setenv(MOJOR_TEST_STRESS_OPT_LEVEL=%s)", dQuote(stress_opt_level)),
    sprintf("Sys.setenv(MOJOR_TEST_DISABLE_R_JIT=%s)", dQuote(if (isTRUE(disable_r_jit)) "1" else "0")),
    sprintf("Sys.setenv(MOJOR_TEST_PER_TEST_RESET=%s)", dQuote(if (isTRUE(per_test_reset)) "1" else "0")),
    sprintf(
      "testthat::test_file(%s, reporter = %s, stop_on_failure = FALSE, stop_on_warning = FALSE)",
      dQuote(subset_norm),
      reporter_ctor
    )
  )
  writeLines(runner_lines, runner_file)

  out <- tryCatch(system2("Rscript", c(runner_file), stdout = TRUE, stderr = TRUE), error = function(e) structure(conditionMessage(e), status = 127L))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  crashed <- any(grepl("caught segfault", out, fixed = TRUE)) || any(grepl("An irrecoverable exception occurred", out, fixed = TRUE)) || status %in% c(134L, 136L, 139L)
  list(status = status, crashed = crashed, output = out)
}

cat(sprintf("File: %s\n", file_path))
cat(sprintf("Context: tests 1..%d\n", context_end))
cat(sprintf("Targets: %s\n", paste(target_ids, collapse = ",")))
cat(sprintf("Repeats: %d\n", repeats))
cat(sprintf("Harness env: reset_mode=%s file_reset_mode=%s keep_backend=%s trace_tests=%s diag_index=%s\n",
            reset_mode, file_reset_mode, keep_backend, trace_tests, diag_index))
cat(sprintf("Harness opt: stress_opt_level=%s\n",
            if (nzchar(stress_opt_level)) stress_opt_level else "<unset>"))
cat(sprintf("Harness opt: disable_r_jit=%s\n", disable_r_jit))
cat(sprintf("Harness opt: per_test_reset=%s\n", per_test_reset))
cat(sprintf("Harness reporter: %s\n", reporter))
cat(sprintf("Harness cache: clear_cache_each_run=%s (cli=%s)\n",
            clear_cache_each_run, if (file.exists(cache_cli)) "present" else "missing"))

crashes <- 0L
for (i in seq_len(repeats)) {
  res <- run_once(i)
  crashes <- crashes + as.integer(res$crashed)
  cat(sprintf("run %02d | exit=%d | crash=%s\n", i, res$status, res$crashed))
  if (isTRUE(verbose) || isTRUE(res$crashed)) {
    tail_out <- tail(res$output, 60L)
    cat(paste(tail_out, collapse = "\n"), "\n")
  }
}
cat(sprintf("SUMMARY crashes=%d/%d\n", crashes, repeats))
