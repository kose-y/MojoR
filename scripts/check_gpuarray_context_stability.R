#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_opt <- function(name, default = NULL) {
  key <- paste0("--", name, "=")
  hit <- args[startsWith(args, key)]
  if (length(hit) == 0L) return(default)
  sub(key, "", hit[[1L]], fixed = TRUE)
}

as_int <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  y <- suppressWarnings(as.integer(x))
  if (is.na(y)) default else y
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "yes", "y", "on")
}

as_int_or_empty <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  y <- suppressWarnings(as.integer(x))
  if (is.na(y)) stop("Expected integer value, got: ", x)
  as.character(y)
}

reporter_ctor <- function(name) {
  switch(
    tolower(name),
    summary = "testthat::SummaryReporter$new()",
    check = "testthat::CheckReporter$new()",
    minimal = "testthat::MinimalReporter$new()",
    stop("--reporter must be one of: summary, check, minimal")
  )
}

file_path <- get_opt("file", "packages/mojor/tests/testthat/test_gpu_array.R")
repeats <- as_int(get_opt("repeats", "2"), 2L)
reset_mode <- tolower(get_opt("reset_mode", "soft"))
file_reset_mode <- tolower(get_opt("file_reset_mode", reset_mode))
per_test_reset <- as_flag(get_opt("per_test_reset", "false"), FALSE)
keep_backend <- as_flag(get_opt("keep_backend", "true"), TRUE)
disable_r_jit <- as_flag(get_opt("disable_r_jit", "true"), TRUE)
stress_opt_level <- as_int_or_empty(get_opt("stress_opt_level", "0"))
clear_cache_each_run <- as_flag(get_opt("clear_cache_each_run", "false"), FALSE)
reporter <- tolower(get_opt("reporter", "summary"))
verbose <- as_flag(get_opt("verbose", "false"), FALSE)

if (!file.exists(file_path)) {
  stop("File not found: ", file_path)
}
if (repeats < 1L) {
  stop("--repeats must be >= 1")
}
if (!(reset_mode %in% c("soft", "hard"))) {
  stop("--reset_mode must be one of: soft, hard")
}
if (!(file_reset_mode %in% c("off", "none", "soft", "hard"))) {
  stop("--file_reset_mode must be one of: off, none, soft, hard")
}
reporter_expr <- reporter_ctor(reporter)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
cache_cli <- file.path(repo_root, "tools", "mojor_cache_cli.R")
file_norm <- normalizePath(file_path, winslash = "/", mustWork = TRUE)
runner_file <- tempfile("gpu_context_stability_runner_", fileext = ".R")
on.exit(unlink(runner_file), add = TRUE)

runner_lines <- c(
  "library(testthat)",
  sprintf("Sys.setenv(MOJOR_TEST_RESET_MODE=%s)", dQuote(reset_mode)),
  sprintf("Sys.setenv(MOJOR_TEST_FILE_RESET_MODE=%s)", dQuote(file_reset_mode)),
  sprintf("Sys.setenv(MOJOR_TEST_KEEP_BACKEND=%s)", dQuote(if (isTRUE(keep_backend)) "true" else "false")),
  sprintf("Sys.setenv(MOJOR_TEST_PER_TEST_RESET=%s)", dQuote(if (isTRUE(per_test_reset)) "1" else "0")),
  sprintf("Sys.setenv(MOJOR_TEST_DISABLE_R_JIT=%s)", dQuote(if (isTRUE(disable_r_jit)) "1" else "0")),
  sprintf("Sys.setenv(MOJOR_TEST_STRESS_OPT_LEVEL=%s)", dQuote(stress_opt_level)),
  sprintf("for (.rep in seq_len(%dL)) {", repeats)
)
if (isTRUE(clear_cache_each_run) && file.exists(cache_cli)) {
  cache_cli_norm <- normalizePath(cache_cli, winslash = "/", mustWork = TRUE)
  runner_lines <- c(
    runner_lines,
    sprintf(
      "  try(system2(%s, c(%s, %s, %s), stdout = TRUE, stderr = TRUE), silent = TRUE)",
      dQuote("Rscript"),
      dQuote(cache_cli_norm),
      dQuote("clear"),
      dQuote("--remove-builds")
    )
  )
}
runner_lines <- c(
  runner_lines,
  "  cat(sprintf('GPU_CONTEXT_STABILITY_REPEAT %02d start\\n', .rep))",
  sprintf(
    "  testthat::test_file(%s, reporter = %s, stop_on_failure = FALSE, stop_on_warning = FALSE)",
    dQuote(file_norm),
    reporter_expr
  ),
  "  cat(sprintf('GPU_CONTEXT_STABILITY_REPEAT %02d end\\n', .rep))",
  "}"
)
writeLines(runner_lines, runner_file, useBytes = TRUE)

cat(sprintf("Context stability file: %s\n", file_path))
cat(sprintf("Policy: reset_mode=%s file_reset_mode=%s per_test_reset=%s keep_backend=%s disable_r_jit=%s stress_opt_level=%s\n",
            reset_mode, file_reset_mode, per_test_reset, keep_backend, disable_r_jit,
            if (nzchar(stress_opt_level)) stress_opt_level else "<unset>"))
cat(sprintf("Repeats: %d | Reporter: %s | clear_cache_each_run=%s\n",
            repeats, reporter, clear_cache_each_run))

out <- tryCatch(
  system2("Rscript", c(runner_file), stdout = TRUE, stderr = TRUE),
  error = function(e) structure(paste0("system2 error: ", conditionMessage(e)), status = 127L)
)
status <- attr(out, "status")
if (is.null(status)) status <- 0L

context_markers <- c(
  "mojor_gpu_ctx: invalid context",
  "gpu context unavailable",
  "probe requires GPU context"
)
has_context_failure <- any(vapply(context_markers, function(m) {
  any(grepl(tolower(m), tolower(out), fixed = TRUE))
}, logical(1L)))

if (isTRUE(verbose) || status != 0L || has_context_failure) {
  tail_out <- tail(out, 120L)
  cat(paste(tail_out, collapse = "\n"), "\n")
}

if (has_context_failure) {
  stop("GPU context stability lane detected invalid-context markers", call. = FALSE)
}
if (status != 0L) {
  stop(sprintf("GPU context stability lane failed with exit status %d", status), call. = FALSE)
}

cat(sprintf("PASS: GPU context stability lane (%d repeats)\n", repeats))
