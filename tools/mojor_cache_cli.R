#!/usr/bin/env Rscript

usage <- function() {
  cat(
"Usage: mojor_cache_cli.R <command> [options]

Commands:
  info   [--cache-dir DIR] [--json] [--no-entries]
  evict  [--cache-dir DIR] [--max-bytes N] [--max-entries N] [--max-age-days N] [--dry-run]
  prune  [--cache-dir DIR] [--dry-run]
  clear  [--cache-dir DIR] [--remove-builds]

Examples:
  Rscript tools/mojor_cache_cli.R info
  Rscript tools/mojor_cache_cli.R info --json
  Rscript tools/mojor_cache_cli.R evict --max-bytes 2e9
  Rscript tools/mojor_cache_cli.R clear --remove-builds
"
  )
}

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0L) return(NA_character_)
  sub("^--file=", "", file_arg[[1]])
}

repo_root <- function() {
  path <- script_path()
  if (!is.na(path)) {
    return(normalizePath(file.path(dirname(path), ".."), mustWork = FALSE))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

load_mojor_namespace <- function() {
  if (requireNamespace("mojor", quietly = TRUE)) {
    return(asNamespace("mojor"))
  }
  pkg_dir <- file.path(repo_root(), "packages", "mojor")
  if (dir.exists(pkg_dir) && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(pkg_dir, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    return(asNamespace("mojor"))
  }
  stop(
    "mojor_cache_cli: unable to load mojor namespace; install mojor or run from repo root with pkgload available",
    call. = FALSE
  )
}

parse_option <- function(args, key, default = NULL) {
  idx <- which(args == key)
  if (length(idx) == 0L) return(default)
  if (idx[[1]] >= length(args)) {
    stop("missing value for ", key, call. = FALSE)
  }
  args[[idx[[1]] + 1L]]
}

has_flag <- function(args, key) {
  any(args == key)
}

to_numeric_or_null <- function(x) {
  if (is.null(x)) return(NULL)
  as.numeric(x)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L || any(args %in% c("--help", "-h"))) {
    usage()
    quit(status = 0L)
  }

  cmd <- args[[1]]
  cmd_args <- if (length(args) > 1L) args[-1] else character(0)
  mojor <- load_mojor_namespace()

  if (identical(cmd, "info")) {
    cache_dir <- parse_option(cmd_args, "--cache-dir", default = NULL)
    include_entries <- !has_flag(cmd_args, "--no-entries")
    info <- mojor$mojor_cache_info(cache_dir = cache_dir, include_entries = include_entries)
    if (has_flag(cmd_args, "--json")) {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("mojor_cache_cli: jsonlite is required for --json output", call. = FALSE)
      }
      cat(jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE), "\n", sep = "")
    } else {
      invisible(mojor$mojor_cache_print(cache_dir = cache_dir))
    }
    quit(status = 0L)
  }

  if (identical(cmd, "evict")) {
    cache_dir <- parse_option(cmd_args, "--cache-dir", default = NULL)
    max_bytes <- to_numeric_or_null(parse_option(cmd_args, "--max-bytes", default = NULL))
    max_entries <- to_numeric_or_null(parse_option(cmd_args, "--max-entries", default = NULL))
    max_age_days <- to_numeric_or_null(parse_option(cmd_args, "--max-age-days", default = NULL))
    dry_run <- has_flag(cmd_args, "--dry-run")
    res <- mojor$mojor_cache_evict(
      cache_dir = cache_dir,
      max_bytes = max_bytes,
      max_entries = max_entries,
      max_age_days = max_age_days,
      dry_run = dry_run
    )
    print(res)
    quit(status = 0L)
  }

  if (identical(cmd, "prune")) {
    cache_dir <- parse_option(cmd_args, "--cache-dir", default = NULL)
    dry_run <- has_flag(cmd_args, "--dry-run")
    res <- mojor$mojor_cache_prune(cache_dir = cache_dir, dry_run = dry_run)
    print(res)
    quit(status = 0L)
  }

  if (identical(cmd, "clear")) {
    cache_dir <- parse_option(cmd_args, "--cache-dir", default = NULL)
    remove_builds <- has_flag(cmd_args, "--remove-builds")
    res <- mojor$mojor_jit_clear_cache(cache_dir = cache_dir, remove_builds = remove_builds)
    cat(if (isTRUE(res)) "TRUE\n" else "FALSE\n")
    quit(status = if (isTRUE(res)) 0L else 1L)
  }

  stop("Unknown command: ", cmd, call. = FALSE)
}

main()
