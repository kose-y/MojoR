#!/usr/bin/env Rscript

parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

quit_input_error <- function(msg) {
  cat(msg, "\n", file = stderr())
  quit(status = 2)
}

extract_mean_from_summary <- function(lines, header_pattern, required = TRUE) {
  idx <- grep(header_pattern, lines)
  if (length(idx) == 0) {
    if (isTRUE(required)) {
      quit_input_error(paste0("missing summary header: ", header_pattern))
    }
    return(NA_real_)
  }
  start <- idx[[1]] + 1L
  while (start <= length(lines) && !nzchar(trimws(lines[[start]]))) {
    start <- start + 1L
  }
  if (start > length(lines)) {
    if (isTRUE(required)) {
      quit_input_error(paste0("missing summary values after: ", header_pattern))
    }
    return(NA_real_)
  }
  line <- trimws(lines[[start]])
  if (!grepl("mean=", line, fixed = TRUE)) {
    if (isTRUE(required)) {
      quit_input_error(paste0("summary line missing mean= after: ", header_pattern))
    }
    return(NA_real_)
  }
  mean_str <- sub("^.*mean=([0-9]+(?:\\.[0-9]+)?).*$", "\\1", line, perl = TRUE)
  val <- suppressWarnings(as.numeric(mean_str))
  if (!is.finite(val)) {
    if (isTRUE(required)) {
      quit_input_error(paste0("failed to parse mean value after: ", header_pattern))
    }
    return(NA_real_)
  }
  val
}

extract_scalar_sec <- function(lines, label_pattern, required = TRUE) {
  idx <- grep(label_pattern, lines)
  if (length(idx) == 0) {
    if (isTRUE(required)) {
      quit_input_error(paste0("missing line: ", label_pattern))
    }
    return(NA_real_)
  }
  line <- trimws(lines[[idx[[1]]]])
  val_str <- sub("^.*: *([0-9]+(?:\\.[0-9]+)?) sec$", "\\1", line, perl = TRUE)
  val <- suppressWarnings(as.numeric(val_str))
  if (!is.finite(val)) {
    if (isTRUE(required)) {
      quit_input_error(paste0("failed to parse scalar value: ", label_pattern))
    }
    return(NA_real_)
  }
  val
}

args <- commandArgs(trailingOnly = TRUE)
input_path <- parse_cli_arg(args, "input", "")
output_path <- parse_cli_arg(args, "output", "")

if (!nzchar(input_path)) {
  quit_input_error("usage: parse_gibbs_benchmark_log.R --input=<log> [--output=<csv>]")
}
if (!file.exists(input_path)) {
  quit_input_error(paste0("input log not found: ", input_path))
}

lines <- readLines(input_path, warn = FALSE)

metrics <- c(
  gibbs_r_mean_ms = extract_mean_from_summary(lines, "^Summary \\(sec\\)$", required = TRUE) * 1000,
  gibbs_mojor_build_ms = extract_scalar_sec(lines, "^MojoR build time:", required = TRUE) * 1000,
  gibbs_mojor_mean_ms = extract_mean_from_summary(lines, "^MojoR Summary \\(sec\\)$", required = TRUE) * 1000,
  gibbs_mojor_fast_build_ms = extract_scalar_sec(lines, "^MojoR fast build time:", required = TRUE) * 1000,
  gibbs_mojor_fast_mean_ms = extract_mean_from_summary(lines, "^MojoR fast Summary \\(sec\\)$", required = TRUE) * 1000
)

bulk_build_sec <- extract_scalar_sec(lines, "^MojoR bulk build time:", required = FALSE)
bulk_mean_sec <- extract_mean_from_summary(lines, "^MojoR bulk Summary \\(sec\\)$", required = FALSE)
if (is.finite(bulk_build_sec)) {
  metrics <- c(metrics, gibbs_bulk_build_ms = bulk_build_sec * 1000)
}
if (is.finite(bulk_mean_sec)) {
  metrics <- c(metrics, gibbs_bulk_mean_ms = bulk_mean_sec * 1000)
}

out <- data.frame(
  name = names(metrics),
  ms = as.numeric(metrics),
  stringsAsFactors = FALSE
)

if (nzchar(output_path)) {
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  write.csv(out, output_path, row.names = FALSE, quote = FALSE)
  cat(sprintf("Wrote Gibbs benchmark metrics CSV: %s\n", output_path))
} else {
  write.csv(out, stdout(), row.names = FALSE, quote = FALSE)
}
