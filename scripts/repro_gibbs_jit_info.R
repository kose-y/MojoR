#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

parse_int <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  out <- suppressWarnings(as.integer(x))
  if (is.na(out) || out < 1L) return(default)
  out
}

all_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", all_args, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Could not resolve script path from --file")
}
this_file <- sub("^--file=", "", file_arg)
repo_root <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)

N <- parse_int(if (length(args) >= 1) args[[1]] else NULL, 300L)
thin <- parse_int(if (length(args) >= 2) args[[2]] else NULL, 20L)

source(file.path(repo_root, "packages", "mojor", "R", "mojor.R"))

load_candidates <- c(
  file.path(repo_root, "packages", "mojor", "build"),
  file.path(repo_root, "build")
)
load_ok <- FALSE
for (p in load_candidates) {
  bridge <- file.path(p, "mojor_bridge.so")
  if (file.exists(bridge)) {
    try({
      mojor_load(p)
      load_ok <- TRUE
    }, silent = TRUE)
  }
  if (load_ok) break
}
if (!load_ok) {
  stop("Mojo backend not loaded. Build packages/mojor/build or build first.")
}

Rgibbs <- function(N, thin) {
  mat <- matrix(0, nrow = N, ncol = 2)
  x <- 0
  y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

cat(sprintf("Gibbs JIT info repro (N=%d, thin=%d)\n", N, thin))

cache_root <- file.path(tempdir(), paste0("gibbs_jit_repro_", Sys.getpid(), "_", as.integer(Sys.time())))
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

old_opts <- mojor_options(jit_disk_cache = TRUE)
on.exit(do.call(mojor_options, old_opts), add = TRUE)

run_timed <- function(fn) {
  value <- NULL
  elapsed <- system.time({
    value <- fn()
  })[["elapsed"]]
  list(value = value, elapsed = elapsed)
}

lazy_cache <- file.path(cache_root, "lazy")
dir.create(lazy_cache, recursive = TRUE, showWarnings = FALSE)
lazy <- mojor_jit(Rgibbs, cache_dir = lazy_cache)

mojor_rng_seed(42L)
lazy_1 <- run_timed(function() lazy(as.integer(N), as.integer(thin)))
mojor_rng_seed(42L)
lazy_2 <- run_timed(function() lazy(as.integer(N), as.integer(thin)))
lazy_info <- mojor_jit_info(lazy)

cat("\n[Lazy JIT]\n")
cat(sprintf("call1_sec=%.3f call2_sec=%.3f\n", lazy_1$elapsed, lazy_2$elapsed))
cat(sprintf("stats: calls_total=%d memory_hits=%d disk_hits=%d compile_misses=%d compiles=%d\n",
  lazy_info$stats$calls_total,
  lazy_info$stats$cache_hits_memory,
  lazy_info$stats$cache_hits_disk,
  lazy_info$stats$cache_misses_compile,
  lazy_info$stats$compile_count
))
cat(sprintf("signature: last_event=%s compiled_via=%s failed_compiles=%d\n",
  lazy_info$signatures[[1]]$last_event,
  if (is.null(lazy_info$signatures[[1]]$compiled_via)) "<null>" else lazy_info$signatures[[1]]$compiled_via,
  lazy_info$signatures[[1]]$failed_compiles
))
cat(sprintf("last_row=%.6f,%.6f\n",
  lazy_2$value[nrow(lazy_2$value), 1],
  lazy_2$value[nrow(lazy_2$value), 2]
))

lazy_disk <- mojor_jit(Rgibbs, cache_dir = lazy_cache)
mojor_rng_seed(42L)
lazy_disk_run <- run_timed(function() lazy_disk(as.integer(N), as.integer(thin)))
lazy_disk_info <- mojor_jit_info(lazy_disk)
cat("\n[Lazy JIT new wrapper same cache_dir]\n")
cat(sprintf("call1_sec=%.3f\n", lazy_disk_run$elapsed))
cat(sprintf("stats: calls_total=%d memory_hits=%d disk_hits=%d compile_misses=%d compiles=%d\n",
  lazy_disk_info$stats$calls_total,
  lazy_disk_info$stats$cache_hits_memory,
  lazy_disk_info$stats$cache_hits_disk,
  lazy_disk_info$stats$cache_misses_compile,
  lazy_disk_info$stats$compile_count
))
cat(sprintf("signature: last_event=%s compiled_via=%s\n",
  lazy_disk_info$signatures[[1]]$last_event,
  if (is.null(lazy_disk_info$signatures[[1]]$compiled_via)) "<null>" else lazy_disk_info$signatures[[1]]$compiled_via
))

eager_cache <- file.path(cache_root, "eager")
dir.create(eager_cache, recursive = TRUE, showWarnings = FALSE)
eager <- mojor_jit(
  Rgibbs,
  cache_dir = eager_cache,
  signatures = list(list(N = "i32", thin = "i32")),
  eager = TRUE
)

mojor_rng_seed(7L)
eager_1 <- run_timed(function() eager(as.integer(N), as.integer(thin)))
eager_info <- mojor_jit_info(eager)

cat("\n[Eager JIT]\n")
cat(sprintf("call1_sec=%.3f\n", eager_1$elapsed))
cat(sprintf("stats: calls_total=%d memory_hits=%d disk_hits=%d compile_misses=%d compiles=%d eager_attempted=%d eager_succeeded=%d\n",
  eager_info$stats$calls_total,
  eager_info$stats$cache_hits_memory,
  eager_info$stats$cache_hits_disk,
  eager_info$stats$cache_misses_compile,
  eager_info$stats$compile_count,
  eager_info$stats$eager_compiles_attempted,
  eager_info$stats$eager_compiles_succeeded
))
cat(sprintf("signature: last_event=%s compiled_via=%s failed_compiles=%d\n",
  eager_info$signatures[[1]]$last_event,
  if (is.null(eager_info$signatures[[1]]$compiled_via)) "<null>" else eager_info$signatures[[1]]$compiled_via,
  eager_info$signatures[[1]]$failed_compiles
))
cat(sprintf("last_row=%.6f,%.6f\n",
  eager_1$value[nrow(eager_1$value), 1],
  eager_1$value[nrow(eager_1$value), 2]
))

cat(sprintf("\ncache_root=%s\n", cache_root))
