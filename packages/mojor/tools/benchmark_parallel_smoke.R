#!/usr/bin/env Rscript

.mojor_parallel_find_mojor <- function() {
  cur <- getwd()
  for (i in 0:8) {
    cand_proto <- file.path(cur, "prototype", "R", "mojor.R")
    if (file.exists(cand_proto)) return(normalizePath(cand_proto))
    cand_pkg <- file.path(cur, "packages", "mojor", "R", "mojor.R")
    if (file.exists(cand_pkg)) return(normalizePath(cand_pkg))
    cand_local <- file.path(cur, "R", "mojor.R")
    if (file.exists(cand_local)) return(normalizePath(cand_local))
    cur <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
  }
  stop("could not locate mojor.R")
}

.mojor_parallel_ensure_loaded <- function() {
  if (!exists("mojor_build", mode = "function")) {
    source(.mojor_parallel_find_mojor())
  }
}

.mojor_parallel_has_mojo <- function() {
  nzchar(Sys.which("mojo"))
}

.mojor_parallel_is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

.mojor_parallel_try_load_backend <- function() {
  mojor_path <- .mojor_parallel_find_mojor()
  root <- dirname(dirname(mojor_path))
  build_dir <- file.path(root, "build")
  bridge <- file.path(build_dir, "mojor_bridge.so")
  if (file.exists(bridge)) {
    Sys.setenv(MOJOR_LIB_PATH = build_dir)
    try(mojor_load(build_dir), silent = TRUE)
  }
  isTRUE(mojor_is_loaded())
}

.mojor_parallel_try_build_backend <- function() {
  if (!.mojor_parallel_has_mojo()) return(FALSE)
  mojor_path <- .mojor_parallel_find_mojor()
  root <- dirname(dirname(mojor_path))
  build_sh <- file.path(root, "build.sh")
  if (!file.exists(build_sh)) {
    build_sh <- file.path(root, "tools", "build_backend.sh")
  }
  if (!file.exists(build_sh)) return(FALSE)
  isTRUE(system2("bash", build_sh, stdout = FALSE, stderr = FALSE) == 0L)
}

.mojor_parallel_ensure_backend <- function() {
  .mojor_parallel_ensure_loaded()
  if (isTRUE(mojor_is_loaded()) || isTRUE(.mojor_parallel_try_load_backend())) {
    return(list(ok = TRUE, reason = NULL))
  }
  if (!.mojor_parallel_has_mojo()) {
    return(list(ok = FALSE, reason = "mojo compiler not available"))
  }
  if (!isTRUE(.mojor_parallel_try_build_backend())) {
    return(list(ok = FALSE, reason = "unable to build Mojo backend"))
  }
  if (!isTRUE(.mojor_parallel_try_load_backend())) {
    return(list(ok = FALSE, reason = "backend build completed but mojor_load failed"))
  }
  list(ok = TRUE, reason = NULL)
}

.mojor_parallel_perf_bench <- function(fun, runs = 3L, warmup = 1L) {
  runs <- as.integer(runs)
  warmup <- as.integer(warmup)
  if (is.na(runs) || runs < 1L) stop("runs must be >= 1")
  if (is.na(warmup) || warmup < 0L) stop("warmup must be >= 0")

  if (warmup > 0L) {
    for (i in seq_len(warmup)) invisible(fun())
  }

  samples <- numeric(runs)
  for (i in seq_len(runs)) {
    gc()
    t0 <- proc.time()[["elapsed"]]
    invisible(fun())
    t1 <- proc.time()[["elapsed"]]
    samples[[i]] <- as.numeric(t1 - t0)
  }

  list(
    samples = samples,
    median_sec = stats::median(samples),
    mean_sec = mean(samples)
  )
}

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

.mojor_parse_cli_bool <- function(value, default = FALSE) {
  if (is.null(value) || !nzchar(value)) return(isTRUE(default))
  tolower(value) %in% c("1", "true", "yes", "y", "on")
}

.mojor_parallel_write_current_csv <- function(path, metrics) {
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(metrics[, c("name", "ms")], path, row.names = FALSE)
  invisible(TRUE)
}

mojor_parallel_benchmark_smoke <- function(
  n = 3000000L,
  runs = 3L,
  warmup = 1L,
  min_speedup = 1.01,
  enforce = TRUE,
  allow_skip = TRUE,
  require_inprocess_linux = TRUE,
  write_current = ""
) {
  n <- as.integer(n)
  if (is.na(n) || n < 1024L) stop("n must be >= 1024")
  if (!is.finite(min_speedup) || min_speedup <= 0) {
    stop("min_speedup must be > 0")
  }
  hard_require_inprocess <- isTRUE(require_inprocess_linux) && isTRUE(.mojor_parallel_is_linux())

  backend <- .mojor_parallel_ensure_backend()
  if (!isTRUE(backend$ok)) {
    if (isTRUE(hard_require_inprocess) && isTRUE(enforce)) {
      stop("parallel benchmark smoke failed: Linux runner requires in-process compiled mode (", backend$reason, ")")
    }
    if (!isTRUE(allow_skip) && isTRUE(enforce)) {
      stop("parallel benchmark smoke skipped: ", backend$reason)
    }
    return(list(status = "skipped", reason = backend$reason, metrics = data.frame()))
  }

  f_seq <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      v <- x[i]
      out[i] <- (((v * 1.000001) + 0.125) * ((v * 0.5) + 1.75)) - 0.375
    }
    out
  }
  f_par <- function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      v <- x[i]
      out[i] <- (((v * 1.000001) + 0.125) * ((v * 0.5) + 1.75)) - 0.375
    }
    out
  }

  built_seq <- mojor_build(
    f_seq,
    x = "f64[]",
    name = "bench_parallel_seq",
    cache = FALSE,
    load = TRUE
  )
  built_par <- mojor_build(
    f_par,
    x = "f64[]",
    name = "bench_parallel_explicit",
    cache = FALSE,
    load = TRUE
  )

  if (!isTRUE(built_par$trans$parallel$uses_parallelize)) {
    stop("parallel benchmark smoke: explicit parallel kernel did not emit parallelize")
  }
  if (!isTRUE(built_par$compiled) || !isTRUE(built_par$trans$parallel$inprocess_enabled)) {
    reason <- "parallel kernel is not running in-process compiled mode"
    if (isTRUE(hard_require_inprocess) && isTRUE(enforce)) {
      stop("parallel benchmark smoke failed: Linux runner requires in-process compiled mode (", reason, ")")
    }
    if (!isTRUE(allow_skip) && isTRUE(enforce)) {
      stop("parallel benchmark smoke skipped: ", reason)
    }
    return(list(status = "skipped", reason = reason, metrics = data.frame()))
  }

  x <- as.double((seq_len(n) %% 4096L) / 4096)
  expected <- built_seq$func(x)
  observed <- built_par$func(x)
  if (!isTRUE(all.equal(observed, expected, tolerance = 1e-9))) {
    stop("parallel benchmark smoke: compiled sequential and parallel outputs differ")
  }

  seq_bench <- .mojor_parallel_perf_bench(function() built_seq$func(x), runs = runs, warmup = warmup)
  par_bench <- .mojor_parallel_perf_bench(function() built_par$func(x), runs = runs, warmup = warmup)

  seq_ms <- as.numeric(seq_bench$median_sec) * 1000
  par_ms <- as.numeric(par_bench$median_sec) * 1000
  speedup <- seq_ms / max(par_ms, .Machine$double.eps)

  metrics <- data.frame(
    name = c("mojor_seq_ms", "mojor_parallel_ms", "mojor_parallel_speedup"),
    ms = c(seq_ms, par_ms, speedup),
    stringsAsFactors = FALSE
  )
  .mojor_parallel_write_current_csv(write_current, metrics)

  ok <- is.finite(speedup) && speedup >= min_speedup
  if (isTRUE(enforce) && !isTRUE(ok)) {
    stop(
      "parallel benchmark smoke failed: speedup ",
      sprintf("%.4f", speedup),
      " < min_speedup ",
      sprintf("%.4f", min_speedup)
    )
  }

  list(
    status = "ok",
    reason = NULL,
    speedup = speedup,
    min_speedup = min_speedup,
    metrics = metrics
  )
}

if (identical(environment(), globalenv()) && !length(grep("^source\\(", sys.calls()))) {
  args <- commandArgs(trailingOnly = TRUE)
  runs <- as.integer(.mojor_parse_cli_arg(args, "runs", "3"))
  warmup <- as.integer(.mojor_parse_cli_arg(args, "warmup", "1"))
  n <- as.integer(.mojor_parse_cli_arg(args, "n", "3000000"))
  min_speedup <- as.numeric(.mojor_parse_cli_arg(args, "min-speedup", "1.01"))
  enforce <- .mojor_parse_cli_bool(.mojor_parse_cli_arg(args, "enforce", "1"), default = TRUE)
  allow_skip <- .mojor_parse_cli_bool(.mojor_parse_cli_arg(args, "allow-skip", "1"), default = TRUE)
  require_inprocess_linux <- .mojor_parse_cli_bool(
    .mojor_parse_cli_arg(args, "require-inprocess-linux", "1"),
    default = TRUE
  )
  write_current <- .mojor_parse_cli_arg(args, "write-current", "")

  res <- mojor_parallel_benchmark_smoke(
    n = n,
    runs = runs,
    warmup = warmup,
    min_speedup = min_speedup,
    enforce = enforce,
    allow_skip = allow_skip,
    require_inprocess_linux = require_inprocess_linux,
    write_current = write_current
  )

  if (identical(res$status, "skipped")) {
    cat("parallel benchmark smoke: skipped (", res$reason, ")\n", sep = "")
    quit(status = 0)
  }

  print(res$metrics, row.names = FALSE)
  cat(
    "parallel benchmark smoke: speedup=",
    sprintf("%.4f", res$speedup),
    " (min=",
    sprintf("%.4f", res$min_speedup),
    ")\n",
    sep = ""
  )
}
