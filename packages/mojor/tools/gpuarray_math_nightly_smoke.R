#!/usr/bin/env Rscript

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

.mojor_gpuarray_find_mojor <- function() {
  cur <- getwd()
  for (i in 0:8) {
    cand_pkg <- file.path(cur, "packages", "mojor", "R", "mojor.R")
    if (file.exists(cand_pkg)) return(normalizePath(cand_pkg))
    cand_local <- file.path(cur, "R", "mojor.R")
    if (file.exists(cand_local)) return(normalizePath(cand_local))
    cur <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
  }
  stop("could not locate mojor.R")
}

.mojor_gpuarray_repo_root <- function() {
  mojor_path <- .mojor_gpuarray_find_mojor()
  pkg_root <- dirname(dirname(mojor_path))
  if (identical(basename(pkg_root), "mojor") &&
      identical(basename(dirname(pkg_root)), "packages")) {
    return(dirname(dirname(pkg_root)))
  }
  pkg_root
}

.mojor_gpuarray_nightly_run_leak_loop <- function(iters = 24L) {
  x <- matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L)
  y <- matrix(c(-1.5, 0.5, 0.2, 2.0, 1.5, 3.5), nrow = 2L, ncol = 3L)
  xi <- matrix(as.integer(1:6), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")
  gi <- GPUArray(xi, dtype = "i32")

  on.exit({
    try(mojor_gpu_array_free(gi), silent = TRUE)
    try(mojor_gpu_array_free(gy), silent = TRUE)
    try(mojor_gpu_array_free(gx), silent = TRUE)
  }, add = TRUE)

  gc()
  base_live <- mojor_gpu_buf_f32_live_count()
  iters <- as.integer(iters)
  if (is.na(iters) || iters < 1L) {
    stop("leak-iters must be >= 1")
  }

  for (iter in seq_len(iters)) {
    out_log1p <- suppressWarnings(log1p(gx))
    out_expm1 <- suppressWarnings(expm1(gx))
    out_round <- suppressWarnings(round(gx, digits = 2L))
    out_pmin <- suppressWarnings(gpu_minimum(gx, gy, -0.25))
    out_pmax <- suppressWarnings(gpu_maximum(gx, gy, 0.75))
    out_atan <- suppressWarnings(gpu_atan2(gx, gy))
    out_exp_i32 <- suppressWarnings(exp(gi))
    idx_min <- gpu_which_min(gx)
    idx_max <- gpu_which_max(gy)

    if (!(is.numeric(idx_min) || is.integer(idx_min))) {
      stop("gpu_which_min returned non-numeric index")
    }
    if (!(is.numeric(idx_max) || is.integer(idx_max))) {
      stop("gpu_which_max returned non-numeric index")
    }

    suppressWarnings(
      invisible(
        lapply(
          list(out_exp_i32, out_atan, out_pmax, out_pmin, out_round, out_expm1, out_log1p),
          function(buf) {
            try(mojor_gpu_array_free(buf), silent = TRUE)
            NULL
          }
        )
      )
    )
    gc()
    live_now <- mojor_gpu_buf_f32_live_count()
    if (!identical(live_now, base_live)) {
      stop(
        sprintf(
          "math wrapper leak loop diverged at iter %d: base_live=%d current_live=%d",
          iter, as.integer(base_live), as.integer(live_now)
        )
      )
    }
  }
  cat(sprintf("Leak loop OK (%d iterations)\n", as.integer(iters)))
}

args <- commandArgs(trailingOnly = TRUE)
repeats <- as.integer(.mojor_parse_cli_arg(args, "repeats", "8"))
profile <- .mojor_parse_cli_arg(args, "profile", "closeout")
leak_iters <- as.integer(.mojor_parse_cli_arg(args, "leak-iters", "24"))
quiet_fallback_warnings <- tolower(.mojor_parse_cli_arg(args, "quiet-fallback-warnings", "true")) %in% c("1", "true", "yes", "y", "on")
progress_every <- as.integer(.mojor_parse_cli_arg(args, "progress-every", "0"))
repo_root <- .mojor_gpuarray_repo_root()
baseline_path <- .mojor_parse_cli_arg(
  args,
  "baseline",
  file.path(repo_root, "docs", "BASELINES", "GPUARRAY_MATH_ROUTE_BASELINE.csv")
)

source(.mojor_gpuarray_find_mojor())
audit_path <- file.path(repo_root, "packages", "mojor", "tools", "audit_gpuarray_math_routes.R")
if (!file.exists(audit_path)) {
  stop("could not locate audit_gpuarray_math_routes.R")
}
sys.source(audit_path, envir = globalenv())

if (!isTRUE(mojor_is_loaded())) {
  .mojor_gpuarray_try_load_backend()
}
if (!isTRUE(mojor_is_loaded()) || !isTRUE(mojor_has_gpu())) {
  cat("GPU unavailable on runner; skipping GPUArray math nightly smoke.\n")
  quit(save = "no", status = 0L)
}
if (!file.exists(baseline_path)) {
  stop("math route baseline CSV not found: ", baseline_path)
}

.mojor_gpuarray_nightly_run_leak_loop(iters = leak_iters)
gc()
if (!identical(mojor_gpu_buf_f32_live_count(), 0L)) {
  stop("leak loop did not return live f32 buffer count to zero")
}

thresholds <- mojor_gpuarray_math_route_thresholds(profile = profile)
current <- mojor_gpuarray_audit_math_routes(
  repeats = repeats,
  quiet_fallback_warnings = quiet_fallback_warnings,
  progress_every = progress_every
)
print(current)
baseline <- mojor_gpuarray_load_math_route_baseline(baseline_path)
gate <- mojor_gpuarray_math_route_gate(current, baseline, thresholds = thresholds)
if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
  cat("\nMath route gate profile:", profile, "\n")
  cat("Math route gate policy:", gate$note, "\n")
  cat("\nMath route gate:\n")
  print(gate$checks)
}
if (!isTRUE(gate$ok)) {
  stop("GPUArray math route gate failed")
}
cat("GPUArray math nightly smoke: PASS\n")
