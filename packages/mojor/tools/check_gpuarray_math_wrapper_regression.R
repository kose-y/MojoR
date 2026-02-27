#!/usr/bin/env Rscript

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0L) return(default)
  sub(pat, "", hit[[1L]])
}

.mojor_parse_bool_arg <- function(args, key, default = FALSE) {
  raw <- .mojor_parse_cli_arg(args, key, "")
  if (!nzchar(raw)) {
    return(isTRUE(default))
  }
  val <- tolower(trimws(raw))
  if (val %in% c("1", "true", "yes", "y", "on")) return(TRUE)
  if (val %in% c("0", "false", "no", "n", "off")) return(FALSE)
  isTRUE(default)
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

.mojor_gpuarray_try_load_backend <- function() {
  mojor_path <- .mojor_gpuarray_find_mojor()
  pkg_root <- dirname(dirname(mojor_path))
  repo_root <- pkg_root
  if (identical(basename(pkg_root), "mojor") &&
      identical(basename(dirname(pkg_root)), "packages")) {
    repo_root <- dirname(dirname(pkg_root))
  }
  build_dirs <- unique(c(
    file.path(pkg_root, "build"),
    file.path(repo_root, "build")
  ))
  for (build_dir in build_dirs) {
    bridge <- file.path(build_dir, "mojor_bridge.so")
    if (!file.exists(bridge)) {
      next
    }
    Sys.setenv(MOJOR_LIB_PATH = build_dir)
    try(mojor_load(build_dir), silent = TRUE)
    if (isTRUE(mojor_is_loaded())) {
      break
    }
  }
  invisible(isTRUE(mojor_is_loaded()))
}

.mojor_gpuarray_expect_lane <- function(op, out, allowed) {
  route <- attr(out, "gpu_fallback", exact = TRUE)
  reason <- attr(out, "gpu_fallback_reason_code", exact = TRUE)
  route_norm <- if (is.null(route)) "<gpu>" else as.character(route)
  reason_norm <- if (is.null(reason)) "<none>" else as.character(reason)
  allowed_rows <- allowed[allowed$op == op, , drop = FALSE]
  if (nrow(allowed_rows) == 0L) {
    stop("missing allowed-lane contract for op: ", op)
  }
  matches <- allowed_rows$route == route_norm & allowed_rows$reason == reason_norm
  if (!any(matches)) {
    stop(
      sprintf(
        "route contract violation for %s: route=%s reason=%s",
        op,
        route_norm,
        reason_norm
      )
    )
  }
  list(route = route_norm, reason = reason_norm)
}

args <- commandArgs(trailingOnly = TRUE)
iters <- suppressWarnings(as.integer(.mojor_parse_cli_arg(args, "iters", "16")))
if (is.na(iters) || iters < 1L) {
  stop("iters must be >= 1")
}
quiet_fallback_warnings <- .mojor_parse_bool_arg(args, "quiet-fallback-warnings", TRUE)
progress_every <- suppressWarnings(as.integer(.mojor_parse_cli_arg(args, "progress-every", "0")))
if (is.na(progress_every) || progress_every < 0L) {
  progress_every <- 0L
}

source(.mojor_gpuarray_find_mojor())
if (!isTRUE(mojor_is_loaded())) {
  .mojor_gpuarray_try_load_backend()
}
if (!isTRUE(mojor_is_loaded()) || !isTRUE(mojor_has_gpu())) {
  cat("GPU unavailable; skipping GPUArray math wrapper regression check.\n")
  quit(save = "no", status = 0L)
}

x_pos <- matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L)
x_mix <- matrix(c(-2.2, -1.1, 0.0, 1.1, 2.2, 3.3), nrow = 2L, ncol = 3L)
y <- matrix(c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), nrow = 2L, ncol = 3L)
x_na <- matrix(c(NA_real_, 1, NA_real_, 3), nrow = 2L, ncol = 2L)
y_na <- matrix(c(2, NA_real_, 4, NA_real_), nrow = 2L, ncol = 2L)
y_shape <- matrix(c(0.5, 1.5, 2.5), nrow = 1L, ncol = 3L)

gx <- GPUArray(x_pos, dtype = "f32")
gx_mix <- GPUArray(x_mix, dtype = "f32")
gy <- GPUArray(y, dtype = "f32")
gx_na <- GPUArray(x_na, dtype = "f32")
gy_na <- GPUArray(y_na, dtype = "f32")
gy_shape <- GPUArray(y_shape, dtype = "f32")

on.exit({
  try(mojor_gpu_array_free(gy_shape), silent = TRUE)
  try(mojor_gpu_array_free(gy_na), silent = TRUE)
  try(mojor_gpu_array_free(gx_na), silent = TRUE)
  try(mojor_gpu_array_free(gy), silent = TRUE)
  try(mojor_gpu_array_free(gx_mix), silent = TRUE)
  try(mojor_gpu_array_free(gx), silent = TRUE)
}, add = TRUE)

allowed <- data.frame(
  op = c(
    "log", "log", "log", "log", "log", "log",
    "log10", "log10", "log10", "log10", "log10", "log10",
    "log2", "log2", "log2", "log2", "log2", "log2",
    "log1p", "log1p", "log1p", "log1p", "log1p", "log1p",
    "round_digits2",
    "gpu_atan2", "gpu_atan2",
    "gpu_minimum_na_rm",
    "gpu_minimum_shape",
    "gpu_sum", "gpu_sum"
  ),
  route = c(
    "<gpu>", "cpu_arith", "cpu_arith", "cpu_kernel_host", "cpu_kernel_host", "cpu_kernel_host",
    "<gpu>", "cpu_arith", "cpu_arith", "cpu_kernel_host", "cpu_kernel_host", "cpu_kernel_host",
    "<gpu>", "cpu_arith", "cpu_arith", "cpu_kernel_host", "cpu_kernel_host", "cpu_kernel_host",
    "<gpu>", "cpu_arith", "cpu_arith", "cpu_kernel_host", "cpu_kernel_host", "cpu_kernel_host",
    "cpu_arith",
    "<gpu>", "cpu_arith",
    "cpu_arith",
    "cpu_arith",
    "gpu_reduce", "cpu_reduce"
  ),
  reason = c(
    "<none>", "kernel_dispatch_failed", "raw_kernel_unavailable", "<none>", "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable",
    "<none>", "kernel_dispatch_failed", "raw_kernel_unavailable", "<none>", "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable",
    "<none>", "kernel_dispatch_failed", "raw_kernel_unavailable", "<none>", "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable",
    "<none>", "kernel_dispatch_failed", "raw_kernel_unavailable", "<none>", "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable",
    "round_digits_host_fallback",
    "<none>", "kernel_dispatch_failed",
    "na_rm_host_parity",
    "shape_mismatch_host_parity",
    "<none>", "<none>"
  ),
  stringsAsFactors = FALSE
)

with_fallback_muffle <- function(expr) {
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (isTRUE(quiet_fallback_warnings) &&
          grepl("^mojor gpu fallback to CPU via ", msg, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

gc()
base_live <- mojor_gpu_buf_f32_live_count()
obs <- vector("list", length = 0L)

for (iter in seq_len(iters)) {
  outs <- list(
    log = with_fallback_muffle(log(gx)),
    log10 = with_fallback_muffle(log10(gx)),
    log2 = with_fallback_muffle(log2(gx)),
    log1p = with_fallback_muffle(log1p(gx)),
    round_digits2 = with_fallback_muffle(round(gx_mix, digits = 2L)),
    gpu_atan2 = with_fallback_muffle(gpu_atan2(gx, gy)),
    gpu_minimum_na_rm = with_fallback_muffle(gpu_minimum(gx_na, gy_na, na.rm = TRUE)),
    gpu_minimum_shape = with_fallback_muffle(gpu_minimum(gx, gy_shape)),
    gpu_sum = with_fallback_muffle(gpu_sum(gx))
  )

  for (op in names(outs)) {
    out <- outs[[op]]
    lane <- .mojor_gpuarray_expect_lane(op, out, allowed)
    obs[[length(obs) + 1L]] <- data.frame(
      iter = as.integer(iter),
      op = op,
      route = lane$route,
      reason = lane$reason,
      stringsAsFactors = FALSE
    )
  }

  suppressWarnings(invisible(lapply(outs, function(out) {
    try(mojor_gpu_array_free(out), silent = TRUE)
    NULL
  })))

  gc()
  live_now <- mojor_gpu_buf_f32_live_count()
  if (!identical(live_now, base_live)) {
    stop(
      sprintf(
        "math wrapper regression leak at iter %d: base_live=%d current_live=%d",
        iter,
        as.integer(base_live),
        as.integer(live_now)
      )
    )
  }
  if (progress_every > 0L && (iter %% progress_every) == 0L) {
    cat(sprintf("[gpu-math-wrapper-regression] completed %d/%d iterations\n", iter, iters))
    flush.console()
  }
}

obs_df <- do.call(rbind, obs)
summary_df <- stats::aggregate(
  rep.int(1L, nrow(obs_df)),
  by = list(op = obs_df$op, route = obs_df$route, reason = obs_df$reason),
  FUN = sum
)
names(summary_df)[[4L]] <- "count"
summary_df <- summary_df[order(summary_df$op, summary_df$route, summary_df$reason), , drop = FALSE]

cat("GPUArray math wrapper regression summary:\n")
print(summary_df)
cat(sprintf("PASS: %d iterations, no temp-buffer growth\n", as.integer(iters)))
