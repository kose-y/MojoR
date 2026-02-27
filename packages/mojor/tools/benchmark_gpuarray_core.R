#!/usr/bin/env Rscript

.mojor_gpuarray_find_mojor <- function() {
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

.mojor_gpuarray_ensure_loaded <- function() {
  if (!exists("GPUArray", mode = "function")) {
    source(.mojor_gpuarray_find_mojor())
  }
}

.mojor_gpuarray_try_load_backend <- function() {
  mojor_path <- .mojor_gpuarray_find_mojor()
  root <- dirname(dirname(mojor_path))
  build_dir <- file.path(root, "build")
  bridge <- file.path(build_dir, "mojor_bridge.so")
  if (file.exists(bridge)) {
    Sys.setenv(MOJOR_LIB_PATH = build_dir)
    try(mojor_load(build_dir), silent = TRUE)
  }
  invisible(isTRUE(mojor_is_loaded()))
}

.mojor_gpuarray_perf_bench <- function(fun, runs = 3L, warmup = 1L) {
  runs <- as.integer(runs)
  warmup <- as.integer(warmup)
  if (runs < 1L) stop("runs must be >= 1")
  if (warmup < 0L) stop("warmup must be >= 0")

  if (warmup > 0L) {
    for (i in seq_len(warmup)) invisible(fun())
  }

  samples <- numeric(runs)
  routes <- rep.int(NA_character_, runs)
  for (i in seq_len(runs)) {
    t0 <- proc.time()[["elapsed"]]
    route <- fun()
    t1 <- proc.time()[["elapsed"]]
    samples[[i]] <- as.numeric(t1 - t0)
    if (is.character(route) && length(route) == 1L && !is.na(route) && nzchar(route)) {
      routes[[i]] <- route
    }
  }

  list(
    samples = samples,
    routes = routes,
    median_sec = stats::median(samples),
    mean_sec = mean(samples)
  )
}

.mojor_gpuarray_dominant_route <- function(routes) {
  routes <- as.character(routes)
  routes <- routes[!is.na(routes) & nzchar(routes)]
  if (length(routes) == 0L) {
    return("unknown")
  }
  tab <- sort(table(routes), decreasing = TRUE)
  as.character(names(tab)[[1L]])
}

mojor_gpuarray_benchmark_core <- function(
  runs = 3L,
  warmup = 1L,
  n = 1048576L,
  m = 512L,
  k = 512L,
  p = 512L,
  include_indexing = FALSE
) {
  .mojor_gpuarray_ensure_loaded()
  if (!isTRUE(mojor_is_loaded())) {
    .mojor_gpuarray_try_load_backend()
  }
  if (!isTRUE(mojor_is_loaded())) {
    stop("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    stop("GPU not available")
  }

  n <- as.integer(n)
  m <- as.integer(m)
  k <- as.integer(k)
  p <- as.integer(p)

  vec_host <- as.numeric(seq_len(n) %% 97L) / 97
  cast_src <- GPUArray(vec_host, dtype = "f32")

  bcast_src_host <- matrix(as.numeric(seq_len(m)) / max(1, m), nrow = m, ncol = 1L)
  bcast_src <- GPUArray(bcast_src_host, dtype = "f32")

  reduce_src_host <- matrix(as.numeric(seq_len(m * k)) / max(1, m * k), nrow = m, ncol = k)
  reduce_src <- GPUArray(reduce_src_host, dtype = "f32")

  mat_a_host <- matrix(as.numeric(seq_len(m * k)) / max(1, m * k), nrow = m, ncol = k)
  mat_b_host <- matrix(as.numeric(seq_len(k * p)) / max(1, k * p), nrow = k, ncol = p)
  mat_a <- GPUArray(mat_a_host, dtype = "f32")
  mat_b <- GPUArray(mat_b_host, dtype = "f32")
  idx_src_host <- matrix(as.numeric(seq_len(m * k)) / max(1, m * k), nrow = m, ncol = k)
  idx_src <- GPUArray(idx_src_host, dtype = "f32")

  on.exit({
    try(mojor_gpu_array_free(mat_b), silent = TRUE)
    try(mojor_gpu_array_free(mat_a), silent = TRUE)
    try(mojor_gpu_array_free(idx_src), silent = TRUE)
    try(mojor_gpu_array_free(reduce_src), silent = TRUE)
    try(mojor_gpu_array_free(bcast_src), silent = TRUE)
    try(mojor_gpu_array_free(cast_src), silent = TRUE)
  }, add = TRUE)

  tasks <- list(
    gpu_cast = function() {
      out <- gpu_cast(cast_src, "f64")
      route <- attr(out, "gpu_fallback")
      invisible(mojor_gpu_array_read(out))
      mojor_gpu_array_free(out)
      route
    },
    gpu_broadcast = function() {
      out <- gpu_broadcast(bcast_src, c(m, k))
      route <- attr(out, "gpu_fallback")
      invisible(mojor_gpu_array_read(out))
      mojor_gpu_array_free(out)
      route
    },
    gpu_reduce = function() {
      out <- gpu_reduce(reduce_src, "sum", dims = 1L, keepdims = FALSE)
      route <- attr(out, "gpu_fallback")
      invisible(mojor_gpu_array_read(out))
      mojor_gpu_array_free(out)
      route
    },
    gpu_matmul = function() {
      out <- gpu_matmul(mat_a, mat_b)
      route <- attr(out, "gpu_fallback")
      invisible(mojor_gpu_array_read(out))
      mojor_gpu_array_free(out)
      route
    }
  )
  if (isTRUE(include_indexing)) {
    tasks$gpu_index_gather <- function() {
      out <- idx_src[c(1L, 3L, 5L), c(2L, 4L, 6L)]
      route <- attr(out, "gpu_fallback")
      invisible(mojor_gpu_array_read(out))
      mojor_gpu_array_free(out)
      route
    }
    tasks$gpu_index_assign <- function() {
      idx_src[c(1L, 3L, 5L), c(2L, 4L, 6L)] <- matrix(
        as.numeric(seq_len(9L)) / 9,
        nrow = 3L,
        ncol = 3L
      )
      route <- attr(idx_src, "gpu_fallback")
      invisible(mojor_gpu_array_read(idx_src))
      route
    }
  }

  rows <- vector("list", length(tasks))
  names(rows) <- names(tasks)
  sample_map <- list()
  route_map <- list()
  idx <- 1L
  for (nm in names(tasks)) {
    res <- .mojor_gpuarray_perf_bench(tasks[[nm]], runs = runs, warmup = warmup)
    sample_map[[nm]] <- res$samples
    route_map[[nm]] <- res$routes
    rows[[idx]] <- data.frame(
      metric = nm,
      median_sec = as.numeric(res$median_sec),
      mean_sec = as.numeric(res$mean_sec),
      route = .mojor_gpuarray_dominant_route(res$routes),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  metrics <- do.call(rbind, rows)
  rownames(metrics) <- NULL
  list(
    metrics = metrics,
    samples = sample_map,
    routes = route_map,
    runs = as.integer(runs),
    warmup = as.integer(warmup)
  )
}

mojor_gpuarray_load_perf_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("metric", "median_sec")
  if (!all(req %in% names(dat))) {
    stop("baseline CSV must contain columns: metric, median_sec")
  }
  dat
}

mojor_gpuarray_perf_gate <- function(
  current_metrics,
  baseline_metrics,
  thresholds = c(
    gpu_cast = 0.75,
    gpu_broadcast = 0.50,
    gpu_reduce = 0.75,
    gpu_matmul = 0.60
  ),
  baseline_floor_sec = 1e-4
) {
  if (is.null(baseline_metrics) || nrow(baseline_metrics) == 0) {
    return(list(ok = TRUE, checks = data.frame(), note = "no baseline provided"))
  }
  if (is.null(current_metrics) || nrow(current_metrics) == 0) {
    return(list(ok = FALSE, checks = data.frame(), note = "no current metrics"))
  }
  if (!is.finite(baseline_floor_sec) || baseline_floor_sec <= 0) {
    stop("baseline_floor_sec must be > 0")
  }

  bmed <- stats::setNames(as.numeric(baseline_metrics$median_sec), as.character(baseline_metrics$metric))
  cmed <- stats::setNames(as.numeric(current_metrics$median_sec), as.character(current_metrics$metric))

  rows <- list()
  ok <- TRUE
  for (metric in names(thresholds)) {
    if (!metric %in% names(bmed) || !metric %in% names(cmed)) next
    base <- bmed[[metric]]
    cur <- cmed[[metric]]
    if (!is.finite(base) || base < 0 || !is.finite(cur) || cur < 0) {
      pass <- FALSE
      delta <- NA_real_
      compare_base <- NA_real_
      mode <- "invalid"
    } else {
      compare_base <- max(base, baseline_floor_sec)
      mode <- if (base < baseline_floor_sec) "floor" else "relative"
      delta <- (cur - compare_base) / compare_base
      pass <- (delta <= thresholds[[metric]])
    }
    ok <- ok && isTRUE(pass)
    rows[[length(rows) + 1L]] <- data.frame(
      metric = metric,
      baseline_sec = base,
      compare_baseline_sec = compare_base,
      current_sec = cur,
      delta = delta,
      threshold = thresholds[[metric]],
      comparison_mode = mode,
      pass = pass,
      stringsAsFactors = FALSE
    )
  }

  checks <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  list(
    ok = isTRUE(ok),
    checks = checks,
    note = sprintf("baseline floor seconds: %.6f", baseline_floor_sec)
  )
}

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

if (identical(environment(), globalenv()) && !length(grep("^source\\(", sys.calls()))) {
  args <- commandArgs(trailingOnly = TRUE)
  runs <- as.integer(.mojor_parse_cli_arg(args, "runs", "3"))
  warmup <- as.integer(.mojor_parse_cli_arg(args, "warmup", "1"))
  n <- as.integer(.mojor_parse_cli_arg(args, "n", "1048576"))
  m <- as.integer(.mojor_parse_cli_arg(args, "m", "512"))
  k <- as.integer(.mojor_parse_cli_arg(args, "k", "512"))
  p <- as.integer(.mojor_parse_cli_arg(args, "p", "512"))
  with_indexing <- identical(.mojor_parse_cli_arg(args, "with-indexing", "0"), "1")
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")

  bench <- mojor_gpuarray_benchmark_core(
    runs = runs,
    warmup = warmup,
    n = n,
    m = m,
    k = k,
    p = p,
    include_indexing = with_indexing
  )
  print(bench$metrics, row.names = FALSE)

  baseline <- mojor_gpuarray_load_perf_baseline(baseline_path)
  gate <- mojor_gpuarray_perf_gate(bench$metrics, baseline)
  if (nrow(gate$checks) > 0) {
    cat("\nPerf gate:\n")
    print(gate$checks, row.names = FALSE)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray perf benchmark gate failed")
  }

  if (nzchar(write_baseline)) {
    utils::write.csv(bench$metrics[, c("metric", "median_sec", "route")], write_baseline, row.names = FALSE)
    cat("\nWrote baseline: ", write_baseline, "\n", sep = "")
  }
}
