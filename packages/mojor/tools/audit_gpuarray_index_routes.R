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

.mojor_gpuarray_index_routes_df <- function(route_vec) {
  route_vec <- as.character(route_vec)
  route_vec <- route_vec[!is.na(route_vec) & nzchar(route_vec)]
  all_routes <- c(
    "gpu_gather", "cpu_gather",
    "gpu_scatter", "cpu_scatter",
    "gpu_slice", "cpu_slice",
    "cpu_slice_assign"
  )
  tab <- table(factor(route_vec, levels = all_routes))
  out <- data.frame(
    route = all_routes,
    count = as.integer(tab),
    stringsAsFactors = FALSE
  )
  out
}

mojor_gpuarray_audit_index_routes <- function(repeats = 8L) {
  .mojor_gpuarray_ensure_loaded()
  if (!isTRUE(mojor_is_loaded())) {
    .mojor_gpuarray_try_load_backend()
  }
  if (!isTRUE(mojor_is_loaded())) stop("Mojo backend not loaded")
  if (!mojor_has_gpu()) stop("GPU not available")

  repeats <- as.integer(repeats)
  if (is.na(repeats) || repeats < 1L) {
    stop("repeats must be >= 1")
  }

  routes <- character()
  rec_route <- function(route) {
    route <- as.character(route)
    if (length(route) == 1L && !is.na(route) && nzchar(route)) {
      routes <<- c(routes, route)
    }
    invisible(NULL)
  }

  v <- GPUArray(as.numeric(seq_len(64)), dtype = "f32")
  m <- GPUArray(matrix(as.numeric(seq_len(48)), nrow = 6L, ncol = 8L), dtype = "f32")
  a3 <- GPUArray(array(as.numeric(seq_len(60)), dim = c(3L, 4L, 5L)), dtype = "f32")
  a4 <- GPUArray(array(as.numeric(seq_len(120)), dim = c(2L, 3L, 4L, 5L)), dtype = "f32")
  vi <- GPUArray(as.integer(seq_len(64)), dtype = "i32")
  mi <- GPUArray(matrix(as.integer(seq_len(48)), nrow = 6L, ncol = 8L), dtype = "i32")
  i32_scatter_cap <- isTRUE(.mojor_gpu_i32_scatter_capable(api = "metal"))
  on.exit({
    try(mojor_gpu_array_free(mi), silent = TRUE)
    try(mojor_gpu_array_free(vi), silent = TRUE)
    try(mojor_gpu_array_free(a4), silent = TRUE)
    try(mojor_gpu_array_free(a3), silent = TRUE)
    try(mojor_gpu_array_free(m), silent = TRUE)
    try(mojor_gpu_array_free(v), silent = TRUE)
  }, add = TRUE)

  for (iter in seq_len(repeats)) {
    # Gather paths: rank-1 logical/integer, rank-2 mixed, rank-3 mixed.
    g1 <- v[c(2L, 4L, 7L, 11L, 13L)]
    rec_route(attr(g1, "gpu_fallback"))
    mojor_gpu_array_free(g1)

    mask <- rep_len(c(TRUE, FALSE, FALSE, TRUE), 64L)
    g2 <- v[mask]
    rec_route(attr(g2, "gpu_fallback"))
    mojor_gpu_array_free(g2)

    g3 <- m[c(1L, 3L, 6L), c(2L, 4L, 8L)]
    rec_route(attr(g3, "gpu_fallback"))
    mojor_gpu_array_free(g3)

    g4 <- a3[c(1L, 3L), c(2L, 4L), c(1L, 5L)]
    rec_route(attr(g4, "gpu_fallback"))
    mojor_gpu_array_free(g4)

    g5 <- m[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), c(2L, 4L, 8L)]
    rec_route(attr(g5, "gpu_fallback"))
    mojor_gpu_array_free(g5)

    g6 <- a3[c(TRUE, FALSE, TRUE), c(2L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)]
    rec_route(attr(g6, "gpu_fallback"))
    mojor_gpu_array_free(g6)

    g7 <- a4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)]
    rec_route(attr(g7, "gpu_fallback"))
    mojor_gpu_array_free(g7)

    # Scatter paths: rank-1/2/3 non-contiguous writes.
    v[c(1L, 5L, 9L, 15L)] <- as.numeric(iter + c(10, 20, 30, 40))
    rec_route(attr(v, "gpu_fallback"))

    m[c(2L, 5L), c(3L, 7L)] <- matrix(as.numeric(iter + c(100, 200, 300, 400)), nrow = 2L, ncol = 2L)
    rec_route(attr(m, "gpu_fallback"))

    a3[c(1L, 3L), c(1L, 4L), c(2L, 5L)] <- array(
      as.numeric(iter + seq_len(8L)),
      dim = c(2L, 2L, 2L)
    )
    rec_route(attr(a3, "gpu_fallback"))

    m[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), c(1L, 5L)] <- matrix(
      as.numeric(iter + seq_len(6L)),
      nrow = 3L,
      ncol = 2L
    )
    rec_route(attr(m, "gpu_fallback"))

    a3[c(TRUE, FALSE, TRUE), c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- array(
      as.numeric(iter + seq_len(12L)),
      dim = c(2L, 2L, 3L)
    )
    rec_route(attr(a3, "gpu_fallback"))

    a4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)] <- array(
      as.numeric(iter + seq_len(8L)),
      dim = c(2L, 2L, 2L)
    )
    rec_route(attr(a4, "gpu_fallback"))

    # Duplicate-index scatter (last-write-wins) via deduped scatter-linearized path.
    v[c(2L, 5L, 2L, 9L, 5L)] <- as.numeric(iter + c(10, 20, 30, 40, 50))
    rec_route(attr(v, "gpu_fallback"))

    m[c(2L, 2L, 5L), c(3L, 3L, 7L)] <- matrix(
      as.numeric(iter + seq_len(9L)),
      nrow = 3L,
      ncol = 3L
    )
    rec_route(attr(m, "gpu_fallback"))

    if (isTRUE(i32_scatter_cap)) {
      vi[c(1L, 5L, 9L, 15L)] <- as.integer(iter + c(10L, 20L, 30L, 40L))
      rec_route(attr(vi, "gpu_fallback"))

      mi[c(2L, 5L), c(3L, 7L)] <- matrix(as.integer(iter + c(100L, 200L, 300L, 400L)), nrow = 2L, ncol = 2L)
      rec_route(attr(mi, "gpu_fallback"))
    }

    # Slice/slice-assign paths: contiguous vector/matrix write + contiguous read.
    v[10:14] <- as.numeric(iter + c(1, 2, 3, 4, 5))
    rec_route(attr(v, "gpu_fallback"))

    m[1:3, 4:6] <- matrix(as.numeric(iter + seq_len(9L)), nrow = 3L, ncol = 3L)
    rec_route(attr(m, "gpu_fallback"))

    vi[10:14] <- as.integer(iter + c(1L, 2L, 3L, 4L, 5L))
    rec_route(attr(vi, "gpu_fallback"))

    mi[1:3, 4:6] <- matrix(as.integer(iter + seq_len(9L)), nrow = 3L, ncol = 3L)
    rec_route(attr(mi, "gpu_fallback"))

    s1 <- v[20:25]
    rec_route(attr(s1, "gpu_fallback"))
    mojor_gpu_array_free(s1)

    s2 <- m[2:5, 2:6]
    rec_route(attr(s2, "gpu_fallback"))
    mojor_gpu_array_free(s2)
  }

  .mojor_gpuarray_index_routes_df(routes)
}

mojor_gpuarray_load_index_route_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "count")
  if (!all(req %in% names(dat))) {
    stop("index route baseline CSV must contain columns: route, count")
  }
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  dat
}

mojor_gpuarray_index_route_gate <- function(
  current_routes,
  baseline_routes,
  thresholds = c(cpu_gather = 0.75, cpu_scatter = 0.85, cpu_slice = 1.00, cpu_slice_assign = 0.50)
) {
  if (is.null(baseline_routes) || nrow(baseline_routes) == 0) {
    return(list(ok = TRUE, checks = data.frame(), note = "no baseline provided"))
  }
  if (is.null(current_routes) || nrow(current_routes) == 0) {
    return(list(ok = FALSE, checks = data.frame(), note = "no current routes"))
  }

  bmap <- stats::setNames(as.integer(baseline_routes$count), as.character(baseline_routes$route))
  cmap <- stats::setNames(as.integer(current_routes$count), as.character(current_routes$route))
  rows <- list()
  ok <- TRUE
  for (route in names(thresholds)) {
    base <- if (route %in% names(bmap)) as.integer(bmap[[route]]) else 0L
    cur <- if (route %in% names(cmap)) as.integer(cmap[[route]]) else 0L
    limit <- as.integer(floor(as.numeric(base) * as.numeric(thresholds[[route]]) + 1e-9))
    pass <- isTRUE(cur <= limit)
    ok <- ok && pass
    rows[[length(rows) + 1L]] <- data.frame(
      route = route,
      baseline_count = base,
      current_count = cur,
      threshold = as.numeric(thresholds[[route]]),
      allowed_max = limit,
      pass = pass,
      stringsAsFactors = FALSE
    )
  }

  list(
    ok = isTRUE(ok),
    checks = do.call(rbind, rows),
    note = "target routes: cpu_gather/cpu_scatter/cpu_slice/cpu_slice_assign"
  )
}

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  args <- commandArgs(trailingOnly = TRUE)
  repeats <- as.integer(.mojor_parse_cli_arg(args, "repeats", "8"))
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  th_cpu_gather <- as.numeric(.mojor_parse_cli_arg(args, "threshold-cpu-gather", "0.75"))
  th_cpu_scatter <- as.numeric(.mojor_parse_cli_arg(args, "threshold-cpu-scatter", "0.85"))
  th_cpu_slice <- as.numeric(.mojor_parse_cli_arg(args, "threshold-cpu-slice", "1.00"))
  th_cpu_slice_assign <- as.numeric(.mojor_parse_cli_arg(args, "threshold-cpu-slice-assign", "0.50"))

  current <- mojor_gpuarray_audit_index_routes(repeats = repeats)
  print(current)

  if (nzchar(write_baseline)) {
    dir.create(dirname(write_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current, write_baseline, row.names = FALSE)
    cat("Wrote index route baseline:", write_baseline, "\n")
  }

  baseline <- mojor_gpuarray_load_index_route_baseline(baseline_path)
  gate <- mojor_gpuarray_index_route_gate(
    current,
    baseline,
    thresholds = c(
      cpu_gather = th_cpu_gather,
      cpu_scatter = th_cpu_scatter,
      cpu_slice = th_cpu_slice,
      cpu_slice_assign = th_cpu_slice_assign
    )
  )
  if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
    cat("\nIndex route gate:\n")
    print(gate$checks)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray index route gate failed")
  }
}
