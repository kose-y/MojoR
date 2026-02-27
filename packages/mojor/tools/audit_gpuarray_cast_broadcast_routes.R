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

.mojor_gpuarray_cast_broadcast_routes_df <- function(route_vec) {
  route_vec <- as.character(route_vec)
  route_vec <- route_vec[!is.na(route_vec) & nzchar(route_vec)]
  all_routes <- c("gpu_cast", "cpu_cast", "gpu_broadcast", "cpu_broadcast")
  tab <- table(factor(route_vec, levels = all_routes))
  data.frame(
    route = all_routes,
    count = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

mojor_gpuarray_audit_cast_broadcast_routes <- function(repeats = 8L) {
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

  x_f32 <- GPUArray(matrix(as.numeric(1:6), nrow = 2L, ncol = 3L), dtype = "f32")
  x_f64 <- GPUArray(matrix(as.numeric(1:6) / 2, nrow = 2L, ncol = 3L), dtype = "f64")
  x_i32 <- GPUArray(matrix(as.integer(1:6), nrow = 2L, ncol = 3L), dtype = "i32")

  b_vec <- GPUArray(as.numeric(c(1, 2, 3)), dtype = "f32")
  b_mat <- GPUArray(matrix(as.integer(c(1L, 2L)), nrow = 2L, ncol = 1L), dtype = "i32")
  b_arr <- GPUArray(array(as.numeric(c(10, 20)), dim = c(1L, 2L, 1L)), dtype = "f32")

  on.exit({
    try(mojor_gpu_array_free(b_arr), silent = TRUE)
    try(mojor_gpu_array_free(b_mat), silent = TRUE)
    try(mojor_gpu_array_free(b_vec), silent = TRUE)
    try(mojor_gpu_array_free(x_i32), silent = TRUE)
    try(mojor_gpu_array_free(x_f64), silent = TRUE)
    try(mojor_gpu_array_free(x_f32), silent = TRUE)
  }, add = TRUE)

  for (iter in seq_len(repeats)) {
    out_f32_f64 <- gpu_cast(x_f32, "f64")
    rec_route(attr(out_f32_f64, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_f32_f64))
    mojor_gpu_array_free(out_f32_f64)

    out_f32_i32 <- gpu_cast(x_f32, "i32")
    rec_route(attr(out_f32_i32, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_f32_i32))
    mojor_gpu_array_free(out_f32_i32)

    out_f64_f32 <- gpu_cast(x_f64, "f32")
    rec_route(attr(out_f64_f32, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_f64_f32))
    mojor_gpu_array_free(out_f64_f32)

    out_f64_i32 <- gpu_cast(x_f64, "i32")
    rec_route(attr(out_f64_i32, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_f64_i32))
    mojor_gpu_array_free(out_f64_i32)

    out_i32_f32 <- gpu_cast(x_i32, "f32")
    rec_route(attr(out_i32_f32, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_i32_f32))
    mojor_gpu_array_free(out_i32_f32)

    out_i32_f64 <- gpu_cast(x_i32, "f64")
    rec_route(attr(out_i32_f64, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_i32_f64))
    mojor_gpu_array_free(out_i32_f64)

    out_same <- gpu_cast(x_f32, "f32")
    rec_route(attr(out_same, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_same))
    mojor_gpu_array_free(out_same)

    b2 <- gpu_broadcast(b_vec, c(2L, 3L))
    rec_route(attr(b2, "gpu_fallback"))
    invisible(mojor_gpu_array_read(b2))
    mojor_gpu_array_free(b2)

    b2_i32 <- gpu_broadcast(b_mat, c(2L, 4L))
    rec_route(attr(b2_i32, "gpu_fallback"))
    invisible(mojor_gpu_array_read(b2_i32))
    mojor_gpu_array_free(b2_i32)

    b3 <- gpu_broadcast(b_arr, c(3L, 2L, 4L))
    rec_route(attr(b3, "gpu_fallback"))
    invisible(mojor_gpu_array_read(b3))
    mojor_gpu_array_free(b3)

    b_same <- gpu_broadcast(x_f32, c(2L, 3L))
    rec_route(attr(b_same, "gpu_fallback"))
    invisible(mojor_gpu_array_read(b_same))
    mojor_gpu_array_free(b_same)
  }

  .mojor_gpuarray_cast_broadcast_routes_df(routes)
}

mojor_gpuarray_load_cast_broadcast_route_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "count")
  if (!all(req %in% names(dat))) {
    stop("cast/broadcast route baseline CSV must contain columns: route, count")
  }
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  dat
}

mojor_gpuarray_cast_broadcast_route_thresholds <- function(profile = c("closeout", "targeted-drop")) {
  profile <- match.arg(profile)
  if (identical(profile, "targeted-drop")) {
    return(c(cpu_cast = 0.90, cpu_broadcast = 0.90))
  }
  c(cpu_cast = 1.00, cpu_broadcast = 1.00)
}

mojor_gpuarray_cast_broadcast_route_gate <- function(
  current_routes,
  baseline_routes,
  thresholds = mojor_gpuarray_cast_broadcast_route_thresholds("closeout")
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

  policy_note <- vapply(names(thresholds), function(route) {
    th <- as.numeric(thresholds[[route]])
    if (isTRUE(abs(th - 1.00) < 1e-9)) {
      paste0(route, ": no-growth")
    } else {
      paste0(route, ": targeted-drop(", format(th, digits = 3, nsmall = 2), ")")
    }
  }, FUN.VALUE = character(1L))

  list(
    ok = isTRUE(ok),
    checks = do.call(rbind, rows),
    note = paste(policy_note, collapse = ", ")
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
  profile <- .mojor_parse_cli_arg(args, "profile", "closeout")
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  th_cpu_cast <- .mojor_parse_cli_arg(args, "threshold-cpu-cast", "")
  th_cpu_broadcast <- .mojor_parse_cli_arg(args, "threshold-cpu-broadcast", "")
  thresholds <- mojor_gpuarray_cast_broadcast_route_thresholds(profile = profile)
  if (nzchar(th_cpu_cast)) {
    thresholds[["cpu_cast"]] <- as.numeric(th_cpu_cast)
  }
  if (nzchar(th_cpu_broadcast)) {
    thresholds[["cpu_broadcast"]] <- as.numeric(th_cpu_broadcast)
  }

  current <- mojor_gpuarray_audit_cast_broadcast_routes(repeats = repeats)
  print(current)

  if (nzchar(write_baseline)) {
    dir.create(dirname(write_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current, write_baseline, row.names = FALSE)
    cat("Wrote cast/broadcast route baseline:", write_baseline, "\n")
  }

  baseline <- mojor_gpuarray_load_cast_broadcast_route_baseline(baseline_path)
  gate <- mojor_gpuarray_cast_broadcast_route_gate(
    current,
    baseline,
    thresholds = thresholds
  )

  if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
    cat("\nCast/broadcast route gate profile:", profile, "\n")
    cat("Cast/broadcast route gate policy:", gate$note, "\n")
    cat("\nCast/broadcast route gate:\n")
    print(gate$checks)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray cast/broadcast route gate failed")
  }
}
