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

.mojor_gpuarray_i32_routes_df <- function(route_vec) {
  route_vec <- as.character(route_vec)
  route_vec <- route_vec[!is.na(route_vec) & nzchar(route_vec)]
  all_routes <- c(
    "gpu_matmul", "cpu_matmul",
    "gpu_crossprod", "cpu_crossprod",
    "gpu_tcrossprod", "cpu_tcrossprod",
    "gpu_reduce", "cpu_reduce"
  )
  tab <- table(factor(route_vec, levels = all_routes))
  data.frame(
    route = all_routes,
    count = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

.mojor_gpuarray_record_route <- function(routes_ref, route) {
  route <- as.character(route)
  if (length(route) == 1L && !is.na(route) && nzchar(route)) {
    routes_ref$routes <- c(routes_ref$routes, route)
  }
  invisible(NULL)
}

mojor_gpuarray_audit_i32_routes <- function(repeats = 8L) {
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

  routes_ref <- new.env(parent = emptyenv())
  routes_ref$routes <- character()

  x_mm <- GPUArray(matrix(as.integer(seq_len(16L)), nrow = 4L, ncol = 4L), dtype = "i32")
  y_mm <- GPUArray(matrix(as.integer(seq_len(16L) * 2L), nrow = 4L, ncol = 4L), dtype = "i32")

  x_gemv <- GPUArray(matrix(as.integer(seq_len(15L)), nrow = 5L, ncol = 3L), dtype = "i32")
  y_gemv <- GPUArray(matrix(as.integer(seq_len(3L)), nrow = 3L, ncol = 1L), dtype = "i32")

  x_gevm <- GPUArray(matrix(as.integer(seq_len(5L)), nrow = 1L, ncol = 5L), dtype = "i32")
  y_gevm <- GPUArray(matrix(as.integer(seq_len(15L) * 2L), nrow = 5L, ncol = 3L), dtype = "i32")

  x_cp <- GPUArray(matrix(as.integer(seq_len(12L)), nrow = 4L, ncol = 3L), dtype = "i32")
  y_cp <- GPUArray(matrix(as.integer(seq_len(12L) * 3L), nrow = 4L, ncol = 3L), dtype = "i32")

  r <- GPUArray(matrix(as.integer(seq_len(20L)), nrow = 4L, ncol = 5L), dtype = "i32")

  on.exit({
    try(mojor_gpu_array_free(r), silent = TRUE)
    try(mojor_gpu_array_free(y_cp), silent = TRUE)
    try(mojor_gpu_array_free(x_cp), silent = TRUE)
    try(mojor_gpu_array_free(y_gevm), silent = TRUE)
    try(mojor_gpu_array_free(x_gevm), silent = TRUE)
    try(mojor_gpu_array_free(y_gemv), silent = TRUE)
    try(mojor_gpu_array_free(x_gemv), silent = TRUE)
    try(mojor_gpu_array_free(y_mm), silent = TRUE)
    try(mojor_gpu_array_free(x_mm), silent = TRUE)
  }, add = TRUE)

  for (iter in seq_len(repeats)) {
    out_mm <- gpu_matmul(x_mm, y_mm)
    .mojor_gpuarray_record_route(routes_ref, attr(out_mm, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_mm))
    mojor_gpu_array_free(out_mm)

    out_op <- x_mm %*% y_mm
    .mojor_gpuarray_record_route(routes_ref, attr(out_op, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_op))
    mojor_gpu_array_free(out_op)

    out_into <- gpu_zeros(dim = c(4L, 4L), dtype = "i32")
    out_into_res <- gpu_matmul_into(out_into, x_mm, y_mm)
    .mojor_gpuarray_record_route(routes_ref, attr(out_into_res, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_into_res))
    mojor_gpu_array_free(out_into)

    out_gemv <- gpu_matmul(x_gemv, y_gemv)
    .mojor_gpuarray_record_route(routes_ref, attr(out_gemv, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_gemv))
    mojor_gpu_array_free(out_gemv)

    out_gevm <- gpu_matmul(x_gevm, y_gevm)
    .mojor_gpuarray_record_route(routes_ref, attr(out_gevm, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_gevm))
    mojor_gpu_array_free(out_gevm)

    out_cp <- crossprod.mojor_gpu_array(x_cp, y_cp)
    .mojor_gpuarray_record_route(routes_ref, attr(out_cp, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_cp))
    mojor_gpu_array_free(out_cp)

    out_tcp <- tcrossprod.mojor_gpu_array(x_cp, y_cp)
    .mojor_gpuarray_record_route(routes_ref, attr(out_tcp, "gpu_fallback"))
    invisible(mojor_gpu_array_read(out_tcp))
    mojor_gpu_array_free(out_tcp)

    red_scalar <- gpu_reduce(r, "sum", dims = NULL, keepdims = FALSE)
    .mojor_gpuarray_record_route(routes_ref, attr(red_scalar, "gpu_fallback"))
    invisible(mojor_gpu_array_read(red_scalar))
    mojor_gpu_array_free(red_scalar)

    red_dims1 <- gpu_reduce(r, "sum", dims = 1L, keepdims = FALSE)
    .mojor_gpuarray_record_route(routes_ref, attr(red_dims1, "gpu_fallback"))
    invisible(mojor_gpu_array_read(red_dims1))
    mojor_gpu_array_free(red_dims1)

    red_dims2_keep <- gpu_reduce(r, "sum", dims = 2L, keepdims = TRUE)
    .mojor_gpuarray_record_route(routes_ref, attr(red_dims2_keep, "gpu_fallback"))
    invisible(mojor_gpu_array_read(red_dims2_keep))
    mojor_gpu_array_free(red_dims2_keep)
  }

  .mojor_gpuarray_i32_routes_df(routes_ref$routes)
}

mojor_gpuarray_load_i32_route_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "count")
  if (!all(req %in% names(dat))) {
    stop("i32 route baseline CSV must contain columns: route, count")
  }
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  dat
}

mojor_gpuarray_i32_route_thresholds <- function(profile = c("closeout", "targeted-drop")) {
  profile <- match.arg(profile)
  if (identical(profile, "targeted-drop")) {
    return(c(
      cpu_matmul = 0.95,
      cpu_crossprod = 0.90,
      cpu_tcrossprod = 0.90,
      cpu_reduce = 0.90
    ))
  }
  c(
    cpu_matmul = 1.00,
    cpu_crossprod = 1.00,
    cpu_tcrossprod = 1.00,
    cpu_reduce = 1.00
  )
}

mojor_gpuarray_i32_route_gate <- function(
  current_routes,
  baseline_routes,
  thresholds = mojor_gpuarray_i32_route_thresholds("closeout")
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
  th_cpu_matmul <- .mojor_parse_cli_arg(args, "threshold-cpu-matmul", "")
  th_cpu_crossprod <- .mojor_parse_cli_arg(args, "threshold-cpu-crossprod", "")
  th_cpu_tcrossprod <- .mojor_parse_cli_arg(args, "threshold-cpu-tcrossprod", "")
  th_cpu_reduce <- .mojor_parse_cli_arg(args, "threshold-cpu-reduce", "")
  thresholds <- mojor_gpuarray_i32_route_thresholds(profile = profile)
  if (nzchar(th_cpu_matmul)) {
    thresholds[["cpu_matmul"]] <- as.numeric(th_cpu_matmul)
  }
  if (nzchar(th_cpu_crossprod)) {
    thresholds[["cpu_crossprod"]] <- as.numeric(th_cpu_crossprod)
  }
  if (nzchar(th_cpu_tcrossprod)) {
    thresholds[["cpu_tcrossprod"]] <- as.numeric(th_cpu_tcrossprod)
  }
  if (nzchar(th_cpu_reduce)) {
    thresholds[["cpu_reduce"]] <- as.numeric(th_cpu_reduce)
  }

  current <- mojor_gpuarray_audit_i32_routes(repeats = repeats)
  print(current)

  if (nzchar(write_baseline)) {
    dir.create(dirname(write_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current, write_baseline, row.names = FALSE)
    cat("Wrote i32 route baseline:", write_baseline, "\n")
  }

  baseline <- mojor_gpuarray_load_i32_route_baseline(baseline_path)
  gate <- mojor_gpuarray_i32_route_gate(
    current,
    baseline,
    thresholds = thresholds
  )

  if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
    cat("\nI32 route gate profile:", profile, "\n")
    cat("I32 route gate policy:", gate$note, "\n")
    cat("\nI32 route gate:\n")
    print(gate$checks)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray i32 route gate failed")
  }
}
