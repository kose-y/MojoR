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

.mojor_gpuarray_repo_root <- function() {
  mojor_path <- .mojor_gpuarray_find_mojor()
  root <- dirname(dirname(mojor_path))
  if (identical(basename(root), "prototype")) {
    return(dirname(root))
  }
  if (identical(basename(root), "mojor") && identical(basename(dirname(root)), "packages")) {
    return(dirname(dirname(root)))
  }
  root
}

.mojor_gpuarray_f64_audit_lock_dir <- function() {
  repo_root <- .mojor_gpuarray_repo_root()
  build_dir <- file.path(repo_root, "build")
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(build_dir, "gpuarray_f64_route_audit.lockdir")
}

.mojor_gpuarray_with_f64_audit_lock <- function(code, wait_seconds = 120, stale_seconds = 900) {
  lock_dir <- .mojor_gpuarray_f64_audit_lock_dir()
  started <- Sys.time()
  repeat {
    if (isTRUE(dir.create(lock_dir, showWarnings = FALSE))) {
      break
    }

    info <- file.info(lock_dir)
    if (!is.na(info$mtime[[1L]])) {
      age_sec <- as.numeric(difftime(Sys.time(), info$mtime[[1L]], units = "secs"))
      if (is.finite(age_sec) && age_sec > as.numeric(stale_seconds)) {
        unlink(lock_dir, recursive = TRUE, force = TRUE)
        next
      }
    }

    waited <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (is.finite(waited) && waited > as.numeric(wait_seconds)) {
      stop("GPUArray f64 route audit lock timeout (another audit process is still running)")
    }
    Sys.sleep(0.1)
  }

  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)
  eval.parent(substitute(code))
}

.mojor_gpuarray_f64_routes_df <- function(route_vec) {
  route_vec <- as.character(route_vec)
  route_vec <- route_vec[!is.na(route_vec) & nzchar(route_vec)]
  all_routes <- c("gpu_matmul", "cpu_matmul", "gpu_reduce", "cpu_reduce")
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

mojor_gpuarray_audit_f64_routes <- function(repeats = 8L, allow_gpu_probe = FALSE) {
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
  allow_gpu_probe <- isTRUE(allow_gpu_probe)

  .mojor_gpuarray_with_f64_audit_lock({
    routes_ref <- new.env(parent = emptyenv())
    routes_ref$routes <- character()

    x_mm <- GPUArray(matrix(as.numeric(seq_len(16L)) / 16, nrow = 4L, ncol = 4L), dtype = "f64")
    y_mm <- GPUArray(matrix(as.numeric(seq_len(16L)) / 8, nrow = 4L, ncol = 4L), dtype = "f64")

    x_gemv <- GPUArray(matrix(as.numeric(seq_len(15L)) / 15, nrow = 5L, ncol = 3L), dtype = "f64")
    y_gemv <- GPUArray(matrix(as.numeric(seq_len(3L)) / 3, nrow = 3L, ncol = 1L), dtype = "f64")

    x_gevm <- GPUArray(matrix(as.numeric(seq_len(5L)) / 5, nrow = 1L, ncol = 5L), dtype = "f64")
    y_gevm <- GPUArray(matrix(as.numeric(seq_len(15L)) / 10, nrow = 5L, ncol = 3L), dtype = "f64")

    r <- GPUArray(matrix(as.numeric(seq_len(20L)), nrow = 4L, ncol = 5L), dtype = "f64")

    on.exit({
      try(mojor_gpu_array_free(r), silent = TRUE)
      try(mojor_gpu_array_free(y_gevm), silent = TRUE)
      try(mojor_gpu_array_free(x_gevm), silent = TRUE)
      try(mojor_gpu_array_free(y_gemv), silent = TRUE)
      try(mojor_gpu_array_free(x_gemv), silent = TRUE)
      try(mojor_gpu_array_free(y_mm), silent = TRUE)
      try(mojor_gpu_array_free(x_mm), silent = TRUE)
    }, add = TRUE)

    if (!isTRUE(allow_gpu_probe)) {
      ctx <- tryCatch(.mojor_gpu_ctx_get(), error = function(e) NULL)
      if (!is.null(ctx)) {
        ctx_epoch <- .mojor_gpu_ctx_epoch_get()
        cache <- .mojor_state$gpu_capability_cache
        if (is.null(cache) || !is.list(cache)) {
          cache <- list()
        }
        for (key in c(
          "f64_matmul_matmul",
          "f64_matmul_gemv",
          "f64_matmul_gevm",
          "f64_reduce_scalar_value",
          "f64_reduce_scalar_arg",
          "f64_reduce_dims_value",
          "f64_reduce_dims_arg"
        )) {
          cache <- .mojor_gpu_capability_cache_store(
            cache,
            key,
            ctx,
            ctx_epoch,
            available = FALSE,
            reason = "f64 audit safe-mode (cpu route)"
          )
        }
        .mojor_state$gpu_capability_cache <- cache
      }
    }

    for (iter in seq_len(repeats)) {
      out_mm <- gpu_matmul(x_mm, y_mm)
      .mojor_gpuarray_record_route(routes_ref, attr(out_mm, "gpu_fallback"))
      invisible(mojor_gpu_array_read(out_mm))
      mojor_gpu_array_free(out_mm)

      out_gemv <- gpu_matmul(x_gemv, y_gemv)
      .mojor_gpuarray_record_route(routes_ref, attr(out_gemv, "gpu_fallback"))
      invisible(mojor_gpu_array_read(out_gemv))
      mojor_gpu_array_free(out_gemv)

      out_gevm <- gpu_matmul(x_gevm, y_gevm)
      .mojor_gpuarray_record_route(routes_ref, attr(out_gevm, "gpu_fallback"))
      invisible(mojor_gpu_array_read(out_gevm))
      mojor_gpu_array_free(out_gevm)

      red_scalar <- gpu_reduce(r, "sum", dims = NULL, keepdims = FALSE)
      .mojor_gpuarray_record_route(routes_ref, attr(red_scalar, "gpu_fallback"))
      invisible(mojor_gpu_array_read(red_scalar))
      mojor_gpu_array_free(red_scalar)

      red_dims <- gpu_reduce(r, "sum", dims = 1L, keepdims = FALSE)
      .mojor_gpuarray_record_route(routes_ref, attr(red_dims, "gpu_fallback"))
      invisible(mojor_gpu_array_read(red_dims))
      mojor_gpu_array_free(red_dims)

      red_arg_scalar <- gpu_reduce(r, "argmax", dims = NULL, keepdims = FALSE)
      .mojor_gpuarray_record_route(routes_ref, attr(red_arg_scalar, "gpu_fallback"))
      invisible(mojor_gpu_array_read(red_arg_scalar))
      mojor_gpu_array_free(red_arg_scalar)

      red_arg_dims <- gpu_reduce(r, "argmax", dims = 1L, keepdims = FALSE)
      .mojor_gpuarray_record_route(routes_ref, attr(red_arg_dims, "gpu_fallback"))
      invisible(mojor_gpu_array_read(red_arg_dims))
      mojor_gpu_array_free(red_arg_dims)

    }

    .mojor_gpuarray_f64_routes_df(routes_ref$routes)
  })
}

mojor_gpuarray_load_f64_route_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "count")
  if (!all(req %in% names(dat))) {
    stop("f64 route baseline CSV must contain columns: route, count")
  }
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  dat
}

mojor_gpuarray_f64_route_thresholds <- function(profile = c("closeout", "targeted-drop")) {
  profile <- match.arg(profile)
  if (identical(profile, "targeted-drop")) {
    return(c(cpu_matmul = 0.90, cpu_reduce = 0.85))
  }
  c(cpu_matmul = 1.00, cpu_reduce = 1.00)
}

mojor_gpuarray_f64_route_gate <- function(
  current_routes,
  baseline_routes,
  thresholds = mojor_gpuarray_f64_route_thresholds("closeout")
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
  allow_gpu_probe <- tolower(.mojor_parse_cli_arg(args, "allow-gpu-probe", "0")) %in% c("1", "true", "yes", "y", "on")
  profile <- .mojor_parse_cli_arg(args, "profile", "closeout")
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  th_cpu_matmul <- .mojor_parse_cli_arg(args, "threshold-cpu-matmul", "")
  th_cpu_reduce <- .mojor_parse_cli_arg(args, "threshold-cpu-reduce", "")
  thresholds <- mojor_gpuarray_f64_route_thresholds(profile = profile)
  if (nzchar(th_cpu_matmul)) {
    thresholds[["cpu_matmul"]] <- as.numeric(th_cpu_matmul)
  }
  if (nzchar(th_cpu_reduce)) {
    thresholds[["cpu_reduce"]] <- as.numeric(th_cpu_reduce)
  }

  current <- mojor_gpuarray_audit_f64_routes(
    repeats = repeats,
    allow_gpu_probe = allow_gpu_probe
  )
  print(current)

  if (nzchar(write_baseline)) {
    dir.create(dirname(write_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current, write_baseline, row.names = FALSE)
    cat("Wrote f64 route baseline:", write_baseline, "\n")
  }

  baseline <- mojor_gpuarray_load_f64_route_baseline(baseline_path)
  gate <- mojor_gpuarray_f64_route_gate(
    current,
    baseline,
    thresholds = thresholds
  )

  if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
    cat("\nF64 route gate profile:", profile, "\n")
    cat("F64 route gate policy:", gate$note, "\n")
    cat("\nF64 route gate:\n")
    print(gate$checks)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray f64 route gate failed")
  }
}
