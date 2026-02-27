#!/usr/bin/env Rscript

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

.mojor_gpuarray_ensure_loaded <- function() {
  if (!exists("GPUArray", mode = "function")) {
    source(.mojor_gpuarray_find_mojor())
  }
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

.mojor_gpuarray_math_routes_df <- function(route_vec, op_vec, known_ops = character()) {
  route_vec <- as.character(route_vec)
  op_vec <- as.character(op_vec)
  known_ops <- unique(as.character(known_ops))
  known_ops <- known_ops[!is.na(known_ops) & nzchar(known_ops)]
  keep <- !is.na(route_vec) & nzchar(route_vec) & !is.na(op_vec) & nzchar(op_vec)
  route_vec <- route_vec[keep]
  op_vec <- op_vec[keep]
  base_routes <- c("cpu_kernel_host", "cpu_arith")
  extra_routes <- sort(setdiff(unique(route_vec), base_routes))
  all_routes <- c(base_routes, extra_routes)
  op_levels <- unique(c(known_ops, op_vec))

  rows <- vector("list", length = 0L)
  append_rows <- function(op_name, mask) {
    tab <- table(factor(route_vec[mask], levels = all_routes))
    rows[[length(rows) + 1L]] <<- data.frame(
      op = rep.int(op_name, length(all_routes)),
      route = all_routes,
      count = as.integer(tab),
      stringsAsFactors = FALSE
    )
  }

  append_rows("__all__", rep.int(TRUE, length(route_vec)))
  for (op_name in op_levels) {
    append_rows(op_name, op_vec == op_name)
  }
  do.call(rbind, rows)
}

.mojor_gpuarray_math_reason_counts_df <- function(route_vec, reason_vec, op_vec, known_ops = character()) {
  route_vec <- as.character(route_vec)
  reason_vec <- as.character(reason_vec)
  op_vec <- as.character(op_vec)
  known_ops <- unique(as.character(known_ops))
  known_ops <- known_ops[!is.na(known_ops) & nzchar(known_ops)]
  keep <- !is.na(route_vec) & nzchar(route_vec) & !is.na(op_vec) & nzchar(op_vec)
  route_vec <- route_vec[keep]
  reason_vec <- reason_vec[keep]
  op_vec <- op_vec[keep]
  reason_vec[is.na(reason_vec) | !nzchar(reason_vec)] <- "<none>"

  base_routes <- c("cpu_kernel_host", "cpu_arith", "cpu_reduce")
  route_levels <- c(base_routes, sort(setdiff(unique(route_vec), base_routes)))
  reason_levels <- unique(c(
    "<none>",
    "raw_kernel_unavailable",
    "kernel_dispatch_failed",
    "round_digits_host_fallback",
    "na_rm_host_parity",
    "shape_mismatch_host_parity",
    "kernel_wrapper_dispatch_failed",
    "kernel_wrapper_unavailable",
    sort(unique(reason_vec))
  ))
  op_levels <- unique(c(known_ops, op_vec))

  rows <- vector("list", length = 0L)
  append_rows <- function(op_name, route_name, mask) {
    tab <- table(factor(reason_vec[mask], levels = reason_levels))
    rows[[length(rows) + 1L]] <<- data.frame(
      op = rep.int(op_name, length(reason_levels)),
      route = rep.int(route_name, length(reason_levels)),
      reason = reason_levels,
      count = as.integer(tab),
      stringsAsFactors = FALSE
    )
  }

  for (route_name in route_levels) {
    append_rows("__all__", route_name, route_vec == route_name)
    for (op_name in op_levels) {
      append_rows(op_name, route_name, route_vec == route_name & op_vec == op_name)
    }
  }
  do.call(rbind, rows)
}

.mojor_gpuarray_record_route <- function(routes_ref, route, op, reason = NULL) {
  route <- as.character(route)
  op <- as.character(op)
  reason <- as.character(reason)
  if (length(route) == 1L &&
      !is.na(route) &&
      nzchar(route) &&
      length(op) == 1L &&
      !is.na(op) &&
      nzchar(op)) {
    routes_ref$routes <- c(routes_ref$routes, route)
    routes_ref$ops <- c(routes_ref$ops, op)
    reason_norm <- if (length(reason) == 1L && !is.na(reason) && nzchar(reason)) {
      reason
    } else {
      "<none>"
    }
    routes_ref$reasons <- c(routes_ref$reasons, reason_norm)
  }
  invisible(NULL)
}

.mojor_gpuarray_eval_math_op <- function(expr, quiet_fallback_warnings = TRUE) {
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

mojor_gpuarray_audit_math_routes <- function(
  repeats = 8L,
  quiet_fallback_warnings = TRUE,
  progress_every = 0L,
  return_details = FALSE
) {
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
  progress_every <- suppressWarnings(as.integer(progress_every))
  if (is.na(progress_every) || progress_every < 0L) {
    progress_every <- 0L
  }

  routes_ref <- new.env(parent = emptyenv())
  routes_ref$routes <- character()
  routes_ref$ops <- character()
  routes_ref$reasons <- character()
  routes_ref$ops_seen <- character()

  x_pos <- GPUArray(matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L), dtype = "f32")
  x_mix <- GPUArray(matrix(c(-2.2, -1.1, 0.0, 1.1, 2.2, 3.3), nrow = 2L, ncol = 3L), dtype = "f32")
  y <- GPUArray(matrix(c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), nrow = 2L, ncol = 3L), dtype = "f32")
  y_shape <- GPUArray(matrix(c(0.5, 1.5, 2.5), nrow = 1L, ncol = 3L), dtype = "f32")
  x_na <- GPUArray(matrix(c(NA_real_, 1, NA_real_, 3), nrow = 2L, ncol = 2L), dtype = "f32")
  y_na <- GPUArray(matrix(c(2, NA_real_, 4, NA_real_), nrow = 2L, ncol = 2L), dtype = "f32")

  on.exit({
    try(mojor_gpu_array_free(y_na), silent = TRUE)
    try(mojor_gpu_array_free(x_na), silent = TRUE)
    try(mojor_gpu_array_free(y_shape), silent = TRUE)
    try(mojor_gpu_array_free(y), silent = TRUE)
    try(mojor_gpu_array_free(x_mix), silent = TRUE)
    try(mojor_gpu_array_free(x_pos), silent = TRUE)
  }, add = TRUE)

  rec <- function(op, out) {
    routes_ref$ops_seen <- c(routes_ref$ops_seen, as.character(op))
    .mojor_gpuarray_record_route(
      routes_ref,
      attr(out, "gpu_fallback"),
      op,
      reason = attr(out, "gpu_fallback_reason_code")
    )
    invisible(mojor_gpu_array_read(out))
    mojor_gpu_array_free(out)
    invisible(NULL)
  }
  run_op <- function(op, expr) {
    rec(op, .mojor_gpuarray_eval_math_op(expr, quiet_fallback_warnings = quiet_fallback_warnings))
  }

  for (iter in seq_len(repeats)) {
    run_op("sin", sin(x_pos))
    run_op("cos", cos(x_pos))
    run_op("tan", tan(x_pos))
    run_op("exp", exp(x_pos))
    run_op("log", log(x_pos))
    run_op("log10", log10(x_pos))
    run_op("log2", log2(x_pos))
    run_op("log1p", log1p(x_pos))
    run_op("expm1", expm1(x_pos))
    run_op("sqrt", sqrt(x_pos))
    run_op("abs", abs(x_mix))
    run_op("sign", sign(x_mix))
    run_op("trunc", trunc(x_mix))
    run_op("floor", floor(x_mix))
    run_op("ceiling", ceiling(x_mix))
    run_op("round_digits0", round(x_mix, digits = 0L))
    run_op("round_digits2", round(x_mix, digits = 2L))
    run_op("gpu_minimum", gpu_minimum(x_pos, y))
    run_op("gpu_maximum", gpu_maximum(x_pos, y, 0.75))
    run_op("gpu_atan2", gpu_atan2(x_pos, y))
    run_op("gpu_sum", gpu_sum(x_pos))
    run_op("gpu_mean", gpu_mean(x_pos))
    run_op("gpu_prod", gpu_prod(x_pos))
    run_op("gpu_min_reduce", gpu_min(x_pos))
    run_op("gpu_max_reduce", gpu_max(x_pos))
    run_op("gpu_minimum_na_rm", gpu_minimum(x_na, y_na, na.rm = TRUE))
    run_op("gpu_minimum_shape_mismatch", gpu_minimum(x_pos, y_shape))
    if (progress_every > 0L && (iter %% progress_every) == 0L) {
      cat(sprintf("[gpu-math-audit] completed %d/%d iterations\n", iter, repeats))
      flush.console()
    }
  }

  routes_df <- .mojor_gpuarray_math_routes_df(
    routes_ref$routes,
    routes_ref$ops,
    known_ops = unique(routes_ref$ops_seen)
  )
  reason_df <- .mojor_gpuarray_math_reason_counts_df(
    routes_ref$routes,
    routes_ref$reasons,
    routes_ref$ops,
    known_ops = unique(routes_ref$ops_seen)
  )
  if (!isTRUE(return_details)) {
    return(routes_df)
  }
  list(routes = routes_df, reasons = reason_df)
}

mojor_gpuarray_load_math_route_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "count")
  if (!all(req %in% names(dat))) {
    stop("math route baseline CSV must contain columns: route, count (optional: op)")
  }
  if (!("op" %in% names(dat))) {
    dat$op <- "__all__"
  }
  dat$op <- as.character(dat$op)
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  dat[, c("op", "route", "count")]
}

mojor_gpuarray_load_math_reason_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("route", "reason", "count")
  if (!all(req %in% names(dat))) {
    stop("math reason baseline CSV must contain columns: route, reason, count (optional: op)")
  }
  if (!("op" %in% names(dat))) {
    dat$op <- "__all__"
  }
  dat$op <- as.character(dat$op)
  dat$route <- as.character(dat$route)
  dat$reason <- as.character(dat$reason)
  dat$count <- as.integer(dat$count)
  dat[, c("op", "route", "reason", "count")]
}

mojor_gpuarray_math_route_thresholds <- function(profile = c("closeout", "targeted-drop", "ci-fast", "nightly")) {
  profile <- match.arg(profile)
  ops <- c(
    "__all__",
    "gpu_minimum", "gpu_maximum", "gpu_atan2",
    "gpu_sum", "gpu_mean", "gpu_prod", "gpu_min_reduce", "gpu_max_reduce"
  )
  routes <- c("cpu_arith", "cpu_kernel_host", "cpu_reduce")
  if (identical(profile, "ci-fast")) {
    ops <- "__all__"
  }
  threshold <- if (identical(profile, "targeted-drop")) 0.85 else 1.00
  out <- expand.grid(
    op = ops,
    route = routes,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out$threshold <- rep.int(as.numeric(threshold), nrow(out))
  out
}

mojor_gpuarray_math_reason_thresholds <- function(profile = c("closeout", "targeted-drop", "ci-fast", "nightly")) {
  profile <- match.arg(profile)
  ops <- c(
    "__all__",
    "log", "log10", "log2", "log1p", "round_digits2",
    "gpu_atan2", "gpu_minimum_na_rm", "gpu_minimum_shape_mismatch",
    "gpu_sum", "gpu_mean", "gpu_prod", "gpu_min_reduce", "gpu_max_reduce"
  )
  routes <- c("cpu_arith", "cpu_kernel_host", "cpu_reduce")
  reasons <- c(
    "<none>",
    "raw_kernel_unavailable",
    "kernel_dispatch_failed",
    "round_digits_host_fallback",
    "na_rm_host_parity",
    "shape_mismatch_host_parity",
    "kernel_wrapper_dispatch_failed",
    "kernel_wrapper_unavailable"
  )
  if (identical(profile, "ci-fast")) {
    ops <- "__all__"
  }
  threshold <- if (identical(profile, "targeted-drop")) 0.85 else 1.00
  out <- expand.grid(
    op = ops,
    route = routes,
    reason = reasons,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out$threshold <- rep.int(as.numeric(threshold), nrow(out))
  out
}

.mojor_gpuarray_math_route_threshold_table <- function(thresholds) {
  if (is.null(thresholds)) {
    return(data.frame(op = character(), route = character(), threshold = numeric(), stringsAsFactors = FALSE))
  }
  if (is.data.frame(thresholds)) {
    dat <- thresholds
    if (!all(c("route", "threshold") %in% names(dat))) {
      stop("threshold data.frame must contain: route, threshold (optional: op)")
    }
    if (!("op" %in% names(dat))) {
      dat$op <- "__all__"
    }
    dat$op <- as.character(dat$op)
    dat$route <- as.character(dat$route)
    dat$threshold <- as.numeric(dat$threshold)
    keep <- !is.na(dat$op) & nzchar(dat$op) &
      !is.na(dat$route) & nzchar(dat$route) &
      !is.na(dat$threshold)
    return(unique(dat[keep, c("op", "route", "threshold"), drop = FALSE]))
  }
  if (is.numeric(thresholds) && !is.null(names(thresholds)) && length(thresholds) > 0L) {
    keep <- !is.na(names(thresholds)) & nzchar(names(thresholds)) &
      !is.na(as.numeric(thresholds))
    return(data.frame(
      op = "__all__",
      route = as.character(names(thresholds)[keep]),
      threshold = as.numeric(thresholds[keep]),
      stringsAsFactors = FALSE
    ))
  }
  stop("thresholds must be a named numeric vector or data.frame(op?, route, threshold)")
}

.mojor_gpuarray_math_route_count_map <- function(routes) {
  if (is.null(routes) || nrow(routes) == 0) {
    return(structure(integer(), names = character()))
  }
  dat <- routes
  if (!("op" %in% names(dat))) {
    dat$op <- "__all__"
  }
  dat$op <- as.character(dat$op)
  dat$route <- as.character(dat$route)
  dat$count <- as.integer(dat$count)
  key <- paste0(dat$op, "\t", dat$route)
  stats::setNames(dat$count, key)
}

.mojor_gpuarray_math_route_lookup <- function(map, op, route) {
  key <- paste0(op, "\t", route)
  if (key %in% names(map)) {
    return(as.integer(map[[key]]))
  }
  0L
}

.mojor_gpuarray_math_reason_count_map <- function(reasons) {
  if (is.null(reasons) || nrow(reasons) == 0) {
    return(structure(integer(), names = character()))
  }
  dat <- reasons
  if (!("op" %in% names(dat))) {
    dat$op <- "__all__"
  }
  dat$op <- as.character(dat$op)
  dat$route <- as.character(dat$route)
  dat$reason <- as.character(dat$reason)
  dat$count <- as.integer(dat$count)
  key <- paste0(dat$op, "\t", dat$route, "\t", dat$reason)
  stats::setNames(dat$count, key)
}

.mojor_gpuarray_math_reason_lookup <- function(map, op, route, reason) {
  key <- paste0(op, "\t", route, "\t", reason)
  if (key %in% names(map)) {
    return(as.integer(map[[key]]))
  }
  0L
}

.mojor_gpuarray_math_route_policy_note <- function(threshold_table) {
  if (is.null(threshold_table) || nrow(threshold_table) == 0L) {
    return("no thresholds")
  }
  notes <- vapply(seq_len(nrow(threshold_table)), function(i) {
    row <- threshold_table[i, , drop = FALSE]
    th <- as.numeric(row$threshold[[1L]])
    lane <- paste0(row$op[[1L]], "/", row$route[[1L]])
    if (isTRUE(abs(th - 1.00) < 1e-9)) {
      paste0(lane, ": no-growth")
    } else {
      paste0(lane, ": targeted-drop(", format(th, digits = 3, nsmall = 2), ")")
    }
  }, FUN.VALUE = character(1L))
  paste(notes, collapse = ", ")
}

mojor_gpuarray_math_route_gate <- function(
  current_routes,
  baseline_routes,
  thresholds = mojor_gpuarray_math_route_thresholds("closeout")
) {
  threshold_table <- .mojor_gpuarray_math_route_threshold_table(thresholds)
  if (nrow(threshold_table) == 0L) {
    return(list(ok = TRUE, checks = data.frame(), note = "no thresholds configured"))
  }
  if (is.null(baseline_routes) || nrow(baseline_routes) == 0) {
    return(list(ok = TRUE, checks = data.frame(), note = "no baseline provided"))
  }
  if (is.null(current_routes) || nrow(current_routes) == 0) {
    return(list(ok = FALSE, checks = data.frame(), note = "no current routes"))
  }

  bmap <- .mojor_gpuarray_math_route_count_map(baseline_routes)
  cmap <- .mojor_gpuarray_math_route_count_map(current_routes)
  rows <- vector("list", nrow(threshold_table))
  ok <- TRUE
  for (i in seq_len(nrow(threshold_table))) {
    op <- threshold_table$op[[i]]
    route <- threshold_table$route[[i]]
    th <- as.numeric(threshold_table$threshold[[i]])
    base <- .mojor_gpuarray_math_route_lookup(bmap, op, route)
    cur <- .mojor_gpuarray_math_route_lookup(cmap, op, route)
    limit <- as.integer(floor(as.numeric(base) * th + 1e-9))
    pass <- isTRUE(cur <= limit)
    ok <- ok && pass
    rows[[i]] <- data.frame(
      op = op,
      route = route,
      baseline_count = base,
      current_count = cur,
      threshold = th,
      allowed_max = limit,
      pass = pass,
      stringsAsFactors = FALSE
    )
  }

  list(
    ok = isTRUE(ok),
    checks = do.call(rbind, rows),
    note = .mojor_gpuarray_math_route_policy_note(threshold_table)
  )
}

.mojor_gpuarray_math_reason_threshold_table <- function(thresholds) {
  if (is.null(thresholds)) {
    return(data.frame(
      op = character(),
      route = character(),
      reason = character(),
      threshold = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(thresholds)) {
    stop("reason thresholds must be data.frame(op?, route, reason, threshold)")
  }
  dat <- thresholds
  if (!all(c("route", "reason", "threshold") %in% names(dat))) {
    stop("reason threshold data.frame must contain: route, reason, threshold (optional: op)")
  }
  if (!("op" %in% names(dat))) {
    dat$op <- "__all__"
  }
  dat$op <- as.character(dat$op)
  dat$route <- as.character(dat$route)
  dat$reason <- as.character(dat$reason)
  dat$threshold <- as.numeric(dat$threshold)
  keep <- !is.na(dat$op) & nzchar(dat$op) &
    !is.na(dat$route) & nzchar(dat$route) &
    !is.na(dat$reason) & nzchar(dat$reason) &
    !is.na(dat$threshold)
  unique(dat[keep, c("op", "route", "reason", "threshold"), drop = FALSE])
}

mojor_gpuarray_math_reason_gate <- function(
  current_reasons,
  baseline_reasons,
  thresholds = mojor_gpuarray_math_reason_thresholds("closeout")
) {
  threshold_table <- .mojor_gpuarray_math_reason_threshold_table(thresholds)
  if (nrow(threshold_table) == 0L) {
    return(list(ok = TRUE, checks = data.frame(), note = "no thresholds configured"))
  }
  if (is.null(baseline_reasons) || nrow(baseline_reasons) == 0) {
    return(list(ok = TRUE, checks = data.frame(), note = "no baseline provided"))
  }
  if (is.null(current_reasons) || nrow(current_reasons) == 0) {
    return(list(ok = FALSE, checks = data.frame(), note = "no current reasons"))
  }

  bmap <- .mojor_gpuarray_math_reason_count_map(baseline_reasons)
  cmap <- .mojor_gpuarray_math_reason_count_map(current_reasons)
  rows <- vector("list", nrow(threshold_table))
  ok <- TRUE
  for (i in seq_len(nrow(threshold_table))) {
    op <- threshold_table$op[[i]]
    route <- threshold_table$route[[i]]
    reason <- threshold_table$reason[[i]]
    th <- as.numeric(threshold_table$threshold[[i]])
    base <- .mojor_gpuarray_math_reason_lookup(bmap, op, route, reason)
    cur <- .mojor_gpuarray_math_reason_lookup(cmap, op, route, reason)
    limit <- as.integer(floor(as.numeric(base) * th + 1e-9))
    pass <- isTRUE(cur <= limit)
    ok <- ok && pass
    rows[[i]] <- data.frame(
      op = op,
      route = route,
      reason = reason,
      baseline_count = base,
      current_count = cur,
      threshold = th,
      allowed_max = limit,
      pass = pass,
      stringsAsFactors = FALSE
    )
  }
  list(
    ok = isTRUE(ok),
    checks = do.call(rbind, rows),
    note = "reason-code no-growth/targeted-drop policy"
  )
}

.mojor_gpuarray_set_threshold <- function(thresholds, op, route, value) {
  dat <- .mojor_gpuarray_math_route_threshold_table(thresholds)
  idx <- which(dat$op == op & dat$route == route)
  if (length(idx) == 0L) {
    dat <- rbind(
      dat,
      data.frame(op = op, route = route, threshold = as.numeric(value), stringsAsFactors = FALSE)
    )
  } else {
    dat$threshold[idx[[1L]]] <- as.numeric(value)
  }
  dat
}

.mojor_parse_cli_arg <- function(args, key, default = NULL) {
  pat <- paste0("^--", key, "=")
  hit <- args[grepl(pat, args)]
  if (length(hit) == 0) return(default)
  sub(pat, "", hit[[1]])
}

.mojor_has_cli_arg <- function(args, key) {
  any(grepl(paste0("^--", key, "="), args))
}

.mojor_gpuarray_run_profile_defaults <- function(run_profile = c("custom", "ci-fast", "nightly")) {
  run_profile <- match.arg(run_profile)
  switch(
    run_profile,
    "ci-fast" = list(
      repeats = 2L,
      profile = "ci-fast",
      quiet_fallback_warnings = TRUE,
      progress_every = 0L
    ),
    "nightly" = list(
      repeats = 8L,
      profile = "nightly",
      quiet_fallback_warnings = TRUE,
      progress_every = 2L
    ),
    list(
      repeats = 8L,
      profile = "closeout",
      quiet_fallback_warnings = TRUE,
      progress_every = 0L
    )
  )
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

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  args <- commandArgs(trailingOnly = TRUE)
  run_profile <- .mojor_parse_cli_arg(args, "run-profile", "custom")
  defaults <- .mojor_gpuarray_run_profile_defaults(run_profile = run_profile)

  repeats <- if (.mojor_has_cli_arg(args, "repeats")) {
    as.integer(.mojor_parse_cli_arg(args, "repeats", as.character(defaults$repeats)))
  } else {
    as.integer(defaults$repeats)
  }
  quiet_fallback_warnings <- if (.mojor_has_cli_arg(args, "quiet-fallback-warnings")) {
    .mojor_parse_bool_arg(args, "quiet-fallback-warnings", defaults$quiet_fallback_warnings)
  } else {
    isTRUE(defaults$quiet_fallback_warnings)
  }
  progress_every <- if (.mojor_has_cli_arg(args, "progress-every")) {
    as.integer(.mojor_parse_cli_arg(args, "progress-every", as.character(defaults$progress_every)))
  } else {
    as.integer(defaults$progress_every)
  }
  profile <- if (.mojor_has_cli_arg(args, "profile")) {
    .mojor_parse_cli_arg(args, "profile", defaults$profile)
  } else {
    defaults$profile
  }
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  write_reason_baseline <- .mojor_parse_cli_arg(args, "write-reason-baseline", "")
  reason_baseline_path <- .mojor_parse_cli_arg(args, "reason-baseline", "")
  th_cpu_arith <- .mojor_parse_cli_arg(args, "threshold-cpu-arith", "")
  th_cpu_kernel_host <- .mojor_parse_cli_arg(args, "threshold-cpu-kernel-host", "")
  th_cpu_reduce <- .mojor_parse_cli_arg(args, "threshold-cpu-reduce", "")
  thresholds <- mojor_gpuarray_math_route_thresholds(profile = profile)
  if (nzchar(th_cpu_arith)) {
    thresholds <- .mojor_gpuarray_set_threshold(
      thresholds,
      op = "__all__",
      route = "cpu_arith",
      value = as.numeric(th_cpu_arith)
    )
  }
  if (nzchar(th_cpu_kernel_host)) {
    thresholds <- .mojor_gpuarray_set_threshold(
      thresholds,
      op = "__all__",
      route = "cpu_kernel_host",
      value = as.numeric(th_cpu_kernel_host)
    )
  }
  if (nzchar(th_cpu_reduce)) {
    thresholds <- .mojor_gpuarray_set_threshold(
      thresholds,
      op = "__all__",
      route = "cpu_reduce",
      value = as.numeric(th_cpu_reduce)
    )
  }

  current_detail <- mojor_gpuarray_audit_math_routes(
    repeats = repeats,
    quiet_fallback_warnings = quiet_fallback_warnings,
    progress_every = progress_every,
    return_details = TRUE
  )
  current <- current_detail$routes
  current_reasons <- current_detail$reasons
  print(current)
  cat("\nMath reason counts:\n")
  reason_print <- current_reasons[current_reasons$count > 0L, , drop = FALSE]
  if (nrow(reason_print) == 0L) {
    reason_print <- current_reasons[
      current_reasons$op == "__all__" &
        current_reasons$route %in% c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
      ,
      drop = FALSE
    ]
  }
  print(reason_print)

  if (nzchar(write_baseline)) {
    dir.create(dirname(write_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current, write_baseline, row.names = FALSE)
    cat("Wrote math route baseline:", write_baseline, "\n")
  }
  if (nzchar(write_reason_baseline)) {
    dir.create(dirname(write_reason_baseline), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(current_reasons, write_reason_baseline, row.names = FALSE)
    cat("Wrote math reason baseline:", write_reason_baseline, "\n")
  }

  baseline <- mojor_gpuarray_load_math_route_baseline(baseline_path)
  reason_baseline <- mojor_gpuarray_load_math_reason_baseline(reason_baseline_path)
  gate <- mojor_gpuarray_math_route_gate(
    current,
    baseline,
    thresholds = thresholds
  )
  reason_gate <- mojor_gpuarray_math_reason_gate(
    current_reasons,
    reason_baseline,
    thresholds = mojor_gpuarray_math_reason_thresholds(profile = profile)
  )

  if (!is.null(gate$checks) && nrow(gate$checks) > 0) {
    cat("\nMath route gate profile:", profile, "\n")
    cat("Math route gate policy:", gate$note, "\n")
    cat("\nMath route gate:\n")
    print(gate$checks)
  }
  if (!isTRUE(gate$ok)) {
    stop("GPUArray math route gate failed")
  }
  if (!is.null(reason_gate$checks) && nrow(reason_gate$checks) > 0) {
    cat("\nMath reason gate:\n")
    print(reason_gate$checks)
  }
  if (!isTRUE(reason_gate$ok)) {
    stop("GPUArray math reason gate failed")
  }
}
