#!/usr/bin/env Rscript

.mojor_find_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- args[grepl("^--file=", args)]
  script_dir <- if (length(script_arg) > 0L) {
    dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }

  roots <- unique(c(script_dir, getwd()))
  for (root in roots) {
    cur <- normalizePath(root, winslash = "/", mustWork = FALSE)
    for (i in 0:8) {
      cand_check <- file.path(cur, "00_pkg_src", "mojor", "R", "mojor.R")
      if (file.exists(cand_check)) return(normalizePath(cand_check))
      cand <- file.path(cur, "packages", "mojor", "R", "mojor.R")
      if (file.exists(cand)) return(normalizePath(cand))
      cand_local <- file.path(cur, "R", "mojor.R")
      if (file.exists(cand_local)) return(normalizePath(cand_local))
      cur <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
    }
  }
  stop("could not locate packages/mojor/R/mojor.R (or local R/mojor.R) from current working directory")
}

.mojor_ensure_loaded <- function() {
  if (!exists(".mojor_ir_prepare_ssa_backend", mode = "function")) {
    mojor_path <- .mojor_find_root()
    pkg_root <- dirname(dirname(mojor_path))
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(pkg_root)
    source(mojor_path)
  }
}

.mojor_parse_int <- function(x, default) {
  val <- suppressWarnings(as.integer(x))
  if (!is.finite(val) || is.na(val)) return(as.integer(default))
  as.integer(val)
}

.mojor_parse_bool <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) return(isTRUE(default))
  lowered <- tolower(trimws(x))
  if (lowered %in% c("1", "true", "yes", "on")) return(TRUE)
  if (lowered %in% c("0", "false", "no", "off")) return(FALSE)
  isTRUE(default)
}

.mojor_perf_stats <- function(samples) {
  if (!is.numeric(samples) || length(samples) == 0L) {
    stop("samples must be a non-empty numeric vector")
  }
  mean_sec <- as.numeric(mean(samples))
  sd_sec <- if (length(samples) > 1L) as.numeric(stats::sd(samples)) else 0
  cv_pct <- if (is.finite(mean_sec) && mean_sec > 0) 100 * sd_sec / mean_sec else NA_real_
  list(
    median_sec = as.numeric(stats::median(samples)),
    mean_sec = mean_sec,
    sd_sec = sd_sec,
    p05_sec = as.numeric(stats::quantile(samples, probs = 0.05, names = FALSE, type = 7)),
    p95_sec = as.numeric(stats::quantile(samples, probs = 0.95, names = FALSE, type = 7)),
    min_sec = as.numeric(min(samples)),
    max_sec = as.numeric(max(samples)),
    cv_pct = as.numeric(cv_pct)
  )
}

.mojor_perf_bench <- function(fun, runs = 5L, warmup = 1L) {
  runs <- as.integer(runs)
  warmup <- as.integer(warmup)
  if (runs < 1L) stop("runs must be >= 1")
  if (warmup < 0L) stop("warmup must be >= 0")

  if (warmup > 0L) {
    for (i in seq_len(warmup)) invisible(fun())
  }

  samples <- numeric(runs)
  for (i in seq_len(runs)) {
    t0 <- proc.time()[["elapsed"]]
    invisible(fun())
    t1 <- proc.time()[["elapsed"]]
    samples[[i]] <- as.numeric(t1 - t0)
  }
  stats <- .mojor_perf_stats(samples)

  list(
    samples = samples,
    median_sec = stats$median_sec,
    mean_sec = stats$mean_sec,
    sd_sec = stats$sd_sec,
    p05_sec = stats$p05_sec,
    p95_sec = stats$p95_sec,
    min_sec = stats$min_sec,
    max_sec = stats$max_sec,
    cv_pct = stats$cv_pct
  )
}

.mojor_ir_workload_catalog <- function() {
  list(
    branch_if_vec = list(
      description = "branch-heavy vector loop",
      fn = function(x, n) {
        out <- numeric(n)
        for (i in seq_len(n)) {
          if (x[i] > 0) {
            out[i] <- x[i] + 1
          } else {
            out[i] <- x[i] - 1
          }
        }
        out
      },
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32"))
    ),
    affine_vec = list(
      description = "straight-line affine vector loop",
      fn = function(x, n) {
        out <- numeric(n)
        for (i in seq_len(n)) {
          out[i] <- 2 * x[i] + 3
        }
        out
      },
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32"))
    ),
    reduction_preloop = list(
      description = "pre-loop reduction followed by indexed loop",
      fn = function(x, n) {
        s <- 0
        for (i in seq_len(n)) {
          s <- s + x[i]
        }
        out <- numeric(n)
        for (i in seq_len(n)) {
          out[i] <- x[i] + s
        }
        out
      },
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32"))
    )
  )
}

.mojor_ir_select_workloads <- function(workload_set = c("core", "representative")) {
  workload_set <- match.arg(workload_set)
  catalog <- .mojor_ir_workload_catalog()
  if (identical(workload_set, "core")) {
    return(catalog["branch_if_vec"])
  }
  catalog
}

.mojor_ir_profile <- function(name, opt_level, ssa_passes, fusion_analysis) {
  list(
    name = as.character(name),
    opt_level = as.integer(opt_level),
    ssa_passes = as.character(ssa_passes),
    fusion_analysis = isTRUE(fusion_analysis)
  )
}

.mojor_ir_default_ssa_passes <- function() {
  c("prune_unreachable", "copy_propagate", "prune_dead_stmts")
}

.mojor_ir_profile_catalog <- function(ablation = FALSE) {
  base <- .mojor_ir_profile(
    name = "opt2_full",
    opt_level = 2L,
    ssa_passes = .mojor_ir_default_ssa_passes(),
    fusion_analysis = TRUE
  )
  if (!isTRUE(ablation)) {
    return(list(opt2_full = base))
  }
  list(
    opt2_full = base,
    no_hir_opt = .mojor_ir_profile(
      name = "no_hir_opt",
      opt_level = 0L,
      ssa_passes = .mojor_ir_default_ssa_passes(),
      fusion_analysis = TRUE
    ),
    no_ssa_passes = .mojor_ir_profile(
      name = "no_ssa_passes",
      opt_level = 2L,
      ssa_passes = character(0),
      fusion_analysis = TRUE
    ),
    no_fusion_analysis = .mojor_ir_profile(
      name = "no_fusion_analysis",
      opt_level = 2L,
      ssa_passes = .mojor_ir_default_ssa_passes(),
      fusion_analysis = FALSE
    )
  )
}

.mojor_ir_benchmark_tasks <- function(fn, layout_ctx, profile, include_e2e) {
  prep_seed <- .mojor_ir_prepare_ssa_backend(
    fn,
    layout_ctx = layout_ctx,
    opt_level = as.integer(profile$opt_level),
    ssa_passes = profile$ssa_passes,
    fusion_analysis = isTRUE(profile$fusion_analysis),
    fusion_rewrite = FALSE,
    verify = TRUE,
    enforce_typing_boundary = TRUE,
    instrument_passes = FALSE
  )
  ssa_text <- .mojor_ir_ssa_format(prep_seed$ssa$raw)
  ssa <- prep_seed$ssa$raw
  cfg <- prep_seed$backend_cfg

  tasks <- list(
    parse_lossless = function() .mojor_ssa_parse_lossless(ssa_text),
    verify_ssa = function() .mojor_ir_verify_ssa(ssa, strict_schema = FALSE),
    lower_backend = function() .mojor_ir_ssa_backend_lower(cfg, verify = FALSE)
  )
  if (isTRUE(include_e2e)) {
    tasks$prepare_backend_e2e <- function() {
      .mojor_ir_prepare_ssa_backend(
        fn,
        layout_ctx = layout_ctx,
        opt_level = as.integer(profile$opt_level),
        ssa_passes = profile$ssa_passes,
        fusion_analysis = isTRUE(profile$fusion_analysis),
        fusion_rewrite = FALSE,
        verify = TRUE,
        enforce_typing_boundary = TRUE,
        instrument_passes = FALSE
      )
    }
  }
  tasks
}

.mojor_ir_benchmark_profile_workload <- function(
  workload_name,
  workload,
  profile_name,
  profile,
  runs,
  warmup,
  include_e2e
) {
  tasks <- .mojor_ir_benchmark_tasks(
    fn = workload$fn,
    layout_ctx = workload$layout_ctx,
    profile = profile,
    include_e2e = include_e2e
  )

  rows <- vector("list", length(tasks))
  sample_map <- list()
  idx <- 1L
  for (metric_name in names(tasks)) {
    res <- .mojor_perf_bench(tasks[[metric_name]], runs = runs, warmup = warmup)
    sample_key <- paste(workload_name, profile_name, metric_name, sep = "::")
    sample_map[[sample_key]] <- res$samples
    rows[[idx]] <- data.frame(
      workload = workload_name,
      profile = profile_name,
      metric = metric_name,
      median_sec = as.numeric(res$median_sec),
      mean_sec = as.numeric(res$mean_sec),
      sd_sec = as.numeric(res$sd_sec),
      p05_sec = as.numeric(res$p05_sec),
      p95_sec = as.numeric(res$p95_sec),
      min_sec = as.numeric(res$min_sec),
      max_sec = as.numeric(res$max_sec),
      cv_pct = as.numeric(res$cv_pct),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
  list(rows = rows, samples = sample_map)
}

.mojor_ir_ablation_table <- function(metrics, baseline_profile = "opt2_full") {
  if (is.null(metrics) || nrow(metrics) == 0) return(data.frame())
  req_cols <- c("workload", "profile", "metric", "median_sec")
  if (!all(req_cols %in% names(metrics))) return(data.frame())

  baseline <- metrics[
    metrics$profile == baseline_profile,
    c("workload", "metric", "median_sec")
  ]
  if (nrow(baseline) == 0) return(data.frame())
  names(baseline)[names(baseline) == "median_sec"] <- "baseline_median_sec"

  current <- metrics[
    metrics$profile != baseline_profile,
    c("workload", "profile", "metric", "median_sec")
  ]
  if (nrow(current) == 0) return(data.frame())
  names(current)[names(current) == "median_sec"] <- "current_median_sec"

  merged <- merge(current, baseline, by = c("workload", "metric"), all.x = TRUE)
  if (nrow(merged) == 0) return(data.frame())

  merged$delta_sec <- merged$current_median_sec - merged$baseline_median_sec
  merged$delta_ratio <- ifelse(
    is.finite(merged$baseline_median_sec) & merged$baseline_median_sec > 0,
    merged$delta_sec / merged$baseline_median_sec,
    NA_real_
  )
  merged$delta_pct <- 100 * merged$delta_ratio
  merged <- merged[order(merged$workload, merged$metric, merged$profile), ]
  rownames(merged) <- NULL
  merged
}

mojor_ir_benchmark_pipeline <- function(runs = 5L, warmup = 1L, include_e2e = TRUE) {
  .mojor_ensure_loaded()
  workloads <- .mojor_ir_select_workloads("core")
  profile <- .mojor_ir_profile(
    name = "classic_opt0",
    opt_level = 0L,
    ssa_passes = .mojor_ir_default_ssa_passes(),
    fusion_analysis = TRUE
  )
  run <- .mojor_ir_benchmark_profile_workload(
    workload_name = names(workloads)[[1L]],
    workload = workloads[[1L]],
    profile_name = profile$name,
    profile = profile,
    runs = runs,
    warmup = warmup,
    include_e2e = include_e2e
  )
  out <- do.call(rbind, run$rows)
  out <- out[, c("metric", "median_sec", "mean_sec", "sd_sec", "p95_sec", "cv_pct")]
  rownames(out) <- NULL
  list(
    metrics = out,
    samples = run$samples,
    runs = as.integer(runs),
    warmup = as.integer(warmup)
  )
}

mojor_ir_benchmark_evidence <- function(
  runs = 20L,
  warmup = 3L,
  include_e2e = TRUE,
  workload_set = c("representative", "core"),
  ablation = TRUE
) {
  .mojor_ensure_loaded()
  workload_set <- match.arg(workload_set)
  workloads <- .mojor_ir_select_workloads(workload_set)
  profiles <- .mojor_ir_profile_catalog(ablation = ablation)

  all_rows <- list()
  sample_map <- list()
  row_idx <- 1L
  for (workload_name in names(workloads)) {
    for (profile_name in names(profiles)) {
      run <- .mojor_ir_benchmark_profile_workload(
        workload_name = workload_name,
        workload = workloads[[workload_name]],
        profile_name = profile_name,
        profile = profiles[[profile_name]],
        runs = runs,
        warmup = warmup,
        include_e2e = include_e2e
      )
      for (row in run$rows) {
        all_rows[[row_idx]] <- row
        row_idx <- row_idx + 1L
      }
      sample_map <- c(sample_map, run$samples)
    }
  }
  metrics <- if (length(all_rows) == 0L) data.frame() else do.call(rbind, all_rows)
  rownames(metrics) <- NULL
  ablation_tbl <- .mojor_ir_ablation_table(metrics, baseline_profile = "opt2_full")

  list(
    metrics = metrics,
    ablation = ablation_tbl,
    samples = sample_map,
    runs = as.integer(runs),
    warmup = as.integer(warmup),
    workload_set = workload_set,
    ablation_enabled = isTRUE(ablation)
  )
}

mojor_ir_load_perf_baseline <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  dat <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("metric", "median_sec")
  if (!all(req %in% names(dat))) {
    stop("baseline CSV must contain columns: metric, median_sec")
  }
  dat
}

mojor_ir_perf_gate <- function(
  current_metrics,
  baseline_metrics,
  thresholds = c(
    parse_lossless = 0.10,
    verify_ssa = 0.10,
    lower_backend = 0.15,
    prepare_backend_e2e = 0.20
  ),
  baseline_floor_sec = 1e-3
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
  mode <- .mojor_parse_cli_arg(args, "mode", "classic")
  runs <- .mojor_parse_int(.mojor_parse_cli_arg(args, "runs", "5"), 5L)
  warmup <- .mojor_parse_int(.mojor_parse_cli_arg(args, "warmup", "1"), 1L)
  baseline_path <- .mojor_parse_cli_arg(args, "baseline", "")
  write_baseline <- .mojor_parse_cli_arg(args, "write-baseline", "")
  workload_set <- .mojor_parse_cli_arg(args, "workload-set", "representative")
  ablation <- .mojor_parse_bool(.mojor_parse_cli_arg(args, "ablation", "1"), TRUE)
  write_metrics <- .mojor_parse_cli_arg(args, "write-metrics", "")
  write_ablation <- .mojor_parse_cli_arg(args, "write-ablation", "")

  if (!mode %in% c("classic", "evidence")) {
    stop("mode must be one of: classic, evidence")
  }

  if (identical(mode, "classic")) {
    bench <- mojor_ir_benchmark_pipeline(runs = runs, warmup = warmup, include_e2e = TRUE)
    print(bench$metrics, row.names = FALSE)

    baseline <- mojor_ir_load_perf_baseline(baseline_path)
    gate <- mojor_ir_perf_gate(bench$metrics, baseline)
    if (nrow(gate$checks) > 0) {
      cat("\nPerf gate:\n")
      print(gate$checks, row.names = FALSE)
    }
    if (!isTRUE(gate$ok)) {
      stop("IR perf benchmark gate failed")
    }

    if (nzchar(write_baseline)) {
      utils::write.csv(bench$metrics[, c("metric", "median_sec")], write_baseline, row.names = FALSE)
      cat("\nWrote baseline: ", write_baseline, "\n", sep = "")
    }
  } else {
    bench <- mojor_ir_benchmark_evidence(
      runs = runs,
      warmup = warmup,
      include_e2e = TRUE,
      workload_set = workload_set,
      ablation = ablation
    )
    print(bench$metrics, row.names = FALSE)
    if (nrow(bench$ablation) > 0) {
      cat("\nAblation deltas:\n")
      print(bench$ablation, row.names = FALSE)
    }
    if (nzchar(write_metrics)) {
      utils::write.csv(bench$metrics, write_metrics, row.names = FALSE)
      cat("\nWrote evidence metrics: ", write_metrics, "\n", sep = "")
    }
    if (nzchar(write_ablation)) {
      utils::write.csv(bench$ablation, write_ablation, row.names = FALSE)
      cat("\nWrote ablation metrics: ", write_ablation, "\n", sep = "")
    }
  }
}
