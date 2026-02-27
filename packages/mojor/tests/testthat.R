library(testthat)
library(mojor)

has_file_failures <- function(results) {
  if (!inherits(results, "testthat_results") || length(results) == 0L) {
    return(FALSE)
  }
  entries <- unlist(
    lapply(results, function(x) {
      if (is.list(x) && is.list(x$results)) x$results else list()
    }),
    recursive = FALSE
  )
  if (length(entries) == 0L) {
    return(FALSE)
  }
  any(vapply(
    entries,
    function(x) {
      inherits(x, "expectation_failure") || inherits(x, "expectation_error")
    },
    logical(1)
  ))
}

run_test_file_subprocess <- function(file, reporter = "summary") {
  r_home <- Sys.getenv("R_HOME", unset = "")
  r_bin <- if (nzchar(r_home))
    file.path(r_home, "bin", "Rscript") else ""
  if (!nzchar(r_bin) || !file.exists(r_bin)) {
    r_bin <- Sys.which("Rscript")
  }
  if (nzchar(r_bin) && grepl("/", r_bin, fixed = TRUE)) {
    r_bin <- normalizePath(r_bin, winslash = "/", mustWork = FALSE)
  }
  if (!nzchar(r_bin)) {
    stop("Rscript not found on PATH for subprocess test run.")
  }
  expr <- sprintf(
    "testthat::test_file(%s, reporter = %s, stop_on_failure = TRUE, stop_on_warning = FALSE)",
    dQuote(normalizePath(file, winslash = "/", mustWork = TRUE)),
    dQuote(reporter)
  )
  out <- tryCatch(
    system2(r_bin, c("-e", shQuote(expr)), stdout = TRUE, stderr = TRUE),
    error = function(e) structure(
      sprintf("system2 error while running %s: %s", basename(file), conditionMessage(e)),
      status = 127L
    )
  )
  log_dir <- Sys.getenv("MOJOR_TEST_SUBPROCESS_LOG_DIR", unset = "")
  if (nzchar(log_dir)) {
    ok <- tryCatch({
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok) && dir.exists(log_dir)) {
      stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      stem <- gsub("[^A-Za-z0-9._-]+", "_", basename(file))
      log_path <- file.path(log_dir, paste0(stamp, "-", Sys.getpid(), "-", stem, ".log"))
      try(writeLines(out, con = log_path, useBytes = TRUE), silent = TRUE)
    } else {
      log_path <- ""
    }
  } else {
    log_path <- ""
  }
  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0L
  }
  if (!identical(as.integer(status), 0L)) {
    cat(sprintf(
      "MOJOR_TEST_SUBPROCESS_FAIL: file=%s reporter=%s status=%d%s\n",
      basename(file),
      reporter,
      as.integer(status),
      if (nzchar(log_path)) paste0(" log=", log_path) else ""
    ))
  }
  if (length(out) > 0L) {
    cat(paste(out, collapse = "\n"), "\n")
  }
  as.integer(status)
}

.mojor_test_env_truthy <- function(name) {
  tolower(Sys.getenv(name, unset = "")) %in% c("1", "true", "yes", "y", "on")
}

.mojor_test_env_int <- function(name, default, min_value = 0L) {
  raw <- Sys.getenv(name, unset = as.character(default))
  val <- suppressWarnings(as.integer(raw))
  if (!is.finite(val) || is.na(val) || val < min_value) {
    return(as.integer(default))
  }
  as.integer(val)
}

.mojor_test_subprocess_reporter <- function(default = "summary") {
  reporter <- trimws(Sys.getenv("MOJOR_TEST_SUBPROCESS_REPORTER", unset = default))
  if (!nzchar(reporter)) {
    reporter <- default
  }
  reporter
}

.mojor_test_leak_guard_mode <- function() {
  mode <- tolower(Sys.getenv("MOJOR_TEST_LEAK_GUARD", unset = "off"))
  if (!mode %in% c("off", "warn", "fail")) {
    mode <- "off"
  }
  mode
}

.mojor_test_snapshot_metric <- function(snapshot, name) {
  if (!is.list(snapshot) || is.null(snapshot[[name]])) {
    return(0L)
  }
  val <- suppressWarnings(as.integer(snapshot[[name]][[1L]]))
  if (!is.finite(val) || is.na(val)) {
    return(0L)
  }
  as.integer(val)
}

.mojor_test_leak_guard_init <- function() {
  mode <- .mojor_test_leak_guard_mode()
  enabled <- !identical(mode, "off") &&
    exists(".mojor_wrapper_leak_snapshot", mode = "function", inherits = TRUE) &&
    exists(".mojor_wrapper_leak_delta", mode = "function", inherits = TRUE)
  state <- new.env(parent = emptyenv())
  state$enabled <- enabled
  state$mode <- mode
  if (!isTRUE(enabled)) {
    return(state)
  }
  state$max_wrapper_delta <- .mojor_test_env_int("MOJOR_TEST_LEAK_MAX_WRAPPER_DELTA", 16L, 0L)
  state$max_dll_delta <- .mojor_test_env_int("MOJOR_TEST_LEAK_MAX_DLL_DELTA", 8L, 0L)
  state$max_tracked_delta <- .mojor_test_env_int("MOJOR_TEST_LEAK_MAX_TRACKED_DLL_DELTA", 4L, 0L)
  state$streak_limit <- .mojor_test_env_int("MOJOR_TEST_LEAK_STREAK", 2L, 1L)
  state$trace <- .mojor_test_env_truthy("MOJOR_TEST_LEAK_TRACE")
  state$streak <- 0L
  state$baseline <- .mojor_wrapper_leak_snapshot(prune = TRUE)
  state
}

.mojor_test_leak_guard_update <- function(state, file, phase = "post_reset") {
  if (!is.environment(state) || !isTRUE(state$enabled)) {
    return(invisible(FALSE))
  }
  snap <- .mojor_wrapper_leak_snapshot(prune = TRUE)
  delta <- .mojor_wrapper_leak_delta(snap, state$baseline)
  wrapper_delta <- .mojor_test_snapshot_metric(delta, "registered_unique_paths")
  dll_delta <- .mojor_test_snapshot_metric(delta, "loaded_dll_count")
  tracked_delta <- .mojor_test_snapshot_metric(delta, "tracked_loaded_dll_count")
  breach <- wrapper_delta > state$max_wrapper_delta ||
    dll_delta > state$max_dll_delta ||
    tracked_delta > state$max_tracked_delta

  if (isTRUE(state$trace)) {
    cat(sprintf(
      "MOJOR_TEST_LEAK_TRACE: %s %s wrappers=%d tracked=%d dlls=%d delta(w=%+d,t=%+d,d=%+d)\n",
      basename(file),
      phase,
      .mojor_test_snapshot_metric(snap, "registered_unique_paths"),
      .mojor_test_snapshot_metric(snap, "tracked_loaded_dll_count"),
      .mojor_test_snapshot_metric(snap, "loaded_dll_count"),
      wrapper_delta,
      tracked_delta,
      dll_delta
    ))
  }

  if (!isTRUE(breach)) {
    state$baseline$registered_unique_paths <- min(
      .mojor_test_snapshot_metric(state$baseline, "registered_unique_paths"),
      .mojor_test_snapshot_metric(snap, "registered_unique_paths")
    )
    state$baseline$loaded_dll_count <- min(
      .mojor_test_snapshot_metric(state$baseline, "loaded_dll_count"),
      .mojor_test_snapshot_metric(snap, "loaded_dll_count")
    )
    state$baseline$tracked_loaded_dll_count <- min(
      .mojor_test_snapshot_metric(state$baseline, "tracked_loaded_dll_count"),
      .mojor_test_snapshot_metric(snap, "tracked_loaded_dll_count")
    )
    state$streak <- 0L
    return(invisible(TRUE))
  }

  state$streak <- state$streak + 1L
  msg <- sprintf(
    paste(
      "MOJOR_TEST_LEAK_GUARD: %s %s delta(w=%+d,t=%+d,d=%+d)",
      "limits(w=%d,t=%d,d=%d) streak=%d/%d mode=%s"
    ),
    basename(file),
    phase,
    wrapper_delta,
    tracked_delta,
    dll_delta,
    state$max_wrapper_delta,
    state$max_tracked_delta,
    state$max_dll_delta,
    state$streak,
    state$streak_limit,
    state$mode
  )
  if (identical(state$mode, "fail") && state$streak >= state$streak_limit) {
    stop(msg, call. = FALSE)
  }
  message(msg)
  invisible(FALSE)
}

run_test_files_sequential <- function(package) {
  test_dir <- "testthat"
  if (!dir.exists(test_dir)) {
    test_dir <- system.file("tests", "testthat", package = package)
  }
  if (!nzchar(test_dir) || !dir.exists(test_dir)) {
    stop("Could not locate tests/testthat directory.")
  }
  files <- sort(list.files(test_dir, pattern = "^test_.*[.]R$", full.names = TRUE))
  in_pkg_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = ""))
  if (isTRUE(in_pkg_check)) {
    files <- files[!grepl("_script\\.R$", basename(files))]
  }
  test_files_raw <- Sys.getenv("MOJOR_TEST_FILES", unset = "")
  if (nzchar(test_files_raw)) {
    requested <- trimws(unlist(strsplit(test_files_raw, ",", fixed = TRUE)))
    requested <- basename(requested[nzchar(requested)])
    requested <- unique(requested)
    if (length(requested) == 0L) {
      stop("MOJOR_TEST_FILES is set but no valid file names were provided.")
    }
    file_by_name <- setNames(files, basename(files))
    selected <- unname(file_by_name[requested])
    selected <- selected[!is.na(selected)]
    missing <- setdiff(requested, basename(selected))
    if (length(missing) > 0L) {
      warning(
        "MOJOR_TEST_FILES entries not found: ",
        paste(missing, collapse = ", ")
      )
    }
    files <- unique(selected)
    message(
      sprintf(
        "MOJOR_TEST_FILES: running %d selected file(s).",
        length(files)
      )
    )
  }
  if (length(files) == 0L) {
    message("No test files found under tests/testthat.")
    return(invisible(TRUE))
  }

  had_failures <- FALSE
  subprocess_files <- c(
    "test_gpu_math_route_audit.R",
    "test_indexing_comprehensive.R",
    "test_mojor_fn_expression_fallback.R",
    "test_negative_indices.R",
    "test_type_promotion.R",
    "test_type_runtime.R"
  )
  subprocess_from <- suppressWarnings(
    as.integer(Sys.getenv("MOJOR_TEST_SUBPROCESS_FROM_INDEX", unset = "130"))
  )
  if (!nzchar(Sys.getenv("MOJOR_TEST_SUBPROCESS_FROM_INDEX", unset = "")) &&
      isTRUE(in_pkg_check)) {
    subprocess_from <- 1L
  }
  if (!is.finite(subprocess_from) || is.na(subprocess_from) || subprocess_from < 1L) {
    subprocess_from <- .Machine$integer.max
  }
  leak_guard <- .mojor_test_leak_guard_init()
  for (i in seq_along(files)) {
    file <- files[[i]]
    if (exists(".mojor_test_reset", mode = "function", inherits = TRUE)) {
      try(.mojor_test_reset(mode = "hard", keep_backend = TRUE), silent = TRUE)
      invisible(gc(verbose = FALSE))
    }
    .mojor_test_leak_guard_update(leak_guard, file, phase = "post_reset")
    is_subprocess <- (basename(file) %in% subprocess_files) || (i >= subprocess_from)
    cat(sprintf(
      "[%03d/%03d] %s%s\n",
      i,
      length(files),
      basename(file),
      if (is_subprocess) " (subprocess)" else ""
    ))
    if (is_subprocess) {
      status <- run_test_file_subprocess(
        file,
        reporter = .mojor_test_subprocess_reporter(default = "summary")
      )
      if (!identical(status, 0L)) {
        had_failures <- TRUE
      }
    } else {
      result <- test_file(file, reporter = "summary")
      if (has_file_failures(result)) {
        had_failures <- TRUE
      }
    }
  }
  if (isTRUE(had_failures)) {
    stop("One or more test files reported failures.")
  }
  invisible(TRUE)
}

filter <- Sys.getenv("MOJOR_TEST_FILTER", unset = "")
if (nzchar(filter)) {
  tryCatch(
    test_check("mojor", filter = filter),
    error = function(e) {
      if (grepl("No test files found", conditionMessage(e), fixed = TRUE)) {
        message("MOJOR_TEST_FILTER matched no tests; running full test suite.")
        test_check("mojor")
      } else {
        stop(e)
      }
    }
  )
} else {
  run_test_files_sequential("mojor")
}
