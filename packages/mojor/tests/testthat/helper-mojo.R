find_mojor <- function() {
  cur <- getwd()
  for (i in 0:6) {
    cand <- file.path(cur, "packages", "mojor", "R", "mojor.R")
    if (file.exists(cand)) return(normalizePath(cand))
    cand_check <- file.path(cur, "00_pkg_src", "mojor", "R", "mojor.R")
    if (file.exists(cand_check)) return(normalizePath(cand_check))
    cand_local <- file.path(cur, "R", "mojor.R")
    if (file.exists(cand_local)) return(normalizePath(cand_local))
    cur <- normalizePath(file.path(cur, ".."))
  }
  pkg_dir <- tryCatch(system.file(package = "mojor"), error = function(e) "")
  if (is.character(pkg_dir) && length(pkg_dir) == 1L && nzchar(pkg_dir)) {
    cand_inst <- file.path(pkg_dir, "R", "mojor.R")
    if (file.exists(cand_inst)) return(normalizePath(cand_inst))
    cand_loader <- file.path(pkg_dir, "R", "mojor")
    if (file.exists(cand_loader)) return(normalizePath(cand_loader))
  }
  NULL
}

.mojor_test_bind_namespace_objects <- function() {
  if (!requireNamespace("mojor", quietly = TRUE)) {
    stop("package 'mojor' must be installed or loadable for tests")
  }
  ns <- asNamespace("mojor")
  symbols <- ls(ns, all.names = TRUE)
  symbols <- symbols[grepl("^\\.mojor|^mojor_|^result_|^with_cleanup$", symbols)]
  for (name in symbols) {
    value <- tryCatch(get(name, envir = ns, inherits = FALSE), error = function(e) NULL)
    if (is.null(value)) {
      next
    }
    assign(name, value, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

.mojor_test_env_truthy <- function(name) {
  tolower(Sys.getenv(name, unset = "")) %in% c("1", "true", "yes", "y", "on")
}

.mojor_skip_gpu_runtime_file_unless_opt_in <- function() {
  if (.mojor_test_env_truthy("MOJOR_TEST_RUN_GPU_RUNTIME_TESTS")) {
    return(invisible(NULL))
  }
  skip("set MOJOR_TEST_RUN_GPU_RUNTIME_TESTS=1 to run test_gpu* runtime files")
}

.mojor_test_source_once <- function() {
  mojor_path <- find_mojor()
  stamp_name <- ".__MOJOR_TEST_SOURCE_ONCE__"
  force_reload <- .mojor_test_env_truthy("MOJOR_TEST_FORCE_RELOAD_SOURCE")
  has_stamp <- exists(stamp_name, envir = .GlobalEnv, inherits = FALSE)
  need_source <- isTRUE(force_reload) ||
    !isTRUE(has_stamp) ||
    !exists("mojor_build", mode = "function", inherits = TRUE)
  if (isTRUE(need_source)) {
    if (!is.null(mojor_path) &&
        is.character(mojor_path) &&
        nzchar(mojor_path) &&
        identical(basename(mojor_path), "mojor.R")) {
      old_wd <- getwd()
      pkg_root <- dirname(dirname(mojor_path))
      on.exit(setwd(old_wd), add = TRUE)
      if (dir.exists(pkg_root)) {
        setwd(pkg_root)
      }
      source_ok <- tryCatch({
        source(mojor_path)
        TRUE
      }, error = function(e) {
        FALSE
      })
      if (isTRUE(source_ok)) {
        assign(stamp_name, normalizePath(mojor_path, winslash = "/"), envir = .GlobalEnv)
      } else {
        .mojor_test_bind_namespace_objects()
        assign(stamp_name, "<namespace:mojor>", envir = .GlobalEnv)
      }
    } else {
      .mojor_test_bind_namespace_objects()
      assign(stamp_name, "<namespace:mojor>", envir = .GlobalEnv)
    }
  }
  invisible(TRUE)
}

.mojor_test_source_once()

.mojor_test_local_options <- function(..., .local_envir = parent.frame()) {
  opts <- list(...)
  if (length(opts) == 0) return(invisible(NULL))
  old <- mojor_options(names(opts))
  do.call(mojor_options, opts)
  withr::defer(do.call(mojor_options, old), envir = .local_envir)
  invisible(old)
}

.mojor_test_local_state <- function(name, value, envir = .mojor_state, .local_envir = parent.frame()) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop(".mojor_test_local_state: name must be a non-empty string")
  }
  had <- exists(name, envir = envir, inherits = FALSE)
  old <- if (had) get(name, envir = envir, inherits = FALSE) else NULL
  assign(name, value, envir = envir)
  withr::defer({
    if (had) {
      assign(name, old, envir = envir)
    } else if (exists(name, envir = envir, inherits = FALSE)) {
      rm(list = name, envir = envir)
    }
  }, envir = .local_envir)
  invisible(old)
}

.mojor_test_local_env <- function(name, value = NULL, .local_envir = parent.frame()) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop(".mojor_test_local_env: name must be a non-empty string")
  }
  old <- Sys.getenv(name, unset = NA_character_)
  if (is.null(value)) {
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, stats::setNames(list(as.character(value)), name))
  }
  withr::defer({
    if (is.na(old)) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, stats::setNames(list(old), name))
    }
  }, envir = .local_envir)
  invisible(old)
}

.mojor_testthat_base <- testthat::test_that

.mojor_test_baseline_bindings <- local({
  nm <- c(
    "mojor_has_gpu",
    "mojor_is_loaded",
    ".mojor_gpu_ctx_get",
    ".mojor_runtime_dispatch_call",
    ".mojor_bridge_pkg_for_symbol"
  )
  out <- list()
  for (name in nm) {
    if (exists(name, mode = "function", inherits = TRUE)) {
      out[[name]] <- get(name, mode = "function", inherits = TRUE)
    }
  }
  out
})

.mojor_test_restore_core_bindings <- function() {
  if (!is.list(.mojor_test_baseline_bindings) || length(.mojor_test_baseline_bindings) == 0L) {
    return(invisible(FALSE))
  }
  for (name in names(.mojor_test_baseline_bindings)) {
    binding <- .mojor_test_baseline_bindings[[name]]
    if (is.function(binding)) {
      assign(name, binding, envir = .GlobalEnv)
    }
  }
  invisible(TRUE)
}

.mojor_test_reset_mode <- function() {
  mode <- tolower(Sys.getenv("MOJOR_TEST_RESET_MODE", unset = "soft"))
  if (!mode %in% c("soft", "hard")) mode <- "hard"
  mode
}

.mojor_test_reset_keep_backend <- function() {
  flag <- tolower(Sys.getenv("MOJOR_TEST_KEEP_BACKEND", unset = "true"))
  flag %in% c("1", "true", "yes", "y", "on")
}

.mojor_test_file_reset_mode <- function() {
  mode <- tolower(Sys.getenv("MOJOR_TEST_FILE_RESET_MODE", unset = "soft"))
  if (!mode %in% c("off", "none", "soft", "hard")) mode <- "hard"
  mode
}

.mojor_test_file_reset_enabled <- function(mode) {
  !(mode %in% c("off", "none"))
}

.mojor_test_force_gc <- function() {
  flag <- tolower(Sys.getenv("MOJOR_TEST_FORCE_GC", unset = "false"))
  flag %in% c("1", "true", "yes", "y", "on")
}

.mojor_test_stress_opt_level <- function() {
  raw <- Sys.getenv("MOJOR_TEST_STRESS_OPT_LEVEL", unset = "")
  if (!nzchar(raw)) return(NULL)
  lvl <- suppressWarnings(as.integer(raw))
  if (is.na(lvl)) return(NULL)
  lvl
}

.mojor_test_per_test_reset_enabled <- function() {
  flag <- tolower(Sys.getenv("MOJOR_TEST_PER_TEST_RESET", unset = "true"))
  flag %in% c("1", "true", "yes", "y", "on")
}

{
  # Optional guard for repeated same-process stress lanes where R bytecode JIT
  # can destabilize long testthat runs.
  if (.mojor_test_env_truthy("MOJOR_TEST_DISABLE_R_JIT") &&
      requireNamespace("compiler", quietly = TRUE)) {
    suppressWarnings(compiler::enableJIT(0L))
  }

  # File-boundary reset prevents cross-file state accumulation in same-process
  # stress lanes while keeping per-test reset mode configurable.
  file_reset_mode <- .mojor_test_file_reset_mode()
  if (.mojor_test_file_reset_enabled(file_reset_mode)) {
    .mojor_test_reset(
      mode = file_reset_mode,
      keep_backend = .mojor_test_reset_keep_backend()
    )
  }
}

test_that <- function(desc, code) {
  reset_mode <- .mojor_test_reset_mode()
  keep_backend <- .mojor_test_reset_keep_backend()
  force_gc <- .mojor_test_force_gc()
  stress_opt_level <- .mojor_test_stress_opt_level()
  per_test_reset <- .mojor_test_per_test_reset_enabled()
  desc_label <- as.character(desc)
  if (length(desc_label) != 1L || is.na(desc_label)) {
    desc_label <- "<unnamed test>"
  }
  is_golden_test <- grepl("^golden(\\b|:)", tolower(desc_label), perl = TRUE)
  code_expr <- substitute(code)
  call_expr <- substitute(
    .mojor_testthat_base(DESC, {
      if (.mojor_test_env_truthy("MOJOR_TEST_TRACE_GPU_LIVE") &&
          exists("mojor_gpu_buf_f32_live_count", mode = "function", inherits = TRUE)) {
        live <- tryCatch(mojor_gpu_buf_f32_live_count(), error = function(e) NA_integer_)
        cat(sprintf("MOJOR_TEST_GPU_LIVE: %s :: %s\n", as.character(live[[1L]]), DESC_LABEL))
      }
      if (.mojor_env_truthy("MOJOR_TEST_TRACE_TESTS")) {
        cat(sprintf("MOJOR_TEST_BEGIN: %s\n", DESC_LABEL))
      }
      if (isTRUE(FORCE_GC)) {
        invisible(gc(verbose = FALSE))
      }
      if (isTRUE(PER_TEST_RESET)) {
        .mojor_test_restore_core_bindings()
        .mojor_test_reset(mode = RESET_MODE, keep_backend = KEEP_BACKEND)
        if (exists(".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv, inherits = FALSE)) {
          rm(list = ".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv)
        }
      }
      if (isTRUE(FORCE_GC)) {
        on.exit({
          invisible(gc(verbose = FALSE))
        }, add = TRUE)
      }
      if (isTRUE(PER_TEST_RESET) &&
          exists("mojor_gpu_buf_f32_live_count", mode = "function", inherits = TRUE)) {
        on.exit({
          live_after <- tryCatch(mojor_gpu_buf_f32_live_count(), error = function(e) NA_integer_)
          live_after <- suppressWarnings(as.integer(live_after[[1L]]))
          if (!is.na(live_after) && live_after > 0L) {
            invisible(gc(verbose = FALSE))
            live_after_gc <- tryCatch(mojor_gpu_buf_f32_live_count(), error = function(e) NA_integer_)
            live_after_gc <- suppressWarnings(as.integer(live_after_gc[[1L]]))
            if (!is.na(live_after_gc) && live_after_gc > 0L) {
              invisible(gc(verbose = FALSE))
            }
          }
        }, add = TRUE)
      }
      if (.mojor_env_truthy("MOJOR_TEST_DIAG_INDEX")) {
        # Diagnostic lane: maximize fail-fast bounds checks and disable aggressive optimization.
        .mojor_test_local_options(index_bounds = TRUE, opt_level = 0L)
      } else if (!is.null(STRESS_OPT_LEVEL) && !isTRUE(IS_GOLDEN_TEST)) {
        # Stress lane: allow deterministic opt-level override without changing
        # package defaults.
        .mojor_test_local_options(opt_level = STRESS_OPT_LEVEL)
      }
      CODE
    }),
    list(
      DESC = desc,
      CODE = code_expr,
      DESC_LABEL = desc_label,
      FORCE_GC = force_gc,
      PER_TEST_RESET = per_test_reset,
      IS_GOLDEN_TEST = is_golden_test,
      RESET_MODE = reset_mode,
      KEEP_BACKEND = keep_backend,
      STRESS_OPT_LEVEL = stress_opt_level
    )
  )
  eval(call_expr, envir = parent.frame())
}

.mojor_has_mojo <- function() {
  nzchar(Sys.which("mojo"))
}

.mojor_find_mojo_bin <- function() {
  bin <- Sys.which("mojo")
  if (nzchar(bin)) return(bin)
  alt_bin <- "/opt/anaconda3/bin/mojo"
  if (file.exists(alt_bin)) return(alt_bin)
  ""
}

.mojor_test_pkg_root <- function() {
  mojor_path <- find_mojor()
  if (!is.null(mojor_path) && is.character(mojor_path) && nzchar(mojor_path)) {
    return(dirname(dirname(mojor_path)))
  }
  pkg_dir <- tryCatch(system.file(package = "mojor"), error = function(e) "")
  if (is.character(pkg_dir) && length(pkg_dir) == 1L && nzchar(pkg_dir)) {
    return(pkg_dir)
  }
  NULL
}

.mojor_try_load_backend <- function() {
  if (exists("mojor_is_loaded", mode = "function", inherits = TRUE) &&
      isTRUE(tryCatch(mojor_is_loaded(), error = function(e) FALSE))) {
    return(invisible(TRUE))
  }
  pkg_root <- .mojor_test_pkg_root()
  candidate_dirs <- character(0)
  if (!is.null(pkg_root)) {
    candidate_dirs <- c(candidate_dirs, file.path(pkg_root, "build"), file.path(pkg_root, "libs"))
    check_pkg <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")
    if (is.character(check_pkg) && nzchar(check_pkg)) {
      candidate_dirs <- c(
        candidate_dirs,
        file.path(dirname(dirname(pkg_root)), check_pkg, "libs")
      )
    }
  }
  candidate_dirs <- unique(candidate_dirs)
  for (build_dir in candidate_dirs) {
    bridge <- file.path(build_dir, "mojor_bridge.so")
    if (!file.exists(bridge)) {
      next
    }
    Sys.setenv(MOJOR_LIB_PATH = build_dir)
    load_res <- try(mojor_load(build_dir), silent = TRUE)
    if (!inherits(load_res, "try-error") &&
        isTRUE(tryCatch(mojor_is_loaded(), error = function(e) FALSE))) {
      return(invisible(TRUE))
    }
  }
  load_default <- try(mojor_load(), silent = TRUE)
  if (!inherits(load_default, "try-error") &&
      isTRUE(tryCatch(mojor_is_loaded(), error = function(e) FALSE))) {
    return(invisible(TRUE))
  }
  FALSE
}

.mojor_try_build_backend <- function() {
  if (identical(Sys.getenv("MOJOR_TEST_SKIP_BUILD"), "1")) {
    return(FALSE)
  }
  if (!.mojor_has_mojo()) return(FALSE)
  pkg_root <- .mojor_test_pkg_root()
  if (is.null(pkg_root)) {
    return(FALSE)
  }
  build_sh <- file.path(pkg_root, "build.sh")
  if (!file.exists(build_sh)) {
    build_sh <- file.path(pkg_root, "tools", "build_backend.sh")
  }
  if (!file.exists(build_sh)) return(FALSE)
  status <- tryCatch({
    system2("bash", build_sh, stdout = TRUE, stderr = TRUE)
    0L
  }, error = function(e) {
    message("mojor tests: build failed: ", conditionMessage(e))
    1L
  })
  status == 0L
}

if (!.mojor_try_load_backend()) {
  if (.mojor_try_build_backend()) {
    .mojor_try_load_backend()
  }
}

.mojor_backend_checked <- FALSE

skip_if_no_mojo <- function() {
  if (!.mojor_has_mojo()) {
    testthat::skip("mojo compiler not available")
  }
  loaded <- isTRUE(.mojor_try_load_backend())
  if (!loaded && !isTRUE(.mojor_backend_checked)) {
    .mojor_backend_checked <<- TRUE
    if (.mojor_try_build_backend()) {
      loaded <- isTRUE(.mojor_try_load_backend())
    }
  }
  if (!loaded) {
    testthat::skip("Mojo backend not loaded")
  }
}

.mojor_env_truthy <- function(name) {
  .mojor_test_env_truthy(name)
}

.mojor_gpu_f64_available <- NULL
.mojor_gpu_f64_reason <- NULL

.mojor_probe_gpu_f64 <- function() {
  if (!mojor_is_loaded() || !mojor_has_gpu()) {
    return(FALSE)
  }
  ok <- tryCatch({
    buf <- mojor_gpu_buf_f64(c(1, 2, 3))
    on.exit(try(mojor_gpu_buf_f64_free(buf), silent = TRUE), add = TRUE)
    vals <- mojor_gpu_buf_f64_read(buf)
    is.numeric(vals) && length(vals) == 3L
  }, error = function(e) {
    .mojor_gpu_f64_reason <<- conditionMessage(e)
    FALSE
  })
  if (isTRUE(ok)) {
    .mojor_gpu_f64_reason <<- NULL
  }
  ok
}

skip_if_no_gpu_f64 <- function() {
  skip_if_no_mojo()
  if (!.mojor_env_truthy("MOJOR_TEST_GPU_F64")) {
    testthat::skip("set MOJOR_TEST_GPU_F64=1 to run f64 GPU tests")
  }
  if (!mojor_has_gpu()) {
    testthat::skip("GPU not available")
  }
  if (is.null(.mojor_gpu_f64_available)) {
    .mojor_gpu_f64_available <<- isTRUE(.mojor_probe_gpu_f64())
  }
  if (!isTRUE(.mojor_gpu_f64_available)) {
    reason <- .mojor_gpu_f64_reason
    if (is.null(reason) || !nzchar(reason)) {
      reason <- "backend does not support f64 GPU buffers"
    }
    testthat::skip(paste0("f64 GPU unavailable: ", reason))
  }
}
