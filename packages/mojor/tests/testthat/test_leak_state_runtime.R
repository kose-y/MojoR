library(testthat)

.mojor_test_r_binary <- function() {
  path <- file.path(R.home("bin"), "R")
  if (file.exists(path)) {
    return(path)
  }
  "R"
}

.mojor_compile_dummy_call <- function(symbol, value, dir) {
  c_path <- file.path(dir, paste0(symbol, ".c"))
  so_path <- file.path(dir, paste0(symbol, ".so"))
  code <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "",
    paste0("SEXP ", symbol, "(SEXP x) {"),
    paste0("  return Rf_ScalarReal(", format(value, scientific = FALSE), ");"),
    "}"
  )
  writeLines(code, c_path)
  out <- suppressWarnings(system2(
    .mojor_test_r_binary(),
    c("CMD", "SHLIB", "-o", so_path, c_path),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  if (!identical(status, 0L) || !file.exists(so_path)) {
    stop("failed to compile dummy wrapper: ", paste(out, collapse = "\n"))
  }
  so_path
}

test_that(".mojor_test_reset soft restores baseline options/env and scratch state", {  .mojor_configure_test_baseline(
    options = list(warn_parallel = FALSE, warn_constructor = FALSE),
    env = list(MOJOR_PARALLEL_RUNTIME = "thread")
  )
  on.exit(
    .mojor_configure_test_baseline(
      options = list(warn_parallel = FALSE, warn_constructor = FALSE),
      env = list()
    ),
    add = TRUE
  )

  mojor_options(warn_parallel = TRUE, warn_constructor = TRUE, simd_mode = "auto")
  .mojor_state$declared_c_functions <- list(foo = list(name = "foo"))
  .mojor_state$diagnostics <- list("x")
  Sys.setenv(MOJOR_PARALLEL_RUNTIME = "serial")

  .mojor_test_reset(mode = "soft", keep_backend = TRUE)

  expect_false(isTRUE(mojor_options("warn_parallel")[[1]]))
  expect_false(isTRUE(mojor_options("warn_constructor")[[1]]))
  expect_equal(.mojor_state$declared_c_functions, list())
  expect_equal(length(.mojor_state$diagnostics), 0L)
  expect_identical(Sys.getenv("MOJOR_PARALLEL_RUNTIME", unset = NA_character_), "thread")
})

test_that(".mojor_test_reset hard clears wrapper registry", {  .mojor_state$loaded_wrapper_dlls <- list(
    list(pkg = "fake_pkg", path = tempfile(fileext = ".so"), kind = "expression")
  )
  .mojor_test_reset(mode = "hard", keep_backend = TRUE)
  expect_equal(.mojor_state$loaded_wrapper_dlls, list())
})

test_that(".mojor_test_reset soft does not auto-unload wrappers by default", {
  old_unload <- .mojor_unload_tracked_wrappers
  called <- 0L
  assign(
    ".mojor_unload_tracked_wrappers",
    function() {
      called <<- called + 1L
      character(0)
    },
    envir = .GlobalEnv
  )
  on.exit(
    assign(".mojor_unload_tracked_wrappers", old_unload, envir = .GlobalEnv),
    add = TRUE
  )

  Sys.unsetenv("MOJOR_SOFT_RESET_AGGRESSIVE_UNLOAD")
  .mojor_state$loaded_wrapper_dlls <- replicate(
    256L,
    list(pkg = "fake_pkg", path = tempfile(fileext = ".so"), kind = "expression"),
    simplify = FALSE
  )

  .mojor_test_reset(mode = "soft", keep_backend = TRUE)
  expect_identical(called, 0L)
})

test_that(".mojor_test_reset soft supports opt-in aggressive unload", {
  old_unload <- .mojor_unload_tracked_wrappers
  called <- 0L
  assign(
    ".mojor_unload_tracked_wrappers",
    function() {
      called <<- called + 1L
      character(0)
    },
    envir = .GlobalEnv
  )
  on.exit(
    assign(".mojor_unload_tracked_wrappers", old_unload, envir = .GlobalEnv),
    add = TRUE
  )

  Sys.setenv(MOJOR_SOFT_RESET_AGGRESSIVE_UNLOAD = "true")
  Sys.setenv(MOJOR_SOFT_RESET_MAX_WRAPPERS = "4")
  on.exit(
    {
      Sys.unsetenv("MOJOR_SOFT_RESET_AGGRESSIVE_UNLOAD")
      Sys.unsetenv("MOJOR_SOFT_RESET_MAX_WRAPPERS")
    },
    add = TRUE
  )

  .mojor_state$loaded_wrapper_dlls <- replicate(
    8L,
    list(pkg = "fake_pkg", path = tempfile(fileext = ".so"), kind = "expression"),
    simplify = FALSE
  )

  .mojor_test_reset(mode = "soft", keep_backend = TRUE)
  expect_identical(called, 1L)
})

test_that(".mojor_wrapper_registry_paths normalizes and deduplicates paths", {
  p <- tempfile(fileext = ".so")
  regs <- list(
    list(pkg = "a", path = p, kind = "kernel"),
    list(pkg = "b", path = normalizePath(p, winslash = "/", mustWork = FALSE), kind = "jit"),
    list(pkg = "c", path = NA_character_, kind = "kernel")
  )
  paths <- .mojor_wrapper_registry_paths(regs)
  expect_equal(length(paths), 1L)
  expect_identical(paths[[1L]], normalizePath(p, winslash = "/", mustWork = FALSE))
})

test_that(".mojor_wrapper_leak_snapshot respects prune flag", {
  old_regs <- .mojor_state$loaded_wrapper_dlls
  on.exit({
    .mojor_state$loaded_wrapper_dlls <- old_regs
  }, add = TRUE)

  .mojor_state$loaded_wrapper_dlls <- list(
    list(pkg = "fake_pkg", path = tempfile(fileext = ".so"), kind = "expression")
  )
  snap_no_prune <- .mojor_wrapper_leak_snapshot(prune = FALSE)
  expect_identical(snap_no_prune$registered_count, 1L)
  expect_identical(snap_no_prune$registered_unique_paths, 1L)
  expect_true(snap_no_prune$tracked_loaded_dll_count <= snap_no_prune$registered_unique_paths)

  snap_pruned <- .mojor_wrapper_leak_snapshot(prune = TRUE)
  expect_identical(snap_pruned$registered_count, 0L)
  expect_identical(snap_pruned$registered_unique_paths, 0L)
  expect_true(snap_pruned$loaded_dll_count >= 0L)
})

test_that(".mojor_wrapper_leak_delta computes signed metric differences", {
  baseline <- list(
    registered_unique_paths = 2L,
    loaded_dll_count = 100L,
    tracked_loaded_dll_count = 3L
  )
  snap <- list(
    registered_unique_paths = 7L,
    loaded_dll_count = 96L,
    tracked_loaded_dll_count = 9L
  )
  delta <- .mojor_wrapper_leak_delta(snap, baseline = baseline)
  expect_identical(delta$registered_unique_paths, 5L)
  expect_identical(delta$loaded_dll_count, -4L)
  expect_identical(delta$tracked_loaded_dll_count, 6L)
})

test_that("expression wrapper closures bind .Call symbols with PACKAGE scope", {  skip_if_no_mojo()

  f <- function(x) x + 1.0
  built <- mojor_build(
    f,
    x = "f64",
    name = "t_expr_scope_leak",
    cache = FALSE,
    load = TRUE
  )
  expect_true(is.character(built$wrapper_pkg))
  expect_true(length(built$wrapper_pkg) == 1L)
  expect_true(nzchar(built$wrapper_pkg))

  symbol <- paste0(built$name, "_call")
  tmp <- tempfile("mojor_sym_scope_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  so <- .mojor_compile_dummy_call(symbol, value = 9999, dir = tmp)
  dll <- dyn.load(so)
  on.exit({
    try(dyn.unload(so), silent = TRUE)
    unlink(tmp, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  expect_true(is.character(dll[["name"]]))

  out <- built$func(1.0)
  expect_equal(as.numeric(out), 2.0)
})
