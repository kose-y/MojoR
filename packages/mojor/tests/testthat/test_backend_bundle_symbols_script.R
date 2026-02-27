library(testthat)

.bundle_symbol_script_path <- function() {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", "check_backend_bundle_symbols.sh")
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  ""
}

.bundle_symbol_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

.bundle_platform_info <- function() {
  os <- Sys.info()[["sysname"]]
  arch <- Sys.info()[["machine"]]
  if (identical(arch, "aarch64")) arch <- "arm64"
  if (identical(arch, "amd64")) arch <- "x86_64"

  if (identical(os, "Darwin")) {
    return(list(platform = paste0("darwin-", arch), ext = "dylib"))
  }
  if (identical(os, "Linux")) {
    return(list(platform = paste0("linux-", arch), ext = "so"))
  }
  NULL
}

test_that("backend bundle symbol script passes on current bundle", {
  script <- .bundle_symbol_script_path()
  if (!nzchar(script)) {
    skip("scripts/check_backend_bundle_symbols.sh not available in this test layout")
  }
  if (!nzchar(Sys.which("nm"))) {
    skip("nm not available")
  }
  info <- .bundle_platform_info()
  if (is.null(info)) {
    skip("unsupported platform for bundle symbol script")
  }

  repo_root <- dirname(dirname(script))
  package_dir <- file.path(repo_root, "packages", "mojor")
  bundle_lib <- file.path(package_dir, "inst", "backend", info$platform, paste0("libmojor_backend.", info$ext))
  if (!file.exists(bundle_lib)) {
    skip(paste("platform bundle missing:", bundle_lib))
  }

  out <- system2(
    "bash",
    c(script, "--package-dir", package_dir),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.bundle_symbol_status(out), 0L)
  expect_true(any(grepl("Backend bundle symbol check: clean", out, fixed = TRUE)))
})

test_that("backend bundle symbol script returns input error for unknown flag", {
  script <- .bundle_symbol_script_path()
  if (!nzchar(script)) {
    skip("scripts/check_backend_bundle_symbols.sh not available in this test layout")
  }
  out <- suppressWarnings(system2(
    "bash",
    c(script, "--unknown-flag"),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.bundle_symbol_status(out), 2L)
})

test_that("backend bundle symbol script fails when bundle library is missing", {
  script <- .bundle_symbol_script_path()
  if (!nzchar(script)) {
    skip("scripts/check_backend_bundle_symbols.sh not available in this test layout")
  }
  info <- .bundle_platform_info()
  if (is.null(info)) {
    skip("unsupported platform for bundle symbol script")
  }

  fake_pkg <- tempfile("fake_pkg_")
  dir.create(file.path(fake_pkg, "inst", "backend", info$platform), recursive = TRUE)
  on.exit(unlink(fake_pkg, recursive = TRUE), add = TRUE)

  out <- suppressWarnings(system2(
    "bash",
    c(script, "--package-dir", fake_pkg),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.bundle_symbol_status(out), 1L)
  expect_true(any(grepl("missing backend library", out, fixed = TRUE)))
})
