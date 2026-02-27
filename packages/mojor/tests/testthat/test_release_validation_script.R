library(testthat)

.release_script_path <- function(name) {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", name)
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  stop(sprintf("could not locate scripts/%s from test root", name))
}

.release_script_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

.release_write_script <- function(path, lines) {
  writeLines(lines, path)
  Sys.chmod(path, mode = "0755")
}

.release_report_field <- function(report_lines, field) {
  hit <- grep(paste0("^- ", field, ":"), report_lines, value = TRUE)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  sub(paste0("^- ", field, ":[[:space:]]*"), "", hit[[1L]])
}

.release_create_fake_repo <- function(readiness_lines, rc_lines) {
  tmp <- tempfile("release_validation_repo_")
  dir.create(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "scripts"), recursive = TRUE)
  dir.create(file.path(tmp, "docs", "BASELINES"), recursive = TRUE)

  runner_src <- .release_script_path("run_release_validation.sh")
  runner_dst <- file.path(tmp, "scripts", "run_release_validation.sh")
  file.copy(runner_src, runner_dst)
  Sys.chmod(runner_dst, mode = "0755")

  .release_write_script(
    file.path(tmp, "scripts", "check_release_readiness.sh"),
    readiness_lines
  )
  .release_write_script(
    file.path(tmp, "scripts", "check_release_candidate.sh"),
    rc_lines
  )

  list(
    root = tmp,
    runner = runner_dst,
    report = file.path(tmp, "docs", "BASELINES", "RELEASE_REPORT_LATEST.md")
  )
}

test_that("run_release_validation captures non-zero readiness status and marks FAIL", {
  marker <- tempfile("rc_called_")
  repo <- .release_create_fake_repo(
    readiness_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'readiness failed'",
      "exit 7"
    ),
    rc_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      paste0("echo called > ", shQuote(marker)),
      "exit 0"
    )
  )
  on.exit(unlink(repo$root, recursive = TRUE), add = TRUE)

  out <- suppressWarnings(system2(
    "bash",
    c(repo$runner, "--without-perf", "--report", repo$report),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.release_script_status(out), 1L)
  expect_false(file.exists(marker))
  expect_true(file.exists(repo$report))

  report_lines <- readLines(repo$report, warn = FALSE)
  expect_identical(.release_report_field(report_lines, "Readiness status"), "7")
  expect_identical(.release_report_field(report_lines, "Release-candidate status"), "0")
  expect_identical(.release_report_field(report_lines, "Overall"), "FAIL")
  expect_true(any(grepl("Skipped release-candidate gate due readiness failure\\.", report_lines)))
})

test_that("run_release_validation captures non-zero release-candidate status and marks FAIL", {
  repo <- .release_create_fake_repo(
    readiness_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'ready'",
      "exit 0"
    ),
    rc_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'rc failed'",
      "exit 9"
    )
  )
  on.exit(unlink(repo$root, recursive = TRUE), add = TRUE)

  out <- suppressWarnings(system2(
    "bash",
    c(repo$runner, "--without-perf", "--report", repo$report),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.release_script_status(out), 1L)
  expect_true(file.exists(repo$report))

  report_lines <- readLines(repo$report, warn = FALSE)
  expect_identical(.release_report_field(report_lines, "Readiness status"), "0")
  expect_identical(.release_report_field(report_lines, "Release-candidate status"), "9")
  expect_identical(.release_report_field(report_lines, "Overall"), "FAIL")
  expect_true(any(grepl("^rc failed$", report_lines)))
})

test_that("run_release_validation reports PASS only when readiness and release-candidate pass", {
  repo <- .release_create_fake_repo(
    readiness_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'ready'",
      "exit 0"
    ),
    rc_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'rc clean'",
      "exit 0"
    )
  )
  on.exit(unlink(repo$root, recursive = TRUE), add = TRUE)

  out <- system2(
    "bash",
    c(repo$runner, "--without-perf", "--report", repo$report),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.release_script_status(out), 0L)
  expect_true(file.exists(repo$report))

  report_lines <- readLines(repo$report, warn = FALSE)
  expect_identical(.release_report_field(report_lines, "Readiness status"), "0")
  expect_identical(.release_report_field(report_lines, "Release-candidate status"), "0")
  expect_identical(.release_report_field(report_lines, "Overall"), "PASS")
})

test_that("run_release_validation forwards --without-perf to release-candidate gate", {
  args_file <- tempfile("rc_args_")
  repo <- .release_create_fake_repo(
    readiness_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "exit 0"
    ),
    rc_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      paste0("printf '%s\\n' \"$*\" > ", shQuote(args_file)),
      "case \" $* \" in",
      "  *' --without-perf '*) exit 0 ;;",
      "  *) exit 6 ;;",
      "esac"
    )
  )
  on.exit(unlink(repo$root, recursive = TRUE), add = TRUE)
  on.exit(unlink(args_file), add = TRUE)

  out <- system2(
    "bash",
    c(repo$runner, "--without-perf", "--report", repo$report),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.release_script_status(out), 0L)
  expect_true(file.exists(args_file))
  expect_true(any(grepl("--without-perf", readLines(args_file, warn = FALSE), fixed = TRUE)))
})

test_that("run_release_validation report preserves fallback isolation clean output", {
  repo <- .release_create_fake_repo(
    readiness_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo 'ready'",
      "exit 0"
    ),
    rc_lines = c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "echo '[run] fallback isolation'",
      "echo 'packages/mojor/R: current=5 baseline=5 delta=0'",
      "echo 'Fallback isolation check: clean'",
      "echo '[ok ] fallback isolation'",
      "exit 0"
    )
  )
  on.exit(unlink(repo$root, recursive = TRUE), add = TRUE)

  out <- system2(
    "bash",
    c(repo$runner, "--without-perf", "--report", repo$report),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.release_script_status(out), 0L)
  report_lines <- readLines(repo$report, warn = FALSE)
  expect_true(any(grepl("\\[ok \\] fallback isolation", report_lines)))
  expect_true(any(grepl("Fallback isolation check: clean", report_lines, fixed = TRUE)))
})

test_that("check_release_report_consistency rejects inconsistent status and overall markers", {
  checker <- .release_script_path("check_release_report_consistency.sh")
  report <- tempfile("release_report_", fileext = ".md")
  writeLines(c(
    "# Release Validation Report",
    "",
    "- Generated: 2026-02-19 00:00:00 UTC",
    "- Readiness status: 0",
    "- Release-candidate status: 3",
    "- Overall: PASS",
    "",
    "## Readiness Output",
    "",
    "```text",
    "ready",
    "```",
    "",
    "## Release Candidate Output",
    "",
    "```text",
    "failed",
    "```"
  ), report)
  on.exit(unlink(report), add = TRUE)

  out <- suppressWarnings(system2(
    "bash",
    c(checker, "--report", report),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.release_script_status(out), 1L)
  expect_true(any(grepl("overall must be FAIL", out, fixed = TRUE)))
})
