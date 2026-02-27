library(testthat)

.ir_docs_script_path <- function() {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", "check_ir_docs_consistency.sh")
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  stop("could not locate scripts/check_ir_docs_consistency.sh from test root")
}

.ir_docs_script_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

test_that("IR docs consistency script passes on repository docs", {
  out <- system2(
    "bash",
    .ir_docs_script_path(),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_identical(.ir_docs_script_status(out), 0L)
  expect_true(any(grepl("IR docs consistency check: clean", out, fixed = TRUE)))
})
