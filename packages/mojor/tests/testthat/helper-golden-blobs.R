.mojor_test_golden_root <- function() {
  dirname(dirname(find_mojor()))
}

.mojor_test_golden_path <- function(name) {
  file.path(.mojor_test_golden_root(), "tests", "testthat", "golden", name)
}

.mojor_test_read_golden <- function(name) {
  path <- .mojor_test_golden_path(name)
  if (!file.exists(path)) {
    stop("missing golden file: ", path)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

.mojor_test_norm_blob <- function(text) {
  gsub("\r\n", "\n", text, fixed = TRUE)
}

expect_mojor_golden <- function(actual, name) {
  testthat::expect_identical(
    .mojor_test_norm_blob(actual),
    .mojor_test_read_golden(name)
  )
}
