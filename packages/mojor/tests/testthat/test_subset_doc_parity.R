library(testthat)

test_that("SUBSET_V1.md mentions map to tests", {
  mojor_path <- find_mojor()
  skip_if(is.null(mojor_path), "could not resolve package root for SUBSET_V1.md")
  subset_file <- file.path(dirname(dirname(mojor_path)), "SUBSET_V1.md")
  skip_if_not(file.exists(subset_file), "SUBSET_V1.md not found in package root")
  subset_text <- paste(readLines(subset_file, warn = FALSE), collapse = "\n")
  required <- c(
    "seq_along",
    "seq_len",
    "seq.int",
    "1:n",
    "as.integer",
    "as.double",
    "as.logical",
    "which.min",
    "which.max",
    "any()",
    "all()",
    "is.na",
    "is.nan",
    "is.finite",
    "is.infinite",
    "ifelse",
    "%/%",
    "%%"
  )
  for (tok in required) {
    expect_true(grepl(tok, subset_text, fixed = TRUE), info = paste("missing in doc:", tok))
  }
})
