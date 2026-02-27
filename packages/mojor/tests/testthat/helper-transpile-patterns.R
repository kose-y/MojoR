.mojor_any_match <- function(text, patterns, fixed = FALSE, perl = FALSE, ignore.case = FALSE) {
  if (!is.character(text) || length(text) != 1L || !nzchar(text)) return(FALSE)
  pats <- as.character(patterns)
  if (length(pats) == 0L) return(FALSE)
  any(vapply(
    pats,
    function(p) grepl(p, text, fixed = fixed, perl = perl, ignore.case = ignore.case),
    logical(1)
  ))
}

expect_mojor_any_match <- function(text, patterns, fixed = FALSE, perl = FALSE, ignore.case = FALSE, info = NULL) {
  if (is.null(info)) {
    info <- paste("expected any match:", paste(as.character(patterns), collapse = " | "))
  }
  testthat::expect_true(
    .mojor_any_match(text, patterns, fixed = fixed, perl = perl, ignore.case = ignore.case),
    info = info
  )
}

expect_mojor_error_or_result <- function(run, error_patterns, on_success = NULL) {
  res <- tryCatch(run(), error = function(e) e)
  if (inherits(res, "error")) {
    expect_mojor_any_match(conditionMessage(res), error_patterns)
    return(invisible(res))
  }
  if (is.function(on_success)) on_success(res)
  invisible(res)
}

.mojor_wrapper_path <- function(built) {
  if (!is.list(built)) stop("built must be a list")
  if (!is.null(built$build_dir) && !is.null(built$kernel)) {
    return(file.path(built$build_dir, paste0(built$kernel, "_wrapper.c")))
  }
  if (!is.null(built$lib) && !is.null(built$name)) {
    return(file.path(dirname(built$lib), paste0(built$name, "_wrapper.c")))
  }
  stop("unable to determine wrapper path from build result")
}

mojor_wrapper_source <- function(built) {
  wrapper <- .mojor_wrapper_path(built)
  testthat::expect_true(file.exists(wrapper), info = paste("wrapper not found:", wrapper))
  paste(readLines(wrapper, warn = FALSE), collapse = "\n")
}

expect_wrapper_has <- function(built, patterns, fixed = TRUE, perl = FALSE, ignore.case = FALSE) {
  src <- mojor_wrapper_source(built)
  pats <- as.character(patterns)
  for (p in pats) {
    testthat::expect_true(
      grepl(p, src, fixed = fixed, perl = perl, ignore.case = ignore.case),
      info = paste("expected wrapper to contain pattern:", p)
    )
  }
  invisible(src)
}

expect_wrapper_lacks <- function(built, patterns, fixed = TRUE, perl = FALSE, ignore.case = FALSE) {
  src <- mojor_wrapper_source(built)
  pats <- as.character(patterns)
  for (p in pats) {
    testthat::expect_false(
      grepl(p, src, fixed = fixed, perl = perl, ignore.case = ignore.case),
      info = paste("expected wrapper to not contain pattern:", p)
    )
  }
  invisible(src)
}
