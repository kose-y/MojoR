.mojor_elementwise_report_lines <- function(ew) {
  lines <- c(
    paste0("elementwise.enabled: ", ew$enabled),
    paste0("elementwise.target: ", ew$target),
    paste0("elementwise.emitted: ", ew$emitted),
    paste0("elementwise.inplace: ", ew$inplace),
    paste0("elementwise.gpu_buf_emitted: ", ew$gpu_buf_emitted)
  )
  if (!is.null(ew$reason) && !is.na(ew$reason) && nzchar(ew$reason)) {
    lines <- c(lines, paste0("elementwise.reason: ", ew$reason))
  }
  if (!is.null(ew$gpu_buf_reason) && !is.na(ew$gpu_buf_reason) && nzchar(ew$gpu_buf_reason)) {
    lines <- c(lines, paste0("elementwise.gpu_buf_reason: ", ew$gpu_buf_reason))
  }
  if (!is.null(ew$gpu_buf_dtype) && !is.na(ew$gpu_buf_dtype) && nzchar(ew$gpu_buf_dtype)) {
    lines <- c(lines, paste0("elementwise.gpu_buf_dtype: ", ew$gpu_buf_dtype))
  }
  lines
}

.mojor_simd_report_lines <- function(simd) {
  lines <- c(
    paste0("simd.mode: ", simd$mode),
    paste0("simd.safe: ", simd$safe),
    paste0("simd.emitted: ", simd$emitted)
  )
  if (!is.null(simd$reason) && !is.na(simd$reason) && nzchar(simd$reason)) {
    lines <- c(lines, paste0("simd.reason: ", simd$reason))
  }
  if (!is.null(simd$explicit_disabled) && isTRUE(simd$explicit_disabled)) {
    lines <- c(lines, "simd.note: explicit SIMD disabled (auto)")
  }
  if (!is.null(simd$assume_aligned) && !is.na(simd$assume_aligned)) {
    lines <- c(lines, paste0("simd.assume_aligned: ", simd$assume_aligned))
  }
  lines
}

mojor_simd_report <- function(fn = NULL, ..., trans = NULL, na_mode = NULL, assume_aligned = NULL, simd_mode = NULL) {
  if (is.null(trans)) {
    if (is.null(fn)) {
      stop("mojor_simd_report: provide fn or trans")
    }
    trans <- mojor_transpile(fn, ..., na_mode = na_mode, assume_aligned = assume_aligned, simd_mode = simd_mode)
  }
  if (is.null(trans$simd)) {
    stop("mojor_simd_report: trans has no simd info")
  }
  lines <- .mojor_simd_report_lines(trans$simd)
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(trans$simd)
}
