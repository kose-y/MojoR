# MojoR Memory Usage Reporting (Stage B)
#
# Lightweight memory stats for built kernels.
# No runtime overhead <U+2014> computes from kernel metadata.

#' Compute memory usage statistics for a built kernel
#'
#' @param built A built kernel object from mojor_build()
#' @param n Integer: array length to estimate for (if not provided,
#' uses 1000 as default)
#' @return A list with input_bytes, output_bytes, total_bytes, and details
mojor_memory_stats <- function(built, n = 1000L) {
  if (is.null(built)) {
    stop("mojor_memory_stats: expected a built kernel object from mojor_build()")
  }

 # Support both legacy top-level shape and current built$trans shape.
  types <- built$types
  out_type <- built$out_type
  out_kind <- built$out_kind
  if ((is.null(types) || is.null(out_type) || is.null(out_kind)) && !is.null(built$trans)) {
    if (is.null(types)) types <- built$trans$types
    if (is.null(out_type)) out_type <- built$trans$out_type
    if (is.null(out_kind)) out_kind <- built$trans$out_kind
  }
  if (is.null(types) || is.null(out_type) || is.null(out_kind)) {
    stop("mojor_memory_stats: expected a built kernel object from mojor_build()")
  }

  type_bytes <- function(spec) {
    base <- sub("(\\[\\]|\\[1d\\])$", "", spec)
    switch(base,
      "f64" = 8L,
      "f32" = 4L,
      "i32" = 4L,
      "lgl" = 4L,
      8L # default
    )
  }

  input_bytes <- 0L
  input_details <- list()
  for (nm in names(types)) {
    spec <- types[[nm]]
    bpe <- type_bytes(spec)
    if (grepl("(\\[\\]|\\[1d\\])$", spec)) {
 # Array argument
      nbytes <- as.integer(n) * bpe
      input_details[[nm]] <- list(
        type = spec, bytes_per_element = bpe,
        elements = as.integer(n), total_bytes = nbytes
      )
    } else {
 # Scalar argument
      nbytes <- bpe
      input_details[[nm]] <- list(
        type = spec, bytes_per_element = bpe,
        elements = 1L, total_bytes = nbytes
      )
    }
    input_bytes <- input_bytes + nbytes
  }

 # Output
  out_bpe <- type_bytes(if (!is.null(out_type)) out_type else "f64")
  if (identical(out_kind, "vector")) {
    output_bytes <- as.integer(n) * out_bpe
    out_elements <- as.integer(n)
  } else {
    output_bytes <- out_bpe
    out_elements <- 1L
  }

  list(
    input_bytes = input_bytes,
    output_bytes = output_bytes,
    total_bytes = input_bytes + output_bytes,
    n = as.integer(n),
    details = list(
      inputs = input_details,
      output = list(
        type = out_type, kind = out_kind,
        bytes_per_element = out_bpe,
        elements = out_elements,
        total_bytes = output_bytes
      )
    )
  )
}
