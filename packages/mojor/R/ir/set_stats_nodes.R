# =============================================================================
# Additional IR Node Constructors
# =============================================================================

# Set/Match Primitives

# unique(x) - returns unique values from a vector
# x: IR expression node for input vector
.mojor_ir_unique <- function(x, src = NULL) {
  list(kind = "unique", x = x, src = src)
}

# duplicated(x) - returns logical vector indicating duplicated values
# x: IR expression node for input vector
.mojor_ir_duplicated <- function(x, src = NULL) {
  list(kind = "duplicated", x = x, src = src)
}

# anyDuplicated(x) - returns index of first duplicate or 0 if none
# x: IR expression node for input vector
.mojor_ir_any_duplicated <- function(x, src = NULL) {
  list(kind = "any_duplicated", x = x, src = src)
}

# match(x, table) - returns positions of first matches
# x: IR expression node for input vector
# table: IR expression node for lookup table
.mojor_ir_match <- function(x, table, src = NULL) {
  list(kind = "match", x = x, table = table, src = src)
}

# %in% operator - returns logical vector indicating matches
# x: IR expression node for input vector
# table: IR expression node for lookup table
.mojor_ir_in <- function(x, table, src = NULL) {
  list(kind = "in", x = x, table = table, src = src)
}

# Quantiles & Robust Stats

# median(x) - returns median value
# x: IR expression node for input vector
.mojor_ir_median <- function(x, src = NULL) {
  list(kind = "median", x = x, src = src)
}

# quantile(x, probs, na.rm = FALSE, type = 7) - returns quantile values
# x: IR expression node for input vector
# probs: IR expression node for probability values
# na_rm: scalar logical literal or NULL (FALSE)
# type: scalar integer interpolation type (default 7)
.mojor_ir_quantile <- function(x, probs, na_rm = FALSE, type = 7, src = NULL) {
  list(kind = "quantile", x = x, probs = probs, na_rm = na_rm, type = type, src = src)
}

# IQR(x) - returns interquartile range
# x: IR expression node for input vector
.mojor_ir_iqr <- function(x, src = NULL) {
  list(kind = "iqr", x = x, src = src)
}

# mad(x) - returns median absolute deviation
# x: IR expression node for input vector
.mojor_ir_mad <- function(x, src = NULL) {
  list(kind = "mad", x = x, src = src)
}
