# =============================================================================
# Additional IR Node Constructors
# =============================================================================

# apply() - matrix apply with row/column margins
# HIR node for apply() operation
# x: IR expression node for input matrix
# margin: 1 (rows) or 2 (columns)
# fun: reduction function ("sum", "mean", "min", "max")
.mojor_ir_apply <- function(x, margin, fun, na_rm = FALSE, src = NULL) {
  list(kind = "apply", x = x, margin = margin, fun = fun, na_rm = na_rm, src = src)
}

# sample constructors are defined in ir_nodes.R.
