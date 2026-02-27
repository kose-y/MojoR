# =============================================================================
# Stage 5a: Structured -> CFG SSA Lowering Entry Points
# =============================================================================

.mojor_ir_structured_done_flag_required <- function(node) {
 # Stage 5a seed keeps direct edge semantics as the default lowering strategy.
 # Done-flag canonicalization is fallback-only and currently not required.
  FALSE
}

.mojor_ir_structured_lower_to_ssa <- function(
    node,
    verify_ir = FALSE,
    attach_loop_metadata = TRUE,
    strict_schema = NULL) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(NULL)
  }

  structured <- .mojor_ir_structured_promote(node, assign_loop_ids = TRUE)
  if (isTRUE(verify_ir)) .mojor_ir_verify(structured)

  ssa <- .mojor_ir_to_ssa(structured)
  if (isTRUE(attach_loop_metadata) &&
    exists(".mojor_ssa_attach_loop_destination_metadata", mode = "function")) {
    ssa <- .mojor_ssa_attach_loop_destination_metadata(ssa)
  }

  ssa$structured_lowering <- list(
    strategy = "direct_edges",
    used_done_flag_fallback = FALSE
  )

  if (isTRUE(verify_ir)) .mojor_ir_verify_ssa(ssa, strict_schema = strict_schema)
  ssa
}
