# Transpile SSA backend preparation tail helper.

.mojor_prepare_transpile_ssa_tail <- function(ctx) {
  if (!is.environment(ctx)) {
    stop(".mojor_prepare_transpile_ssa_tail: ctx must be an environment")
  }
  evalq(
    {
      ssa_stage <- .mojor_prepare_ssa_backend_stage(
        emit_ssa_backend = emit_ssa_backend,
        fn = fn,
        local_types = local_types,
        reduction = reduction,
        schedule = schedule,
        opt_level = opt_level
      )
      list2env(.mojor_ssa_stage_normalize(ssa_stage), envir = environment())
    },
    envir = ctx
  )
}
