# Transpile post-loop pipeline orchestration helper.

.mojor_gpu_preview_blocker_labels <- function(blockers) {
  if (!is.list(blockers)) {
    return(character(0))
  }
  labels <- character(0)
  if (isTRUE(blockers$reduce_calls > 0L)) {
    reduce_ops <- blockers$reduce_ops
    labels <- c(labels, if (length(reduce_ops) > 0) {
      paste0("_mojor_gpu_reduce(op=", paste(reduce_ops, collapse = "/"), ")")
    } else {
      "_mojor_gpu_reduce"
    })
  }
  if (isTRUE(blockers$matmul_calls > 0L)) {
    labels <- c(labels, "_mojor_gpu_matmul(expression)")
  }
  if (isTRUE(blockers$matmul_into_calls > 0L)) {
    labels <- c(labels, "_mojor_gpu_matmul_into(assign)")
  }
  labels
}

.mojor_effective_elementwise_target <- function(elementwise, elementwise_target) {
  if (isTRUE(elementwise)) elementwise_target else "off"
}

.mojor_enforce_gpu_jit_mode <- function(
    mojo_lines,
    gpu_jit_mode = "auto",
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off",
    context = "mojor_transpile") {
  mode <- match.arg(gpu_jit_mode, c("auto", "unified_preview"))
  info <- .mojor_detect_gpu_jit_coverage(
    mojo_lines = mojo_lines,
    elementwise_gpu_emitted = elementwise_gpu_emitted,
    elementwise_target = elementwise_target
  )
  info$mode <- mode

  if (identical(mode, "unified_preview") && isTRUE(info$helper_backed_used)) {
    blocker_labels <- .mojor_gpu_preview_blocker_labels(info$preview_blockers)
    blocker_msg <- if (length(blocker_labels) > 0) {
      paste0(" blocked by ", paste(blocker_labels, collapse = ", "), ". ")
    } else {
      " "
    }
    stop(
      context,
      ": gpu_jit_mode='unified_preview' does not allow helper-backed GPU routes (_mojor_gpu_reduce/_mojor_gpu_matmul); ",
      blocker_msg,
      "use unified elementwise GPU loops or set gpu_jit_mode='auto'",
      call. = FALSE
    )
  }

  info
}

.mojor_finalize_transpile_pipeline <- function(ctx) {
  if (!is.environment(ctx)) {
    stop(".mojor_finalize_transpile_pipeline: ctx must be an environment")
  }
  evalq(
    {
      .mojor_emit_transpile_gpu_buffer_block(environment())
      elementwise_gpu_emitted <- if (exists("elementwise_gpu_buf_emitted", inherits = TRUE)) {
        isTRUE(elementwise_gpu_buf_emitted)
      } else {
        FALSE
      }
      mojo_lines <- .mojor_finalize_mojo_postamble(
        mojo_lines = mojo_lines,
        index_bounds = index_bounds
      )
      gpu_jit_gate_info <- .mojor_enforce_gpu_jit_mode(
        mojo_lines = mojo_lines,
        gpu_jit_mode = gpu_jit_mode,
        elementwise_gpu_emitted = elementwise_gpu_emitted,
        elementwise_target = .mojor_effective_elementwise_target(elementwise, elementwise_target),
        context = "mojor_transpile"
      )
      .mojor_finalize_transpile_scalar_output(environment())
      .mojor_prepare_transpile_ssa_tail(environment())
      .mojor_build_transpile_output(environment())
    },
    envir = ctx
  )
}
