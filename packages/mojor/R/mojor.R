# Autoload split MojoR R source files from the package tree.

.mojor_root <- function() {
  src <- sys.frame(1)$ofile
  starts <- unique(c(
    if (!is.null(src) && is.character(src) && length(src) == 1L && nzchar(src)) dirname(src) else NULL,
    getwd()
  ))
  for (start in starts) {
    cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in 0:16) {
      if (file.exists(file.path(cur, "core", "core.R")) &&
          file.exists(file.path(cur, "mojor.R"))) {
        return(normalizePath(cur, winslash = "/", mustWork = FALSE))
      }
      cand_pkg <- file.path(cur, "packages", "mojor", "R")
      if (file.exists(file.path(cand_pkg, "core", "core.R"))) {
        return(normalizePath(cand_pkg, winslash = "/", mustWork = FALSE))
      }
      cand_local <- file.path(cur, "R")
      if (file.exists(file.path(cand_local, "core", "core.R"))) {
        return(normalizePath(cand_local, winslash = "/", mustWork = FALSE))
      }
      next_cur <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
      if (identical(next_cur, cur)) {
        break
      }
      cur <- next_cur
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

.mojor_source_all <- function() {
  root <- .mojor_root()
  target_env <- environment(.mojor_source_all)
  files <- c(
    "core/core.R",
    "core/gpu.R",
    "ir/nodes.R",
    "ir/utils.R",
    "ir/build.R",
    "ir/type_guards.R",
    "ir/ir.R",
    "ir/apply_nodes.R",
    "runtime/context.R",
    "runtime/error.R",
    "runtime/constants.R",
    "runtime/ir_types.R",
    "ir/eval_const.R",
    "ir/fusion.R",
    "ir/lowering.R",
    "ir/expr_build.R",
    "ir/index_emit.R",
    "ir/stmt_range_emit.R",
    "ir/stmt_loop_emit.R",
    "ir/stmt_assign_emit.R",
    "ir/stmt_emit.R",
    "ir/typing.R",
    "ir/bounds_emit.R",
    "ir/verify.R",
    "ir/expr_emit.R",
    "ir/set_stats_nodes.R",
    "ir/ssa.R",
    "ir/ssa_verify.R",
    "ir/ssa_semantic.R",
    "ir/ssa_backend.R",
    "ir/structured_ops.R",
    "ir/structured_lowering.R",
    "ir/loop_recovery.R",
    "ir/loop_recanonicalize.R",
    "ir/op_schema.R",
    "core/reductions_jit.R",
    "transpile/helpers_core.R",
    "transpile/helpers_analysis.R",
    "transpile/helpers_codegen.R",
    "transpile/entry_stages.R",
    "transpile/error_diagnostics.R",
    "transpile/reports.R",
    "transpile/output.R",
    "transpile/gpu_emit.R",
    "transpile/finalize.R",
    "transpile/ssa_tail.R",
    "transpile/pipeline.R",
    "transpile/ir_helpers.R",
    "transpile/loop_helpers.R",
    "transpile/prelude_helpers.R",
    "transpile/reduction_paths.R",
    "transpile/loop_codegen_units.R",
    "transpile/route_dispatch.R",
    "transpile/expression_registry.R",
    "transpile/expression_only.R",
    "transpile/scalar_expr.R",
    "transpile/transpile.R",
    "build/helpers.R",
    "build/build_compiled_subset.R",
    "build/build.R",
    "ir/error_recovery.R",
    "runtime/parallel_subprocess.R",
    "runtime/memory.R",
    "runtime/serialization.R",
    "runtime/ffi.R",
    "runtime/profiling.R"
  )
  for (f in files) {
    sys.source(file.path(root, f), envir = target_env, keep.source = FALSE)
  }
  invisible(TRUE)
}

.mojor_source_all()

if (exists(".mojor_state", envir = .GlobalEnv, inherits = FALSE) &&
    is.environment(.mojor_state)) {
  fn_names <- grep("^(\\.mojor_|mojor_)", ls(.GlobalEnv, all.names = TRUE), value = TRUE)
  fn_objs <- mget(fn_names, envir = .GlobalEnv, inherits = FALSE)
  fn_objs <- fn_objs[vapply(fn_objs, is.function, logical(1))]
  .mojor_state$test_function_baseline <- fn_objs
}

rm(.mojor_root, .mojor_source_all)
