# MöjoR Known Issues and Limitations

This document lists active limitations and intentional constraints in the current
MöjoR release.

For supported behavior, use:
- `docs/SUBSET.md`
- `docs/IR/BASIC_FUNCTION_SUPPORT.md`
- `docs/IR/FUNCTION_SUPPORT_BREADTH.md`
- `docs/GPU_FUNCTION_SUPPORT_MATRIX.md`

## Language and IR Scope Limits

- MöjoR compiles a strict subset of R; arbitrary helper/function calls outside
  the covered set are rejected in strict mode.
- NSE and full-language R semantics are not available in compiled kernels.
- R bytecode is not a compilation target.
- Compiled data-frame `[ ]` access is limited to canonical scalar-read forms;
  broader selector/arity variants are rejected.
- Compiled data-frame lanes require explicit `df_schema` declarations for each
  `df` argument and accessed column.
- Implicit vectorized array kernels reject control flow (`for`, `while`,
  `repeat`), side effects (RNG, I/O), user-defined helper calls, and non
  let-binding multi-statement bodies.

## Loop Body and Structure Limits

- User-defined/closure/recursive calls are rejected inside compiled loop bodies.
- Dynamic structures in loop bodies are rejected (lists, data.frames, factors,
  environments, S3/S4 objects).
- Loop-body error-handling constructs are rejected (`switch()`, `tryCatch()`,
  `try()`, `stop()`).
- String operations are subset-limited in loop contexts.
- `sample()` / `sample.int()` are rejected as loop iterators.
- `repeat` without explicit `break` is rejected.
- Inside loop bodies, non-canonical whole-vector slice allocation and
  slice-as-reduction-argument forms remain unsupported.

## Indexing and Shape Limits

- General vector-valued intermediates in loop bounds remain subset-limited;
  unsupported forms fail with diagnostics.
- Selector `0` and mixed positive/negative literal selectors in `c(...)` are
  rejected with deterministic diagnostics.
- Direct nested selector with missing index (`mat[row, ][cols]` class) is
  unsupported in strict loop/index lanes.
- Negative-index writes are restricted to the output base variable; writes on
  non-output bases are rejected.
- Bounds-check fail-fast behavior is opt-in (`bounds_check=TRUE` or
  `mojor_options(index_bounds=TRUE)`); non-strict mode remains sentinel-based.
- Strict matrix/array output indexing remains partial for non-canonical ND
  gather/scatter writes and non-canonical RHS gather shapes.
- Matrix/ND typed layout completeness remains partial in strict lanes; typed
  pass support is present but not yet universal across all ND/indexing routes.
- Dynamic-rank indexing (variable number of dimensions) is unsupported.
- Character indexing remains route-constrained by compile-time metadata and
  runtime lane requirements.
- Whole-vector no-loop indexing routes remain partial in strict mode.
- Higher-rank chained matrix/ND gather forms remain partial.
- Non-typed/unresolved loop-bound expressions remain unsupported.
- 3D array runtime dispatch can still fail with shape/type mismatch errors in
  some routes.
- In scalar assignment lanes using negative indexing (`out[i] <- x[-j]`), RHS is
  treated as a scalar lane (first element of exclusion result), not a full
  vector result.

## Wrapper and API Surface Limits

- `gpu_jit_mode='unified_preview'` rejects helper-shim GPU routes
  (`_mojor_gpu_reduce`, `_mojor_gpu_matmul`).
- `gpu_matmul` expression/return forms remain constrained by GPU JIT mode.
- `mojor_guvectorize(core_dims=...)` implements a strict subset grammar; full
  Numba core-dimension grammar is not implemented.
- Rank-N gufunc strict-lowering reductions are limited to `sum`, `mean`,
  `min`, `max`.
- GPU-target core-dims kernels are strict-compile only; strict compile failure
  does not silently fall back to CPU.
- Multi-output `core_dims` wrappers remain constrained by callable mode,
  terminal return form, and exact output-shape metadata requirements.
- Rank-N strict indexed core-dims lanes require canonical core-index order.
- Rank-N neighbor indexing in strict indexed core-dims lanes requires canonical
  explicit in-bounds guards (`&&` conjunction form); complex guard operators and
  non-literal neighbor offsets are rejected.
- Higher-order compiled coverage remains subset-limited (`vapply`/`sapply`/
  `lapply`/`mapply` constraints).
- `mojor_fn()` preview higher-order lanes are stricter (anonymous,
  single-expression, capture-free `FUN`; direct callable inputs; constrained
  `FUN.VALUE`).
- In no-loop compatibility-lowering routes, duplicate `mapply` `FUN` arguments
  are accepted only when identical; conflicting duplicates are rejected.
- No-loop compatibility-lowering higher-order routes reject non-scalar/list-like
  return forms outside the fixed scalar constructor subset.
- `expand.grid` no-loop compatibility-lowering remains constrained to direct and
  subset-safe vector-expression forms.
- Scalar string expression emission is unsupported outside loop/index contexts.
- Scalar `paste()`/`paste0()` emission with non-`NULL` `collapse` is unsupported
  on strict lanes.
- Weighted sampling (`prob`) remains constrained: indexed-dynamic weighted lanes
  require `replace = TRUE`.
- Vectorized RNG lanes are implemented for `runif`/`rnorm`; broader distribution
  strict-lane coverage remains subset-limited by route and argument form.
- No-loop compatibility-lowering `quantile()` keeps strict `probs` constraints.
- No-loop compatibility-lowering `dim()` remains constrained to direct-array
  metadata lanes.
- Strict local matrix/array constructor-data emission rejects unsupported helper
  calls/expression-node families in data expressions.
- No-arg vector outputs fail compilation when output extent is not inferable.
- Tier9 constructor runtime assembly remains strict:
  - `data.frame(...)` requires explicitly named, unique, equal-length columns
    and rejects reserved control arguments.
  - `list(...)` requires at least one entry and homogeneous atomic-vector
    element base types.
- Full IR-native compiled string ABI is not available.
- Compiled regex replacement excludes backreference substitution in
  `sub()` / `gsub()` replacement strings.
- JIT strict-signature mode (`strict_signatures=TRUE`) rejects undeclared
  runtime signatures.
- JIT `bench`/`prod` profile presets enable strict-signature mode by default.

## Runtime and Fallback Limits

- Parallel loops may fall back to subprocess execution depending on host/runtime
  constraints.
- Subprocess standalone runner supports scalar and 1D arg/output lanes only.
- `mojor_run_chains_parallel(backend="fork")` is unsupported on Windows.
- In strict IR mode (`ir_only = TRUE`), fallback paths are blocked by policy.
- Compatibility rewrite fallback remains possible for unsupported forms in
  non-strict flows.
- Some unsupported `mojor_fn()` quantile/robust preview signatures can still
  route through interpreted fallback in non-strict flows.
- Strict loop diagnostics cover common failures, but uncommon failures may still
  surface under broad `Loop not supported: ...` diagnostics.
- Runtime shadow comparison excludes RNG/seed-sensitive routes from strict
  equality checks.
- Mixed-length arrays require explicit broadcast policy (`broadcast = "recycle"`
  or `"recycle_warn"`).
- Long mixed-indexing runs can remain sensitive to cache/process state.
- Under `semantics = "r"`, copy-on-modify routing remains conservative for
  alias-sensitive/uncertain mutation paths.
- `MOJOR_IR_SERVICE` / `MOJOR_IR_EMIT_SHADOW` environment controls are
  deprecated and ignored; unified IR emitter is always used.
- Runtime-v1 speedups remain workload-shape dependent; some call-heavy fused
  patterns can benchmark slower than legacy routes.
- Backend reduction ABI remains `nomiss`-oriented; NA/NaN policy enforcement is
  in R/runtime preflight lanes, not backend reduction kernels.
- Generic `na_mode='forbid'` preflight intentionally skips logical arrays for
  mask-style NA-as-skip semantics; logical scalar reduction lanes use dedicated
  forbid-mode NA checks.

## GPU and Backend Limits

- On Apple Metal, `f64` device tests are opt-in and may skip when backend `f64`
  buffers are unavailable.
- On macOS Metal, `f64` linalg/reduce routes use explicit CPU fallback
  (`cpu_matmul`, `cpu_reduce`) until backend `f64` GPU support is available.
- Matrix2D GPU kernels are subset-limited to bounded strict loop/index/guard
  forms; unguarded neighbor reads, irregular indexing, in-kernel reductions,
  and broader non-canonical guard predicates are unsupported.
- Matrix2D GPU strict elementwise lanes are constrained by loop-shape and
  dtype/shape entry requirements.
- MöjoR does not provide a full GPU worker/IPC layer; GPU buffer flows are
  in-process.
- GPU context lifecycle controls are constrained in-process; free/reset paths
  reject live-buffer states.
- GPUArray `Ops` coverage remains subset-limited; shape-mismatch routes can
  fall back to host parity paths.
- GPUArray bracket semantics remain route-constrained; broader selector classes
  use explicit host-parity fallback.
- `GPUArray` `[[` / `$` coverage is narrower than full base R behavior.
- Standard GPUArray argument-contract diagnostics (`drop`, logical masks,
  `gpu_slice` bounds/stride) are documented in
  `docs/GPUARRAY_CLASS_SUPPORT.md`.

## Toolchain and Build Limits

- Full backend functionality requires a working Mojo toolchain in `PATH`.
- If Mojo is unavailable, backend-dependent MöjoR features are unavailable.
- Stale build/cache artifacts can cause stale behavior until cleanup.

## Where to Look Next

- Usage and examples: `docs/COOKBOOK.md`
- Error diagnosis and fixes: `docs/TROUBLESHOOTING.md`
- Strict subset contract: `docs/SUBSET.md`
- GPU support matrix: `docs/GPU_FUNCTION_SUPPORT_MATRIX.md`
