# MöjoR Diagnostics Index

This page is the centralized diagnostics map for common MöjoR failures.

Use it to answer two questions quickly:
1. What boundary failed first?
2. Which document owns that boundary contract and fix guidance?

## How To Use This Index

1. Match your error text to the closest diagnostic pattern below.
2. Identify the first failing boundary (`API`, `Verify`, `Typing`, `Emit`,
   `Runtime`, `SSA`, `CFG`).
3. Apply the "fix-first action" before debugging later pipeline stages.

## Diagnostic Map

| Diagnostic text/pattern | First failing boundary | Typical cause | Fix-first action | Owner docs |
|---|---|---|---|---|
| `mojor_transpile: opt_level must be an integer between 0 and 3 (or NULL)` | API | Invalid optimization option value. | Fix API options first, rerun transpile. | [API](docs/PIPELINE/API.md), [AST/Build](docs/PIPELINE/AST_AND_BUILD.md) |
| `mojor_transpile: unroll must be an integer between 1 and 16` | API | Invalid schedule knob value. | Correct schedule args before compiler debugging. | [API](docs/PIPELINE/API.md), [Scheduling](docs/PIPELINE/SCHEDULING.md) |
| `mojor_transpile: tile must be a numeric vector of length 1, 2, or 3` | API | Tile vector shape/value invalid. | Normalize tile format and retry. | [API](docs/PIPELINE/API.md), [Scheduling](docs/PIPELINE/SCHEDULING.md) |
| `mojor_build: object_mode must be 'off' when ir_only=TRUE` | API | Strict mode and object-mode fallback policy conflict. | In strict mode, keep `object_mode='off'`; use non-strict mode only when intended. | [API](docs/PIPELINE/API.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_build: strict mode (ir_only=TRUE) forbids object-mode fallback: ...` | API | Strict policy blocked fallback-class failure handling. | Rewrite to strict-supported form or disable strict mode intentionally for this run. | [API](docs/PIPELINE/API.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_transpile: gpu_jit_mode='unified_preview' does not allow helper-backed GPU routes ...` | API | `unified_preview` mode detected helper-backed GPU lowering. | Rewrite to unified helper-free lowering or switch to `gpu_jit_mode='auto'`. | [API](docs/PIPELINE/API.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `IR verify: node missing valid 'kind' field` | Verify | Malformed or incomplete Tree IR node. | Fix node construction/build output first. | [Normalize/Verify](docs/PIPELINE/NORMALIZE_AND_VERIFY.md), [Tree Node Ref](docs/IR/TREE_NODE_REFERENCE.md) |
| `IR verify [break]: break statement outside loop` | Verify | Loop-control legality violation. | Fix control-flow placement in source/IR. | [Normalize/Verify](docs/PIPELINE/NORMALIZE_AND_VERIFY.md), [Contract](docs/IR/CONTRACT.md) |
| `IR verify [raw]: raw fallback node not allowed in ir_only mode` | Verify | Compatibility escape used in strict mode. | Rewrite to strict-supported IR form or disable strict lane intentionally. | [DSL Support](docs/IR/DSL_FUNCTION_SUPPORT.md), [Contract](docs/IR/CONTRACT.md) |
| `IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview' ...` | Verify | Expression-form `gpu_matmul` was verified without a supported GPU JIT mode context. | Use `gpu_jit_mode='auto'/'unified_preview'`, or rewrite to assignment-form. | [DSL Support](docs/IR/DSL_FUNCTION_SUPPORT.md), [Breadth Support](docs/IR/FUNCTION_SUPPORT_BREADTH.md) |
| `IR verify [scheduled_reduce]: mode must be one of tree/simd` | Verify | Scheduled reduction metadata invalid. | Fix reduction mode contract before lowering/emission checks. | [Scheduling](docs/PIPELINE/SCHEDULING.md), [Spec](docs/IR/SPEC.md) |
| `IR verify [scheduled_reduce]: dtype is invalid for mode` | Verify | Reduction route/dtype mismatch. | Adjust dtype or route to legal combination. | [Scheduling](docs/PIPELINE/SCHEDULING.md), [Codegen Design](docs/DESIGN/CODEGEN.md) |
| `mojor_transpile: character indexing requires dimnames/names for '<var>'` | Lower/Rewrite | Character index lane referenced an object without usable dimnames/names metadata. | Add dimnames/names for referenced axes or rewrite to numeric index lane. | [LIR](docs/PIPELINE/LIR.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_transpile: index name '<name>' not found in dimnames for '<var>'` | Lower/Rewrite | Character index label was not found in the target axis dimnames/names metadata. | Fix the label spelling/casing, or align runtime values to target dimnames before indexing. | [LIR](docs/PIPELINE/LIR.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)` | Lower/Rewrite | Compile-time-resolved local character loop/index value does not exist in the referenced dimname axis. | Ensure local character literals/values match axis dimnames, or switch to a `chr[]` argument lane for runtime-provided keys. | [LIR](docs/PIPELINE/LIR.md), [COOKBOOK](docs/COOKBOOK.md) |
| `mojor_transpile: dim() index in loop range must be a positive integer literal or supported scalar expression` | Build/Lower | `dim(arg)[k]` loop-range route requires `k` to be positive and integer-valued after scalar coercion. | Use a positive literal or supported scalar expression/argument for `k`, and keep `arg` as a direct matrix/fixed-rank ND function argument. | [DSL Support](docs/IR/DSL_FUNCTION_SUPPORT.md), [SUBSET](docs/SUBSET.md) |
| `IR emit [dim]: unresolved runtime dim pointer for '<arg>' (ndim=<n>)` | Emit | Emission tried to lower `dim(arg)` without a resolved dim-metadata pointer in the current strict lane. | Keep `arg` as a direct matrix/fixed-rank ND variable on covered routes so dim metadata is available at emit time. | [Emission](docs/PIPELINE/EMISSION.md), [SUBSET](docs/SUBSET.md) |
| `IR emit [dim]: unsupported dim() source; use direct array variables with available dim metadata` | Emit | `dim()` was applied to a non-covered expression/source without strict dim metadata tracking. | Rewrite to direct array-variable `dim()` use, or materialize the array source before `dim()` access. | [Emission](docs/PIPELINE/EMISSION.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_transpile: strict IR type check failed: ...` | Typing | Unknown/incompatible strict-lane result type. | Fix type contract or add explicit cast/rewrite expression. | [Typing](docs/PIPELINE/TYPING.md), [Typed Design](docs/DESIGN/TYPED.md) |
| `mojor_transpile: integer output requires explicit cast from f64 (use as.integer())` | Typing | Narrowing conversion requires explicit cast. | Insert explicit cast at source/IR level. | [Typing](docs/PIPELINE/TYPING.md), [Typed Design](docs/DESIGN/TYPED.md) |
| `mojor_transpile: unsupported subscript assignment index type; expected scalar index or supported vector index in assignment` | Typing | Assignment LHS index lane is outside strict supported scalar/vector index forms. | Rewrite assignment LHS to scalar index or covered vector-index lane (`i32[]`/`chr[]` direct vars or covered constructors). | [Typing](docs/PIPELINE/TYPING.md), [Typed Design](docs/DESIGN/TYPED.md) |
| `.mojor_ir_expr_emit: c_call function '<name>' not declared` | Emit | FFI declaration metadata missing/incomplete. | Declare C function via supported declaration path before emit. | [Emission](docs/PIPELINE/EMISSION.md), [Breadth Support](docs/IR/FUNCTION_SUPPORT_BREADTH.md) |
| `IR emit [vapply]: FUN.VALUE type must be numeric(1), integer(1), or logical(1)` | Emit | HOF strict-lane function contract violated. | Use supported `FUN.VALUE` shape/type only. | [Emission](docs/PIPELINE/EMISSION.md), [Breadth Support](docs/IR/FUNCTION_SUPPORT_BREADTH.md) |
| `error: expression must be mutable in assignment` | Emit | Assignment LHS index was emitted through non-write-context read-helper path. | Verify assignment-LHS indexing route and rerun on build with write-context index emission. | [Emission](docs/PIPELINE/EMISSION.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `Mojo build failed` | Runtime | Toolchain compile/link failure. | Separate transpile success from build failure; inspect build artifacts/logs first. | [Runtime/ABI](docs/PIPELINE/RUNTIME_AND_ABI.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `C wrapper build failed` | Runtime | Wrapper compile/link boundary failure. | Fix C wrapper generation/build environment before kernel logic debugging. | [Runtime/ABI](docs/PIPELINE/RUNTIME_AND_ABI.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_load: bridge not found at <path>` | Runtime | Bridge path/symbol load issue. | Fix bridge path/install/load step first. | [Runtime/ABI](docs/PIPELINE/RUNTIME_AND_ABI.md), [ABI](docs/BACKEND/ABI.md) |
| `Mojo backend not loaded; call mojor_load() first` | Runtime | Runtime backend not loaded before call. | Load backend explicitly and retry call path. | [Runtime/ABI](docs/PIPELINE/RUNTIME_AND_ABI.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_gpu_reduce: invalid dims` | Runtime | Reduction axis argument incompatible with input rank/shape. | Normalize dims argument first; confirm rank-aware dims contract. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `mojor_gpu_reduce: op must be a single character string` | Runtime | Invalid reduce op argument shape/type. | Use a supported scalar op string (`sum`, `mean`, `min`, `max`, `argmin`, `argmax`). | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `mojor_gpu_matmul: inner dimensions must match` | Runtime | Matrix shapes are incompatible for matmul contract. | Validate `ncol(lhs) == nrow(rhs)` (after transpose flags) before runtime call. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `mojor_gpu_matmul: output dtype mismatch` | Runtime | `gpu_matmul_into` output buffer dtype does not match computed dtype contract. | Allocate output buffer with matching dtype before `*_into` call. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `[.mojor_gpu_array: logical mask must not contain NA` | Runtime | Logical index mask includes `NA`; mask contract rejects unknown selection state. | Pre-clean mask (`is.na(mask) <- FALSE` or explicit filtering) before indexing. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `gpu_slice: strides must be non-zero` | Runtime | Slice stride vector contains zero entries or rank/shape-incompatible stride metadata. | Use non-zero stride values and direction-consistent bounds for each axis. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `mojor_gpu: unsupported operator '<op>'` | Runtime | Operator not implemented in GPU arithmetic dispatch surface. | Restrict to supported GPU operators or route operation through supported path. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [Troubleshooting](docs/TROUBLESHOOTING.md) |
| `gpu_cast: x must be numeric/integer or mojor_gpu_array` | Runtime | Cast input not in supported value/object lanes. | Convert to numeric/integer vector or `GPUArray` before cast call. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `gpu_broadcast: x must be numeric/integer or mojor_gpu_array` | Runtime | Broadcast input object type unsupported by helper contract. | Normalize input to numeric/integer vector or `GPUArray`. | [GPU Class Support](docs/GPUARRAY_CLASS_SUPPORT.md), [GPU Matrix](docs/GPU_FUNCTION_SUPPORT_MATRIX.md) |
| `SSA verify: br from '<from>' targets unknown block` | SSA/CFG | Invalid CFG target edge. | Fix block naming/targeting in SSA generation before optimization/backend stages. | [SSA](docs/PIPELINE/SSA.md), [CFG](docs/PIPELINE/CFG.md) |
| `SSA verify: condbr ... arg arity mismatch` | SSA/CFG | Branch argument count does not match block params. | Repair branch arg arity and block param contracts first. | [SSA](docs/PIPELINE/SSA.md), [CFG](docs/PIPELINE/CFG.md) |
| `SSA verifier: LEGACY_MEMORY_SPELLING: ...` | SSA boundary | Memory canonicalization boundary not satisfied. | Run/fix canonicalization before later SSA stages. | [Contract](docs/IR/CONTRACT.md), [SSA Op Ref](docs/BACKEND/SSA_OP_REFERENCE.md) |

## Related References

- [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md)
- [PIPELINE/README.md](docs/PIPELINE/README.md)
- [IR/PLAN.md](docs/IR/PLAN.md)
- [DIAGNOSTICS_API.md](docs/DIAGNOSTICS_API.md)
- [GPU_FUNCTION_SUPPORT_MATRIX.md](docs/GPU_FUNCTION_SUPPORT_MATRIX.md)
