# IR Coverage Matrix: Current Transpile Pipeline

**Date:** 2026-02-23
**Status:** Core R semantics implemented (NA semantics, apply, set/match, quantiles, indexing hardening increments)

Maps major transpiler constructs to current IR coverage using split-module wiring.

## Plain-Language Mental Model

This document answers: "Does this feature really go through IR, or does it still use legacy logic?"

- "Fully IR" means the construct is represented and emitted through the shared IR pipeline.
- "Legacy active" means a fallback path still exists.
- "Bypassed" means old code is intentionally not used in the main loop pipeline.

Use this as a reality check before claiming a feature is fully IR-native.

## Mini Example

If you ask "is `out[mask] <- expr` IR-native?":

1. Find the construct in coverage tables.
2. Confirm it appears under full IR routing (not legacy).
3. Check listed node kinds and call sites to identify exact ownership.

This quickly distinguishes implementation gaps from test/configuration issues.

## Beginner Debug Checklist

1. Start from the construct row, then follow module/function call sites.
2. If a behavior seems legacy, verify whether it is listed under active gap notes.
3. Confirm strict-mode behavior for the same construct, because non-strict mode can mask missing IR ownership.
4. Update this matrix whenever routing behavior changes, to keep docs and code aligned.

---

## IR Call Sites

| Module | Function | Context |
|------|----------|---------|
| `transpile/transpile.R` | `.mojor_transpile(...)` | Main transpile entry and loop/statement routing |
| `transpile/ir_helpers.R` | `.mojor_prepare_transpile_ir_stmt(...)` | Normalize + verify prep and strict checks |
| `transpile/ir_helpers.R` | `.mojor_run_transpile_ir_pipeline(...)` | Optimize/lower/schedule/fusion pipeline wiring |
| `ir/verify.R` | `.mojor_ir_verify(...)` | Tree IR verifier gate |
| `ir/stmt_emit.R` | `.mojor_ir_stmt_emit(...)` | Emit IR -> Mojo code |
| `transpile/reduction_paths.R` | `.mojor_emit_transpile_scalar_reduce_ir_path(...)` | Scalar-reduce IR path with deterministic loop fallback when IR preparation/emission is unavailable |

---

## Coverage by Construct

### Fully IR (shared pipeline + statement emitter)

| Construct | IR Node Kind(s) |
|-----------|-----------------|
| For loop | `loop` + `range_expr` |
| While loop | `while` |
| Repeat loop | `repeat` |
| Array index assignment `out[i] <- expr` | `assign` + `index` |
| Matrix slice `mat[i, ] <- c(x[i], y[i])` | `assign` + `subscript` + `missing_index` + `c` |
| Logical mask assignment `out[mask] <- expr` | `assign` + mask emission |
| If/else branches | `if` |
| Inline reduction `acc <- acc + x[i]` | `loop` + `reduce` annotation |
| Scalar reduction `min(x)` / `max(x)` / `which.min(x)` / `which.max(x)` | `scalar_reduce` |
| any/all short-circuit loops | `loop` + `if` + `assign` + `break` |
| NA guards (forbid/assign/unsafe) | Emitted by `.mojor_ir_emit_na_guard()` |
| Bounds checks | Emitted by `.mojor_ir_emit_bounds_guards()` |
| Loop unrolling | Applied in `.mojor_ir_loop_emit()` |
| Broadcast_nd indexing | Via index normalization + `_mojor_bcast_index()` |
| Character dimname indexing (matrix/fixed-rank ND) | `index`/`subscript` + position-aware dimname rewrite (missing-index safe; includes named-vector `names()` tracking and local compile-time character loop lanes) |
| Submatrix/ND vector-index writes via strict computed `i32[]` lanes | `subscript` + vector-index lowering (`expr_i32`) for supported arithmetic (`+`/`-`/`*`/`%/%`/`%%`) and supported `ifelse(cond, yes, no)` selector lanes |
| Loop bounds from `dim(arr)[k]` (for example `seq_len(dim(arr)[1])`) | `range_expr` + dim-map scalar rewrite |
| ifelse expression | `ifelse` |
| Math functions | `call` |
| Type casts | `cast` |
| RNG calls | `call` (rnorm, rgamma) |
| Vectorized RNG | `rng_vec` |
| `apply()` | `apply` |
| `sample.int()` | `sample_int` |
| `sample()` | `sample` |
| Implicit loops (elementwise) | `vexpr` |
| c() / rep() / rep_len() | `c`, `rep`, `rep_len` |
| Pre-loop output assignments | `assign` via shared IR pipeline |
| Post-loop output assignments | `assign` via shared IR pipeline |

### Core R Semantics (2026-02-16+)

| Construct | IR Node Kind(s) |
|-----------|-----------------|
| NA propagation mode | `scalar_reduce` with `na_rm=TRUE` |
| NA skip mode | `scalar_reduce` with `na_rm=TRUE` |
| `unique(x)` | `unique` |
| `duplicated(x)` | `duplicated` |
| `anyDuplicated(x)` | `any_duplicated` |
| `match(x, table)` | `match` |
| `%in%(x, table)` | `in` (`%in%` source operator lowers to `in`) |
| `median(x)` | `median` |
| `quantile(x, probs)` | `quantile` |
| `IQR(x)` | `iqr` |
| `mad(x)` | `mad` |

### Legacy - Active (none for loop pipeline)

| Construct | Why Not IR |
|-----------|-----------|
| None (loop pipeline) | Active loop compilation routes are IR-owned across strict and non-strict loop lanes. |

### Legacy - Intentionally Bypassed in Current Loop Pipeline

| Construct | Why bypassed |
|-----------|-----------------|
| Legacy while/repeat loop emission path | Shared IR loop pipeline takes precedence |
| Legacy mean-with-NA-ignore shim path | Superseded by IR NA handling |
| Legacy elementwise GPU/CPU/SIMD paths in loop codegen branch | Gated by explicit feature routing; not used by default loop IR path |

### Pre-IR (setup/metadata; not IR codegen)

- Signature generation
- Input/output pointer declarations
- LayoutTensor setup for inputs
- Math import injection
- `.mojor_collect_*()` metadata helpers
- AST analysis helpers

---

## IR Coverage Gaps (current)

All major loop constructs now route through IR-owned paths in both strict and non-strict flows.

Previously active gaps now closed:
- Pre-loop assignments: full IR pipeline.
- Post-loop assignments: full IR pipeline.
- Custom min/max reductions: `scalar_reduce` IR node.
- Short-circuit logical (`any/all`): routed through IR pipeline.
- NA-aware reductions: `scalar_reduce` with `na_rm` handling.
- Set/match primitives: `unique`, `duplicated`, `match`, `%in%` IR nodes.
- Quantiles and robust stats: `median`, `quantile`, `iqr`, `mad` IR nodes.

---

## Key Invariants

- IR is mandatory for loop compilation paths; IR build/emit failure is a hard error in strict paths.
- Legacy loop emission blocks are bypassed once the IR loop path is selected.
- `.mojor_ir_verify()` is part of the shared IR preparation pipeline.
- Typed IR is optional for emission (compile can continue in non-strict flows when type-checking is unavailable).
- Typed pass exists today, but matrix/ND strict layout completeness remains partial and is tracked under `docs/KNOWN_ISSUES.md`.

## Representative Coverage-Boundary Diagnostics

When a construct is marked covered but fails, these diagnostics usually identify the real boundary:

- `IR verify: node missing valid 'kind' field` -> build/shape issue before coverage claim applies.
- `IR verify [raw]: raw fallback node not allowed in ir_only mode` -> strict-mode fallback escape rejected.
- `IR verify [sample]: x must be a direct vector variable in strict mode` -> construct is covered, but strict-lane input constraints were violated.
- `IR verify [gpu_reduce]: transpile helper currently supports only dims=NULL and keepdims=FALSE` -> GPU route constraint, not missing IR node support.

Coverage means "implemented route exists"; it does not bypass strict verifier and lane constraints.

---

## Related Documents

- [IMPLEMENTATION.md](./IMPLEMENTATION.md) - Implementation details
- [PLAN.md](./PLAN.md) - IR direction
- [plans/legacy_implementation_plan.md](../../plans/legacy_implementation_plan.md) - Legacy implementation plan
