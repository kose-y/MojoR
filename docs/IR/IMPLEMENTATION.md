# MöjoR IR Implementation (Current State)

This document captures **what exists today** for the MöjoR IR work: files,
helpers, wiring points, and current limitations. It is a companion to
`PLAN.md`, which focuses on direction and future architecture.

**See also:**
- [COVERAGE.md](./COVERAGE.md) - Tracks feature coverage and parity status

## Plain-Language Mental Model

This is the "what exists right now" inventory.

- `PLAN.md` explains direction.
- `SPEC.md` explains rules.
- `IMPLEMENTATION.md` explains what is actually wired in code today.

Use this file when you need concrete ownership and current behavior, not design intent.

## Mini Example

If you are debugging a transpile issue:

1. Find the relevant section here (build/verify/lower/emit/SSA/runtime touchpoint).
2. Jump to listed owning file/function.
3. Confirm whether behavior is implemented, partial, or explicitly deferred.

This avoids chasing outdated assumptions from planning docs.

## Beginner Debug Checklist

1. Confirm the package path and owning module in `packages/mojor/`.
2. Locate authoritative owner function(s) before making changes.
3. Check nearby "closeout" and "scope" notes for strict/compat lane constraints.
4. Keep behavior changes and tests aligned within `packages/mojor/`.

## Refactor Notes (2026-02-17)

No-behavior-change cleanup in progress in `packages/mojor/`:

- IR verify helper consolidation in `ir/verify.R`:
 - strict-type mode check helper
 - logical-literal validation helper
 - `replace`/`prob` sampling argument validators
 - repeated variable-field shape checks for set/match/quantile nodes
- IR emit helper consolidation in `ir/stmt_emit.R`:
 - shared length/pointer lookup helpers for and emit paths
- Transpile helper consolidation:
 - shared context assertions in `transpile/ir_helpers.R` and
 `transpile/loop_codegen_units.R`
 - shared type-base resolver used by both `.mojor_mojo_type()` and
 `.mojor_mojo_dtype()` in `transpile/helpers_core.R`
- Backend decomposition and helper ownership:
 - `backend.mojo` GPU buffer paths now route through shared internal helpers:
 - status helpers (`_set_status_ok`, `_set_status_invalid`)
 - device-buffer lifecycle helpers (`_gpu_clone_device_buffer`, `_gpu_box_device_buffer`, `_gpu_wrap_gpubuf`)
 - generated GPU buffer export dispatch helpers (`_gpu_buf_export_*`)
 - generated wrapper ownership is centralized in `tools/gen_gpu_buf_exports.mojo`.
- RNG/Gamma dedup:
 - `backend_rng.mojo` and `backend_gamma.mojo` now import canonical RNG math/ziggurat helpers from `rng_helpers.mojo` instead of carrying local duplicated implementations.
- Repository hygiene:
 - local object artifacts (`*.o`, `*.obj`) should be treated as non-source build byproducts.

## Sampling Expression Emit Closeout (2026-02-17)

- `ir/expr_emit.R` now supports indexed scalar extraction for sampling expressions when:
 - index is a compile-time integer
 - index is >= 1 (one-based semantics)
 - `size` is a compile-time integer with `size >= index`
- `ir/stmt_emit.R` now supports runtime-index extraction in loop assignments for:
 - `out[i] <- sample.int(...)[i]`
 - `out[i] <- sample(x, ...)[i]`
 under the strict subset constraints (`prob` as direct `f64[]`/`NULL`, scalar
 boolean/int `replace` in strict IR verify/emit, and direct vector variable `x`
 for `sample`).
- `ir/expr_emit.R` now also supports runtime-index extraction in composed scalar
 loop expressions (for example `sample.int(...)[i] + c`, `sample(x, ...)[i] + c`)
 via shared sampling index helper emission.
- `rng_helpers.mojo` now provides `_mojor_sample_pick_index(...)` to preserve
 sampling draw-order semantics for dynamic indexed extraction in strict IR emit paths.
- Direct scalar sampling emission remains supported for `size = 1` without explicit indexing.
- Unsupported forms outside these strict paths (invalid arity, non-direct/non-`f64[]` `prob` in strict typed lanes, non-scalar `replace` forms, or scalar indexed forms with `index > size`)
 emit stable strict diagnostics.
- closeout is scoped to strict statement-mode + strict no-loop sampling behavior.
 Broader non-scalar sampling expression generalization remains post-Tier-8 expansion work,
 not part of this closeout boundary.
- Focused validation was extended in both trees via `test_apply_sample.R`.

## Breadth/ABI Hardening Increment (2026-02-17)

- Sampling increment (`ir/stmt_emit.R`, `test_apply_sample.R`):
 - non-strict statement emission supports weighted `prob` for:
 - whole-vector assignment (`out <- sample.int(...)` / `out <- sample(...)`)
 - indexed-dynamic loop assignment (`out[i] <- sample.int(...)[i]`, `out[i] <- sample(...)[i]`)
 - weighted lane constraints:
 - whole-vector weighted lanes: `replace = TRUE/FALSE` (or scalar `replace` flag)
 - indexed-dynamic weighted lanes: `replace = TRUE`
 - `prob` must be a direct variable
 - typed lane expects `prob` as `f64[]`
 - strict no-loop weighted sampling is supported for direct `f64[]` `prob` with
   strict scalar/literal `replace`; indexed-dynamic weighted assignment remains
   `replace = TRUE` only.

- Higher-order increment (`ir/stmt_emit.R`, `transpile/route_dispatch.R`, `test_mojor_fn_expression_fallback.R`):
 - `mapply` accepts curated named `FUN` in addition to anonymous inline lambdas.
 - current curated named set for `mapply` (arity >= 2): `pmin`, `pmax`, `min`, `max`.
 - existing strict constraints remain: direct vector args, scalar return, arity >= 2, and stable rejection for unsupported named-argument forms.

- FFI ABI hardening increment (`runtime/ffi.R`, `ir/expr_build.R`, `ir/verify.R`, `ir/expr_emit.R`, `test_ffi.R`):
 - `c_call` nodes now preserve declaration-derived ABI metadata (`returns`, `expected_arity`, `arg_types`, `arg_names`) end-to-end.
 - verifier and emitter normalize metadata shapes (`character`/`list` inputs -> character vectors) before parity checks.
 - declaration parity checks are enforced for `returns`, `arg_types`, and `arg_names`, alongside strict arity/type checks.

## String Wrapper Increment (2026-02-17)

- Strict no-loop subset now supports native compiled wrappers for:
 - `nchar(x)`, `nzchar(x)`
 - `substr(x, start, stop)`, `substring(x, first, last)`
 - `paste(x, y, sep = "...", collapse = "..."|NULL)`, `paste0(x, y, collapse = "..."|NULL)`
- Wrapper execution remains strict and native (no `Rf_eval` path in generated wrapper C).
- `paste`/`paste0` enforce scalar character literal `sep`/`collapse` (or `NULL` for `collapse`) in the strict no-loop subset.
- Runtime semantics include vector recycling for `paste`/`paste0` wrappers and scalar collapsed output when `collapse` is provided.

## IR-Native Emit Closeout (2026-02-17)

- `ir/stmt_emit.R` now includes an initial IR-native whole-vector assignment emitter for:
 - `out <- nchar(x)`
 - `out <- nzchar(x)`
 - `out <- substr(x, start, stop)`
 - `out <- paste(x, y, sep = "...", collapse = "..."|NULL)`
 - `out <- paste0(x, y, collapse = "..."|NULL)`
 under strict typed contexts (`x: chr[]`, `out: i32[]/lgl[]/chr[]`).
- `ir/expr_emit.R` now emits elementwise `nchar`/`nzchar`/`substr`/`paste`/`paste0` only in loop/index contexts and raises stable errors for unsupported scalar contexts.
- closeout is scoped to strict no-loop forms; broad non-expression string-kernel/runtime ABI coverage is intentionally out of scope for this closeout.
- Wrapper-backed shared runtime bridge remains in place for strict boundaries and for forms outside current IR-native string emission scope.

## Shared Runtime Closeout (2026-02-17)

- Strict no-loop subset now uses shared runtime bridge execution for:
 - `grepl`, `grep`, `sub`, `gsub`
 under strict argument constraints (direct `chr[]` input vector, scalar literal/arg pattern and replacement forms, literal `fixed`/`perl` booleans).
- Strict no-loop subset now uses:
 - native matrix-kernel path for `row`/`col`
 - shared runtime bridge path for `expand.grid` (2-4 unnamed direct vector args).
- strict paths no longer synthesize operation-specific generated wrapper C.
- `expand.grid` parity includes base-compatible data.frame attribute shaping for strict subset forms (`names`, `row.names`, `class`, `out.attrs`).
- Focused closeout validation:
 - `packages/mojor/tests/testthat/test_mojor_fn_expression_fallback.R` -> `PASS 216`
 - `packages/mojor/tests/testthat/test_mojor_fn_expression_fallback.R` -> `PASS 216`

## Indexing/Top-Level IR Increment (2026-02-19)

- Assignment LHS index emission now uses `.mojor_ir_index_emit(..., write_context = TRUE)` so emitted Mojo keeps mutable lvalue indexing and avoids `error: expression must be mutable in assignment` from read-helper LHS routing.
- Character-index validation/collection for matrix and fixed-rank ND routes is now missing-index safe (for example `x[, j]`, `x[i, , k]`) and ignores control args (`drop`, `exact`) in index-position counting.
- Character dimname diagnostics are position-specific in strict routes:
 - `mojor_transpile: character indexing requires dimnames/names for '<var>'`
 - `mojor_transpile: index name '<name>' not found in dimnames for '<var>'`
 - `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)`
- Direct `chr[]` function arguments used as index vectors are collected and routed through the compiled hashed-index lane in strict matrix/array dimname indexing paths.
- Named vector character indexing now uses compile-time `names()` tracking on
  `c(...)` constructor nodes (`element_names` metadata) for strict dimname/name
  resolution.
- Local character vector element-wise loop indexing is supported on compile-time
  resolvable lanes against target dimnames.
- Strict matrix/array constructor call nodes keep literal `dimnames` metadata on
  compiled lanes (no forced raw fallback for local `matrix(..., dimnames=...)`
  / `array(..., dimnames=...)` literal forms).
- Strict submatrix/ND vector-index lowering now accepts supported computed
  `i32[]` arithmetic index expressions (`+`, `-`, `*`, `%/%`, `%%`) in addition
  to direct vars and constructor lanes, when vector lengths can be resolved.
- Strict submatrix/ND vector-index lowering also accepts supported
  `ifelse(cond, yes, no)` `i32[]` index expressions (supported logical
  conditions and supported `i32[]` branch lanes).
- Bounds-guard emission now skips synthetic selector/exclusion marker indices so
  internal marker names do not leak into scalar guard checks.
- Top-level statement routing through shared IR now covers both pre-loop and post-loop output assignments.
- Loop-range scalar rewriting now includes `dim(arr)[k]` dimension lookups in strict routes (for example patterns like `seq_len(dim(arr)[1])`).

## N-d Subscript Classification and Emission (2026-02-22)

N-d post-loop subscript patterns (`out <- mat[-1, ]`, `out <- arr[c(1,3), -2, ]`, `out[c(1,3), ] <- val`)
are now handled by a unified classification + emission pipeline:

- **Build** (`packages/mojor/R/ir/build.R`):
  - `.mojor_ir_build_positive_vector_selection()` detects `c(1,3)`-style selector sets and creates `pos_vec_selection` metadata on index nodes.
  - `:` range selectors are kept on canonical slice/index lowering routes (no `pos_vec_selection` range marker rewrite).
  - `.mojor_ir_build_exclusion_index()` creates `neg_exclusion` metadata for scalar `-k`.
  - `.mojor_ir_build_vector_exclusion_index()` creates `neg_vec_exclusion` metadata for `-c(1,2)` / `-(1:3)`.
  - Both `.mojor_ir_build_index()` (RHS) and `.mojor_ir_build_subscript()` (LHS) route through these builders.

- **Lowering** (`packages/mojor/R/ir/lowering.R`):
  - `.mojor_ir_lower_subscript()` passes through N-d exclusion-heavy patterns (`neg_exclusion`, `neg_vec_exclusion` + scalar/missing dims), preserving metadata for the emission layer.

- **Transpile detection** (`packages/mojor/R/transpile/transpile.R`):
  - Post-loop output assignment detection classifies each dimension as missing, scalar, neg-scalar-excl, neg-vec-excl, or pos-vec-sel.
  - Produces `out_len_source` with `kind = "nd_exclusion"` carrying per-dim classification for output length computation.

- **Emission** (`packages/mojor/R/ir/ir.R`):
  - `.mojor_classify_nd_indices(indices, n_dims)` classifies index nodes into five dim kinds.
  - `.mojor_emit_nd_loops(cls, n_dims, dim_vars, indent, loop_depth_start)` generates nested loops with the correct pattern for each dim kind:
    - Missing → `for lv in range(dim_size)`
    - Scalar → fixed 0-based index expression
    - Neg scalar excl → loop + `if lv != excl_idx`
    - Neg vector excl → loop + multi-condition skip or range skip
    - Pos vector select (set) → `for sel in range(count)` + ternary chain
  - `.mojor_ir_nd_exclusion_subset_emit()` (read-side) and `.mojor_ir_nd_exclusion_write_emit()` (write-side) use the shared helpers.

- **Output length** (`packages/mojor/R/transpile/helpers_core.R`):
  - `.mojor_nd_exclusion_len_expr_c()` computes C-level output length as a product of per-dim contributions: `dim_size` for missing, `1` (omitted) for scalar, `dim_size - count` for exclusion, literal count for vector selection.

## Files
IR helpers live in split module trees:
- `packages/mojor/R/ir/`

Core IR modules:
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/expr_emit.R`
- `packages/mojor/R/ir/stmt_emit.R`
- `packages/mojor/R/ir/op_schema.R`

Integration points:
- `packages/mojor/R/transpile/transpile.R`
- `packages/mojor/R/transpile/ir_helpers.R`
- `packages/mojor/R/transpile/transpile.R`
- `packages/mojor/R/transpile/ir_helpers.R`

## IR Node Shapes (Current)
IR nodes are plain R lists with a `kind` field and optional `src` metadata.

For the canonical, complete node-by-node reference (including current usage in
pipeline stages), see [TREE_NODE_REFERENCE.md](./TREE_NODE_REFERENCE.md).

### Expressions
- `const`: `list(kind="const", value, src)`
- `var`: `list(kind="var", name, src)`
- `unop`: `list(kind="unop", op, expr, src)`
- `binop`: `list(kind="binop", op, lhs, rhs, src)`
- `cast`: `list(kind="cast", to, expr, src)`
- `call`: `list(kind="call", fn, args, src)`
- `index`: `list(kind="index", base, indices, index_base, src)`
- `ifelse`: `list(kind="ifelse", cond, yes, no, src)`
- `type_predicate`: `list(kind="type_predicate", predicate, x, src)` — strict verifier-covered; typed/type-fold route
- `type_query`: `list(kind="type_query", query, x, src)` — strict verifier-covered; typed/type-fold route
- `dim`: `list(kind="dim", x, src)` — strict verifier/typing-covered for direct matrix/fixed-rank ND vars

### Indexing helpers (slice assignments)
- `subscript`: `list(kind="subscript", var, indices, src)`
- `slice_index`: `list(kind="slice_index", start, end, src)`
- `scalar_index`: `list(kind="scalar_index", expr, src)`
- `missing_index`: `list(kind="missing_index", src)`
- Canonical slice lowering rewrites direct array-variable RHS lanes
  (`mat[i, ] <- vals`, including `cast(var_array)` wrappers) into per-element
  reads with modulo recycling over slice loop extent.

Index nodes in `indices[]` may carry optional metadata for exclusion/selection:
- `neg_exclusion` (string): Scalar negative exclusion marker (`-k` → 0-based skip).
- `neg_vec_exclusion` (list): Vector exclusion (`-c(1,2)`, `-(1:3)`). Fields: `type`, `indices`/`start_0`/`end_0`, `count`.
- `pos_vec_selection` (list): Positive vector selection set metadata (`c(1,3)`). Fields: `type="c"`, `indices_0`, `count`.

`:` range selectors (for example `1:3`) remain on canonical slice/index nodes
instead of being rewritten to `pos_vec_selection`.

N-d subscripts with these metadata fields pass through lowering unchanged and are emitted via `.mojor_classify_nd_indices()` + `.mojor_emit_nd_loops()` in `ir.R`.

### Statements
- `block`: `list(kind="block", stmts, src)`
- `assign`: `list(kind="assign", lhs, rhs, src)`
- `if`: `list(kind="if", cond, then, else_block, src)`
- `loop`: `list(kind="loop", var, range, body, metadata, src)`
- `while`: `list(kind="while", cond, body, src)`
- `repeat`: `list(kind="repeat", body, src)`
- `break`: `list(kind="break", src)`
- `next`: `list(kind="next", src)`
- `return`: `list(kind="return", value, src)`
- `scalar_reduce`: `list(kind="scalar_reduce", op, acc, arg, src)` — reduction-node route (implemented)
- `rng_vec`: `list(kind="rng_vec", dist, n, params, src)` — (vectorized RNG)
- `alloc`: `list(kind="alloc", len, dtype, src)` — (temporary allocation)
- `vexpr`: `list(kind="vexpr", len, body, src)` — (implicit loop generation)
- `fusion_candidate`: `list(kind="fusion_candidate", loop1, loop2, src)` — (loop fusion)

### Range/Raw wrappers
- `range`: `list(kind="range", start, end, step, src)`
- `range_expr`: `list(kind="range_expr", expr, src)`
- `raw`: `list(kind="raw", expr, src)`

## IR Command Set (Helpers)
Constructors (node builders):
- `.mojor_ir_const(value, src = NULL)`
- `.mojor_ir_var(name, src = NULL)`
- `.mojor_ir_unop(op, expr, src = NULL)`
- `.mojor_ir_binop(op, lhs, rhs, src = NULL)`
- `.mojor_ir_cast(to, expr, src = NULL)`
- `.mojor_ir_call(fn, args, src = NULL)`
- `.mojor_ir_index(base, indices, index_base = "one_based", src = NULL)`
- `.mojor_ir_ifelse(cond, yes, no, src = NULL)`
- `.mojor_ir_block(stmts, src = NULL)`
- `.mojor_ir_assign(lhs, rhs, src = NULL)`
- `.mojor_ir_if(cond, then, else_block = NULL, src = NULL)`
- `.mojor_ir_loop(var, range, body, src = NULL)`
- `.mojor_ir_while(cond, body, src = NULL)`
- `.mojor_ir_break(src = NULL)`
- `.mojor_ir_next(src = NULL)`
- `.mojor_ir_repeat(body, src = NULL)`
- `.mojor_ir_return(value = NULL, src = NULL)`
- `.mojor_ir_scalar_reduce(op, acc, arg, src = NULL)` — reduction-node constructor
- `.mojor_ir_rng_vec(dist, n, params = NULL, src = NULL)` — (vectorized RNG)
- `.mojor_ir_alloc(len, dtype, src = NULL)` — (temporary allocation)
- `.mojor_ir_vexpr(len, body, src = NULL)` — (implicit loop generation)
- `.mojor_ir_fusion_candidate(loop1, loop2, src = NULL)` — (loop fusion)
- `.mojor_ir_slice_index(start, end, src = NULL)`
- `.mojor_ir_scalar_index(expr, src = NULL)`
- `.mojor_ir_missing_index(src = NULL)`
- `.mojor_ir_subscript(var, indices, src = NULL)`
- `.mojor_ir_range(start, end, step = NULL, src = NULL)`
- `.mojor_ir_raw(expr, src = NULL)`
- `.mojor_ir_range_expr(expr, src = NULL)`

### Array utility constructors (implemented set)
- `.mojor_ir_transpose(x, src = NULL)` — Matrix transpose; 1D: `t(x)[i]`, 2D: `t(x)[i,j]`
- `.mojor_ir_cbind(args, src = NULL)` — Column binding; cycles through args with modular indexing
- `.mojor_ir_rbind(args, src = NULL)` — Row binding; cycles through args with modular indexing
- `.mojor_ir_seq(from, to, length_out, src = NULL)` — Sequence generation with linear interpolation
- `.mojor_ir_diag(x = NULL, n = NULL, src = NULL)` — Diagonal operations; extraction (x=matrix), creation (x=vector), identity (n=scalar)

### SSA syntax/semantic boundary APIs (initial complete set)
- `.mojor_ssa_parse_lossless(text)`
- `.mojor_ssa_parse_norm(text)`
- `.mojor_ssa_print_lossless(tree)`
- `.mojor_ssa_print_semantic(ir, mode = c("pretty", "compat"))`
- `.mojor_ssa_lower_to_semantic(tree)`

## Pipeline Stages

### 1. Build (R AST → Tree IR)
Produces untyped Tree IR (HIR + Core nodes).

### 2. Normalize
Structural normalization only (no semantic assumptions).

### 3. Verify
Structural + scope + loop legality checks (depending on verifier flags).

### 4. Optimize (CSE/fold/LICM/DCE)
Uses the effect/resource legality model; conservative about ReadsMem/RNG/Unknown.

### 5. Lower (HIR → LIR)
Eliminates sugar; produces Core-only LIR (except allowed scheduled reductions / legacy escapes when compat is enabled).

### 6. Schedule
Adds metadata transforms (tiling/SIMD routing, reduction scheduling). Must be type-agnostic.

### 7. Type Check
Inserts casts/coercions; enforces typing invariants (or falls back if compatibility mode is enabled).

### 8. Emit (Mojo)
Emits Mojo source + guards (NA/bounds) with guard CSE.

## Stage Input/Output Snapshot

| Stage | Primary input | Primary output | Typical failure class |
|---|---|---|---|
| Build/Normalize | Source-derived Tree IR | Canonical Tree IR | Unsupported subset or non-canonical forms. |
| Verify | Canonical Tree IR | Verifier-clean Tree IR | Node shape/scope/loop legality failures. |
| Optimize/Lower/Schedule | Verified Tree IR | Emit-ready Tree IR | Legal no-op due to effect barriers or unsupported sugar lowering. |
| Typing | Scheduled Tree IR + type env | Typed Tree IR | Strict-lane unknown/mismatch type failures. |
| Emit | Typed Tree IR + guard/layout context | Mojo source lines | Unsupported emit form/operator. |
| SSA boundary path | Structured/canonical Tree IR | Semantic `ssa_fn` + backend CFG prep artifacts | CFG arity/target/use-def or boundary invariant failures. |

## Representative Diagnostics

Frequent implementation-boundary diagnostics:

- `IR verify: node missing valid 'kind' field`
- `IR verify [raw]: raw fallback node not allowed in ir_only mode`
- `IR verify [scheduled_reduce]: dtype is invalid for mode`
- `SSA verify: br from '<from>' targets unknown block`
- `SSA verifier: <CODE>: <message> [block=<name>]`

Treat these as contract/shape failures first; do not debug runtime backend behavior until IR boundaries pass.

---

## Current Limitations

This document maps current implementation ownership and stage behavior, not an
exhaustive limitations registry.

Use:
- [../KNOWN_ISSUES.md](../KNOWN_ISSUES.md) for consolidated current limitations/deferred lanes.
- [../SUBSET.md](../SUBSET.md) for supported strict subset boundaries.
- [../TROUBLESHOOTING.md](../TROUBLESHOOTING.md) for debugging workflow and mitigations.

---

## Related Documents

- [PLAN.md](./PLAN.md) - IR Direction and Architecture
- [SPEC.md](./SPEC.md) - IR Specification
- [CONTRACT.md](./CONTRACT.md) - IR Contracts
- [EFFECT_SYSTEM.md](./EFFECT_SYSTEM.md) - Effect System
- [COVERAGE.md](./COVERAGE.md) - Feature Parity Checklist
