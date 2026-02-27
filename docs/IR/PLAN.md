# MöjoR IR Direction

This document sketches a pragmatic intermediate representation (IR) between R AST and emitted Mojo.
It aims to reduce direct string codegen complexity while staying within the current supported subset.

## Plain-Language Mental Model

This plan explains how MöjoR turns R code into safer, predictable compiler data before generating Mojo code.

Think of it as:
1. Parse source into structured IR.
2. Clean and verify IR.
3. Optimize safely.
4. Emit code.

The key idea is to move "smart logic" out of ad-hoc string generation and into explicit IR passes.

## Mini Example

Source idea:

```r
for (i in 1:n) out[i] <- x[i] + y[i]
```

Plan-level flow:
- Build nodes for loop, index, add, assign.
- Normalize and verify node legality.
- Optionally optimize (for example CSE/LICM where legal).
- Lower and emit final Mojo loop code.

## Beginner Debug Checklist

1. If output looks wrong, identify which stage first diverges (build/verify/lower/emit).
2. Use strict mode checks to catch compatibility escapes early.
3. Confirm index-base assumptions (R one-based vs emitted zero-based) before debugging math.
4. Treat this doc as architecture intent; use `IMPLEMENTATION.md` for current concrete behavior.

## Two-Layer IR Architecture

MöjoR uses a **two-layer IR design**:

1. **Untyped IR** (current implementation)
 - Structural representation only
 - No type annotations on nodes
 - No shape metadata
 - Built directly from R AST
 - Focus: normalize structure, not semantics

2. **Typed IR** (implemented pass; matrix/ND strict/layout completeness remains partial, see [KNOWN_ISSUES.md](../KNOWN_ISSUES.md))
 - Separate pass over untyped IR
 - Adds type annotations to all expression nodes
 - Adds shape/layout metadata where currently inferable in supported lanes
 - Inserts explicit cast nodes
 - Focus: type safety, validation, optimization

**Note:** The node schemas below show type fields for completeness. Untyped IR leaves them empty; the typed IR pass populates them when it runs (with a safe fallback to untyped IR if type checking fails).

**Related documents:**
- [IMPLEMENTATION.md](./IMPLEMENTATION.md) - Current IR state (untyped + typed)
- [TYPED.md](../DESIGN/TYPED.md) - Typed IR design (implemented; reference)
- [COVERAGE.md](./COVERAGE.md) - Features needed for 100% IR parity

## Goals
- Normalize indexing (1-based R -> 0-based Mojo) in one place.
- Centralize type rules and explicit cast insertion.
- Make bounds checks and NA guards a pass, not ad-hoc codegen branches.
- Improve diagnostics with stable source spans carried through IR nodes.
- Enable future optimizations (loop fusion, unrolling, reduction lowering).

## Limitations Source of Truth

This document is architecture intent and stage planning; it is not the
canonical list of current strict-lane limitations.

- Use `docs/KNOWN_ISSUES.md` for current deferred/subset-limited routes.
- Use `docs/SUBSET.md` for supported source-language subset boundaries.
- Use `IMPLEMENTATION.md` for current concrete IR behavior and ownership.

## Implemented Optimizations

### Loop Fusion (2026-02-14)
Loop fusion combines adjacent loops operating on the same iteration domain, reducing memory traffic and loop overhead.

**What's implemented:**
- elem0 normalization for canonical index space conversion
- Loop domain extraction and matching
- Access pattern matching (LHS elem0 expression comparison)
- Noalias checking (resource disjointness)
- Effect checking (RNG/Unknown detection)
- Guard policy checking (NA guard and bounds check consistency)
- Sequential composition detection (adjacent loops)
- Control flow detection (loop with control flow rejection)
- vexpr loop fusion legality enabled in Tree IR (2026-02-17)

**Status:** ✅ Complete (34 tests passing)

**Files:**
- [`packages/mojor/R/ir/fusion.R`](../../packages/mojor/R/ir/fusion.R)
- [`docs/DESIGN/FUSION.md`](../DESIGN/FUSION.md)
- [`plans/loop_fusion_plan.md`](../../plans/loop_fusion_plan.md)

## Non-goals (Historical v0 Scope)
- No attempt to model full R semantics (lists, environments, S3/S4, lazy eval).
- No new features beyond the current subset.
- No SSA or heavy control-flow graph work initially (historical; SSA backend-prep is now implemented).

## IR v0: Minimal Node Set
The v0 IR covers the current loop-oriented subset.

### Top-level
- `Function(name, args, body, out, options, src)`
 - `args`: list of `Arg(name, type, shape, src)`
 - `out`: optional `Out(name, type, shape, src)`

### Statements
- `Block(stmts, src)`
- `Loop(var, range, body, src)`
- `If(cond, then, else, src)`
- `Assign(lhs, rhs, src)`
- `Return(expr, src)`
- `Decl(name, type, init, src)` for scalars

### Expressions
- `Var(name, type, src)`
- `Const(value, type, src)`
- `Index(base, indices, base_type, index_base, src)`
 - `index_base`: `one_based` or `zero_based`
- `BinOp(op, lhs, rhs, type, src)`
- `UnOp(op, expr, type, src)`
- `Call(fn, args, type, src)`
- `Cast(type, expr, src)`
- `IfElse(cond, yes, no, type, src)` (ternary expression support)

## IR Commands (R Helpers)
These helpers live in `packages/mojor/R/ir/*.R`.
They are the current "IR command set" used to build nodes in R:

- `.mojor_ir_const(value, src = NULL)`
- `.mojor_ir_var(name, src = NULL)`
- `.mojor_ir_unop(op, expr, src = NULL)`
- `.mojor_ir_binop(op, lhs, rhs, src = NULL)`
- `.mojor_ir_cast(to, expr, src = NULL)`
- `.mojor_ir_call(fn, args, src = NULL)`
- `.mojor_ir_index(base, indices, index_base = "one_based", src = NULL)`
- `.mojor_ir_block(stmts, src = NULL)`
- `.mojor_ir_assign(lhs, rhs, src = NULL)`
- `.mojor_ir_if(cond, then, else = NULL, src = NULL)`
- `.mojor_ir_loop(var, range, body, src = NULL)`
- `.mojor_ir_range(start, end, step = NULL, src = NULL)`
- Structured-tier helpers (Stage 5a seed):
 - `.mojor_ir_s_if(cond, then, else_block = NULL, attrs = NULL, src = NULL)`
 - `.mojor_ir_s_for(var, range, body, loop_id = NULL, attrs = NULL, src = NULL)`
 - `.mojor_ir_s_while(cond, body, loop_id = NULL, attrs = NULL, src = NULL)`
 - `.mojor_ir_s_repeat(body, loop_id = NULL, attrs = NULL, src = NULL)`
 - `.mojor_ir_structured_promote(node, assign_loop_ids = TRUE)`
 - `.mojor_ir_structured_lower_to_ssa(node, verify_ir = FALSE, attach_loop_metadata = TRUE, strict_schema = NULL)`
- Loop recovery/recanonicalization helpers (Stage 5b seed):
 - `.mojor_loop_recovery_analysis(ssa, require_reducible = TRUE, require_single_exit = TRUE)`
 - `.mojor_recanonicalize_loop_cfg(ssa, recovery = NULL, verify = TRUE, strict_schema = NULL)`
 - `.mojor_loop_recovery_boundary_enabled(recovery)`
- Stage 5c pipeline/boundary helpers:
 - `.mojor_ssa_verify_or_stop(...)`
 - `.mojor_ssa_fusion_candidate_analysis(ssa, recovery = NULL)`
 - `.mojor_ssa_attach_fusion_candidate_analysis(ssa, analysis = NULL)`

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

| Stage | Input | Output | Typical failure surface |
|---|---|---|---|
| Build | R AST + arg specs | Structured Tree IR | Unsupported source subset shapes. |
| Normalize | Tree IR | Canonical Tree IR | Unexpected node-shape variants. |
| Verify | Canonical Tree IR + verifier context | Verified Tree IR | Missing required fields, loop/scope legality violations. |
| Optimize | Verified Tree IR | Rewritten Tree IR | Legal no-op due to effect/resource barriers. |
| Lower | HIR-oriented Tree IR | LIR/core-oriented Tree IR | Unsupported sugar lowerings or missing layout metadata. |
| Schedule | Lowered Tree IR + schedule policy | Schedule-routed Tree IR | Invalid schedule directives or ineligible tree/SIMD routes. |
| Type Check | Schedule-ready Tree IR + type env | Typed Tree IR | Unknown/unsupported strict-lane type contracts. |
| Emit | Typed Tree IR + guard policy | Mojo source text | Unsupported node/operator in emit lane. |

## Representative Diagnostics

Common diagnostics across planned/current IR stages:

- `IR verify: node missing valid 'kind' field`
- `IR verify [break]: break statement outside loop`
- `IR verify [scheduled_reduce]: mode must be one of tree/simd`
- `mojor_transpile: strict IR type check failed: ...`
- `.mojor_ir_expr_emit: c_call function '<name>' not declared`

Use the first failing boundary to choose the right owner doc:
- build/lower issues -> `IMPLEMENTATION.md`
- shape/legality issues -> `SPEC.md` + `CONTRACT.md`
- effect blockers -> `EFFECT_SYSTEM.md`

---

## Related Documents

- [../DIAGNOSTICS_INDEX.md](../DIAGNOSTICS_INDEX.md) - Centralized diagnostics map (error text -> boundary -> owner docs)
- [IMPLEMENTATION.md](./IMPLEMENTATION.md) - Current IR state
- [SPEC.md](./SPEC.md) - IR Specification
- [CONTRACT.md](./CONTRACT.md) - IR Contracts
- [EFFECT_SYSTEM.md](./EFFECT_SYSTEM.md) - Effect System
