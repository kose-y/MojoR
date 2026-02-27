# MöjoR IR Specification (Tree IR + SSA Boundary)

**Date:** 2026-02-23 **Status:** Active **Scope:** Implemented IR behavior in `packages/mojor/R/` and mirrored package code.

This document is the implementation-aligned IR spec for current MöjoR. It covers Tree IR (HIR/LIR), the SSA boundary used by the backend pipeline, and the invariants that are currently enforced.

## Plain-Language Mental Model

This is the contract-style "rules of the road" for IR.

- It defines what shapes are valid.
- It defines which stage is allowed to transform what.
- It defines what must stay true as code moves from Tree IR to SSA/backend prep.

If you are unsure whether behavior is a bug or expected, this spec is where you check first.

## Mini Example

For a simple assignment `out[i] <- x[i] + 1`:

- Tree IR must encode legal `assign/index/binop` node shapes.
- Lowering must preserve meaning while making index behavior explicit.
- Typing must confirm operand compatibility.
- Emission must preserve one-based R semantics through index conversion.

Each step has rules in this spec; violating any rule should be rejected by verifier/boundary checks.

## Beginner Debug Checklist

1. Confirm node kind/fields are valid for the stage where failure appears.
2. Check whether the failing path is Tree IR primary path or SSA backend-prep path.
3. For optimization surprises, check effect/resource legality sections before pass logic.
4. For runtime mismatches, follow spec sections in order: indexing -> typing -> guards -> ABI boundary.

## 1. Architecture

### 1.1 Tree IR pipeline (primary transpile path)

```
R AST
 -> Build Tree IR
 -> Normalize
 -> Verify
 -> Optimize (Tree IR CSE/fold/LICM/DCE)
 -> Lower (HIR -> LIR subset)
 -> Schedule (tiling/reduction metadata transforms)
 -> Type check + typed annotation
 -> Emit Mojo
```

### 1.2 SSA backend prep pipeline (structured/canonical backend path)

`normalize -> verify -> optimize_ir -> lower_ir -> schedule_ir -> to_ssa -> memory_canonicalize -> annotate_effects_resources -> loop_recovery -> recanonicalize_loop_cfg -> fusion_candidate_analysis (analysis-only) -> typing -> optimize_ssa -> backend_cfg`

Optional trailing backend artifacts:
- `lower_backend`
- `select_backend`
- `codegen_backend`

### 1.3 Two-tier model

- Tree IR is the authoring/optimization IR for transpilation.
- SSA is a downstream boundary IR used for canonical memory/effect/type checks and backend staging.
- Typed IR is a pass result, not a separate IR family.

## 2. Tree IR Node Model

All Tree IR nodes are R lists with:
- required `kind`
- optional `src`
- kind-specific fields

### 2.1 Core structural/statement kinds

- `block(stmts)`
- `assign(lhs, rhs)`
- `if(cond, then, else_block)`
- `loop(var, range, body, metadata)`
- `while(cond, body)`
- `repeat(body)`
- `break()`
- `next()`
- `return(value)`
- `scalar_reduce(op, acc, arg, init, axis, associative, commutative, na_rm)`
- `scheduled_reduce(mode, op, acc, arg, n_var, dtype, ...)`

Structured aliases also exist and normalize to base kinds:
- `s_if`, `s_for`, `s_while`, `s_repeat`

### 2.2 Core expression kinds

- `const(value)`
- `var(name)`
- `unop(op, expr)`
- `binop(op, lhs, rhs)`
- `cast(to, expr)`
- `call(fn, args)`
- `index(base, indices, index_base)`
- `ifelse(cond, yes, no)`
- `range(start, end, step, end_exclusive)`
- `range_expr(expr)`
- `raw(expr)` (compatibility escape; not allowed in `ir_only=TRUE`)

### 2.3 Index helper kinds

- `subscript(var, indices)`
- `slice_index(start, end)`
- `scalar_index(expr)`
- `missing_index()`

Index nodes in `indices[]` lists may carry optional metadata fields for negative exclusion and positive selection patterns:
- `neg_exclusion` (string): 0-based exclusion index for scalar `-k`. Present on `kind="var"`, `name="__neg_excl__"`.
- `neg_vec_exclusion` (object): Vector exclusion for `-c(1,2)` / `-(1:3)`. Fields: `type` (`"c"`/`"range"`), `indices`/`start_0`/`end_0`, `count`.
- `pos_vec_selection` (object): Positive vector selection metadata for `c(1,3)`-style selector sets. Fields: `type="c"`, `indices_0`, `count`.

Range selectors (`1:3`) remain on canonical slice/index lowering routes rather than
being rewritten to `pos_vec_selection` metadata.

### 2.4 Breadth and staged kinds currently present

Constructors and breadth ops currently represented in Tree IR include:

- construction/index helpers: `rep`, `rep_len`, `c`, `seq`, `transpose`, `cbind`, `rbind`, `diag`, `dim`
- reductions/stats/cumulative: `mean`, `var_stat`, `sd`, `median`, `quantile`, `iqr`, `mad`, `cumsum`, `cumprod`, `cummax`, `cummin`
- set/match: `unique`, `duplicated`, `any_duplicated`, `match`, `in`
- sampling/RNG/allocation: `sample_int`, `sample`, `rng_vec`, `alloc`
- higher-order/string: `vapply`, `sapply`, `lapply`, `mapply`, `nchar`, `nzchar`, `substr`, `paste`
- matrix/gpu/vexpr: `matmul`, `crossprod`, `tcrossprod`, `vexpr`, `gpu_reduce`, `gpu_matmul`
- type metadata helpers: `type_predicate`, `type_query`

Not all staged kinds are lowered to core loops today; several are emitted via specialized codegen paths.

### 2.5 Normative status by node family

| Node family | Normative status |
|---|---|
| Core structural and expression kinds (`block/assign/if/loop/while/repeat/break/next/return`, `const/var/unop/binop/cast/call/index/ifelse/range/range_expr`, indexing helpers) | Must be structurally verifiable in Tree IR and remain legal through lower/schedule/type/emit. |
| Structured aliases (`s_if/s_for/s_while/s_repeat`) | Must normalize/lower to canonical forms before SSA boundary checks. |
| `raw` compatibility nodes | Allowed only in compatibility flows; must be rejected in strict `ir_only=TRUE` paths. |
| Scheduled reduction forms (`scheduled_reduce`) | Allowed only after schedule-phase routing and must preserve reduction semantics. |
| Staged breadth kinds (Section 2.4) | May lower to core loops or emit through specialized codegen. Unsupported strict-path forms must fail explicitly with diagnostics; no silent fallback in strict mode. |

## 3. Lowering and Scheduling Semantics

### 3.1 Implemented lowering behavior

- `scalar_reduce` lowers to explicit loop IR unless deferred for scheduled reduction mode.
- slice/missing-index assignments (`subscript` + `slice_index`/`missing_index`) lower to explicit loop nests when applicable.
- `:`-range selector lanes are preserved on canonical slice routes (no
`pos_vec_selection` marker rewrite for range forms).
- `vexpr` lowers to allocation + loop + return.
- many breadth nodes currently pass through lowering and are handled in emit/feature-specific code paths.

### 3.2 Schedule pass behavior

- attaches/propagates schedule metadata on loops.
- supports loop tiling transforms.
- rewrites eligible `scalar_reduce` to `scheduled_reduce` in `tree` or `simd` mode.
- remains type-agnostic except for schedule eligibility checks.

## 4. Type System and Metadata

Vectors, matrices, and higher-dimensional arrays are all ndarrays
distinguished only by rank (ndim). Type string forms:

- scalar (rank 0): `f64`, `f32`, `i32`, `bool`, `lgl`, `unknown`
- 1D / vector (rank 1): `f64[]`, `f32[]`, `i32[]`, `bool[]`, `lgl[]`
  - `[1d]` is accepted as an alias for `[]` (e.g. `f64[1d]` ≡ `f64[]`)
- 2D / matrix (rank 2): `f64[,]` or `f64[2d]`, etc.
- ND / array (rank N): `f64[3d]`, `f64[4d]`, etc.

Type behavior:
- inference via `.mojor_ir_infer_type`
- cast insertion/coercion via `.mojor_ir_type_check_stmt`
- typed annotation pass via `.mojor_ir_annotate_typed`

## 5. Indexing, Layout, and Guards

### 5.1 Indexing

- R-facing semantics are 1-based.
- emitted Mojo indexing is 0-based.
- loop IVs are generally emitted 1-based (`range(1, n + 1)`), with conversion at index sites.
- `index_base` carries semantic convention (`one_based` or `zero_based`).
- `zero_based_vars` is runtime emission tracking for variables already 0-based.

### 5.2 Layout

- default array/matrix layout: `col_major`.
- layout context is threaded via `.mojor_ir_layout_ctx(...)` with maps for len/nrow/ncol/dim/ndim/tensor metadata.

### 5.3 Guards

- NA guard policy is forbid mode unless explicitly overridden.
- bounds checks use layout-context length metadata (`len_var_map` etc.), not raw pointer lengths.
- bounds-checked reads route through `_mojor_read_*` helpers.
- guard CSE is statement-local.

## 6. Effect/Resource Model (Tree IR)

Tree expression effect labels include:
- `Pure`
- `ReadsMem`
- `RNG`
- `Unknown`

Legality mapping used by optimization:
- `Pure -> None`
- `ReadsMem -> Read`
- write effects from assignment/resource summaries -> `Write`
- `RNG -> RNG`
- `Unknown/Status -> Unknown`

Resource IDs are tracked (for legality) in forms like:
- Tree: `var:<name>`
- SSA: `arg_slot:<n>`, `ssa_value:<n>`, `symbol:<name>`

## 7. SSA Boundary Spec (Implemented)

### 7.1 Representations

- `raw_ssa_fn`: lossless parse tree with preserved source text.
- `ssa_fn`: semantic SSA IR consumed by passes.

### 7.2 Parser/printer APIs

- `.mojor_ssa_parse_lossless(text)`
- `.mojor_ssa_parse_norm(text)`
- `.mojor_ssa_print_lossless(raw_ssa_fn)`
- `.mojor_ssa_print_semantic(ssa_fn, mode = "pretty"|"compat")`
- `.mojor_ssa_lower_to_semantic(raw_ssa_fn)`

### 7.3 Canonical op namespaces in current implementation

- `mojor.arith.*`
- `mojor.mem.*`
- `mojor.cf.*`
- `mojor.misc.*`

Historical planning/docs aliases (semantic meaning only, not canonical spellings):
- `mojor.control.*` -> `mojor.cf.*`
- `mojor.undef.*` -> `mojor.misc.const_*`
- `mojor.rng.*` -> `mojor.misc.call.*` with RNG effect/resource annotation
- `mojor.raw.*` -> `mojor.misc.stmt.raw`

### 7.4 Boundary verifier modes

- core verifier: SSA structural/dominance/arity rules.
- boundary verifier: `post_canonicalize`, `post_structured_lowering`, `post_memory_canonicalize`, `post_recanonicalize`, `post_typing`.

### 7.5 Structured and fusion status

- structured->CFG lowering is implemented with loop metadata attachment.
- loop recovery + recanonicalization are implemented and boundary-checked.
- SSA fusion in P3 remains analysis-only (`fusion_candidate_analysis`), no rewrite pass in SSA backend prep.

## 8. Current Scope Notes

- P3 is complete and closed; + breadth work is active.
- Tree IR still includes compatibility/staged paths while SSA boundaries enforce stricter canonical checks.
- `raw` nodes remain compatibility escape nodes and are disallowed in strict `ir_only` paths.
- Sampling (`sample.int`/`sample`) scope is constrained by strict policy:
 - strict IR verify/emit (including normalized no-loop forms): `prob` accepts `NULL` or direct `f64[]` variable forms, `replace` is scalar boolean/int, and `sample(x, ...)` requires direct-vector `x`
 - statement lanes: weighted `prob` supports whole-vector assignment with `replace = TRUE/FALSE` (or scalar `replace` flags) when `prob` is a direct `f64[]` variable
 - indexed-dynamic weighted assignment remains limited to `replace = TRUE`
- Higher-order `mapply` scope includes direct vector arguments (arity >= 2) with scalar-return `FUN`; supported `FUN` forms include anonymous inline lambdas and curated named functions (`pmin`, `pmax`, `min`, `max`) under current strict lane constraints.
- `c_call` nodes carry declaration-derived ABI metadata (`returns`, `expected_arity`, `arg_types`, `arg_names`) and enforce declaration parity during verify and emit.

## 9. Representative Diagnostics

Common spec-boundary diagnostics (Tree IR and SSA boundary):

- `IR verify: node missing valid 'kind' field`
- `IR verify [break]: break statement outside loop`
- `IR verify [index]: indices list cannot be empty`
- `mojor_transpile: index name '<name>' not found in dimnames for '<var>'`
- `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)`
- `IR verify [scheduled_reduce]: mode must be one of tree/simd`
- `IR verify [scheduled_reduce]: dtype is invalid for mode`
- `SSA verify: br arg arity mismatch from '<from>' to '<to>' (<n> != <m>)`
- `SSA verifier: <CODE>: <message> [block=<name>]`

Interpretation:
- `IR verify ...` means Tree IR shape/legality contract failure.
- `SSA verify ...` means CFG/dataflow boundary failure in SSA path.

## 10. References

- `docs/IR/CONTRACT.md`
- `docs/IR/EFFECT_SYSTEM.md`
- `docs/IR/IMPLEMENTATION.md`
- `docs/IR/TREE_NODE_REFERENCE.md`
- `docs/BACKEND/SSA_OP_REFERENCE.md`
- `docs/CHANGELOG.md`
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/ssa_semantic.R`
- `packages/mojor/R/ir/ssa_backend.R`
