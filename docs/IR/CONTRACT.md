# MöjoR IR Contract (Current Enforced Invariants)

**Date:** 2026-02-17 **Status:** Active, implementation-aligned **Purpose:** Define non-negotiable semantics and boundaries for Tree IR and SSA backend staging.

This contract describes what passes can assume, what they must preserve, and which diagnostics/boundary checks are currently enforced in code.

## Plain-Language Mental Model

This file is the strict safety contract for IR behavior.

- "Can assume" means passes are allowed to rely on it.
- "Must preserve" means transforms cannot break it.
- "Boundary checks" are mandatory gates between major stages.

If a transform violates contract rules, it should fail verification rather than silently continue.

## Mini Example

Contract example:

- `break` outside loops is illegal.
- Tree verifier must reject it.
- Later stages are allowed to assume that all surviving `break` nodes are loop-legal.

This is why early verifier failures are desirable: they protect all downstream passes.

## Beginner Debug Checklist

1. Find which contract section matches the failure class (Tree verifier, SSA verifier, typing, guards, ABI).
2. Determine if failure happened before or after a boundary (`post_*` checks).
3. If behavior differs across strict/compat routes, check strictness clauses first.
4. When in doubt, treat unresolved/unknown effects as intentional optimization blockers.

## 1. Contract Scope

This contract applies to:
- Tree IR verification/optimization/lowering/scheduling/emit paths.
- SSA semantic boundary paths used by backend preparation.

It is anchored to current implementation in:
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/ssa_semantic.R`
- `packages/mojor/R/ir/ssa_backend.R`

## 2. Representation Boundary Contract

### 2.1 Tree IR

- Nodes are list objects with `kind`.
- Structured aliases (`s_if/s_for/s_while/s_repeat`) must be semantically equivalent to base kinds (`if/loop/while/repeat`).
- `raw` is compatibility-only and represents unknown semantics for optimization safety.

### 2.2 SSA syntax vs semantic IR

- `raw_ssa_fn` is parse/lossless representation.
- semantic passes must operate on `ssa_fn` only.
- lowering from raw to semantic must drop trivia and retain semantic structure.

### 2.3 V2 syntax/semantic boundary guarantees

These contracts carry forward the legacy V2 boundary model:
- lossless parse/print pair:
 - `.mojor_ssa_parse_lossless(text)`
 - `.mojor_ssa_print_lossless(raw_ssa_fn)`
- normalized parse path:
 - `.mojor_ssa_parse_norm(text)` (semantic canonicalization + pretty reparse)
- semantic printer:
 - `.mojor_ssa_print_semantic(ssa_fn, mode = "pretty"|"compat")`
- explicit lowering boundary:
 - `.mojor_ssa_lower_to_semantic(raw_ssa_fn)`

Invariants:
- trivia must not influence semantic behavior.
- pretty-form roundtrips must be semantically idempotent.
- canonical semantic forms are validated via SSA verifier + boundary checks.

### 2.4 Canonical SSA namespace and historical aliases

Canonical namespace families in the current implementation are:
- `mojor.arith.*`
- `mojor.mem.*`
- `mojor.cf.*`
- `mojor.misc.*`

Historical planning documents may use older labels. Interpret them as semantic aliases only:
- `mojor.control.*` -> `mojor.cf.*`
- `mojor.undef.*` -> `mojor.misc.const_*`
- `mojor.rng.*` -> `mojor.misc.call.*` with RNG effect/resource annotation
- `mojor.raw.*` -> `mojor.misc.stmt.raw`

The canonical spellings above are authoritative for verifier/schema contracts.

## 3. Core Verifier Contract

### 3.1 Tree verifier (`.mojor_ir_verify`)

Must enforce:
- required fields by node kind.
- legal operators and type tokens.
- loop legality (`break`/`next` only inside loop contexts).
- scope checks when requested.
- no `raw` nodes when `ir_only=TRUE`.
- assignment lhs shape constraints (`var/index/subscript`).

### 3.2 SSA core verifier (`.mojor_ir_verify_ssa`)

Must enforce:
- root shape (`ssa_fn`, valid entry, non-empty blocks).
- unique SSA defs.
- valid terminators (`br`, `condbr`, `ret`).
- valid CFG targets and branch argument arity.
- use-before-def and dominance visibility rules.
- edge-only `break`/`next` semantics (effect-form encodings are illegal in CFG SSA).
- optional strict schema checks when enabled.

## 4. Boundary Verifier Contract (SSA)

Boundary entrypoint: `.mojor_verify_semantic_ir(mode="boundary", boundary=...)`

Supported boundaries:
- `post_canonicalize`
- `post_structured_lowering`
- `post_memory_canonicalize`
- `post_recanonicalize`
- `post_typing`

Boundary guarantees:
- `post_canonicalize`: canonicalized semantic SSA remains structurally valid and ready for boundary checks.
- `post_structured_lowering`: no structured-op spellings remain; loop destination metadata must exist and be coherent.
- `post_memory_canonicalize`: legacy memory spellings are rejected (`LEGACY_MEMORY_SPELLING`).
- `post_recanonicalize`: loop metadata must contain valid continue/break destinations and anchor/header attrs.
- `post_typing`: typing diagnostics can be enforced when requested.

## 5. Memory Canonicalization Contract

Canonical memory boundary contract:
- canonical memory ops are `mojor.mem.load` and `mojor.mem.store`.
- legacy memory spellings may exist pre-boundary but are forbidden at `post_memory_canonicalize`.
- backend prep must include memory canonicalization before effect/resource annotation and typing.

## 6. Effect and Resource Contract

### 6.1 Tree effect classes

Tree expression effect labels:
- `Pure`
- `ReadsMem`
- `RNG`
- `Unknown`

Optimization legality classes map to:
- `None`, `Read`, `Write`, `RNG`, `Unknown`

### 6.2 SSA effect/resource annotations

SSA statement effect classes:
- `None`, `Read`, `Write`, `RNG`, `Unknown`

Resource IDs are required for legality reasoning and produced in stable forms:
- `arg_slot:<n>`
- `ssa_value:<n>`
- `symbol:<name>`
- fallback `unknown:*` when unresolved

### 6.3 Optimization legality requirements

- CSE may only eliminate/extract when effect class is `None`.
- DCE may only remove assignments when RHS is effect class `None`.
- LICM read hoists require no interference with loop-written resource IDs.
- `RNG` and `Unknown` are hard barriers for motion/elimination.
- `raw` nodes are treated conservatively as `Unknown`.

## 7. Typing Contract

- typing must annotate/infer expression types consistently.
- cast insertion must preserve semantics and enforce assignment compatibility.
- boolean contexts must coerce non-boolean numeric expressions explicitly.
- index-first and shape-aware typing checks are staged through boundary verification and SSA typing passes.

## 8. Index, Layout, and Guard Contract

### 8.1 Index semantics

- semantic index base is tracked by `index_base`.
- emit-time zero-based runtime vars are tracked separately (`zero_based_vars`).
- one-based loop semantics must be converted safely to zero-based memory access.

### 8.2 Layout and bounds

- bounds checks must use threaded layout metadata (`len_var_map`, `nrow/ncol/dim` maps), not raw pointer length.
- bounds-checked reads must use `_mojor_read_*` helpers.

### 8.3 NA guards

- default NA guard mode is forbid.
- guard generation is centralized and deduplicated per statement.

### 8.4 Sampling contract ( closeout + increment)

- strict sampling expression support for `sample.int`/`sample` remains a constrained subset:
 - `prob` accepts `NULL` or a direct `f64[]` variable in strict typed lanes
 - `replace` accepts scalar boolean/int flags in strict IR verify/emit
 - `sample(x, ...)` requires `x` as a direct vector variable
- strict scalar-expression emission is contractually supported for:
 - direct scalar draws (`size = 1`)
 - indexed scalar extraction with compile-time integer index and compile-time integer `size` satisfying `size >= index`
- loop-assignment emission supports runtime-index extraction for forms like:
 - `out[i] <- sample.int(...)[i]`
 - `out[i] <- sample(x, ...)[i]`
 in loop context with typed/index-safe emit guards.
- composed scalar loop expressions additionally support runtime-index extraction
 over sampling terms (for example `sample(...)[i] + c`) through shared sampling
 index helper emission.
- statement emission additionally supports weighted `prob` for:
 - whole-vector statement assignment
 - indexed-dynamic loop assignment forms (`...)[i]`)
 when all of the following hold:
 - whole-vector weighted lanes: `replace = TRUE/FALSE` (or scalar `replace` flag)
 - indexed-dynamic weighted lanes: `replace = TRUE`
 - `prob` is a direct vector variable
 - `prob` is typed as `f64[]` (typed contexts)
- strict no-loop weighted sampling is supported when `prob` is a direct `f64[]`
  argument and `replace` is a strict scalar/literal flag.
- broader non-scalar sampling expression emission is not yet part of this contract and must fail with stable diagnostics in strict flows.

### 8.5 FFI `c_call` ABI contract ( hardening)

- `mojor_c_call(...)` must match the declared C signature from `mojor_declare_c(...)`.
- verifier/emitter/runtime enforce:
 - argument count equality (`expected_arity == supplied_arity`)
 - per-argument type compatibility against declared FFI types
 - declaration-parity metadata checks for `returns`, `arg_types`, and `arg_names`
- `c_call` metadata normalization/parity rules:
 - `returns` must be a non-empty scalar character type token
 - `arg_types` and `arg_names`, when present, are normalized from character/list shapes to character vectors
 - normalized `arg_types`/`arg_names` length must match supplied arity
 - when declarations exist, normalized node metadata must match declaration metadata
- stable diagnostics for contract violations include:
 - `argument count mismatch`
 - `argument type mismatch`
 - `return type mismatch`
 - `arg_types metadata mismatch`
 - `arg_names metadata mismatch`
- in strict mode (`ir_only=TRUE`), `c_call` argument types must be statically known for signature checking; unknown argument types are rejected.

### 8.6 higher-order function contract (current)

- strict HOF lanes (`vapply`/`sapply`/`lapply`/`mapply`) require direct vector arguments and scalar `FUN` results.
- `mapply` is constrained to direct vectors with arity >= 2 and matching vector element types in strict paths.
- accepted `mapply` `FUN` forms:
 - anonymous inline single-expression lambdas
 - curated named functions: `pmin`, `pmax`, `min`, `max`
- strict HOF lowering rejects unsupported named vector-argument forms and unsupported named `FUN` symbols with stable diagnostics.

## 9. Structured Lowering and Loop Metadata Contract

- structured->SSA lowering must produce valid CFG SSA and attach loop destination metadata.
- loop recovery can produce `Recovered` or `Unknown` status.
- recanonicalization boundary checks may be disabled with warning when recovery status is `Unknown`.
- when enabled, recanonicalization must restore coherent loop metadata:
 - loop id
 - continue destination block
 - break destination block
 - anchor/header attrs

## 10. Fusion Contract

### 10.1 Tree IR path

- Tree loop fusion rewrite exists as an optimization path and is guarded by legality checks.

### 10.2 SSA/P3 contract

- SSA backend prep fusion remains analysis-only (`analysis_only=TRUE`).
- rewrite transforms are not allowed in this stage.
- stable rejection codes:
 - `FUSE_REJECT_DOMAIN_MISMATCH`
 - `FUSE_REJECT_INDEXMAP_NOT_IDENTITY`
 - `FUSE_REJECT_NOALIAS_MISSING`
 - `FUSE_REJECT_CONTROL_FLOW`
 - `FUSE_REJECT_EFFECTS_RNG`
 - `FUSE_REJECT_EFFECTS_UNKNOWN`
 - `FUSE_REJECT_BROADCAST_ND`
 - `FUSE_REJECT_GUARD_POLICY`
- these code names are contract-stable diagnostics even when explicit opt-in
 policies suppress them for proven-safe subsets.
- opt-in policy gates (default `FALSE`):
 - `fusion_allow_control_flow_simple`
 - `fusion_allow_broadcast_nd_identity`

## 11. Backend Pipeline Ordering Contract (SSA prep)

Required relative ordering:

`schedule_ir -> to_ssa -> memory_canonicalize -> annotate_effects_resources -> typing -> backend_cfg`

Additional ordering constraints:
- `loop_recovery` after `annotate_effects_resources`
- `recanonicalize_loop_cfg` after `annotate_effects_resources` and before `typing`
- `fusion_candidate_analysis` after `annotate_effects_resources` and before `typing`
- if present: `select_backend` before `codegen_backend`

Pipeline invariant failures must be treated as hard errors.

## 12. Compatibility and Strictness Contract

- strict mode (`ir_only=TRUE`) must reject raw fallback paths.
- compatibility mode may retain legacy/raw escapes, but they are optimization barriers.
- strict schema mode is opt-in (`strict_schema=TRUE` or option) and upgrades schema issues to hard failures.

## 13. Stable Diagnostic Families

The following diagnostic codes are contract-significant and should remain stable:
- `OP_SCHEMA_UNKNOWN`
- `OP_SCHEMA_ARITY`
- `LEGACY_MEMORY_SPELLING`
- `STRUCTURED_OP_REMAINING`
- `LOOP_METADATA_MISSING`
- `LOOP_METADATA_INCOMPLETE`
- `LOOP_RECANONICALIZE_METADATA_MISSING`
- `LOOP_RECANONICALIZE_METADATA_INCOMPLETE`
- `LOOP_RECANONICALIZE_ANCHOR_MISSING`
- `LOOP_RECANONICALIZE_ANCHOR_ATTRS`
- `RECANONICALIZE_BOUNDARY_DISABLED_RECOVERY_UNKNOWN`
- fusion reject codes listed in Section 10.2

## 14. Representative Diagnostics Map

Use this map to route failures to the right contract boundary quickly:

- `IR verify [break]: break statement outside loop` -> Tree verifier contract (Section 3.1).
- `IR verify [raw]: raw fallback node not allowed in ir_only mode` -> strict/compat contract (Section 12).
- `IR verify [scheduled_reduce]: mode must be one of tree/simd` -> reduction/scheduling contract (Sections 3.1, 8).
- `SSA verify: br from '<from>' targets unknown block` -> SSA core verifier contract (Section 3.2).
- `SSA verify: condbr ... arg arity mismatch` -> SSA CFG/arity contract (Section 3.2).
- `SSA verifier: LEGACY_MEMORY_SPELLING: ...` -> memory canonicalization boundary (Section 5).
- `SSA verifier: LOOP_RECANONICALIZE_*` -> loop metadata/recanonicalization contract (Section 9).
- `FUSE_REJECT_*` codes -> fusion analysis-only contract (Section 10.2).

## 15. References

- `docs/IR/SPEC.md`
- `docs/IR/EFFECT_SYSTEM.md`
- `docs/CHANGELOG.md`
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/ssa.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/ssa_semantic.R`
- `packages/mojor/R/ir/ssa_backend.R`

## 16. Traceability (Invariant -> Enforcement -> Regression)

| Contract invariant | Primary enforcement | Regression coverage |
|---|---|---|
| Semantic passes consume `ssa_fn`, not lossless `raw_ssa_fn` | `packages/mojor/R/ir/ssa_verify.R`, `packages/mojor/R/ir/ssa_semantic.R` | `packages/mojor/tests/testthat/test_ir_semantic_boundary.R` |
| Tree strict mode rejects compatibility `raw` nodes | `packages/mojor/R/ir/verify.R` | `packages/mojor/tests/testthat/test_ir_verify.R`, `packages/mojor/tests/testthat/test_ir_verify_invariants.R` |
| SSA core validity (CFG targets, arity, dominance, defs) | `packages/mojor/R/ir/ssa.R` | `packages/mojor/tests/testthat/test_ir_ssa_skeleton.R`, `packages/mojor/tests/testthat/test_ir_verify_invariants.R` |
| `post_structured_lowering` forbids structured spellings and requires loop metadata | `packages/mojor/R/ir/ssa_verify.R` | `packages/mojor/tests/testthat/test_ir_break_next_cfg_lowering.R`, `packages/mojor/tests/testthat/test_ir_semantic_boundary.R` |
| `post_memory_canonicalize` rejects legacy memory spellings | `packages/mojor/R/ir/ssa_verify.R`, `packages/mojor/R/ir/ssa_semantic.R` | `packages/mojor/tests/testthat/test_ir_memory_canonicalization.R` |
| Strict schema validation enforces stable op namespace/arity contracts | `packages/mojor/R/ir/op_schema.R`, `packages/mojor/R/ir/ssa_verify.R` | `packages/mojor/tests/testthat/test_ir_op_schema.R` |
| Backend prep pipeline ordering is hard-enforced | `packages/mojor/R/ir/ssa_backend.R` | `packages/mojor/tests/testthat/test_ir_pipeline_invariants.R` |
| SSA fusion in backend prep remains analysis-only during P3 | `packages/mojor/R/ir/ssa.R`, `packages/mojor/R/ir/ssa_backend.R` | `packages/mojor/tests/testthat/test_ir_fusion_candidate_analysis.R` |
| Effect/resource legality blocks unsafe motion/elimination | `packages/mojor/R/ir/ssa.R`, `packages/mojor/R/ir/fusion.R` | `packages/mojor/tests/testthat/test_ir_effect_legality.R`, `packages/mojor/tests/testthat/test_ir_cse.R`, `packages/mojor/tests/testthat/test_ir_licm.R`, `packages/mojor/tests/testthat/test_ir_dce.R` |
| Loop recanonicalization diagnostics are stable at boundary checks | `packages/mojor/R/ir/loop_recanonicalize.R`, `packages/mojor/R/ir/ssa_verify.R` | `packages/mojor/tests/testthat/test_ir_loop_recanonicalization.R` |
| strict sampling subset enforces scalar/indexed constraints with stable diagnostics | `packages/mojor/R/ir/expr_emit.R`, `packages/mojor/R/ir/stmt_emit.R` | `packages/mojor/tests/testthat/test_apply_sample.R` |
