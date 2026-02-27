# Pipeline Deep Dives

This section gives implementation-aligned, plain-language documentation for each major MöjoR compiler/runtime step.

## End-to-End Flow

`R API -> AST -> Build -> HIR -> Normalize/Verify -> Optimize -> LIR -> Schedule -> Type Check -> Emit -> Build/Load -> Runtime Call`

Optional backend-prep path:

`... -> to_ssa -> memory_canonicalize -> annotate_effects_resources -> loop_recovery -> recanonicalize_loop_cfg -> fusion_candidate_analysis -> typing -> verify_ssa -> optimize_ssa -> verify_ssa -> backend_cfg`

## Plain-Language Mental Model

The pipeline is a sequence of transforms that turn R functions into callable native code. Documents in this directory follow the data flow:

1. **Entry** (`API.md`) - how you call into the system
2. **Frontend** (`AST_AND_BUILD.md`, `HIR.md`) - R AST becomes structured IR
3. **Safety gate** (`NORMALIZE_AND_VERIFY.md`) - structural legality checks
4. **Optimization** (`OPTIMIZATION.md`, `CSE.md`, `LICM.md`, `DCE.md`) - rewrites preserving semantics
5. **Lowering** (`LIR.md`, `SCHEDULING.md`, `TYPING.md`, `EMISSION.md`) - IR becomes native source
6. **Runtime** (`RUNTIME_AND_ABI.md`) - native code becomes callable from R
7. **Backend** (`SSA.md`, `CFG.md`, `EFFECTS_LEGALITY.md`) - SSA transforms and effect analysis

Think of each stage as a filter: it receives structured input, validates/contracts it, produces structured output. Failures at any stage surface diagnostics pointing to that stage's boundary.

## Beginner Reading Order

If you are new, read in this order:

1. `API.md` (how you enter the system)
2. `AST_AND_BUILD.md` and `HIR.md` (how source becomes IR)
3. `NORMALIZE_AND_VERIFY.md` (safety gate before heavy transforms)
4. `OPTIMIZATION.md`, then `CSE.md`, `LICM.md`, `DCE.md` (what rewrites happen and why)
5. `LIR.md`, `SCHEDULING.md`, `TYPING.md`, `EMISSION.md` (how IR becomes code)
6. `RUNTIME_AND_ABI.md` (how emitted code becomes callable from R)
7. `SSA.md` and `CFG.md` (backend-prep and control-flow internals)
8. `EFFECTS_LEGALITY.md` (why some optimizations are blocked)

Each stage doc includes:
- A plain-language mental model
- A mini example
- A beginner debug checklist
- An internal symbol index for code mapping

## Mini Example

Navigating a compilation issue:

```r
library(mojor)

# Step 1: Verify IR production (frontend)
trans <- mojor_transpile(function(x) x + 1)
# If this fails, see AST_AND_BUILD.md, HIR.md

# Step 2: Verify strict-mode acceptance (safety gate)
mojor_options(ir_only = TRUE)
trans <- mojor_transpile(function(x) x + 1)
# If this fails, see NORMALIZE_AND_VERIFY.md, SUBSET.md

# Step 3: Build and call (runtime)
fn <- mojor_build(function(x) x + 1)
fn(1:10)
# If build fails, see stages 4-6. If call fails, see RUNTIME_AND_ABI.md.
```

## Beginner Debug Checklist

- [ ] Start at the symptom: use **Troubleshooting by Symptom** section below
- [ ] Check which stage actually fails: look at the error prefix (e.g., `IR verify`, `Emit`, `Runtime`)
- [ ] For diagnostics text, check `docs/DIAGNOSTICS_INDEX.md` first
- [ ] Verify strict vs compatibility path: `ir_only = TRUE` vs `ir_only = FALSE`
- [ ] Confirm options are not conflicting: see `API.md` option contract tables

## Limitation Source of Truth

This directory explains stage behavior, but it is not the canonical limitations registry.

- Use `docs/KNOWN_ISSUES.md` for the consolidated current limitations/deferred routes.
- Use `docs/SUBSET.md` for the supported source-language subset.
- In strict mode (`ir_only = TRUE`), unsupported routes fail explicitly.
- In compatibility mode, some unsupported routes can still use guarded fallback policy.

## Stage Input/Output Snapshot

| Stage | Primary input | Primary output | Most common failure class |
|---|---|---|---|
| API | R function + user options | Normalized pipeline/build request | Option contract errors (`opt_level`, schedule knobs, mode combinations). |
| AST/Build | Parsed R AST + arg specs | Structured Tree IR (HIR-oriented) | Unsupported/ambiguous source subset forms. |
| HIR | Built Tree IR nodes | Structured high-level IR graph | Missing/invalid node fields from builder routes. |
| Normalize/Verify | HIR | Canonicalized, verifier-clean Tree IR | Structural legality failures (`break/next` scope, invalid assignment shape). |
| Optimization | Verified Tree IR | Rewritten Tree IR (same semantics) | Legality barriers (`RNG`, unknown effects/resources). |
| LIR | Optimized Tree IR + layout context | Lowered core Tree IR | Unsupported sugar lowerings or missing layout metadata. |
| Scheduling | Lowered Tree IR + schedule policy | Schedule-annotated/reduction-routed Tree IR | Illegal route requests (simd/tree/tile no-op or rejection). |
| Typing | Schedule-ready Tree IR + type env | Typed Tree IR (+ inserted casts) | Assignment/operator type mismatch in strict routes. |
| Emission | Typed Tree IR + guard policy | Mojo source text + emission metadata | Unsupported node/operator at emitter boundary. |
| Runtime/ABI | Emitted source + build options | Shared lib + R-callable wrapper | Toolchain, bridge, or ABI contract mismatch. |
| SSA | Structured Tree IR | Semantic SSA + boundary checks | Terminator/edge/arity/use-def violations. |
| CFG | SSA blocks/terminators | Backend-ready CFG representation | Invalid branch target/arity and loop recanonicalization failures. |
| Effects/Legality | Tree or SSA statements | Effect/resource legality annotations | Conservative blocker due to unresolved side effects/alias risk. |

## Troubleshooting by Symptom

- "Option rejected immediately": start at `API.md`.
- "Builds IR but fails before optimization": start at `NORMALIZE_AND_VERIFY.md`.
- "No optimization happened": start at `EFFECTS_LEGALITY.md`, then `OPTIMIZATION.md`.
- "Typed but cannot emit": start at `TYPING.md`, then `EMISSION.md`.
- "Compiled artifact exists but call fails": start at `RUNTIME_AND_ABI.md` and `docs/BACKEND/ABI.md`.
- "SSA backend path fails only": start at `SSA.md` and `CFG.md`.
- "Need error-text to owner mapping quickly": use `docs/DIAGNOSTICS_INDEX.md`.

## Jargon Quick Map

- "HIR/LIR": high-level vs lowered Tree IR forms.
- "Legality class": optimizer safety class (`None`, `Read`, `Write`, `RNG`, `Unknown`).
- "Boundary verifier": stage gate that rejects invalid SSA/CFG shape before proceeding.
- "Bridge": runtime loader/call path between R and native artifacts.

## Documents

- [API.md](./API.md) - User-facing entry points and what each returns.
- [AST_AND_BUILD.md](./AST_AND_BUILD.md) - How R functions become Tree IR.
- [HIR.md](./HIR.md) - High-level Tree IR forms and invariants.
- [NORMALIZE_AND_VERIFY.md](./NORMALIZE_AND_VERIFY.md) - Canonicalization and legality checks.
- [OPTIMIZATION.md](./OPTIMIZATION.md) - CSE, fold, LICM, DCE, and loop fusion.
- [CSE.md](./CSE.md) - Common subexpression elimination details.
- [LICM.md](./LICM.md) - Loop-invariant code motion details.
- [DCE.md](./DCE.md) - Dead code elimination details.
- [LIR.md](./LIR.md) - Lowered core Tree IR contract before emit.
- [SCHEDULING.md](./SCHEDULING.md) - Unroll/tile/reduction/simd scheduling metadata.
- [TYPING.md](./TYPING.md) - Type inference, cast insertion, typed annotations.
- [EMISSION.md](./EMISSION.md) - Mojo statement/expression emission and guards.
- [SSA.md](./SSA.md) - Structured-to-SSA conversion and SSA pipeline role.
- [CFG.md](./CFG.md) - Backend CFG shape and loop recovery/recanonicalization.
- [EFFECTS_LEGALITY.md](./EFFECTS_LEGALITY.md) - Effect classes, resources, legality classes.
- [RUNTIME_AND_ABI.md](./RUNTIME_AND_ABI.md) - Build artifacts, bridge, ABI/runtime constraints.
- [../DIAGNOSTICS_INDEX.md](../DIAGNOSTICS_INDEX.md) - Centralized diagnostics map (error text -> boundary -> owner docs).
- [../KNOWN_ISSUES.md](../KNOWN_ISSUES.md) - Consolidated current limitations/deferred routes.

## Primary Code Locations

- `packages/mojor/R/transpile/transpile.R`
- `packages/mojor/R/transpile/entry_stages.R`
- `packages/mojor/R/transpile/ir_helpers.R`
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/lowering.R`
- `packages/mojor/R/ir/typing.R`
- `packages/mojor/R/ir/stmt_emit.R`
- `packages/mojor/R/ir/expr_emit.R`
- `packages/mojor/R/ir/ssa.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/ssa_backend.R`

Primary implementation lives in `packages/mojor/R/...`.

## Internal Symbol Index

Stage router and common helpers:

- `.mojor_pipeline_entry()` - entry dispatch (`packages/mojor/R/transpile/entry_stages.R`)
- `.mojor_normalize_options()` - option normalization (`packages/mojor/R/transpile/entry_stages.R`)
- `.mojor_build_ir()` - AST to HIR builder (`packages/mojor/R/transpile/transpile.R`)
- `.mojor_run_optimize_passes()` - optimization orchestrator (`packages/mojor/R/ir/lowering.R`)
- `.mojor_emit_mojo()` - emission driver (`packages/mojor/R/ir/stmt_emit.R`)
- `.mojor_runtime_load()` - runtime bridge loader (`packages/mojor/R/runtime/bridge.R`)

For stage-specific symbols, see individual stage documents.

## Reference Conventions

Each pipeline document is expected to include:
- Stage purpose and where it sits in end-to-end flow.
- Exact implementation ownership (function names + files).
- Stage contract (inputs, outputs, invariants).
- Practical diagnostics/failure surfaces.
- Internal symbol index (stage-specific internal entrypoints/helpers used in implementation).

For node/op-level ownership details spanning Tree and SSA:
- `docs/IR/TREE_NODE_REFERENCE.md`
- `docs/BACKEND/SSA_OP_REFERENCE.md`
