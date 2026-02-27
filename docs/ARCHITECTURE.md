# MöjoR Architecture

**Document Version:** 1.2 **Last Updated:** 2026-02-19 **Status:** Architecture Overview

---

## Executive Summary

MöjoR is a "Numba for R" system that transpiles a supported subset of R into Mojo and compiles native kernels for fast numeric loops.

**Scope:** Pragmatic loop acceleration—not a full R reimplementation.

**Architecture:** Two-tier IR model (Tree IR with downstream SSA tier for structured lowering and backend prep).

## Plain-Language Mental Model

MöjoR is a staged translator with safety gates:

1. Read R code into structured IR.
2. Prove the IR is legal.
3. Apply safe optimizations.
4. Emit Mojo code.
5. Compile and call native code from R.

If any safety gate fails in strict mode, compilation stops with diagnostics instead of guessing.

## Mini End-to-End Example

For a function like:

```r
f <- function(x, y, n) {
  out <- numeric(n)
  for (i in 1:n) out[i] <- x[i] + y[i]
  out
}
```

High-level flow:
- Build/normalize/verify create legal Tree IR nodes (`loop`, `index`, `binop`, `assign`).
- Optimize may remove redundancy if legality allows.
- Lower/schedule/type stages prepare emit-ready IR with explicit casts/index behavior.
- Emit generates Mojo loop code with guard policy applied.
- Build/ABI bridge compiles and exposes a callable wrapper to R.

Result: R-facing behavior is preserved while loop execution runs through native code.

## Beginner Debug Checklist

1. Identify first failing stage (build, verify, optimize, lower, schedule, type, emit, build/load).
2. Check strict-vs-compatible mode; strict failures are expected to stop early.
3. For optimization surprises, inspect effect/resource legality before pass internals.
4. For runtime mismatches, check ABI/bridge contracts after confirming emitted code shape.
5. Use stage docs in `docs/PIPELINE` for step-specific ownership.

---

## Core Architecture

### Two-Tier IR Model

MöjoR uses a **Tree IR** pipeline:

1. **Tree IR (HIR)** - High-level IR with:
 - Structured control flow with implicit merges
 - Core IR nodes (emittable after typing)
 - HIR sugar (must be lowered before emission)

2. **Tree IR (LIR)** - Low-level IR subset:
 - Contains only Core IR nodes
 - Stable input to scheduling, type check, and emission

3. **SSA Tier (Downstream)** - For structured lowering:
 - Enforces strict syntax/semantic boundaries
 - Canonical ops and memory/effect/resource legality
 - Deterministic structured→CFG lowering

### Pipeline Stages

```
R function → AST → Tree IR (HIR) → normalize → verify → optimize → lower to LIR → schedule → type-check → emit Mojo → `mojo build` → .so → .Call()
```

1. **Build** - R AST → Tree IR (produces untyped Tree IR)
2. **Normalize** - Structural normalization only
3. **Verify** - Structural + scope + loop legality checks
4. **Optimize** - CSE/fold/LICM/DCE passes
5. **Lower** - HIR → LIR (eliminates sugar)
6. **Schedule** - Adds metadata transforms (tiling/SIMD routing)
7. **Type Check** - Inserts casts/coercions
8. **Emit** - Emits Mojo source with guards

Detailed per-step docs are in [PIPELINE/README.md](./PIPELINE/README.md).

### Detailed Stage Mechanics

This section describes what each stage consumes, what it transforms, and what
it must guarantee before handing off to the next stage.

#### 1) Build (R AST -> Tree IR HIR)

What happens:
- Function arguments and declared types are normalized.
- The function body is decomposed into blocks and loop structures.
- Early scans collect feature needs (for example RNG helpers, FFI usage, and
  set/match helpers).
- Statements/expressions are mapped to Tree IR nodes.

Primary modules:
- `packages/mojor/R/transpile/transpile.R`
- `packages/mojor/R/ir/build.R`
- `packages/mojor/R/ir/expr_build.R`
- `packages/mojor/R/ir/nodes.R`

Outputs:
- Untyped Tree IR (HIR + core nodes).
- Initial transpile context (types, loop metadata seeds, output-shape intent).

#### 2) Normalize

What happens:
- IR is canonicalized into stable structural forms (`.mojor_ir_normalize(...)`).
- Shape/type-adjacent simplifications are applied before hard verification
  (type-folding and dim-lowering helpers in the transpile prep path).

Primary modules:
- `packages/mojor/R/transpile/ir_helpers.R`
- `packages/mojor/R/ir/ir.R`

Outputs:
- Normalized Tree IR with fewer representation variants.

#### 3) Verify

What happens:
- Tree verifier checks node shape, required fields, legal operator/type tokens,
  assignment LHS legality, and loop context rules (`break`/`next` inside loops).
- In strict mode (`ir_only = TRUE`), compatibility `raw` escapes are rejected.

Primary modules:
- `packages/mojor/R/ir/verify.R`

Outputs:
- Verified IR (or hard error in strict mode).

#### 4) Optimize

What happens:
- Tree IR optimization pass runs CSE/folding/LICM/DCE according to `opt_level`.
- Effect/resource legality gates rewrites (RNG and Unknown are barriers).
- If suspicious LICM hoists are detected, optimization is retried conservatively
  at a lower level.

Primary modules:
- `packages/mojor/R/transpile/ir_helpers.R`
- `packages/mojor/R/ir/fusion.R`
- `packages/mojor/R/ir/ssa.R` (shared legality foundations)

Outputs:
- Optimized Tree IR with semantics-preserving rewrites.

#### 5) Lower (HIR -> LIR)

What happens:
- High-level nodes are lowered to core loop/index forms using layout context.
- Lowering uses maps such as `len_var_map`, `nrow_var_map`, `ncol_var_map`,
  and tensor metadata for correct indexing and bounds semantics.
- Typical lowered forms: `scalar_reduce`, slice/subscript assignment patterns,
  and vector-expression loop expansion.

Primary modules:
- `packages/mojor/R/ir/lowering.R`
- `packages/mojor/R/transpile/ir_helpers.R`

Outputs:
- LIR-style Tree IR suitable for scheduling, typing, and emission.

#### 6) Schedule

What happens:
- Scheduling metadata is attached or rewritten (unroll/tile/reduction routing).
- Auto-policy can choose modes based on loop shape/safety and optimization
  level.
- Eligible reductions may be scheduled as tree/simd forms.
- Optional loop fusion pass may run when enabled and legal.

Primary modules:
- `packages/mojor/R/transpile/entry_stages.R`
- `packages/mojor/R/transpile/ir_helpers.R`
- `packages/mojor/R/ir/lowering.R`

Outputs:
- Scheduled IR plus effective scheduling metadata.

#### 7) Type Check

What happens:
- Statement typing infers/validates expression types.
- Cast/coercion insertion enforces assignment and boolean-context rules.
- Typed annotations are attached for emission and diagnostics.

Primary modules:
- `packages/mojor/R/ir/typing.R`
- `packages/mojor/R/transpile/ir_helpers.R`

Outputs:
- Typed Tree IR (or strict failure when typing cannot be established).

#### 8) Emit (Tree IR -> Mojo source)

What happens:
- Statement emitter walks typed IR and generates Mojo code.
- Expression emission handles scalar/index contexts and helper routing.
- Guard emission inserts NA and bounds checks according to policy.
- Emitted code uses threaded layout/index context (`index_base`,
  `zero_based_vars`, `layout_ctx`) to preserve R-facing semantics.

Primary modules:
- `packages/mojor/R/ir/stmt_emit.R`
- `packages/mojor/R/ir/expr_emit.R`
- `packages/mojor/R/ir/bounds_emit.R`

Outputs:
- Mojo source lines plus helper/runtime requirements for build packaging.

### Strict vs Compatible Behavior

- **Strict (`ir_only = TRUE`)**: normalize/verify/type/emit stage failures are
  hard errors with explicit diagnostics.
- **Compatible mode**: stage helpers may return `NULL` for a failed step; the
  caller can use guarded fallbacks where the feature surface allows it.
- Loop compilation is IR-first; fallback boundaries are explicit and policy
  gated.

### Post-Emit Runtime Path

After emission, Mojo source is compiled and linked into a shared library, then
called through generated `.Call` wrappers. Bridge and backend layers enforce
runtime contracts (shape/type checks, copy-on-modify behavior under
`semantics = "r"`, and backend availability handling).

### SSA Backend Prep Path (Optional)

When SSA artifacts are requested (`emit_ssa_backend = TRUE`), MöjoR runs a
downstream SSA-oriented pipeline after Tree IR preparation:

- `to_ssa`: converts structured Tree IR into SSA form.
- `memory_canonicalize`: normalizes memory ops to canonical spellings.
- `annotate_effects_resources`: attaches legality metadata for motion/elimination safety.
- `loop_recovery` and `recanonicalize_loop_cfg`: re-establishes coherent loop metadata at CFG tier.
- `fusion_candidate_analysis`: performs analysis-only fusion candidacy checks (no rewrite in this stage).
- `typing` and `optimize_ssa`: runs SSA-level typing and cleanup passes.
- `backend_cfg` (and optional backend-specific lowering/selection/codegen): produces backend-facing IR artifacts.

Key contract point:
- Boundary verifiers enforce stage-specific invariants (`post_canonicalize`,
  `post_structured_lowering`, `post_memory_canonicalize`, `post_recanonicalize`,
  `post_typing`) before downstream stages proceed.

### Implementation Map (Current)

Primary transpile entry and orchestration:
- `packages/mojor/R/transpile/transpile.R`
- `packages/mojor/R/transpile/entry_stages.R`
- `packages/mojor/R/transpile/ir_helpers.R`

Primary Tree IR modules:
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/verify.R`
- `packages/mojor/R/ir/lowering.R`
- `packages/mojor/R/ir/stmt_emit.R`
- `packages/mojor/R/ir/expr_emit.R`

Primary SSA boundary/backend modules:
- `packages/mojor/R/ir/ssa.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/ssa_semantic.R`
- `packages/mojor/R/ir/ssa_backend.R`

All of the above live under `packages/mojor/R/...`.

---

## Key Conventions

### Indexing Convention

- R semantics are 1-based and end-inclusive for ranges
- Mojo is 0-based and end-exclusive
- **MöjoR convention:** Loop variables are 1-based in generated Mojo

```mojo
for i in range(1, n + 1): # 1-based induction variable
 x[Int(i - 1)] # Subtract 1 for 0-based indexing
```

### NA Policy

- Default: `na_guard = "forbid"`
- `unsafe` allowed where explicitly requested
- NA guard emission is centralized and deduplicated per-statement

### Bounds Policy

- Bounds checks use `len_var_map`/layout metadata
- For bounds-checked reads, emit `_mojor_read_*` helpers

### Layout

- Matrices/arrays use column-major layout by default
- Layout metadata is threaded through `layout_ctx`

### RNG

- RNG uses `src/rng_helpers.mojo`
- RNG is an effect boundary: never CSE, never LICM, never fuse across

---

## Effect System

MöjoR uses an effect/resource legality model for optimization safety:

- **Tree effect classes:** `Pure`, `ReadsMem`, `RNG`, `Unknown`
- **Legality classes (optimization):** `None`, `Read`, `Write`, `RNG`, `Unknown`

### Effect Classes (Tree IR)

- **`Pure`**: Expression has no observable side effects and does not depend on mutable runtime state.
  - Typical use: arithmetic on already-loaded scalars.
- **`ReadsMem`**: Expression reads memory/state but does not write.
  - Typical use: indexed reads from arrays/vectors.
- **`RNG`**: Expression consumes random state or produces stochastic output.
  - Typical use: `rnorm`, `runif`, and other random draws.
- **`Unknown`**: Safety cannot be proven with current analysis; treat conservatively.
  - Typical use: compatibility/raw paths or operations without full effect annotation.

### Legality Classes (Optimization Contract)

Legality classes are the optimizer-facing form used for rewrite decisions.

- **`None`**: Safe for elimination/reuse/reordering in the local legality scope.
- **`Read`**: Reads resources; cannot be blindly reordered across conflicting writes.
- **`Write`**: Mutates resources; creates ordering and aliasing constraints.
- **`RNG`**: Stateful random effect; ordering is semantically significant.
- **`Unknown`**: Analysis-incomplete; treated as a hard safety barrier.

### Mapping (Effect -> Legality)

- `Pure -> None`
- `ReadsMem -> Read`
- write-style statement/resource effects -> `Write`
- `RNG -> RNG`
- `Unknown -> Unknown`

### How Optimizations Use These Classes

- **CSE (Common Subexpression Elimination):**
  - Requires legality `None`.
  - `Read`, `Write`, `RNG`, and `Unknown` are not CSE-eliminated by default.
- **LICM (Loop-Invariant Code Motion):**
  - May hoist when invariance is proven and resource interference is absent.
  - `RNG`/`Unknown` are never hoisted.
- **DCE (Dead Code Elimination):**
  - Removes only statements with legality `None` when result is unused.
  - Statements with `Read`/`Write`/`RNG`/`Unknown` are preserved unless a stronger proof exists.
- **Fusion/loop rewrites:**
  - Require compatible resource/effect profiles.
  - `RNG` and `Unknown` block unsafe loop reordering/merging.

---

## Jargon Glossary

For the full shared glossary used across docs, see [GLOSSARY.md](./GLOSSARY.md).
For deep implementation pages by concept/stage, see [PIPELINE/README.md](./PIPELINE/README.md).

- **AST (Abstract Syntax Tree):** Parsed tree form of the original R function before IR conversion.
- **IR (Intermediate Representation):** Internal compiler form used between parsing and final code emission.
- **HIR (High-Level IR):** Richer Tree IR with convenient/sugary node forms.
- **LIR (Low-Level IR):** Lowered Tree IR subset used for scheduling, typing, and emission.
- **SSA (Static Single Assignment):** IR form where each value is assigned once, used for stronger backend verification/analysis.
- **CFG (Control-Flow Graph):** Graph of basic blocks and branches that models control-flow paths.
- **CSE (Common Subexpression Elimination):** Reuses equivalent pure computations instead of recomputing them.
- **LICM (Loop-Invariant Code Motion):** Moves loop-invariant computations outside loops.
- **DICM:** Common typo/shorthand in discussions; in this codebase the intended term is usually **LICM**.
- **DCE (Dead Code Elimination):** Removes computations whose results are never needed.
- **Fusion (Loop Fusion):** Merges compatible adjacent loops to reduce overhead and improve locality.
- **SIMD (Single Instruction, Multiple Data):** Vectorized execution mode for data-parallel operations.
- **FFI (Foreign Function Interface):** Mechanism for calling native code from R and vice versa.
- **ABI (Application Binary Interface):** Binary-level calling and symbol contract between compiled components.
- **`.Call` bridge:** R C-API entrypoint used to call compiled native wrappers.
- **`ir_only` strict mode:** Policy mode that forbids compatibility raw escapes and requires strict IR success.
- **NA guard:** Emitted runtime checks/policy for missing values (for example forbid/assign behavior).
- **`layout_ctx`:** Threaded layout/index metadata used during lowering and emission.
- **`index_base`:** Whether an index expression is semantically one-based (R style) or zero-based.

## Supported R Subset

See [SUBSET.md](./SUBSET.md) for the current supported subset.

**Current focus:** Numerical loops and array operations

**Not supported (by design):**
- S3/S4 method dispatch (would require runtime dispatch infrastructure)
- Non-Standard Evaluation (NSE)
- Full R semantics (lists, environments, lazy eval)

---

## Project Structure

```
MojoR/
├── docs/ # Documentation (this directory)
├── packages/mojor/ # Core package
├── scripts/ # Validation and release scripts
└── tools/ # Development tools
```

---

## Getting Started

1. Read [COOKBOOK.md](./COOKBOOK.md) for examples
2. Check [FEATURES.md](./FEATURES.md) for supported features
3. Review [IR/](./IR/) for technical details

---

## Contributing

See [IMPLEMENTATION.md](./IR/IMPLEMENTATION.md) for implementation details and [PLAN.md](./IR/PLAN.md) for future direction.
