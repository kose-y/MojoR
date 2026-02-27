# API Step

This page documents the front door into MöjoR and how API calls map to compiler steps.

## Main Entry Points

- `mojor_transpile(fn, ...)`
- `mojor_build(fn, ...)`
- `mojor_fn(signature, body, ...)`
- `mojor_jit(fn, ...)`
- `mojor_njit(fn, ...)`
- `mojor_vectorize(fn, ...)`
- `mojor_guvectorize(fn, ...)`
- `mojor_prange(n)`
- `mojor_run_chains_parallel(chain_fun, ...)`

## What Each API Does

`mojor_transpile`:
- Input: R function + explicit type annotations.
- Output: transpilation payload (`mojo`, metadata, optional IR/SSA artifacts).
- Does not compile shared libraries.

`mojor_build`:
- Calls transpile, then compiles and optionally loads callable wrappers.
- Returns build artifacts (`func`, wrapper paths, transpile payload, success status).

`mojor_fn`:
- Convenience wrapper around `mojor_build`.
- Accepts function-first or signature-string forms.

`mojor_jit` / `mojor_njit` / `mojor_vectorize`:
- Runtime-specializing wrappers.
- Infer call signatures and cache compiled kernels per signature.
- `mojor_njit` enforces strict object mode (`object_mode = "off"`).

`mojor_guvectorize`:
- Generalized vectorization wrapper with explicit signature/core-dims contracts.
- Supports CPU/GPU target selection under strict subset constraints.

`mojor_prange`:
- Parallel loop alias used in transpiled loop bodies.
- Interpreted fallback behavior is `seq_len(n)`.

`mojor_run_chains_parallel`:
- Runtime helper to run independent chains with deterministic per-chain seeding.
- Supports `backend = "auto" | "fork" | "sequential"` with OS/backend guards.

## Plain-Language Mental Model

API choice is mostly about when compilation happens and how strict routing is:

- `transpile`: produce artifacts and diagnostics, no native build.
- `build`: transpile + compile + load callable wrapper.
- `jit` family: compile lazily from runtime-observed signatures and cache.

Use strict variants (`mojor_njit`, strict options) when you want unsupported
forms to fail early instead of using non-strict fallback routes.

## API Input/Output Snapshot

| API | Input shape | Output shape | Typical usage |
|---|---|---|---|
| `mojor_transpile(...)` | Function + type/spec/options | Transpile payload (source + diagnostics + optional IR/SSA artifacts) | Inspect and debug compile pipeline without native build. |
| `mojor_build(...)` | Same as transpile + build options | Build payload (`func`, build paths, transpile payload, status) | Produce callable compiled function. |
| `mojor_fn(...)` | Signature/body shorthand or function-form args | Build payload (same contract as `mojor_build`) | Concise API for scripted kernels and examples. |
| `mojor_jit(...)` | Runtime function call arguments | Value result from cached/compiled wrapper | Lazy compile by observed signature. |
| `mojor_njit(...)` | Runtime function call arguments | Value result (strict object-mode policy) | Same as JIT but stricter no-fallback behavior. |
| `mojor_vectorize(...)` | Function + runtime vector arguments | Value result via vectorized wrapper route | Vectorized dispatch with JIT-style specialization. |
| `mojor_guvectorize(...)` | Function + typed signature/core-dims metadata | Value result or build payload (when `load = FALSE`) | Generalized vectorization wrapper with strict core-dims contracts. |
| `mojor_prange(...)` | Non-negative scalar loop extent | Parallel range alias in transpiled loops | Prefer in compiled loop bodies (interpreted behavior matches `seq_len`). |
| `mojor_run_chains_parallel(...)` | Chain function + chain/runtime backend options | Ordered list of per-chain outputs | Independent chain execution helper with deterministic seeding. |

## Plain-Language Choosing Guide

If you are unsure which API to use, choose by goal:

- "Inspect generated code only" -> `mojor_transpile(...)`
- "Get a callable compiled function now" -> `mojor_build(...)`
- "Build from compact signature/body form" -> `mojor_fn(...)`
- "Compile lazily on first call, then reuse cached binaries" -> `mojor_jit(...)`
- "JIT with stricter behavior (no relaxed object-mode route)" -> `mojor_njit(...)`
- "Vectorized wrapper style entrypoint" -> `mojor_vectorize(...)`
- "Generalized vectorization with explicit core-dims signatures" -> `mojor_guvectorize(...)`
- "Parallel loop syntax alias inside transpiled loop bodies" -> `mojor_prange(...)`
- "Run independent chains across workers with deterministic seeds" -> `mojor_run_chains_parallel(...)`

Simple mental model:
- `transpile` = translate only
- `build` = translate + compile + load
- `jit` = build on demand per signature and cache

## Mini Example

Typical debugging-to-production flow:

1. Start with `mojor_transpile(...)` to inspect IR/emission and catch shape or
   strict-mode issues.
2. Move to `mojor_build(...)` when transpile output looks correct and you want a
   callable native function.
3. Switch to `mojor_jit(...)` when you want runtime signature specialization and
   cache reuse in repeated calls.

## What Happens on a Typical `mojor_jit(...)` Call

1. You call the wrapper with concrete argument types/shapes.
2. MöjoR builds a signature key from those runtime arguments.
3. It checks cache for that exact key.
4. On cache miss, it runs transpile/build and stores artifacts.
5. It invokes the compiled wrapper.
6. Later calls with the same signature skip compilation.

## Parallel Helper Mini Examples

`mojor_prange` loop alias in a transpiled kernel:

```r
f <- function(x, out) {
 for (i in mojor_prange(length(x))) {
 out[i] <- x[i] * 2
 }
 out
}
```

`mojor_run_chains_parallel` deterministic local smoke:

```r
chain_fun <- function(n) {
 x <- numeric(n)
 for (i in seq_len(n)) x[i] <- i
 sum(x)
}

mojor_run_chains_parallel(
 chain_fun = chain_fun,
 n_chains = 2,
 chain_args = list(n = 5),
 backend = "sequential",
 use_mojor_rng = FALSE
)
```

Constraint reminders:
- `mojor_prange(...)` expects scalar argument forms accepted by transpile loop analysis.
- `mojor_run_chains_parallel(...)` requires positive integer `n_chains` and valid backend policy.

## Key Controls Passed Into the Pipeline

Common controls:
- Semantics: `semantics = "r" | "raw"`
- Optimization: `opt_level`, `fusion`
- Scheduling: `unroll`, `tile`, `reduction`, `simd_mode`, `parallel`
- Safety: `bounds_check`, `na_mode`, `index_base`
- Artifact emission: `emit_ir`, `emit_ssa_backend`

## Mode Map

These are the main policy/runtime modes users trip over:

| Mode | Allowed values | Where set | What it controls | Key interactions |
|---|---|---|---|---|
| `ir_only` | `TRUE`, `FALSE` | `mojor_options(...)`, per-call args | Strict IR policy gate. | `TRUE` forbids non-strict fallback paths (including object-mode fallback and `raw` escapes). |
| `object_mode` | `"off"`, `"fallback"`, `"hybrid"` | `mojor_build(...)`, `mojor_fn(...)` pass-through | Behavior when strict compile/transpile misses occur. | Must be `"off"` when `ir_only=TRUE`; `"fallback"` and `"hybrid"` enable object-mode fallback lanes. |
| `gpu_jit_mode` | `"auto"`, `"unified_preview"` | `mojor_options(...)`, transpile/build args | GPU lowering policy for helper-backed vs unified helper-free emission. | `"auto"` allows unified/helper-backed selection by lowering analysis; `"unified_preview"` rejects helper-backed emission; `gpu_matmul` expression form requires `gpu_jit_mode` in `{auto, unified_preview}`. |
| `semantics` | `"r"`, `"raw"` | transpile/build args | Runtime semantic contract strictness. | `"r"` preserves R-facing behavior; `"raw"` is performance-oriented and less conservative. |

`preview` in MöjoR means a constrained lowering/specialization status, not a product beta flag.
In `mojor_fn(...)`, "preview path" diagnostics refer to no-loop-specialized forms lowered through
the same loop/IR pipeline, with strict/object-mode policy deciding fallback behavior.

## Strict vs Non-Strict Behavior

- Strict mode (`ir_only = TRUE` via options) treats stage failures as hard errors.
- Non-strict mode may allow controlled fallback behaviors depending on route.

## Code Mapping

- API docs/signatures: `packages/mojor/R/roxygen_api_docs.R`
- Transpile entrypoint: `packages/mojor/R/transpile/transpile.R`
- Build entrypoint: `packages/mojor/R/build/build.R`

## Detailed Ownership and Contracts

### Dispatch Ownership

| Surface API | Primary implementation owner | Contract boundary |
|---|---|---|
| `mojor_transpile(...)` | `packages/mojor/R/transpile/transpile.R` | Produces transpile payload (source + analysis metadata + optional IR/SSA debug artifacts) without compiling shared libraries. |
| `mojor_build(...)` | `packages/mojor/R/build/build.R` | Extends transpile with compile/link/load steps and wrapper creation. |
| `mojor_fn(...)` | `packages/mojor/R/core/core.R` | Convenience entry that parses signatures and routes into build (including fallback policy handling). |
| `mojor_jit(...)`, `mojor_njit(...)`, `mojor_vectorize(...)` | `packages/mojor/R/core/reductions_jit.R` | Runtime signature-specializing wrappers over build/transpile routes with cache policy. |
| `mojor_guvectorize(...)` | `packages/mojor/R/core/reductions_jit.R` | Generalized vectorization wrapper with signature/core-dims contracts. |
| `mojor_prange(...)` | `packages/mojor/R/core/reductions_jit.R` | Parallel range alias helper for transpiled loops. |
| `mojor_run_chains_parallel(...)` | `packages/mojor/R/runtime/parallel_subprocess.R` | Parallel/sequential chain execution helper with deterministic seeding. |
| Entry option normalization | `.mojor_prepare_transpile_entry_options(...)` in `packages/mojor/R/transpile/entry_stages.R` | Canonicalizes user controls before pipeline execution. |

### Artifact Expectations

- `mojor_transpile(...)` returns a payload centered on emitted source and analysis fields; optional fields are lane-policy dependent (`emit_ir`, `emit_ssa_backend`, no-loop specialization metadata, strict mode/fallback policy outcomes).
- `mojor_build(...)` returns build/runtime artifacts (`func`, build paths, compile status, embedded transpile payload) and may annotate object-mode/fallback details.
- JIT-family APIs maintain per-signature cache entries; compiled artifacts are reused when signature and policy constraints match.

### Failure Surfaces

- Option contract violations are rejected in entry-stage normalization (invalid `opt_level`, schedule knobs, semantics, etc.).
- Strict mode (`ir_only=TRUE`) upgrades fallback-capable errors into hard failures.
- Build-time failures include toolchain/link/load problems and bridge/wrapper ABI mismatch.

## Representative API Diagnostics

These are common first-failure messages from API entry or option normalization:

- `mojor_transpile: opt_level must be an integer between 0 and 3 (or NULL)`
- `mojor_transpile: unroll must be an integer between 1 and 16`
- `mojor_transpile: tile values must be positive integers`
- `mojor_fn: each argument must be annotated like x: f64[]`
- `mojor_build: object_mode must be 'off' when ir_only=TRUE`
- `mojor_transpile: mojor_prange() scalar must be i32`
- `mojor_run_chains_parallel: n_chains must be a positive integer`
- `mojor_run_chains_parallel: backend='fork' is not supported on Windows`

If one of these appears, fix API/options first before debugging deeper pipeline stages.

## Beginner Debug Checklist

1. Verify you picked the right API (`transpile` vs `build` vs `jit`).
2. Start with minimal options and defaults.
3. Run `mojor_transpile(...)` first to isolate front-end issues from toolchain issues.
4. If transpile passes but build fails, focus on build/link/load and ABI diagnostics.
5. If JIT behaves unexpectedly, check whether the runtime signature changed and forced a new compile.

## Internal Symbol Index

`Entry APIs`
- `mojor_transpile`
- `mojor_build`
- `mojor_fn`
- `mojor_jit`
- `mojor_njit`
- `mojor_vectorize`
- `mojor_guvectorize`
- `mojor_prange`
- `mojor_run_chains_parallel`

`Entry-stage option and policy internals`
- `.mojor_prepare_transpile_entry_options`
- `.mojor_apply_auto_schedule_policy`
- `.mojor_prepare_ssa_backend_stage`
- `.mojor_ssa_stage_status`
- `.mojor_setup_transpile_state_context`

`API routing/helpers`
- `.mojor_build_with_object_mode_fallback`
- `.mojor_resolve_effective_ir_only`
- `.mojor_parse_signature`
- `.mojor_normalize_arg_specs`
