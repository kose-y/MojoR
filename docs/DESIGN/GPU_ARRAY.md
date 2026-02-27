# GPUArray Status and Roadmap (MöjoR)

**Status:** Active **Last Updated:** 2026-02-17 **Scope:** `packages/mojor/` authoritative

---

## Plain-Language Mental Model

GPUArray gives array operations with explicit route transparency.

- Fast path: native GPU helpers/kernels.
- Fallback path: CPU/host routes when GPU path is unavailable or unsafe.
- Contract: every route should be explicit via `gpu_fallback` attribution.

This design prioritizes correctness and observability over hidden behavior.

## Mini Example

For a call like `gpu_reduce("sum", x)`:

1. Runtime checks dtype/shape/capability.
2. If GPU path is legal, route uses GPU helper and tags result as `gpu_reduce`.
3. Otherwise, fallback route executes and tags result as `cpu_reduce`.

Same API call, explicit route outcome.

## Beginner Debug Checklist

1. Always inspect `attr(out, "gpu_fallback")` first.
2. If route is CPU unexpectedly, verify capability probes and dtype/shape constraints.
3. Confirm behavior in `packages/mojor/` runtime and tests.
4. Use focused GPUArray tests before broad integration tests when changing route logic.

## 1) Summary

GPUArray already has a broad runtime surface (constructors, elementwise ops, reductions, matmul/crossprod/tcrossprod, indexing with GPU gather/scatter paths).

Current focus is:

1. Stabilize fallback contracts and transpile/runtime parity.
2. Prefer native GPU routes on fallback-heavy helper paths (`gpu_cast`, `gpu_broadcast`) while preserving explicit fallbacks.
3. Keep additive API growth explicit and test-backed.
4. Keep behavior changes test-backed in `packages/mojor/`.

---

## 2) Implemented Surface (Current)

### 2.1 Core Arrays

- `mojor_gpu_array()`, `GPUArray()`, `gpu_array()`
- Constructors by shape/length: `gpu_zeros()`, `gpu_ones()`, `gpu_full()`, `gpu_empty()`
- Random constructors: `gpu_rand()`, `gpu_randn()`, `gpu_randi()`
- Like constructors: `gpu_*_like()` family
- Dtypes: `f32`, `f64`, `i32`

### 2.2 Ops and Kernels

- `Ops.GPUArray`: `+`, `-`, `*`, `/`
- GPU kernel authoring: `mojor_gpu_kernel()`, `gpu_kernel()`

### 2.3 Reductions and Linear Algebra

- Reductions: `gpu_reduce()`, `gpu_sum()`, `gpu_mean()`, `gpu_min()`, `gpu_max()`, `gpu_argmin()`, `gpu_argmax()`
- Matmul: `gpu_matmul()`, `gpu_matmul_into()`, `%*%`
- Cross products: `crossprod()`, `tcrossprod()` methods for `GPUArray`

### 2.4 Indexing

- `gpu_slice()` with non-zero stride support (forward/reverse)
- `[` extraction using contiguous slice and gather routes
- `[<-` assignment using GPU/CPU scatter routes; contiguous `i32` slice-assign now attempts a native GPU helper path before explicit CPU fallback
- Duplicate index-write semantics are deterministic (`last-write-wins`); rank-1 duplicate assignments now attempt deduped GPU scatter before explicit CPU scatter fallback
- Logical indexing:
 - single-index vector masks (`x[mask]`) remain supported with strict validation
 - per-axis logical masks for rank-N are supported in `[` and `[<-` when `length(mask) == extent`, `NA` is absent, and all-false masks are rejected via the existing empty-index contract

### 2.5 New Additive APIs

- `gpu_cast(x, dtype)`
- `gpu_promote(x, y)`
- `gpu_broadcast(x, shape)`

These APIs are implemented with explicit fallback markers and tests. `gpu_cast` and `gpu_broadcast` now attempt native GPU execution first for `GPUArray` inputs.
`gpu_reduce` now routes full-axis reductions (`dims` covering all axes) through the scalar GPU reduce path before explicit CPU fallback.

---

## 3) Fallback Contract (Standardized)

`attr(out, "gpu_fallback")` values:

- Reductions: `gpu_reduce`, `cpu_reduce`
- Matmul: `gpu_matmul`, `cpu_matmul`
- Cross products: `gpu_crossprod`, `cpu_crossprod`, `gpu_tcrossprod`, `cpu_tcrossprod`
- Indexing/slice paths: `gpu_slice`, `cpu_slice`, `gpu_gather`, `cpu_gather`, `gpu_scatter`, `cpu_scatter`, `cpu_slice_assign`
- Additive helpers: `gpu_cast`, `cpu_cast`, `host_cast`, `gpu_broadcast`, `cpu_broadcast`, `host_broadcast`

Policy:

- Prefer explicit route attribution over silent behavior changes.
- Unsupported or unsafe paths either return a clear fallback route or error with stable diagnostics.

### 3.1 M0 Baseline Inventory (2026-02-17)

Fallback route assertions currently present in the split GPUArray suite (`packages/mojor/tests/testthat/test_gpu_array_*.R`):

| Route | Assertion Count |
|---|---|
| `cpu_broadcast` | 1 |
| `cpu_cast` | 1 |
| `cpu_crossprod` | 4 |
| `cpu_gather` | 12 |
| `cpu_matmul` | 8 |
| `cpu_reduce` | 5 |
| `cpu_scatter` | 10 |
| `cpu_slice` | 11 |
| `cpu_slice_assign` | 2 |
| `cpu_tcrossprod` | 4 |
| `gpu_crossprod` | 3 |
| `gpu_gather` | 22 |
| `gpu_matmul` | 5 |
| `gpu_reduce` | 5 |
| `gpu_scatter` | 17 |
| `gpu_slice` | 12 |
| `gpu_tcrossprod` | 3 |

### 3.2 Extension Touchpoints

Authoritative touchpoints:

- `packages/mojor/R/core/gpu.R`
- `packages/mojor/src/backend.mojo`
- `packages/mojor/src/bridge.c`
- `packages/mojor/src/backend_stub.h`
- `packages/mojor/tests/testthat/test_gpu_array_*.R` (split suite; see `test_gpu_array_SPLIT_MANIFEST.tsv`)

### 3.3 i32 Overflow Contract (Linalg/Reduce Lock-Down)

For `i32` linalg/reduce routes (`gpu_matmul`, `%*%`, `crossprod`, `tcrossprod`, `gpu_reduce("sum")`), overflow-edge behavior at `INT_MAX + 1` is locked and test-backed:

- Host-visible result is route-deterministic at that edge:
 - `gpu_*` route: wrapped `i32` value (two's-complement modulo behavior).
 - `cpu_*` fallback route: `NA_integer_` via host integer-coercion overflow handling.
- Route attribution remains explicit and unchanged:
 - matmul family: `gpu_matmul` / `cpu_matmul`
 - crossprod family: `gpu_crossprod` / `cpu_crossprod`
 - tcrossprod family: `gpu_tcrossprod` / `cpu_tcrossprod`
 - reduce family: `gpu_reduce` / `cpu_reduce`
- Lock-down breadth includes:
 - direct `gpu_matmul` and `%*%`,
 - `gpu_matmul_into`,
 - transpose matmul forms (`transpose_a` / `transpose_b`) including `gpu_matmul_into` output contracts,
 - host-RHS `%*%`, `crossprod`, `tcrossprod`,
 - scalar and dims (`dims=2`) `gpu_reduce("sum")` multi-term accumulation cases,
 - rank-3 `gpu_reduce("sum")` dims/keepdims permutations (`dims=1`, `dims=c(2,3)`, full-axis permutations),
 - matrix-output `gpu_matmul_into` overflow checks and mixed `dims`/`keepdims` reduce permutations.
- This contract is validated in the split suite (`packages/mojor/tests/testthat/test_gpu_array_*.R`).

---

## 4) Transpile Helper Parity

Transpiled helper boundaries are explicit:

- `_mojor_gpu_reduce(...)` uses concrete non-raising `f32`/`f64` helper overloads in transpiled postamble code.
- Verifier enforces transpile helper constraints for `gpu_reduce`:
 - only `dims = NULL`
 - only `keepdims = FALSE`
- Verifier enforces `gpu_matmul` usage only in assignment RHS form.

This avoids silent mismatches between strict transpiled paths and runtime wrappers.

---

## 5) Priority Matrix

| Priority | Area | Current State | Direction |
|---|---|---|---|
| P0 | Fallback contract | Implemented and standardized | Keep reason set stable, extend assertions |
| P0 | Transpile parity | Explicit helper constraints + diagnostics | Keep strict and documented |
| P0 | Docs/status truth sync | Updated | Maintain per release |
| P1 | Dtype helpers | `gpu_cast` / `gpu_promote` implemented with GPU-first cast attempt; same-dtype cast now uses device clone path before CPU fallback | Complete (P1 closeout 2026-02-17) |
| P1 | Explicit broadcast | `gpu_broadcast` implemented with GPU gather-first route and route-audit coverage for rank-1/2/3 plus shape-stable paths | Complete (P1 closeout 2026-02-17) |
| P1 | Indexing breadth | strict per-axis logical masks implemented for `[` / `[<-` across rank-N; contiguous `i32` slice-assign and rank-1 duplicate-write dedup now have GPU-first paths | Complete (P1 closeout 2026-02-17) |
| P2 | `i32` reduction/matmul | Implemented across reduce/matmul/%*%/crossprod/tcrossprod; overflow lock-down now includes transpose matmul paths, rank-3 reduce dims permutations, and dedicated i32 linalg/reduce route-audit gating | Complete (P2 closeout 2026-02-17) |
| P2 | f64 backend weakness reduction | mode-aware f64 capability probes + cache (`matmul`/`gemv`/`gevm`, `value`/`arg` reduce classes); route-audit runner uses stale-lock-safe serialized execution with CPU-routed safe mode; live probe path uses capped f64 matmul probe breadth (`MOJOR_GPU_F64_PROBE_MAX_SHAPES=1`) to avoid repeated hang-prone probe ganging while keeping opt-in deeper triage | Complete (P2.1 closed 2026-02-17) |
| P3 | Perf extras (pooling/BLAS) | Deferred | Evaluate after correctness lock |

---

## 6) Function Checklist

| Function/Surface | Status |
|---|---|
| `mojor_gpu_array`, `GPUArray`, `gpu_array` | Implemented |
| `gpu_zeros/ones/full/empty`, `gpu_rand/randn/randi`, `*_like` | Implemented |
| `Ops.GPUArray` (`+ - * /`) | Implemented |
| `gpu_reduce`, wrappers (`sum/mean/min/max/argmin/argmax`) | Implemented |
| `gpu_matmul`, `gpu_matmul_into`, `%*%`, `crossprod`, `tcrossprod` | Implemented |
| `gpu_slice`, `[`/`[<-` gather/scatter | Implemented |
| `gpu_cast` | Implemented |
| `gpu_promote` | Implemented |
| `gpu_broadcast` | Implemented |
| logical-mask single-index extraction | Implemented |
| logical-mask per-axis indexing/assignment (rank-N) | Implemented |
| non-zero stride slicing (forward/reverse) | Implemented |
| contiguous `i32` slice-assign GPU helper route | Implemented |
| `i32` reduction/matmul/crossprod/tcrossprod | Implemented |

---

## 7) Testing Expectations

Required focused checks:

```bash
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_ir_verify.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_transpile_helpers.R")'
```

---

### 7.1 Perf Smoke Baseline and Gate (M3-B/M4)

GPUArray perf smoke covers core routes:

- `gpu_cast`
- `gpu_broadcast`
- `gpu_reduce`
- `gpu_matmul`

Authoritative harness:

- `packages/mojor/tools/benchmark_gpuarray_core.R`

Committed baseline:

- `docs/BASELINES/GPUARRAY_PERF_BASELINE.csv`

Baseline refresh command:

```bash
Rscript packages/mojor/tools/benchmark_gpuarray_core.R \
 --runs=5 --warmup=2 --n=1048576 --m=512 --k=512 --p=512 \
 --write-baseline=docs/BASELINES/GPUARRAY_PERF_BASELINE.csv
```

Regression gate command:

```bash
Rscript packages/mojor/tools/benchmark_gpuarray_core.R \
 --runs=3 --warmup=1 --n=1048576 --m=512 --k=512 --p=512 \
 --baseline=docs/BASELINES/GPUARRAY_PERF_BASELINE.csv
```

Gate policy:

- Thresholded median-time deltas by metric (`gpu_cast`, `gpu_broadcast`, `gpu_reduce`, `gpu_matmul`).
- Near-zero baseline medians are compared with a floor value to avoid false spikes from timer resolution.
- Route attribution (`route` column) is captured as dominant observed fallback label for smoke observability.

---

### 7.2 Index Route Baseline and Gate (M5/M6/M7)

GPUArray indexing fallback burn-down is tracked with a dedicated route audit (including contiguous `i32` assignment scenarios that previously hit `cpu_slice_assign` on Metal):

- `packages/mojor/tools/audit_gpuarray_index_routes.R`

Committed index-route baseline:

- `docs/BASELINES/GPUARRAY_INDEX_ROUTE_BASELINE.csv`

Baseline capture command (before a new indexing route-change slice):

```bash
Rscript packages/mojor/tools/audit_gpuarray_index_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_INDEX_ROUTE_BASELINE.csv
```

Targeted drop gate command:

```bash
Rscript packages/mojor/tools/audit_gpuarray_index_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_INDEX_ROUTE_BASELINE.csv
```

Gate policy:

- `cpu_gather` current count must be `<= floor(baseline * 0.75)`
- `cpu_scatter` current count must be `<= floor(baseline * 0.85)`
- `cpu_slice` current count must be `<= floor(baseline * 1.00)` (no-growth)
- `cpu_slice_assign` current count must be `<= floor(baseline * 0.50)`
- Existing strict diagnostics/fallback labels remain unchanged.

---

### 7.3 f64 Linalg/Reduce Route Baseline and Gate

GPUArray f64 linalg/reduce fallback tracking is captured by a dedicated route audit:

- `packages/mojor/tools/audit_gpuarray_f64_routes.R`

Audit coverage:

- matmul families: `matmul`, `gemv`, `gevm`
- reduce families: scalar/dims `sum`, scalar/dims `argmax`

Committed f64-route baseline:

- `docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv`

Baseline capture command:

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv
```

Release closeout gate (no-growth profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv \
 --profile=closeout
```

Targeted-drop gate (tightening profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv \
 --profile=targeted-drop
```

Live-probe opt-in (debug/triage only):

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv \
 --profile=closeout \
 --allow-gpu-probe=1
```

Operational notes:

- Route-audit execution is serialized via a lockdir (`build/gpuarray_f64_route_audit.lockdir`) with stale-lock eviction.
- Default mode pre-seeds f64 capability cache to explicit CPU routes for stable closeout gating.
- Live probe mode remains opt-in for backend stall triage.
- f64 live matmul probe breadth is capped by default (`MOJOR_GPU_F64_PROBE_MAX_SHAPES=1`) to avoid repeated backend probe calls in hang-prone paths.
- Deep triage can raise probe breadth explicitly (for example `MOJOR_GPU_F64_PROBE_MAX_SHAPES=2`).
- CI usage split:
 - safe closeout gate: run default mode (no `--allow-gpu-probe`).
 - live probe debug lane: run with `--allow-gpu-probe=1` and `MOJOR_GPU_F64_PROBE_MODE=subprocess` (optionally `MOJOR_GPU_F64_PROBE_TIMEOUT_SEC=45`) to isolate hang-prone probe calls.

---

### 7.4 Cast/Broadcast Route Baseline and Gate

GPUArray cast/broadcast fallback tracking is captured by a dedicated route audit:

- `packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R`

Audit coverage:

- cast matrix: `f32->f64`, `f32->i32`, `f64->f32`, `f64->i32`, `i32->f32`, `i32->f64`, same-dtype cast clone
- broadcast families: rank-1->2, rank-2->2 expansion, rank-3 expansion, and shape-stable broadcast

Committed cast/broadcast baseline:

- `docs/BASELINES/GPUARRAY_CAST_BROADCAST_ROUTE_BASELINE.csv`

Baseline capture command:

```bash
Rscript packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_CAST_BROADCAST_ROUTE_BASELINE.csv
```

Release closeout gate (no-growth profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_CAST_BROADCAST_ROUTE_BASELINE.csv \
 --profile=closeout
```

Targeted-drop gate (tightening profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_CAST_BROADCAST_ROUTE_BASELINE.csv \
 --profile=targeted-drop
```

Gate policy:

- Route labels remain unchanged: `gpu_cast`, `cpu_cast`, `gpu_broadcast`, `cpu_broadcast`.
- `closeout` profile enforces no-growth (`threshold = 1.00`) for `cpu_cast` and `cpu_broadcast`.
- `targeted-drop` profile enforces tighter thresholds for both CPU fallback routes.

---

### 7.5 i32 Linalg/Reduce Route Baseline and Gate

GPUArray i32 linalg/reduce fallback tracking is captured by a dedicated route audit:

- `packages/mojor/tools/audit_gpuarray_i32_routes.R`

Audit coverage:

- matmul family: `gpu_matmul`, `%*%`, `gpu_matmul_into`, `matmul`, `gemv`, `gevm` route shapes
- crossprod family: `crossprod`
- tcrossprod family: `tcrossprod`
- reduce family: scalar/dims `sum` (`keepdims` permutations)

Committed i32-route baseline:

- `docs/BASELINES/GPUARRAY_I32_ROUTE_BASELINE.csv`

Baseline capture command:

```bash
Rscript packages/mojor/tools/audit_gpuarray_i32_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_I32_ROUTE_BASELINE.csv
```

Release closeout gate (no-growth profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_i32_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_I32_ROUTE_BASELINE.csv \
 --profile=closeout
```

Targeted-drop gate (tightening profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_i32_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_I32_ROUTE_BASELINE.csv \
 --profile=targeted-drop
```

Gate policy:

- Route labels remain unchanged: `gpu_matmul`, `cpu_matmul`, `gpu_crossprod`, `cpu_crossprod`, `gpu_tcrossprod`, `cpu_tcrossprod`, `gpu_reduce`, `cpu_reduce`.
- `closeout` profile enforces no-growth (`threshold = 1.00`) for `cpu_matmul`, `cpu_crossprod`, `cpu_tcrossprod`, and `cpu_reduce`.
- `targeted-drop` profile enforces tightening thresholds (`cpu_matmul=0.95`, `cpu_crossprod=0.90`, `cpu_tcrossprod=0.90`, `cpu_reduce=0.90`).

---

## 8) Implementation Rules

- Edit `packages/mojor/` directly for runtime/codegen/ABI behavior.
- Keep strict mode behavior explicit (no hidden fallback semantics).
- Add pair tests for newly executable behavior and diagnostics tests for rejected forms.
