# GPUArray Guide

This guide documents the new `GPUArray` class and GPU kernels in MöjoR. The API is intentionally strict and minimal in v1.

For the class-level support matrix (methods, signatures, support levels, and route labels), see [GPUARRAY_CLASS_SUPPORT.md](./GPUARRAY_CLASS_SUPPORT.md).
For canonical cross-layer GPU notation (wrappers + IR + runtime/class), see [GPU_FUNCTION_SUPPORT_MATRIX.md](./GPU_FUNCTION_SUPPORT_MATRIX.md).

## Requirements

- Mojo backend must be built and loaded.
- GPU must be available (`mojor_has_gpu()` returns TRUE).
- For `f32` kernels, the `float` package must be installed.
- `i32` GPU buffers are supported for constructors/read/write, `gpu_randi*`, reductions, and matrix products.
- f64 GPU tests are opt-in via `MOJOR_TEST_GPU_F64=1`; on Apple Metal they may skip.

## Create a GPUArray

```r
x <- runif(1024)
ax <- mojor_gpu_array(x) # class: GPUArray / mojor_gpu_array
print(ax)
```

You can control the dtype and backend:

```r
ax <- mojor_gpu_array(x, dtype = "f32", api = "metal")
```

Allocate by shape or length:

```r
z <- gpu_zeros(dim = c(2L, 3L), dtype = "f32")
e <- gpu_empty(dim = c(2L, 3L), dtype = "f32") # uninitialized
r <- gpu_rand(dim = c(2L, 3L), min = -1, max = 1, dtype = "f32")
rn <- gpu_randn(dim = c(2L, 3L), mean = 0, sd = 1, dtype = "f32")
ri <- gpu_randi(dim = c(2L, 3L), low = 0L, high = 10L, dtype = "f32") # [low, high)
ri_i32 <- gpu_randi(dim = c(2L, 3L), low = 0L, high = 10L, dtype = "i32")
mojor_gpu_array_write(e, matrix(runif(6), nrow = 2L))
```

Create like-shaped arrays from host arrays or existing `GPUArray` values:

```r
src <- mojor_gpu_array(matrix(runif(6), nrow = 2), dtype = "f32")
z <- gpu_zeros_like(src) # inherits shape/api/dtype from src
o <- gpu_ones_like(src)
f <- gpu_full_like(src, 3.5)
e <- gpu_empty_like(src) # uninitialized values; write before reading
r <- gpu_rand_like(src, min = -1, max = 1)
rn <- gpu_randn_like(src, mean = 0, sd = 1)
ri <- gpu_randi_like(src, low = 0L, high = 10L) # [low, high)
```

`gpu_randi*` emits integer-valued samples with `low` inclusive and `high` exclusive bounds.
Use `dtype = "i32"` for true integer GPU buffers.

## Basic Operations

Elementwise operations are supported for `+`, `-`, `*`, `/`.

```r
ax <- mojor_gpu_array(runif(8))
ay <- mojor_gpu_array(runif(8))

az <- ax + ay
bw <- ax * 2
cx <- 3 - ay
```

Direct unary math methods are also available on `GPUArray`:

```r
sx <- sin(ax)
tx <- tan(ax)
lx <- log(ax)
lx10 <- log10(ax)
lp <- log1p(ax)
em1 <- expm1(ax)
sg <- sign(ax)
tr <- trunc(ax)
rx <- round(ax, digits = 2L)
fx <- floor(ax)
```

Function-style binary math methods are available too:

```r
mn <- gpu_minimum(ax, ay)
mx <- gpu_maximum(ax, ay)
mn3 <- gpu_minimum(ax, ay, 0.0)
mx4 <- gpu_maximum(ax, ay, 0.0, 1.0)
ang <- gpu_atan2(ax, ay)
```

Scalar summary helpers are available directly:

```r
s <- sum(ax)
m <- mean(ax)
imin <- gpu_which_min(ax)
imax <- gpu_which_max(ax)
```

Notes:
- Mixed dtype operands auto-promote on `Ops`; use `gpu_promote()` when you need explicit target-dtype control.
- Scalar ops require a numeric scalar.
- Function-style binary math (`gpu_minimum`, `gpu_maximum`, `gpu_atan2`) is GPU-first for elementwise-compatible inputs; `gpu_minimum`/`gpu_maximum` support variadic folds.
- Binary wrapper route contract is locked:
 - success can return direct GPU output (no `gpu_fallback` attr), or CPU-tagged fallback (`cpu_kernel_host` or `cpu_arith`)
 - kernel-fail lanes (`arr_arr`, `arr_sca`, `sca_arr`) use `cpu_arith` with `gpu_fallback_reason_code = "kernel_dispatch_failed"` (including `gpu_atan2` when backend intrinsics are unavailable)
 - `gpu_minimum`/`gpu_maximum` with `na.rm = TRUE` use host parity route `cpu_arith` with `gpu_fallback_reason_code = "na_rm_host_parity"`
 - shape/recycle mismatch parity uses `cpu_arith` with `gpu_fallback_reason_code = "shape_mismatch_host_parity"`
 - scalar-host-lhs `gpu_minimum`/`gpu_maximum` (`gpu_minimum(0.5, gy)`, `gpu_maximum(0.5, gy)`) intentionally drop `dim`/`dimnames` to match base host parity
- Unary `Math`/`round` paths are GPU-first (`round` uses GPU-first for `digits = 0`), including `log10`/`log2`/`sign` via lowered kernel expressions.
- Unary fallback lanes route through `cpu_arith` with locked reason codes (`raw_kernel_unavailable`, `kernel_dispatch_failed`, `round_digits_host_fallback`).
- `Summary.GPUArray` (`sum`/`prod`/`min`/`max`) and `mean.GPUArray` return host scalars with GPU-first reducer lanes where eligible.
- `gpu_which_min` / `gpu_which_max` are the canonical index-reducer API for `GPUArray`.
- Base `which.min()` / `which.max()` S3 dispatch is not guaranteed across all R6-backed call contexts; treat those as compatibility-only lanes and use `gpu_which_*` in production code.
- Operations may fall back to host execution if GPU kernels cannot be emitted.
- Set `mojor_options(gpu_reject_fallback = TRUE)` to reject any CPU fallback route with an explicit error.
- `gpu_slice()` supports non-zero stride values (forward or reverse); non-unit strides may route through GPU gather or CPU fallback.
- Logical indexing supports strict masks:
 - single-index vector masks (`x[mask]`) with `length(mask) == length(x)` and no `NA`
 - per-axis masks for rank-N `[` and `[<-` when each mask has exact axis extent and no `NA`
 - all-false masks are rejected as empty indices
- `i32` slice/gather indexing is GPU-backed; contiguous `i32` slice-assignment now attempts a dedicated native GPU helper path before `cpu_slice_assign`.
- Non-contiguous `i32` scatter on Metal still uses a runtime capability probe and falls back to CPU scatter when unsupported (set `options(mojor.gpu.i32_scatter_on_metal = TRUE)` to force a capability re-probe).
- GPU reduction and matmul support `f32`/`f64`/`i32`.
- f64 reduce/matmul paths use runtime capability probes with mode-specific gating (`matmul`/`gemv`/`gevm` for matmul; `value`/`arg` classes for reduce); unsupported backends still short-circuit to explicit CPU fallback routes.
- On macOS Metal, f64 linalg/reduce is intentionally forced to explicit CPU fallback routes (`cpu_matmul`, `cpu_reduce`) to keep behavior deterministic while f64 GPU backend support is unavailable.

## Dtype and Broadcast Helpers

Use explicit dtype/broadcast helpers when mixing inputs:

```r
gx <- GPUArray(matrix(runif(6), nrow = 2), dtype = "f32")
gy <- GPUArray(matrix(runif(6), nrow = 2), dtype = "f64")

# Explicit cast
gx64 <- gpu_cast(gx, "f64")

# Promote pair to a common dtype
p <- gpu_promote(gx, gy)
out <- p$x + p$y

# Explicit ND broadcast (NumPy-style shape compatibility)
v <- GPUArray(c(1, 2, 3), dtype = "f32")
bv <- gpu_broadcast(v, c(2L, 3L))
```

`gpu_cast()` now attempts native GPU buffer casts first for `GPUArray` inputs and falls back explicitly when unavailable.
`gpu_promote()` uses `gpu_cast()` internally.
`gpu_broadcast()` now attempts GPU gather-based broadcast first for `GPUArray` inputs and falls back explicitly when needed.

## Performance Smoke and Regression Gate

Core GPUArray perf smoke metrics are tracked for:

- `gpu_cast`
- `gpu_broadcast`
- `gpu_reduce`
- `gpu_matmul`

Use the benchmark harness:

```bash
Rscript packages/mojor/tools/benchmark_gpuarray_core.R --runs=3 --warmup=1
```

Refresh baseline (commit after review):

```bash
Rscript packages/mojor/tools/benchmark_gpuarray_core.R \
 --runs=5 --warmup=2 --n=1048576 --m=512 --k=512 --p=512 \
 --write-baseline=docs/BASELINES/GPUARRAY_PERF_BASELINE.csv
```

Run gate against committed baseline:

```bash
Rscript packages/mojor/tools/benchmark_gpuarray_core.R \
 --runs=3 --warmup=1 --n=1048576 --m=512 --k=512 --p=512 \
 --baseline=docs/BASELINES/GPUARRAY_PERF_BASELINE.csv
```

Mirror harness path:

- `packages/mojor/tools/benchmark_gpuarray_core.R`

## Cast/Broadcast Route Audit and Gate

Cast/broadcast fallback tracking uses a dedicated route-audit harness:

```bash
Rscript packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R --repeats=8
```

This audit includes:
- cast matrix coverage (`f32->f64`, `f32->i32`, `f64->f32`, `f64->i32`, `i32->f32`, `i32->f64`, same-dtype cast clone)
- broadcast coverage (rank-1/2/3 and shape-stable broadcast)

Write/update baseline:

```bash
Rscript packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_CAST_BROADCAST_ROUTE_BASELINE.csv
```

Release closeout no-growth gate:

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

Gate labels and checks:
- `cpu_cast` (no-growth in `closeout`; targeted-drop in `targeted-drop`)
- `cpu_broadcast` (no-growth in `closeout`; targeted-drop in `targeted-drop`)

Mirror harness path:
- `packages/mojor/tools/audit_gpuarray_cast_broadcast_routes.R`

## Math Route Audit and Gate

Unary/binary math fallback tracking uses a dedicated route-audit harness:

```bash
Rscript packages/mojor/tools/audit_gpuarray_math_routes.R --repeats=8
```

This audit includes:
- unary `Math`/`round` direct methods
- binary wrappers (`gpu_minimum`, `gpu_maximum`, `gpu_atan2`) including `na.rm`/shape-parity fallback lanes

Write/update baseline:

```bash
Rscript packages/mojor/tools/audit_gpuarray_math_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv \
 --write-reason-baseline=docs/BASELINES/GPUARRAY_MATH_REASON_BASELINE.csv
```

Release closeout no-growth gate:

```bash
Rscript packages/mojor/tools/audit_gpuarray_math_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv \
 --reason-baseline=docs/BASELINES/GPUARRAY_MATH_REASON_BASELINE.csv \
 --profile=closeout
```

Targeted-drop gate (tightening profile):

```bash
Rscript packages/mojor/tools/audit_gpuarray_math_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv \
 --reason-baseline=docs/BASELINES/GPUARRAY_MATH_REASON_BASELINE.csv \
 --profile=targeted-drop
```

Gate labels and checks:
- Baseline CSV now records `op,route,count` rows:
 - `op="__all__"` rows are aggregate counts used for gating
 - per-op rows provide route counters per lane (`sin`, `log10`, `gpu_minimum`, etc.)
- Reason baseline CSV records `op,route,reason,count` rows for deterministic reason-code lanes.
- `cpu_arith` (`closeout`: no-growth; `targeted-drop`: `<= floor(baseline * 0.85)`)
- `cpu_kernel_host` (`closeout`: no-growth; `targeted-drop`: `<= floor(baseline * 0.85)`)
- Nightly smoke harness (leak loop + math gate):
```bash
Rscript packages/mojor/tools/gpuarray_math_nightly_smoke.R \
 --repeats=8 \
 --leak-iters=24 \
 --baseline=docs/BASELINES/GPUARRAY_MATH_ROUTE_BASELINE.csv \
 --profile=closeout
```

Mirror harness path:
- `packages/mojor/tools/audit_gpuarray_math_routes.R`

### Cache Memoization Diagnostics

GPU capability/kernel memoization now exposes internal diagnostic snapshots:

- `.mojor_gpu_capability_cache_diag(include_entries = TRUE, reset = FALSE)`
- `.mojor_gpu_kernel_cache_diag(include_entries = TRUE, reset = FALSE)`
- `.mojor_gpu_cache_diag(include_entries = TRUE, reset = FALSE)`

Each helper returns `stats` counters (`lookups`, `hits`, `misses`, `negative_hits`, `stores`) and optional cache-entry tables.
Use `reset = TRUE` to reset counters after collecting a snapshot.

### GPU Context Stability Lane

For repeated full-file GPU context stability checks with an explicit reset policy:

```bash
for f in packages/mojor/tests/testthat/test_gpu_array_[0-9][0-9].R; do
 Rscript scripts/check_gpuarray_context_stability.R \
  --file="$f" \
  --repeats=2 \
  --reset_mode=soft \
  --file_reset_mode=soft \
  --per_test_reset=false \
  --disable_r_jit=true \
  --stress_opt_level=0
done
```

## Index Route Audit and Gate

Indexing fallback burn-down uses a dedicated route audit harness:

```bash
Rscript packages/mojor/tools/audit_gpuarray_index_routes.R --repeats=8
```

Write/update baseline:

```bash
Rscript packages/mojor/tools/audit_gpuarray_index_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_INDEX_ROUTE_BASELINE.csv
```

Run targeted drop gate against the committed baseline:

```bash
Rscript packages/mojor/tools/audit_gpuarray_index_routes.R \
 --repeats=8 \
 --baseline=docs/BASELINES/GPUARRAY_INDEX_ROUTE_BASELINE.csv
```

Gate checks:
- `cpu_gather <= floor(baseline * 0.75)`
- `cpu_scatter <= floor(baseline * 0.85)`
- `cpu_slice <= floor(baseline * 1.00)` (no growth)
- `cpu_slice_assign <= floor(baseline * 0.50)`

## f64 Linalg/Reduce Route Audit and Gate

f64 linalg/reduce fallback tracking uses a dedicated route-audit harness:

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R --repeats=8
```

This audit includes:
- matmul mode coverage (`matmul`, `gemv`, `gevm`)
- reduce mode coverage (scalar/dims `sum`, scalar/dims `argmax`)

Write/update baseline:

```bash
Rscript packages/mojor/tools/audit_gpuarray_f64_routes.R \
 --repeats=8 \
 --write-baseline=docs/BASELINES/GPUARRAY_F64_ROUTE_BASELINE.csv
```

Release closeout no-growth gate:

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

Gate labels and checks:
- `cpu_matmul` (no-growth in `closeout`; targeted-drop in `targeted-drop`)
- `cpu_reduce` (no-growth in `closeout`; targeted-drop in `targeted-drop`)

Mirror harness path:
- `packages/mojor/tools/audit_gpuarray_f64_routes.R`

## Linear Algebra

Matrix multiplication and cross products attempt GPU execution first (including vector `%*%`, `crossprod`, and `tcrossprod` paths routed through matmul views) and fall back to host execution when needed.

```r
A <- mojor_gpu_array(matrix(runif(6), nrow = 2))
B <- mojor_gpu_array(matrix(runif(12), nrow = 3))
C <- A %*% B

CP <- crossprod(A)
TCP <- tcrossprod(A)
```

The output includes `attr(out, "gpu_fallback")` set to the fallback reason.

On macOS with Metal, f64 linalg paths are expected to report explicit CPU routes (`cpu_matmul`, `cpu_crossprod`, `cpu_tcrossprod`).

## Fallback Contract

Key `gpu_fallback` values are standardized:

- reductions: `gpu_reduce`, `cpu_reduce`
- matmul: `gpu_matmul`, `cpu_matmul`
- cross products: `gpu_crossprod`, `cpu_crossprod`, `gpu_tcrossprod`, `cpu_tcrossprod`
- math wrappers and `Ops`: `cpu_arith`, `cpu_kernel_host`, `cpu_compare`, `cpu_logic`
- slicing/indexing: `gpu_slice`, `cpu_slice`, `gpu_gather`, `cpu_gather`, `gpu_scatter`, `cpu_scatter`, `cpu_slice_assign`
- dtype/broadcast helpers: `gpu_cast`, `cpu_cast`, `host_cast`, `gpu_broadcast`, `cpu_broadcast`, `host_broadcast`

For reason-coded fallback lanes, these `gpu_fallback_reason_code` values are locked in the current contract:

- unary math: `raw_kernel_unavailable`, `kernel_dispatch_failed`, `round_digits_host_fallback`
- binary math: `kernel_dispatch_failed`, `na_rm_host_parity`, `shape_mismatch_host_parity`

Set `mojor_options(gpu_reject_fallback = TRUE)` to make any `cpu_*` route fail fast instead of downgrading.

## Writing GPU Kernels

Use `mojor_gpu_kernel()` to compile elementwise GPU kernels.

```r
f <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2 + y[i]
 }
 out
}

k <- mojor_gpu_kernel(
 f,
 x = "f32[]",
 y = "f32[]",
 name = "gpu_axpy"
)

ax <- mojor_gpu_array(runif(256), dtype = "f32")
ay <- mojor_gpu_array(runif(256), dtype = "f32")

az <- k(ax, ay)
```

Constraints:
- Elementwise kernels only.
- Supported output annotations are `f32[]`/`f64[]` and matrix outputs `f32[,]`/`f64[,]`.
- Supported kernel loop shapes in this release:
 - 1D linearized form (`seq_along(x)` with `out[i] <- ...`).
- Canonical 2D indexed form (`seq_len(nrow(X))` + `seq_len(ncol(X))` with `out[i, j] <- ...`).
- Inner body supports direct assignment and guarded `if`/`if-else` assignment forms.
- Matrix reads support canonical `a[i, j]` plus guarded neighbor reads
  `a[i + di, j + dj]` where `di`/`dj` are integer literals, compile-time
  constants, or runtime scalar integer offsets.
- Neighbor reads require explicit in-bounds guards in the same `if` condition.
  Supported guard shapes include canonical `&&` bounds checks, negated
  single-atom bounds, and de-morgan negation-of-disjunction bounds forms.
- Matrix inputs must remain same-shape at runtime in matrix2d mode.
- Unsupported forms stay explicit: reductions in the kernel body, irregular
  indexing, mixed dtype arithmetic without explicit cast, neighbor guards
  lacking explicit in-bounds coverage, and broader non-canonical 2D
  loop/index/guard patterns.

Linearized matrix example:

```r
f2 <- function(x, y) {
 out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i]
 }
 out
}

k2 <- mojor_gpu_kernel(
 f2,
 x = "f64[,]",
 y = "f64[,]",
 name = "gpu_mat_add_linearized"
)
```

Canonical 2D indexed matrix example:

```r
f3 <- function(x, y, b) {
 out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
 for (i in seq_len(nrow(x))) {
 for (j in seq_len(ncol(x))) {
 out[i, j] <- x[i, j] + y[i, j] + b
 }
 }
 out
}

k3 <- mojor_gpu_kernel(
 f3,
 x = "f32[,]",
 y = "f32[,]",
 b = "f32",
 name = "gpu_mat_add_indexed"
)
```

Guarded neighbor example:

```r
f4 <- function(x, b) {
 out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
 for (i in seq_len(nrow(x))) {
 for (j in seq_len(ncol(x))) {
 out[i, j] <- x[i, j]
 if (i > 1 && i < nrow(x) && j > 1 && j < ncol(x)) {
 out[i, j] <- x[i - 1, j + 1] + b
 }
 }
 }
 out
}
```

## `mojor_guvectorize` Target, Core-Dims, and Output Contract

`mojor_guvectorize()` now supports `target = c("cpu", "gpu")` (default `cpu`).
When `target = "gpu"`, it forces the GPU elementwise compilation lane.

`core_dims` supports subset-v7 grammar in this release:
- one or more input tuples
- one or more output tuples
- core rank is unbounded (rank-N)
- examples: `"(n)->(n)"`, `"(m,n),(n,p)->(m,p)"`, `"(a,b,c)->(a,b,c)"`, `"(n),(n)->(n),(n)"`, `"(n,3)->(n,3)"`
- core-dimension tokens may be symbolic names (`n`, `_m`, `axis2`) or fixed positive integer literals (`1`, `2`, `3`, ...).
- signature rank forms accepted in `mojor_guvectorize()`:
 - comma-bracket ranks: `[]`, `[,]`, `[,,]`, ...
 - dimensional-tag ranks: `[1d]`, `[2d]`, `[3d]`, ...

Multi-output core-dims wrappers still require terminal `list(...)` or
`return(list(...))` in the gufunc body and currently require `load = TRUE`.

For rank-N cores, MöjoR now attempts a strict indexed lane first when the gufunc
body is strict-expressible:
- single-output: terminal expression or `return(expr)`
- multi-output: terminal `list(...)`/`return(list(...))`
- allowed expression nodes: argument refs, scalar literals, `+ - * /`, unary `-`,
  explicit casts (`as.double`, `as.integer`, `as.logical`), canonical indexed reads
  (`x[i1, i2, ...]`), and guarded `if` / `if-else` forms
- neighbor reads (`x[i1 + k, ...]`) require explicit canonical in-bounds guards using
  `&&` conjunction-only bounds checks
- disallowed in strict indexed lane: non-literal neighbor offsets, `||`/`!`
  guards, and opaque helper predicates

Rank-N reduction subset (non-elementwise core outputs):
- recognized ops: `sum`, `mean`, `min`, `max`
- reduced axes are inferred from output tuple symbols (missing input core symbols)
- mixed multi-output reduced + non-reduced tuples are supported when satisfiable
- unsupported ops and non-strict reduction expressions fail with explicit diagnostics

Rank-N compile policy:
- `target = "gpu"` remains strict; failures are explicit hard errors (no CPU downgrade).
- `target = "cpu"` uses fallback only when strict mode allows it:
 - if `ir_only = FALSE`, strict failures can retry with `object_mode = "fallback"`.
 - if `ir_only = TRUE`, fallback is blocked and an explicit error is raised.

Returned wrappers/builds also expose additive rank-N diagnostics metadata:
- `mojor_core_dims_engine`: `"strict_elementwise"` or `"direct_core"`
- `mojor_core_dims_strict_error`: last strict compile error (if any)
- `mojor_core_dims_batch_engine`: `"compiled_batch"` or `"r_loop"`
- `mojor_core_dims_batch_cache_hit`: last-known compiled batch cache-hit flag
  using deterministic cache keys over canonicalized signature/core-dims metadata

If `output_shape` is provided, loaded wrappers enforce it at runtime:

```r
g <- mojor_guvectorize(
 f,
 signature = list(x = "f64[]", n = "i32"),
 target = "cpu",
 core_dims = "(n),()->(n)",
 output_shape = c(1L)
)
```

For multi-output wrappers, pass a list of per-output shapes:

```r
g2 <- mojor_guvectorize(
 function(x, y) list(sum = x + y, diff = x - y),
 signature = list(x = "f64[]", y = "f64[]"),
 target = "gpu",
 core_dims = "(n),(n)->(n),(n)",
 output_shape = list(c(3L), c(3L))
)
```

`output_shape` mismatches now raise an explicit `mojor_guvectorize: output_shape mismatch` error.

## Reading Back to Host

```r
host <- mojor_gpu_array_read(ax)
```

## Freeing GPU Memory

```r
mojor_gpu_array_free(ax)
```

## Optional f64 GPU Test Run

Enable f64 GPU coverage in the split GPUArray suite (`test_gpu_array_*.R`) explicitly:

```bash
MOJOR_TEST_GPU_F64=1 \
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
```

This is intended for backends with f64 GPU buffer support (for example, CUDA builds).
