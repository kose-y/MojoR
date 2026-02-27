# GPUArray Class Support Reference

This document is the class-level reference for `GPUArray` in MöjoR: what methods exist, what is GPU-native vs CPU-routed, and which gaps are still intentional.

For the canonical cross-layer GPU support notation (wrapper + IR + runtime/class),
see [`./GPU_FUNCTION_SUPPORT_MATRIX.md`](./GPU_FUNCTION_SUPPORT_MATRIX.md).

Scope:
- Runtime/class surface in `core/gpu.R` (`GPUArray`, `mojor_gpu_array`, S3 methods, and helper APIs).
- Route transparency (`attr(x, "gpu_fallback")` and reason attributes).

Out of scope:
- IR node construction rules (see IR docs).
- Backend kernel internals (see design/backend docs).

## Plain-Language Model

`GPUArray` is a GPU-backed R6 reference object with S3 methods.

- Primary constructors return class `GPUArray` (which also behaves as `mojor_gpu_array`).
- Data is stored in backend handles; shape metadata (`dim`, `dimnames`, `strides`) is tracked on the R side.
- Most compute/index APIs try a GPU path first, then explicitly route to CPU when needed.
- Route transparency is part of the contract: results carry `gpu_fallback`/`gpu_route` labels.
- Because this is R6-backed, object identity matters (`x2 <- x` aliases the same underlying object).

## Support Legend

| Level | Meaning |
|---|---|
| `S4` | Implemented and normally GPU-routed |
| `S3` | Implemented with explicit GPU/CPU route gating |
| `S2` | Implemented but host-seeded or partial GPU behavior |
| `S1` | Not implemented for `GPUArray` |

## 1) API Coverage Matrix

### 1.1 Constructors and allocation

| API | Signature | Level | Notes |
|---|---|---|---|
| `mojor_gpu_array` | `(x = NULL, n = NULL, api = c("auto","metal","cuda","amd"), dtype = c("auto","f32","f64","i32"))` | `S4` | Canonical constructor. |
| `GPUArray` | same as `mojor_gpu_array` | `S4` | Class-first alias. |
| `gpu_array` | same as `mojor_gpu_array` | `S4` | Short alias. |
| `as.GPUArray` | `(x, ...)` | `S4` | No-op for existing `GPUArray`; uploads host value otherwise. |
| `gpu_zeros`, `gpu_ones`, `gpu_full`, `gpu_empty` | shape allocators | `S4` | `dim` or `n`; positive extent checks. |
| `gpu_zeros_like`, `gpu_ones_like`, `gpu_full_like`, `gpu_empty_like` | like allocators | `S4` | Inherit shape/api/dtype when `auto`. |
| `gpu_rand`, `gpu_randn`, `gpu_randi` | RNG allocators | `S2` | Host-generated samples uploaded to GPU buffer. |
| `gpu_rand_like`, `gpu_randn_like`, `gpu_randi_like` | like RNG allocators | `S2` | Host-generated, shape-aware. |

### 1.2 Data movement and lifecycle

| API | Level | Notes |
|---|---|---|
| `mojor_gpu_array_read` | `S4` | Reads device data to host with shape restoration. |
| `mojor_gpu_array_write` | `S4` | Writes host data into existing GPU buffer. |
| `mojor_gpu_array_free` | `S4` | Frees handle, clears shape/dtype metadata, keeps object shell. |
| `mojor_gpu_check_release` | `S3` | Leak-check helper for repeated call paths. |

### 1.3 Class methods and operators

| API | Level | Notes |
|---|---|---|
| `print.GPUArray` | `S4` | Prints length, backend, dtype, dims. |
| `length.GPUArray`, `dim.GPUArray`, `dimnames.GPUArray` | `S4` | Reads object metadata. |
| `as.array.GPUArray`, `as.matrix.GPUArray`, `as.numeric.GPUArray` | `S4` | Materializes host values via read path. |
| `Ops.GPUArray` | `S3` | Supports arithmetic (`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, unary `+`/`-`), comparisons (`<`, `>`, `<=`, `>=`, `==`, `!=`), and logical operators (`!`, `&`, `|`); mixed-dtype operands auto-promote. |
| `gpu_atan2`, `gpu_minimum`, `gpu_maximum` | `S3` | Function-style binary math routes GPU-first for elementwise-compatible lanes; `gpu_minimum`/`gpu_maximum` support variadic folds; explicit host fallback uses `cpu_arith`/`cpu_kernel_host` tags with reason codes. |
| `Math.GPUArray` (`sin`, `cos`, `tan`, `exp`, `log`, `log10`, `log2`, `log1p`, `expm1`, `sqrt`, `abs`, `sign`, `trunc`, `floor`, `ceiling`) | `S3` | Unary math routes GPU-first through elementwise kernels; known unsupported intrinsics (`log10`, `log2`, `sign`) short-circuit to host parity with explicit reason codes; host fallback routes may tag `cpu_arith` or kernel-wrapper `cpu_kernel_host`. |
| `round.GPUArray` | `S3` | Direct rounding method with scalar `digits`; GPU-first for `digits = 0`, host-parity fallback for other `digits` values. |
| `Summary.GPUArray`, `mean.GPUArray`, `gpu_which_min`, `gpu_which_max` | `S3` | Scalar summary methods are exposed directly on `GPUArray`; direct single-array lanes use `gpu_*` reducers when eligible and broader forms route through host parity. `gpu_which_*` wrappers call the `GPUArray` arg-reducer path and return host indices. |
| `%*%.GPUArray` | `S3` | GPU-first matmul dispatch with CPU fallback route; mixed GPUArray dtypes auto-promote. |
| `crossprod.GPUArray`, `tcrossprod.GPUArray` | `S3` | GPU-first when shape/device checks pass; explicit CPU routes otherwise; mixed GPUArray dtypes auto-promote. |
| `[.GPUArray` | `S3` | Slice/gather path with canonical GPU-first selectors and host-parity fallback on broader/non-canonical forms. |
| `[<-.GPUArray` | `S3` | Scatter/slice-assign path with canonical GPU-first selectors and host-parity fallback on broader/non-canonical forms. |
| `[[.GPUArray`, `[[<-.GPUArray`, `$.GPUArray`, `$<-.GPUArray` families | `S3` | `[[` supports single-element extraction including base-style scalar linear indexing on rank>1 arrays and tuple-style multi-axis selectors; `[[<-` enforces scalar-target replacement and supports rank>1 scalar linear assignment parity; `$` supports metadata fields plus unique dimname-axis extraction (unknown/ambiguous keys return `NULL`); `$<-` supports metadata writes and unique dimname-axis replacement. |

### 1.4 Compute helpers

| API | Level | Notes |
|---|---|---|
| `gpu_cast` | `S3` | GPU cast when available, else `cpu_cast`/`host_cast` route. |
| `gpu_promote` | `S3` | Returns `{x, y, dtype}` promoted pair (`mojor_gpu_promoted`). |
| `gpu_broadcast` | `S3` | GPU gather-based broadcast when available, else CPU/host route. |
| `gpu_slice` | `S3` | Non-zero stride slice helper (forward or reverse); can route through gather/cpu slice. |
| `gpu_reduce`, `gpu_sum`, `gpu_mean`, `gpu_prod`, `gpu_min`, `gpu_max`, `gpu_pmin`, `gpu_pmax`, `gpu_argmin`, `gpu_argmax` | `S3` | `op` set is fixed; `gpu_pmin/gpu_pmax` are reduction aliases for min/max wrappers; dims and dtype route checks enforced. |
| `gpu_matmul`, `gpu_matmul_into` | `S3` | 2D kernel interface with strict shape/dtype/output checks. |

### 1.5 Kernel/session and legacy helper surface

| API | Level | Notes |
|---|---|---|
| `mojor_gpu_kernel`, `gpu_kernel` | `S3` | Builds elementwise GPU kernel wrapper from R function. |
| `mojor_gpu_session`, `mojor_gpu_session_run`, `mojor_gpu_session_sum`, `mojor_gpu_session_free` | `S2` | Session helper surface (`mojor_gpu_session` class), not `GPUArray` methods. |
| `mojor_gpu_sigmoid`, `mojor_gpu_affine`, `mojor_gpu_linear`, `mojor_gpu_chain_array`, `mojor_gpu_sum` | `S2` | Legacy helper APIs in same module; mixed GPUArray/host paths. |

## 2) Copy-On-Modify and Object Identity

`GPUArray` is reference-semantics R6 data, not copy-on-modify value semantics.

1. Aliasing is real:
 - `x2 <- x` points to the same object/handle.
2. In-place mutation APIs mutate the object:
 - `[<-.GPUArray`
 - `mojor_gpu_array_write`
 - `mojor_gpu_array_free`
3. Most arithmetic/reduction/matmul calls produce new outputs, but assignment-style APIs modify existing objects.
4. `as.GPUArray(x)` returns `x` unchanged when `x` is already `GPUArray`.

## 3) Indexing and Assignment Contract

`[.GPUArray` and `[<-.GPUArray` use GPU-first index routes for canonical selector
forms and host-parity fallback for broader/non-canonical selector classes.

1. Numeric indices:
 - Canonical GPU-first lanes use finite integer selectors.
 - Positive indices must be in bounds.
 - Negative indices are treated as exclusions; only zeros may mix with negatives.
 - Zeros are allowed and ignored (empty selectors are valid).
 - Empty indices are allowed and route through empty `gpu_gather` results.
 - Empty-selection host-parity fallback lanes are wrapped as empty `GPUArray` results (`cpu_gather`).
 - Broader forms (for example factor/list selectors, `NA`-containing numeric selectors, and ND `NULL` selector shapes) use explicit host-parity fallback.
2. Arity compatibility:
 - Rank-1 `[`/`[<-` accepts canonical secondary selectors (`j = 1`/`TRUE`).
 - Trailing extra indices are accepted when canonical no-op selectors (`missing`, `1`, `TRUE`) are used.
 - Non-canonical secondary/extra selector forms route through host parity when base-compatible.
3. Logical masks:
 - Must be atomic logical vectors.
 - Must not contain `NA`.
 - Must match axis extent exactly.
 - All-false masks are allowed and produce empty-result lanes.
4. `drop` behavior:
 - `drop = TRUE` may return host scalar numeric when all indexed axes collapse.
 - `drop = FALSE` preserves array shape (including empty-result lanes).
5. Slice constraints:
 - `gpu_slice()` requires non-zero strides with direction-consistent bounds.
6. Assignment route behavior:
 - Contiguous index patterns may use native slice/scatter paths.
 - Non-contiguous or unsupported cases degrade to CPU assignment routes (`cpu_scatter` / `cpu_slice_assign`).
 - Non-unit/irregular `gpu_slice()` paths attempt GPU gather routing first (including plan-cache-miss lanes) before CPU downgrade.
 - Assignment values support scalar, exact-length, and exact-multiple recycling; non-multiple recycling fails explicitly.
 - GPUArray replacement values in assignment families (`[<-`, `[[<-`, `$<-`) are dtype-aligned to the target before assignment.

## 4) DType and Backend Behavior

### DTypes

| DType | Status | Notes |
|---|---|---|
| `f32` | `S4` | Main fast path for GPU compute. |
| `f64` | `S3` | Capability-gated. On macOS + Metal, linalg/reduce routes are intentionally CPU-routed. |
| `i32` | `S3` | Constructors/read/write/linalg/reduce supported; some scatter routes are capability-gated. |

### Backend/API

1. API options: `"auto"`, `"metal"`, `"cuda"`, `"amd"`.
2. `"auto"` currently resolves to `"metal"` unless overridden.
3. `options(mojor_gpu_api=...)` and `MOJOR_GPU_API` control API override (`option` takes precedence).
4. GPU availability and backend load are hard requirements for device allocation paths.

## 5) Configuration and Capability Probes

### Runtime knobs

| Knob | Type | Purpose |
|---|---|---|
| `options(mojor_gpu_api=...)` | R option | API override (`auto`/`metal`/`cuda`/`amd`). |
| `MOJOR_GPU_API` | env var | API override fallback when option is not set. |
| `mojor_options(gpu_reject_fallback = TRUE)` | package option | Reject CPU fallback routes (`cpu_*`) with an explicit error instead of downgrading. |
| `options(mojor.gpu.i32_scatter_on_metal = TRUE)` | R option | Force i32 scatter capability probe refresh on Metal path. |
| `MOJOR_GPU_F64_PROBE_MODE` | env var | `inprocess` or `subprocess` f64 capability probe mode. |
| `MOJOR_GPU_F64_PROBE_TIMEOUT_SEC` | env var | Subprocess probe timeout (seconds). |
| `MOJOR_GPU_F64_PROBE_MAX_SHAPES` | env var | Cap probe shape breadth (default 1). |
| `MOJOR_TEST_GPU_F64=1` | env var | Test-lane opt-in for f64 GPU tests. |

## 6) Route Transparency Contract

All major GPUArray compute/index paths set route tags.

1. Main route attribute: `attr(out, "gpu_route")`
2. Compatibility alias: `attr(out, "gpu_fallback")`
3. Optional diagnostics:
 - `gpu_route_reason` / `gpu_fallback_reason`
 - `gpu_route_reason_code` / `gpu_fallback_reason_code`
4. Strict rejection policy:
 - when `mojor_options(gpu_reject_fallback = TRUE)`, any `cpu_*` route raises an error.

Common route labels:

- Cast/broadcast: `gpu_cast`, `cpu_cast`, `host_cast`, `gpu_broadcast`, `cpu_broadcast`, `host_broadcast`
- Indexing: `gpu_slice`, `cpu_slice`, `gpu_gather`, `cpu_gather`, `gpu_scatter`, `cpu_scatter`, `cpu_slice_assign`
- Reductions/linalg: `gpu_reduce`, `cpu_reduce`, `gpu_matmul`, `cpu_matmul`, `gpu_crossprod`, `cpu_crossprod`, `gpu_tcrossprod`, `cpu_tcrossprod`

### Reason code catalog

Most frequent/stable reason codes:

- `disabled_on_metal_backend`
- `dispatch_failed`
- `probe_unavailable`
- `probe_validation_failed`
- `probe_kernel_call_failed`
- `probe_context_missing`
- `subprocess_script_missing`
- `subprocess_rscript_missing`
- `subprocess_failed`
- `invalid_result_shape`
- `capability_precheck_call_failed`
- `capability_precheck_unavailable`
- `probe_signature_mismatch`
- `probe_unexpected_result`
- `probe_error`
- `kernel_dispatch_failed`
- `unsupported_gpu_intrinsic`
- `shape_mismatch_host_parity`
- `na_rm_host_parity`
- `invalid_round_digits`
- `round_digits_host_fallback`

Notes:

1. Some CPU fallbacks set reason text without a reason code (for example kernel call failed cases), so `gpu_fallback_reason_code` may be `NULL`.
2. For route audits, inspect route first, then reason text/code.

## 7) Lifecycle and Error Contract

### Lifecycle

1. `mojor_gpu_array_free()` clears handle and metadata (`n=0`, dtype/dim/dimnames/strides `NULL`).
2. After free, object shell still exists but no device handle remains.

### Canonical runtime error families

| API family | Typical errors |
|---|---|
| Read/write/free | `buf must be mojor_gpu_array`, `missing handle`, `length mismatch` |
| Arithmetic | `unsupported operand types`, `unsupported operator` |
| Matmul | `x must be 2D`, `y must be 2D`, `inner dimensions must match`, `output dtype mismatch` |
| Reduce | `unsupported op`, `invalid dims`, `missing handle` |
| Indexing | `logical mask must not contain NA`, `logical mask length must equal extent`, out-of-bounds/index type errors, replacement-length recycling errors |

## 8) Important Constraints and Known Gaps

1. `Ops` surface is still subset-limited, but now includes arithmetic (`+ - * / ^ %% %/%`, unary `+/-`), comparisons (`< > <= >= == !=`), and logical operators (`! & |`).
2. Direct unary `Math`/`round` methods are implemented for `GPUArray`; unsupported `Math` generics outside this set still fail explicitly.
3. `[[`/`$` families are still narrower than full base R (`[[`/`[[<-` require scalar-target selection; `$`/`$<-` support metadata fields and unique dimname-axis routes only).
4. Mixed-dtype auto-promotion applies to `Ops` and core linalg (`%*%`, `crossprod`, `tcrossprod`, `gpu_matmul`). Assignment families (`[<-`, `[[<-`, `$<-`), `mojor_gpu_array_write()`, and compiled GPU elementwise raw-wrapper lanes (`gpu_func_raw`) auto-align GPUArray input/replacement dtypes to the target/kernel dtype.
5. Matmul kernels require matrix-compatible shape views; kernel entrypoint checks are strict.
6. `gpu_reduce` supports only `sum/min/max/mean/argmin/argmax`; `Summary.GPUArray` currently exposes `sum/prod/min/max` (with host parity for broader forms).
7. On Metal, f64 linalg/reduce is expected to route to CPU labels.
8. Integer scatter uses capability checks and may downgrade to CPU routes.
9. RNG constructors are host-seeded then uploaded (not device-native RNG stream kernels).

## 9) Minimal Sanity Checks

```r
# 1) Constructor + route transparency
x <- GPUArray(matrix(runif(6), nrow = 2), dtype = "f32")
y <- GPUArray(matrix(runif(6), nrow = 3), dtype = "f32")
z <- x %*% y
attr(z, "gpu_fallback")
attr(z, "gpu_fallback_reason")
attr(z, "gpu_fallback_reason_code")
```

```r
# 2) Copy/alias behavior (reference semantics)
x <- GPUArray(1:4, dtype = "i32")
x_alias <- x
x_alias[1] <- 99
as.numeric(x)[1]  # reflects mutation through alias
```

```r
# 3) Cast + broadcast route checks
c1 <- gpu_cast(x, "f64")
b1 <- gpu_broadcast(c1, c(2L, 4L))
attr(c1, "gpu_fallback")
attr(b1, "gpu_fallback")
```

```r
# 4) Reduction coverage
gpu_sum(x); gpu_mean(x); gpu_min(x); gpu_max(x); gpu_argmin(x); gpu_argmax(x)
```

```r
# 5) Indexing rules and route check
s <- x[1:2]
attr(s, "gpu_fallback")
```

```r
# 6) f64 on Metal: expected CPU routes
gx <- GPUArray(matrix(as.double(1:4), nrow = 2), dtype = "f64", api = "metal")
gy <- GPUArray(matrix(as.double(1:4), nrow = 2), dtype = "f64", api = "metal")
mm <- gx %*% gy
rr <- gpu_sum(gx)
attr(mm, "gpu_fallback")
attr(rr, "gpu_fallback")
```

```r
# 7) Lifecycle: free then read/write should fail
t <- GPUArray(1:4, dtype = "f32")
mojor_gpu_array_free(t)
try(mojor_gpu_array_read(t))
try(mojor_gpu_array_write(t, c(1, 2, 3, 4)))
```

## Source of Truth

Runtime implementation:
- `packages/mojor/R/core/gpu.R`

Related docs:
- `docs/GPUARRAY_GUIDE.md`
- `docs/DESIGN/GPU_ARRAY.md`
