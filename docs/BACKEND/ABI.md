# MöjoR Backend ABI (v0)

The IR-first pipeline is now the standard path between R AST and emitted Mojo
(see `../PIPELINE/README.md` and `../IR/PLAN.md`).

These are the C-level functions the backend must provide.
The R bridge links to these symbols directly. The implementation is
provided by Mojo-generated shared libraries that export the same ABI.

## Plain-Language Mental Model

ABI is the "binary handshake" between R and compiled backend code.

- R bridge calls exported C symbols.
- Signatures, argument types, and ownership rules must match exactly.
- If the handshake is wrong, code may compile but fail or misbehave at runtime.

Treat ABI as a strict contract, not a guideline.

## Where ABI Sits in the Call Path

`R call -> bridge wrapper (.Call) -> ABI symbol (C/Mojo export) -> native compute -> bridge result normalization -> R`

Ownership split:
- R/bridge side allocates and validates inputs/outputs.
- Backend side computes using provided pointers and declared lengths.
- In-place APIs are only legal when bridge has already enforced mutability safety.

## Core Contract Rules (v0)

1. Input pointers are treated as immutable unless API explicitly says in-place.
2. Output buffers are pre-allocated by caller and must be fully written by callee.
3. Length/dtype/layout expectations are fixed by function signature.
4. NA policy for these reduction paths is bridge-layer enforced; backend receives NA-blind lane assumptions.
5. Existing symbol signatures are ABI-stable and cannot be changed in place.

## Mini Example: Read-Only Reduction

For:

```c
double mojor_sum_f64(const double* x, int n);
```

Contract:
- Caller passes contiguous `x` with `n` elements.
- Callee must not mutate `x`.
- Return value is scalar sum as `double`.

## Mini Example: Output-Buffer Kernel

For:

```c
void mojor_scale_f64(const double* x, double* out, int n, double scalar);
```

Contract:
- `out` is allocated by caller with at least `n` elements.
- Callee writes scaled values into `out`.
- Callee must not reallocate or assume ownership of `out`.

```c
// Sum a numeric vector
// Returns the sum as a double
// x: pointer to contiguous double array
// n: number of elements
// Must not mutate x

double mojor_sum_f64(const double* x, int n);
double mojor_sum_f64_std(const double* x, int n);
double mojor_sum_f64_nomiss(const double* x, int n);
double mojor_sum_f64_nomiss_manual(const double* x, int n);
double mojor_prod_f64(const double* x, int n);
double mojor_prod_f64_nomiss(const double* x, int n);
int mojor_which_min_f64_nomiss(const double* x, int n);
int mojor_which_max_f64_nomiss(const double* x, int n);
double mojor_min_f64(const double* x, int n);
double mojor_min_f64_nomiss(const double* x, int n);
double mojor_max_f64(const double* x, int n);
double mojor_max_f64_nomiss(const double* x, int n);
double mojor_mean_f64(const double* x, int n);
double mojor_mean_f64_nomiss(const double* x, int n);
float mojor_sum_f32(const float* x, int n);
float mojor_sum_f32_pairwise(const float* x, int n);
float mojor_sum_f32_nomiss(const float* x, int n);
float mojor_prod_f32(const float* x, int n);
float mojor_prod_f32_nomiss(const float* x, int n);
int mojor_which_min_f32_nomiss(const float* x, int n);
int mojor_which_max_f32_nomiss(const float* x, int n);
float mojor_min_f32(const float* x, int n);
float mojor_min_f32_nomiss(const float* x, int n);
float mojor_max_f32(const float* x, int n);
float mojor_max_f32_nomiss(const float* x, int n);
float mojor_mean_f32(const float* x, int n);
float mojor_mean_f32_nomiss(const float* x, int n);
double mojor_sd_f64_nomiss(const double* x, int n);
float mojor_sd_f32_nomiss(const float* x, int n);
double mojor_var_f64_nomiss(const double* x, int n);
float mojor_var_f32_nomiss(const float* x, int n);
double mojor_var_f64_nomiss_twopass(const double* x, int n);
float mojor_var_f32_nomiss_twopass(const float* x, int n);
double mojor_sd_f64_nomiss_twopass(const double* x, int n);
float mojor_sd_f32_nomiss_twopass(const float* x, int n);

// Reductions are NA-blind. The R layer enforces na_mode='forbid' by
// preflight checks; na_mode='unsafe' skips checks and still uses
// the same nomiss ABI.

// Scale a numeric vector into an output buffer
// out must be pre-allocated with n elements

void mojor_scale_f64(const double* x, double* out, int n, double scalar);

// In-place absolute value
// Only called after R's MAYBE_SHARED check in the bridge

void mojor_abs_f64(double* x, int n);

// Running max: out[i] = max(x[0..i])
// out must be pre-allocated with n elements

void mojor_running_max_f64(const double* x, double* out, int n);

// Count runs above threshold.
// out[0] = number of runs, out[1] = longest run length
// out must be pre-allocated with length 2 (int32)

void mojor_count_runs_f64(const double* x, int n, double threshold, int* out);

// Sigmoid: out[i] = 1 / (1 + exp(-x[i]))
// out must be pre-allocated with n elements

void mojor_sigmoid_f64(const double* x, double* out, int n);

// GPU sigmoid (returns >0 if GPU used; <=0 indicates failure)

int mojor_sigmoid_f64_gpu(const double* x, double* out, int n);
int mojor_sigmoid_f32_gpu(const float* x, float* out, int n);
int mojor_sigmoid_affine_f32_gpu(const float* x, float* out, int n, float scale, float bias);
int mojor_has_gpu(void);
int mojor_sigmoid_f32_gpu_iters(const float* x, float* out, int n, int iters);
int mojor_sigmoid_affine_f32_gpu_iters(
 const float* x,
 float* out,
 int n,
 int iters,
 float scale,
 float bias
);
```

## Semantics
- Inputs are contiguous and column-major for matrices.
- Outputs must be pre-allocated by the caller.
- Functions must not mutate input arrays (unless explicitly documented as in-place).
- NA handling is done at the R layer; backend functions receive NA-blind inputs.

## Return and Error Conventions

- Most compute kernels use typed scalar/void returns with no side-channel status.
- GPU entrypoints in this ABI use integer status style:
  - `> 0`: GPU path used successfully
  - `<= 0`: GPU path unavailable/failed and caller should follow fallback policy
- Callers must treat status contract consistently with bridge-level fallback routing.

## Backend Limitation Snapshot

- Reduction kernels are NA-blind by ABI contract; NA/NaN policy enforcement is bridge/runtime preflight.
- GPU ABI entrypoints expose status and rely on explicit bridge fallback handling on non-success.
- Subprocess standalone runner coverage is currently scalar/1D arg/output only.
- For consolidated current backend/runtime limitations, use `docs/KNOWN_ISSUES.md`.

## ABI Stability
- ABI v0 is stable and should not change without major version bump.
- New functions can be added, but existing functions must not change signature.

## Beginner Debug Checklist

1. Confirm symbol name matches exactly between bridge lookup and backend export.
2. Confirm argument count/order/types match function declaration.
3. Confirm pointer mutability assumptions (`const` vs in-place) are respected.
4. For buffer outputs, confirm caller allocated correct length and dtype.
5. For GPU functions, check status return and fallback path behavior together.
6. If behavior differs across environments, verify bridge/backend export parity in `packages/mojor/src/`.

---

## Related Documents

- [IR/PLAN.md](../IR/PLAN.md) - IR Direction
- [BACKEND/SSA_GUIDE.md](./SSA_GUIDE.md) - SSA Backend Guide
