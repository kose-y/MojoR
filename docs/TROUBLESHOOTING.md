# MöjoR Troubleshooting Guide

This is the canonical troubleshooting reference for MöjoR.
Use `docs/COOKBOOK.md` for examples and workflows, and this
guide for error diagnosis and fixes.

This guide helps diagnose and resolve common errors when using MöjoR.
MöjoR is intentionally strict-subset: unsupported patterns are expected to fail
with diagnostics rather than silently running in broad non-strict fallback
mode.

For fast error-text triage, use the centralized diagnostics map:
[DIAGNOSTICS_INDEX.md](docs/DIAGNOSTICS_INDEX.md).
For callable diagnostics helpers (`mojor_diagnostics*`), use:
[DIAGNOSTICS_API.md](docs/DIAGNOSTICS_API.md).
For canonical GPU support notation across wrappers/IR/runtime, use:
[GPU_FUNCTION_SUPPORT_MATRIX.md](docs/GPU_FUNCTION_SUPPORT_MATRIX.md).

---

## Table of Contents

1. [Mode Misconfiguration (`ir_only` / `object_mode` / `gpu_jit_mode`)](#mode-misconfiguration-ir_only--object_mode--gpu_jit_mode)
2. [Runtime Bridge Dispatch (`mojo_rt_v1` aliases)](#runtime-bridge-dispatch-mojo_rt_v1-aliases)
3. [IR Emitter Env Var Deprecation (`MOJOR_IR_SERVICE` / `MOJOR_IR_EMIT_SHADOW`)](#ir-emitter-env-var-deprecation-mojor_ir_service--mojor_ir_emit_shadow)
4. [Top-Level Loop Not Supported by IR](#top-level-loop-not-supported-by-ir)
5. [Mojo Compilation Errors](#mojo-compilation-errors)
6. [Runtime Errors](#runtime-errors)
7. [Parallel Helper Errors (`mojor_prange` / `mojor_run_chains_parallel`)](#parallel-helper-errors-mojor_prange--mojor_run_chains_parallel)
8. [GPU Issues (Apple Metal)](#gpu-issues-apple-metal)
9. [JIT Compile Time Got Slower](#jit-compile-time-got-slower)
10. [Build System Issues](#build-system-issues)
11. [Fallback Isolation Gate (Behavior-Based)](#fallback-isolation-gate-behavior-based)

---

## Mode Misconfiguration (`ir_only` / `object_mode` / `gpu_jit_mode`)

If you see strict/fallback surprises, start by checking mode combinations first.

### Common Mode Errors

1. Strict + object fallback mismatch:

```
mojor_build: object_mode must be 'off' when ir_only=TRUE
```

Fix:
- Use `object_mode = "off"` with `ir_only = TRUE`, or
- Set `ir_only = FALSE` before using `object_mode = "fallback"`/`"hybrid"`.

2. Strict forbids fallback entry:

```
mojor_build: strict mode (ir_only=TRUE) forbids object-mode fallback: ...
```

Fix:
- Keep strict mode and rewrite to strict-supported forms, or
- Intentionally run non-strict mode (`ir_only = FALSE`) for this workload.

3. Unified GPU route gate rejects helper-backed lowering:

```
mojor_transpile: gpu_jit_mode='unified_preview' does not allow helper-backed GPU routes ...
```

Fix:
- Keep `gpu_jit_mode = "unified_preview"` and rewrite to unified helper-free forms, or
- Use `gpu_jit_mode = "auto"` when helper-backed lowering is acceptable.

4. `gpu_matmul` expression form route mismatch:

```
IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'; otherwise assign the result to a variable
```

Fix:
- Use `gpu_jit_mode = "auto"` or `"unified_preview"` for expression/return forms, or
- Rewrite as assignment-form (`out <- gpu_matmul(...)`) when mode constraints are intentionally stricter.

Mode references:
- `docs/PIPELINE/API.md`
- `docs/GLOSSARY.md`
- `docs/DIAGNOSTICS_INDEX.md`

## Runtime Bridge Dispatch (`mojo_rt_v1` aliases)

### Common Symptoms

1. Runtime-v1 alias missing or failing:

```
mojor runtime v1 dispatch failure [symbol=<name>, reason=<reason>]: ...
```

Meaning:
- Bridge-managed calls resolve through `mojor_rt_v1_*` aliases.
- `reason=v1_symbol_not_loaded` means no runtime-v1 alias candidate was loaded.
- `reason=v1_call_error` means alias dispatch resolved but the runtime-v1 call failed.

### Fix

1. Rebuild bridge/backend artifacts and reload the bridge:

```bash
bash packages/mojor/build.sh
```

2. Verify runtime-v1 alias coverage in the loaded bridge package:

```r
is.loaded("mojor_rt_v1_mojor_sum_f64", PACKAGE = "<bridge_pkg>")
is.loaded("mojor_rt_v1_sum_f64", PACKAGE = "<bridge_pkg>")
```

3. If only base symbols are present, add the missing `mojor_rt_v1_*` aliases in
the bridge/backend and rebuild.

Notes:
- Runtime dispatch is a single lane for bridge-managed calls; runtime engine and
  shadow env toggles are no longer used.
- Non-bridge package calls still route through direct `.Call(...)` without v1
  alias resolution.

## IR Emitter Env Var Deprecation (`MOJOR_IR_SERVICE` / `MOJOR_IR_EMIT_SHADOW`)

### Common Symptoms

1. Deterministic deprecation warning:

```
mojor_transpile: MOJOR_IR_SERVICE and MOJOR_IR_EMIT_SHADOW are deprecated and ignored; unified IR emitter is always used
```

Meaning:
- Unified IR emitter is always active.
- Split-emitter env toggles are accepted only for migration compatibility and do not change behavior.

2. Unsupported IR kind fails in unified emitter:

```
mojor_transpile: unified IR emitter does not support IR kind '<kind>'
```

Meaning:
- This is a strict compile-time boundary in the unified lane.
- There is no pilot/legacy fallback route in the emitter.

### Fix

1. Remove deprecated env toggles from your shell/session:

```r
Sys.unsetenv("MOJOR_IR_SERVICE")
Sys.unsetenv("MOJOR_IR_EMIT_SHADOW")
```

2. Rewrite unsupported forms to supported lowered IR shapes, or run non-strict object-mode policy when appropriate for your workflow.

Notes:
- `trans_out$ir_service` was removed from transpile output.
- Emission diagnostics should now be interpreted through standard transpile/build errors rather than split-emitter outcome tags.

## Top-Level Loop Not Supported by IR

### Error Message

```
mojor_transpile: top-level loop not supported by IR: ... (hint: extend IR support)
```

### What This Means

This error occurs when MöjoR's IR (Intermediate Representation) layer cannot
handle a specific lowered loop form in strict mode.

### Common Causes

- A strict-subset boundary is hit (unsupported helper/call/indexing form).
- Loop/indexing shape is not in a currently supported canonical form.
- Mixed type patterns require explicit casts and fail during strict typing.

### Matrix/Array Indexing Triage

If this error appears on matrix/array writes, use this quick boundary check:

1. Confirm the index form is within the strict route:
- Prefer direct scalar indices or direct vector-index forms documented in:
  - [MATRIX_OUTPUT.md](./DESIGN/MATRIX_OUTPUT.md)
  - [SUBSET.md](./SUBSET.md)

2. For character-dimname indexing failures:
- Ensure the target dimension has dimnames.
- Literal `matrix(dimnames=...)` / `array(dimnames=...)` forms (`list(...)` of
  character literals, or `NULL`) are supported on strict compiled lanes.
- Named vector lookups (`c(a=1, b=2)["a"]`) are supported via compile-time
  `names()` tracking.
- `chr[]` function-argument index vectors are supported in strict matrix/array
  dimname routes; declare them as `chr[]` and pass character vectors at runtime.
- Empty `chr[]` index vectors are supported on strict dimname indexing lanes;
  they behave as empty selections (reads) or no-op writes.
- Local character vector element-wise loop indexing is supported when values are
  compile-time resolvable against the target dimnames (for example
  `rows <- c("r1", "r2"); mat[rows[i], "c1"]`).
- Missing-index dimname forms (for example `x[, "c2"]`, `x["r1", , "k2"]`) are
  supported on covered canonical routes; check dimnames on every referenced axis.
- If failures persist, verify each referenced dimname exists on the target axis.
  For `local chr var '<var>' value '<name>' not found in dimnames ...`, confirm
  that the local character values are present in the referenced axis dimnames.

3. For computed vector-index failures:
- Prefer direct `i32[]`/`chr[]` vars or covered constructor lanes (`c()`,
  `rep()`, `rep_len()`, `seq()` -> `i32[]`) for vector-index writes.
- Strict `i32[]` arithmetic index expressions (`+`, `-`, `*`, `%/%`, `%%`)
  are supported when they stay on supported `i32[]`/scalar lanes with
  resolvable vector lengths.
- Strict `ifelse(cond, yes, no)` index expressions are supported when `cond`
  is a supported logical lane and `yes`/`no` stay on supported `i32[]`/scalar
  lanes.
- Materialize complex computed index expressions to local vars first, then index
  with those local vectors.

4. For slice/submatrix RHS write failures:
- Direct indexed RHS slice forms are supported on covered canonical routes
  (for example `out[1:2, 2:3] <- rhs[1:2, 1:2]` and fixed-rank ND analogs).
- If strict IR still fails, the RHS expression is likely non-canonical for the
  current lowering route; materialize the RHS slice/index expression to a local
  variable first, then assign.

5. For loop bounds using shape metadata:
- `seq_len(dim(arr)[k])` is supported when `k` is a compile-time positive
  integer and `arr` is matrix/fixed-rank ND.
- Aliased forms are also supported (`dim(alias)[k]`, `nrow(alias)`,
  `ncol(alias)`, `length(alias)`), and local i32 scalar expressions
  (`max(1L, n)`, `as.integer(n + 1L)`) are accepted for loop bounds.
- Matrix type annotations are whitespace-insensitive (`f64[, ]` normalizes to
  `f64[,]`).

6. For local constructor-data failures (`unsupported expression` around
   `matrix(...)` / `array(...)`):
- Keep constructor data in strict lanes as direct literals or declared
  function-argument values.
- If constructor data is built through unsupported helper calls or complex
  expressions, precompute it in R before entering the compiled kernel.

For canonical support status, treat these references as source of truth:
- [SUBSET.md](./SUBSET.md)
- [MATRIX_OUTPUT.md](./DESIGN/MATRIX_OUTPUT.md)
- [KNOWN_ISSUES.md](./KNOWN_ISSUES.md)

### Fast Triage

1. Compile with strict diagnostics first:

```r
mojor_transpile(f, ..., ir_only = TRUE)
```

2. Check the current subset boundaries:

- [KNOWN_ISSUES.md](./KNOWN_ISSUES.md)
- [SUBSET.md](./SUBSET.md)
- [DIAGNOSTICS_INDEX.md](./DIAGNOSTICS_INDEX.md)

3. If strict mode is not required for the workload, use non-strict mode:

```r
mojor_build(f, ..., ir_only = FALSE)
```

### More Information

- [ERROR_RECOVERY_GUIDE.md](./ERROR_RECOVERY_GUIDE.md) - Detailed workarounds
- [BASELINES/README.md](./BASELINES/README.md) - Baseline artifact location and usage notes

---

## Mojo Compilation Errors

### Mixed Int/Float Arithmetic

**Error Message:**

```
Mojo compilation error: cannot apply operator * to operands of type Int32 and Float64
```

**Quick Fix:**

Add explicit type conversion:

```r
# Before (fails):
out[i] <- x[i] * y[i] - 0.5 # x: Int32, y: Float64

# After (works):
out[i] <- as.double(x[i]) * y[i] - 0.5
```

**See:** [ERROR_RECOVERY_GUIDE.md](./ERROR_RECOVERY_GUIDE.md)

### Mutable Assignment LHS Error

**Error Message:**

```
error: expression must be mutable in assignment
```

**What It Usually Means:**

Index emission for an assignment LHS was routed through a read-helper form
instead of write-context indexing.

**Quick Triage:**

1. Confirm the failing expression is assignment LHS indexing (`out[...] <- ...`).
2. Re-run in strict mode to surface the first failing route:

```r
mojor_transpile(f, ..., ir_only = TRUE)
```

3. If this appears on an older snapshot, update to a build where assignment LHS
indexing is emitted via write-context IR route.

### Inf/NaN Literal Errors

**Error Message:**

```
Mojo compilation error: use of undeclared identifier 'Inf'
```

**Why It Happens:**

Mojo doesn't recognize `Inf` as a literal. MöjoR should emit special tokens like `_MOJOR_INF` or helpers like `inf[]()`.

**Quick Fix:**

This is usually a code generation bug. If you see this:

1. Check that you're using a supported version of MöjoR
2. Report the issue with your R code snippet
3. Workaround: Avoid operations that produce Inf in the kernel

---

### SIMD Import Not Found

**Error Message:**

```
error: no module named 'simd'
```

**Fix:**

Do not import `SIMD` from `simd`. Use the built-in `SIMD` type directly, or remove the import if it is unused.

---

## Runtime Errors

### Bounds Check Failures

**Error Message:**

```
Runtime error: index out of bounds
```

**Common Causes:**

1. **Off-by-one errors with 1-based indexing:**

```r
# Wrong: Using 0-based thinking
for (i in 0:(n-1)) { # ❌ R loops are 1-based
 out[i] <- x[i]
}

# Right: 1-based indexing
for (i in 1:n) { # ✅ Correct
 out[i] <- x[i]
}
```

2. **Recycling assumptions:**

```r
# Wrong: Assumes recycling
factors <- c(1.0, -1.0)
for (i in 1:100) {
 out[i] <- x[i] * factors[i] # ❌ factors only has 2 elements
}

# Right: Manual modulo
factors <- c(1.0, -1.0)
for (i in 1:100) {
 idx <- ((i - 1) %% 2) + 1
 out[i] <- x[i] * factors[idx] # ✅ Wraps around
}
```

**See:** [ERROR_RECOVERY_GUIDE.md](./ERROR_RECOVERY_GUIDE.md)

### NA Handling Errors

**Error Message:**

```
Runtime error: NA value encountered with na_guard = "forbid"
```

**What It Means:**

Your data contains NA values, but the compiled kernel uses the default `na_guard = "forbid"` policy.

**Quick Fixes:**

1. **Clean data before calling kernel:**

```r
# Remove NAs in R before compilation
x_clean <- x[!is.na(x)]
result <- compiled_kernel(x_clean, length(x_clean))
```

2. **Use unsafe mode (not recommended):**

```r
# Only if you're certain NAs are safe to ignore
kernel <- mojor_transpile(my_function, na_guard = "unsafe")
```

---

## Parallel Helper Errors (`mojor_prange` / `mojor_run_chains_parallel`)

### `mojor_prange` transpile argument errors

Typical messages:

```
mojor_transpile: mojor_prange() must take an i32 scalar expression
mojor_transpile: mojor_prange() arg must be a function argument
mojor_transpile: mojor_prange() scalar must be i32
```

Fix-first actions:

1. Use `mojor_prange(...)` only in transpiled loop ranges.
2. Use accepted bound forms: scalar names, `length(...)`/`nrow(...)`/`ncol(...)`/`dim(...)[k]`,
   and i32 scalar expressions (for example `max(1L, n)` or `as.integer(n + 1L)`).
3. Ensure scalar range expressions remain `i32`-typed in strict kernels.

### `mojor_run_chains_parallel` runtime contract errors

Typical messages:

```
mojor_run_chains_parallel: n_chains must be a positive integer
mojor_run_chains_parallel: backend='fork' is not supported on Windows
mojor_run_chains_parallel: unsupported backend
```

Fix-first actions:

1. Validate `n_chains`, `workers`, and `seed` as scalar numeric/integer values.
2. Use `backend = "sequential"` for deterministic local smoke runs.
3. On Windows, avoid `backend = "fork"` and use `auto` or `sequential`.

Reference:
- `docs/PIPELINE/API.md`
- `docs/API_SURFACE.md`

---

## GPU Issues (Apple Metal)

### GPU Detection Crashes in Shared Libraries

**Symptom:**

Creating and freeing a `DeviceContext` during GPU detection can crash later GPU context creation (segfaults when `mojor_gpu_ctx_create` is called).

**Fix:**

Use `has_accelerator()` for detection and avoid creating a `DeviceContext` in `mojor_has_gpu`. Only create the context in `mojor_gpu_ctx_create`, and guard against NULL allocation failures.

### Block Reduction Template Errors

**Symptom:**

Mojo build errors like:

```
error: expected type in template parameter list
```

or

```
error: invalid use of non-constant template argument
```

when calling `block.min` / `block.max` / `block.sum`.

**Fix:**

Use keyword generics for block primitives and explicitly set `broadcast`:

```mojo
var v = block.sum[dtype=DType.float32, width=1, block_size=256, broadcast=False](val)
```

### `DeviceBuffer` Not Subscriptable in Kernels

**Symptom:**

Errors like:

```
error: invalid subscript operation
```

when indexing a `DeviceBuffer` inside a GPU kernel.

**Fix:**

Use `UnsafePointer[Scalar[DType.float32]]` or `UnsafePointer[Scalar[DType.float64]]` for kernel args and index through the pointer.

### `enqueue_function` Generic Signature Mismatch

**Symptom:**

Calling `enqueue_function` with a generic kernel causes compile-time errors about mismatched function signatures or inferred types.

**Fix:**

Define concrete kernels for each dtype (e.g., `_reduce_kernel_1d_gpu_f32` and `_reduce_kernel_1d_gpu_f64`) and pass those directly to `enqueue_function`.

### GPU Reduce Op Code Mismatch

**Symptom:**

`gpu_reduce()` returns the wrong result (e.g., min/max swapped) with no compile errors.

**Fix:**

Ensure the R wrapper op codes match the backend constants (`sum=0`, `min=1`, `max=2`, `mean=3`) and reject unsupported ops early.

### Missing C Bridge Wrapper for New Backend APIs

**Symptom:**

R errors like:

```
Error in .Call: C symbol 'mojor_gpu_buf_f32_reduce_call' not in load table
```

**Fix:**

Add a C bridge wrapper in `bridge.c`, register it in `CallEntries`, declare the symbol in `backend_stub.h`, and implement/export it from `backend.mojo`.

### Float32-Only Device Tests on macOS Metal

**Symptom:**

GPU tests fail or crash when using `float64` (common on Apple Metal when running device-only tests).

**Fix:**

By default, keep GPU tests on float32 and keep operations on device. In R, use `float::fl(...)` for float32 inputs (note: `float::float32(...)` does not exist).

f64 GPU tests are now opt-in via:

```bash
MOJOR_TEST_GPU_F64=1 Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
```

On Apple Metal, these f64 tests may still skip if backend f64 GPU buffers are unavailable.

### f64 Linalg/Reduce Routes Show CPU Fallback on Mac Studio

**Symptom:**

On macOS/Metal, `gpu_matmul()` / `%*%` / `gpu_reduce()` for `f64` consistently report CPU routes (for example `cpu_matmul`, `cpu_reduce`) even when GPU is available.

**What This Means:**

This is expected contract behavior. MöjoR now forces explicit CPU fallback for f64 linalg/reduce on macOS Metal to keep results deterministic until backend f64 GPU support is available.

**How To Verify:**

```r
gx <- GPUArray(matrix(as.double(1:4), nrow = 2), dtype = "f64", api = "metal")
gy <- GPUArray(matrix(as.double(1:4), nrow = 2), dtype = "f64", api = "metal")

out_mm <- gx %*% gy
out_red <- gpu_sum(gx)

attr(out_mm, "gpu_fallback")  # "cpu_matmul"
attr(out_red, "gpu_fallback") # "cpu_reduce"
```

### Mojo `@parameter` Annotation Inside `try`

**Symptom:**

Mojo build error when using `@parameter` on a variable declared within a `try` block.

**Fix:**

Move the declaration outside the `try` or drop the annotation on that variable.

---

## JIT Compile Time Got Slower

### Symptom

You notice `mojor_jit()` wrappers re-compiling more often than expected,
especially after creating a new wrapper instance for the same function/signature.

### Quick Checks

1. Verify global default:

```r
mojor_options("jit_disk_cache")$jit_disk_cache
```

2. Verify wrapper-level behavior:
- `disk_cache = TRUE` always uses disk cache.
- `disk_cache = FALSE` always disables disk cache.
- `disk_cache = NULL` follows global option at call-time.
- `eager = TRUE` compiles declared signatures at wrapper creation.
- `strict_signatures = TRUE` rejects undeclared runtime signatures.

You can also inspect the active profile bundle:

```r
mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
```

3. Inspect runtime events:

```r
info <- mojor_jit_info(jit_fn)
info$stats
info$last_error
info$signatures[[1]]$last_event
```

`last_event` should typically be:
- `"compiled"` on first specialization
- `"memory_hit"` on repeated same-wrapper calls
- `"disk_hit"` on new-wrapper reuse with disk cache enabled
- `"api_compiled"` when explicit compile API compiles a signature

Useful failure counters/fields:
- `info$stats$failed_compiles`
- `info$stats$signature_rejects`
- `info$stats$eager_compiles_attempted`
- `info$stats$eager_compiles_succeeded`
- `info$stats$api_compiles_attempted`
- `info$stats$api_compiles_succeeded`
- `info$stats$api_compiles_failed`
- `info$signatures[[i]]$compiled_via` (`"eager"`, `"runtime"`, or `"api"`)
- `info$last_error$stage` (`eager_compile|runtime_compile|runtime_execute|signature_reject|api_compile`)

### Common Causes

1. Disk cache is disabled globally:

```r
mojor_options(jit_disk_cache = TRUE)
```

2. Wrapper explicitly forced cache off:

```r
jit_fn <- mojor_jit(f, disk_cache = FALSE)  # overrides global option
```

3. Signature/options changed between calls (expected recompile):
- argument types changed
- `parallel`, `broadcast`, `elementwise_target`, or `object_mode` changed

4. Cache entry rejected by compatibility checks (expected recompile):
- transpiler/API/runtime/compiler environment differs from the entry stamp
- legacy unstamped entries are still attempted for backward compatibility

5. Strict signature gate blocked a runtime call:
- wrapper/global strict mode enabled
- inferred runtime signature not in declared `signatures`

Inspect with:

```r
info <- mojor_jit_info(jit_fn)
info$stats$signature_rejects
info$last_error
```

6. Profile preset changed defaults:

```r
mojor_options(jit_profile = "dev")   # strict off
mojor_options(jit_profile = "bench") # strict on
mojor_options(jit_profile = "prod")  # strict on
```

### `mojor_guvectorize` core-dims errors

If you use `core_dims`, MöjoR currently supports a strict subset:
- one or more input tuples
- one or more output tuples
- rank-N tuples
- core-dimension tokens may be symbolic names (`n`, `_m`, `axis2`) or fixed positive integer literals (`1`, `2`, `3`, ...)

Common errors:
- `mojor_guvectorize: core_dims must be a non-empty string like '(n),(n)->(n)'`
Cause: malformed grammar string.

- `mojor_guvectorize: core_dims must declare at least one input tuple and at least one output tuple`
Cause: `core_dims` omits either the input tuple side or output tuple side.

- `mojor_guvectorize: invalid rank syntax in signature spec for '<arg>'`
Cause: the signature type uses an unsupported rank form. Use comma-bracket syntax (`[]`, `[,]`, `[,,]`, ...) or dimensional-tag syntax (`[1d]`, `[2d]`, `[3d]`, ...).

- `mojor_guvectorize: core_dims/input rank mismatch for '<arg>'`
Cause: type spec rank and tuple rank disagree (scalar vs `[]` vs `[,]`), or runtime input rank is smaller than declared core rank.

- `mojor_guvectorize: core dimension '<dim>' mismatch (expected <x>, got <y>)`
Cause: shared symbolic core dim sizes differ across inputs, or a fixed literal core dim (for example `3` in `(n,3)`) does not match runtime input shape.

- `mojor_guvectorize: batch dimensions are not broadcast-compatible`
Cause: non-core (outer) dims cannot be NumPy-broadcasted.

- `mojor_guvectorize: output core dimension '<dim>' is not bound by inputs`
Cause: output tuple references a symbolic core dim never declared by any input tuple.

- `mojor_guvectorize: core_dims multi-output requires terminal list(...) return`
Cause: multi-output gufunc body does not end in `list(...)` or `return(list(...))`.

- `mojor_guvectorize: core_dims multi-output requires load=TRUE in this release`
Cause: multi-output wrappers are currently callable-only; `load=FALSE` is rejected.

- `mojor_guvectorize: core_dims multi-output return length mismatch (expected <x>, got <y>)`
Cause: terminal list arity does not match output tuple count in `core_dims`.

- `mojor_guvectorize: output_shape for multi-output must be a list of length <k>`
Cause: multi-output wrappers require `output_shape` as a per-output list.

- `mojor_guvectorize: output #<k> core shape mismatch`
Cause: output `<k>` returned a core shape inconsistent with its declared output tuple.

- `mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body`
Cause: rank-N strict lane accepts only the subset-v6 strict indexed forms. Unsupported calls/control-flow/index grammar are rejected.

- `mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables`
Cause: indexed reads must use canonical core indices in-order (`i1, i2, ..., iN`) for rank-N strict indexed lowering.

- `mojor_guvectorize: core_dims rank-n neighbor indexing requires explicit in-bounds guard`
Cause: neighbor reads (offset indexing) were used without sufficient explicit in-bounds coverage in the same guard scope.

- `mojor_guvectorize: core_dims rank-n does not support non-literal neighbor offsets`
Cause: neighbor offset was symbolic or non-literal (for example `i1 + k`).

- `mojor_guvectorize: core_dims rank-n guard must be canonical conjunction of bounds checks`
Cause: guard predicate was not the supported canonical `&&` bounds-check conjunction form.

- `mojor_guvectorize: core_dims rank-n does not support complex boolean guard operators`
Cause: guard used unsupported boolean operators such as `||` or `!`.

- `mojor_guvectorize: core_dims rank-n reduction axes cannot be resolved from output tuple`
Cause: reduction output tuple does not map to a valid subset of input core symbols.

- `mojor_guvectorize: core_dims rank-n reduction op is not supported in this release`
Cause: only `sum`, `mean`, `min`, and `max` are recognized for the current rank-N reduction subset.

- `mojor_guvectorize: core_dims rank-n reduction expression is not strict-compilable`
Cause: reduction argument expression includes unsupported calls or non-strict constructs.

- `mojor_guvectorize: core_dims rank-n reduction/output tuple contract is not satisfiable`
Cause: one or more outputs declare reduced core tuples but the corresponding expression is non-reduction, or tuple/expression pairing cannot be resolved.

- `mojor_guvectorize: gpu target core kernel #<k> failed strict compilation for core_dims multi-output`
Cause: one GPU-target output kernel fails strict compile; no silent fallback is applied.

- `mojor_guvectorize: gpu target core kernel failed strict compilation for core_dims rank-n`
Cause: GPU target keeps strict compile mode for rank-N core kernels; no implicit CPU downgrade is applied.

- `mojor_guvectorize: cpu target core kernel failed strict compilation for core_dims rank-n: ...`
Cause: rank-N CPU kernels are strict-only in this release path; unsupported bodies and strict compile misses now fail explicitly (no object-mode retry).

- `mojor_core_dims_batch_engine` and `mojor_core_dims_batch_cache_hit`
Cause/Interpretation: additive metadata for rank-N wrapper dispatch (`compiled_batch` vs `r_loop` lane and batch-cache status).

### `mojor_gpu_kernel` matrix2d subset errors

For canonical 2D indexed GPU kernels, MöjoR expects:
- outer loop: `seq_len(nrow(X))`
- inner loop: `seq_len(ncol(X))`
- assignment: `out[i, j] <- ...`
- matrix reads: canonical `a[i, j]` plus guarded neighbor reads `a[i + di, j + dj]` with integer-literal offsets
- guarded control flow: `if (...) out[i, j] <- ...` and `if (...) ... else ...` assignment forms
- neighbor guards: canonical `&&` bounds checks in the same condition scope

Common diagnostics:
- `gpu elementwise matrix2d requires outer loop seq_len(nrow(X))`
Cause: outer loop is not the canonical row loop.

- `gpu elementwise matrix2d requires inner loop seq_len(ncol(X))`
Cause: inner loop is not the canonical column loop over the same matrix source.

- `gpu elementwise matrix2d requires out[i, j] assignment`
Cause: inner loop writes to a non-canonical output target.

- `gpu elementwise matrix2d does not support irregular indexing`
Cause: index pairs are swapped/offset/mixed (for example `x[j, i]`).

- `gpu elementwise matrix2d neighbor indexing requires explicit in-bounds guard`
Cause: neighbor read uses offsets without sufficient same-scope bounds guards.

- `gpu elementwise matrix2d does not support non-literal neighbor offsets`
Cause: neighbor offset is not an integer literal (for example `x[i + di, j]`).

- `gpu elementwise matrix2d does not support complex boolean guard operators`
Cause: neighbor guard uses unsupported boolean operators (`||`, `!`) instead of canonical `&&` bounds conjunctions.

- `gpu elementwise matrix2d guard must be canonical conjunction of bounds checks`
Cause: guard predicate is not in the supported conjunction subset (`i/j` compared to integer literals or `nrow(X)`/`ncol(X)` bounds forms).

- `gpu elementwise matrix2d does not support reductions in kernel body`
Cause: reduction expression (`sum`, `mean`, `gpu_reduce`, etc.) inside the kernel body.

- `gpu matrix2d buffers require f32[,] inputs` (or `f64[,]`)
Cause: matrix2d kernel mixes matrix dtypes without explicit cast.

### Practical Fix Pattern

Use a shared `cache_dir` and leave `disk_cache = NULL` (or set `TRUE`) when you
want Numba-like reuse across wrapper instances:

```r
mojor_options(jit_disk_cache = TRUE)
cache_dir <- file.path(tempdir(), "mojor_jit_cache_shared")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

j1 <- mojor_jit(f, cache_dir = cache_dir)  # first call compiles
j2 <- mojor_jit(f, cache_dir = cache_dir)  # can hit disk cache on first call
```

Use eager signatures when you know hot signatures up front:

```r
jit_fast_start <- mojor_jit(
  f,
  signatures = list(list(x = "f64[]", y = "f64[]")),
  eager = TRUE,
  cache_dir = cache_dir
)
```

Or precompile later with explicit API (same cache path/key logic):

```r
jit_fn <- mojor_jit(
  f,
  signatures = list(list(x = "f64[]", y = "f64[]")),
  eager = FALSE,
  cache_dir = cache_dir
)

mojor_jit_compile(jit_fn, "(x: f64[], y: f64[])")
mojor_jit_signatures(jit_fn, kind = "compiled", format = "string")
```

### Check cache compatibility summary directly

```r
cache_info <- mojor_cache_info(cache_dir = cache_dir, include_entries = FALSE)
cache_info$jit_compat_summary
```

If `incompatible_entries > 0`, inspect `incompatible_reasons` for mismatched stamp fields.

### Gibbs benchmark guardrail check

Use this when you suspect regressions in Gibbs JIT latency/throughput:

```bash
bash scripts/repro_gibbs_runs.sh --write-current-csv /tmp/gibbs_current.csv
bash scripts/check_gibbs_perf_guardrail.sh \
  --baseline docs/BASELINES/GIBBS_PERF_BASELINE.csv \
  --current /tmp/gibbs_current.csv
```

For release-candidate gating, use the canonical command from
`docs/COOKBOOK.md` (Gibbs guardrail workflow), or run
`bash scripts/check_release_candidate.sh --help` for profile-specific inputs.

```bash
bash scripts/check_release_candidate.sh \
  --profile release-freeze \
  --perf-baseline docs/BASELINES/PERF_BASELINE_SAMPLE.csv \
  --perf-current docs/BASELINES/PERF_BASELINE_SAMPLE.csv \
  --without-gpuarray-f64-routes
```

---

## Build System Issues

### Mojo Not Found

**Error Message:**

```
Error: mojo compiler not found in PATH
```

**Fix:**

Ensure Mojo is installed and available:

```bash
# Check if mojo is in PATH
which mojo

# If not, add to PATH (adjust path as needed)
export PATH="$HOME/.modular/pkg/packages.modular.com_mojo/bin:$PATH"
```

### Stale Build Cache

**Symptoms:**
- Changes to code not reflected in compiled kernel
- Inconsistent behavior between runs

**Fix:**

Clear the build cache:

```r
# In R:
mojor_clear_cache()

# Or manually:
unlink("~/.mojor_cache", recursive = TRUE)
```

### Library Load Failures (macOS)

**Error Message:**

```
Error: Unable to load shared library 'mojor_bridge.so'
```

**Fix:**

Check rpaths:

```bash
# Check what the library is looking for
otool -L path/to/mojor_bridge.so

# If rpaths are incorrect, this is a build system bug
# Workaround: Set DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/path/to/mojo/lib:$DYLD_LIBRARY_PATH
```

---

## Test Suite Issues

### Stale Test Cache

**Symptoms:**
- Test failures with no details
- `testthat-problems.rds` exists but is unhelpful

**Fix:**

```bash
# Delete stale test cache
rm packages/mojor/tests/testthat/testthat-problems.rds

# Re-run tests
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat")'
```

### Mojo Build Timeouts

**Symptoms:**
- Tests hang during compilation
- No error message, just timeout

**Fix:**

Run tests sequentially with timeouts disabled:

```r
# Use test_file() for one test file at a time
for (f in Sys.glob("packages/mojor/tests/testthat/test_loop_runtime_*.R")) testthat::test_file(f)
```

### RNG Helper Build Failure (Stub Fallback)

**Symptoms:**

- `packages/mojor/build.sh` reports an RNG helper compilation failure
- Build continues with a stubbed RNG module

**Fix:**

If your changes do not depend on RNG, this is safe to ignore. If you need RNG support, rebuild the RNG helper with the correct Mojo toolchain and retry the build.

---

## Fallback Isolation Gate (Behavior-Based)

Fallback isolation now uses behavior-anchored patterns only (text mention counting
was removed).

### Common Errors

**Error Message:**

```
Fallback isolation check: patterns file not found: ...
```

**Fix:**

```bash
ls scripts/fallback_behavior_patterns.txt
bash scripts/check_fallback_isolation.sh --verbose
```

---

**Error Message:**

```
Fallback isolation check: patterns file has no active patterns: ...
```

**Fix:**

Ensure the pattern file includes at least one non-comment regex line.

---

**Error Message:**

```
Fallback hotspot growth detected.
```

**Fix:**

Inspect behavior hotspots and refresh baseline only after validated intent:

```bash
bash scripts/check_fallback_isolation.sh --verbose
```

Use `--allow-growth` only for temporary diagnostics in local runs.

---

For object-mode investigations in `mojor_build`, inspect build metadata:

- `object_mode_entry_summary` (`reason`, `count`) gives deterministic entry classification.
- `object_mode_entry_message` preserves the original strict compile/transpile miss message.
- IR expression emission now prefers static shape metadata (matrix row/col maps,
  local array dims, dim-var maps) before generic `n_i` fallback.

---

## Getting More Help

### Check Documentation

1. **[BASELINES/README.md](./BASELINES/README.md)** - Baseline artifact location and usage notes
2. **[ERROR_RECOVERY_GUIDE.md](./ERROR_RECOVERY_GUIDE.md)** - Comprehensive workaround guide
3. **[SUBSET.md](./SUBSET.md)** - Supported R subset
4. **[COOKBOOK.md](./COOKBOOK.md)** - Working examples

### Reporting Issues

If your error isn't covered here:

1. Check [existing GitHub issues](https://github.com/kose-y/MojoR/issues)
2. File a new issue with:
 - **Minimal reproducible example** (simplest code that shows the problem)
 - **Complete error message** (including any Mojo compiler output)
 - **Expected behavior** (what you thought should happen)
 - **MöjoR version** (`packageVersion("mojor")`)
 - **Mojo version** (`mojo --version`)
 - **Platform** (macOS/Linux, architecture)

### Debug Mode

Enable verbose output for detailed diagnostics:

```r
# Enable all debug flags
kernel <- mojor_transpile(
 my_function,
 ir_only = TRUE, # Strict IR mode (no fallbacks)
 verbose = TRUE, # Print pipeline stages
 emit_ssa_backend = TRUE # Include SSA IR dump
)
```

---

## Version History

- **2026-02-19:** Added `mojor_build` object-mode entry summary diagnostics (`object_mode_entry_summary`, `object_mode_entry_message`)
- **2026-02-19:** Added behavior-based fallback isolation gate troubleshooting
- **2026-02-18:** Added JIT disk-cache and `mojor_jit_info()` troubleshooting
- **2026-02-16:** Added `MOJOR_TEST_GPU_F64` troubleshooting guidance
- **2026-02-14:** Initial version
 - Top-level loop IR error (matrix slice assignment)
 - Mojo compilation errors (mixed types, Inf/NaN)
 - Runtime errors (bounds, NA handling)
 - Build system issues
