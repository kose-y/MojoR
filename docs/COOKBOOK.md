# MöjoR Cookbooks

This document gives small, end-to-end examples for each major capability area.
Most examples assume a development checkout:

```r
source("packages/mojor/R/mojor.R")
```

For installed-package usage, prefer exported API examples and start with:

```r
library(mojor)
```

MöjoR is a scoped, Numba-style accelerator for typed hot loops. Keep strict-subset
constraints in mind when adapting examples.

**Related Guides:**
- **Supported Subset:** [SUBSET.md](./SUBSET.md)
- **Building and Testing:** [BUILD_AND_TEST.md](../BUILD_AND_TEST.md)
- **Architecture:** [ARCHITECTURE.md](./ARCHITECTURE.md)
- **API Boundary (Exported vs Dev):** [API_SURFACE.md](./API_SURFACE.md)
- **GPU Support Matrix (Canonical):** [GPU_FUNCTION_SUPPORT_MATRIX.md](./GPU_FUNCTION_SUPPORT_MATRIX.md)
- **Diagnostics Map (Canonical):** [DIAGNOSTICS_INDEX.md](./DIAGNOSTICS_INDEX.md)
- **Diagnostics Helpers (Checkout):** [DIAGNOSTICS_API.md](./DIAGNOSTICS_API.md)

### Package Documentation Workflow

Exported API docs are roxygen-driven.

```bash
Rscript scripts/document_roxygen.R
```

This regenerates `man/` for `packages/mojor` and `packages/mojorGPU`
and refreshes `packages/mojorGPU/NAMESPACE`.

### GPU Strict Binding (Recommended)

If you are running on a GPU-capable machine, use strict binding to avoid
silent fallback to host routes when GPU bindings are unavailable.

```bash
export MOJOR_STRICT_GPU_BIND=1
bash packages/mojor/build.sh
```

To run the strict bridge smoke test:

```bash
MOJOR_TEST_GPU_STRICT_BIND=1 \
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_gpu_bridge_binding.R")'
```

To run optional f64 GPU array tests (on f64-capable GPU backends):

```bash
MOJOR_TEST_GPU_F64=1 \
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
```

On Apple Metal, these f64 tests are expected to skip.

`MOJOR_STRICT_GPU_BIND=0` explicitly allows non-strict GPU binding behavior.

## Quick Navigation

If you are:
- **Using MöjoR:** start with `Numba-like wrappers` for fastest onboarding; use `Hybrid DSL: mojor_fn()` when you need explicit compiler/IR control.
- **Tuning performance:** jump to `Performance & codegen quality`, `Performance Tips`, and `Troubleshooting`.
- **Adding compiler support:** jump to `Developer Walkthrough: Add a New Supported Operation` and [BUILD_AND_TEST.md](../BUILD_AND_TEST.md).

### Contents

- [Numba-like wrappers: jit, njit, vectorize, guvectorize](#numba-like-wrappers-jit-njit-vectorize-guvectorize)
- [Core API quick checks](#core-api-quick-checks)
- [Hybrid DSL: `mojor_fn()` (compiler-explicit entry point)](#hybrid-dsl-mojor_fn-compiler-explicit-entry-point)
- [Stable subset kernels](#stable-subset-kernels)
- [Multi-loop & richer expressions](#multi-loop-richer-expressions)
- [Development Checkout Examples (Non-Exported Helpers)](#development-checkout-examples-non-exported-helpers)
- [Performance & codegen quality](#performance-codegen-quality)
- [Performance Tips](#performance-tips)
- [GPU pipeline (explicit)](#gpu-pipeline-explicit)
- [GPUArray object model (R6-style)](#gpuarray-object-model-r6-style)
- [Random Number Generation (RNG)](#random-number-generation-rng)
- [Count/Histogram Reductions](#counthistogram-reductions)
- [Advanced Indexing Patterns](#advanced-indexing-patterns)
- [Type Predicates and Metadata Queries (Current)](#type-predicates-and-metadata-queries-current)
- [Foreign Function Interface (FFI)](#foreign-function-interface-ffi)
- [Apply Family (Current)](#apply-family-current)
- [Set/Match Primitives (Current)](#setmatch-primitives-current)
- [Quantiles & Robust Stats (Current)](#quantiles-robust-stats-current)
- [Higher-Order Functions (Current)](#higher-order-functions-current)
- [Data Structure Examples (Current)](#data-structure-examples-current)
- [Near-Term Roadmap](#near-term-roadmap)
- [Current Caveats and Workarounds](#current-caveats-and-workarounds)
- [Debug Mode (Current)](#debug-mode-current)
- [Troubleshooting](#troubleshooting)

### Reading Conventions

- `**Notes:**` highlights constraints and caveats.
- `**Expected:**` describes the behavior you should see.
- `**Status: exported API**` marks examples safe for installed-package usage.
- `**Status: non-exported helper (development checkout only)**` marks examples that rely on non-exported checkout helpers.
- Code blocks are complete, runnable snippets unless otherwise noted.

---

## Numba-like wrappers: jit, njit, vectorize, guvectorize

Use wrappers for Numba-style runtime specialization workflows:
- `mojor_jit()`: runtime-specializing default wrapper.
- `mojor_njit()`: strict wrapper (`object_mode = "off"`).
- `mojor_vectorize()`: elementwise wrapper with strict mode off.
- `mojor_guvectorize()`: explicit typed signature/core-dims wrapper.

Keep `mojor_fn()` as the best entry point when you need explicit compile-time type hints, IR/codegen inspection, or lower-level compiler debugging.

`mojor_jit()`, `mojor_njit()`, and `mojor_vectorize()` use
`disk_cache = NULL`, which defers to the global option
`mojor_options("jit_disk_cache")`.

### Wrapper chooser

| Wrapper | Use when | Key behavior |
|---|---|---|
| `mojor_jit()` | You want auto-specialization from runtime arguments | General JIT wrapper with optional eager signatures |
| `mojor_njit()` | You want strict nopython-style behavior | Forces `object_mode = "off"` |
| `mojor_vectorize()` | You want elementwise kernels with JIT specialization | Forces `elementwise = TRUE`, strict mode off |
| `mojor_guvectorize()` | You want explicit signature/core-dims contracts | Typed build wrapper for generalized vectorization |

### `mojor_njit()`: strict nopython-style wrapper

```r
f_njit <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + 1
 out
}

nj <- mojor_njit(f_njit, name = "cookbook_njit")
nj(as.double(1:8))
```

### `mojor_vectorize()`: elementwise wrapper

```r
f_vec <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 3 + 1
 out
}

vec_cpu <- mojor_vectorize(f_vec, name = "cookbook_vectorize", target = "cpu")
vec_cpu(as.double(1:8))

if (mojor_has_gpu()) {
 vec_gpu <- mojor_vectorize(f_vec, name = "cookbook_vectorize_gpu", target = "gpu")
 vec_gpu(as.double(1:8))
}
```

### Global default + precedence

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2 + 1
 out
}

# Global default (default is TRUE)
old <- mojor_options(jit_disk_cache = TRUE)
on.exit(do.call(mojor_options, old), add = TRUE)

j_follow <- mojor_jit(f)                      # disk_cache = NULL -> follows global option
j_on <- mojor_jit(f, disk_cache = TRUE)       # always on
j_off <- mojor_jit(f, disk_cache = FALSE)     # always off

# For wrappers created with disk_cache = NULL, option is read at call-time:
mojor_options(jit_disk_cache = FALSE)
j_follow(as.double(1:8))                      # uses disk cache OFF
mojor_options(jit_disk_cache = TRUE)
j_follow(as.double(1:8))                      # uses disk cache ON
```

Precedence rules:
- `disk_cache = TRUE`: always use disk cache.
- `disk_cache = FALSE`: never use disk cache.
- `disk_cache = NULL`: follow `mojor_options("jit_disk_cache")` at call-time.

### Eager signatures

You can precompile known signatures at wrapper creation:

```r
f2 <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}

# List-form signatures
jit_eager <- mojor_jit(
 f2,
 signatures = list(list(x = "f64[]", y = "f64[]")),
 eager = TRUE
)

# String-form signatures (mojor_fn-style)
jit_eager2 <- mojor_jit(
 f2,
 signatures = c("(x: f64[], y: f64[])", "(x: i32[], y: i32[])"),
 eager = TRUE
)
```

Notes:
- `eager = TRUE` requires `signatures`.
- Eager mode is fail-fast: wrapper creation errors on the first failing signature.
- `mojor_fn()` remains unchanged in this wrapper flow.

### Runtime introspection with `mojor_jit_info()`

```r
cache_dir <- file.path(tempdir(), "jit_demo")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

x <- as.double(1:8)
jit1 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
jit1(x)                                        # first call: compile
jit1(x)                                        # second call: memory hit

info1 <- mojor_jit_info(jit1)
info1$stats
# $calls_total, $cache_hits_memory, $cache_hits_disk, $cache_misses_compile, ...

# New wrapper in same cache dir can reuse disk cache:
jit2 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
jit2(x)
mojor_jit_info(jit2)$signatures[[1]]$last_event
# "disk_hit"
```

`mojor_jit_info()` returns:
- dispatcher metadata (`name`, `created_at`)
- aggregate counters (`stats`) including eager and failure stats
- last error (`last_error`) with stage and signature context
- per-signature counters/events (`signatures`) including `compiled_via` and `last_error`

### Explicit compile + signature listing

Use explicit dispatcher compile to remove first-call compile work without forcing
eager compile at wrapper construction:

```r
f4 <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}

jit4 <- mojor_jit(
 f4,
 signatures = list(
   list(x = "f64[]", y = "f64[]"),
   list(x = "i32[]", y = "i32[]")
 ),
 eager = FALSE
)

mojor_jit_compile(
 jit4,
 signatures = "(x: f64[], y: f64[])"
)

mojor_jit_signatures(jit4, kind = "declared", format = "key")
mojor_jit_signatures(jit4, kind = "compiled", format = "string")
mojor_jit_signatures(jit4, kind = "all", format = "typed_list")

mojor_jit_info(jit4)$stats[
 c("api_compiles_attempted", "api_compiles_succeeded", "api_compiles_failed")
]
```

Notes:
- `mojor_jit_compile()` reuses normal JIT cache keying and compile/cache path.
- `stop_on_error = TRUE` is fail-fast; `FALSE` returns aggregated mixed results.
- `mojor_jit_info()$last_error$stage` includes `api_compile`.
- Per-signature `last_event` includes `api_compiled`.

### Advanced `mojor_guvectorize`: target + core_dims + output shape

```r
f_g <- function(x, n) {
 out <- numeric(n)
 for (i in seq_len(n)) out[i] <- x[i] + 1
 out
}

# CPU by default
g_cpu <- mojor_guvectorize(
 f_g,
 signature = list(x = "f64[]", n = "i32"),
 output_shape = c(4L)
)

# Explicit GPU target (forces GPU elementwise lane)
g_gpu <- mojor_guvectorize(
 f_g,
 signature = list(x = "f64[]", n = "i32"),
 target = "gpu",
 output_shape = c(4L)
)

# Core-dims unary form
g_u <- mojor_guvectorize(
 f_g,
 signature = list(x = "f64[]", n = "i32"),
 core_dims = "(n),()->(n)"
)

# Core-dims binary form
g_b <- mojor_guvectorize(
 function(x, y) x + y,
 signature = list(x = "f64[]", y = "f64[]"),
 core_dims = "(n),(n)->(n)"
)

# Core-dims matrix-like form
g_m <- mojor_guvectorize(
 function(A, B) A %*% B,
 signature = list(A = "f64[,]", B = "f64[,]"),
 core_dims = "(m,n),(n,p)->(m,p)"
)

# Core-dims rank-N form (rank-3 shown)
g_n <- mojor_guvectorize(
 function(x) x + 1,
 signature = list(x = "f64[3d]"),
 core_dims = "(a,b,c)->(a,b,c)"
)

# Dual signature syntax for rank-N (comma form + Nd form)
g_dual <- mojor_guvectorize(
 function(x, y) x + y,
 signature = "(x: f64[,,], y: f64[3d])",
 core_dims = "(a,b,c),(a,b,c)->(a,b,c)"
)

# Core-dims indexed + guarded neighbor form (rank-N strict indexed lane)
g_idx <- mojor_guvectorize(
 function(x) if (i1 >= 2L && i1 <= n1 && i2 >= 1L && i2 <= n2 && i3 >= 1L && i3 <= n3) x[i1 - 1L, i2, i3] else x[i1, i2, i3],
 signature = list(x = "f64[3d]"),
 core_dims = "(a,b,c)->(a,b,c)",
 target = "gpu"
)

# Core-dims multi-output form
g_multi <- mojor_guvectorize(
 function(x, y) list(sum = x + y, diff = x - y),
 signature = list(x = "f64[]", y = "f64[]"),
 target = "gpu",
 core_dims = "(n),(n)->(n),(n)",
 output_shape = list(c(4L), c(4L))
)

# Rank-N reduction forms
g_red_m <- mojor_guvectorize(
 function(x) sum(x),
 signature = list(x = "f64[,]"),
 target = "gpu",
 core_dims = "(m,n)->(m)"
)
g_red_mix <- mojor_guvectorize(
 function(x) list(raw = x, total = sum(x)),
 signature = list(x = "f64[,]"),
 target = "gpu",
 core_dims = "(m,n)->(m,n),()"
)
```

Notes:
- `target` is `c("cpu", "gpu")`, default `"cpu"`.
- `core_dims` supports subset-v7 grammar:
 - one or more input tuples
 - one or more output tuples
 - rank-N tuples
 - examples: `"(n)->(n)"`, `"(m,n),(n,p)->(m,p)"`, `"(a,b,c)->(a,b,c)"`, `"(n),(n)->(n),(n)"`, `"(n,3)->(n,3)"`
 - core-dimension tokens may be symbolic names (`n`, `_m`, `axis2`) or fixed positive integer literals (`1`, `2`, `3`, ...).
 - signature rank syntax accepts both comma-bracket (`[]`, `[,]`, `[,,]`, ...) and dimensional-tag (`[1d]`, `[2d]`, `[3d]`, ...) forms.
- Multi-output core-dims wrappers require terminal `list(...)`/`return(list(...))` and `load = TRUE`.
- When `output_shape` is provided, loaded wrappers validate output shape at runtime.
 - single-output: integer shape vector
 - multi-output: list of integer shape vectors (one per output tuple)
- Rank-N compile policy:
 - MöjoR attempts a strict indexed lane first for strict-expressible bodies
   (terminal expression/list-return using arg refs, scalar literals, `+ - * /`,
   unary `-`, explicit casts, canonical indexed reads `x[i1, i2, ...]`, and
   guarded `if` / `if-else` expressions).
 - Neighbor indexing uses integer literal offsets only and requires explicit
   canonical `&&` bounds guards.
 - Rank-N core-dims reduction subset (`sum`, `mean`, `min`, `max`) is recognized
   for non-elementwise output tuples, with reduced axes inferred from output symbols.
 - Mixed multi-output reduced + non-reduced tuples are supported when each tuple/
   expression pair is satisfiable.
 - Unsatisfiable reduced-output contracts raise
   `core_dims rank-n reduction/output tuple contract is not satisfiable`.
 - `target = "gpu"` is strict and fails explicitly on strict compile failures.
 - `target = "cpu"` retries rank-N strict failures with `object_mode = "fallback"`
   only when `ir_only = FALSE`.
- With `ir_only = TRUE`, rank-N CPU fallback is blocked and raises an explicit error.
 - **Status: non-exported helper (development checkout only)** for the
   metadata keys below.
 - Wrapper/build metadata includes `mojor_core_dims_engine`
   (`"strict_elementwise"` vs `"direct_core"`), `mojor_core_dims_strict_error`,
   and batch dispatch metadata (`mojor_core_dims_batch_engine`,
   `mojor_core_dims_batch_cache_hit`) with deterministic cache keying over
   canonicalized signature/core-dims metadata.
- Full Numba core-dimension grammar remains future work.

### Strict signatures + JIT profiles

Use profiles to apply a JIT/cache preset:

```r
old <- mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
on.exit(do.call(mojor_options, old), add = TRUE)

mojor_options(jit_profile = "bench")
mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
# jit_profile="bench", jit_disk_cache=TRUE, jit_strict_signatures=TRUE, r_jit_level=0

# Explicit options in the same call override profile values:
mojor_options(jit_profile = "bench", jit_strict_signatures = FALSE, r_jit_level = 2L)
```

Strict signature gate (global or per-wrapper):

```r
f3 <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}

jit_strict <- mojor_jit(
 f3,
 signatures = list(list(x = "f64[]", y = "f64[]")),
 strict_signatures = TRUE
)

jit_strict(as.double(1:4), as.double(1:4))   # allowed
# jit_strict(as.integer(1:4), as.integer(1:4))  # errors: signature_reject

info <- mojor_jit_info(jit_strict)
info$stats$signature_rejects
info$last_error
```

### Cache observability with compatibility summary

**Status: non-exported helper (development checkout only)**

```r
cache_info <- mojor_cache_info(cache_dir = cache_dir, include_entries = FALSE)
cache_info$jit_compat_summary
```

`jit_compat_summary` includes:
- `total_index_entries`, `stamped_entries`, `legacy_entries`
- `compatible_entries`, `incompatible_entries`
- `incompatible_reasons` (`reason`, `count`)

### Gibbs guardrail workflow (optional RC gate)

```bash
# 1) Reproduce Gibbs runs and write current CSV
bash scripts/repro_gibbs_runs.sh --write-current-csv /tmp/gibbs_current.csv

# 2) Compare against baseline
bash scripts/check_gibbs_perf_guardrail.sh \
  --baseline docs/BASELINES/GIBBS_PERF_BASELINE.csv \
  --current /tmp/gibbs_current.csv

# 3) Optional release-candidate gate hook
bash scripts/check_release_candidate.sh \
  --allow-mismatch \
  --allow-artifacts \
  --allow-drift \
  --without-gpuarray-f64-routes \
  --with-gibbs-perf \
  --gibbs-perf-baseline docs/BASELINES/GIBBS_PERF_BASELINE.csv \
  --gibbs-perf-current /tmp/gibbs_current.csv \
  --with-perf \
  --perf-baseline docs/BASELINES/PERF_BASELINE_REAL.csv \
  --perf-current docs/BASELINES/PERF_CURRENT_REAL.csv
```

Package-only note: prototype-lane checks are still migration debt in current
release scripts; keep `--allow-mismatch`/`--allow-drift` and disable the
prototype GPU route lane until scripts are migrated.

### Fallback isolation gate (behavior-based)

Fallback isolation uses behavior-anchored patterns only (hard cutover from
text mention counting). Gate inputs:

- `scripts/check_fallback_isolation.sh`
- `scripts/fallback_behavior_patterns.txt`
- `docs/BASELINES/FALLBACK_HOTSPOTS_BASELINE.csv`

Run directly:

```bash
bash scripts/check_fallback_isolation.sh --verbose
```

Optional pattern override:

```bash
bash scripts/check_fallback_isolation.sh --patterns /tmp/custom_patterns.txt --verbose
```

Notes:
- Baseline schema remains `scope,max_hits`.
- Text-based fallback/compatibility mention counting is removed from this gate.
- Object-mode build results include deterministic entry diagnostics:
  - `object_mode_entry_summary` (`reason`, `count`)
  - `object_mode_entry_message` (raw strict miss message)
- `expr_emit` now resolves known matrix/array lengths from shape metadata before
  falling back to generic `n_i` in dynamic cases.

## Core API quick checks

Use these snippets to validate the exported high-level API before
moving into larger cookbook workflows.

**Status: exported API**

```r
# Bridge/runtime availability
mojor_load()
mojor_has_gpu()

# IR inspection
f <- function(x) {
 y <- 0
 for (i in 1:length(x)) y <- y + x[i]
 y
}
ir <- mojor_ir_dump(body(f))
mojor_ir_print(ir)

# Runtime-specializing JIT wrapper
jf <- mojor_jit(f)

# Parallel range alias in loop syntax
f_prange <- function(x, out) {
 for (i in mojor_prange(length(x))) out[i] <- x[i] * 2
 out
}

# Parallel chain runner (sequential backend for deterministic local smoke)
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

Constraints:
- `mojor_prange(...)` is a transpile-time loop alias and expects scalar loop bounds in accepted forms.
- `mojor_run_chains_parallel(...)` requires positive integer `n_chains` and valid backend selection.

## Hybrid DSL: `mojor_fn()` (compiler-explicit entry point)

Use `mojor_fn()` to compile a hot loop into Mojo.

## Stable subset kernels

**Goal:** single-loop kernels, predictable semantics, clear errors.

### Vector output (simple arithmetic)

Wrapper-first (recommended for users):

```r
f_add <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}

add_jit <- mojor_jit(f_add, name = "subset_add_jit")
add_njit <- mojor_njit(f_add, name = "subset_add_njit")
add_vec <- mojor_vectorize(f_add, name = "subset_add_vec", target = "cpu")

add_jit(as.double(1:3), as.double(4:6))
add_njit(as.double(1:3), as.double(4:6))
add_vec(as.double(1:3), as.double(4:6))
```

Compiler-explicit equivalent (`mojor_fn()`):

```r
add <- mojor_fn(
 function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i]
 }
 out
 },
 x = "f64[]", y = "f64[]"
)

add(1:3, 4:6)
```

### Scalar reduction (sum)

Wrapper-first (recommended for users):

```r
f_sum <- function(x) {
 acc <- 0
 for (i in seq_along(x)) acc <- acc + x[i]
 acc
}

sum_jit <- mojor_jit(f_sum, name = "subset_sum_jit")
sum_njit <- mojor_njit(f_sum, name = "subset_sum_njit")

sum_jit(as.double(1:10))
sum_njit(as.double(1:10))
```

Compiler-explicit equivalent (`mojor_fn()`):

```r
sum_loop <- mojor_fn(
 function(x) {
 acc <- 0
 for (i in seq_along(x)) {
 acc <- acc + x[i]
 }
 acc
 },
 x = "f64[]"
)

sum_loop(1:10)
```

### Loop range transforms (allowed)

```r
range_affine <- mojor_fn(
 function(n) {
 acc <- 0
 for (i in seq_len(n) / 2) {
 acc <- acc + i
 }
 for (j in 10 / seq_len(n)) {
 acc <- acc + j
 }
 acc
 },
 n = "i32"
)
```

**Notes:**
- Range transforms only accept **scalar** expressions on the left/right.
- Not allowed: `seq_len(n) / x` (array), `x + y` (arbitrary vector), `seq(..., by = 0.5)` for indexing (float steps are value‑loop only).
- `seq.int(from, to)` auto‑descends when `to < from` (dynamic sign, same behavior as `:`).

Explicit min/max/product accumulators:

```r
min_loop <- mojor_fn(
 function(x) {
 acc <- x[1]
 for (i in seq_along(x)) {
 acc <- min(acc, x[i])
 }
 acc
 },
 x = "f64[]"
)

prod_loop <- mojor_fn(
 function(x) {
 acc <- 1
 for (i in seq_along(x)) {
 acc <- acc * x[i]
 }
 acc
 },
 x = "f64[]"
)
```

**Expected:**
- Accumulators are explicitly initialized before the loop.

which.min / which.max loop pattern:

```r
which_min <- mojor_fn(
 function(x) {
 acc <- x[1]
 idx <- 1L
 for (i in seq_along(x)) {
 if (x[i] < acc) {
 acc <- x[i]
 idx <- as.integer(i)
 }
 }
 idx
 },
 x = "f64[]"
)
```

**Expected:**
- Explicit init for both `acc` and `idx`.
- Returns `idx` as an integer scalar.

Top-level which.min / which.max (no loop required):

```r
which_min_top <- mojor_fn(function(x) which.min(x), x = "f64[]")
which_max_top <- mojor_fn(function(x) which.max(x), x = "f64[]")
```

NA handling for `which.min` / `which.max` follows `na_mode`: `forbid` errors on
NA/NaN, `unsafe` skips checks (undefined results if NA/NaN are present). Empty
inputs return `integer(0)`.

You can also call stdlib-backed reductions directly:

**Status: non-exported helper (development checkout only)**

```r
mojor_min_f64(1:10)
mojor_max_f64(1:10)
mojor_mean_f64(1:10)
mojor_prod_f64(1:10)
mojor_which_min_f64(1:10)
mojor_which_max_f64(1:10)
mojor_sum_f32(float::fl(1:10))
mojor_prod_f32(float::fl(1:10))
mojor_which_min_f32(float::fl(1:10))
mojor_which_max_f32(float::fl(1:10))
mojor_min_f32(float::fl(1:10))
mojor_max_f32(float::fl(1:10))
mojor_mean_f32(float::fl(1:10))
```

### Any/all (logical reductions)

Wrapper-first (recommended for users):

```r
any_pos_jit <- mojor_jit(function(x) any(x > 0), name = "subset_any_jit")
all_pos_njit <- mojor_njit(function(x) all(x > 0), name = "subset_all_njit")

any_pos_jit(as.double(c(-1, -2, 3)))
all_pos_njit(as.double(c(1, 2, 3)))
```

Compiler-explicit equivalent (`mojor_fn()`):

```r
any_pos <- mojor_fn(function(x) any(x > 0), x = "f64[]")
all_pos <- mojor_fn(function(x) all(x > 0), x = "f64[]")
```

**Expected:**
- Output kind is scalar logical (`lgl`), and the loop contains a short‑circuit break.

**Selector-index variants** (index array selects elements to check):
```r
any_sel <- mojor_fn(function(x, idx) any(x[idx] > 0), x = "f64[]", idx = "i32[]")
all_sel_shift <- mojor_fn(function(x, idx) all(x[idx + 1L] > 0), x = "f64[]", idx = "i32[]")
```

**Multi-arg wrappers with named `...`:**
```r
any_named <- mojor_fn(
 function(x, y) any(lhs = x > 0, rhs = y > 0, na.rm = FALSE),
 x = "f64[]", y = "f64[]"
)
all_named <- mojor_fn(
 function(x, y) all(lhs = x > 0, rhs = y > 0, na.rm = FALSE),
 x = "f64[]", y = "f64[]"
)
```

**Scalar-only wrappers** (no array arguments):
```r
any_scalar <- mojor_fn(
 function(a, b) any(lhs = a > 0, rhs = b > 0, na.rm = FALSE),
 a = "f64", b = "f64"
)
all_scalar <- mojor_fn(
 function(a, b) all(lhs = a > 0, rhs = b > 0, na.rm = FALSE),
 a = "f64", b = "f64"
)
```

**Zero-operand wrappers** (constant forms):
```r
any_zero <- mojor_fn(function(a) any(na.rm = FALSE), a = "f64")
all_zero <- mojor_fn(function(a) all(na.rm = FALSE), a = "f64")
any_zero_rm <- mojor_fn(function(a) any(na.rm = TRUE), a = "f64")
all_zero_rm <- mojor_fn(function(a) all(na.rm = TRUE), a = "f64")
```

**Notes:**
- `na.rm` must be a literal scalar logical (`TRUE` or `FALSE`) when provided.
- `na.rm = TRUE` skips missing values in supported `any()/all()` wrappers.
- Scalar-only wrappers compile without synthetic loop scaffolding.
- Foldable constant forms (zero-operand or literal-only) become direct scalar constants.

### Constructor-only vectors (no explicit loops)

```r
rep_twice <- mojor_fn(function(x) rep(x, times = 2L), x = "f64[]")
concat <- mojor_fn(function(x, y) c(x, 3, y), x = "f64[]", y = "f64[]")

rep_twice(1:3)
concat(1:2, 10:11)
```

**Notes:**
- `rep()` / `rep.int()` support scalar `times=` or `length.out=`.
- `each=` is supported for constructor-only `rep()` / `rep.int()` when `length.out` is not set (if both are given, `length.out` wins).
- `rep_len()` requires scalar `length.out=`.
- `c()` supports scalar literals and array names; output length is computed from parts.

### Array utility functions (cbind, rbind, t)

**Column and row binding:**

```r
# cbind cycles through arguments by column
bind_cols <- mojor_fn(function(x, y, n) {
 out <- numeric(n)
 for (i in 1:n) {
 out[i] <- cbind(x, y)[i]
 }
 out
}, x = "f64[]", y = "f64[]", n = "i32")

# With x = c(1, 3, 5), y = c(2, 4, 6), n = 6
# Returns: c(1, 2, 3, 4, 5, 6) - cycling through columns
bind_cols(c(1, 3, 5), c(2, 4, 6), 6L)

# rbind works similarly for row-wise binding
bind_rows <- mojor_fn(function(x, y, n) {
 out <- numeric(n)
 for (i in 1:n) {
 out[i] <- rbind(x, y)[i]
 }
 out
}, x = "f64[]", y = "f64[]", n = "i32")
```

**Matrix transpose:**

```r
# t() for matrix transpose (linearized indexing)
transpose_vals <- mojor_fn(function(x, n) {
 out <- numeric(n)
 for (i in 1:n) {
 out[i] <- t(x)[i]
 }
 out
}, x = "f64[]", n = "i32")

# t() with 2D indexing (matrix input only)
transpose_mat <- mojor_fn(function(mat, nr, nc) {
 out <- matrix(0, nc, nr) # Note swapped dimensions
 for (i in 1:nc) {
 for (j in 1:nr) {
 # t(mat)[i, j] = mat[j, i] - indices are swapped
 out[i, j] <- mat[(i - 1) * nr + j] # Manual transpose for vectors
 }
 }
 out
}, mat = "f64[]", nr = "i32", nc = "i32")
```

**Notes:**
- `cbind()` and `rbind()` cycle through arguments using modular arithmetic
- Index calculation: `arg = pos % n_args`, `idx = pos / n_args`
- All arguments should have sufficient length for the calculated indices
- `t()` supports:
 - 1D linearized indexing: `t(x)[i]` → `x[i]`
 - 2D index transformation: `t(mat)[i, j]` → `mat[j, i]` (when mat has matrix type with tensor wrapper)

---

## Multi-loop & richer expressions

**Goal:** richer AST, still deterministic.

### Multiple loops (same output)

```r
two_pass <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i]
 }
 for (i in seq_along(x)) {
 out[i] <- out[i] + 1
 }
 out
}

built <- mojor_fn(two_pass, x = "f64[]", load = FALSE)
built$trans$fused # TRUE if fusion applied
```

**Expected:**
- When fusion applies, the generated Mojo has a single loop.

### Multiple loops with different ranges (scalar output only)

```r
sum_two <- function(x, y) {
 acc <- 0
 for (i in seq_along(x)) {
 acc <- acc + x[i]
 }
 for (j in seq_along(y)) {
 acc <- acc + y[j]
 }
 acc
}

built <- mojor_fn(sum_two, x = "f64[]", y = "f64[]", load = FALSE)
```

**Expected:**
- Mixed top‑level ranges are allowed only when the output is scalar.
- Vector outputs still require a shared range source.

### Nested loops (same range source)

```r
outer_inner <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 for (j in seq_along(x)) {
 out[i] <- out[i] + x[j]
 }
 }
 out
}

built <- mojor_fn(outer_inner, x = "f64[]", load = FALSE)
```

**Expected:**
- Nested loops are preserved (two `for` loops).
- `built$trans$parallel$safe` is FALSE (loop‑carried dependency).

### Mixed ranges with vector output (rejected)

```r
bad <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i]
 for (j in seq_along(y)) out[j] <- y[j]
 out
}

mojor_fn(bad, x = "f64[]", y = "f64[]", load = FALSE)
```

### xor() logical operation

```r
xor_check <- mojor_fn(
 function(x, y) {
 out <- logical(length(x))
 for (i in seq_along(x)) {
 out[i] <- xor(x[i] > 0, y[i] > 0)
 }
 out
 },
 x = "f64[]", y = "f64[]"
)

# xor in any()/all() expressions
xor_any <- mojor_fn(
 function(x, y) any(xor(x > 0, y > 0)),
 x = "f64[]", y = "f64[]"
)
```

**Expected:**
- `xor(a, b)` lowers as boolean inequality: `as.logical(a) != as.logical(b)`.
- Works in loop assignments, conditions, and `any()`/`all()` expressions.

### Scalar logical & and |

```r
and_or <- mojor_fn(
 function(x, y, z) {
 out <- logical(length(x))
 for (i in seq_along(x)) {
 out[i] <- (x[i] > 0) & (y[i] > 0) | (z[i] > 0)
 }
 out
 },
 x = "f64[]", y = "f64[]", z = "f64[]"
)
```

**Expected:**
- Scalar `&` and `|` in loop contexts normalize to IR logical `&&` and `||`.
- These are elementwise boolean operations (not short-circuit at the R level).

### ifelse lowering inside expression

```r
mix <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- ifelse(x[i] > 0, x[i], 0) + y[i]
 }
 out
}

built <- mojor_fn(mix, x = "f64[]", y = "f64[]", load = FALSE)
```

**Expected:**
- A temporary `_mojor_ifelse_` is emitted.
- A warning is emitted unless `mojor_options(warn_ifelse = FALSE)` is set.

---

## Development Checkout Examples (Non-Exported Helpers)

These patterns are available in a development checkout and rely on non-exported
helpers. For installed-package-safe entrypoints, use exported APIs listed in
`docs/API_SURFACE.md`.

### FFI declaration + header export

**Status: non-exported helper (development checkout only)**

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2
 out
}
built <- mojor_build(f, x = "f64[]", load = FALSE)

# Declare a C symbol and build an IR c_call node (declaration-time check)
mojor_declare_c("demo_scale", args = list(x = "f64", y = "f64"), returns = "f64")
node <- mojor_c_call("demo_scale", 1.0, 2.0)
node$kind

# Export generated C header for the compiled kernel ABI
hdr <- tempfile(fileext = ".h")
mojor_export_c_header(built, hdr)
file.exists(hdr)
```

### Serialization helpers (metadata-first load)

**Status: non-exported helper (development checkout only)**

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2
 out
}
built <- mojor_build(f, x = "f64[]", load = FALSE)

path <- tempfile(fileext = ".mojor")
mojor_save(built, path)

info <- mojor_serialize(path)
serial <- readRDS(path)
mojor_check_compatibility(serial)

# Metadata-only load; no dyn.load required
loaded <- mojor_load_kernel(path, load_library = FALSE)
c(info$kernel, loaded$kernel)
```

### Profiling report + suggestions

**Status: non-exported helper (development checkout only)**

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2
 out
}
built_prof <- mojor_build(f, x = "f64[]", load = FALSE, profile = TRUE)

prof <- mojor_profile_report(built_prof)
sugg <- mojor_suggest(built_prof)

length(sugg)
print(prof)
```

### R-style function (type hints required)

```r
fast_add <- mojor_fn(
 function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i]
 }
 out
 },
 x = "f64[]", y = "f64[]"
)

fast_add(1:3, 4:6)
```

**Notes:**
- **Type hints are required** when you pass a function. MöjoR does not infer
 types yet, and type hints are how we generate safe, fast Mojo code.
- The function name is optional; `mojor_fn()` auto‑generates a stable name.
- Reduction variants are controlled via flags (`reduction=`, `simd_mode=`), not by calling `mojor_*` helpers inside `mojor_fn()` bodies.

### Using flags with `mojor_fn()`

```r
fast_sum <- mojor_fn(
 function(x) sum(x),
 x = "f64[]",
 reduction = "tree",
 simd_mode = "explicit"
)
```

### Type hint quick reference

Use these as named args in `mojor_fn()`:

| Hint | Meaning | R input type |
| --- | --- | --- |
| `f64` | scalar double | `numeric` length 1 |
| `f64[]` | double vector | `numeric` vector |
| `i32` | scalar integer | `integer` length 1 |
| `i32[]` | integer vector | `integer` vector |
| `bool` | scalar logical | `logical` length 1 |
| `bool[]` | logical vector | `logical` vector |

**Notes:**
- `f32` support is available via `float::fl()` wrappers in runtime helpers.
- NA handling uses `na_mode="forbid"` unless overridden (errors on NA/NaN).
- `na_mode="propagate"` enables deterministic NA propagation for supported arithmetic/comparison/ifelse paths.
- `na_mode="na_rm"` skips NA values for supported reductions and `any()/all()` wrappers.
- `na_mode="unsafe"` skips checks; results are undefined if NA/NaN are present.

### Signature string (types inline)

```r
fast_double <- mojor_fn(
 "(x: f64[])",
 "out <- x * 2; out"
)
```

**Notes:**
- Use this form when you want a compact, inline DSL.
- The signature string must include types for each argument.

### No-Loop Specialization (No Explicit Loops Required)

MöjoR supports compiling standalone expressions without explicit loops:

**Standalone reductions:**
```r
# Compile reduction functions directly
fast_sum <- mojor_fn(function(x) sum(x), x = "f64[]")
fast_mean <- mojor_fn(function(x) mean(x), x = "f64[]")
fast_min <- mojor_fn(function(x) min(x), x = "f64[]")
fast_max <- mojor_fn(function(x) max(x), x = "f64[]")
fast_prod <- mojor_fn(function(x) prod(x), x = "f64[]")

fast_sum(1:10) # Returns 55
fast_mean(1:10) # Returns 5.5
```

**Scalar expressions:**
```r
# Simple binary operations
add_scalars <- mojor_fn(function(x, y) x + y, x = "f64", y = "f64")
multiply <- mojor_fn(function(x, y) x * y, x = "f64", y = "f64")

# Compound expressions
formula <- mojor_fn(function(x, y, z) x * y + z, x = "f64", y = "f64", z = "f64")

add_scalars(3.0, 4.0) # Returns 7.0
formula(3.0, 4.0, 5.0) # Returns 17.0
```

**Reduction expressions:**
```r
# Reductions combined with operations
sum_plus_one <- mojor_fn(function(x) sum(x) + 1, x = "f64[]")
sum_times_two <- mojor_fn(function(x) sum(x) * 2, x = "f64[]")

# Multiple reductions
sum_times_mean <- mojor_fn(function(x, y) sum(x) * mean(y), x = "f64[]", y = "f64[]")

# Complex expressions with parentheses
avg_of_stats <- mojor_fn(function(x, y) (sum(x) + mean(y)) / 2, x = "f64[]", y = "f64[]")

sum_plus_one(c(1,2,3,4,5)) # Returns 16.0
sum_times_mean(c(1,2,3), c(4,5,6)) # Returns 30.0
```

**Implicit vectorized expressions:**
```r
# Single-expression vectorized operations (no loops needed)
vec_add <- mojor_fn(function(x, y) x + y, x = "f64[]", y = "f64[]")
vec_norm <- mojor_fn(function(x) sqrt(x * x), x = "f64[]")

# Type checks and NA handling
na_replace <- mojor_fn(function(x, fill) ifelse(is.na(x), fill, x),
                        x = "f64[]", fill = "f64")

# Type casts in expressions
to_indicator <- mojor_fn(function(x) as.integer(x > 0), x = "f64[]")

# xor and mixed logical operations
exclusive <- mojor_fn(function(x, y) xor(x > 0, y > 0),
                       x = "f64[]", y = "f64[]")

# Multi-statement let-binding bodies (automatically inlined)
diff_of_squares <- mojor_fn(function(x, y) {
  s <- x + y
  d <- x - y
  s * d
}, x = "f64[]", y = "f64[]")

vec_add(c(1, 2, 3), c(4, 5, 6))       # Returns c(5, 7, 9)
na_replace(c(1, NaN, 3), 0)            # Returns c(1, 0, 3)
diff_of_squares(c(5, 3), c(2, 1))      # Returns c(21, 8)
```

**Notes:**
- No-loop bodies are specialized within the same strict pipeline when there are no explicit loops
- Reductions are compiled inline for optimal performance
- All scalar return types are supported
- Implicit vectorization accepts typed array args (`f64[]`, `i32[]`, `lgl[]`)
  with scalar args for automatic type promotion
- Multi-statement bodies with local variable assignments are automatically
  inlined into a single expression before compilation
- Type promotion works automatically (Int32 → Float64 when needed)

### Matrix multiplication operations (PR-B6)

MöjoR supports optimized matrix multiplication without explicit loops:

**Standard matrix multiplication:**
```r
# A[m×n] %*% B[n×p] -> C[m×p]
matmul <- mojor_fn(function(A, B) A %*% B, A = "f64[,]", B = "f64[,]")

A <- matrix(as.double(1:6), nrow = 2, ncol = 3) # 2×3 numeric matrix
B <- matrix(as.double(1:6), nrow = 3, ncol = 2) # 3×2 numeric matrix
matmul(A, B) # Returns 2×2 matrix
```

**Cross products (optimized transpose operations):**
```r
# crossprod(A) = t(A) %*% A
crossprod_fn <- mojor_fn(function(A) crossprod(A), A = "f64[,]")

# crossprod(A, B) = t(A) %*% B
crossprod_2arg <- mojor_fn(function(A, B) crossprod(A, B),
 A = "f64[,]", B = "f64[,]")

# tcrossprod(A) = A %*% t(A)
tcrossprod_fn <- mojor_fn(function(A) tcrossprod(A), A = "f64[,]")

# tcrossprod(A, B) = A %*% t(B)
tcrossprod_2arg <- mojor_fn(function(A, B) tcrossprod(A, B),
 A = "f64[,]", B = "f64[,]")

A <- matrix(as.double(1:6), nrow = 2, ncol = 3) # 2×3 numeric matrix
crossprod_fn(A) # Returns 3×3 matrix (t(A) %*% A)
tcrossprod_fn(A) # Returns 2×2 matrix (A %*% t(A))
```

**Matrix operation dimension rules:**
- `A[m×n] %*% B[n×p]` → `C[m×p]` (inner dimensions must match)
- `crossprod(A[m×n])` → `C[n×n]` (always square)
- `crossprod(A[m×n], B[m×p])` → `C[n×p]` (same row count)
- `tcrossprod(A[m×n])` → `C[m×m]` (always square)
- `tcrossprod(A[m×n], B[p×n])` → `C[m×p]` (same column count)

**Implementation notes:**
- Uses optimized triple nested loops with column-major indexing
- No explicit memory allocation needed in user code
- Generates efficient BLAS-style operations
- All operations return matrices with proper dimensions

### Covariance and correlation (PR-B7.2)

MöjoR supports covariance and correlation functions for vectors:

**Covariance between two vectors:**
```r
# cov(x, y) computes sample covariance
cov_fn <- mojor_fn(function(x, y) cov(x, y), x = "f64[]", y = "f64[]")

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
cov_fn(x, y) # Returns scalar covariance
```

**Correlation between two vectors:**
```r
# cor(x, y) computes Pearson correlation coefficient
cor_fn <- mojor_fn(function(x, y) cor(x, y), x = "f64[]", y = "f64[]")

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
cor_fn(x, y) # Returns scalar correlation (1.0 for perfect positive correlation)
```

**Important notes:**
- Both functions require **two vector arguments** (matches R's behavior)
- For variance of a single vector, use `var(x)` (already supported)
- For correlation of a vector with itself, use `cor(x, x)` (returns 1.0)
- Matrix inputs are supported on the expression-only lane:
- `cov(X)` / `cor(X)` for `X[m x p]` return `p x p` matrices
- `cov(X, Y)` / `cor(X, Y)` for `X[m x p]`, `Y[m x q]` return `p x q` matrices
- Matrix pair forms require matching row count (`nrow(X) == nrow(Y)`)
- Uses Pearson method with sample variance divisor (n-1)

**Mathematical formulas:**
- `cov(x, y) = sum((x - mean(x)) * (y - mean(y))) / (n - 1)`
- `cor(x, y) = cov(x, y) / (sd(x) * sd(y))`

---

## Performance & codegen quality

**Goal:** predictable performance levers.

### SIMD mode selection

```r
add_r <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i]
 }
 out
}

# Explicit SIMD path (default) when eligible
mojor_options(simd_mode = "explicit")
built_explicit <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
built_explicit$trans$simd$safe

# Compiler-decided vectorization only
mojor_options(simd_mode = "auto")
built_auto <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
built_auto$trans$simd$safe

# Disable SIMD explicitly
mojor_options(simd_mode = "off")
built_off <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
built_off$trans$simd$safe
```

**Expected:**
- `trans$simd$safe` reports SIMD eligibility in current IR-backed paths.
- `simd_mode = "off"` disables explicit SIMD even when eligibility checks pass.

Report SIMD eligibility:

**Status: non-exported helper (development checkout only)**

```r
mojor_simd_report(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, simd_mode = "auto")
```

Using a precomputed trans:

```r
built <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
mojor_simd_report(trans = built$trans)
```

Sample output:

```
simd.mode: explicit
simd.safe: TRUE
simd.assume_aligned: 32
```

fusion_debug example (build + inspect):

```r
built <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, fusion_debug = TRUE, load = FALSE)
built$trans$fused
```

IR pipeline inspection recipe:

**Status: non-exported helper (development checkout only)**

```r
trans <- mojor_transpile(
 add_r,
 x = "f64[]",
 y = "f64[]",
 emit_ir = TRUE,
 emit_ssa_backend = TRUE
)

# Was SSA backend prep successful?
is.null(trans$ssa_backend_error)

# Which pipeline stages executed?
trans$ssa_backend$pipeline
names(trans$ssa_backend$verifier$boundary)

# See generated Mojo
cat(trans$mojo)

# See tree IR
mojor_ir_print(trans$ir)

# See SSA/backend tiers (developer helpers)
cat(.mojor_ir_ssa_format(trans$ssa_backend$ssa$optimized), "\n")
cat(.mojor_ir_ssa_backend_format(trans$ssa_backend$backend_lowered), "\n")
```

### Developer Walkthrough: Add a New Supported Operation

**Status: non-exported helper (development checkout only)**

This is the end-to-end playbook for breadth work (adding support for a new op).
It is written for contributors, not end users.

#### Step 1: Start with a concrete operation probe

Use a tiny kernel that exercises only the new operation:

```r
f_op <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- log1p(x[i]) # Replace with the operation you are adding
 }
 out
}
```

Check current behavior:

```r
try(
 mojor_transpile(
 f_op,
 x = "f64[]",
 emit_ir = TRUE,
 emit_ssa_backend = TRUE
 )
)
```

If this fails, keep the error text stable and explicit (`mojor_transpile: ...`).

#### Step 2: Force strict IR (no silent fallback)

When implementing new operation support, always verify it works in strict IR mode:

```r
mojor_options(ir_only = TRUE)
trans <- mojor_transpile(
 f_op,
 x = "f64[]",
 emit_ir = TRUE,
 emit_ssa_backend = TRUE
)
mojor_options(ir_only = FALSE)
```

If this fails in `ir_only = TRUE`, the path still depends on fallback and is not complete.

#### Step 3: Inspect emitted artifacts

Use these checks to confirm the op is actually represented through the pipeline:

**Status: non-exported helper (development checkout only)**

```r
# Tree IR
mojor_ir_print(trans$ir)

# SSA tiers
cat(.mojor_ir_ssa_format(trans$ssa_backend$ssa$typed), "\n")
cat(.mojor_ir_ssa_backend_format(trans$ssa_backend$backend_lowered), "\n")
cat(.mojor_ir_ssa_backend_selected_format(trans$ssa_backend$backend_selected), "\n")

# Final Mojo text
cat(trans$mojo)
grepl("log1p", trans$mojo, fixed = TRUE) # Replace pattern for your op
```

Useful status fields:

```r
is.null(trans$ssa_backend_error)
trans$ssa_backend$pipeline
names(trans$ssa_backend$verifier$boundary)
```

#### Step 4: Implement in the correct lane

Choose one lane and keep changes localized to package sources.

Lane A: transpile/IR operation (most ops)
- Edit `packages/mojor/R/transpile/transpile.R`.
- Edit relevant `packages/mojor/R/ir/*.R` modules (commonly `expr_build.R`, `verify.R`, `expr_emit.R`, `stmt_emit.R`).

Lane B: native ABI operation (`.Call`/C/Mojo runtime)
- Edit `packages/mojor/src/backend.mojo`.
- Edit `packages/mojor/src/backend_stub.h`.
- Edit `packages/mojor/src/bridge.c`.
- Add or adjust R wrappers in `packages/mojor/R/core/reductions_jit.R`.

**Lane A vs Lane B: when to use which**

- **Lane A (transpile/IR):** teach the compiler to understand and emit a new R-level operation.
- **Lane B (native ABI):** add a new runtime primitive exposed through `.Call`.

Use Lane A when:
- The operation can be represented in existing IR/codegen structure.
- You are expanding language coverage or semantics.
- You want fast iteration with `emit_ir`, `emit_ssa_backend`, and `ir_only = TRUE`.

Use Lane B when:
- You need a dedicated runtime/kernel primitive for performance or reuse.
- The operation is hard to express cleanly in transpiler-only logic.
- You want a shared native helper callable from multiple front-end paths.

Tradeoffs:
- Lane A: lower ABI risk, faster iteration; risk is fallback/codegen pattern bugs.
- Lane B: higher performance/control; risk is symbol/registration/linkage errors.

Practical rule:
- Start in Lane A for breadth.
- Move to Lane B when a stable runtime primitive is justified.
- Many features are hybrid: Lane A semantics + Lane B helper call.

Validation emphasis:
- Lane A: transpile tests + runtime tests + strict `ir_only = TRUE`.
- Lane B: all of the above, plus `R CMD INSTALL` and direct native symbol smoke checks.

**Examples for each line in Step 4**

**Lane A Example (Transpile/IR operation)**

1. Edit `packages/mojor/R/ir/expr_build.R` Example: map a new scalar call in expression build/emission.

```r
# Example pattern inside scalar_expr(...)
scalar_expr <- function(node) deparse(node)
op <- "log1p"
expr <- quote(log1p(x))
if (op == "log1p" && length(expr) == 2) {
 arg <- scalar_expr(expr[[2]])
 return(paste0("log1p(", arg, ")"))
}
```

2. Edit `packages/mojor/R/ir/typing.R` Example: add IR effects/type behavior for the same call.

```r
# Example pattern inside IR call handling
if (kind == "call" && identical(node$fn, "log1p")) {
 return("f64") # type inference result for numeric input
}
```

3. Keep the change scoped to `packages/mojor/R/transpile/` and `packages/mojor/R/ir/`.

**Lane B Example (Native ABI operation)**

**Status: non-exported helper (development checkout only)**

4. Edit `packages/mojor/src/backend.mojo` Example: add exported ABI symbol.

```mojo
@export("mojor_log1p_f64", ABI="C")
fn mojor_log1p_f64(x: UnsafePointer[DType.float64], out: UnsafePointer[DType.float64], n: Int):
 for i in range(n):
 out[i] = log1p(x[i])
```

5. Edit `packages/mojor/src/backend_stub.h` Example: declare matching C symbol used by `bridge.c`.

```c
void mojor_log1p_f64(const double* x, double* out, int n);
```

6. Keep implementation in `packages/mojor/src/backend.mojo` only (no C fallback source is maintained).

7. Edit `packages/mojor/src/bridge.c` Example: add `.Call` wrapper and register it.

```c
SEXP mojor_log1p_f64_call(SEXP r_x) {
 int n = LENGTH(r_x);
 SEXP out = PROTECT(allocVector(REALSXP, n));
 mojor_log1p_f64(REAL(r_x), REAL(out), n);
 UNPROTECT(1);
 return out;
}

/* CallEntries addition */
{"mojor_log1p_f64", (DL_FUNC) &mojor_log1p_f64_call, 1},
```

8. Keep native edits scoped to `packages/mojor/src/...`.

9. Add or adjust wrappers in `packages/mojor/R/core/reductions_jit.R`. Example: expose the new native op to R users/tests.

```r
mojor_log1p <- function(x) {
 if (!mojor_is_loaded()) stop("Mojo backend not loaded")
 .Call("mojor_log1p_f64", as.double(x))
}
```

**Lane A End-to-End Example (R -> Mojo -> execution)**

```r
# 1) R function using the new operation
f_log1p <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- log1p(x[i])
 }
 out
}

# 2) Inspect generated Mojo for the function
trans <- mojor_transpile(
 f_log1p,
 x = "f64[]",
 emit_ir = TRUE,
 emit_ssa_backend = TRUE
)
is.null(trans$ssa_backend_error)
cat(trans$mojo)
grepl("log1p", trans$mojo, fixed = TRUE)

# 3) Compile + run through Mojo
fast_log1p <- mojor_fn(f_log1p, x = "f64[]")
fast_log1p(c(0, 1, 3))
all.equal(fast_log1p(c(0, 1, 3)), log1p(c(0, 1, 3)))
```

#### Step 5: Add tests in pairs

Minimum test coverage for new op support:

Transpilation tests (`test_transpile_<feature>.R`)
- Assert emitted Mojo/IR patterns with `grepl(...)`.
- Include negative tests for unsupported forms and stable errors.

Runtime tests (`test_loop_runtime_*.R` or a feature runtime file)
- Compare compiled output with reference R output via `expect_equal(...)`.
- Use `skip_if_no_mojo()` where appropriate.

Strict IR coverage
- Add at least one `ir_only = TRUE` test for the new op path.

#### Step 6: Validate locally before moving on

```bash
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_transpile_<feature>.R")'
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_ir_guard_cse.R")'
R CMD INSTALL --preclean packages/mojor
```

If you touched `.Call` bindings, also run a direct smoke check from R for that symbol.

#### Step 7: Keep docs/checklists in sync

For operation breadth work, update at least:
- `./SUBSET.md`
- `packages/mojor/CPU_FEATURES_CHECKLIST.md`
- this cookbook
- package docs where applicable

If the change touches IR contracts/staging, also update:
- `docs/IR/PLAN.md`
- `docs/IR/IMPLEMENTATION.md`
- `docs/IR/CONTRACT.md`
- `docs/IR/SPEC.md`
- `IR_PARITY_CHECKLIST.md`

FAQ: Why can `simd_mode = "auto"` be slower for reductions?

- Auto‑vectorization often picks a conservative reduction strategy.
- Explicit mode allows schedule-backed SIMD reduction where implemented.
- For scalar `min`/`max`, `reduction = "simd"` can be materially faster on large arrays.

### Alignment hints

```r
built <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
built$trans$simd$assume_aligned
```

**Expected:**
- `trans$simd$assume_aligned` is 32.
- Generated Mojo includes an aligned fast path (vector load/store).

### Sum kernels (stdlib vs manual)

**Status: non-exported helper (development checkout only)**

```r
mojor_sum_f64(x)
mojor_sum_f64(x, method = "manual")
```

**Expected:**
- Both return the same numeric result (within floating‑point tolerance).
- The stdlib path is usually fastest; manual exists for regression checks.

### Reduction NA handling

Compiled reductions honor `na_mode`:
- `forbid`: errors on NA/NaN.
- `propagate`: preserves NA propagation for supported expression paths.
- `na_rm`: skips NA values for supported reductions (for example `sum/mean/var/sd` with `na.rm = TRUE`).
- `unsafe`: skips checks (undefined results on NA/NaN).

### Scalar mean reduction

```r
mean_like <- function(x) {
 acc <- 0
 for (i in seq_along(x)) {
 acc <- acc + x[i]
 }
 return(acc / length(x))
}
mojor_fn(mean_like, x = "f64[]", load = FALSE)
```

**Expected:**
- Transpile succeeds for f64.

### Standard deviation

**Status: non-exported helper (development checkout only)**

```r
mojor_sd_f64(c(1, 2, 3))
mojor_var_f64(c(1, 2, 3))
mojor_sd_f32(float::fl(c(1, 2, 3)))
mojor_var_f32(float::fl(c(1, 2, 3)))
```

**Expected:**
- Matches base R on NA‑free inputs.

### f32 precision

**Status: non-exported helper (development checkout only)**

```r
x <- runif(10000, -10, 10)
mojor_sum_f32(x) # float32 accumulation
```

**Expected:**
- f32 reductions may differ slightly from base R (double) due to float32 accumulation.

### Diagnostics for codegen decisions

**Status: non-exported helper (development checkout only)**

```r
mojor_options(warn_parallel = FALSE)
built <- mojor_fn(two_pass, x = "f64[]", load = FALSE)
built$trans$parallel$safe
built$trans$parallel$reason
mojor_diagnostics_report()
```

**Expected:**
- `trans$parallel$safe` is TRUE for simple elementwise loops, FALSE for nested loops.
- Diagnostics report contains any info/warn entries from the run.
---

## Performance Tips

- Prefer large, contiguous numeric vectors (`f64[]`) for best throughput.
- Scalar `min`/`max` can use schedule-backed tree/SIMD via `reduction = "tree"` or `reduction = "simd"`.
- Other scalar reduction forms may still lower to linear code paths.
- Use `assume_aligned` only when you can guarantee alignment (e.g., from aligned allocators).
- `simd_mode = "auto"` is safer for correctness on mixed patterns but less predictable for speed.
- For repeated runs, enable caching (`cache = TRUE`) and reuse compiled kernels.
- Benchmark SIMD for reductions with `packages/mojor/benchmarks/simd_mode_reduction.R`.
- Set optional JIT cache policies to keep disk usage bounded.

### Loop Unrolling

Unroll loops to reduce branch overhead and improve instruction-level parallelism:

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2 + 1
 }
 out
}

# Unroll 4x
built <- mojor_fn(f, x = "f64[]", unroll = 4, load = FALSE)
```

**Notes:**
- Valid range: 1-16 (1 = no unrolling, default)
- Unrolling is applied to `seq_along`, `seq_len`, and `:` loops
- Cleanup loop handles remaining iterations when length is not divisible by unroll factor

### Loop Tiling (Blocking)

Improve cache locality for 2D nested loops (matrix operations):

```r
f <- function(a, n) {
 out <- matrix(0, n, n)
 for (i in seq_len(n)) {
 for (j in seq_len(n)) {
 out[i, j] <- a[i, j] * 2
 }
 }
 out
}

# Tile with 64x64 blocks
built <- mojor_fn(f, a = "f64[]", n = "i32", tile = 64, load = FALSE)

# Rectangular tiles
built <- mojor_fn(f, a = "f64[]", n = "i32", tile = c(32, 128), load = FALSE)
```

**Notes:**
- Only applies to 2D nested loops with matrix output
- Tile size should match L1 cache size (typically 32-64 elements)
- Rectangular tiles useful when matrix dimensions differ significantly

### Bounds Check Elimination

Skip runtime bounds checks when indices are provably safe:

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] # Safe: i is always in bounds
 }
 out
}

# Disable bounds checks for speed
built <- mojor_fn(f, x = "f64[]", bounds_check = FALSE, load = FALSE)
```

Warning: Only disable bounds checks when you're certain all indices are valid. Invalid indices may cause crashes or silent memory corruption.

### Reduction Patterns

Control schedule strategy for supported scalar reductions. Tree/SIMD is
implemented for scalar `min`/`max`; other reduction forms may use linear fallback:

```r
f <- function(x) {
 acc <- 0
 for (i in seq_along(x)) {
 acc <- acc + x[i]
 }
 acc
}

# Options: "auto", "linear", "tree", "simd"
built <- mojor_fn(f, x = "f64[]", reduction = "tree", load = FALSE)
```

- `"linear"`: Sequential accumulation (most predictable)
- `"tree"`: Tree schedule (used for scalar `min`/`max`)
- `"simd"`: SIMD schedule (used for scalar `min`/`max`)
- `"auto"`: Select based on context (default)

Tree schedule example (`min`):

```r
min_tree <- mojor_fn(
 function(x) min(x),
 x = "f64[]",
 reduction = "tree"
)

min_tree(c(3, 1, 4, 1, 5))
is.nan(min_tree(numeric(0))) # TRUE
```

SIMD schedule example (`max`, f32):

```r
if (requireNamespace("float", quietly = TRUE)) {
 max_simd_f32 <- mojor_fn(
 function(x) max(x),
 x = "f32[]",
 reduction = "simd"
 )

 x <- float::fl(c(3, 1, 4, 1, 5))
 as.double(max_simd_f32(x))
}
```

Inspect scheduled transpile markers:

```r
tree_trans <- mojor_transpile(function(x) min(x), x = "f64[]", reduction = "tree")
simd_trans <- mojor_transpile(function(x) min(x), x = "f64[]", reduction = "simd")

tree_code <- paste(tree_trans$mojo, collapse = "\n")
simd_code <- paste(simd_trans$mojo, collapse = "\n")

grepl("# Tree reduction for min\\(x\\)", tree_code)
grepl("# SIMD reduction for min\\(x\\)", simd_code)
grepl("simd_width_of", simd_code)
grepl("load\\[width=simd_width\\]", simd_code)
```

NA-mode example for scheduled reductions:

```r
min_simd_forbid <- mojor_fn(function(x) min(x), x = "f64[]", reduction = "simd", na_mode = "forbid")
min_simd_unsafe <- mojor_fn(function(x) min(x), x = "f64[]", reduction = "simd", na_mode = "unsafe")

# forbid: errors on NA/NaN
try(min_simd_forbid(c(1, NA_real_, 2)))

# unsafe: skips NA checks (result undefined when NA/NaN is present)
min_simd_unsafe(c(1, NA_real_, 2))
```

### Copy-on-Modify Semantics

Control R's copy-on-modify behavior for in-place operations:

```r
f <- function(x) {
 for (i in seq_along(x)) {
 x[i] <- x[i] * 2 # In-place modification
 }
 x
}

# R semantics (default): safe, may duplicate shared objects
built_safe <- mojor_fn(f, x = "f64[]", semantics = "r", load = FALSE)

# Raw semantics: zero overhead, user manages aliasing
built_fast <- mojor_fn(f, x = "f64[]", semantics = "raw", load = FALSE)
```

- `"r"`: Check `MAYBE_SHARED` before modifying inputs (safe)
- `"raw"`: No checks, modify in-place (fast but may violate R semantics)

**Status: non-exported helper (development checkout only)**

```r
mojor_options(
 jit_cache_max_bytes = 2e9,
 jit_cache_max_entries = 50,
 jit_cache_max_age_days = 14
)

# Prune using policy defaults
mojor_cache_prune()

# Formatted cache summary
mojor_cache_print()
```

CLI helper:

```sh
Rscript tools/mojor_cache_cli.R info
Rscript tools/mojor_cache_cli.R info --json
Rscript tools/mojor_cache_cli.R evict --max-bytes 2e9
Rscript tools/mojor_cache_cli.R clear --remove-builds
```

JSON output example:

```sh
Rscript tools/mojor_cache_cli.R info --json | head -40
```

Sample JSON (truncated):

```json
{"path":".../mojor","total_bytes":123456,"jit_total_bytes":0,"jit_policy":{"max_bytes":2000000000,"max_entries":50,"max_age_days":14}}
```

Note: CLI `--json` output requires the `jsonlite` package.

Quick example:

```r
mojor_options(simd_mode = "explicit")
add_r <- function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}
built <- mojor_fn(add_r, x = "f64[]", y = "f64[]", assume_aligned = 32L, load = FALSE)
built$trans$simd$emitted
```

### Shape Helpers (nrow, ncol, dim)

Use `nrow()`, `ncol()`, and `dim()` to extract dimensions from matrix/array inputs:

```r
# Extract number of rows from matrix
f <- function(mat) {
 nr <- nrow(mat)
 out <- numeric(nr)
 for (i in 1:nr) {
 out[i] <- i * 10
 }
 out
}

built <- mojor_fn(f, mat = "f64[]")
built(matrix(1:12, nrow = 3, ncol = 4)) # c(10, 20, 30)

# Extract number of columns
f <- function(mat) {
 nc <- ncol(mat)
 out <- numeric(nc)
 for (i in 1:nc) {
 out[i] <- i * 100
 }
 out
}

built <- mojor_fn(f, mat = "f64[]")
built(matrix(1:12, nrow = 3, ncol = 4)) # c(100, 200, 300, 400)

# Use dim() for any dimension
f <- function(mat) {
 nr <- dim(mat)[1] # Same as nrow(mat)
 nc <- dim(mat)[2] # Same as ncol(mat)
 out <- numeric(nr)
 for (i in 1:nr) {
 out[i] <- nc
 }
 out
}

built <- mojor_fn(f, mat = "f64[]")
built(matrix(1:12, nrow = 3, ncol = 4)) # c(4, 4, 4)
```

**Notes:**
- Shape helpers work with matrix inputs (must have `dim` attribute with at least 2 dimensions)
- The extracted dimensions can be used for loop bounds or stored in scalars
- For 3D+ arrays, `dim(arr)[k]` extracts the k-th dimension (1-indexed)
- `dim(x)[k]` accepts strict scalar-`i32` index lanes (`k` as positive literal or
  supported scalar `i32` expression/argument)
- No-loop metadata queries support `dim(x)`, `dim(x)[k]`, and
  `length(dim(x))` for direct matrix/fixed-rank ND arguments

**Advanced dim() usage:**

```r
# Get number of dimensions
f <- function(mat) {
 ndims <- length(dim(mat)) # Compile-time constant 2 for matrices
 out <- numeric(1)
 out[1] <- ndims
 out
}

built <- mojor_fn(f, mat = "f64[,]")
built(matrix(1:12, 3, 4)) # 2
```

**How it works:** `dim(x)[i]` patterns are lowered at compile time to direct dimension variable references:
- `dim(mat)[1]` → `nrow_mat` (zero overhead)
- `dim(mat)[2]` → `ncol_mat` (zero overhead)
- `length(dim(mat))` → constant `2` (zero overhead)

This provides dimension access without any runtime allocation.

---

## GPU pipeline (explicit)

**Goal:** opt-in GPU kernels for elementwise chains that keep data on GPU.
Canonical support notation for these surfaces is in
`docs/GPU_FUNCTION_SUPPORT_MATRIX.md`.

**Status: non-exported helper (development checkout only)**

```r
library(float)

x <- fl(runif(1e6))

# Single pass (sigmoid)
y0 <- mojor_sigmoid_f32_gpu(x)

# Fused sigmoid + affine (scale/bias)
y1 <- mojor_sigmoid_affine_f32_gpu(x, scale = 1.1, bias = 0.05)

# Multi-pass chain (stays on GPU for N iterations)
y2 <- mojor_gpu_chain(x, iters = 10, op = "sigmoid", scale = 1.1, bias = 0.05)

attr(y2, "gpu_status")
```

### GPUArray object model (R6-style)

MöjoR GPU arrays are exposed as R6-style mutable objects (class `mojor_gpu_array`).
Use explicit read/write/free operations when you want deterministic lifecycle control.

**Status: non-exported helper (development checkout only)**

```r
ga <- mojor_gpu_array(matrix(runif(12), nrow = 3), dtype = "f32")

# Generic helpers
show(ga)
length(ga)
dim(ga)

# Mutating + host roundtrip
mojor_gpu_array_write(ga, runif(12))
host <- as.numeric(ga) # equivalent to mojor_gpu_array_read(ga)

# Explicit release
ga <- mojor_gpu_array_free(ga)
```

**Notes:**
- `mojor_gpu_array_read()` / `as.numeric()` transfer data back to host memory.
- Keep arrays on GPU across chained calls (`mojor_gpu_linear()`, `mojor_gpu_chain_array()`) to avoid host-device traffic.
- Call `mojor_gpu_array_free()` for predictable buffer release in long-running sessions.

### GPU reductions, matmul, and sessions

**Status: non-exported helper (development checkout only)**

```r
if (mojor_has_gpu()) {
 A <- mojor_gpu_array(matrix(runif(12), nrow = 3), dtype = "f32")
 B <- mojor_gpu_array(matrix(runif(8), nrow = 4), dtype = "f32")

 # Matrix multiplication
 C <- gpu_matmul(A, B)
 host_C <- mojor_gpu_array_read(C)

 # Dimension-aware reduction
 col_sum <- gpu_reduce(C, op = "sum", dims = 1, keepdims = TRUE)
 host_col_sum <- mojor_gpu_array_read(col_sum)

 # Stateful session workflow
 s <- mojor_gpu_session(runif(1024))
 s <- mojor_gpu_session_run(s, iters = 3, scale = 1.1, bias = 0.05)
 total <- mojor_gpu_session_sum(s)

 # Cleanup
 mojor_gpu_session_free(s)
 mojor_gpu_array_free(col_sum)
 mojor_gpu_array_free(C)
 mojor_gpu_array_free(B)
mojor_gpu_array_free(A)
}
```

macOS Metal note:
- f64 linalg/reduce routes are intentionally explicit CPU fallback on Mac Studio (`cpu_matmul`, `cpu_reduce`) until backend f64 GPU support is available.

### Matrix elementwise GPU kernels (linearized + canonical 2D)

For matrix-shaped elementwise kernels, you can use either:
- Linearized indexing (`seq_along` + `out[i]`) for deterministic column-major behavior.
- Canonical 2D indexed loops (`seq_len(nrow(X))` + `seq_len(ncol(X))` with `out[i, j] <- ...`).
- Guarded `if` / `if-else` assignment in the inner body.
- Guarded neighbor reads (`x[i + di, j + dj]`) with integer-literal offsets.

**Status: non-exported helper (development checkout only)**

```r
f_mat <- function(x, y) {
 out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
 for (i in seq_along(x)) out[i] <- x[i] + y[i]
 out
}

k_mat <- mojor_gpu_kernel(
 f_mat,
 x = "f64[,]",
 y = "f64[,]",
 name = "gpu_mat_add_linearized"
)
```

Canonical 2D indexed form:

```r
f_mat2d <- function(x, y, b) {
 out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
 for (i in seq_len(nrow(x))) {
 for (j in seq_len(ncol(x))) {
 out[i, j] <- x[i, j] + y[i, j] + b
 }
 }
 out
}

k_mat2d <- mojor_gpu_kernel(
 f_mat2d,
 x = "f32[,]",
 y = "f32[,]",
 b = "f32",
 name = "gpu_mat_add_indexed"
)
```

Guarded neighbor + scalar form:

```r
f_mat2d_guard <- function(x, b) {
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

k_mat2d_guard <- mojor_gpu_kernel(
 f_mat2d_guard,
 x = "f32[,]",
 b = "f32",
 name = "gpu_mat_guard_neighbor"
)
```

Unsupported matrix2d forms remain explicit in this section:
- irregular indexing (`x[j, i]`, mixed index pairs),
- neighbor reads without explicit in-bounds guards,
- guard predicates that are not reducible to canonical in-bounds checks
  (beyond supported conjunction / negated-atom / de-morgan bounds forms),
- reductions inside the kernel body,
- non-`seq_len` loop ranges,
- mixed dtype arithmetic without explicit cast.

---

## Random Number Generation (RNG)

**Status: non-exported helper (development checkout only)**

### Basic RNG Functions

MöjoR provides fast random number generation:

```r
# Seed for reproducibility
mojor_rng_seed(42)

# Uniform random numbers U(0, 1)
u <- mojor_runif(1000)

# Uniform random numbers U(min, max)
u <- mojor_runif(1000, min = 10, max = 20)

# Standard normal N(0, 1)
z <- mojor_rnorm(1000)

# Normal with custom mean and sd
z <- mojor_rnorm(1000, mean = 50, sd = 10)
```

Wrapper breadth parity (development helper surface), aligned with
`docs/IR/FUNCTION_SUPPORT_BREADTH.md` RNG contracts (`L3`):

| Wrapper | Level | Params (`min..max`) excluding `n` |
|---|---|---|
| `mojor_runif` | `L3` | `0..2` |
| `mojor_rnorm` | `L3` | `0..2` |
| `mojor_rgamma` | `L3` | `1..2` |
| `mojor_rbinom` | `L3` | `2..2` |
| `mojor_rexp` | `L3` | `0..1` |
| `mojor_rpois` | `L3` | `1..1` |
| `mojor_rlnorm` | `L3` | `0..2` |
| `mojor_rchisq` | `L3` | `1..1` |
| `mojor_rt` | `L3` | `1..1` |
| `mojor_rf` | `L3` | `2..2` |
| `mojor_rbeta` | `L3` | `2..2` |
| `mojor_rweibull` | `L3` | `1..2` |
| `mojor_rlogis` | `L3` | `0..2` |
| `mojor_rcauchy` | `L3` | `0..2` |
| `mojor_rgeom` | `L3` | `1..1` |
| `mojor_rnbinom` | `L3` | `2..2` |
| `mojor_rhyper` | `L3` | `3..3` |
| `mojor_rsignrank` | `L3` | `1..1` |
| `mojor_rwilcox` | `L3` | `2..2` |

RNG semantics in compiled kernels:

**Status: non-exported helper (development checkout only)**

- `mojor_rng_seed()` controls the RNG stream used by compiled MöjoR kernels.
- Base R `set.seed()` does not seed that kernel RNG stream.
- For deterministic sampling tests, call `mojor_rng_seed()` immediately before compiled kernel calls.

 sampling lane note:
- Strict IR verify/emit accepts `prob = NULL` or direct `f64[]` `prob` and accepts scalar boolean/int `replace`.
- Strict no-loop specialization supports weighted sampling when `prob` is a direct `f64[]` argument and `replace` is a strict scalar/literal flag.
- Statement emit supports weighted `prob` for whole-vector assignment forms with `replace = TRUE/FALSE` (or scalar `replace` flags), and indexed-dynamic assignment forms when `replace = TRUE`.
- Weighted sampling still requires `prob` as a direct `f64[]` variable in typed paths.

Implementation note:
- RNG helpers live in `rng_helpers.mojo` and are imported by generated kernels (copied into the build dir when RNG is used). This keeps kernels small and the RNG logic consistent.

### RNG in Loops

MöjoR supports using RNG functions (`rnorm`, `rgamma`, `runif`) inside transpiled loops. These are translated to fast Mojo implementations.

### RNG in Loops Example

This example shows how to use RNG functions inside transpiled loops:

```r
# Simple RNG example with runif in a loop
simple_rng <- function(n_dummy) {
 out <- numeric(length(n_dummy))
 for (i in seq_along(n_dummy)) {
 out[i] <- runif(1, 0, 1)
 }
 out
}

# Build the compiled version
simple_rng_fast <- mojor_fn(simple_rng, n_dummy = "f64[]")

# Test
result <- simple_rng_fast(c(1, 2, 3, 4, 5))
print(result) # Random values in [0, 1]
```

### Gibbs Sampler Example

A more complex example using both `rgamma()` and `rnorm()` in nested loops:

```r
# Gibbs sampler for x samples
# Note: The transpiler requires at least one array argument to determine output size.
rgibbs_x <- function(dummy) {
 x_out <- numeric(length(dummy))
 x <- 0.0
 y <- 0.0
 for (i in seq_along(dummy)) {
 for (j in 1:10) { # Fixed thinning parameter
 x <- rgamma(1, 3, y * y + 4)
 y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
 }
 x_out[i] <- x
 }
 x_out
}

# Build the compiled version
rgibbs_fast <- mojor_fn(rgibbs_x, dummy = "f64[]")

# Generate samples
samples <- rgibbs_fast(numeric(1000))
hist(samples, breaks = 30, main = "Gibbs Sampler Output (Mojo)")
```

### Supported RNG Functions in Loops

| Function | Example | Notes |
|----------|---------|-------|
| `rnorm()` | `rnorm(1, mean = 0, sd = 1)` | Standard or custom mean/sd |
| `rgamma()` | `rgamma(1, shape, rate)` | Shape >= 1 optimized |
| `runif()` | `runif(1, min = 0, max = 1)` | Uniform random numbers |

**Important**: Only `n = 1` is supported inside loops. For vectorized generation, use the `mojor_*` functions:

**Status: non-exported helper (development checkout only)**

```r
# Vectorized approach (outside loops)
mojor_rng_seed(42)
u <- mojor_runif(1000) # 1000 uniform random numbers
z <- mojor_rnorm(1000) # 1000 normal random numbers
g <- mojor_rgamma(1000, shape = 3, rate = 1) # 1000 gamma random numbers
```

## Count/Histogram Reductions

### Built-in BinCount (Recommended)

**Status: non-exported helper (development checkout only)**

Use `mojor_bincount()` for fast histogram binning:

```r
x <- runif(1000)
breaks <- seq(0, 1, by = 0.1)
counts <- mojor_bincount(x, breaks, right = FALSE, include.lowest = TRUE)
```

This calls an optimized Mojo implementation equivalent to R's internal C_BinCount.

### Custom Histogram with Transpiler

For custom binning logic:

```r
f <- function(x, bins) {
 counts <- integer(length(bins))
 for (i in seq_along(x)) {
 for (j in seq_along(bins)) {
 if (x[i] <= bins[j]) {
 counts[j] <- counts[j] + 1L
 break
 }
 }
 }
 counts
}

hist_fn <- mojor_fn(f, x = "f64[]", bins = "f64[]")
hist_fn(c(1, 5, 10, 15, 20), c(5, 10, 15))
# Returns: c(2, 1, 1)
```

The transpiler detects `integer(length(bins))` and uses the correct output length for the C wrapper.

## Advanced Indexing Patterns

### Index Arithmetic (Scalar Offsets and Scaling)

Index expressions support arithmetic beyond simple `i +/- literal`:

**Scalar variable offsets:**
```r
f <- function(x, offset) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i + offset] # offset is a scalar argument
 }
 out
}

mojor_fn(f, x = "f64[]", offset = "i32")
```

**Multiplication/division by scalars:**
```r
# Extract every other element
even_elements <- function(x, n) {
 out <- numeric(n)
 for (i in seq_len(n)) {
 out[i] <- x[i * 2] # Read from positions 2, 4, 6, ...
 }
 out
}

mojor_fn(even_elements, x = "f64[]", n = "i32")
```

**Supported patterns:**
- `x[i + offset]`, `x[i - offset]` where offset is scalar argument
- `x[offset + i]`, `x[offset - i]` (scalar first)
- `x[i * k]`, `x[i / k]` where k is integer literal

**Not supported:**
- Nested arithmetic: `x[(i + 1) * 2]`
- Array offsets: `x[i + y[j]]` where y is an array

### drop = FALSE Parameter

The `drop = FALSE` parameter in matrix indexing is parsed and accepted (for compatibility with R), but has no effect for scalar element access:

```r
f <- function(a, n) {
 out <- matrix(0, n, n)
 for (i in seq_len(n)) {
 for (j in seq_len(n)) {
 out[i, j] <- a[i, j, drop = FALSE] # drop=FALSE is parsed but ignored
 }
 }
 out
}

mojor_fn(f, a = "f64[]", n = "i32", load = FALSE)
```

### Mixed Indices (Vector Subsetting)

Subset rows or columns using vector indices passed as arguments:

```r
# Row subsetting
f <- function(x, rows, nr, nc) {
 out <- matrix(0, nr, nc)
 for (i in seq_along(rows)) {
 for (j in seq_len(nc)) {
 out[i, j] <- x[rows[i], j] # Access specific rows
 }
 }
 out
}

subset_rows <- mojor_fn(f, x = "f64[]", rows = "i32[]", nr = "i32", nc = "i32")

# Use with specific row indices
x <- matrix(1:20, nrow = 5, ncol = 4)
result <- subset_rows(x, c(1L, 3L, 5L), 3L, 4L) # Extract rows 1, 3, 5
```

### Character Indexing with dimnames

Use character names for matrix/array indexing when dimnames are specified:

```r
f <- function(x, n) {
 out <- matrix(0.0, n, n, dimnames = list(c("a", "b"), c("c", "d")))
 for (i in seq_len(n)) {
 for (j in seq_len(n)) {
 if (i == 1 && j == 1) {
 out["a", "c"] <- 999.0 # Character indexing (compile-time resolved)
 } else {
 out[i, j] <- x[i, j]
 }
 }
 }
 out # Returns matrix with dimnames preserved
}

f_fast <- mojor_fn(f, x = "f64[]", n = "i32")
result <- f_fast(matrix(1:4, 2, 2), 2L)
dimnames(result) # [[1]] "a" "b" [[2]] "c" "d"
result["a", "c"] # 999
```

Direct `chr[]` function arguments can also drive strict character-index routing:

```r
f <- function(x, row_keys, col_keys) {
 out <- matrix(0.0, 2, 2, dimnames = list(c("a", "b"), c("c", "d")))
 out[row_keys[1], col_keys[1]] <- x[1, 1]
 out[row_keys[2], col_keys[2]] <- x[2, 2]
 out
}

f_fast <- mojor_fn(f, x = "f64[]", row_keys = "chr[]", col_keys = "chr[]")
f_fast(matrix(as.double(1:4), 2, 2), c("a", "b"), c("c", "d"))
```

Scalar character indexing also works for higher-rank arrays when dimnames are
provided for the indexed dimensions:

```r
f <- function(x) {
 arr <- array(
  0.0,
  dim = c(2, 2, 2),
  dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
 )
 for (i in 1:2) {
 for (j in 1:2) {
 for (k in 1:2) {
 arr[i, j, k] <- x[i, j, k]
 }
 }
 }
 arr["r1", "c2", "k1"] <- arr["r2", "c1", "k2"]
 arr
}
```

Missing-index forms are supported on covered dimname axes:

```r
f <- function() {
 arr <- array(
  0.0,
  dim = c(2, 2, 2),
  dimnames = list(c("r1", "r2"), c("c1", "c2"), c("k1", "k2"))
 )
 arr[, "c2", "k1"] <- c(10.0, 20.0) # missing first index
 arr["r1", , "k2"] <- c(30.0, 40.0) # missing middle index
 arr
}
```

### Negative Index Error

Negative indices raise an error at transpile time (not R-style exclusion):

```r
f <- function(x, n) {
 out <- numeric(n)
 for (i in 1:n) {
 out[i] <- x[-1] # Error: negative indices are not supported (-1)
 }
 out
}

mojor_fn(f, x = "f64[]", n = "i32", load = FALSE)
# Error: negative indices are not supported (-1)
```

Use explicit positive indices or vector subsetting patterns instead.

### 3D and Higher-Dimensional Arrays

MöjoR supports N-dimensional arrays with full indexing and slice assignments:

**3D Array Creation and Full Indexing:**

```r
f <- function(x, d1, d2, d3) {
 arr <- array(0, dim = c(d1, d2, d3))
 for (i in 1:d1) {
 for (j in 1:d2) {
 for (k in 1:d3) {
 arr[i, j, k] <- x[i] + x[j] + x[k]
 }
 }
 }
 arr
}

built <- mojor_fn(f, x = "f64[]", d1 = "i32", d2 = "i32", d3 = "i32")
result <- built(array(1:10), 2L, 2L, 2L) # 2x2x2 array
```

**4D Arrays:**

```r
f <- function(d1, d2, d3, d4) {
 arr <- array(0, dim = c(d1, d2, d3, d4))
 for (i in 1:d1) {
 for (j in 1:d2) {
 for (k in 1:d3) {
 for (l in 1:d4) {
 arr[i, j, k, l] <- i*1000 + j*100 + k*10 + l
 }
 }
 }
 }
 arr
}

built <- mojor_fn(f, d1 = "i32", d2 = "i32", d3 = "i32", d4 = "i32")
```

**3D Slice Assignments:**

Slice assignments work for any dimension using `c()` to provide values in column-major order:

```r
# Assign to k-th "slice" (last dimension)
f <- function(x, n) {
 arr <- array(0, dim = c(2, 2, n))
 for (k in seq_len(n)) {
 arr[, , k] <- c(x[k], x[k]*2, x[k]*3, x[k]*4)
 }
 arr
}

# Assign to j-th "column" (middle dimension)
f <- function(x, n) {
 arr <- array(0, dim = c(2, n, 2))
 for (j in seq_len(n)) {
 arr[, j, ] <- c(x[j], x[j]*2, x[j]*3, x[j]*4)
 }
 arr
}

# Assign to i-th "row" (first dimension)
f <- function(x, n) {
 arr <- array(0, dim = c(n, 2, 2))
 for (i in seq_len(n)) {
 arr[i, , ] <- c(x[i], x[i]*2, x[i]*3, x[i]*4)
 }
 arr
}
```

**Important:** The `c()` constructor fills values in **column-major order** (R's native layout). For a 2x2 slice:
- `c(1, 2, 3, 4)` fills as `matrix(c(1, 2, 3, 4), 2, 2)` which is `[1, 3; 2, 4]` (row 1, col 1; row 2, col 1; row 1, col 2; row 2, col 2)

**Reading from 3D Input Arrays:**

```r
# Extract diagonal elements from 3D input
f <- function(arr3d) {
 n <- dim(arr3d)[1]
 out <- numeric(n)
 for (i in 1:n) {
 out[i] <- arr3d[i, i, i]
 }
 out
}

input_arr <- array(1:27, dim = c(3, 3, 3))
built <- mojor_fn(f, arr3d = "f64[]")
result <- built(input_arr) # c(1, 14, 27)
```

**Limitations:**
- Output length must match input length for some patterns (workaround: use full-length output vectors)
- `seq_len(dim(arr)[k])` is supported when `k` is compile-time constant.
- Full vector materialization (`d <- dim(arr)`) is supported for direct-array metadata forms; non-subset helper-composed metadata expressions remain out of scope.

---

## Type Predicates and Metadata Queries (Current)

### Type Predicates with Compile-Time Folding

Type predicates on function parameters are folded to constants at compile time, enabling dead code elimination:

```r
# Conditional behavior based on type
f <- function(x, out) {
 n <- length(x)
 for (i in 1:n) {
 if (is.vector(x)) {
 out[i] <- x[i] * 2.0 # Vector path
 } else {
 out[i] <- x[i] * 3.0 # Matrix path (eliminated at compile time)
 }
 }
 out
}

# When x is annotated as f64[], is.vector(x) folds to TRUE
built <- mojor_fn(f, x = "f64[]", out = "f64[]")
# Generated code contains only: out[i] <- x[i] * 2.0
```

**Supported type predicates:**
- `is.vector(x)`, `is.matrix(x)`, `is.array(x)`
- `is.numeric(x)`, `is.integer(x)`, `is.double(x)`, `is.logical(x)`

**How it works:**
- Type annotations (`f64[]`, `i32[,]`, etc.) are parsed at compile time
- `is.vector(x)` → folded to `TRUE` when `x = "f64[]"`
- `is.matrix(x)` → folded to `TRUE` when `x = "f64[,]"`
- Dead branches are eliminated (never compiled into Mojo)

### Type Queries

Type queries return string constants at compile time:

```r
f <- function(x, out) {
 # These fold to constants based on x's type annotation
 type_name <- typeof(x) # "double" for f64[], "integer" for i32[]
 mode_name <- mode(x) # "numeric" for f64[]/i32[], "logical" for logical[]
 class_name <- class(x) # "numeric" for f64[], "matrix" for f64[,]
 # Use in computations (though not very useful in practice)
 out[1] <- if (typeof(x) == "double") 1.0 else 0.0
 out
}

built <- mojor_fn(f, x = "f64[]", out = "f64[]")
```

**Supported type queries:**
- `typeof(x)` → `"double"`, `"integer"`, or `"logical"`
- `mode(x)` → `"numeric"` or `"logical"`
- `class(x)` → `"numeric"`, `"integer"`, `"logical"`, `"matrix"`, or `"array"`

**Limitations:**
- Only works on function parameters (not local variables or expressions)
- Only works in standard loop contexts (scalar no-loop specialization still has gaps)
- Type must be known at compile time (requires type annotation)

### Practical Use Case: Polymorphic Functions

```r
# Same function works for both vectors and matrices
f <- function(x, out) {
 if (is.vector(x)) {
 # Vector: simple element-wise multiplication
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2.0
 }
 } else {
 # Matrix: multiply by row index
 nr <- nrow(x)
 nc <- ncol(x)
 for (i in 1:nr) {
 for (j in 1:nc) {
 out[(i-1)*nc + j] <- x[i,j] * i
 }
 }
 }
 out
}

# Compile for vectors - matrix branch is eliminated
built_vec <- mojor_fn(f, x = "f64[]", out = "f64[]")

# Compile for matrices - vector branch is eliminated
built_mat <- mojor_fn(f, x = "f64[,]", out = "f64[]")
```

**Value:** Write one function, compile specialized versions for different types with zero runtime overhead.

---

## Foreign Function Interface (FFI)

Use FFI to call existing C libraries from MöjoR kernels or export MöjoR kernels as C functions.

**Status: non-exported helper (development checkout only)**

### Calling C Functions from MöjoR Kernels

```r
# Declare a C function from an external library
mojor_declare_c(
 name = "my_c_func",
 args = list(x = "f64*", n = "i32"),
 returns = "f64",
 library = "libmylib.so"
)

# Use the C function in a MöjoR kernel
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- mojor_c_call("my_c_func", x[i], length(x))
 }
 out
}

# Compile the kernel
fast_f <- mojor_fn(f, x = "f64[]")
```

**Notes:**
- **Type signatures must match** between R declaration and C function
- **Supported types:** `f64`, `f64[]`, `f32`, `f32[]`, `i32`, `i32[]`, `lgl`, `lgl[]`
- **Library path** is optional if the library is already in the system path
- **ABI checks are strict:** `mojor_c_call(...)` enforces declared arity and per-argument type compatibility (`argument count mismatch` / `argument type mismatch`).
- **Declaration parity is enforced:** verifier/emitter validate `returns`, `arg_types`, and `arg_names` metadata against the declaration.
- In strict `ir_only = TRUE` verification, `mojor_c_call` argument types must be statically known.

### Exporting MöjoR Kernels as C Functions

**Status: non-exported helper (development checkout only)**

```r
# Compile a kernel
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2.0
 }
 out
}
built <- mojor_build(f, x = "f64[]")

# Export as C header
mojor_export_c_header(built, "my_kernel.h")
```

This creates a header file with:
```c
// my_kernel.h
#ifndef MOJOR_MY_KERNEL_H
#define MOJOR_MY_KERNEL_H

#include <stdint.h>

double* mojor_my_kernel(double* x);

#endif /* MOJOR_MY_KERNEL_H */
```

**Notes:**
- **ABI-stable signatures** - use fixed-width types (`int32_t`, `double`)
- **Namespace support** - use `namespace` parameter to prefix function names
- **Type mapping:**
 - `f64` → `double`
 - `f64[]` → `double*`
 - `i32` → `int32_t`
 - `i32[]` → `int32_t*`
 - `lgl` → `int32_t`
 - `lgl[]` → `int32_t*`

### Complete Example: Using FFTW

**Status: non-exported helper (development checkout only)**

```r
# Declare FFTW functions
mojor_declare_c(
 name = "fftw_plan_dft_1d",
 args = list(
 rank = "i32",
 n = "i32*",
 `in` = "f64*",
 out = "f64*",
 sign = "i32",
 flags = "i32"
 ),
 returns = "void*",
 library = "libfftw3.so"
)

mojor_declare_c(
 name = "fftw_execute",
 args = list(plan = "void*"),
 returns = "void",
 library = "libfftw3.so"
)

mojor_declare_c(
 name = "fftw_destroy_plan",
 args = list(plan = "void*"),
 returns = "void",
 library = "libfftw3.so"
)

# Use FFTW in a MöjoR kernel
f <- function(x) {
 n <- length(x)
 out <- numeric(n)
 # Create plan
 plan <- mojor_c_call("fftw_plan_dft_1d", 1, n, x, out, -1, 1)
 # Execute
 mojor_c_call("fftw_execute", plan)
 # Clean up
 mojor_c_call("fftw_destroy_plan", plan)
 out
}

# Compile
fast_fft <- mojor_fn(f, x = "f64[]")
```

**Value:** Leverage existing C libraries while keeping your hot loops in MöjoR.

### Apply Family (Current)

`apply()` row/column reductions on matrices are supported in the current strict subset.

| Form | Status |
|------|--------|
| `apply(X, 1, FUN)` | Supported |
| `apply(X, 2, FUN)` | Supported |
| `FUN = sum/mean/min/max` | Supported |
| `FUN = function(v) sum/mean/min/max(v, na.rm=...)` | Supported (strict inline form) |

Current constraints:
- Matrix input only (`f64[,]`, `f32[,]`, `i32[,]`).
- Inline `FUN` must be a single-argument lambda with a single-expression body (or `{ return(...) }`).
- `na.rm` must be a literal scalar logical; for inline lambda use it inside the reduction call.
- Supported return styles include direct and alias forms:
 - `apply(A, 1, sum)`
 - `tmp <- apply(A, 1, sum); tmp`
 - `return(apply(A, 1, sum))`

#### Examples

```r
# Row-wise sum with symbol FUN
f_apply_row <- mojor_fn(
 function(A, nr) {
 tmp <- apply(A, 1, sum)
 out <- numeric(nr)
 for (i in 1:nr) out[i] <- tmp[i]
 out
 },
 A = "f64[,]", nr = "i32", object_mode = "off"
)
```

```r
# Row-wise sum with strict inline lambda FUN + internal na.rm
f_apply_row_na <- mojor_fn(
 function(A, nr) {
 tmp <- apply(A, 1, function(v) sum(v, na.rm = TRUE))
 out <- numeric(nr)
 for (i in 1:nr) out[i] <- tmp[i]
 out
 },
 A = "f64[,]", nr = "i32", object_mode = "off"
)
```

```r
# Direct-return no-loop form
f_apply_direct <- mojor_fn(
 function(A) apply(A, 2, mean),
 A = "f64[,]", object_mode = "off"
)
```

### Set/Match Primitives (Current)

These compile on the strict no-loop specialization path with `object_mode = "off"`.
Compatibility preview fallback remains only for unsupported forms.

| Function | Description |
|----------|-------------|
| `unique(x)` | Returns unique values from a vector |
| `duplicated(x)` | Returns logical vector indicating duplicated values |
| `anyDuplicated(x)` | Returns index of first duplicate or 0 if none |
| `match(x, table)` | Returns positions of first matches |
| `%in%(x, table)` | Returns logical vector indicating matches |

#### Examples

```r
# Get unique values
f_unique <- mojor_fn(function(x) unique(x), x = "f64[]", object_mode = "off")
f_unique(c(1, 2, 2, 3, 3, 3)) # Returns c(1, 2, 3)

# Check for duplicates
f_dup <- mojor_fn(function(x) duplicated(x), x = "f64[]", object_mode = "off")
f_dup(c(1, 2, 2, 3, 3, 3)) # Returns c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)

# Find first duplicate
f_any_dup <- mojor_fn(function(x) anyDuplicated(x), x = "f64[]", object_mode = "off")
f_any_dup(c(1, 2, 2, 3, 3, 3)) # Returns 3 (index of first duplicate)

# Match values
f_match <- mojor_fn(function(x, table) match(x, table), x = "f64[]", table = "f64[]", object_mode = "off")
f_match(c(2, 4, 6), c(1, 2, 3, 4, 5)) # Returns c(2, 4, NA)

# %in% operator
f_in <- mojor_fn(function(x, table) x %in% table, x = "f64[]", table = "f64[]", object_mode = "off")
f_in(c(2, 4, 6), c(1, 2, 3, 4, 5)) # Returns c(TRUE, TRUE, FALSE)
```

### Quantiles & Robust Stats (Current)

These compile on the strict no-loop specialization path with `object_mode = "off"`.
Compatibility preview fallback remains only for unsupported forms.

| Function | Description |
|----------|-------------|
| `median(x)` | Returns median value |
| `quantile(x, probs)` | Returns quantile values (`probs` parameter `f64[]` or numeric literal `c(...)`) |
| `IQR(x)` | Returns interquartile range (Q3 - Q1) |
| `mad(x)` | Returns median absolute deviation |

#### Examples

```r
# Median
f_median <- mojor_fn(function(x) median(x), x = "f64[]", object_mode = "off")
f_median(c(1, 2, 3, 4, 5)) # Returns 3

# Quantiles
f_quantile <- mojor_fn(function(x) quantile(x, c(0.25, 0.5, 0.75)), x = "f64[]", object_mode = "off")
f_quantile(1:10) # Returns c(2.75, 5.5, 8.25)

# Quantiles with dynamic probs parameter
f_quantile_p <- mojor_fn(function(x, p) quantile(x, p), x = "f64[]", p = "f64[]", object_mode = "off")
f_quantile_p(1:10, c(0.1, 0.9)) # Returns c(1.9, 9.1)

# IQR
f_iqr <- mojor_fn(function(x) IQR(x), x = "f64[]", object_mode = "off")
f_iqr(1:10) # Returns 5.5 (Q3 - Q1 = 8.25 - 2.75)

# MAD
f_mad <- mojor_fn(function(x) mad(x), x = "f64[]", object_mode = "off")
f_mad(c(1, 2, 3, 4, 5)) # Returns ~1.4826 (median absolute deviation)
```

### Higher-Order Functions (Current)

#### Works Today

These run today with `object_mode = "off"` on the strict compiled path.
You may still see a warning when preview rewrite fallback is used for unsupported forms. In strict mode (`ir_only = TRUE`), unexpected rewrite fallback is rejected with an explicit diagnostic.

| Function | Description |
|----------|-------------|
| `vapply(X, FUN, FUN.VALUE)` | Apply function to each element with known return type |
| `sapply(X, FUN)` | Apply function to each element, simplify to vector |
| `lapply(X, FUN)` | Apply function to each element (returns vector for scalar results) |
| `mapply(FUN, ...)` | Multivariate apply for same-length vectors (arity >= 2) |

Current compiled behavior:
- `vapply`/`sapply`/`lapply`/`mapply` compile for the strict subset (`mojor_fn`, `object_mode = "off"`).
- Required subset: single-expression scalar-return `FUN`, direct vector args, and `mapply` arity >= 2.
- `mapply` also supports curated named `FUN` (`pmin`, `pmax`, `min`, `max`) under the same direct-arg constraints.
- Preview rewrite fallback is policy-guarded and should only appear for unsupported forms.

#### Examples (Current)

```r
# vapply with known return type
f_vapply <- mojor_fn(function(x) vapply(x, function(v) v * 2, FUN.VALUE = numeric(1)), x = "f64[]", object_mode = "off")
f_vapply(c(1, 2, 3)) # Returns c(2, 4, 6)

# sapply for vector simplification
f_sapply <- mojor_fn(function(x) sapply(x, function(v) v + 1), x = "f64[]", object_mode = "off")
f_sapply(c(1, 2, 3)) # Returns c(2, 3, 4)

# lapply strict subset (returns vector in v1)
f_lapply <- mojor_fn(function(x) lapply(x, function(v) v - 1), x = "f64[]", object_mode = "off")
f_lapply(c(1, 2, 3)) # Returns c(0, 1, 2)

# mapply strict subset (direct vectors, arity >= 2)
f_mapply <- mojor_fn(function(x, y) mapply(function(a, b) a + b, x, y), x = "f64[]", y = "f64[]", object_mode = "off")
f_mapply(c(1, 2, 3), c(4, 5, 6)) # Returns c(5, 7, 9)

# mapply curated named FUN subset
f_mapply_named <- mojor_fn(function(x, y) mapply(FUN = pmax, x, y), x = "f64[]", y = "f64[]", object_mode = "off")
f_mapply_named(c(1, 5, 3), c(2, 4, 6)) # Returns c(2, 5, 6)
```

#### Planned

- Broaden beyond the current subset (full named-arg coverage and richer function forms).
- Add deeper codegen/IR golden coverage for kernels.
- Keep stable diagnostics for unsupported forms (closure captures, arity mismatch, non-vector args, list-return expectations).

### String Basics (Current)

#### Works Today (Strict Wrapper Subset)

These run today with `mojor_fn(..., object_mode = "off")` on the strict no-loop wrapper specialization path. This path is intentionally narrow and emits stable diagnostics for unsupported forms.

Current wrapper subset:
- `nchar(x)` / `nzchar(x)` with direct `chr` or `chr[]` argument.
- `substr(x, start, stop)` / `substring(x, first, last)` with direct `chr` or `chr[]` and scalar integer/literal bounds.
- `paste(x, y, sep = "<literal>", collapse = "<literal>"|NULL)` / `paste0(x, y, collapse = "<literal>"|NULL)` with direct `chr`/`chr[]` args.
- Unsupported forms (for example non-direct args, non-literal `sep`/`collapse`, or >2 concat inputs) fail explicitly.

Preview-style guard policy remains in effect: strict flows (`ir_only = TRUE`) must not silently degrade into unsupported non-strict fallback behavior.

#### Examples (Current)

```r
f_nchar <- mojor_fn(function(x) nchar(x), x = "chr[]", object_mode = "off")
f_nchar(c("a", "bc", "")) # Returns c(1, 2, 0)

f_nchar_scalar <- mojor_fn(function(x) nchar(x), x = "chr", object_mode = "off")
f_nchar_scalar("mojor") # Returns 5L

f_substr <- mojor_fn(function(x) substr(x, 1L, 2L), x = "chr[]", object_mode = "off")
f_substr(c("alpha", "be")) # Returns c("al", "be")

f_substr_scalar <- mojor_fn(function(x, s, e) substr(x, s, e), x = "chr", s = "i32", e = "i32", object_mode = "off")
f_substr_scalar("alpha", 2L, 4L) # Returns "lph"

f_paste <- mojor_fn(function(x, y) paste(x, y, sep = "-"), x = "chr[]", y = "chr[]", object_mode = "off")
f_paste(c("a", "b"), c("1", "2")) # Returns c("a-1", "b-2")

f_paste_mixed <- mojor_fn(function(x, y) paste(x, y, sep = "-"), x = "chr", y = "chr[]", object_mode = "off")
f_paste_mixed("row", c("1", "2")) # Returns c("row-1", "row-2")

f_paste0 <- mojor_fn(function(x, y) paste0(x, y), x = "chr[]", y = "chr[]", object_mode = "off")
f_paste0(c("a", "b"), c("1", "2")) # Returns c("a1", "b2")
```

#### Current Status: Not runnable in compiled string kernels yet

The runtime does not have a native compiled string type/ABI path yet. Character scalar/vector string semantics (`chr`/`chr[]`) run through the strict wrapper subset above, not through IR-native compiled string kernels.

#### Planned API subset

| Function | Description |
|----------|-------------|
| `nchar(x)` | Number of characters in string(s) |
| `nzchar(x)` | Non-zero length strings (returns logical) |
| `substr(x, start, stop)` / `substring(x, first, last)` | Extract substring |
| `paste(x, y, ..., sep = "", collapse = NULL)` | String concatenation |
| `paste0(x, y, ..., collapse = NULL)` | Paste with empty separator |

Planned constraints:
- UTF-8 byte-wise behavior in v1 (no locale/regex support).
- `collapse` support deferred.

#### Planned Pseudocode (IR-native Kernels; Not Runnable Yet)

```r
# Planned IR-native examples only (not runnable in compiled string kernels yet):
f_nchar <- mojor_fn(function(x) nchar(x), x = "chr[]")
f_substr <- mojor_fn(function(x) substr(x, 1, 3), x = "chr[]")
f_paste <- mojor_fn(function(x, y) paste(x, y, sep = " "), x = "chr[]", y = "chr[]")
```

## Data Structure Examples (Current)

These examples use data-structure bridge paths that can be policy-gated by
runtime configuration. If a path is blocked in your environment, keep
`object_mode = "off"` and use `docs/TROUBLESHOOTING.md` to
pick a fallback.

### 1) DataFrame Input Column Access (`df_schema`)

Canonical compiled subset coverage now includes scalar-read bracket forms
(`df[i, j]`, `df[i, "col"]`) in addition to `$`/`[[...]]` column routes when
`df_schema` is declared.
Strict compiled lanes also accept scalar `i32` argument selectors in
homogeneous-schema lanes (`df[i, col_i32]`).
Compiled dataframe column specs on strict compiled lanes currently include
`f64[]`, `f32[]`, `i32[]`, `lgl[]`, and `chr[]`.

```r
f <- function(df, n) {
 out <- numeric(n)
 for (i in seq_len(n)) {
 out[i] <- df$val[i] + df[["off"]][i]
 }
 out
}

b <- mojor_build(
 f,
 df = "df",
 n = "i32",
 df_schema = list(df = c(val = "f64[]", off = "f64[]"))
)
```

### 2) Return-Boundary `data.frame(...)` (Runtime Constructor Wrapper)

```r
f_df <- function(x, n) {
 a <- numeric(n)
 b <- numeric(n)
 for (i in seq_len(n)) {
 a[i] <- x[i]
 b[i] <- x[i] * 2
 }
 data.frame(a = a, b = b)
}

b_df <- mojor_build(f_df, x = "f64[]", n = "i32")
res <- b_df$func(runif(8), 8L)
```

### 3) Return-Boundary `list(...)` (Homogeneous Vectors Only)

```r
f_list <- function(x, n) {
 a <- numeric(n)
 b <- numeric(n)
 for (i in seq_len(n)) {
 a[i] <- x[i] + 1
 b[i] <- x[i] * 3
 }
 list(a = a, b = b)
}

b_list <- mojor_build(f_list, x = "f64[]", n = "i32")
out <- b_list$func(runif(8), 8L)
```

### 4) Regex Shared Runtime Subset (`grepl` / `grep` / `sub` / `gsub`)

```r
f_grepl <- mojor_fn(
 function(pattern, x) grepl(pattern, x),
 pattern = "chr",
 x = "chr[]",
 object_mode = "off"
)

f_grep <- mojor_fn(
 function(x) grep("a", x),
 x = "chr[]",
 object_mode = "off"
)

f_sub <- mojor_fn(
 function(pattern, replacement, x) sub(pattern, replacement, x),
 pattern = "chr",
 replacement = "chr",
 x = "chr[]",
 object_mode = "off"
)

f_gsub <- mojor_fn(
 function(pattern, replacement, x) gsub(pattern, replacement, x),
 pattern = "chr",
 replacement = "chr",
 x = "chr[]",
 object_mode = "off"
)
```

Current constraints:
- `x` must be a direct `chr[]` arg.
- `pattern` / `replacement` must be scalar literal or `chr` scalar arg.
- `fixed` / `perl` must be literal booleans; `fixed=TRUE` with `perl=TRUE` is rejected.

### 5) Table Mixed Subset (`row` / `col` native, `expand.grid` shared runtime)

```r
f_row <- mojor_fn(
 function(x) row(x),
 x = "f64[,]",
 object_mode = "off"
)

f_col <- mojor_fn(
 function(x) col(x),
 x = "i32[,]",
 object_mode = "off"
)

f_expand <- mojor_fn(
 function(x, y) expand.grid(x, y),
 x = "i32[]",
 y = "i32[]",
 object_mode = "off"
)
```

Current constraints:
- `row(x)` / `col(x)` require direct matrix args.
- `expand.grid(...)` supports only 2-4 unnamed direct vector args (`f64[]`/`i32[]`/`lgl[]`).
- Compile path split:
 - `row(x)` / `col(x)` compile to native matrix kernels.
 - `expand.grid(...)` runs through the shared runtime bridge path.
## Near-Term Roadmap

Use this section as a practical "what to do next" checklist when a pattern is
close but not fully covered in strict compilation.

1. Check known active limitations first:
 - [Known issues and limits](./KNOWN_ISSUES.md)
2. If your pattern is listed as constrained/deferred, choose a currently
   supported rewrite and validate with the troubleshooting playbook:
 - [Troubleshooting and workarounds](./TROUBLESHOOTING.md)
3. For IR-route coverage questions (strict vs fallback edges), verify the
   feature in the coverage matrix:
 - [IR coverage matrix](./IR/COVERAGE.md)
4. Prefer strict-safe rewrites while waiting on broader coverage:
 - explicit loops instead of implicit vectorized kernels
 - direct variable arguments for strict HOF/sample/string lanes
 - canonical matrix indexing/slice forms documented in this cookbook

---

## Current Caveats and Workarounds

**For complete workarounds and technical details, see [docs/TROUBLESHOOTING.md](./TROUBLESHOOTING.md)**

### Local Matrix Slice Assignment (Supported)

Local matrix row/column slice assignment with local allocation works in current builds.

```r
gibbs_local_slice <- mojor_fn(
 function(N, thin) {
 mat <- matrix(0, nrow = N, ncol = 2)
 x <- 0
 y <- 0
 for (i in 1:N) {
  for (j in 1:thin) {
   x <- x + 1
   y <- y + 2
  }
  mat[i, ] <- c(x, y)
 }
 mat
 },
 N = "i32", thin = "i32"
)
```

Direct array-variable RHS assignment is also supported on canonical lanes:

```r
slice_rhs_array <- mojor_fn(
 function(vals, n) {
  mat <- matrix(0, n, 2)
  for (i in seq_len(n)) {
   mat[i, ] <- vals
  }
  mat
 },
 vals = "f64[]", n = "i32"
)
```

This route uses strict modulo recycling across the slice extent.

If you need portability across older snapshots, a flattened-vector fallback remains valid.

### Mixed Integer/Float Arithmetic (Supported)

```r
mixed_auto_promote <- mojor_fn(
 function(x, y) {
  out <- numeric(length(x))
  for (i in seq_along(x)) {
   out[i] <- x[i] * y[i] - 0.5
  }
  out
 },
 x = "i32[]", y = "f64[]"
)
```

Use explicit casts when you want to force output type or avoid ambiguity in complex expressions.

### Implicit Vectorization (Supported)

Elementwise array-vectorized forms are supported without explicit loops:

```r
# Implicit vectorization — no loop needed
elementwise <- mojor_fn(
 function(x, y) x + y * 2.0,
 x = "f64[]", y = "f64[]"
)

# Multi-statement let-binding bodies also work (automatically inlined)
elementwise2 <- mojor_fn(
 function(x, y) {
   scaled <- y * 2.0
   x + scaled
 },
 x = "f64[]", y = "f64[]"
)

# Explicit loop form still works and is equivalent
elementwise3 <- mojor_fn(
 function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i] * 2.0
 }
 out
 },
 x = "f64[]", y = "f64[]"
)
```

### Vector Recycling (Supported via Broadcast Policy)

```r
add_recycle <- mojor_fn(
 function(x, y) {
  out <- numeric(length(x))
  for (i in seq_along(x)) out[i] <- x[i] + y[i]
  out
 },
 x = "f64[]", y = "f64[]",
 broadcast = "recycle_warn"
)
```

For strict/manual behavior, modulo indexing is still a good explicit alternative.

### Loop Fusion (Automatic When Legal)

```r
two_pass <- mojor_fn(
 function(x) {
  out <- numeric(length(x))
  for (i in seq_along(x)) out[i] <- x[i]
  for (i in seq_along(x)) out[i] <- out[i] + 1
  out
 },
 x = "f64[]", load = FALSE
)
two_pass$trans$fused
```

Manual fusion is still useful when you want deterministic structure for profiling or debugging.

### Known Remaining Limitations

- Parallel loops are emitted, but in-process runtime can be disabled and fall back to subprocess mode depending on host/runtime constraints.
- Multiple slice writes to different locally allocated outputs in the same loop body are supported on covered canonical forms.
- Advanced vector-indexed subarray writes beyond covered strict lanes (direct vars, constructor lanes, arithmetic `i32[]`, and supported `ifelse` `i32[]`) remain subset-limited.
- Implicit vectorized array kernels (for example `function(x, y) x + y * 2`) are not generally supported; use explicit loops.
- String support remains on strict wrapper/runtime paths rather than a full IR-native string kernel ABI.
- See [KNOWN_ISSUES.md](./KNOWN_ISSUES.md) for the consolidated limitations inventory.

---

## Debug Mode (Current)

MöjoR includes a debug mode with rich bounds checking and execution tracing.

### Quick Start

```r
# Development: enable bounds checking
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2.0
 }
 out
}

built_debug <- mojor_build(f, x = "f64[]", debug = TRUE)
result <- built_debug$func(c(1, 2, 3))
```

### Debug vs Production

**Production Mode (default):**
```r
built <- mojor_build(f, x = "f64[]", debug = FALSE) # or omit debug=
```
- Maximum performance
- Out-of-bounds access returns NaN (silent failure)
- Zero debug overhead

**Debug Mode:**
```r
built <- mojor_build(f, x = "f64[]", debug = TRUE)
```
- Bounds checking enabled
- Rich error messages with variable names, file names, line numbers
- Small performance overhead (~10-50%)

**Trace Mode:**
```r
built <- mojor_build(f, x = "f64[]", debug = TRUE, trace = TRUE)
```
- All debug mode features
- Loop iteration logging
- Higher overhead (~20-100%)

### Error Messages

**Production mode (silent failure):**
```r
f_buggy <- function(x) {
 out <- numeric(4)
 for (i in seq_len(4)) {
 out[i] <- x[i] # x only has 3 elements!
 }
 out
}

built_prod <- mojor_build(f_buggy, x = "f64[]", debug = FALSE)
result <- built_prod$func(c(1, 2, 3))
# Result: [1] 1 2 3 NaN <- NaN indicates problem, but where?
```

**Debug mode (informative error):**
```r
built_debug <- mojor_build(f_buggy, x = "f64[]", debug = TRUE)
result <- built_debug$func(c(1, 2, 3))
# Error: Bounds check failed at kernel.R line 4:
# Index 3 out of range [0, 3) for variable 'x'
```

### Best Practices

**1. Develop with debug mode:**
```r
# During development
built <- mojor_build(my_kernel, x = "f64[]", debug = TRUE)
# Test thoroughly, fix bounds errors
```

**2. Deploy in production:**
```r
# In production
built <- mojor_build(my_kernel, x = "f64[]", debug = FALSE)
# Maximum performance, zero overhead
```

**3. Use trace mode for complex debugging:**
```r
# Only when needed
built <- mojor_build(my_kernel, x = "f64[]", debug = TRUE, trace = TRUE)
# Understand execution flow, then disable
```

### With mojor_fn()

Debug mode works with `mojor_fn()`:

```r
# Debug-enabled function
fast_add_debug <- mojor_fn(
 function(x, y) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] + y[i]
 }
 out
 },
 x = "f64[]", y = "f64[]",
 debug = TRUE # Enable debug mode
)
```

### Performance

Typical overhead on element-wise kernels:

| Mode | Overhead |
|------|----------|
| Production | 0% (baseline) |
| Debug | ~10-50% |
| Trace | ~20-100% |

**Note:** Production builds have **zero overhead** - debug code is completely
eliminated at compile time through conditional compilation.

### More Information

See [DEBUG_MODE_GUIDE.md](./DEBUG_MODE_GUIDE.md) for:
- Complete API reference
- Error message examples
- Implementation details
- Troubleshooting guide
- FAQ

---

## Troubleshooting

Use `docs/TROUBLESHOOTING.md` as the canonical troubleshooting guide.

Fast links:
- SIMD/parallel/diagnostics behavior: [JIT Compile Time Got Slower](./TROUBLESHOOTING.md#jit-compile-time-got-slower)
- Build/runtime failures: [Build System Issues](./TROUBLESHOOTING.md#build-system-issues)
- GPU-specific failures: [GPU Issues (Apple Metal)](./TROUBLESHOOTING.md#gpu-issues-apple-metal)
- Gate/report issues: [Fallback Isolation Gate (Behavior-Based)](./TROUBLESHOOTING.md#fallback-isolation-gate-behavior-based)

Quick local diagnostics:

**Status: non-exported helper (development checkout only)**

```r
trans$simd$safe
trans$simd$reason
trans$parallel$safe
trans$parallel$reason
mojor_diagnostics_report()
mojor_diagnostics_snapshot()
```

---
