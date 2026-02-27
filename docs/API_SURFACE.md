# MöjoR API Surface

This page is the canonical API boundary map for MöjoR.

## Usage Rule

- Installed-package users: use exported APIs only.
- Development checkouts: non-exported helpers are available, but must be treated as unstable and explicitly labeled in examples.

## Exported Package API

Source of truth: `../packages/mojor/NAMESPACE`.

| Function | Category | Stable contract owner doc | Notes |
|---|---|---|---|
| `mojor_transpile` | Compile pipeline entry | `docs/PIPELINE/API.md` | Transpile only; no native build. |
| `mojor_build` | Compile pipeline entry | `docs/PIPELINE/API.md` | Transpile + compile/link/load flow. |
| `mojor_fn` | Compile convenience wrapper | `docs/PIPELINE/API.md` | Signature/body convenience over build. |
| `mojor_jit` | Runtime-specializing wrapper | `docs/PIPELINE/API.md` | Signature-specialized JIT wrapper. |
| `mojor_njit` | Runtime-specializing wrapper | `docs/PIPELINE/API.md` | Strict object-mode policy wrapper. |
| `mojor_vectorize` | Runtime-specializing wrapper | `docs/PIPELINE/API.md` | Elementwise wrapper surface. |
| `mojor_guvectorize` | Runtime-specializing wrapper | `docs/PIPELINE/API.md` | Core-dims generalized vectorization wrapper. |
| `mojor_jit_info` | JIT introspection | `docs/COOKBOOK.md` | Per-wrapper cache/signature/introspection metadata. |
| `mojor_jit_compile` | JIT control | `docs/COOKBOOK.md` | Explicit compile for declared signatures. |
| `mojor_jit_signatures` | JIT introspection | `docs/COOKBOOK.md` | Declared/compiled signature listing. |
| `mojor_ir_dump` | IR inspection | `docs/PIPELINE/HIR.md` | Build Tree IR from function/body. |
| `mojor_ir_print` | IR inspection | `docs/PIPELINE/HIR.md` | Pretty-print Tree IR payloads. |
| `mojor_load` | Runtime bridge | `docs/PIPELINE/RUNTIME_AND_ABI.md` | Load/bind bridge runtime. |
| `mojor_has_gpu` | Runtime capability | `docs/GPUARRAY_GUIDE.md` | Detect GPU runtime availability. |
| `mojor_options` | Global controls | `docs/PIPELINE/API.md` | Compiler/runtime options surface. |
| `mojor_prange` | Parallel loop alias | `docs/PIPELINE/API.md` | Parallel range alias for transpiled loops. |
| `mojor_run_chains_parallel` | Parallel runtime helper | `docs/PIPELINE/API.md` | Independent chain execution helper. |

## Development/Checkout-Only Surfaces

These helpers exist in the source tree but are not exported in the installed package API.

| Function | Status | Where used | Risk |
|---|---|---|---|
| `mojor_declare_c` | non-exported helper | `docs/COOKBOOK.md` (FFI) | Declaration/ABI shape may change. |
| `mojor_c_call` | non-exported helper | `docs/COOKBOOK.md` (FFI) | Strict verification/typing contract may evolve. |
| `mojor_export_c_header` | non-exported helper | `docs/COOKBOOK.md` | Header/schema details may change. |
| `mojor_save`, `mojor_load_kernel`, `mojor_serialize`, `mojor_check_compatibility` | non-exported helper | `docs/COOKBOOK.md` | Serialization metadata format may evolve. |
| `mojor_cache_info`, `mojor_cache_print`, `mojor_cache_prune`, `mojor_cache_evict`, `mojor_jit_clear_cache` | dev-only | `docs/COOKBOOK.md` | Cache schema and CLI contract may evolve. |
| `mojor_diagnostics*` family | non-exported helper | `docs/DIAGNOSTICS_API.md` | Diagnostics schema/sink behavior may evolve. |
| `mojor_runif`, `mojor_rnorm`, RNG wrapper family | non-exported helper | `docs/COOKBOOK.md` | Wrapper surface is not package-stable yet. |
| `mojor_gpu_*` helper family (for example `mojor_gpu_array`, `mojor_gpu_kernel`) | non-exported helper | `docs/GPUARRAY_GUIDE.md`, `docs/COOKBOOK.md` | GPU helper surface is development-facing and route-dependent. |

## Advanced/Dev Callable Appendix

### Cache/JIT helpers

| Function | Exported? | Notes |
|---|---|---|
| `mojor_cache_info` | `No` | Summarize cache entries/bytes and compatibility metadata. |
| `mojor_cache_print` | `No` | Human-readable cache report. |
| `mojor_cache_prune` | `No` | Prune by configured cache limits. |
| `mojor_cache_evict` | `No` | Targeted cache eviction helper. |
| `mojor_jit_clear_cache` | `No` | Clear JIT cache index/artifacts. |

### Serialization/FFI helpers

| Function | Exported? | Notes |
|---|---|---|
| `mojor_save` | `No` | Persist build payload and metadata. |
| `mojor_load_kernel` | `No` | Reload serialized payload (optional metadata-only mode). |
| `mojor_export_c_header` | `No` | Emit C header for compiled ABI surface. |
| `mojor_declare_c` | `No` | Register C declaration metadata for strict checks. |
| `mojor_c_call` | `No` | Build C-call node checked against declaration metadata. |

### RNG wrapper breadth (development checkout)

The wrappers below mirror RNG distribution metadata used by IR strict lanes.
Parameter counts align with `docs/IR/FUNCTION_SUPPORT_BREADTH.md` (`L3` RNG table).

| Wrapper | Params (`min..max`) excluding `n` | Exported? | Notes |
|---|---|---|---|
| `mojor_runif` | `0..2` | `No` | Uniform wrapper helper. |
| `mojor_rnorm` | `0..2` | `No` | Normal wrapper helper. |
| `mojor_rgamma` | `1..2` | `No` | Gamma wrapper helper. |
| `mojor_rbinom` | `2..2` | `No` | Binomial wrapper helper. |
| `mojor_rexp` | `0..1` | `No` | Exponential wrapper helper. |
| `mojor_rpois` | `1..1` | `No` | Poisson wrapper helper. |
| `mojor_rlnorm` | `0..2` | `No` | Log-normal wrapper helper. |
| `mojor_rchisq` | `1..1` | `No` | Chi-square wrapper helper. |
| `mojor_rt` | `1..1` | `No` | Student t wrapper helper. |
| `mojor_rf` | `2..2` | `No` | F wrapper helper. |
| `mojor_rbeta` | `2..2` | `No` | Beta wrapper helper. |
| `mojor_rweibull` | `1..2` | `No` | Weibull wrapper helper. |
| `mojor_rlogis` | `0..2` | `No` | Logistic wrapper helper. |
| `mojor_rcauchy` | `0..2` | `No` | Cauchy wrapper helper. |
| `mojor_rgeom` | `1..1` | `No` | Geometric wrapper helper. |
| `mojor_rnbinom` | `2..2` | `No` | Negative-binomial wrapper helper. |
| `mojor_rhyper` | `3..3` | `No` | Hypergeometric wrapper helper. |
| `mojor_rsignrank` | `1..1` | `No` | Signrank wrapper helper. |
| `mojor_rwilcox` | `2..2` | `No` | Wilcoxon wrapper helper. |
| `mojor_rng_seed` | N/A | `No` | Seed helper for MöjoR RNG stream. |

## Related References

- `docs/PIPELINE/API.md`
- `docs/COOKBOOK.md`
- `docs/IR/FUNCTION_SUPPORT_BREADTH.md`
- `docs/DIAGNOSTICS_API.md`
