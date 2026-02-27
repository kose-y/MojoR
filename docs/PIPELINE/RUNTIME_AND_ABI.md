# Runtime and ABI Step

After emission, MöjoR builds native artifacts and exposes callable R wrappers.

## Plain-Language Mental Model

This stage turns generated code into something you can actually call from R.

- Build compiles and links artifacts.
- ABI ensures caller and callee agree on argument/return contracts.
- Bridge/wrapper handles runtime loading and invocation.

If ABI contract is wrong, compiled code can exist but still fail at call time.

## Build Outputs

Typical outputs include:
- Generated source artifacts.
- Compiled shared libraries.
- Wrapper entrypoints callable from R.

## ABI Role

ABI defines the binary contract between generated kernels/wrappers and runtime callers.

Key ABI concerns:
- Argument type/layout agreement.
- Output shape/type agreement.
- Stable call/return conventions.

## Bridge and Wrapper Path

High-level path:

`typed+emitted kernel -> build/link -> shared lib -> bridge wrapper -> R call`

## Mini Example

1. Kernel source is emitted.
2. Toolchain compiles it into a shared library.
3. Runtime bridge loads that library.
4. Wrapper marshals R inputs to the expected ABI format.
5. Native call executes and wrapper normalizes result back to R.

## Semantics Constraints

When `semantics = "r"`, runtime behavior must preserve R-facing expectations (including copy-on-modify constraints where relevant).

## Runtime Dispatch Contract

Bridge-managed runtime calls use one canonical dispatch lane:

- Resolve runtime-v1 alias candidates for each symbol:
  - `mojor_rt_v1_<symbol>`
  - `mojor_rt_v1_<symbol-without-mojor-prefix>` when `<symbol>` starts with `mojor_`
- Call the first loaded alias in the bound bridge package.
- If no alias is loaded, fail with:
  - `mojor runtime v1 dispatch failure [symbol=<name>, reason=v1_symbol_not_loaded]: ...`
- If alias call fails, fail with:
  - `mojor runtime v1 dispatch failure [symbol=<name>, reason=v1_call_error]: ...`

Notes:
- Non-bridge package calls continue using direct `.Call(symbol, PACKAGE=pkg)`.
- Runtime-v1 alias resolution is cached per `pkg::symbol`.

## Failure Surfaces

- Backend toolchain unavailable.
- ABI mismatch or wrapper loading issues.
- Runtime-v1 alias missing/failing for a bridge-managed symbol.

## Runtime Path Checklist

Use this order when debugging runtime failures:

1. Source emission succeeded and build step started.
2. Shared library or wrapper binary was produced.
3. Bridge load succeeded (`mojor_load(...)` path/symbols are valid).
4. Wrapper argument marshalling matches emitted ABI.
5. Return normalization matches declared output contract.

If any step fails, fix that boundary before checking later steps.

## Code Mapping

- Build orchestration: `packages/mojor/R/build/build.R`
- ABI docs: `docs/BACKEND/ABI.md`
- Troubleshooting for runtime/build: `docs/TROUBLESHOOTING.md`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Build orchestration | `mojor_build(...)` in `packages/mojor/R/build/build.R` | Runs transpile -> compile/link -> load pipeline and returns runtime callable artifacts. |
| Shared compiled-subset build | `mojor_build(...)` and helpers in `packages/mojor/R/build/build.R` and `packages/mojor/R/build/helpers.R` | Compiles wrappers/shared libraries for unified transpile outputs. |
| Runtime call wrapper | `.mojor_make_kernel_call_wrapper(...)` in `packages/mojor/R/core/core.R` | Creates R-callable wrapper around native entrypoints and result normalization. |
| Bridge selection/binding | `.mojor_bind_bridge(...)`, `.mojor_bridge_pkg(...)`, `.mojor_call_bridge(...)` in `packages/mojor/R/core/core.R` | Resolves and dispatches bridge symbols to loaded runtime libraries. |
| Bridge load entry | `mojor_load(...)` in `packages/mojor/R/core/core.R` | Loads and binds bridge shared library from explicit or inferred path. |

## ABI Contract Details

- Argument dtype/layout contracts must match generated kernel signatures.
- Return contract (scalar/vector/matrix/list/data-frame) must match wrapper/runtime assembly expectations.
- Bridge symbol names and calling conventions must stay stable across build/load/call boundaries.

## Diagnostics and Failure Surfaces

- Toolchain failure: source compiled but wrapper/shared object link failed.
- Bridge mismatch: shared object loaded but expected symbol/export contract missing.
- Runtime mismatch: wrapper call contract differs from emitted kernel ABI expectations.

## Representative Runtime/ABI Diagnostics

- `Mojo build failed`
- `C wrapper build failed`
- `mojor_load: bridge not found at <path>`
- `Mojo backend not loaded; call mojor_load() first`
- `GPU not available`
- `gpu_func: expected mojor_gpu_array inputs for array args`

These errors are boundary signals:
- Build errors indicate toolchain/link problems.
- Bridge errors indicate load/symbol mismatch.
- Runtime call errors indicate argument/return ABI mismatch.

## Beginner Debug Checklist

1. Separate compile/link failures from load/call failures.
2. If library loads but call fails, inspect bridge symbol expectations first.
3. Verify argument shape/type contract matches emitted kernel signature.
4. Under `semantics = "r"`, check runtime behavior against R-facing expectations (including copy-on-modify-sensitive paths).

## Copy-on-Modify Note

`semantics = "r"` keeps R-facing behavior as the contract. In alias-sensitive or uncertain in-place situations, runtime routes remain conservative instead of assuming unsafe buffer mutation behavior.

## Internal Symbol Index

`Build and compile entrypoints`
- `mojor_build`
- `.mojor_resolve_effective_ir_only`
- `.mojor_is_object_mode_fallback_error`
- `.mojor_object_mode_entry_reason`
- `.mojor_build_object_mode_result`

`Bridge loading and ABI dispatch`
- `mojor_load`
- `.mojor_bind_bridge`
- `.mojor_bridge_pkg`
- `.mojor_bridge_pkg_for_symbol`
- `.mojor_call_bridge`
- `.mojor_runtime_symbol_v1_candidates`
- `.mojor_runtime_v1_cache_key`
- `.mojor_runtime_v1_resolve_symbol`
- `.mojor_runtime_v1_dispatch_error`
- `.mojor_runtime_dispatch_call`
- `.mojor_make_package_scoped_call`
- `.mojor_make_kernel_call_wrapper`
- `.mojor_normalize_kernel_result`

`JIT/runtime artifact lifecycle`
- `mojor_cache_info`
- `mojor_cache_evict`
- `mojor_cache_prune`
- `mojor_jit_clear_cache`
- `.mojor_jit_cache_read`
- `.mojor_jit_cache_write`
- `.mojor_jit_cache_prune`
- `.mojor_jit_cache_enforce`
- `.mojor_resolve_cache_dir`

`Runtime policy and safety hooks`
- `mojor_options`
- `.mojor_effective_na_mode`
- `.mojor_effective_r_jit_level`
- `.mojor_effective_jit_disk_cache`
- `.mojor_effective_jit_strict_signatures`
- `.mojor_check_no_na_args`
- `.mojor_check_no_na_reduction_arg`
