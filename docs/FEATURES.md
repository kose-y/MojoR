# MöjoR Feature Index

**Last validated:** 2026-02-23

MöjoR is a scoped, Numba-style accelerator for typed hot loops in R. It is not a
full R compiler and intentionally enforces strict-subset boundaries.

## Core Compilation

- Typed hot-loop compilation via `mojor_fn()`
- IR-first transpilation pipeline (AST -> IR -> typed IR -> Mojo)
- Strict mode and compatibility/object-mode controls

## Loop and Scheduling Controls

- Loop unrolling (`unroll`)
- Loop tiling (`tile`) for eligible nested loops
- Reduction schedule controls (`reduction = "auto|linear|tree|simd"`)
- Bounds-check mode controls
- Fusion and optimization passes where legality permits

## Data and Math Coverage

- Numeric/logical scalar + array kernels (`f64`, `f32`, `i32`, `lgl`)
- Implicit vectorized expressions over typed array args (arithmetic, comparison,
  logical, math, type checks/casts, `ifelse`, `pmin`/`pmax`, `xor`;
  multi-statement let-binding bodies with automatic inlining)
- Elementwise operations and supported helper math functions
- Cumulative operations (`cumsum`, `cumprod`, `cummax`, `cummin`)
- Statistical helpers (`mean`, `var`, `sd`, selected cov/cor/quantile paths)
- Matrix operations (`%*%`, `crossprod`, `tcrossprod`)

## Indexing and Shape Features

- 1-D, 2-D, and N-D read/write indexing for supported declarations
- Negative exclusion indexing on covered strict lanes (`x[-k]`, `x[-c(...)]`,
  covered matrix/ND exclusion routes)
- Matrix/array output construction (`matrix`, `array`)
- Slice/subscript assignment hardening for covered ND routes (marker-free range
  lowering, selector-length-aware index emission, RHS scalarization in write
  context)
- Logical mask writes and character/dimname-aware indexing paths, including
  named vector lookups (`c(a=1)["a"]`) and compile-time-resolved local
  character loop indices

## Runtime and Tooling

- Build/load helpers (`mojor_build`, `mojor_load`, JIT wrappers)
- Serialization/profile helpers
- Debug mode and runtime diagnostics
- GPU bridge and GPUArray routing (backend capability dependent)
- Built-in RNG wrappers (`mojor_runif`, `mojor_rnorm`, etc.)

## Caveats

- Parallel loops are codegen-supported but currently require subprocess/executable execution for stable runtime behavior.
- GPU behavior is backend-dependent; on Apple Metal, `f32` is the default-safe path.
- Chained positive-selector indexing (`x[i1][i2]`) is still subset-limited in
  strict lanes.
- Some 3D runtime dispatch routes remain partial; see `docs/KNOWN_ISSUES.md`
  for the latest constraints.
- Some advanced features remain strict-subset only and reject unsupported patterns by design.

## References

- `docs/SUBSET.md`
- `docs/COOKBOOK.md`
- `docs/TROUBLESHOOTING.md`
- `packages/mojor/FEATURES_V0_2.md` (legacy detailed mirror)
