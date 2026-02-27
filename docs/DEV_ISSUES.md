# MöjoR Development Issues and Contributor Sharp Edges

**Last Updated:** 2026-02-20

This document tracks development-time issues that affect contributors and CI
signal quality. These are not end-user API limitations; they are workflow,
coverage, and maintenance sharp edges for day-to-day MöjoR development.

## 1) Environment-Gated Test Coverage

- Large portions of runtime/GPU coverage are skip-gated on local environment:
  Mojo backend availability, GPU availability, and `float` package install.
- Several important lanes are additionally opt-in behind environment flags
  (for example `MOJOR_TEST_GPU_STRICT_BIND`, `MOJOR_TEST_GPU_CTX_SMOKE`,
  `MOJOR_TEST_GPU_EW`), so default local runs can miss them.
- Practical impact: a passing local test run may still leave GPU/bridge/f64
  lanes unexercised.

References:
- `packages/mojor/tests/testthat/test_gpu_array_*.R` (split suite; see `test_gpu_array_SPLIT_MANIFEST.tsv`)
- `packages/mojor/tests/testthat/test_gpu_bridge_binding.R`
- `packages/mojor/tests/testthat/test_transpile_elementwise.R`
- `packages/mojor/tests/testthat/helper-mojo.R`

## 2) Skip-On-Failure Patterns Can Hide Regressions

- Some tests skip when transpile/build fails instead of failing the test.
- Practical impact: regressions may appear as "skipped" rather than "failed"
  unless skip counts are actively monitored.

References:
- `packages/mojor/tests/testthat/test_cumulative_runtime.R`
- `packages/mojor/tests/testthat/test_cumulative_expr.R`

## 3) Explicit Technical-Debt Skip Markers

- A subset of tests encode known not-yet-supported behavior as hard skips,
  including standalone slice assignment validation, dynamic-length recycling
  analysis, and implicit downcast support.
- Practical impact: these are known development gaps; contributors should treat
  these skips as backlog markers, not stable "done" behavior.

References:
- `packages/mojor/tests/testthat/test_transpile_slice_ir.R`
- `packages/mojor/tests/testthat/test_implicit_loops.R`
- `packages/mojor/tests/testthat/test_downcast_fncall.R`

## 4) Post-Prototype Migration Debt

- Some docs/scripts/tests still carry assumptions from the removed `packages/mojor/`
  tree.
- Practical impact: stale references can mislead contributors or cause false
  failures in automation paths that have not been fully migrated.

References:
- `scripts/check_fallback_isolation.sh`
- `packages/mojor/tests/testthat/test_fallback_isolation_script.R`
- `BUILD_AND_TEST.md`

## 5) Fallback Isolation Gate Requires Baseline Maintenance

- Fallback growth is checked against a baseline CSV and pattern list.
- The gate fails on growth by default and depends on curated pattern
  specificity plus periodic baseline updates when intentional behavior changes.
- Tooling dependency: requires `rg`.

References:
- `scripts/check_fallback_isolation.sh`
- `scripts/fallback_behavior_patterns.txt`
- `docs/BASELINES/FALLBACK_HOTSPOTS_BASELINE.csv`

## 6) Platform/Tooling-Dependent Verification Paths

- Some verification scripts are platform/tool dependent (for example `nm` and
  platform bundle presence for backend symbol checks).
- Practical impact: these checks frequently skip outside fully provisioned
  contributor environments.

References:
- `packages/mojor/tests/testthat/test_backend_bundle_symbols_script.R`
- `BUILD_AND_TEST.md`

## Suggested Contributor Routine

1. Run baseline fast tests first, then explicitly run opt-in GPU/bridge lanes
   when touching GPU or backend code.
2. Keep all source/test changes in `packages/mojor/` and reject stale
   `packages/mojor/` references during review.
3. Run fallback isolation checks after changing fallback/object-mode behavior.
4. Treat "skip due to failure" patterns as warning signals and inspect skip
   reasons before considering a change complete.
