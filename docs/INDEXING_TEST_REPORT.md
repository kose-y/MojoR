# MöjoR Indexing Test Report (Current)

**Generated:** 2026-02-24  
**Scope:** Current indexing hardening status for strict and non-strict parity lanes.

## Verification Run Summary

| Suite | Result | Notes |
|---|---|---|
| `packages/mojor/tests/testthat/test_loop_index_edge_syntax.R` | `FAIL 0 | WARN 0 | SKIP 0 | PASS 115` | Table-driven wrapper-equivalence matrix (read/write, strict/non-strict, positive/negative forms), selector-safety matrix, and canonicalization invariant tests. |
| `packages/mojor/tests/testthat/test_transpile_slice_index.R` | `FAIL 0 | WARN 1 | SKIP 0 | PASS 15` | Selector-length ABI coverage and slice-index lowering checks pass; one expected ifelse-lowering warning. |
| `packages/mojor/tests/testthat/test_slice_assign_nd.R` | `FAIL 0 | WARN 0 | SKIP 0 | PASS 4` | ND slice assignment lowering and runtime checks pass. |
| `packages/mojor/tests/testthat/test_array_output_dim.R` | `FAIL 0 | WARN 0 | SKIP 0 | PASS 1` | Array-output dimension variable routing checks pass. |
| `packages/mojor/tests/testthat/test_subscript_variants.R` | `FAIL 0 | WARN 1 | SKIP 0 | PASS 24` | Subscript variants pass; one expected ifelse-lowering warning. |
| `packages/mojor/tests/testthat/test_transpile_slice_ir.R` | `FAIL 0 | WARN 0 | SKIP 1 | PASS 16` | Slice IR checks pass; one intentional skip. |
| `packages/mojor/tests/testthat/test_indexing_comprehensive.R` | `FAIL 0 | WARN 0 | SKIP 0 | PASS 200` | Comprehensive indexing lanes execute as assertions/error-contract checks (no skip backlog in this file). |

## Verified Behaviors (Current)

- Parenthesized/unary-plus wrappers around covered loop/index syntax are canonicalized (strict and non-strict).
- Negative selector wrapper equivalents are covered for dynamic scalar and vector-range forms (including `seq.int` range wrappers).
- Canonicalization invariants are gated: wrapped selector/loop forms are normalized before IR build and verified to avoid raw selector leakage.
- Selector safety matrix is gated across strict/non-strict: scalar `0` selectors and mixed-sign literal `c(...)` selectors fail with deterministic compile diagnostics.
- Positive range selectors in assignment/slice paths no longer leak unresolved marker symbols.
- Positive vector selector write paths lower through standard vector-index logic.
- ND write lowering uses valid output/input dimension vars (no matrix-only var leakage in array outputs).
- Selector index expression kernels emit required selector-length parameters (`n_<selector>_i`).
- Slice assignment write paths scalarize RHS array values correctly (no bare pointer writes).
- Full-selector vector forms are covered in strict/non-strict lanes (`out <- x[]`, `out[] <- y`) with runtime-parity recycling.
- RHS missing-dimension slice assignment lowering is covered for direct indexed-slice forms (`out[1:n] <- mat[,2L]`, `out[1:n] <- arr[1L,,2L]`).
- Core chained gather lanes using selector arrays are covered (for example `subset <- x[idx]` then indexing `subset`).
- Comprehensive indexing assertions pass through the targeted regressions, including tail sections around `idx_sample`/`idx_match`.

## Active Gaps / Constraints

- Some chained higher-rank forms remain subset-limited (notably matrix/ND nested gather chains and selected literal-constructor chains).
- Remaining unsupported semantics are represented as explicit `expect_error(...)` contracts in-suite (no skip-only placeholders in the comprehensive file).
- Out-of-bounds behavior is mode-based: strict opt-in bounds mode (`bounds_check=TRUE` / `index_bounds=TRUE`) now routes covered OOB lanes to fail-fast runtime diagnostics; legacy mode remains sentinel-oriented.
- Reporter stress gate status: subprocess harness (`scripts/repro_indexing_segv_subset.R`) completed `SummaryReporter` 3/3 and `CheckReporter` 3/3 with zero crashes.
- Same-process full-file `CheckReporter` runs can still intermittently hit recursive-GC segfaults; track as a separate stability lane from subprocess stress.

## Repro Commands

```bash
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_loop_index_edge_syntax.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_transpile_slice_index.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_slice_assign_nd.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_array_output_dim.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_subscript_variants.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_transpile_slice_ir.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_indexing_comprehensive.R")'
Rscript scripts/repro_indexing_segv_subset.R --file=packages/mojor/tests/testthat/test_indexing_comprehensive.R --reporter=summary --repeats=3 --reset_mode=soft --file_reset_mode=soft --keep_backend=true --per_test_reset=false --stress_opt_level=0 --disable_r_jit=true --clear_cache_each_run=true
Rscript scripts/repro_indexing_segv_subset.R --file=packages/mojor/tests/testthat/test_indexing_comprehensive.R --reporter=check --repeats=3 --reset_mode=soft --file_reset_mode=soft --keep_backend=true --per_test_reset=false --stress_opt_level=0 --disable_r_jit=true --clear_cache_each_run=true
MOJOR_TEST_DISABLE_R_JIT=true MOJOR_TEST_RESET_MODE=soft MOJOR_TEST_FILE_RESET_MODE=soft MOJOR_TEST_PER_TEST_RESET=false MOJOR_TEST_STRESS_OPT_LEVEL=0 Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_indexing_comprehensive.R", reporter = testthat::CheckReporter$new())'
```

## Canonical References

- `docs/SUBSET.md`
- `docs/KNOWN_ISSUES.md`
- `docs/TROUBLESHOOTING.md`
