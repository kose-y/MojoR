# IR Verifier Strengthening — Complete

**Date:** 2026-02-08 **Status:** ✅ Complete (89 new tests, 125 existing tests passing)

## Summary

The IR verifier (`.mojor_ir_verify()`) has been significantly strengthened with comprehensive invariant checks. This addresses the external review recommendation: **"If you do only one thing: Verifier + canonical CFG merge model."**

## Verifier Input/Output Contract

Input contract:
- A Tree IR node (usually function-body `block`) with `kind` fields and required node shape.
- Optional verifier context (`ir_only`, `check_scope`, `defined_vars`, `loop_vars`, `in_loop`, `type_env`).

Output contract:
- Success: returns `TRUE` for shape/legality-valid IR.
- Failure: throws first invariant violation with node-scoped diagnostic text.
- Warnings: reports suspicious but non-fatal forms (for example redundant casts, provably empty ranges).

## Plain-Language Mental Model

Verifier is the pipeline safety gate.

- It rejects malformed or illegal IR early.
- It prevents downstream passes from operating on invalid assumptions.
- It turns subtle runtime bugs into explicit compile-time diagnostics.

If verifier says no, that is usually a feature, not a nuisance.

## Mini Example

If a block contains `return(...)` and then more statements:

- Verifier flags unreachable statements.
- Pipeline stops instead of generating confusing dead code.

This catches correctness issues before optimization/emission.

## Beginner Debug Checklist

1. Start with the first verifier error; later errors are often cascades.
2. Confirm control-flow context (`in_loop`) for `break`/`next`.
3. Check node shape and required fields before checking advanced invariants.
4. Use focused verifier tests when modifying checks to avoid regressions.

## What Was Added

### 1. Control Flow Invariants ✅

**Break/next only inside loops:**
- `break` and `next` statements now require `ctx$in_loop = TRUE`
- Verifier tracks loop context through loop/while/repeat bodies
- **Test coverage:** 6 tests in `test_ir_verify_invariants.R`

**Unreachable code detection:**
- Statements after `return` in a block are flagged as errors
- Verifier tracks `has_returned` state through blocks
- **Test coverage:** 2 tests

**Loop variable shadowing:**
- Already implemented (existing), now explicitly documented
- Nested loops cannot reuse variable names
- **Test coverage:** 2 tests

---

### 2. Type Consistency Checks ✅

**Redundant cast warnings:**
- Casts where source type = destination type generate warnings
- Example: `cast(f64 → f64)` warns "redundant cast"
- Works with both scalar and array types
- **Test coverage:** 3 tests

**Call arity validation:**
- Unary functions (sin, cos, log, sqrt, ...) must have 1 argument
- Binary functions (min, max) must have 2 arguments
- Unknown functions have no arity check (extensibility)
- **Test coverage:** 6 tests

**Index expression scalarity:**
- Index expressions should be scalar-valued (not array-valued)
- Logical masks (`lgl[]`, `bool[]`) are explicitly allowed
- **Test coverage:** 3 tests

---

### 3. Range Well-Formedness Checks ✅

**Zero step rejection:**
- `range(start, end, step=0)` is rejected as invalid
- **Test coverage:** 1 test

**Empty range warnings:**
- `start > end` with positive step warns "empty range"
- `start < end` with negative step warns "empty range"
- Valid descending ranges (`10:1` with step=-1) are silent
- **Test coverage:** 3 tests

---

### 4. Structural Invariants ✅

**Empty index list rejection:**
- `index(x, indices=[])` is invalid (x[] without indices)
- **Test coverage:** 1 test

**Sequential definition tracking:**
- Already implemented (existing), now enhanced
- Variables added to `defined_vars` as assigned in blocks
- **Test coverage:** 2 tests

**Use-before-define detection:**
- Already implemented (existing), now explicitly tested
- Requires `check_scope = TRUE` and `defined_vars` seeded
- **Test coverage:** 2 tests

## Representative Verifier Diagnostics

Common first-failure messages:

- `IR verify: node missing valid 'kind' field`
- `IR verify [break]: break statement outside loop`
- `IR verify [next]: next statement outside loop`
- `IR verify [index]: indices list cannot be empty`
- `IR verify [range]: step cannot be zero`
- `IR verify [call]: fn must be a single character string`
- `IR verify [scheduled_reduce]: mode must be one of tree/simd`

Treat these as contract failures in IR construction/normalization, not backend runtime errors.

## Implementation Mapping

- Tree verifier: `packages/mojor/R/ir/verify.R` (`.mojor_ir_verify`)
- Pipeline wrapper hooks: `packages/mojor/R/transpile/ir_helpers.R` (`.mojor_prepare_transpile_ir_stmt`, `.mojor_verify_ir_stmt`)
- Tests: `packages/mojor/tests/testthat/test_ir_verify_invariants.R`, `packages/mojor/tests/testthat/test_ir_verify.R`

---

## Test Coverage Summary

### New Test File: `test_ir_verify_invariants.R`
**89 tests total:**
- 6 tests: break/next control flow
- 2 tests: unreachable code after return
- 3 tests: redundant cast warnings
- 6 tests: call arity validation
- 3 tests: index expression scalarity
- 4 tests: range well-formedness
- 2 tests: loop variable shadowing
- 2 tests: use-before-define
- 2 tests: sequential definition tracking
- 50+ tests: comprehensive coverage of all math functions
- 6 tests: complex invariant combinations

### Updated Test File: `test_ir_verify.R`

---

## Related Documents

- [IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Implementation Details
- [CONTRACT.md](../IR/CONTRACT.md) - IR Contracts
