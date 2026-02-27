# IR Logical Mask Indexing/Assignment Design

**Date:** 2026-02-06 **Status:** ✅ IMPLEMENTED **Implementation:** `packages/mojor/R/ir/stmt_assign_emit.R` (`.mojor_ir_mask_assign_emit()`) and `packages/mojor/R/ir/ir.R` (`.mojor_ir_mask_extract_assign_emit()`) **Tests:** [`packages/mojor/tests/testthat/test_transpile_mask_ir.R`](../../packages/mojor/tests/testthat/test_transpile_mask_ir.R), [`packages/mojor/tests/testthat/test_loop_runtime_outputs_indexing.R`](../../packages/mojor/tests/testthat/test_loop_runtime_outputs_indexing.R)

## Plain-Language Mental Model

Logical mask indexing means "apply operation only where mask is TRUE."

- `TRUE`: apply operation
- `FALSE`: skip
- `NA`: skip (per current contract)

IR lowers this into explicit loops and checks, so behavior is safe and deterministic.

## Mini Example

For `out[mask] <- rhs`:

1. Loop over mask positions.
2. If mask value is `NA` or `FALSE`, continue.
3. If mask value is `TRUE`, write `rhs` to corresponding output slot.

This is clearer and safer than ad-hoc string-generated indexing logic.

## Beginner Debug Checklist

1. Confirm mask type and length assumptions for the target form.
2. Check how NA-in-mask should behave (skip in current contract).
3. For extraction paths (`out <- x[mask]`), verify compressed TRUE-count allocation behavior.
4. If runtime output mismatches, inspect lowered loop shape before inspecting backend code.

## Current State
- Mask LHS assignments are emitted via IR and lower to explicit mask loops with NA/false checks.
- Mask extraction assignments (`out <- x[mask]`) are supported in IR with compressed selection (TRUE only).
- Output length for `out <- x[mask]` is allocated using the count of TRUE values in `mask` (NA skipped).
- Legacy path is no longer used for mask LHS assignments.
- `ir_only` passes for mask assignment; mask **extraction** is supported for slice/mask assignments and `out <- x[mask]` lowering (compressed selection).

## Limitation Source of Truth

This page focuses on mask-indexing design/implementation details.

- Use `docs/KNOWN_ISSUES.md` for consolidated current limitations/deferred lanes.
- Use `docs/SUBSET.md` for strict subset boundaries.

## Problem Statement (RESOLVED)
Logical mask indexing/assignment was previously handled by legacy string codegen which **segfaulted at runtime** for `out[mask] <- x[i]`.
The IR implementation now provides full support, making mask-based assignment safe and consistent.

## Scope (v0)
**Target:** Assignment with logical masks in loop bodies.
- LHS mask write: `out[mask] <- rhs`
- Mask values are `lgl[]` (stored as `Int32`), with `NA` encoded as `-2147483648`
- **Minimal RHS support:** scalar RHS (e.g., `x[i]`, numeric literal)

**Historical v0 out-of-scope list (retained for design context):**
- `x[mask]` as a general expression in arithmetic (e.g., `sum(x[mask])`) without explicit assignment
- Recycling and length mismatch warnings
- Mask-to-mask assignment (`out[mask] <- y[mask2]`)
- ND array mask indexing

## Requirements
- **Safety:** No direct pointer arithmetic without bounds checks.
- **Semantics:** `NA` in mask must skip assignment (align with current runtime contract).
- **Consistency:** Respect existing `na_mode`/`na_guard` handling for RHS eval.
- **Integration:** Use IR lowering, not legacy string codegen.

## Proposed IR Modeling
Option A (preferred): Extend `Index` to support a logical mask variant.

```
Index(base, indices = NULL, mask = <expr>, index_base = "zero_based")
```

`mask` is an expression that resolves to a logical array (`lgl[]` / `bool[]`).

## Lowering Strategy
Lower `Assign(Index(base, mask), rhs)` to an explicit loop:

```
for j in range(mask_len):
 if mask[j] == NA: continue
 if mask[j] == 0: continue
 base[j] = rhs_value
```

Notes:
- `mask_len` should be resolved via `len_var_map` or `len(mask)` IR helper.
- `rhs_value` is evaluated once per outer loop iteration (scalar RHS only in v0).
- Bounds checks: If `bounds_check = TRUE`, guard `j` against `len(base)` unless
 `mask_len` is known to equal `len(base)`.

## Type Rules
- `mask` must be `lgl[]` / `bool[]`.
- `base` must be a 1D array (`f64[]`, `f32[]`, `i32[]`, `lgl[]`).
- `rhs` must type-check against the element type of `base`.

## Implementation Notes
- Add IR build support: detect `[` with logical index on LHS and emit `Index(..., mask=...)`.
- Add IR emit/lowering pass for `Assign(Index(mask=...), rhs)`.
- Reuse `_mojor_read_lgl()` for safe mask reads when needed.
- Runtime lanes are IR-native for covered logical-mask assignment/extraction routes.

## Test Plan
- Runtime: `test_loop_runtime_outputs_indexing.R` logical index assignment case should pass.
- Transpile: Add a focused IR-only test to verify emitted mask loop structure.

## Decisions (Resolved)

1. **NA handling in masks**: NA in mask always skips assignment (does not raise error even in `na_mode="forbid"`). This aligns with R semantics where `NA` in a logical index is treated as `FALSE`.
 - Implementation: Check for Int32 NA value (-2147483648) and `continue` to skip

2. **Length checking**: The implementation uses `len(mask)` for the mask loop bounds. Length mismatches are handled naturally by the safe read functions (`_mojor_read_lgl`).
 - Recycling is not explicitly implemented for v0
 - Out-of-bounds mask accesses return safe default values

## Status
✅ **COMPLETE** - Logical-mask assignment and extraction support are implemented, tested, and mirrored to `packages/mojor/`.

---

## Related Documents

- [IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Implementation Details
- [GUARDS.md](./GUARDS.md) - Guard Pass Design
