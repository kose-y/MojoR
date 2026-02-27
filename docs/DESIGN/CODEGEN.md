# Tree/SIMD Reduction Codegen Design

**Date:** 2026-02-09 **Status:** Implemented for scalar reductions with remaining non-scalar shape gaps

---

## Plain-Language Mental Model

This document explains how reduction strategy changes generated code shape.

- `linear`: simple one-by-one accumulation.
- `tree`: pairwise merge pattern.
- `simd`: vector lanes + horizontal reduce + scalar tail.

All three should compute the same result; only execution strategy changes.

## Mini Example

For `sum(x)`:

- Linear: one running `acc`.
- Tree: combine pairs, then combine pair-results.
- SIMD: accumulate vectors, reduce lanes, then handle leftover elements.

The compiler chooses the route based on schedule policy and legality.

## Beginner Debug Checklist

1. Verify which reduction route was selected (`linear/tree/simd`).
2. If SIMD/tree did not apply, check dtype/op eligibility and legality gates.
3. Compare emitted shape with this doc before debugging backend/runtime behavior.
4. Keep scalar baseline behavior as correctness oracle for new route changes.

## Current State

Implemented:
- `reduction = "linear"|"tree"|"simd"` is accepted and threaded through IR scheduling.
- Schedule-backed transforms exist for scalar reductions:
 - `.mojor_ir_schedule_tree_reduce()`
 - `.mojor_ir_schedule_simd_reduce()`
- Top-level scalar reduction routing now reaches schedule pass.
- Focused transpile/runtime tests cover:
 - tree: scalar `sum`/`product`/`min`/`max` and tie-stable `which.min`/`which.max`
 - simd: scalar `sum`/`product`/`min`/`max` (f64/f32), scalar `sum`/`product` (i32), tie-stable `which.min`/`which.max` (f64/f32/i32)

Current SIMD codegen uses the updated Mojo API:

```mojo
from sys.info import simd_width_of

comptime simd_width = simd_width_of[DType.float64]()
var vec = arr.load[width=simd_width](offset)
```

## Codegen Entry/Exit Contract

Input contract:
- Reduction intent is already expressed in IR (`scalar_reduce` or `scheduled_reduce`).
- Schedule policy has already selected route (`linear`, `tree`, `simd`) when legal.
- Type/dtype metadata is available for emitter legality checks.

Output contract:
- Emitted Mojo source preserves reduction semantics.
- Route-specific emitted shape is deterministic for the same input IR + schedule policy.
- Unsupported route/dtype combinations fail verification before final emission.

---

## Linear Baseline

Sequential accumulation remains the baseline fallback:

```mojo
acc = arr[0]
for i in range(2, n_i + 1):
 acc = min(acc, arr[i - 1])
```

---

## Tree Reduction Shape

Pairwise combine into a temporary buffer until one value remains:

```mojo
# pairwise pass
var n_pairs = n_i // 2
for i in range(n_pairs):
 temp[i] = min(arr[2 * i], arr[2 * i + 1])

# carry odd tail
if n_i % 2 == 1:
 temp[n_pairs] = arr[n_i - 1]
 n_pairs += 1

# iterative pairwise collapse
while n_pairs > 1:
 ...
```

---

## SIMD Reduction Shape

Vector accumulate, horizontal reduce, scalar tail:

```mojo
from sys.info import simd_width_of

comptime simd_width = simd_width_of[DType.float64]()
var vec_acc = SIMD[DType.float64, simd_width](_MOJOR_INF)

var n_chunks = n_i // simd_width
for c in range(n_chunks):
 var vec = arr.load[width=simd_width](c * simd_width)
 vec_acc = min(vec_acc, vec)

acc = vec_acc[0]
for lane in range(1, simd_width):
 acc = min(acc, vec_acc[lane])

for i in range(n_chunks * simd_width, n_i):
 acc = min(acc, arr[i])
```

---

## Metadata Contract

`scalar_reduce` metadata is the contract for schedule decisions:
- `init`
- `axis`
- `associative`
- `commutative`

Current scheduling use:
- tree: scalar `sum`/`product`/`min`/`max`, plus tie-stable `which.min`/`which.max`
- simd: scalar `sum`/`product`/`min`/`max` for floating-point arrays, scalar `sum`/`product` for i32 arrays, tie-stable SIMD arg-reduction for `which.min`/`which.max`

## Representative Diagnostics

Common reduction-route validation failures include:

- `IR verify [scalar_reduce]: op must be one of ...`
- `IR verify [scheduled_reduce]: mode must be one of tree/simd`
- `IR verify [scheduled_reduce]: op must be one of sum/product/min/max/which.min/which.max`
- `IR verify [scheduled_reduce]: dtype is invalid for mode`

These diagnostics mean route/metadata contracts were invalid before codegen, not that runtime execution failed.

## Limitation Source of Truth

This page documents reduction code-shape design and contracts.
For consolidated current limitations/deferred routes, use:
- `docs/KNOWN_ISSUES.md`
- `docs/SUBSET.md`

## Implementation Mapping

- Scheduling rewrite: `packages/mojor/R/ir/nodes.R` (`.mojor_ir_schedule_tree_reduce`, `.mojor_ir_schedule_simd_reduce`)
- Reduction emission: `packages/mojor/R/ir/stmt_emit.R` (`.mojor_ir_scalar_reduce_emit`, `.mojor_ir_scheduled_reduce_emit`)
- Reduction verifier contracts: `packages/mojor/R/ir/verify.R`

---

## Related Documents

- [IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Implementation Details
- [../PIPELINE/SCHEDULING.md](../PIPELINE/SCHEDULING.md) - Reduction scheduling and route policy
- [../KNOWN_ISSUES.md](../KNOWN_ISSUES.md) - Consolidated current limitations/deferred routes
