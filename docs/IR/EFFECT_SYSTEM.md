# IR Effect System — Complete

**Date:** 2026-02-08 **Status:** ✅ Complete (50 tests, all passing)

## Summary

Implemented a lightweight effect system for IR expressions that tracks side effects (Pure, ReadsMem, WritesMem, RNG, Status). This enables future optimization passes including Common Subexpression Elimination (CSE) and Loop-Invariant Code Motion (LICM).

Current optimizer legality mapping in pipeline docs/contracts:
- `Pure -> None`
- `ReadsMem -> Read`
- write-like statement effects -> `Write`
- `RNG -> RNG`
- unresolved/status-like effects -> `Unknown`

## Plain-Language Mental Model

The effect system decides what optimizations are safe.

- Pure: can usually move/reuse/drop if unused.
- Reads memory: may move/reuse only if no conflicting writes.
- RNG or unknown: do not reorder or eliminate.

If the compiler cannot prove safety, it stays conservative on purpose.

## Mini Example

Two expressions can look similar but behave differently:

- `x[i] + 1` reads memory (`ReadsMem`) and may need alias checks.
- `rnorm(1) + 1` consumes RNG state and cannot be CSE/LICM reordered.

So optimization legality depends on effects, not just syntax.

## Beginner Debug Checklist

1. Classify effect first (`Pure`, `ReadsMem`, `RNG`, `Unknown`).
2. Check whether blocked optimization is expected for that effect class.
3. If behavior seems wrong, inspect effect propagation through parent expressions.
4. Treat `Unknown` as a hard safety barrier unless classification is proven incorrect.

---

## What Was Implemented

### Core API (3 functions + 3 helpers)

**Main Function:**
```r
.mojor_ir_expr_effects(node) # Returns character vector of effects
```

**Helper Functions:**
```r
.mojor_ir_is_pure(node) # TRUE if all effects are "Pure"
.mojor_ir_has_rng(node) # TRUE if any effect is "RNG"
.mojor_ir_reads_mem(node) # TRUE if any effect is "ReadsMem"
```

**Lines of Code:** ~170 (implementation + tests)

---

## Effect Lattice

| Effect | Meaning | Optimization Impact |
|--------|---------|-------------------|
| **Pure** | No side effects, deterministic | Can CSE, hoist, reorder freely |
| **ReadsMem** | Reads from memory | Can CSE/hoist with alias analysis |
| **RNG** | Random number generation | Cannot CSE or hoist (must evaluate fresh) |
| **WritesMem** | Writes to memory | Applies to statements, not expressions |
| **Status** | Modifies flags (NA flag) | Future use |
| **Unknown** | Unknown effects | Conservative (no optimization) |

---

## Effect Classification

### Pure Operations ✅

**Constants and Variables:**
- `42`, `3.14`, `TRUE` → Pure
- `x`, `y`, `z` → Pure (names don't read memory)

**Arithmetic:**
- `x + y`, `x * 2`, `x / y` → Pure
- `-x`, `!x` → Pure
- `x %% y`, `x %/% y` → Pure

**Comparisons:**
- `x > 0`, `x == y`, `x != z` → Pure

**Logical:**
- `x && y`, `x || y` → Pure

**Math Functions:**
- `sin(x)`, `log(y)`, `sqrt(z)` → Pure
- All 18 supported math functions → Pure

**Predicates:**
- `is.na(x)`, `is.nan(x)`, `is.finite(x)` → Pure

**Aggregates:**
- `min(x, y)`, `max(x, y)` → Pure (on scalar args)

### ReadsMem Operations ✅

**Array Indexing:**
- `x[i]` → ReadsMem
- `mat[i, j]` → ReadsMem
- `arr[i, j, k]` → ReadsMem

**Expressions Containing Reads:**
- `sin(x[i])` → ReadsMem (inherits from x[i])
- `x[i] + y[j]` → ReadsMem (both read)
- `as.double(x[i])` → ReadsMem (inherits)

### RNG Operations ✅

**Scalar RNG (call nodes):**
- `rnorm(n)` → RNG
- `runif(n)` → RNG
- `rgamma(n)` → RNG
- `rpois(n)`, `rbinom(n)`, `rexp(n)` → RNG
- `rlnorm(n)`, `rchisq(n)`, `rt(n)`, `rf(n)` → RNG
- `rbeta(n)`, `rweibull(n)`, `rlogis(n)`, `rcauchy(n)` → RNG
- `rgeom(n)`, `rnbinom(n)` → RNG

**Vectorized RNG:**
- `rng_vec(dist, n, params)` → RNG
- `alloc(len, dtype)` → Pure (allocation, no side effects)

### Unknown Operations ✅

**Unknown Calls:**
- `custom_function(x)` → Unknown (conservative)

**Fallback Nodes:**
- `raw` nodes → Unknown

---

## Effect Propagation

### Unary Operators

Inherit from operand:
```r
-x # Pure if x pure
-x[i] # ReadsMem if x[i] ReadsMem
```

### Binary Operators

Union of operand effects:
```r
x[i] + y[j] # ReadsMem(x) ∪ ReadsMem(y)
```

### Statements

Combine LHS and RHS effects:
```r
out[i] <- x[j] # WritesMem(out) ∪ ReadsMem(x)
```

---

## Optimization Rules

### CSE (Common Subexpression Elimination)

- **Pure expressions:** Can be CSE'd freely
- **ReadsMem expressions:** Can be CSE'd only with alias analysis
- **RNG expressions:** Cannot be CSE'd
- **Unknown expressions:** Conservative (no CSE)

### LICM (Loop-Invariant Code Motion)

- **Pure expressions:** Can be hoisted out of loops
- **ReadsMem expressions:** Can be hoisted only with proven non-interference
- **RNG expressions:** Cannot be hoisted
- **Unknown expressions:** Conservative (no hoisting)

### DCE (Dead Code Elimination)

- **Pure expressions:** Can be eliminated if unused
- **ReadsMem expressions:** Can be eliminated if unused
- **RNG expressions:** Cannot be eliminated
- **Unknown expressions:** Conservative (no elimination)

## Representative Outcomes and Diagnostics

Effect system behavior usually appears as optimization outcomes, not hard errors:

- CSE/LICM/DCE no-op because expression includes `RNG` or `Unknown`.
- LICM read-hoist blocked because read resources overlap loop writes.
- DCE retention because RHS is not droppable under legality mapping.

Related diagnostic surfaces when legality reaches verifier boundaries:
- `IR verify [scheduled_reduce]: dtype is invalid for mode`
- `IR verify [raw]: raw fallback node not allowed in ir_only mode`

---

## Implementation Details

### Effect Analysis

Effect analysis is performed structurally on IR nodes:

```r
.mojor_ir_expr_effects <- function(node) {
 switch(class(node),
 "const" = c("Pure"),
 "var" = c("Pure"),
 "binop" = union(.mojor_ir_expr_effects(node$lhs),
 .mojor_ir_expr_effects(node$rhs)),
 "unop" = .mojor_ir_expr_effects(node$expr),
 "call" = .mojor_ir_call_effects(node$fn, node$args),
 "index" = c("ReadsMem"),
 "raw" = c("Unknown")
 )
}
```

### Helper Functions

```r
.mojor_ir_is_pure <- function(node) {
 all(.mojor_ir_expr_effects(node) == "Pure")
}

.mojor_ir_has_rng <- function(node) {
 "RNG" %in% .mojor_ir_expr_effects(node)
}

.mojor_ir_reads_mem <- function(node) {
 "ReadsMem" %in% .mojor_ir_expr_effects(node)
}
```

---

## Testing

- **Test file:** `packages/mojor/tests/testthat/test_ir_effects.R`
- **Test count:** 50 tests
- **Status:** All passing

---

## Related Documents

- [CONTRACT.md](./CONTRACT.md) - IR Contracts
- [SPEC.md](./SPEC.md) - IR Specification
- [IMPLEMENTATION.md](./IMPLEMENTATION.md) - Implementation Details
