# Loop Fusion Design

**Status:** ✅ Implemented ( )
**Date:** 2026-02-10
**Last Updated:** 2026-02-17
**Purpose:** Specification for loop fusion optimization pass over Tree IR

---

## Plain-Language Mental Model

Fusion tries to combine two nearby loops into one loop when it is safe.

- Goal: fewer passes over memory and less loop overhead.
- Constraint: only fuse when semantics stay identical.
- Method: apply strict legality checks before rewrite.

If any safety check fails, loops stay separate.

## Executive Summary

Loop fusion combines adjacent loops that operate on the same iteration domain, reducing memory traffic and loop overhead. This document specifies the **elem0 normalization scheme** and **fusion legality rules** that enable safe, robust fusion across different loop forms (ordinary R, broadcast_nd, etc.).

**Key insights:**
- **Canonical elem0 space** — All loops normalized to `j ∈ [0, n)` for fusion analysis
- **Separation of concerns** — `out_elem0` (fusion alignment) vs `idx_map` (memory effects)
- **NonAffine Pure expressions** — Don't block fusion (e.g., `_mojor_bcast_index()`)
- **Six fusion conditions** — Domain, access pattern, noalias, no effects, guard policy, sequential composition

## Mini Example

Before:
- Loop A computes intermediate value.
- Loop B consumes that intermediate value immediately for same index range.

After legal fusion:
- One loop computes intermediate and final output in a single iteration body.

Benefit: less memory traffic and one loop-control path instead of two.

## Beginner Debug Checklist

1. Confirm loops are truly adjacent and share the same domain.
2. Confirm LHS access patterns match in canonical elem0 space.
3. Check rejection code first; it points directly to the failing legality condition.
4. Treat any RNG/Unknown effect in fusion region as an expected hard blocker.

---

## 1. Motivation

**Why fusion matters:**

Without fusion:
```r
# Loop 1: normalize
for (i in 1:n) out[i] <- x[i] / max_val

# Loop 2: threshold
for (i in 1:n) out[i] <- ifelse(out[i] > threshold, out[i], 0.0)
```

**Problems:**
- Two passes over `out` (write in Loop1, read+write in Loop2)
- Cache pollution between loops
- Loop overhead (bounds checks, induction variable maintenance) duplicated

With fusion:
```r
# Fused: normalize + threshold
for (j in 0:(n-1)) {
 tmp <- x[j] / max_val
 out[j] <- ifelse(tmp > threshold, tmp, 0.0)
}
```

**Benefits:**
- Single pass over `out` (write once)
- `tmp` stays in register (no memory traffic)
- Loop overhead paid once
- Better instruction-level parallelism

---

## 2. elem0 Normalization Scheme

**Core challenge:** Loops can have different induction variable conventions:
- Ordinary R: `i ∈ [1, n+1)` with `one_based` indices
- Broadcast_nd: `i ∈ [1, n+1)` but `i ∈ zero_based_vars` (runtime 0-based)
- Zero-based explicit: `j ∈ [0, n)` with `zero_based` indices

**Solution:** Normalize all loops to a canonical **elem0 space** `[0, n)`.

### elem0 Derivation Rules

Given a loop with:
- Induction variable `i`
- Range `range(start, end, step)`
- `index_base` metadata on accesses
- `i_runtime_zero_based` flag (from `metadata$broadcast_nd` or `zero_based_vars`)

**Canonical elem0 domain:**
```r
elem0_domain <- list(
 start = 0,
 end_exclusive = n, # derived from loop range
 step = 1, # must be 1 for fusion
 order = "increasing"
)
```

**elem0 for LHS access `out[i + k]`:**
```r
if (i_runtime_zero_based) {
 elem0 = i + k # Runtime override: i is already 0-based
} else if (index_base == "one_based") {
 elem0 = (i + k) - 1 # Convert R 1-based to 0-based
} else { # index_base == "zero_based"
 elem0 = i + k # Already 0-based
}
```

**Key insight:** `i_runtime_zero_based` **overrides** `index_base="one_based"`.

### Broadcast_nd Example

For a broadcast_nd loop:
```r
# Tree IR:
loop(var = i, range = range(1, n+1), body = ..., metadata = list(broadcast_nd = TRUE))
```

**elem0 derivation:**
```r
# Runtime: i is 0-based (from zero_based_vars)
elem0 = i # No conversion needed
```

---

## 3. Fusion Legality Rules

### 3.1 Domain Match

**Rule:** Loops must have identical iteration domains.

**Check:**
```r
domain_match <- function(loop1, loop2) {
 loop1$range$start == loop2$range$start &&
 loop1$range$end == loop2$range$end &&
 loop1$range$step == loop2$range$step
}
```

### 3.2 Access Pattern Match

**Rule:** LHS accesses must use the same elem0 expression.

**Check:**
```r
access_pattern_match <- function(loop1, loop2) {
 lhs1_elem0 <- compute_elem0(loop1$lhs)
 lhs2_elem0 <- compute_elem0(loop2$lhs)
 lhs1_elem0 == lhs2_elem0
}
```

### 3.3 Noalias Check

**Rule:** LHS resources must be disjoint or identical.

**Check:**
```r
noalias_check <- function(loop1, loop2) {
 lhs1_resource <- get_resource(loop1$lhs)
 lhs2_resource <- get_resource(loop2$lhs)
 lhs1_resource == lhs2_resource || is_noalias(lhs1_resource, lhs2_resource)
}
```

### 3.4 Effect Check

**Rule:** No RNG or Unknown effects in fusion region.

**Check:**
```r
effect_check <- function(loop1, loop2) {
 !has_rng(loop1$body) && !has_unknown(loop1$body) &&
 !has_rng(loop2$body) && !has_unknown(loop2$body)
}
```

### 3.5 Guard Policy

**Rule:** Guard emission must be consistent across fused loops.

**Check:**
```r
guard_policy_check <- function(loop1, loop2) {
 loop1$na_guard == loop2$na_guard &&
 loop1$bounds_check == loop2$bounds_check
}
```

### 3.6 Sequential Composition

**Rule:** Loops must be adjacent in the AST.

**Check:**
```r
sequential_check <- function(stmts, loop1, loop2) {
 idx1 <- which(sapply(stmts, identical, loop1))
 idx2 <- which(sapply(stmts, identical, loop2))
 idx2 == idx1 + 1
}
```

---

## 4. Fusion Rejection Codes

**Stable rejection codes:**
- `FUSE_REJECT_DOMAIN_MISMATCH` — Loops have different iteration domains
- `FUSE_REJECT_INDEXMAP_NOT_IDENTITY` — Index maps don't match
- `FUSE_REJECT_NOALIAS_MISSING` — Noalias information missing
- `FUSE_REJECT_CONTROL_FLOW` — Tree IR legality rejected loop body control flow
- `FUSE_REJECT_EFFECTS_RNG` — RNG effect blocks fusion
- `FUSE_REJECT_EFFECTS_UNKNOWN` — Unknown effect blocks fusion
- `FUSE_REJECT_BROADCAST_ND` — Reserved analysis/legality rejection for broadcast_nd
- `FUSE_REJECT_GUARD_POLICY` — Guard policy mismatch

---

## 5. Implementation Status

** (2026-02-14):** Full implementation of loop fusion optimization.

### What's Implemented

- **elem0 normalization** - Canonical index space conversion for all loop types
- **Loop domain extraction** - Support for both flat and nested loop structures
- **Domain matching** - Identical iteration domain verification
- **Access pattern matching** - LHS elem0 expression comparison
- **Noalias checking** - Resource disjointness verification
- **Effect checking** - RNG/Unknown effect detection and rejection
- **Guard policy checking** - NA guard and bounds check consistency
- **Sequential composition** - Adjacent loop detection
- **Control flow detection** - Loop with control flow rejection

### Fusion Legality Rules

All six fusion conditions must be satisfied:

1. **Domain Match** - Loops must have identical iteration domains
2. **Access Pattern Match** - LHS accesses must use the same elem0 expression
3. **Noalias Check** - LHS resources must be disjoint or identical
4. **Effect Check** - No RNG or Unknown effects in fusion region
5. **Guard Policy** - Guard emission must be consistent across fused loops
6. **Sequential Composition** - Loops must be adjacent in the AST

### Fusion Rejection Codes

Stable rejection codes for diagnostic messages:
- `FUSE_REJECT_DOMAIN_MISMATCH` — Loops have different iteration domains
- `FUSE_REJECT_INDEXMAP_NOT_IDENTITY` — Index maps don't match
- `FUSE_REJECT_NOALIAS_MISSING` — Noalias information missing
- `FUSE_REJECT_CONTROL_FLOW` — Tree IR legality rejected loop body control flow
- `FUSE_REJECT_EFFECTS_RNG` — RNG effect blocks fusion
- `FUSE_REJECT_EFFECTS_UNKNOWN` — Unknown effect blocks fusion
- `FUSE_REJECT_BROADCAST_ND` — Reserved analysis/legality rejection for broadcast_nd
- `FUSE_REJECT_GUARD_POLICY` — Guard policy mismatch

### Experimental Opt-In Legality Gates

Default behavior remains conservative and backward-compatible. Two experimental
gates can be enabled via `mojor_options()`:

- `fusion_allow_control_flow_simple = TRUE` (default `FALSE`)
- `fusion_allow_broadcast_nd_identity = TRUE` (default `FALSE`)

Current scope:
- Tree IR legality accepts control flow only for a narrowly safe subset:
 `simple_if_assign` loops (single `if`, one assign per branch, same target
 shape/resource, no nested control flow, and no RNG/Unknown condition effects).
- SSA fusion candidate analysis keeps stable rejection codes by default, and can
 suppress:
 - `FUSE_REJECT_CONTROL_FLOW` for simple-if-classified candidates when
 `fusion_allow_control_flow_simple=TRUE`
 - `FUSE_REJECT_BROADCAST_ND` only for identity-safe `broadcast_nd` candidates
 when `fusion_allow_broadcast_nd_identity=TRUE`

### Usage

```r
# Enable fusion in mojor_build or mojor_transpile
built <- mojor_build(
 fn,
 n = "i32",
 fusion = TRUE # Enable loop fusion optimization
)
```

### Test Coverage

See [`packages/mojor/tests/testthat/test_fusion.R`](../../packages/mojor/tests/testthat/test_fusion.R) for comprehensive test coverage:
- elem0 normalization tests
- Loop domain extraction tests
- Noalias checking tests
- Fusion legality tests
- Runtime tests with fusion enabled/disabled

### Known Limitations

- Multiple loops with different iteration domains cannot be fused.
- Control-flow loops reject by default (`FUSE_REJECT_CONTROL_FLOW`) unless they satisfy opt-in `simple_if_assign` legality.
- `broadcast_nd` candidates reject by default (`FUSE_REJECT_BROADCAST_ND`) unless identity-safe opt-in checks pass.
- Alias/effect uncertainty remains a conservative reject path for safety.
- Use `docs/KNOWN_ISSUES.md` as the canonical consolidated limitations list.

---

## 6. Deferred Items

### dim() Function ( )

**Status:** Deferred to post-P3

**Description:** `dim()` handling in strict lanes is now route-constrained to
direct array variables with available dim metadata. Broader `dim()` expression
sources remain deferred.

Canonical tracking for this and other deferred strict-lane constraints lives in
`docs/KNOWN_ISSUES.md`.

- **Current approach:** Emission resolves direct dim-metadata pointers for
  covered array variables and uses those in indexed/dim-bound routes.
- **Deferred alternative:** Use an explicit `alloc`-based IR node to materialize
  standalone dimension vectors for broader dynamic `dim()` sources.

**Why deferred:**
- Current direct-pointer routes cover strict canonical `dim(arg)[k]` and
  related loop-bound forms without extra vector allocation.
- General `dim()` over non-direct or metadata-untracked sources would require:
 - Emitting allocation/materialization of dimension vectors
 - Defining lifetime/ownership and typing contracts for those vectors
 - Additional legality checks across lowering and emission

**Implementation notes:**
- See [`packages/mojor/R/ir/expr_emit.R`](../../packages/mojor/R/ir/expr_emit.R) for current implementation
- See [`packages/mojor/tests/testthat/test_dim_lowering.R`](../../packages/mojor/tests/testthat/test_dim_lowering.R) for tests

---

## 7. Related Documents

- [IR Contract](../IR/CONTRACT.md) - IR Contracts
- [EFFECT_SYSTEM.md](../IR/EFFECT_SYSTEM.md) - Effect System
- [IR Plan](../IR/PLAN.md) - IR direction and integration context
