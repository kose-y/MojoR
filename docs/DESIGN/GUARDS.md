# Guard Pass Design

## Goal
Move bounds checking and NA guards into the IR layer to:
1. Remove `na_guard != "forbid"` restriction from IR wiring
2. Centralize guard logic in one place
3. Make IR more widely applicable
4. Enable future optimizations (guard elimination, hoisting)

**Status:** Emission-time guard strategy is implemented with statement-scope guard deduplication.

## Plain-Language Mental Model

Guards are runtime safety checks generated from IR context.

- Bounds guards stop invalid index access.
- NA guards enforce selected NA policy.

Instead of scattering checks across many codegen branches, guard logic is centralized.

## Mini Example

For `out[i] <- x[i] + y[i]`:

- Bounds guard may check `i` against valid range.
- NA guard may check inputs before arithmetic, depending on policy.
- Then the assignment executes.

Without guard policy, invalid index/NA behavior can become inconsistent across paths.

## Beginner Debug Checklist

1. Confirm active policy knobs (`na_guard`, `bounds_check`).
2. Verify whether guard emission is expected for the specific node shape.
3. If duplicate checks appear, inspect statement-scope guard CSE behavior.
4. For missing checks, confirm required layout/type context exists at emission time.

## Overview
The guard pass is a **transformation** that takes typed IR and inserts explicit guard checks before operations that need them. Guards are represented as IR nodes that emit checking code.

## Guard Types

### 1. NA/NaN Guards
Check for NA/NaN/Inf values in expressions before using them.

**Modes:**
- `forbid`: Error on NA/NaN (default for most ops)
- `assign`: Assign NA result when NA input detected
- `unsafe`: Skip all NA checks

**Example:**
```r
# R code:
out[i] <- x[i] + y[i]

# With NA guard (forbid mode):
if is_nan(x[i]) || is_nan(y[i]):
 _mojor_error_na()
out[i] = x[i] + y[i]
```

### 2. Bounds Guards
Check array indices are within valid range.

**Example:**
```r
# R code:
out[i] <- x[i]

# With bounds guard:
if i < 1 || i > len(x):
 _mojor_error_bounds()
out[i-1] = x[i-1]
```

## IR Node Design

### Guard Node
A new IR node type that wraps expressions with runtime checks:

```r
list(
 kind = "guard",
 check = <guard_expression>, # Boolean expression to check
 error_kind = "na" | "bounds", # Type of error
 expr = <guarded_expression>, # The expression being guarded
 src = ...
)
```

**Alternative approach** (simpler, used initially):
Don't create explicit guard nodes. Instead, generate guard checks during emission based on:
- Expression type (index, binop with NA-producing ops)
- NA guard mode from context
- Whether bounds checking is enabled

## Guard Insertion Strategy

### Approach 1: Explicit Guard Nodes (Future)
Insert guard nodes during a dedicated pass:

```r
.mojor_ir_insert_guards <- function(node, na_guard = "forbid", bounds_check = TRUE) {
 # Walk IR and wrap operations that need guards
 if (node$kind == "binop" && node$op %in% c("+", "-", "*", "/")) {
 # Wrap with NA guard
 if (na_guard == "forbid") {
 return(.mojor_ir_guard_na(node))
 }
 }
 if (node$kind == "index") {
 # Wrap with bounds guard
 if (bounds_check) {
 return(.mojor_ir_guard_bounds(node))
 }
 }
 return(node)
}
```

### Approach 2: Emission-Time Guards (Current)
Generate guards during emission based on context:

```r
.mojor_ir_expr_emit <- function(node, zero_based_vars = NULL, na_guard = "forbid") {
 if (node$kind == "index") {
 # Generate bounds check if needed
 if (should_check_bounds(node, zero_based_vars)) {
 emit_bounds_check(node)
 }
 }
 # Emit the expression
 ...
}
```

## Guard CSE (Common Subexpression Elimination)

Guards can be deduplicated per-statement:
- Same guard expression → emit once
- Guard caching reset between statements

## Representative Guard/Policy Diagnostics

Guard-related failures usually surface as verifier/emission/indexing contract errors:

- `IR verify [index]: indices list cannot be empty`
- `IR verify [index]: base must be a var or constructor node`
- `mojor_transpile: character indexing requires dimnames/names for '<var>'`
- `mojor_transpile: index name '<name>' not found in dimnames for '<var>'`
- `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)`

Policy misconfiguration examples:

- `mojor_options: index_bounds must be TRUE or FALSE`
- `mojor_options: warn_ifelse must be TRUE or FALSE`

## Implementation Mapping

- Bounds and index-guard emission helpers: `packages/mojor/R/ir/bounds_emit.R`, `packages/mojor/R/ir/index_emit.R`
- NA guard helpers: `packages/mojor/R/ir/type_guards.R`
- Expression/statement emission integration: `packages/mojor/R/ir/expr_emit.R`, `packages/mojor/R/ir/stmt_emit.R`
- Guard-related index legality checks: `packages/mojor/R/ir/verify.R`

---

## Related Documents

- [IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Implementation Details
- [EFFECT_SYSTEM.md](../IR/EFFECT_SYSTEM.md) - Effect System
