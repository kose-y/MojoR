# IR Matrix and Array Output Design

**Status:** Implemented for canonical matrix and fixed-rank ND output routes.

## Plain-Language Mental Model

Matrix/array output support means strict IR lanes can lower and emit covered
index/slice output writes directly.

- Matrix and fixed-rank ND output declarations are tracked in IR/layout context.
- Covered index/slice writes are emitted as IR-native tensor/index operations.
- Missing-index matrix slices (`mat[i, ]`, `mat[, j]`) are handled on dedicated routes.

## Mini Example

```r
for (i in 1:n) {
  mat[i, ] <- c(x[i], y[i])
}
```

Pipeline shape:
1. Build subscript/missing-index + constructor nodes.
2. Lower to explicit index/slice assignments with layout metadata.
3. Emit tensor-indexed writes against the output target.

## Beginner Debug Checklist

1. Confirm output declaration is canonical (`matrix(...)` / `array(..., dim=...)`).
2. Check index form is on covered routes (`[i,j]`, `[i, ]`, `[, j]`, canonical fixed-rank ND slices).
3. Verify layout metadata is present before emit.
4. For failures, inspect lowering first, then emit/index helpers.

## Covered Routes

- Matrix 1D/2D output indexing on strict IR lanes.
- Missing-index matrix slices (`mat[i, ]`, `mat[, j]`) on covered routes.
- Canonical fixed-rank ND slice/index writes.
- Canonical RHS indexed-slice assignment forms.
- Supported strict computed `i32[]` selector arithmetic lanes (`+`, `-`, `*`, `%/%`, `%%`).
- Supported strict `ifelse(cond, yes, no)` selector lanes when condition/branch lanes are covered.

## Deferred / Route-Constrained

- Advanced non-canonical vector-indexed ND gather/scatter write shapes.
- Non-canonical RHS expression shapes beyond direct indexed-slice routes.
- Broader non-canonical computed array-typed index expressions in strict lanes.

For latest constraints, use:
- `docs/SUBSET.md`
- `docs/KNOWN_ISSUES.md`
- `docs/IR/IMPLEMENTATION.md`

## Implementation Mapping

- Build/lowering: `packages/mojor/R/ir/build.R`, `packages/mojor/R/ir/lowering.R`
- Index/slice emit: `packages/mojor/R/ir/index_emit.R`, `packages/mojor/R/ir/stmt_emit.R`
- Layout context threading: `packages/mojor/R/transpile/ir_helpers.R`

## Related Documents

- [../IR/IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Current IR behavior and coverage
- [../PIPELINE/LIR.md](../PIPELINE/LIR.md) - Lowering contracts
- [../PIPELINE/EMISSION.md](../PIPELINE/EMISSION.md) - Emission contracts
