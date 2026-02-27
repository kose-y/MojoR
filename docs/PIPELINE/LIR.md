# LIR (Lower-Level Tree IR)

LIR is the lowered Tree IR subset used for scheduling, typing, and emission.

## Purpose

LIR removes high-level sugar and leaves stable, core-oriented forms.

- Closer to direct code emission.
- Less ambiguity in indexing and loop behavior.
- Easier to enforce typed emission contracts.

## Plain-Language Mental Model

LIR is the "near-executable IR."

- HIR is expressive and convenient.
- LIR is explicit and predictable.

If HIR is a design sketch, LIR is the engineered drawing ready for manufacturing steps.

## HIR to LIR Lowering

Lowering handles transformations such as:
- High-level reductions into core loop-compatible forms.
- Subscript/slice assignment expansion.
- Vector-expression (`vexpr`) expansion to explicit loops.
- Dimension/index rewrites using layout context maps.

## Mini Example

A high-level helper expression can become explicit loop logic in LIR:

- HIR: a compact vector expression node (`vexpr`).
- LIR: explicit loop, index calculation, and assignment steps.

This makes downstream scheduling/emission straightforward because implicit behavior is removed.

## Key Context Inputs

Lowering depends on layout/index metadata:
- `len_var_map`
- `nrow_var_map`
- `ncol_var_map`
- `dim_var_map`
- tensor and dim maps
- `index_base`
- `array_layout`

## LIR Contract

- Contains core forms legal for scheduling/type/emission.
- Avoids unresolved sugar that emitter cannot directly handle.
- Preserves semantics required by R-facing behavior.

## Code Mapping

- Lowering implementation: `packages/mojor/R/ir/lowering.R`
- Lowering orchestration: `packages/mojor/R/transpile/ir_helpers.R`
- IR shape references: `docs/IR/SPEC.md`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Main lowering dispatcher | `.mojor_ir_lower(...)` in `packages/mojor/R/ir/lowering.R` | Routes each node kind to its lowerer and recursively lowers child nodes. |
| Subscript/slice lowering | `.mojor_ir_lower_subscript(...)` in `packages/mojor/R/ir/lowering.R` | Expands subscript/slice forms into explicit loop/index-compatible IR. N-d exclusion-heavy subscripts (`neg_exclusion`, `neg_vec_exclusion` plus scalar/missing dims) pass through unchanged for emission-layer handling; `:` range selectors stay on canonical slice/index lowering routes. |
| Reduction lowering | `.mojor_ir_lower_scalar_reduce(...)` in `packages/mojor/R/ir/lowering.R` | Converts supported reductions into core reduction-compatible forms. |
| GPU preview/core lowerers | `.mojor_ir_lower_gpu_reduce*`, `.mojor_ir_lower_gpu_matmul*` in `packages/mojor/R/ir/lowering.R` | Rewrites GPU helper forms into explicit IR patterns used downstream. |
| Layout/dim rewrite helpers | `.mojor_ir_lower_dim_stmt(...)` in `packages/mojor/R/ir/nodes.R` | Resolves `dim`-adjacent helpers with transpile-provided layout context. |

## LIR Contract Details

- LIR should be schedule-ready (no unresolved structural sugar that schedule/emitter cannot interpret).
- LIR preserves structured control flow; only representation of expressions/statements is lowered.
- Lowering must retain R-facing semantics while making index/dimension behavior explicit.

## Common Lowering Diagnostics

- Unsupported sugar forms or inconsistent helper metadata.
- Missing layout/type context required for dimension/index rewrites.
- Strict-mode lowering errors that cannot route through non-strict fallback policies.

## Representative Lowering Diagnostics

Common lowering-time messages include:

- `IR lower [vapply]: missing FUN`
- `IR lower [sapply]: missing FUN`
- `IR lower [lapply]: missing FUN`
- `IR lower [mapply]: args must be a list`
- `IR lower [mapply]: requires at least two vector arguments in the compiled subset`

Some index/layout rewrite failures surface with transpile-prefixed messages when layout metadata is inconsistent:

- `mojor_transpile: character indexing requires dimnames/names for '<var>'`
- `mojor_transpile: index name '<name>' not found in dimnames for '<var>'`
- `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)`
- Missing-index forms (for example `x[, j]` or `x[i, , k]`) are traversed positionally; `drop`/`exact` control args are excluded from index-position counting.

## Beginner Debug Checklist

1. If a node fails emission later, confirm it was lowered out of HIR sugar form.
2. Check that layout context maps (`len`, `nrow`, `ncol`, `dim`) are present when needed.
3. For indexing issues, verify index-position mapping and dimname availability for the referenced axis.
4. Verify `index_base` and array-layout assumptions in lowering context.
5. In strict mode, isolate the first unsupported sugar node and map it to missing lowerer ownership.

## Internal Symbol Index

`Primary lowering entrypoints`
- `.mojor_ir_lower`
- `.mojor_ir_lower_subscript`
- `.mojor_ir_lower_scalar_reduce`
- `.mojor_ir_lower_vexpr`

`GPU lowering families`
- `.mojor_ir_lower_gpu_reduce`
- `.mojor_ir_lower_gpu_reduce_preview_expr`
- `.mojor_ir_lower_gpu_reduce_assign`
- `.mojor_ir_lower_gpu_reduce_expr`
- `.mojor_ir_lower_gpu_matmul`
- `.mojor_ir_lower_gpu_matmul_preview_assign`
- `.mojor_ir_lower_gpu_matmul_preview_return`
- `.mojor_ir_lower_gpu_matmul_assign`

`Dimension/layout rewrite helpers`
- `.mojor_ir_lower_dim_expr`
- `.mojor_ir_lower_dim_stmt`
- `.mojor_ir_lower_dim_block`
- `.mojor_ir_layout_ctx`
