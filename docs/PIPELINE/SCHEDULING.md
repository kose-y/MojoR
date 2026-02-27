# Scheduling

Scheduling attaches execution metadata and routing choices after lowering.

## Plain-Language Mental Model

Scheduling decides "how to run" already-correct logic.

- It does not change what the program computes.
- It changes execution strategy, such as tiling, unrolling, and reduction route.

Think of this stage as choosing a driving route, not changing the destination.

## What Scheduling Controls

- Loop unroll (`unroll`)
- Loop tile sizes (`tile`)
- Reduction route (`linear`, `tree`, `simd`)
- Vectorization hints (`simd_mode`, alignment)
- Parallel route (`parallel` where legal)

## Auto Policy

When controls are not explicit, policy logic may infer settings from:
- `opt_level`
- loop shape and safety
- reduction eligibility
- dtype/operator compatibility

## Reduction Scheduling

Eligible scalar reductions can be rewritten to scheduled forms (`scheduled_reduce`) for tree/SIMD routes.

## Mini Example

Before scheduling:

```text
sum = linear_reduce(x)
```

After scheduling (if legal and enabled):

```text
sum = scheduled_reduce(mode="tree", x)
```

Result is the same. Execution plan changes.

## Safety and Legality

- Schedule changes are type-agnostic at this stage.
- Legality checks ensure route choices are valid for the current IR and metadata.

## Code Mapping

- Auto policy logic: `packages/mojor/R/transpile/entry_stages.R`
- Schedule context/pipeline helpers: `packages/mojor/R/transpile/ir_helpers.R`
- IR schedule transforms: `packages/mojor/R/ir/nodes.R`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Auto policy selection | `.mojor_apply_auto_schedule_policy(...)` in `packages/mojor/R/transpile/entry_stages.R` | Chooses effective unroll/tile/reduction modes when caller leaves them implicit. |
| Schedule context wiring | `.mojor_build_transpile_layout_ctx(...)` in `packages/mojor/R/transpile/ir_helpers.R` | Injects type/layout metadata into schedule-aware lowering and emission stages. |
| Schedule application | `.mojor_ir_schedule(...)` in `packages/mojor/R/ir/nodes.R` | Propagates schedule metadata through loops/expressions/statements. |
| Reduction route rewrite | `.mojor_ir_schedule_tree_reduce(...)`, `.mojor_ir_schedule_simd_reduce(...)` in `packages/mojor/R/ir/nodes.R` | Rewrites eligible `scalar_reduce` into `scheduled_reduce` route forms. |
| Tile rewrites | `.mojor_ir_schedule_tile_loop(...)` in `packages/mojor/R/ir/nodes.R` | Performs strip-mining/tiling loop transformation when tile metadata is valid. |

## Schedule Contract

- Schedule does not redefine semantics; it only changes legal execution strategy and metadata.
- Invalid schedule directives are pruned or rejected instead of partially applied.
- Reduction mode rewrite is guarded by operator/dtype/associativity constraints.

## Diagnostics and Guardrails

- Auto mode records effective policy decisions for transparency in transpile outputs.
- SIMD/tree reduction requests no-op when legality checks fail (falls back to linear behavior).
- Tile rewrites are conservative and require canonicalizable ranges.

## Representative Scheduling Diagnostics

Entry-stage schedule validation messages include:

- `mojor_transpile: unroll must be an integer between 1 and 16`
- `mojor_transpile: tile must be a numeric vector of length 1, 2, or 3`
- `mojor_transpile: tile values must be positive integers`

Scheduling-specific legality failures often appear as verify-time reduction contract errors:

- `IR verify [scheduled_reduce]: mode must be one of tree/simd`
- `IR verify [scheduled_reduce]: dtype is invalid for mode`

If tree/SIMD does not apply and no hard error appears, treat it as legal no-op fallback unless diagnostics say otherwise.

## Beginner Debug Checklist

1. Confirm schedule knobs were actually set or inferred (`unroll`, `tile`, `reduction`).
2. If SIMD/tree does not apply, check legality and dtype/operator constraints first.
3. If tiling does not apply, inspect whether loop ranges are canonicalizable.
4. Compare effective schedule metadata before and after scheduling to see what was accepted.

## Internal Symbol Index

`Policy/entry-stage symbols`
- `.mojor_prune_schedule_nulls`
- `.mojor_apply_auto_schedule_policy`

`Schedule transform symbols`
- `.mojor_ir_schedule_merge`
- `.mojor_ir_schedule_strip_tile`
- `.mojor_ir_schedule_tile_inner_range`
- `.mojor_ir_schedule_tile_loop`
- `.mojor_ir_schedule_tree_reduce`
- `.mojor_ir_schedule_simd_reduce`
- `.mojor_ir_schedule`
- `.mojor_ir_scheduled_reduce`
