# Effect, Resource, and Legality Model

This model decides whether rewrites like CSE/LICM/DCE/fusion are safe.

## Plain-Language Mental Model

This model answers one question: "Can we safely rewrite this?"

- Effects describe what an operation does.
- Resources describe what data/state it touches.
- Legality classes convert that into optimizer rules.

If safety is uncertain, optimization is blocked on purpose.

## Effect Classes (What an operation does)

Tree expression effects include:
- `Pure`: deterministic and side-effect free.
- `ReadsMem`: reads memory/state.
- `RNG`: depends on RNG state; ordering matters.
- `Unknown`: not proven safe.

Additional labels exist in specific contexts (for example status/write tracking).

## Resource Tracking (What it touches)

Resources are identifiers such as:
- `var:<name>`
- `rng:state`
- SSA-level ids (`arg_slot:<n>`, `ssa_value:<n>`, `symbol:<name>`)

Resource info is used to reason about read/write conflicts and aliasing risk.

## Legality Classes (What optimizer may do)

Effect classes are mapped into legality classes used by passes:
- `None`: freely reusable/reorderable in legal scope.
- `Read`: reads state; motion depends on write interference proof.
- `Write`: writes state; ordering constraints apply.
- `RNG`: no reordering/elimination.
- `Unknown`: conservative barrier.

## How Passes Use This

CSE:
- Requires legality safe for reuse and no unsafe effects.

LICM:
- Hoists pure invariants.
- Hoists reads only when non-interference is proven.

DCE:
- Removes unused assignments only when RHS effects are droppable.

Fusion:
- Requires no blocked effects and legal resource disjointness/compatibility.

## Mini Example

Case A:
- Expression reads immutable inputs only.
- Effect class maps to legality `None`.
- CSE/LICM/DCE may proceed (subject to scope rules).

Case B:
- Expression consumes RNG state.
- Effect includes `RNG`.
- Reorder/reuse/drop is blocked.

Same algebraic shape can be treated differently depending on effects/resources.

## Code Mapping

- Tree effects/resources and helpers: `packages/mojor/R/ir/nodes.R`
- Fusion legality checks: `packages/mojor/R/ir/fusion.R`
- SSA effect/resource annotation: `packages/mojor/R/ir/ssa.R`
- Reference design: `docs/IR/EFFECT_SYSTEM.md`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Tree expression effects | `.mojor_ir_expr_effects(...)` in `packages/mojor/R/ir/nodes.R` | Computes effect tags (`Pure`, `ReadsMem`, `RNG`, `Unknown`, etc.) from expression trees. |
| Tree resource identity | `.mojor_ir_expr_resources(...)` in `packages/mojor/R/ir/nodes.R` | Extracts resource IDs for alias/interference reasoning. |
| Effect -> legality mapping | `.mojor_ir_map_effect_classes(...)` in `packages/mojor/R/ir/nodes.R` | Converts effect tags into optimizer legality classes (`None`, `Read`, `Write`, `RNG`, `Unknown`). |
| Pass legality gates | `.mojor_ir_cse_legal_expr(...)`, `.mojor_ir_dce_legal_rhs(...)`, `.mojor_ir_licm_reads_legal(...)` in `packages/mojor/R/ir/nodes.R` | Enforces per-pass legality policy. |
| Fusion legality | `.mojor_ir_fusion_legality(...)` and helpers in `packages/mojor/R/ir/fusion.R` | Applies disjointness/effect guardrails for loop fusion candidates. |
| SSA-side annotation | `.mojor_ssa_annotate_effects_resources(...)` in `packages/mojor/R/ir/ssa.R` | Attaches effect/resource summaries after SSA canonicalization path. |

## Legality Contract

- Any `RNG` or `Unknown` class blocks reorder/reuse/drop transforms.
- `Read` class transforms require explicit non-interference proof against writes.
- `Write` class operations are ordering-sensitive and are never treated as pure.

## Practical Debugging

- Unexpected optimizer no-ops are usually legality barriers, not pass bugs.
- Check resource IDs first when read-hoist or fusion opportunities are rejected.
- Use SSA annotation output to compare Tree and SSA legality classifications when debugging backend-prep behavior.

## Representative Legality Outcomes

- `RNG` present -> CSE/LICM/DCE reorder/reuse/drop blocked.
- `Unknown` present -> conservative barrier, no speculative rewrite.
- `Read` with resource overlap against loop writes -> LICM read-hoist blocked.
- `None` (pure, non-interfering) -> eligible for normal optimization scope rules.

When behavior looks "too conservative," verify effect/resource classification first before changing pass logic.

## Beginner Debug Checklist

1. Identify effect class first (`Pure`, `ReadsMem`, `RNG`, `Unknown`).
2. Identify resources touched by the expression.
3. Map to legality class and verify pass-specific gate.
4. If rewrite is blocked, treat it as safety policy unless evidence shows wrong effect/resource classification.

## Internal Symbol Index

`Effect/resource classification (Tree IR)`
- `.mojor_ir_expr_effects`
- `.mojor_ir_is_pure`
- `.mojor_ir_has_rng`
- `.mojor_ir_reads_mem`
- `.mojor_ir_map_effect_classes`
- `.mojor_ir_resource_from_name`
- `.mojor_ir_expr_resources`
- `.mojor_ir_expr_effect_resource_summary`
- `.mojor_ir_rng_resource`
- `.mojor_ir_is_rng_call_fn`

`Pass legality gates`
- `.mojor_ir_cse_legal_expr`
- `.mojor_ir_dce_legal_rhs`
- `.mojor_ir_collect_written_resource_ids_stmt`
- `.mojor_ir_collect_written_resource_ids_block`
- `.mojor_ir_licm_reads_legal`
- `.mojor_ir_is_loop_invariant`
- `.mojor_ir_loop_fusable`

`Fusion legality and guard rails`
- `.mojor_ir_fusion_enabled`
- `.mojor_ir_fuse_candidates`
- `.mojor_ir_fusion_legality`
- `.mojor_ir_resources_disjoint`
- `.mojor_ir_noalias_check`
- `.mojor_ir_effects_check`
- `.mojor_ir_guard_policy_check`
- `.mojor_ir_sequential_check`
- `.mojor_ir_access_pattern_match`

`SSA-side effect/resource summaries`
- `.mojor_ssa_stmt_effect_resource_summary`
- `.mojor_ssa_annotate_effects_resources`
- `.mojor_ssa_fusion_loop_effect_flags`
- `.mojor_ssa_fusion_loop_effects_pure`
- `.mojor_ssa_fusion_policy`
