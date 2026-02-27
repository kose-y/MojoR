# CFG (Control-Flow Graph) Step

CFG is the block-and-edge representation used for backend-facing control flow.

## What a CFG Contains

- Entry block id.
- Ordered blocks.
- Per-block params/statements/terminator.
- Explicit successors.

## Why It Matters

CFG is where control transfer becomes explicit. This is required for backend analyses, loop reasoning, and target lowering.

## Plain-Language Mental Model

If source code is a paragraph, CFG is a road map.

- Blocks are intersections.
- Branches are one-way roads to the next intersection.
- Terminators (`br`, `condbr`, `ret`) are the traffic signs that decide where execution goes next.

Optimizers and verifiers reason over this map, not raw source text.

## Mini Example

For `if (c) a <- 1 else a <- 2; return(a)`, a simplified CFG looks like:

- `entry`: evaluate `c`, then `condbr` to `then` or `else`
- `then`: assign `a=1`, then `br` to `join`
- `else`: assign `a=2`, then `br` to `join`
- `join`: `ret a`

The point is explicit edges: both `then` and `else` must end at the same merge block before return.

## Loop Recovery and Recanonicalization

After early SSA transforms, loop structure may need recovery/recanonicalization.

Recovery/recanonicalization responsibilities:
- Re-establish coherent loop metadata.
- Validate reducible/single-exit assumptions where required.
- Gate strict recanonicalization checks on recovery status.

## Common CFG Terminators

- `br` (unconditional branch)
- `condbr` (conditional branch)
- `ret` (return)

## Code Mapping

- SSA block/term construction: `packages/mojor/R/ir/ssa.R`
- Loop recovery helpers: `packages/mojor/R/ir/loop_recovery.R`
- Recanonicalization helpers: `packages/mojor/R/ir/loop_recanonicalize.R`
- Backend CFG staging: `packages/mojor/R/ir/ssa_backend.R`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| CFG successor semantics | `.mojor_ssa_successors(...)` in `packages/mojor/R/ir/ssa_verify.R` | Defines successors from terminators (`br`, `condbr`, `ret`). |
| SSA core CFG validity | `.mojor_ir_verify_ssa(...)` in `packages/mojor/R/ir/ssa.R` | Enforces block/terminator/edge arity and dominance/use-def constraints. |
| Loop recovery analysis | `.mojor_loop_recovery_analysis(...)` in `packages/mojor/R/ir/loop_recovery.R` | Recovers loop structure metadata from CFG edges. |
| CFG recanonicalization | `.mojor_recanonicalize_loop_cfg(...)` in `packages/mojor/R/ir/loop_recanonicalize.R` | Rewrites CFG to canonical loop shape when recovery succeeds. |
| Backend CFG artifact | `.mojor_ir_ssa_backend_cfg(...)` in `packages/mojor/R/ir/ssa_backend.R` | Materializes backend-ready block/statement/terminator representation. |

## CFG Contract

- Entry block must exist and be reachable in the block map.
- Branch argument arity must match target block parameter arity.
- Structured-loop assumptions are boundary-checked after structured lowering and recanonicalization stages.

## Common Diagnostic Surfaces

- Use-before-def and non-dominating value references.
- Invalid branch targets or mismatched branch argument lists.
- Incomplete recovered loop metadata when recanonicalization boundaries are enforced.

## Representative CFG Diagnostics

Typical CFG/edge integrity failures include:

- `SSA verify: br from '<from>' targets unknown block`
- `SSA verify: br arg arity mismatch from '<from>' to '<to>' (<n> != <m>)`
- `SSA verify: condbr in '<block>' targets unknown block`
- `SSA verify: condbr else arg arity mismatch from '<from>' to '<to>'`
- `SSA verify: ret in '<block>' uses unknown value`

If these occur, verify block existence and branch-argument arity before investigating loop recovery or backend lowering.

## Beginner Debug Checklist

1. Verify every block ends with exactly one valid terminator.
2. Confirm every branch target block actually exists.
3. Confirm branch argument counts match target block parameter counts.
4. If loop checks fail, inspect recovery/recanonicalization diagnostics before touching later backend passes.

## Internal Symbol Index

`Core CFG shape and edges`
- `.mojor_ssa_successors`
- `.mojor_ssa_cfg_successor_map`
- `.mojor_ssa_cfg_reachable_blocks`
- `.mojor_ssa_cfg_has_cycle`
- `.mojor_ssa_cfg_signature`
- `.mojor_ssa_cfg_cache_key`
- `.mojor_ssa_build_preds_dom`

`Loop recovery and recanonicalization`
- `.mojor_loop_recovery_analysis`
- `.mojor_loop_recovery_guess_kind`
- `.mojor_loop_recovery_collect_intent_attrs`
- `.mojor_loop_recovery_unknown`
- `.mojor_loop_recovery_boundary_enabled`
- `.mojor_recanonicalize_loop_cfg`
- `.mojor_ssa_collect_recanonicalize_diagnostics`

`CFG/SSA verification gates`
- `.mojor_ir_verify_ssa`
- `.mojor_verify_semantic_ir`
- `.mojor_ssa_loop_skeleton_present`
- `.mojor_ssa_collect_structured_lowering_diagnostics`
- `.mojor_ssa_require_semantic_ir`

`Backend CFG materialization and selection`
- `.mojor_ir_ssa_backend_cfg`
- `.mojor_ir_check_ssa_backend_invariants`
- `.mojor_ir_ssa_backend_lower`
- `.mojor_ir_backend_select_stmt`
- `.mojor_ir_backend_select_term`
- `.mojor_ir_ssa_backend_select`
