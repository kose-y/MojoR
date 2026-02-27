# SSA Step

SSA is the downstream, backend-facing IR tier used after Tree IR preparation.

## Why SSA Exists

SSA makes dataflow explicit (single assignment per value identity), which improves verification and backend transforms.

## Plain-Language Mental Model

SSA is the "one value, one definition" form.

- Every computed value gets one defining statement.
- Uses refer to that specific definition.
- Control-flow joins use explicit block parameters/edges.

This makes dataflow checks and backend transforms much safer and easier.

## How SSA Is Produced

Typical path:

`... -> to_ssa -> memory_canonicalize -> annotate_effects_resources -> loop_recovery -> recanonicalize_loop_cfg -> fusion_candidate_analysis -> typing -> verify_ssa -> optimize_ssa -> verify_ssa -> backend_cfg`

## What SSA Adds

- Basic blocks with explicit terminators.
- Value references and block parameters.
- Canonical memory/effect/resource annotations.
- Strong boundary verifier checkpoints between stages.

## Mini Example

Source idea:

```text
if c then v = 1 else v = 2
return v
```

SSA-style shape (simplified):

- `entry` branches to `then` or `else`
- `then` produces one value and branches to `join`
- `else` produces one value and branches to `join`
- `join` receives value via block parameter and returns it

No hidden "latest mutable value" logic is needed.

## Important Status Note

Fusion in this backend-prep path is analysis-only at current state (`fusion_candidate_analysis`), not a full SSA fusion rewrite route.

## Current Limitations Snapshot

- This SSA path is backend-prep/inspection oriented; unsupported strict lanes fail before backend handoff.
- `fusion_candidate_analysis` is analysis-only and does not force SSA rewrite.
- Strict mode (`ir_only=TRUE`) rejects compatibility/raw fallback nodes at SSA boundaries.
- Use `docs/KNOWN_ISSUES.md` for the consolidated current SSA/backend-prep limitations.

## Boundary Verification

Representative boundary modes:
- `post_canonicalize`
- `post_structured_lowering`
- `post_memory_canonicalize`
- `post_recanonicalize`
- `post_typing`

## Representative SSA Diagnostics

Common SSA/semantic verifier failures include:

- `SSA verify: root must be an ssa_fn`
- `SSA verify: entry block not found`
- `SSA verify: block '<name>' missing terminator`
- `SSA verify: br from '<from>' targets unknown block`
- `SSA verify: br arg arity mismatch from '<from>' to '<to>' (<n> != <m>)`
- `SSA verify: condbr in '<block>' has invalid condition`
- `SSA verify: condbr then arg arity mismatch from '<from>' to '<to>'`
- `SSA verifier: <CODE>: <message> [block=<name>]`

These are SSA shape/dataflow contract failures and should be fixed before backend selection/codegen.

## Code Mapping

- Structured lower to SSA: `packages/mojor/R/ir/structured_lowering.R`
- SSA core passes: `packages/mojor/R/ir/ssa.R`
- SSA verifiers: `packages/mojor/R/ir/ssa_verify.R`
- Backend staging: `packages/mojor/R/ir/ssa_backend.R`

## Detailed References

- [../IR/TREE_NODE_REFERENCE.md](../IR/TREE_NODE_REFERENCE.md)
- [../BACKEND/SSA_OP_REFERENCE.md](../BACKEND/SSA_OP_REFERENCE.md)
- Pass ownership appendices are included in both references above.

## Implementation Ownership (Backend-Prep Path)

| Stage | Primary owners | Artifact |
|---|---|---|
| Tree -> SSA lowering | `.mojor_ir_structured_lower_to_ssa(...)` in `packages/mojor/R/ir/structured_lowering.R` (or fallback `.mojor_ir_to_ssa(...)` in `packages/mojor/R/ir/ssa.R`) | Semantic `ssa_fn` root with blocks/terminators. |
| Memory canonicalization | `.mojor_ssa_memory_canonicalize(...)` in `packages/mojor/R/ir/ssa_semantic.R` | Canonical memory-op spelling (`load`/`store` family). |
| Effects/resources | `.mojor_ssa_annotate_effects_resources(...)` in `packages/mojor/R/ir/ssa.R` | Per-statement effect/resource annotations. |
| Boundary verification | `.mojor_verify_semantic_ir(...)` in `packages/mojor/R/ir/ssa_verify.R` | Boundary-specific semantic diagnostics. |
| Typing | `.mojor_ssa_annotate_types(...)` in `packages/mojor/R/ir/ssa.R` | Value and block-param type annotations. |
| SSA optimization | `.mojor_ir_ssa_optimize(...)` in `packages/mojor/R/ir/ssa.R` | Optimized SSA + optional pass trace. |
| Backend CFG materialization | `.mojor_ir_ssa_backend_cfg(...)` in `packages/mojor/R/ir/ssa_backend.R` | Backend CFG object for lower/select/codegen phases. |

## Internal Symbol Index

`SSA construction and CFG skeleton`
- `.mojor_ir_structured_lower_to_ssa`
- `.mojor_ir_to_ssa`
- `.mojor_ssa_block`
- `.mojor_ssa_br`
- `.mojor_ssa_condbr`
- `.mojor_ssa_ret`
- `.mojor_ssa_is_value_ref`
- `.mojor_ir_collect_assigned_vars`

`Semantic canonicalization and schema`
- `.mojor_ssa_lower_to_semantic`
- `.mojor_ssa_canonicalize`
- `.mojor_ssa_memory_canonicalize_stmt`
- `.mojor_ssa_memory_canonicalize`
- `.mojor_ir_ssa_canonicalize_ops`
- `.mojor_ir_validate_op_schema`
- `.mojor_ssa_semantic_hash_fn`
- `.mojor_ssa_attach_loop_destination_metadata`

`Boundary verification and diagnostics`
- `.mojor_verify_semantic_ir`
- `.mojor_ssa_verify_or_stop`
- `.mojor_ssa_verify_has_errors`
- `.mojor_ssa_collect_structured_lowering_diagnostics`
- `.mojor_ssa_collect_memory_legacy_diagnostics`
- `.mojor_ssa_schema_validate_if_strict`
- `.mojor_ir_verify_ssa`
- `.mojor_ir_ssa_prune_unreachable`

`Effects/resources and loop metadata`
- `.mojor_ssa_stmt_effect_resource_summary`
- `.mojor_ssa_annotate_effects_resources`
- `.mojor_loop_recovery_analysis`
- `.mojor_recanonicalize_loop_cfg`
- `.mojor_ssa_collect_recanonicalize_diagnostics`
- `.mojor_ssa_loop_metadata_resolve`
- `.mojor_ssa_fusion_candidate_analysis`
- `.mojor_ssa_attach_fusion_candidate_analysis`

`SSA typing and optimization`
- `.mojor_ssa_collect_declared_symbol_types`
- `.mojor_ssa_infer_value_types`
- `.mojor_ssa_collect_typing_diagnostics`
- `.mojor_ssa_annotate_types`
- `.mojor_ir_ssa_copy_propagate`
- `.mojor_ir_ssa_prune_dead_stmts`
- `.mojor_ir_ssa_metrics`
- `.mojor_ir_ssa_optimize`

`Backend-prep handoff`
- `.mojor_ir_prepare_ssa_backend`
- `.mojor_ir_ssa_backend_cfg`
- `.mojor_ir_ssa_backend_lower`
- `.mojor_ir_ssa_backend_select`
- `.mojor_ir_ssa_backend_codegen`

## Beginner Debug Checklist

1. Ensure each block has a valid terminator (`br`, `condbr`, or `ret`).
2. Ensure branch argument counts match target block parameter counts.
3. If semantic verifier fails, check the boundary mode where it failed (`post_*` stages).
4. For typing failures, inspect value/type annotations before backend handoff.
