# DCE (Dead Code Elimination)

DCE removes assignments whose results are never used and whose RHS is legal to discard.

## Goal

Shrink IR and emitted code by deleting useless computations.

## Plain-Language Mental Model

DCE removes work that has no effect on the final program result.

If a value is computed but never read later, and dropping that computation cannot change externally visible behavior, DCE deletes it.

Short version: "unused and safe to drop" means remove.

## How Tree IR DCE Works

1. Compute block-level def/use information.
2. Run backward liveness analysis (`live_vars`).
3. For each statement, recursively process nested blocks first.
4. Drop assignment statements that are dead and side-effect safe to remove.
5. Keep side-effectful or potentially observable statements.

## Mini Example

Before DCE:

```text
a = x + 1
b = x + 2
return(a)
```

After DCE:

```text
a = x + 1
return(a)
```

`b` is removed because it is never used and its RHS is droppable.

## What Is Never Dropped by Simple DCE

- Subscript/array writes (memory writes are observable).
- Statements with non-droppable RHS effects.
- Control-flow regions where loop-control behavior requires conservatism.

## Scope in Current Implementation

- Block-oriented backward pass.
- Called as part of standard and aggressive optimization levels.

## Code Mapping

- DCE helpers and liveness: `packages/mojor/R/ir/nodes.R`
- Main entrypoints: `.mojor_ir_dce_stmt`, `.mojor_ir_dce_block`
- Pipeline integration: `.mojor_ir_optimize`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Def/use collection | `.mojor_ir_collect_stmt_defs(...)`, `.mojor_ir_collect_stmt_refs(...)` in `packages/mojor/R/ir/nodes.R` | Computes variable dependencies for backward liveness. |
| Deadness decision | `.mojor_ir_is_dead_stmt(...)` in `packages/mojor/R/ir/nodes.R` | Drops only unused simple assigns with droppable RHS legality. |
| Statement recursion | `.mojor_ir_dce_stmt(...)` in `packages/mojor/R/ir/nodes.R` | Applies nested-block DCE while preserving control semantics. |
| Block-level liveness pass | `.mojor_ir_dce_block(...)` in `packages/mojor/R/ir/nodes.R` | Performs reverse traversal and live-set updates. |

## DCE Contract

- DCE is semantics-preserving and conservative by design.
- Memory writes/subscript writes are retained as observable effects.
- Dead assignment elimination is allowed only when RHS legality is `None` (pure/droppable).

## Conservative Retention Cases

- Loop-control-sensitive branches.
- Unknown or side-effectful RHS.
- Cases where liveness cannot be proven safely at current block scope.

## Representative DCE Outcomes

DCE is usually rewrite/no-op rather than hard-error. Typical outcomes:

- Rewrite applied: dead pure assignments are removed.
- Legal retention: assignment result is unused, but RHS is side-effectful/unknown so statement stays.
- Structural retention: memory writes and subscript writes remain by contract, even when result variables look unused.

If a statement is unexpectedly retained, verify liveness and RHS droppability before changing DCE rules.

## Beginner Debug Checklist

1. Ask "is this assigned value ever read later?" If yes, it is live.
2. Ask "does RHS have side effects?" If yes or unknown, keep it.
3. Expect memory writes to remain, even if the assigned variable looks unused.
4. Check nested blocks, because inner uses can keep outer defs alive.

## Internal Symbol Index

`Def/use and liveness helpers`
- `.mojor_ir_collect_stmt_refs`
- `.mojor_ir_collect_stmt_defs`
- `.mojor_ir_collect_block_defs`

`Deadness and recursive pass symbols`
- `.mojor_ir_is_dead_stmt`
- `.mojor_ir_dce_stmt`
- `.mojor_ir_dce_block`
