# CSE (Common Subexpression Elimination)

CSE removes duplicate legal subexpressions by computing them once and reusing a temporary.

## Goal

Reduce repeated computation without changing program behavior.

## Plain-Language Mental Model

CSE is "compute once, reuse many times."

If the same safe expression appears multiple times in the same legal scope, CSE creates one temporary and replaces duplicates with that temp.

This removes redundant work without changing observable behavior.

## How Tree IR CSE Works

1. Walk assignment RHS expression trees.
2. Collect non-trivial subexpressions.
3. Keep only legality-safe candidates.
4. Compute canonical keys for each candidate.
5. Detect duplicates by key frequency.
6. Emit temp assignments (`__cse_tmp*`) and rewrite RHS uses.

## Mini Example

Before CSE:

```text
t1 = (x + y) * (x + y)
```

After CSE (simplified):

```text
__cse_tmp1 = x + y
t1 = __cse_tmp1 * __cse_tmp1
```

Why it helps:
- Fewer repeated operations
- Cleaner downstream IR for later passes

## Safety Conditions

- Candidate must pass legality checks (`.mojor_ir_cse_legal_expr(...)`).
- Expressions with blocked effects (RNG/Unknown/write-like behavior) are excluded.
- Rewrites are local to statement/control-flow contexts handled by the pass.

## Scope in Current Implementation

- Applies to assignment RHS and recursively to nested control-flow bodies.
- Integrated in optimization pipeline before fold/LICM/DCE at standard levels.

## Code Mapping

- CSE key/rewrite helpers: `packages/mojor/R/ir/nodes.R`
- CSE pass entrypoints: `.mojor_ir_cse_stmt`, `.mojor_ir_cse_block`
- Pipeline integration: `.mojor_ir_optimize`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Canonical keying | `.mojor_ir_expr_key(...)` in `packages/mojor/R/ir/nodes.R` | Generates stable keys for structural duplicate detection. |
| Candidate discovery | `.mojor_ir_collect_subexprs(...)` in `packages/mojor/R/ir/nodes.R` | Collects non-trivial subexpressions from RHS trees. |
| Legality gating | `.mojor_ir_cse_legal_expr(...)` in `packages/mojor/R/ir/nodes.R` | Restricts CSE to legality class `None` expressions. |
| Rewrite execution | `.mojor_ir_replace_expr(...)` in `packages/mojor/R/ir/nodes.R` | Rewrites duplicate occurrences to temp references. |

## Candidate Contract

- Trivial nodes (`const`, `var`) are excluded from CSE extraction.
- Only legality-safe expressions are extracted (`RNG`/`Unknown`/read-write sensitive forms are blocked).
- Rewrites preserve statement-local semantics; control-flow bodies are processed recursively through block/stmt entrypoints.

## Typical No-Op Cases

- No repeated keys after legality filtering.
- Repeated expressions exist but are blocked by effect/resource legality.
- Statements outside CSE-handled forms where pass intentionally remains conservative.

## Representative CSE Outcomes

CSE is usually rewrite/no-op rather than hard-error. Typical outcomes:

- Rewrite applied: repeated legal subexpressions become `__cse_tmp*` temp reuse.
- Legal no-op: duplicates exist but are blocked by effect/resource legality (`RNG`, `Unknown`, read/write-sensitive forms).
- Scope no-op: structurally similar expressions in incompatible control-flow regions are intentionally not merged.

When CSE "does nothing," inspect legality and key-equivalence first before treating it as a bug.

## Beginner Debug Checklist

1. Confirm duplicates are structurally identical (not just visually similar).
2. Check legality: RNG, unknown effects, or read/write-sensitive expressions are blocked.
3. Check scope: duplicates across incompatible control-flow regions are not always merged.
4. If no rewrite happens, inspect candidate extraction and key generation first.

## Internal Symbol Index

`Key extraction/rewrite symbols`
- `.mojor_ir_expr_key`
- `.mojor_ir_is_trivial`
- `.mojor_ir_collect_subexprs`
- `.mojor_ir_replace_expr`

`Legality gate`
- `.mojor_ir_cse_legal_expr`

`Pass entrypoints`
- `.mojor_ir_cse_stmt`
- `.mojor_ir_cse_block`
