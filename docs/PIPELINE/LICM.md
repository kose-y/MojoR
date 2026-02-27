# LICM (Loop-Invariant Code Motion)

LICM moves loop-invariant legal expressions outside loops.

## Goal

Avoid recomputing the same value on every iteration when it does not depend on the loop index.

## Plain-Language Mental Model

LICM is "move constant-inside-loop work to before the loop."

If an expression gives the same result every iteration and is safe to move, compute it once before the loop.

This reduces repeated work in hot loops.

## How Tree IR LICM Works

1. For each loop, gather resource ids written in the loop body.
2. Collect candidate subexpressions from loop statements.
3. Reject candidates referencing the loop induction variable.
4. Keep candidates that are legal to hoist:
   - Pure expressions are hoistable.
   - Reads are hoistable only when non-interference is proven.
5. Emit pre-loop temps (`__licm_tmp*`) and rewrite loop body uses.

## Mini Example

Before:

```text
for i in 1..n:
  out[i] = (a + b) * i
```

After LICM (simplified):

```text
__licm_tmp1 = a + b
for i in 1..n:
  out[i] = __licm_tmp1 * i
```

`a + b` moved out because it does not depend on `i`.

## Read-Hoist Safety

Read expressions are hoisted only when all of these are true:
- Effects include `Read` and do not include `Write`, `RNG`, or `Unknown`.
- Read resources are known (not unknown ids).
- No intersection with resources written by the loop body.

## Guardrail in Orchestration

The pipeline has a conservative guard that can retry optimization at lower level if suspicious LICM hoists are detected.

## Code Mapping

- LICM core: `.mojor_ir_licm_loop`, `.mojor_ir_licm_block`
- Invariance checks: `.mojor_ir_is_loop_invariant`, `.mojor_ir_licm_reads_legal`
- Pipeline guard: `packages/mojor/R/transpile/ir_helpers.R`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Loop-var dependency checks | `.mojor_ir_references_var(...)` in `packages/mojor/R/ir/nodes.R` | Rejects expressions that depend on induction variables. |
| Loop write-set discovery | `.mojor_ir_collect_written_resource_ids_block(...)` in `packages/mojor/R/ir/nodes.R` | Builds written resource set for read-hoist safety checks. |
| Invariance and legality gate | `.mojor_ir_is_loop_invariant(...)` and `.mojor_ir_licm_reads_legal(...)` in `packages/mojor/R/ir/nodes.R` | Allows pure expressions directly; allows reads only with proven non-interference. |
| Hoist + rewrite | `.mojor_ir_licm_loop(...)`, `.mojor_ir_licm_block(...)` in `packages/mojor/R/ir/nodes.R` | Emits `__licm_tmp*` preheader assignments and rewrites loop body uses. |

## LICM Contract

- LICM must preserve loop-carried semantics and write ordering.
- Read-hoist legality requires known read resources and zero overlap with loop writes.
- LICM output remains structured Tree IR (`loop` stays `loop`, only expression placement changes).

## Conservative Blockers

- RNG or unknown-effect expressions.
- Reads with unknown resource identity.
- Expressions touching resources that may be written within the same loop body.

## Representative LICM Outcomes

LICM is usually rewrite/no-op rather than hard-error. Typical outcomes:

- Hoist applied: loop-invariant legal expression moved to pre-loop `__licm_tmp*`.
- Read-hoist blocked: expression is invariant but resource interference proof fails.
- Safety no-op: effects include `RNG`/`Unknown` or loop-variable dependency exists.
- Orchestration fallback: suspicious hoist pattern can trigger lower-level retry for safety.

If expected hoists do not appear, inspect loop write-set and effect/resource classification first.

## Beginner Debug Checklist

1. Check whether candidate expression references the loop variable.
2. Check whether the loop writes to any resource read by that expression.
3. If effects include `RNG` or `Unknown`, expect no hoist.
4. Confirm no-hoist cases first; LICM is conservative by design.

## Internal Symbol Index

`Dependency and write-set analysis`
- `.mojor_ir_references_var`
- `.mojor_ir_collect_written_resource_ids_stmt`
- `.mojor_ir_collect_written_resource_ids_block`

`Invariance and legality`
- `.mojor_ir_is_loop_invariant`
- `.mojor_ir_collect_loop_invariants`
- `.mojor_ir_licm_reads_legal`

`Pass entrypoints`
- `.mojor_ir_licm_loop`
- `.mojor_ir_licm_block`
