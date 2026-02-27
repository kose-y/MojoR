# Optimization (CSE, Fold, LICM, DCE)

MöjoR optimization is Tree-IR-first and legality-gated.

## Plain-Language Mental Model

Optimization is "make it cheaper without changing meaning."

Each pass has one simple job:
- CSE: remove repeated math.
- Fold: precompute constants.
- LICM: move loop-invariant work out of loops.
- DCE: remove unused safe computations.
- Fusion: merge compatible neighboring loops.

Safety checks run before every rewrite.

## Passes

CSE (Common Subexpression Elimination):
- Finds repeated legal expressions.
- Hoists them into temporary variables.
- Replaces duplicates with temp references.

Constant folding:
- Evaluates compile-time pure constant expressions.
- Replaces expression subtrees with constants.

LICM (Loop-Invariant Code Motion):
- Finds subexpressions inside loops that do not depend on loop iteration.
- Hoists legal invariants before the loop.
- Uses effect/resource constraints before hoisting memory reads.

DCE (Dead Code Elimination):
- Backward liveness analysis on blocks.
- Removes assignments to unused variables when RHS is legal to drop.
- Preserves side-effectful statements.

Elementwise fusion:
- Fuses adjacent compatible loops when metadata and legality checks allow.

## Optimization Levels

`opt_level = 0`:
- No optimization.

`opt_level = 1`:
- CSE + fold.

`opt_level = 2` (default):
- CSE + fold + LICM + DCE + elementwise fusion.

`opt_level >= 3`:
- Iterative rounds until fixed-point or max rounds reached.

## Mini Example (Pass Effects)

Before:

```text
for i in range:
  t = (a + b) * (a + b)
  out[i] = t
unused = 1 + 2
```

Possible after optimization:

```text
tmp = a + b           # CSE
for i in range:
  t = tmp * tmp
  out[i] = t
# unused removed         # DCE
```

If `1 + 2` appears in a kept expression, it may become `3` via constant fold.

## Safety Contracts

- Effect/resource classes gate CSE/LICM/DCE legality.
- RNG and Unknown act as hard barriers.
- Suspicious LICM hoist patterns trigger conservative retry in orchestration.

## Code Mapping

- Core pass implementation: `packages/mojor/R/ir/nodes.R`
- Fusion legality logic: `packages/mojor/R/ir/fusion.R`
- Pipeline orchestration + LICM guard fallback: `packages/mojor/R/transpile/ir_helpers.R`

## Pass Ownership and Order

| Pass | Primary owners | Core artifact effect |
|---|---|---|
| CSE | `.mojor_ir_cse_stmt(...)`, `.mojor_ir_cse_block(...)` in `packages/mojor/R/ir/nodes.R` | Inserts `__cse_tmp*` assignments and rewrites duplicate legal subexpressions. |
| Constant fold | `.mojor_ir_fold_expr(...)`, `.mojor_ir_fold_block(...)` in `packages/mojor/R/ir/nodes.R` | Replaces compile-time pure constants with literal `const` nodes. |
| LICM | `.mojor_ir_licm_loop(...)`, `.mojor_ir_licm_block(...)` in `packages/mojor/R/ir/nodes.R` | Hoists loop-invariant legal expressions into pre-loop `__licm_tmp*` assignments. |
| DCE | `.mojor_ir_dce_stmt(...)`, `.mojor_ir_dce_block(...)` in `packages/mojor/R/ir/nodes.R` | Drops dead pure assignments while preserving observable behavior. |
| Elementwise fusion | `.mojor_ir_fuse_elementwise_stmt(...)` in `packages/mojor/R/ir/nodes.R`; legality checks in `packages/mojor/R/ir/fusion.R` | Merges adjacent compatible loop bodies when metadata/legality permits. |
| Pipeline orchestrator | `.mojor_ir_optimize(...)` in `packages/mojor/R/ir/nodes.R` | Applies pass order by `opt_level`, including iterative fixed-point behavior at aggressive levels. |

## Optimization Artifact Contract

- Input and output remain Tree IR (same semantic root, transformed subtrees).
- Passes must preserve strict legality semantics and R-facing behavioral expectations.
- Effect/resource legality classes are the gate for motion/elimination/reuse decisions.

## Debugging Optimization Behavior

- If high-level optimization introduces suspicious LICM preheader temps, orchestration can retry with conservative settings (`opt_level=1`) in `.mojor_run_transpile_ir_pipeline(...)`.
- For optimization no-ops, inspect legality barriers first (`RNG`, `Unknown`, read/write interference).

## Representative Optimization Outcomes

Optimization stage is usually rewrite/no-op rather than hard-error. Typical outcomes:

- Legitimate no-op: pass runs but legality gates block rewrite.
- Conservative fallback: orchestration retries with lower optimization level when suspicious LICM patterns are detected.
- Upstream contract failure: verify/type/schedule errors appear before or after optimization and should be fixed at those boundaries.

## Beginner Debug Checklist

1. Start with `opt_level=0` and record baseline IR.
2. Increase to `opt_level=1` and check CSE/fold differences.
3. Increase to `opt_level=2` for LICM/DCE/fusion and compare deltas.
4. If no changes happen, inspect effect/resource legality first.
5. If behavior looks risky, rerun with lower opt level to isolate which pass caused the rewrite.

## Internal Symbol Index

`Top-level optimizer`
- `.mojor_ir_optimize`

`CSE family`
- `.mojor_ir_cse_stmt`
- `.mojor_ir_cse_block`
- `.mojor_ir_expr_key`
- `.mojor_ir_collect_subexprs`
- `.mojor_ir_replace_expr`

`Fold family`
- `.mojor_ir_fold_expr`
- `.mojor_ir_fold_stmt`
- `.mojor_ir_fold_block`
- `.mojor_ir_fold_type_expr`
- `.mojor_ir_fold_type_stmt`
- `.mojor_ir_fold_type_block`

`LICM family`
- `.mojor_ir_licm_loop`
- `.mojor_ir_licm_block`
- `.mojor_ir_is_loop_invariant`
- `.mojor_ir_collect_loop_invariants`
- `.mojor_ir_licm_reads_legal`

`DCE family`
- `.mojor_ir_dce_stmt`
- `.mojor_ir_dce_block`
- `.mojor_ir_is_dead_stmt`

`Fusion family`
- `.mojor_ir_fuse_elementwise_stmt`
- `.mojor_ir_fuse_elementwise_block`
- `.mojor_ir_loop_fusable`

`Orchestration guard`
- `.mojor_run_transpile_ir_pipeline`
