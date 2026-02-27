# SSA Node and Op Reference

This is the implementation-aligned reference for SSA containers, statement kinds, terminators, and canonical op schema.

## Scope

Source-of-truth files:
- `packages/mojor/R/ir/ssa.R`
- `packages/mojor/R/ir/ssa_semantic.R`
- `packages/mojor/R/ir/ssa_verify.R`
- `packages/mojor/R/ir/op_schema.R`

## SSA Container Nodes

### Semantic SSA containers

- `ssa_fn`
  - Fields: `entry`, `blocks`
  - Meaning: semantic SSA function root.

- `ssa_block`
  - Fields: `name`, `params`, `stmts`, `term`
  - Meaning: basic block with block parameters, statements, and terminator.

- `ssa_stmt`
  - Fields: `id`, `op`, `args` (+ optional metadata)
  - Meaning: value-producing, non-side-effect SSA statement.

- `ssa_effect`
  - Fields: `op`, `args` (+ optional metadata)
  - Meaning: side-effect statement (writes/effects/control side effects).

### Terminators

- `br(target, args)`
- `condbr(cond, then, then_args, else, else_args)`
- `ret(value)`

## Lossless Parse-Side Nodes

Parse/lossless representation uses:
- `raw_ssa_fn`
- `raw_ssa_block`

Lowering boundary:
- `.mojor_ssa_lower_to_semantic(raw_ssa_fn) -> ssa_fn`

## Op Canonicalization

Ops are canonicalized through `.mojor_ir_op_canonicalize(...)` before schema checks.

Canonical namespaces:
- `mojor.arith.*`
- `mojor.mem.*`
- `mojor.cf.*`
- `mojor.misc.*`

Compatibility aliases include:
- `const -> mojor.arith.const`
- `const:null -> mojor.misc.const_null`
- `select -> mojor.arith.select`
- `load -> mojor.mem.load`
- `index -> mojor.mem.index`
- `idx_missing -> mojor.misc.idx_missing`
- `idx_slice -> mojor.misc.idx_slice`
- `loop_cond -> mojor.cf.loop_cond`
- `loop_next -> mojor.cf.loop_next`
- `store_index -> mojor.mem.store_index`
- `store_subscript -> mojor.mem.store_subscript`
- `scheduled_reduce -> mojor.misc.stmt.scheduled_reduce`
- `scalar_reduce -> mojor.misc.stmt.scalar_reduce`
- `s_if/s_for/s_while/s_repeat -> mojor.misc.stmt.if/for/while/repeat`

Generated-family canonicalization:
- `binop:<sym> -> mojor.arith.<mapped>` (or `mojor.arith.binop.*` fallback)
- `unop:<sym> -> mojor.arith.<mapped>` (or `mojor.arith.unop.*` fallback)
- `cast:<type> -> mojor.arith.cast.*`
- `call:<fn> -> mojor.misc.call.*`
- `store:<x> -> mojor.mem.store.*`
- `load:<x> -> mojor.mem.load.*`
- `stmt:<x> -> mojor.misc.stmt.*`

## Statement Kind Rules (Schema-Enforced)

From `op_schema` validation:
- Pure/Call ops must be emitted as `ssa_stmt`.
- WritesMem/Effect ops must be emitted as `ssa_effect`.
- `ssa_stmt` requires a non-empty `id`.

## Canonical Op Schema (Exact Entries)

Columns:
- `Interfaces`: behavior class (`Pure`, `ReadsMem`, `WritesMem`, `Effect`, `Call`)
- `Arity`: `min..max` args per schema

| Op | Interfaces | Arity | Typical use |
|---|---|---|---|
| `mojor.arith.const` | `Pure` | `0..0` | Constant value creation |
| `mojor.misc.const_null` | `Pure` | `0..0` | Null constant sentinel |
| `mojor.arith.select` | `Pure` | `3..3` | Ternary/select form |
| `mojor.mem.load` | `ReadsMem` | `2..Inf` | Canonical memory read |
| `mojor.mem.store` | `WritesMem` | `3..Inf` | Canonical memory write |
| `mojor.mem.index` | `ReadsMem` | `2..Inf` | Pre-canonical indexed read form |
| `mojor.misc.idx_missing` | `Pure` | `0..0` | Missing-index token |
| `mojor.misc.idx_slice` | `Pure` | `2..2` | Slice-index token |
| `mojor.cf.loop_cond` | `Pure` | `2..2` | Loop continuation condition helper |
| `mojor.cf.loop_next` | `Pure` | `2..2` | Loop next-iteration helper |
| `mojor.mem.store_index` | `WritesMem` | `3..Inf` | Pre-canonical indexed store |
| `mojor.mem.store_subscript` | `WritesMem` | `3..Inf` | Pre-canonical subscript store |
| `mojor.arith.index_cast` | `Pure` | `1..1` | Index cast/normalization helper |
| `mojor.arith.cmpi` | `Pure` | `2..2` | Integer compare family |
| `mojor.arith.cmpf` | `Pure` | `2..2` | Float compare family |
| `mojor.arith.add` | `Pure` | `2..2` | Arithmetic add |
| `mojor.arith.sub` | `Pure` | `2..2` | Arithmetic subtract |
| `mojor.arith.mul` | `Pure` | `2..2` | Arithmetic multiply |
| `mojor.arith.div` | `Pure` | `2..2` | Arithmetic divide |
| `mojor.arith.mod` | `Pure` | `2..2` | Modulus |
| `mojor.arith.idiv` | `Pure` | `2..2` | Integer division |
| `mojor.arith.cmp_lt` | `Pure` | `2..2` | Less-than compare |
| `mojor.arith.cmp_gt` | `Pure` | `2..2` | Greater-than compare |
| `mojor.arith.cmp_le` | `Pure` | `2..2` | Less-equal compare |
| `mojor.arith.cmp_ge` | `Pure` | `2..2` | Greater-equal compare |
| `mojor.arith.cmp_eq` | `Pure` | `2..2` | Equality compare |
| `mojor.arith.cmp_ne` | `Pure` | `2..2` | Non-equality compare |
| `mojor.arith.and` | `Pure` | `2..2` | Logical and |
| `mojor.arith.or` | `Pure` | `2..2` | Logical or |
| `mojor.arith.bit_and` | `Pure` | `2..2` | Bitwise and |
| `mojor.arith.bit_or` | `Pure` | `2..2` | Bitwise or |
| `mojor.arith.xor` | `Pure` | `2..2` | XOR |
| `mojor.arith.neg` | `Pure` | `1..1` | Unary negate |
| `mojor.arith.pos` | `Pure` | `1..1` | Unary positive |
| `mojor.arith.not` | `Pure` | `1..1` | Unary logical not |
| `mojor.misc.stmt.break` | `Effect` | `0..0` | Break side-effect marker |
| `mojor.misc.stmt.next` | `Effect` | `0..0` | Next side-effect marker |
| `mojor.misc.stmt.raw` | `Effect` | `0..Inf` | Raw side-effect escape |
| `mojor.misc.stmt.if` | `Effect` | `0..0` | Structured if marker |
| `mojor.misc.stmt.for` | `Effect` | `0..0` | Structured for marker |
| `mojor.misc.stmt.while` | `Effect` | `0..0` | Structured while marker |
| `mojor.misc.stmt.repeat` | `Effect` | `0..0` | Structured repeat marker |

## Canonical Op Schema (Pattern Families)

| Pattern | Interfaces | Arity | Typical source |
|---|---|---|---|
| `mojor.arith.index_cast.*` | `Pure` | `1..1` | typed index cast variants |
| `mojor.arith.cmpi.*` | `Pure` | `2..2` | compare subvariants |
| `mojor.arith.cmpf.*` | `Pure` | `2..2` | float-compare subvariants |
| `mojor.arith.cast.*` | `Pure` | `1..1` | cast to specific type |
| `mojor.misc.call.*` | `Call` | `0..Inf` | lowered `call:<fn>` forms |
| `mojor.mem.load.*` | `ReadsMem` | `2..Inf` | typed/specialized loads |
| `mojor.mem.store.*` | `WritesMem` | `3..Inf` | typed/specialized stores |
| `mojor.misc.stmt.*` | `Effect` | `0..Inf` | lowered statement/effect families (`scheduled_reduce`, `scalar_reduce`, etc.) |
| `mojor.arith.binop.*` | `Pure` | `2..2` | unmapped binary-op symbols |
| `mojor.arith.unop.*` | `Pure` | `1..1` | unmapped unary-op symbols |

## Memory Canonicalization Notes

`ssa_semantic` memory canonicalization rewrites legacy spellings:
- `mojor.mem.index -> mojor.mem.load`
- `mojor.mem.store_index -> mojor.mem.store`
- `mojor.mem.store_subscript -> mojor.mem.store`
- `mojor.mem.store.* (non-canonical) -> mojor.mem.store`

## Pass Ownership Appendix

### Canonical Op and Schema Ownership

| Concern | Primary owners (implementation) | What it guarantees |
|---|---|---|
| Op canonical name mapping | `.mojor_ir_op_canonicalize(...)` in `packages/mojor/R/ir/op_schema.R` | Legacy/alias op spellings are normalized into canonical `mojor.*` namespaces. |
| In-IR op rewrite | `.mojor_ir_ssa_canonicalize_ops(...)` in `packages/mojor/R/ir/op_schema.R` | Every SSA stmt/effect op is rewritten to canonical spellings before strict checks. |
| Exact/pattern schema | `.mojor_ir_op_schema_table(...)` and `.mojor_ir_op_schema_patterns(...)` in `packages/mojor/R/ir/op_schema.R` | Defines the exact and wildcard op families, arity, interface class, and namespace requirements. |
| Schema validation | `.mojor_ir_validate_op_schema(...)` in `packages/mojor/R/ir/op_schema.R` | Enforces arity/namespace and stmt-kind legality (`ssa_stmt` vs `ssa_effect`) from interfaces. |
| Boundary verifier integration | `.mojor_verify_semantic_ir(...)` in `packages/mojor/R/ir/ssa_verify.R` | Applies boundary-specific semantic checks at named backend-prep checkpoints. |

### SSA Backend-Prep Stage Ownership

Pipeline owner: `.mojor_ir_prepare_ssa_backend(...)` in `packages/mojor/R/ir/ssa_backend.R`.

| Pipeline stage | Primary owners (implementation) | Main artifact/output |
|---|---|---|
| `to_ssa` | `.mojor_ir_structured_lower_to_ssa(...)` in `packages/mojor/R/ir/structured_lowering.R` (or fallback `.mojor_ir_to_ssa(...)` in `packages/mojor/R/ir/ssa.R`) | Semantic `ssa_fn` (`ssa$raw`) with explicit blocks/terminators. |
| `memory_canonicalize` | `.mojor_ssa_memory_canonicalize(...)` in `packages/mojor/R/ir/ssa_semantic.R` | Canonicalized memory op spellings (`ssa$memory_canonical`). |
| `annotate_effects_resources` | `.mojor_ssa_annotate_effects_resources(...)` in `packages/mojor/R/ir/ssa.R` | Per-op effect/resource metadata (`ssa$effect_annotated`). |
| `loop_recovery` | `.mojor_loop_recovery_analysis(...)` in `packages/mojor/R/ir/loop_recovery.R` | Loop recovery metadata used by boundary checks and recanonicalization. |
| `recanonicalize_loop_cfg` | `.mojor_recanonicalize_loop_cfg(...)` in `packages/mojor/R/ir/loop_recanonicalize.R` | Recanonicalized SSA CFG for recovered loops (`ssa$recanonicalized`). |
| `fusion_candidate_analysis` (optional) | `.mojor_ssa_fusion_candidate_analysis(...)` + `.mojor_ssa_attach_fusion_candidate_analysis(...)` in `packages/mojor/R/ir/ssa.R` | Analysis metadata attached to SSA (analysis-only unless rewrite is enabled). |
| `fusion_rewrite` (optional) | `.mojor_ssa_fusion_rewrite_skeleton(...)` / `.mojor_ssa_fusion_rewrite_constrained(...)` in `packages/mojor/R/ir/ssa.R` | Optional rewritten SSA candidate, re-attached with analysis metadata. |
| `typing` | `.mojor_ssa_annotate_types(...)` and `.mojor_ssa_infer_value_types(...)` in `packages/mojor/R/ir/ssa.R` | SSA values/block params annotated with normalized types (`ssa$typed`). |
| `optimize_ssa` | `.mojor_ir_ssa_optimize(...)` in `packages/mojor/R/ir/ssa.R` | Optimized SSA plus optional pass trace (`ssa$optimized`, `ssa$pass_trace`). |
| `backend_cfg` | `.mojor_ir_ssa_backend_cfg(...)` in `packages/mojor/R/ir/ssa_backend.R` | Backend CFG object (`ssa_backend_cfg`) for lower/select/codegen stages. |

## Related

- [SSA_GUIDE.md](./SSA_GUIDE.md)
- [ABI.md](./ABI.md)
- [../IR/SPEC.md](../IR/SPEC.md)
- [../IR/CONTRACT.md](../IR/CONTRACT.md)
- [../IR/TREE_NODE_REFERENCE.md](../IR/TREE_NODE_REFERENCE.md)
