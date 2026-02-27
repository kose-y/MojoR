# Tree IR Node Reference

This is the complete implementation-aligned reference for Tree IR node kinds currently recognized by MöjoR.

## Plain-Language Mental Model

This file is the dictionary for Tree IR nodes.

- Each row says what a node is.
- Required fields tell you the minimum valid shape.
- Stage tags (`B,V,T,L,E,S`) tell where that node is expected to appear.

If you are unsure whether a node is valid at a stage, check this reference first.

## How to Read One Row

Example reading pattern:

1. Find node `kind`.
2. Check required fields are present.
3. Check stage markers to ensure node is legal in your current pipeline step.
4. Use pass-ownership appendix to map stage marker to owning functions/files.

This gives a fast path from "invalid node error" to exact owner module.

## Beginner Debug Checklist

1. Validate the failing node has all required fields.
2. Confirm the node is expected in that stage (use `B,V,T,L,E,S` tags).
3. If stage tag is missing, node likely should have been lowered earlier.
4. If strict mode rejects a route-constrained node family, treat it as a documented boundary and check support tables.

## Scope

Source-of-truth files:
- `packages/mojor/R/ir/nodes.R`
- `packages/mojor/R/ir/apply_nodes.R`
- `packages/mojor/R/ir/set_stats_nodes.R`
- `packages/mojor/R/runtime/ffi.R`
- `packages/mojor/R/ir/structured_ops.R`
- `packages/mojor/R/ir/verify.R`

Pipeline stage legend used below:
- `B`: built from AST/helpers
- `V`: verified in `.mojor_ir_verify(...)`
- `T`: typed/type-checked
- `L`: lowered or normalized as part of Tree IR pipeline
- `E`: emitted to Mojo (`expr_emit`/`stmt_emit`)
- `S`: lowered into SSA (`.mojor_ir_to_ssa(...)`) on SSA path

## Base and Alias Kinds

Base statement kinds:
- `if`, `loop`, `while`, `repeat`

Structured aliases (normalized to base kinds by `.mojor_ir_base_kind(...)`):
- `s_if -> if`
- `s_for -> loop`
- `s_while -> while`
- `s_repeat -> repeat`

## Core Expressions

| kind | Required fields | How it is used |
|---|---|---|
| `const` | `value` | Scalar/constant leaf node. Stages: `B,V,T,E,S`. |
| `var` | `name` | Variable reference leaf. Stages: `B,V,T,E,S`. |
| `unop` | `op`, `expr` | Unary arithmetic/logical operator. Stages: `B,V,T,E,S`. |
| `binop` | `op`, `lhs`, `rhs` | Binary arithmetic/comparison/logical operator. Stages: `B,V,T,E,S`. |
| `cast` | `to`, `expr` | Explicit cast/coercion node. Stages: `B,V,T,E,S`. |
| `call` | `fn`, `args` | General call node for supported subset calls. Stages: `B,V,T,E,S`. |
| `index` | `base`, `indices`, `index_base` | Indexed read access with one-based/zero-based semantics tracking. Stages: `B,V,T,L,E,S`. |
| `ifelse` | `cond`, `yes`, `no` | Ternary expression form. Stages: `B,V,T,E,S`. |

## Core Statements

| kind | Required fields | How it is used |
|---|---|---|
| `block` | `stmts` | Statement container/root. Stages: `B,V,T,L,E,S`. |
| `assign` | `lhs`, `rhs` | Assignment statement (var/index/subscript lhs forms). Stages: `B,V,T,L,E,S`. |
| `if` | `cond`, `then`, `else_block` | Branch statement. Stages: `B,V,T,L,E,S`. |
| `loop` | `var`, `range`, `body` | Canonical `for` loop statement. Stages: `B,V,T,L,E,S`. |
| `while` | `cond`, `body` | Canonical while loop. Stages: `B,V,T,L,E,S`. |
| `repeat` | `body` | Canonical repeat loop. Stages: `B,V,T,L,E,S`. |
| `break` | none | Loop-control statement. Stages: `B,V,E,S`. |
| `next` | none | Loop-control statement. Stages: `B,V,E,S`. |
| `return` | `value` (optional) | Return statement. Stages: `B,V,T,E,S`. |

## Index and Slice Helpers

| kind | Required fields | How it is used |
|---|---|---|
| `scalar_index` | `expr` | Scalar index wrapper used in subscript forms. Stages: `B,V,T,L,E`. |
| `slice_index` | `start`, `end` | Slice index wrapper. Lowering expands to loop/index behavior where needed. Stages: `B,V,T,L,E`. |
| `missing_index` | none | Missing-dimension index (`[,]` style). Stages: `B,V,T,L,E`. |
| `subscript` | `var`, `indices` | Multi-index assignment/read helper for matrix/array patterns. Stages: `B,V,T,L,E,S`. |

### Index node optional fields (exclusion/selection metadata)

Index nodes in `index.indices[]` and `subscript.indices[]` may carry additional metadata fields that control specialized emission paths:

| Field | Present on | Meaning |
|---|---|---|
| `neg_exclusion` | `kind="var"` (name=`__neg_excl__`) | 0-based exclusion index expression (string). Scalar negative index `-k` → skip element `k-1`. |
| `neg_vec_exclusion` | `kind="var"` (name=`__neg_vec_excl__`) | Vector exclusion metadata object: `{type: "c"|"range", indices: [...], count, start_0, end_0}`. `-c(1,2)` or `-(1:3)` patterns. |
| `pos_vec_selection` | `kind="var"` (name=`__pos_vec_sel__`) | Vector selection metadata object: `{type: "c", indices_0: [...], count}` for positive selector sets like `c(1,3)`. |

**N-d subscript dimension classification:** In multi-dimensional subscripts (≥2 indices), each dimension is classified as one of:
- **missing** (`missing_index`): Full range loop
- **scalar** (`const`/`var`): Fixed 0-based index, no loop
- **neg_scalar_excl** (`neg_exclusion` field): Loop + skip condition
- **neg_vec_excl** (`neg_vec_exclusion` field): Loop + multi-skip or range-skip
- **pos_vec_select** (`pos_vec_selection` field): Set ternary chain (set-selector form)

Lowering passes through N-d subscript patterns unchanged when any index carries exclusion or selection metadata. Emission is handled by `.mojor_classify_nd_indices()` and `.mojor_emit_nd_loops()` in `ir.R`.

`:` range selectors are typically carried by canonical slice/index nodes (for
example `slice_index`) rather than `pos_vec_selection` metadata.

## Range and Compatibility Wrappers

| kind | Required fields | How it is used |
|---|---|---|
| `range` | `start`, `end`, `step`, `end_exclusive` | Structured loop range. Stages: `B,V,T,L,E,S`. |
| `range_expr` | `expr` | Fallback loop-range expression wrapper. Stages: `B,V,T,L,E,S`. |
| `raw` | `expr` | Compatibility escape node. Allowed in compatible flows; rejected in strict `ir_only=TRUE`. Stages: `B,V(compat),E(compat)`. |

## Reduction, Scheduling, and Aggregation Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `scalar_reduce` | `op`, `acc`, `arg` (+ reduction attrs) | Scalar reduction IR form used pre-schedule. Stages: `B,V,T,L,E,S`. |
| `scheduled_reduce` | `mode`, `op`, `acc`, `arg`, `n_var` | Reduction form after schedule routing (`tree`/`simd`). Stages: `L,V,T,E,S`. |
| `mean` | `x`, `na_rm` | Mean reduction/stat node. Stages: `B,V,T,E` (and/or lowered to core forms depending on lane). |
| `var_stat` | `x`, `na_rm` | Variance stat node. Stages: `B,V,T,E`. |
| `sd` | `x`, `na_rm` | Standard deviation stat node. Stages: `B,V,T,E`. |
| `median` | `x` | Robust stat node. Stages: `B,V,T,E`. |
| `quantile` | `x`, `probs`, `na_rm`, `type` | Quantile stat node. Stages: `B,V,T,E`. |
| `iqr` | `x` | Interquartile range node. Stages: `B,V,T,E`. |
| `mad` | `x` | Median absolute deviation node. Stages: `B,V,T,E`. |
| `cov` | `x` (`y` optional) | Covariance descriptor. Strict verifier/typing enforce direct-variable numeric vector/matrix operands with matching rank and element dtype; dedicated lowering/emission remains deferred. Stages: `B,V,T`. |
| `cor` | `x` (`y` optional) | Correlation descriptor. Strict verifier/typing enforce direct-variable numeric vector/matrix operands with matching rank and element dtype; dedicated lowering/emission remains deferred. Stages: `B,V,T`. |
| `apply` | `x`, `margin`, `fun`, `na_rm` | Matrix apply-reduction node for supported functions. Stages: `B,V,T,E`. |

## Constructor and Shape Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `rep` | `x`, `times`, `each`, `length_out` | Repetition constructor. Stages: `B,V,T,L,E`. |
| `rep_len` | `x`, `length_out` | Rep-len constructor. Stages: `B,V,T,L,E`. |
| `c` | `parts` | Concatenation constructor. Optional `element_names` metadata can be attached for named-vector character indexing routes. Stages: `B,V,T,L,E`. |
| `seq` | `from`, `to`, `length_out` | Sequence constructor node. Stages: `B,V,T,L,E`. |
| `diag` | `x` or `n` | Diagonal extract/create/identity forms. Stages: `B,V,T,L,E`. |
| `transpose` | `x` | Matrix transpose constructor. Stages: `B,V,T,L,E`. |
| `cbind` | `args` | Column-bind constructor. Stages: `B,V,T,L,E`. |
| `rbind` | `args` | Row-bind constructor. Stages: `B,V,T,L,E`. |
| `vexpr` | `len`, `body` | Implicit elementwise loop descriptor. Strict verifier/typing enforce scalar-body + scalar-length subset contracts; lowering expands to allocation/loop/return routes. Stages: `B,V,T,L,E`. |
| `alloc` | `len`, `dtype` | Temporary allocation descriptor. Stages: `B,V,T,L,E`. |

## Sampling and RNG Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `sample_int` | `n`, `size`, `replace`, `prob` | Integer sampling node with strict/non-strict constraints. Stages: `B,V,T,E`. |
| `sample` | `x`, `size`, `replace`, `prob` | Vector sampling node with strict/non-strict constraints. Stages: `B,V,T,E`. |
| `rng_vec` | `dist`, `n`, `params` | Vectorized RNG descriptor. Stages: `B,V,T,E`. |

## Higher-Order Function Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `vapply` | `x`, `fun`, `fun_value_type` | HOF node with strict fun/shape constraints. Stages: `B,V,T,E`. |
| `sapply` | `x`, `fun` | HOF node in compiled subset constraints. Stages: `B,V,T,E`. |
| `lapply` | `x`, `fun` | HOF node in compiled subset constraints. Stages: `B,V,T,E`. |
| `mapply` | `fun`, `args` | Multi-arg HOF node (arity-constrained in strict path). Stages: `B,V,T,E`. |

## String and Regex Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `nchar` | `x` | String length node. Stages: `B,V,T,E`. |
| `nzchar` | `x` | Non-empty string predicate node. Stages: `B,V,T,E`. |
| `substr` | `x`, `start`, `stop` | Substring node. Stages: `B,V,T,E`. |
| `paste` | `args`, `sep`, `collapse` | String concatenation node (`paste0` lowers to `paste` with empty sep). Stages: `B,V,T,E`. |
| `regex_grepl` | `pattern`, `x`, regex flags | Regex logical-match node. Stages: `B,V,T,E`. |
| `regex_grep` | `pattern`, `x`, `value`, regex flags | Regex grep node. Stages: `B,V,T,E`. |
| `regex_sub` | `pattern`, `replacement`, `x`, regex flags | Regex substitution node. Stages: `B,V,T,E`. |

## Set/Match Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `unique` | `x` | Set primitive node. Stages: `B,V,T,E`. |
| `duplicated` | `x` | Set primitive node. Stages: `B,V,T,E`. |
| `any_duplicated` | `x` | Set primitive node. Stages: `B,V,T,E`. |
| `match` | `x`, `table` | Match primitive node. Stages: `B,V,T,E`. |
| `in` | `x`, `table` | `%in%` primitive node (`%in%` source operator lowers to `in` kind). Stages: `B,V,T,E`. |

## Matrix/Table Utility Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `matmul` | `lhs`, `rhs` | Matrix multiply descriptor. Strict verifier/typing enforce direct-variable numeric 2D matrix operands with matching element dtype; dedicated lowering/emission remains deferred. Stages: `B,V,T`. |
| `crossprod` | `lhs` (`rhs` optional) | Crossprod descriptor. Strict verifier/typing enforce direct-variable numeric 2D matrix operands with matching element dtype; dedicated lowering/emission remains deferred. Stages: `B,V,T`. |
| `tcrossprod` | `lhs` (`rhs` optional) | Tcrossprod descriptor. Strict verifier/typing enforce direct-variable numeric 2D matrix operands with matching element dtype; dedicated lowering/emission remains deferred. Stages: `B,V,T`. |
| `row_matrix` | `x` | Row-index matrix utility node. Stages: `B,V,T,E`. |
| `col_matrix` | `x` | Column-index matrix utility node. Stages: `B,V,T,E`. |
| `expand_grid` | `args` | Cartesian-product utility node. Stages: `B,V,T,E`. |

## Cumulative Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `cumsum` | `x` | Cumulative sum node. Stages: `B,V,T,E`. |
| `cumprod` | `x` | Cumulative product node. Stages: `B,V,T,E`. |
| `cummax` | `x` | Cumulative max node. Stages: `B,V,T,E`. |
| `cummin` | `x` | Cumulative min node. Stages: `B,V,T,E`. |

## Data Structure and Metadata Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `df_col_read` | `df`, `col` | Data-frame column read node. Stages: `B,V,T,E`. |
| `df_col_exists_guard` | `df`, `col` | Data-frame column existence guard. Stages: `B,V,T,E`. |
| `df_make` | `cols` | Data-frame construction node. Stages: `B,V,T,E`. |
| `list_make` | `items`, `names` | List construction node. Stages: `B,V,T,E`. |
| `dim` | `x` | Dimension metadata query node. Strict verifier/typing coverage is implemented for direct matrix/fixed-rank ND vars; lowering/emission remain route-constrained. Stages: `B,V,T,L,E`. |
| `type_predicate` | `predicate`, `x` | Type predicate metadata node. Strict verifier validates supported predicates on direct vars; typing/type-fold routes handle known-result folding. Stages: `B,V,T,L,E`. |
| `type_query` | `query`, `x` | Type query metadata node. Strict verifier validates supported queries on direct vars; typing/type-fold routes handle known-result folding. Stages: `B,V,T,L,E`. |

## GPU Descriptor Nodes

| kind | Required fields | How it is used |
|---|---|---|
| `gpu_reduce` | `op`, `arg`, `dims`, `keepdims` | GPU reduction descriptor node. Stages: `B,V,T,E`. |
| `gpu_matmul` | `a`, `b`, transpose flags | GPU matmul descriptor node. Stages: `B,V,T,E`. |

## FFI Node

| kind | Required fields | How it is used |
|---|---|---|
| `c_call` | `name`, `args`, `returns`, `library`, `expected_arity`, `arg_types`, `arg_names` | C-FFI call node. Verifier enforces declaration parity/arity/type constraints; emitter routes to FFI call path. Stages: `B,V,T,E`. |

## Notes and Constraints

- `raw` is compatibility-only and rejected in strict `ir_only=TRUE`.
- `%in%` source operator builds an `in` node kind in current builders/verifier/emitter.
- Effect/resource analysis paths also contain `%in%` handling in addition to `in`-path usage in verifier/typing/emission.
- `paste0(...)` builds a `paste` node with `sep = ""`.
- Structured aliases (`s_if/s_for/s_while/s_repeat`) are accepted where structured tier is enabled, then normalized to base kinds.
- No constructor-defined kinds are currently deferred from strict verifier coverage.

## Representative Node-Family Diagnostics

Common first-failure diagnostics by family:

- Structural shape: `IR verify [<kind>]: required field '<field>' missing`
- Loop control: `IR verify [break]: break statement outside loop`, `IR verify [next]: next statement outside loop`
- Indexing: `IR verify [index]: indices list cannot be empty`
- Reduction scheduling: `IR verify [scheduled_reduce]: mode must be one of tree/simd`
- Compatibility escape: `IR verify [raw]: raw fallback node not allowed in ir_only mode`

Use these diagnostics with stage tags to identify whether node construction, lowering, or strict-lane constraints are the actual issue.

## Pass Ownership Appendix

This maps the stage markers used in node rows (`B,V,T,L,E,S`) to concrete implementation owners.

| Stage marker | Primary owners (implementation) | What this stage is responsible for |
|---|---|---|
| `B` | `.mojor_ir_dump(...)` in `packages/mojor/R/ir/utils.R`; `.mojor_ir_build_stmt(...)` in `packages/mojor/R/ir/build.R`; `.mojor_ir_expr_build(...)` in `packages/mojor/R/ir/expr_build.R`; constructors in `packages/mojor/R/ir/nodes.R` | Build Tree IR nodes from R AST/calls, including structured aliases and high-level sugar nodes. |
| `V` | `.mojor_ir_verify(...)` in `packages/mojor/R/ir/verify.R` | Enforce node shape/field invariants, loop/control validity, and strict-vs-compatible acceptance (for example `raw`). |
| `T` | `.mojor_ir_type_check_stmt(...)` and `.mojor_ir_infer_type(...)` in `packages/mojor/R/ir/typing.R`; `.mojor_ir_annotate_typed(...)` in `packages/mojor/R/ir/type_guards.R` | Insert required casts, validate assignment/condition typing, and annotate inferred per-node types used by emission and guards. |
| `L` | `.mojor_ir_optimize(...)` and helpers in `packages/mojor/R/ir/nodes.R`; `.mojor_ir_fold_type_stmt(...)` / `.mojor_ir_lower_dim_stmt(...)` in `packages/mojor/R/ir/nodes.R`; `.mojor_ir_lower(...)` in `packages/mojor/R/ir/lowering.R`; `.mojor_ir_schedule(...)` in `packages/mojor/R/ir/nodes.R` | Normalize and lower HIR sugar into emit-ready forms; run optimization passes (CSE/fold/LICM/DCE/fusion); apply scheduling rewrites (tile/unroll metadata and `scalar_reduce -> scheduled_reduce` for tree/simd modes). |
| `E` | `.mojor_ir_stmt_emit(...)` in `packages/mojor/R/ir/stmt_emit.R`; `.mojor_ir_expr_emit(...)` in `packages/mojor/R/ir/expr_emit.R`; index/range helpers in `packages/mojor/R/ir/index_emit.R` and `packages/mojor/R/ir/stmt_range_emit.R` | Convert typed/lowered Tree IR nodes into Mojo source lines, including bounds/NA guards and specialized emit paths. |
| `S` | `.mojor_ir_structured_lower_to_ssa(...)` in `packages/mojor/R/ir/structured_lowering.R` (when enabled); fallback `.mojor_ir_to_ssa(...)` in `packages/mojor/R/ir/ssa.R`; orchestration in `.mojor_ir_prepare_ssa_backend(...)` in `packages/mojor/R/ir/ssa_backend.R` | Lower Tree IR into semantic SSA (`ssa_fn` + blocks/statements/terminators) for backend-prep verification and optimization. |

## Related

- [SPEC.md](./SPEC.md)
- [CONTRACT.md](./CONTRACT.md)
- [IMPLEMENTATION.md](./IMPLEMENTATION.md)
- [../BACKEND/SSA_OP_REFERENCE.md](../BACKEND/SSA_OP_REFERENCE.md)
