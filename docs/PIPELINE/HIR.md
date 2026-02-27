# HIR (High-Level Tree IR)

HIR is the high-level compiler IR immediately after build.

## Purpose

HIR keeps source-like structure so front-end lowering and analysis are easier.

- Retains structured control flow (`if`, `loop`, `while`, `repeat`).
- Allows convenient/sugar nodes that are not final emission forms.
- Preserves enough context to perform legality-aware rewrites.

## Plain-Language Mental Model

HIR is the "easy to reason about" version of your program.

- It still looks close to source-level logic.
- It is strict enough for compiler passes to analyze safely.
- It is not yet the final form for code emission.

Think of HIR as a cleaned blueprint: readable and structured, but not yet construction-ready.

## Typical HIR Node Families

Core statements:
- `block`, `assign`, `if`, `loop`, `while`, `repeat`, `return`

Core expressions:
- `const`, `var`, `unop`, `binop`, `cast`, `call`, `index`, `ifelse`, `range`

Helper/staged forms seen in HIR:
- Reduction helpers (`scalar_reduce`)
- Index helpers (`subscript`, `slice_index`, `missing_index`)
- Breadth/staged forms (`rep`, `seq`, `diag`, `sample`, `vapply`, `gpu_reduce`, etc.)

## HIR Invariants

- Every node has a `kind` and expected kind-specific fields.
- Structured control constructs remain structured (no raw CFG edges yet).
- HIR may include forms that must be normalized/lowered before emit.

## Why HIR Exists

Without HIR, every subset feature would need direct string codegen. HIR centralizes semantics and makes optimization/type/verify passes consistent.

## Mini Example

Source intent:

```r
if (x > 0) y <- x + 1 else y <- 0
```

HIR-style shape (simplified):

- `if(cond=binop(">", var("x"), const(0)), then=assign(var("y"), ...), else=assign(...))`

The compiler keeps this as a structured `if` node now. It does not split into CFG edges yet.

## Code Mapping

- Node definitions and helpers: `packages/mojor/R/ir/nodes.R`
- IR spec reference: `docs/IR/SPEC.md`

## Implementation Ownership

| Concern | Primary owners | Notes |
|---|---|---|
| HIR node constructors | `packages/mojor/R/ir/nodes.R` | Defines canonical node shape constructors and structured aliases. |
| AST -> HIR conversion | `.mojor_ir_build_stmt(...)` in `packages/mojor/R/ir/build.R`; `.mojor_ir_expr_build(...)` in `packages/mojor/R/ir/expr_build.R` | Produces HIR-rich forms prior to normalization/lowering. |
| HIR root creation | `.mojor_ir_dump(...)` in `packages/mojor/R/ir/utils.R` | Standard entry for converting function/expr into Tree IR root. |

## HIR-to-Next-Stage Contract

- HIR must be structurally valid for `.mojor_ir_verify(...)` and normalizable by `.mojor_ir_normalize(...)`.
- HIR may include helper/sugar kinds, but these must be known to later lower/schedule/emit routes.
- Alias kinds (`s_if`, `s_for`, `s_while`, `s_repeat`) are accepted as structured forms and normalized to base kinds for downstream passes.

## Practical Inspection Points

- Use `docs/IR/TREE_NODE_REFERENCE.md` for exact node-by-node stage coverage.
- If a feature builds but does not emit, the usual gap is between HIR sugar and LIR lowering ownership rather than in builder construction.

## Representative HIR Diagnostics

HIR issues usually surface through verify-stage messages rather than HIR-specific runtime errors. Common examples:

- `IR verify: node missing valid 'kind' field`
- `IR verify [<kind>]: required field '<field>' missing`
- `IR verify [break]: break statement outside loop`
- `IR verify [call]: fn must be a single character string`

If these appear, fix HIR node shape/structure first, then continue to lowering.

## Beginner Debug Checklist

1. Confirm each node has the expected `kind` and required fields.
2. Confirm control flow is still structured (`if`, `loop`, `while`) at this stage.
3. If a node kind appears in HIR but fails later, check LIR lowering ownership for that kind.
4. Use the Tree node reference to verify that node is expected to survive this stage.

## Internal Symbol Index

`Primary HIR constructor families`
- Core expressions/statements: `.mojor_ir_const`, `.mojor_ir_var`, `.mojor_ir_unop`, `.mojor_ir_binop`, `.mojor_ir_cast`, `.mojor_ir_call`, `.mojor_ir_index`, `.mojor_ir_ifelse`, `.mojor_ir_block`, `.mojor_ir_assign`, `.mojor_ir_if`, `.mojor_ir_loop`, `.mojor_ir_while`, `.mojor_ir_repeat`, `.mojor_ir_break`, `.mojor_ir_next`, `.mojor_ir_return`
- Index/range helpers: `.mojor_ir_scalar_index`, `.mojor_ir_slice_index`, `.mojor_ir_missing_index`, `.mojor_ir_subscript`, `.mojor_ir_range`, `.mojor_ir_range_expr`, `.mojor_ir_try_build_range`
- Reductions/scheduling nodes: `.mojor_ir_scalar_reduce`, `.mojor_ir_scheduled_reduce`
- Constructor/breadth nodes: `.mojor_ir_rep`, `.mojor_ir_rep_len`, `.mojor_ir_c`, `.mojor_ir_seq`, `.mojor_ir_diag`, `.mojor_ir_transpose`, `.mojor_ir_cbind`, `.mojor_ir_rbind`, `.mojor_ir_vexpr`
- Sampling/HOF/string/set/table nodes: `.mojor_ir_sample_int`, `.mojor_ir_sample`, `.mojor_ir_vapply`, `.mojor_ir_sapply`, `.mojor_ir_lapply`, `.mojor_ir_mapply`, `.mojor_ir_nchar`, `.mojor_ir_nzchar`, `.mojor_ir_substr`, `.mojor_ir_paste`, `.mojor_ir_paste0`, `.mojor_ir_unique`, `.mojor_ir_duplicated`, `.mojor_ir_any_duplicated`, `.mojor_ir_match`, `.mojor_ir_in`
- GPU/data/ffi helpers: `.mojor_ir_gpu_reduce`, `.mojor_ir_gpu_matmul`, `.mojor_ir_df_col_read`, `.mojor_ir_df_col_exists_guard`, `.mojor_ir_df_make`, `.mojor_ir_list_make`, `.mojor_ir_regex_grepl`, `.mojor_ir_regex_grep`, `.mojor_ir_regex_sub`, `.mojor_ir_c_call`

`Related support symbols`
- `.mojor_ir_rng_metadata`, `.mojor_ir_rng_call_fns`, `.mojor_ir_rng_table_fns`, `.mojor_ir_rng_helper_symbol_map`
- `.mojor_ir_base_kind` (structured-alias normalization)

For per-node field-level completeness, use `docs/IR/TREE_NODE_REFERENCE.md`.
