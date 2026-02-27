# AST and Build Step

Build converts R syntax trees into Tree IR.

## Input and Output

Input:
- R function object.
- Type annotations from API call.

Output:
- Tree IR statement graph (initially HIR + core forms).
- Context metadata used by later passes (types, loop facts, shape sources).

## What Happens During Build

1. Validate API inputs and normalize argument type specs.
2. Extract function blocks from R AST.
3. Canonicalize selected syntactic forms (for example chained assignments).
4. Scan blocks for required runtime features (RNG helpers, FFI usage).
5. Convert R expressions/statements into Tree IR nodes.

## Plain-Language Mental Model

Build is a translator, not an optimizer.

- It rewrites R syntax into a compiler-friendly tree.
- It keeps the original program shape (if/loop nesting).
- It does not try to make code faster yet.

Think of this step as "turn messy source syntax into a clean, typed construction plan."

## Mini Example (Before and After Build)

Source intent:

```r
f <- function(x, y) {
  z <- x + y
  z * z
}
```

Build-stage shape (simplified):

- `assign(var("z"), binop("+", var("x"), var("y")))`
- `return(binop("*", var("z"), var("z")))`

At this point, duplicated math is still duplicated. Later passes (for example CSE) may rewrite it.

## Build Characteristics

- Build is primarily structural, not semantic.
- It can produce richer/sugar forms that are legal in HIR but must be lowered later.
- Loop/body structure is preserved for later optimization and legality checks.

## Common Failure Modes

- Missing or unsupported type annotations.
- Unsupported subset syntax in strict routes.
- Invalid loop/index patterns detected early by helper checks.

## Representative Build/Entry Diagnostics

Common first-stage errors include:

- `mojor_fn: each argument must be annotated like x: f64[]`
- `mojor_fn: invalid argument annotation: <arg>`
- `mojor_transpile: opt_level must be an integer between 0 and 3 (or NULL)`
- `mojor_transpile: unroll must be an integer between 1 and 16`
- `mojor_transpile: tile must be a numeric vector of length 1, 2, or 3`

If one of these appears, fix API input/flags first. Do not debug optimizer/emitter until this stage is clean.

## Beginner Debug Checklist

1. Reduce function body to the smallest failing form.
2. Confirm type annotations exist for each argument.
3. Check for unsupported syntax forms in strict routes.
4. Run just transpile/build-stage diagnostics before tuning optimization/scheduling flags.
5. Re-introduce complexity one construct at a time.

## Code Mapping

- Entry orchestration: `packages/mojor/R/transpile/transpile.R`
- Block extraction/helpers: `packages/mojor/R/transpile/helpers_core.R`
- IR node constructors: `packages/mojor/R/ir/nodes.R`
- IR expression/statement build helpers: `packages/mojor/R/ir/build.R`

## Detailed Stage Ownership

| Sub-stage | Primary owners | Artifact produced |
|---|---|---|
| API option and type-spec normalization | `.mojor_prepare_transpile_entry_options(...)` in `packages/mojor/R/transpile/entry_stages.R`; `.mojor_normalize_arg_specs(...)` in `packages/mojor/R/transpile/helpers_core.R` | Canonical argument/type metadata used by AST and IR build. |
| AST block extraction/canonicalization | `.mojor_extract_block(...)` and chained-assignment expansion helpers in `packages/mojor/R/transpile/helpers_analysis.R` | Ordered statement blocks ready for IR conversion. |
| Tree IR build entry | `.mojor_ir_dump(...)` in `packages/mojor/R/ir/utils.R` | Root Tree IR node for function body. |
| Statement lowering from AST to Tree IR | `.mojor_ir_build_stmt(...)` and `.mojor_ir_build_block(...)` in `packages/mojor/R/ir/build.R` | Structured statement IR (`block`, `assign`, `if`, loops, returns). |
| Expression lowering from AST to Tree IR | `.mojor_ir_expr_build(...)` in `packages/mojor/R/ir/expr_build.R` | Expression nodes (`const`, `call`, `index`, sugar/helper nodes). |

## Build-Time Invariants

- Every produced node must have a valid `kind` with required fields for that kind.
- Structured control flow remains structured; CFG edges are not introduced at build.
- Build may intentionally emit HIR sugar (`scalar_reduce`, `subscript`, `vexpr`, etc.) that later stages must lower.

## Diagnostic Surfaces

- Type-hint/annotation errors are surfaced before or during build entry.
- Unsupported AST patterns fail in builder helpers with node-specific messages.
- In strict mode, builder/normalization failures are terminal and are not silently routed through non-strict fallback policy.

## Internal Symbol Index

`AST block extraction/canonicalization`
- `.mojor_extract_block`
- `.mojor_wrap_block_expr`
- `.mojor_expand_chained_assignment_stmt`
- `.mojor_expand_chained_assignments_blocks`

`Tree build entry and normalization`
- `.mojor_ir_dump`
- `.mojor_ir_normalize`
- `.mojor_ir_detect_slice`
- `.mojor_ir_detect_reduction_pattern`

`Statement builders`
- `.mojor_ir_build_stmt`
- `.mojor_ir_build_block`
- `.mojor_ir_build_index` — detects and marks `neg_exclusion`, `neg_vec_exclusion`, `pos_vec_selection` metadata on index nodes
- `.mojor_ir_build_subscript` — same exclusion/selection detection for assignment LHS indices
- `.mojor_ir_build_positive_vector_selection` — builds `pos_vec_selection` metadata for `c(1,3)`-style positive selector sets (range `1:3` stays on canonical slice/index routes)
- `.mojor_ir_build_exclusion_index` — builds `neg_exclusion` metadata for scalar `-k`
- `.mojor_ir_build_vector_exclusion_index` — builds `neg_vec_exclusion` metadata for `-c(1,2)` / `-(1:3)`
- `.mojor_ir_detect_negative_index` — classifies an index expression as negative (scalar/vector/dynamic) or positive
- `.mojor_ir_collect_var_refs_build`

`Expression builders`
- `.mojor_ir_expr_build`
- `.mojor_ir_expr_get_call_arg`
- `.mojor_ir_expr_get_call_arg_idx`
- `.mojor_ir_parse_reduce_op`
- `.mojor_ir_parse_cpu_reduce_op`
- `.mojor_ir_parse_gpu_reduce_op`
- `.mojor_ir_build_cpu_reduce_call`
- `.mojor_ir_build_gpu_reduce_node`
- `.mojor_ir_parse_apply_reduce_call`
- `.mojor_ir_parse_apply_inline_fun`
