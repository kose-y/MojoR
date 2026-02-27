# MöjoR DSL Function Support Matrix

This page documents the Tree IR DSL constructor functions (`.mojor_ir_*`) and their current support level.

Scope:
- Function-constructor DSL used to build Tree IR nodes.
- Structured-tier DSL helpers (`.mojor_ir_s_*`).
- FFI DSL constructor (`.mojor_ir_c_call`).

Out of scope:
- Internal analysis/optimizer/helper functions that are not DSL constructors.
- Runtime wrapper APIs (`mojor_fn`, `mojor_build`, etc.).

Limitation tracking note:
- This page is a constructor support-level matrix, not a full limitations registry.
- Use `docs/KNOWN_ISSUES.md` for consolidated current limitations/deferred routes.

## Plain-Language Mental Model

Support level answers one question:
"If I build IR with this DSL function, how far does it reliably go in strict compilation?"

- Highest level: strict end-to-end (verify/type/lower-or-direct-emit).
- Middle level: supported, but lane/form constraints apply.
- Lower level: constructor exists, but strict verifier coverage or full lowering coverage is incomplete.
- Compatibility level: allowed only in compatibility flows.

## Mini Example

If you use `.mojor_ir_sample(...)` in strict mode:

1. This matrix marks it as `L3` (supported with constraints).
2. Verify constraints first (direct vector args, strict-lane `prob` rules).
3. If verifier rejects, treat that as expected contract behavior, not backend instability.

## Support Level Legend

Support levels are based on stage coverage in `TREE_NODE_REFERENCE.md`.

| Level | Meaning | Typical stage profile |
|---|---|---|
| `L4` | Strict end-to-end support | Includes `V` and `E` in strict flows (often `B,V,T,L,E` or `B,V,T,E`) |
| `L3` | Supported with explicit constraints | Includes `V` and `E`, but strict lane/form constraints apply |
| `L2` | Partial/deferred strict support | Constructor+emit/type paths exist, but strict verifier/lowering coverage is deferred |
| `L1` | Compatibility-only escape/meta | Not accepted in strict `ir_only=TRUE` or not a direct node constructor |

Stage letters used above:

- `B`: AST/build lowering produces the node.
- `V`: verifier accepts node shape and constraints.
- `T`: typing can infer/check usable types.
- `L`: dedicated lowering pass may rewrite it.
- `E`: emitter can produce Mojo for the lane.

## L4: Core DSL (Strict End-to-End)

These constructors are the baseline strict DSL surface.

### Core expressions/statements

- `.mojor_ir_const`
- `.mojor_ir_var`
- `.mojor_ir_unop`
- `.mojor_ir_binop`
- `.mojor_ir_cast`
- `.mojor_ir_call`
- `.mojor_ir_index`
- `.mojor_ir_ifelse`
- `.mojor_ir_block`
- `.mojor_ir_assign`
- `.mojor_ir_if`
- `.mojor_ir_loop`
- `.mojor_ir_while`
- `.mojor_ir_repeat`
- `.mojor_ir_break`
- `.mojor_ir_next`
- `.mojor_ir_return`
- `.mojor_ir_range`
- `.mojor_ir_range_expr`

### Index/slice helpers

- `.mojor_ir_scalar_index`
- `.mojor_ir_slice_index`
- `.mojor_ir_missing_index`
- `.mojor_ir_subscript`

### Indexing lane notes (effective strict levels)

These constructors are `L4` as DSL node shapes, but some source-level indexing
lanes routed through them are constrained:

| Lane | Effective level | Notes |
|---|---|---|
| Scalar index/subscript lanes (`index`, `scalar_index`, canonical `subscript`) | `L4` | Strict end-to-end on covered typed lanes. |
| Negative exclusion metadata lanes (`neg_exclusion`, `neg_vec_exclusion`) | `L3` | Supported on covered strict routes; N-d exclusion-heavy subscripts pass through lowering for emission handling. |
| Positive selector-set metadata lane (`pos_vec_selection`) | `L3` | Set selectors (`c(...)`) only; `:` ranges stay on canonical `slice_index`/range routes. |
| Character dimname/name resolution lanes feeding index/subscript | `L3` | Matrix/fixed-rank ND dimname routes plus named-vector `names()` and compile-time local-`chr` loop lanes are supported with strict metadata constraints. |
| Chained positive-selector indexing over intermediate selections | `L2` | Still partial in strict mode; many forms fail strict verification or route outside covered lanes. |

### Reduction/scheduling core

- `.mojor_ir_scalar_reduce`
- `.mojor_ir_scheduled_reduce`

### Construction and shape helpers (strictly covered)

- `.mojor_ir_rep`
- `.mojor_ir_rep_len`
- `.mojor_ir_c`
- `.mojor_ir_seq`
- `.mojor_ir_diag`
- `.mojor_ir_transpose`
- `.mojor_ir_cbind`
- `.mojor_ir_rbind`
- `.mojor_ir_alloc`
- `.mojor_ir_cumsum`
- `.mojor_ir_cumprod`
- `.mojor_ir_cummax`
- `.mojor_ir_cummin`

### Structured-tier aliases (normalize to base kinds)

- `.mojor_ir_s_if`
- `.mojor_ir_s_for`
- `.mojor_ir_s_while`
- `.mojor_ir_s_repeat`

## L3: Supported with Constraints

These are supported in strict paths, but with documented subset constraints (arity, argument form, lane, dtype, shape, or mode constraints).

### Higher-order / apply

- `.mojor_ir_apply`
- `.mojor_ir_vapply`
- `.mojor_ir_sapply`
- `.mojor_ir_lapply`
- `.mojor_ir_mapply`

### Sampling / RNG

- `.mojor_ir_sample_int`
- `.mojor_ir_sample`
- `.mojor_ir_rng_vec`

### String / regex

- `.mojor_ir_nchar`
- `.mojor_ir_nzchar`
- `.mojor_ir_substr`
- `.mojor_ir_paste`
- `.mojor_ir_paste0`
- `.mojor_ir_regex_grepl`
- `.mojor_ir_regex_grep`
- `.mojor_ir_regex_sub`

### Set/match/stats breadth

- `.mojor_ir_unique`
- `.mojor_ir_duplicated`
- `.mojor_ir_any_duplicated`
- `.mojor_ir_match`
- `.mojor_ir_in`
- `.mojor_ir_mean`
- `.mojor_ir_variance` (builds `var_stat`)
- `.mojor_ir_sd`
- `.mojor_ir_median`
- `.mojor_ir_quantile`
- `.mojor_ir_iqr`
- `.mojor_ir_mad`

### Data-structure helpers

- `.mojor_ir_df_col_read`
- `.mojor_ir_df_col_exists_guard`
- `.mojor_ir_df_make`
- `.mojor_ir_list_make`
- `.mojor_ir_row_matrix`
- `.mojor_ir_col_matrix`
- `.mojor_ir_expand_grid`

### GPU and FFI descriptors

- `.mojor_ir_gpu_reduce`
- `.mojor_ir_gpu_matmul`
- `.mojor_ir_c_call`

## L2: Partial / Deferred Strict Coverage

These constructors exist and have typed/emission usage, but strict-lane coverage is still partial (for example dedicated lowering/emission remains deferred for some families).

- `.mojor_ir_matmul` (strict verifier/typing covered; dedicated lowering/emission coverage deferred)
- `.mojor_ir_crossprod` (strict verifier/typing covered; dedicated lowering/emission coverage deferred)
- `.mojor_ir_tcrossprod` (strict verifier/typing covered; dedicated lowering/emission coverage deferred)
- `.mojor_ir_cov` (strict verifier/typing covered; dedicated lowering/emission coverage deferred)
- `.mojor_ir_cor` (strict verifier/typing covered; dedicated lowering/emission coverage deferred)
- `.mojor_ir_vexpr` (strict verifier/typing covered for scalar-body + scalar-length forms; lowering/emission remain route-constrained)
- `.mojor_ir_dim` (strict verifier/typing covered for direct matrix/fixed-rank ND vars; lowering/emission remains route-constrained)
- `.mojor_ir_type_predicate` (strict verifier covered for supported predicate names on direct vars; emission relies on typed/type-fold routes)
- `.mojor_ir_type_query` (strict verifier covered for supported query names on direct vars; emission relies on typed/type-fold routes)

Important split for `dim`:
- `.mojor_ir_dim` as a standalone node family has strict verifier/typing coverage for direct matrix/fixed-rank ND vars, but remains `L2` due to route-constrained lowering/emission behavior.
- Source-level scalar forms `dim(arg)[k]` are supported on strict loop-range rewrite lanes for matrix/fixed-rank ND arguments via dim-map scalar rewriting when `k` is a positive integer-valued scalar lane (literal or supported scalar expression coercible to integer).

## L1: Compatibility-Only / Meta Helpers

### Compatibility escape

- `.mojor_ir_raw` (explicit compatibility node; rejected in strict `ir_only=TRUE`)

### DSL utility/meta helpers (not primary node constructors)

- `.mojor_ir_try_build_range`
- `.mojor_ir_base_kind`
- `.mojor_ir_is_structured_kind`
- `.mojor_ir_structured_promote`

## Strict-Mode Gate Summary

These are cross-cutting gates that apply to many constructor families.

| Gate | Effect |
|---|---|
| `ir_only=TRUE` | Enables strict verifier/type checks and rejects compatibility `raw` fallback nodes. |
| Direct-variable requirements | Many L3 breadth constructors require direct `var` inputs in strict mode. |
| Static result type requirements | Strict verifier may reject nodes with unknown result type in typed lanes. |
| Expression-vs-assignment gating | Some nodes are statement/assignment-only in current emission lanes. |
| Dim-query split coverage | Standalone `.mojor_ir_dim` has strict verifier/typing coverage (direct matrix/fixed-rank ND vars) but remains `L2` due to route-constrained lowering/emission; `dim(arg)[k]` scalar loop-range rewrites are supported in strict transpile lanes. |

Notable hard gates:

1. `.mojor_ir_raw` is rejected when `ir_only=TRUE`.
2. `gpu_reduce` currently requires `dims=NULL` and `keepdims=FALSE` in transpile helper lane.
3. `gpu_matmul` expression form requires `gpu_jit_mode` in `{auto, unified_preview}`; otherwise assignment RHS form is required.
4. Strict sampling lanes allow weighted sampling only when `prob` is a direct `f64[]` variable in strict mode; additional route-specific emission constraints still apply.
5. Matrix descriptors (`.mojor_ir_matmul`/`.mojor_ir_crossprod`/`.mojor_ir_tcrossprod`) require direct variable operands with strict numeric 2D matrix types (`f64|f32|i32` matrices) and matching element dtype.
6. Covariance/correlation descriptors (`.mojor_ir_cov`/`.mojor_ir_cor`) require direct variable numeric vector/matrix operands with matching rank and element dtype in strict lanes.

## Notes on Interpretation

1. DSL support level is node-constructor support, not a blanket guarantee that every source-level R form using that concept is supported.
2. `L3` constructors may be strict-supported only for specific argument shapes/literals/lanes.
3. `L2` means "available, but not yet full strict coverage" and should be treated cautiously in new strict-only workflows.
4. For exact stage ownership and per-kind coverage, use:
 - `docs/IR/TREE_NODE_REFERENCE.md`
 - `docs/IR/BASIC_FUNCTION_SUPPORT.md`
 - `docs/IR/FUNCTION_SUPPORT_BREADTH.md`
 - `docs/IR/SPEC.md`
 - `docs/IR/CONTRACT.md`

## Representative Diagnostics

High-frequency verifier failures you should treat as contract, not transient bugs:

1. `IR verify [raw]: raw fallback node not allowed in ir_only mode`
2. `IR verify [call]: '<fn>' expects N arguments, got M`
3. `IR verify [gpu_reduce]: transpile helper currently supports only dims=NULL and keepdims=FALSE`
4. `IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'`
5. `IR verify [sample]: x must be a direct vector variable in strict mode`
6. `IR verify [mapply]: requires at least two vector arguments in the compiled subset`
7. `IR verify [regex_*]: ... must be TRUE or FALSE` (flag validation)
8. `mojor_transpile: dim() index in loop range must be a positive integer literal or scalar i32 expression`
9. `mojor_transpile: index name '<name>' not found in dimnames for '<var>'`
10. `mojor_transpile: local chr var '<var>' value '<name>' not found in dimnames for '<target>' (dim <k>)`

If you hit one of these, update the IR shape first; do not treat it as backend instability.

## Practical Sanity Checks

1. Verify a strict-safe constructor:
```r
n <- .mojor_ir_const("10")
v <- .mojor_ir_var("x")
node <- .mojor_ir_sample_int(n = n, size = n, replace = NULL, prob = NULL)
.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list()))
```

2. Verify strict rejection for compatibility node:
```r
node <- .mojor_ir_raw(quote(x + 1))
try(.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list())))
```

3. Verify expected GPU reduce gate:
```r
node <- .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = .mojor_ir_const("1"), keepdims = FALSE)
try(.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list(x = "f64[]"))))
```

## Beginner Debug Checklist

1. Find the DSL function in this matrix and check its level first.
2. If `L3`, validate documented constraints before treating behavior as regression.
3. If `L2`, expect strict-mode gaps and verify whether your path is in a deferred family.
4. If strict mode rejects a constructor, confirm it is not `L1` compatibility/meta-only.
