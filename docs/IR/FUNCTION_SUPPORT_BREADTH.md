# MöjoR Function Support Breadth (HOF, RNG, Set/Match, Data-Frame, String, GPU, Reductions)

This document is the focused reference for non-basic breadth functions and reduction families in the IR DSL.
For canonical cross-layer GPU notation (wrapper + IR + runtime/class in one matrix),
see [`../GPU_FUNCTION_SUPPORT_MATRIX.md`](../GPU_FUNCTION_SUPPORT_MATRIX.md).

Scope:
- Higher-order functions: `vapply`, `sapply`, `mapply` (and `lapply`).
- RNG functions: `runif`, `rnorm`, and related distribution calls.
- Set/match/data-frame/string/gpu breadth nodes.
- Reductions: `sum`, `prod`, `max`, `min`, `pmax`, `pmin`.

## Plain-Language Mental Model

These functions are supported, but many run with stricter lane rules than basic scalar math.

Typical pattern:
1. Constructor exists in DSL.
2. Verifier enforces shape/type/arity constraints (especially in strict mode).
3. Emission/lowering may apply special routing (for example reduction scheduling, GPU route constraints).

## Mini Example

For `.mojor_ir_mapply(fun, args)` in strict mode:

1. This matrix marks it as `L3` (supported with constraints).
2. Strict verifier requires direct vector args (arity >= 2) and compatible `FUN` shape.
3. If constraints are violated, expect verifier rejection by contract.

## Support Level Legend

| Level | Meaning |
|---|---|
| `L4` | Strictly supported in normal typed+emitted lane |
| `L3` | Supported with explicit strict-lane constraints |
| `L2` | Partial/deferred strict coverage or route-constrained |

## 0) Indexing Breadth Lanes (Cross-Cutting)

Indexing routes are primarily documented in `docs/SUBSET.md`,
`docs/PIPELINE/LIR.md`, and `docs/IR/TREE_NODE_REFERENCE.md`. This section
captures explicit L-level status for breadth-sensitive indexing lanes that
interact with HOF/string/set/routing constraints.

| Lane | Effective level | Strict constraints |
|---|---|---|
| Character dimname indexing (matrix/fixed-rank ND) | `L3` | Requires dimname metadata on referenced axes; missing-index forms are position-safe under control-arg filtering. |
| Named-vector character indexing (`names()`-tracked constructor lanes) | `L3` | Requires compile-time-resolvable names on constructor-fed lanes. |
| Local character variable element-wise loop indexing | `L3` | Supported when local `chr` values resolve against target axis dimnames at compile time. |
| Submatrix/ND vector-index writes via strict computed `i32[]` + supported `ifelse(cond, yes, no)` selector lanes | `L3` | Requires supported logical condition and supported `i32[]` branch lanes with resolvable selector lengths. |
| Negative exclusion lanes (`-k`, `-c(...)`, `-(1:k)`) in covered N-d subscript routes | `L3` | Supported on exclusion-heavy strict routes; behavior remains route-constrained outside canonical forms. |
| Chained positive-selector indexing over intermediate selection results | `L2` | Partial/deferred in strict lanes; many forms remain unsupported. |

Range selector note:
- `:` selector ranges remain on canonical slice/index lowering routes rather
  than `pos_vec_selection` metadata lanes.

## 1) Higher-Order Function DSL

### Constructor Signatures

- `.mojor_ir_vapply(x, fun, fun_value_type, src = NULL)`
- `.mojor_ir_sapply(x, fun, src = NULL)`
- `.mojor_ir_lapply(x, fun, src = NULL)`
- `.mojor_ir_mapply(fun, args, src = NULL)`

### Strict-Mode Constraints (Verifier)

| Function | Level | Strict constraints |
|---|---|---|
| `vapply` | `L3` | `x` must be direct vector var; `x` type in `{f64[], i32[], lgl[], bool[]}`; `FUN` must be anonymous or supported named unary function; `fun_value_type` constrained to numeric/integer/logical variants; result type must be known vector lane. |
| `sapply` | `L3` | Same core constraints as `vapply` without explicit `fun_value_type`. |
| `lapply` | `L3` | Same compiled-subset constraints as `sapply` in current lane. |
| `mapply` | `L3` | Supports direct vector args with arity >= 2; args must be direct vars in strict lane; `FUN` must be anonymous or supported named multi-ary function set (`pmin`, `pmax`, `min`, `max`); result type must be known vector lane. |

## 2) Set/Match Breadth DSL

### Constructor Signatures

- `.mojor_ir_unique(x, src = NULL)`
- `.mojor_ir_duplicated(x, src = NULL)`
- `.mojor_ir_any_duplicated(x, src = NULL)`
- `.mojor_ir_match(x, table, src = NULL)`
- `.mojor_ir_in(x, table, src = NULL)`  (`%in%` lowers to `in`)

### Support Summary

| Function | Level | Notes |
|---|---|---|
| `unique` | `L3` | Strict verifier requires expected var-field shape; typed+emitted lane constraints apply. |
| `duplicated` | `L3` | Strict verifier requires expected var-field shape; typed+emitted lane constraints apply. |
| `any_duplicated` | `L3` | Strict verifier requires expected var-field shape; typed+emitted lane constraints apply. |
| `match` | `L3` | Strict verifier requires var-form fields (`x`, `table`) in compiled subset lane. |
| `in` (`%in%`) | `L3` | `%in%` lowers to `in`; strict var-form checks apply. |

## 3) Data-Frame/List Breadth DSL

### Constructor Signatures

- `.mojor_ir_df_col_read(df, col, src = NULL)`
- `.mojor_ir_df_col_exists_guard(df, col, src = NULL)`
- `.mojor_ir_df_make(cols, src = NULL)`
- `.mojor_ir_list_make(items, names = NULL, src = NULL)`

### Strict-Mode Constraints

| Function | Level | Strict constraints |
|---|---|---|
| `df_col_read` | `L3` | `df` must type-check as `df`; `col` as `chr[]`; result must be one of allowed vector lane types. |
| `df_col_exists_guard` | `L3` | Guard-form helper; strict typing and df/col lane constraints apply at verifier/emit boundaries. |
| `df_make` | `L3` | `cols` must be non-empty list of allowed vector lane types; result type must be `df`. |
| `list_make` | `L3` | `items` non-empty list of allowed vector lane types; optional `names` must match item count and be non-empty strings; result type `list`. |

## 4) String/Regex Breadth DSL

### Constructor Signatures

- `.mojor_ir_nchar(x, src = NULL)`
- `.mojor_ir_nzchar(x, src = NULL)`
- `.mojor_ir_substr(x, start, stop, src = NULL)`
- `.mojor_ir_paste(args, sep = "\" \"", collapse = NULL, src = NULL)`
- `.mojor_ir_paste0(args, collapse = NULL, src = NULL)`
- `.mojor_ir_regex_grepl(pattern, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL)`
- `.mojor_ir_regex_grep(pattern, x, value = FALSE, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL)`
- `.mojor_ir_regex_sub(pattern, replacement, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, global = FALSE, src = NULL)`

### Strict-Mode Constraints

| Function | Level | Strict constraints |
|---|---|---|
| `nchar` | `L3` | `x` must be `chr[]`; result type strict-checks to integer vector lane. |
| `nzchar` | `L3` | `x` must be `chr[]`; result type strict-checks to logical/bool vector lane. |
| `substr` | `L3` | `x: chr[]`, `start: i32`, `stop: i32`, result `chr[]`. |
| `paste` | `L3` | Args strict-compatible char lanes; `sep`/`collapse` strict-lane constraints apply; result `chr[]`. |
| `paste0` | `L3` | Same as `paste` with empty-separator behavior. |
| `regex_grepl` | `L3` | Pattern strict scalar literal/var; `x` direct `chr[]` var; strict logical flags. |
| `regex_grep` | `L3` | Same input constraints as `regex_grepl`; result type depends on `value` flag lane. |
| `regex_sub` | `L3` | Pattern/replacement strict scalar literal/var; `x` direct `chr[]` var; strict logical flags. |

## 5) RNG Breadth DSL

### Constructor / Call Forms

- Vectorized RNG constructor:
 - `.mojor_ir_rng_vec(dist, n, params = NULL, src = NULL)`
- Scalar RNG call form (IR call node):
 - `.mojor_ir_call("<dist>", args)`
 - first argument is always `n` (number of draws)

### Supported Distributions and Parameter Counts

Parameter counts below are distribution-parameter counts (excluding leading `n`).

| Distribution | Level | Params (min..max) | Notes |
|---|---|---|---|
| `runif` | `L3` | `0..2` | Source builder commonly maps to `rng_vec`. |
| `rnorm` | `L3` | `0..2` | Source builder commonly maps to `rng_vec`. |
| `rgamma` | `L3` | `1..2` | Call-form RNG lane common. |
| `rbinom` | `L3` | `2..2` | Call-form RNG lane common. |
| `rexp` | `L3` | `0..1` | Call-form RNG lane common. |
| `rpois` | `L3` | `1..1` | Call-form RNG lane common. |
| `rlnorm` | `L3` | `0..2` | Call-form RNG lane common. |
| `rchisq` | `L3` | `1..1` | Call-form RNG lane common. |
| `rt` | `L3` | `1..1` | Call-form RNG lane common. |
| `rf` | `L3` | `2..2` | Call-form RNG lane common. |
| `rbeta` | `L3` | `2..2` | Call-form RNG lane common. |
| `rweibull` | `L3` | `1..2` | Call-form RNG lane common. |
| `rlogis` | `L3` | `0..2` | Call-form RNG lane common. |
| `rcauchy` | `L3` | `0..2` | Call-form RNG lane common. |
| `rgeom` | `L3` | `1..1` | Call-form RNG lane common. |
| `rnbinom` | `L3` | `2..2` | Call-form RNG lane common. |
| `rhyper` | `L3` | `3..3` | Call-form RNG lane common. |
| `rsignrank` | `L3` | `1..1` | Call-form RNG lane common. |
| `rwilcox` | `L3` | `2..2` | Call-form RNG lane common. |

### Strict/Route Constraints

| RNG form | Level | Strict constraints |
|---|---|---|
| `rng_vec(dist, n, params)` | `L3` | `dist` must be catalog-supported; param count must match metadata bounds. |
| Scalar RNG `call(dist, args)` | `L3` | Arity must satisfy `(min_params+1)..(max_params+1)` because `n` is first arg. |
| `rng_vec` expression emission | `L3` | Expression-lowerable only for scalar draws where `n` is constant `1`. |

### Practical Notes

1. `runif`/`rnorm` are built as `rng_vec` from source AST in current builder path.
2. Other RNG families are commonly built as call-form RNG nodes.
3. RNG is an effect barrier (`RNG` class): no CSE/LICM hoist/elimination.

## 6) Matrix Descriptor DSL

### Constructor Signatures

- `.mojor_ir_matmul(lhs, rhs, src = NULL)`
- `.mojor_ir_crossprod(lhs, rhs = NULL, src = NULL)`
- `.mojor_ir_tcrossprod(lhs, rhs = NULL, src = NULL)`

### Strict/Route Constraints

| Function | Level | Strict constraints |
|---|---|---|
| `matmul` | `L2` | Strict verifier/typing require direct-variable numeric 2D matrix operands with matching element dtype (`f64`/`f32`/`i32`). |
| `crossprod` | `L2` | `lhs` required, `rhs` optional; strict verifier/typing enforce the same matrix/dtype contract as `matmul`. |
| `tcrossprod` | `L2` | `lhs` required, `rhs` optional; strict verifier/typing enforce the same matrix/dtype contract as `matmul`. |

### Practical Notes

1. Strict verifier coverage for these descriptor nodes is now present.
2. Dedicated lowering/emission routes for these constructor nodes are still deferred.

## 7) GPU Breadth DSL

### Constructor Signatures

- `.mojor_ir_gpu_reduce(op, arg, dims = NULL, keepdims = FALSE, src = NULL)`
- `.mojor_ir_gpu_matmul(a, b, transpose_a = FALSE, transpose_b = FALSE, src = NULL)`

Runtime/class-level GPUArray API support (constructors, indexing, reductions, matmul, route labels, and known gaps) is documented in [`../GPUARRAY_CLASS_SUPPORT.md`](../GPUARRAY_CLASS_SUPPORT.md).
Canonical cross-layer GPU support notation is documented in [`../GPU_FUNCTION_SUPPORT_MATRIX.md`](../GPU_FUNCTION_SUPPORT_MATRIX.md).

### Strict/Route Constraints

| Function | Level | Strict constraints |
|---|---|---|
| `gpu_reduce` | `L3` | `op` in `{sum, mean, min, max, argmin, argmax}`; transpile helper currently enforces `dims = NULL`, `keepdims = FALSE`. |
| `gpu_matmul` | `L3` | `a`/`b` must be allowed matrix types, matching element type in strict mode; expression form requires `gpu_jit_mode` in `{auto, unified_preview}`, otherwise assign-form route required. |

## 8) Reduction Families (`sum/prod/min/max/pmin/pmax`)

### Source-Level to IR Mapping

| Source function | Level | Primary IR shape | Notes |
|---|---|---|---|
| `sum(x)` | `L3` | `scalar_reduce(op="sum", ...)` route in reduction path | Scheduling may rewrite to `scheduled_reduce`. |
| `prod(x)` | `L3` | `scalar_reduce(op="product", ...)` route | `prod` maps to `product` in reduction node op space. |
| `min(x)` | `L3` | `scalar_reduce(op="min", ...)` (reduction form) | Also exists as basic call-form in non-reduction contexts. |
| `max(x)` | `L3` | `scalar_reduce(op="max", ...)` (reduction form) | Also exists as basic call-form in non-reduction contexts. |
| `pmin(a, b)` | `L3` | `call("pmin", args)` | Emission aliases to `min` in scalar call lane. |
| `pmax(a, b)` | `L3` | `call("pmax", args)` | Emission aliases to `max` in scalar call lane. |

### DSL Signatures for Reduction Nodes

- `.mojor_ir_scalar_reduce(op, acc, arg, init = NULL, axis = 0L, associative = TRUE, commutative = TRUE, na_rm = FALSE, src = NULL)`
- `.mojor_ir_scheduled_reduce(mode, op, acc, arg, n_var = "n_i", dtype = NULL, init_val = NULL, empty_val = NULL, nan_val = NULL, value_cast = NULL, src = NULL)`

### Allowed Reduction Ops and Modes

| Node | Allowed values |
|---|---|
| `scalar_reduce$op` | `sum`, `product`, `min`, `max`, `which.min`, `which.max` |
| `scheduled_reduce$mode` | `tree`, `simd` |
| `scheduled_reduce$op` | `sum`, `product`, `min`, `max`, `which.min`, `which.max` |

### Relevant Modes (User/Compiler)

1. Reduction scheduling policy (`reduction`) routes:
 - `linear`: baseline scalar reduction behavior.
 - `tree`: eligible reductions rewritten to tree scheduled form.
 - `simd`: eligible reductions rewritten to SIMD scheduled form.
2. Scheduling rewrite is legality- and eligibility-gated.
3. `pmin/pmax` are supported call lanes with strict arity constraints (1 or 2 args at call-node verifier level) and scalar emission aliasing to `min/max`.

### Covariance/Correlation Descriptors

| Function | Level | Strict constraints |
|---|---|---|
| `cov` | `L2` | Strict verifier/typing require direct-variable numeric vector or 2D matrix operands (`f64`/`f32`/`i32`) with matching rank and element dtype. |
| `cor` | `L2` | Same strict contract as `cov`; `x` required and `y` optional. |

## 9) Emission-Lane Constraints (Important)

Some breadth nodes verify and type-check, but are only emitted in specific forms.

| Family | Current emission contract |
|---|---|
| `vapply` / `sapply` / `lapply` / `mapply` | Whole-vector assignment lane only in compiled subset. |
| `sample_int` / `sample` | Statement-mode by default; scalar expression lane only when `size = 1`. |
| `rng_vec` | Scalar expression lowering only when `n` is constant `1`; otherwise vector lane. |
| `nchar` / `nzchar` / `substr` / `paste` | Scalar expression path requires loop/index context; whole-vector assignment path is the safe default. |
| `paste` / `paste0` | Scalar emission with `collapse != NULL` is rejected in this release. |
| Character index lanes | Dimname/name metadata must resolve in strict mode; unresolved labels fail with targeted dimname diagnostics. |
| Selector-set metadata lanes (`pos_vec_selection`) | Set-selector (`c(...)`) routes only; `:` ranges are handled by canonical slice/index routes. |

Practical implication:

1. If an expression-form breadth node fails emission, rewrite to whole-vector assignment first.
2. For sampling/RNG in expressions, use strict scalar forms (`size = 1`, `n = 1`) when possible.

## 10) Representative Diagnostics

High-frequency verifier/emitter rejections you should expect:

1. `IR verify [sample]: x must be a direct vector variable in strict mode`
2. `IR verify [sample_int]: prob must be a direct f64[] argument in strict mode`
3. `IR verify [mapply]: requires at least two vector arguments in the compiled subset`
4. `IR verify [regex_*]: pattern/x/flags must satisfy strict direct-var and logical-scalar constraints`
5. `IR verify [gpu_reduce]: transpile helper currently supports only dims=NULL and keepdims=FALSE`
6. `IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'`
7. Emission errors for HOF/sample/string scalar lanes when used outside their supported context

## 11) Practical Sanity Checks

1. Strict-safe HOF node shape:
```r
node <- .mojor_ir_sapply(.mojor_ir_var("x"), as.name("sin"))
.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list(x = "f64[]")))
```

2. Strict weighted-sampling gate check:
```r
node <- .mojor_ir_sample(
  x = .mojor_ir_var("x"),
  size = .mojor_ir_const("1"),
  replace = .mojor_ir_const("FALSE"),
  prob = .mojor_ir_const("1.0")
)
try(.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list(x = "f64[]"))))
```

3. GPU reduce helper gate check:
```r
node <- .mojor_ir_gpu_reduce("sum", .mojor_ir_var("gx"), dims = .mojor_ir_const("1"), keepdims = FALSE)
try(.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list(gx = "f64[]"))))
```

4. Reduction node contract check:
```r
node <- .mojor_ir_scheduled_reduce(mode = "tree", op = "sum", acc = "acc", arg = "x")
.mojor_ir_verify(node, ctx = list(ir_only = TRUE, type_env = list()))
```

## Source-of-Truth Files

- Constructors: `packages/mojor/R/ir/nodes.R`, `packages/mojor/R/ir/set_stats_nodes.R`, `packages/mojor/R/ir/apply_nodes.R`
- Verifier constraints: `packages/mojor/R/ir/verify.R`
- Typing behavior: `packages/mojor/R/ir/typing.R`
- Emission mapping: `packages/mojor/R/ir/expr_emit.R`
- Pipeline/scheduling context: `docs/PIPELINE/SCHEDULING.md`, `docs/IR/SPEC.md`, `docs/IR/CONTRACT.md`

## Beginner Debug Checklist

1. Confirm the function family and constructor signature in this doc first.
2. Check strict-lane constraints (direct var requirements, allowed types, arity).
3. For reductions, check whether routing stayed `linear` or was rewritten to `tree/simd`.
4. If `pmin/pmax` behavior surprises you, inspect call arity and emission aliasing rules.
