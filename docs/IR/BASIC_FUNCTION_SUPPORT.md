# MöjoR Basic Operators and Math Function Support

This page is a focused reference for basic DSL-level scalar operators and call-style math functions (for example `+`, `-`, `sin`, `cos`, `log`, `sqrt`).

Scope:
- Tree IR `unop`/`binop` operators.
- Tree IR `call` functions in the basic math/predicate set.
- Strict-mode arity/type behavior at verifier/typing/emission boundaries.

Out of scope:
- Higher-order functions (`vapply`, `sapply`, `mapply`).
- Set/match/data-frame/string/gpu-specific breadth nodes.
- Full source-language lowering coverage details (see `TREE_NODE_REFERENCE.md`).

Limitation tracking note:
- This page is a focused support matrix for basic operator/function lanes.
- Use `docs/KNOWN_ISSUES.md` for the consolidated current limitations/deferred routes.

## Plain-Language Mental Model

This is the "can I use this basic function/operator in strict mode?" table.

- `L4`: strong strict support (verify + type + emit).
- `L3`: supported, but with important constraints/caveats.
- `L2`: accepted in some paths, but strict behavior is incomplete or conservative.

## Mini Example

For `.mojor_ir_call("sqrt", list(.mojor_ir_const("4.0")))`:

1. `sqrt` is `L4` here (strict route).
2. Verifier enforces unary arity.
3. Typing infers numeric output and emission proceeds on standard scalar lane.

## Support Legend

| Level | Meaning |
|---|---|
| `L4` | Strictly supported and part of normal typed+emitted scalar path |
| `L3` | Supported with caveats (arity/spelling/type-lane constraints) |
| `L2` | Accepted at parse/verify level, but typing/strict behavior is conservative |

## Unary Operators (`unop`)

| Operator | Level | Notes |
|---|---|---|
| `-` | `L4` | Numeric negation; result type follows operand type. |
| `!` | `L4` | Logical negation; typing returns `bool` and coercion is enforced where needed. |

## Binary Operators (`binop`)

| Operator family | Operators | Level | Notes |
|---|---|---|---|
| Arithmetic | `+`, `-`, `*`, `/`, `%%`, `%/%` | `L4` | Promoted numeric typing in strict paths. |
| Comparison | `>`, `<`, `>=`, `<=`, `==`, `!=` | `L4` | Type-checks to boolean result. |
| Logical short-circuit | `&&`, `||` | `L4` | Typed as boolean result. |
| Bitwise/logical elementwise | `&`, `|`, `^` | `L2` | Accepted by verifier; typing is conservative/unknown in current scalar typing rules. |

## Basic `call(...)` Functions

### Unary math/trig/exponential family

| Function | Level | Arity | Notes |
|---|---|---|---|
| `sin`, `cos`, `tan` | `L4` | 1 | Standard trig scalar route. |
| `asin`, `acos`, `atan` | `L4` | 1 | Inverse trig scalar route. |
| `sinh`, `cosh`, `tanh` | `L4` | 1 | Hyperbolic functions. |
| `asinh`, `acosh`, `atanh` | `L4` | 1 | Inverse hyperbolic functions. |
| `log`, `log10`, `log1p`, `log2` | `L4` | 1 | Scalar log family. |
| `exp`, `expm1` | `L4` | 1 | Exponential family. |
| `sqrt` | `L4` | 1 | Square root. |
| `abs` | `L4` | 1 | Absolute value. |
| `abs2` | `L4` | 1 | Emitted as `x * x`. |
| `floor`, `trunc`, `round` | `L4` | 1 | Scalar rounding family. |
| `ceiling` | `L3` | 1 | Accepted in verifier/build paths; emission maps to Mojo `ceil(...)`. |
| `sign`, `cbrt`, `lgamma`, `erf` | `L4` | 1 | Supported unary math extensions. |

### Unary predicate/coercion utilities

| Function | Level | Arity | Notes |
|---|---|---|---|
| `as.logical` | `L4` | 1 | Coerces scalar expression to boolean semantics. |
| `is.na`, `is.nan`, `is.finite`, `is.infinite` | `L4` | 1 | Typed boolean result; emission uses type-aware checks. |
| `length`, `nrow`, `ncol` | `L4` | 1 | Typed as `i32`. |

### Shape-query note (`dim`)

`dim` is represented as a dedicated IR node (not a normal `call`-family entry in this table).

- Standalone `.mojor_ir_dim` strict coverage is tracked as `L2` in
  `docs/IR/DSL_FUNCTION_SUPPORT.md` (strict verifier/typing covered for direct
  matrix/fixed-rank ND vars; lowering/emission remains route-constrained).
- Source-level scalar forms `dim(arg)[k]` are supported on strict loop-range
  rewrite lanes when `k` is a positive integer-valued scalar lane (positive
  literal or supported scalar expression coercible to integer) and `arg` is a
  direct matrix/fixed-rank ND function argument.

### Binary scalar math

| Function | Level | Arity | Notes |
|---|---|---|---|
| `atan2`, `hypot`, `pow` | `L4` | 2 | Verifier enforces binary arity. |

### Min/max family

| Function | Level | Arity | Notes |
|---|---|---|---|
| `min`, `max` | `L4` | 1 or 2 (call-node arity check) | Additional source-level folding exists for broader `min/max` forms. |
| `pmin`, `pmax` | `L3` | 1 or 2 (call-node arity check) | Emission maps to `min/max` scalar form; lane constraints still apply. |

## Verifier Arity Contract

Exact verifier-side arity guards:

1. Unary call family (`sin`, `cos`, ..., `erf`, `as.logical`, `length`, `is.*`) must have exactly 1 argument.
2. Binary call family (`atan2`, `hypot`, `pow`) must have exactly 2 arguments.
3. `min`, `max`, `pmin`, `pmax` are currently call-validated for 1 or 2 arguments at node verifier level.
4. RNG calls are excluded from this basic arity table and follow RNG metadata bounds.

## Type and Emission Notes

1. Most basic math calls infer `f64` in typing (`sin`, `cos`, `log`, `sqrt`, etc.).
2. Predicate calls (`is.*`, `as.logical`) infer `bool`.
3. `min/max/pmin/pmax` with two args use type-promotion logic.
4. Emission aliases:
 - `ceiling` -> `ceil`
 - `pmin` -> `min`
 - `pmax` -> `max`
5. `abs2(x)` is emitted as multiply (`x * x`), not a runtime helper call.
6. Bitwise/logical elementwise operators (`&`, `|`, `^`) are verifier-accepted but currently infer `unknown` in scalar typing.
7. `ceiling` is emitted via alias to `ceil`; typing caveats can appear before emission in strict lanes.

## Representative Diagnostics

These are common and expected when input shape/arity does not match the contract.

1. `IR verify [call]: '<fn>' expects 1 argument, got N`
2. `IR verify [call]: '<fn>' expects 2 arguments, got N`
3. `IR verify [call]: '<fn>' expects 1 or 2 arguments, got N`
4. Strict-lane type unknowns downstream when using conservative operators (`&`, `|`, `^`) or unsupported call forms.

## Intentional Non-Scope

This document does not claim support for:

1. HOF/apply families (`vapply`, `sapply`, `mapply`, `lapply`)
2. Sampling/RNG breadth
3. Set/match/data-frame/regex/gpu breadth nodes
4. Full variadic `min/max/pmin/pmax` node semantics at call-node verifier layer (handled through source lowering/folding paths)

## Source-of-Truth Files

- `packages/mojor/R/ir/verify.R` (allowed operators + call arity checks)
- `packages/mojor/R/ir/typing.R` (inferred return types and coercion)
- `packages/mojor/R/ir/expr_emit.R` (call/operator emission mapping)
- `packages/mojor/R/ir/expr_build.R` (source-call to IR-call construction)

## Beginner Debug Checklist

1. Confirm operator/function is listed here and check its level.
2. For `L3`/`L2`, check arity and typing caveats first.
3. If verifier rejects a call, inspect function name spelling and argument count.
4. If typing/emission differ from expectation, check alias rules (`ceiling`/`pmin`/`pmax`) and inferred types.

## Practical Sanity Checks

1. Basic unary call:
```r
node <- .mojor_ir_call("sqrt", list(.mojor_ir_const("4.0")))
.mojor_ir_verify(node)
```

2. Expected arity failure:
```r
node <- .mojor_ir_call("atan2", list(.mojor_ir_const("1.0")))
try(.mojor_ir_verify(node))
```

3. Alias path expectations:
```r
node <- .mojor_ir_call("ceiling", list(.mojor_ir_var("x")))
# Emission aliases to ceil(...); inspect emitted code if behavior differs.
```
