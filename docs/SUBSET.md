# MöjoR Stable Subset (Current)

**Last validated:** 2026-02-23  
**Scope:** What strict-mode transpilation supports today.

## Contract

If your function fits this subset, `mojor_transpile()` / `mojor_build()` should compile through IR-native lanes. Unsupported forms fail with explicit diagnostics in strict mode.
This strict contract is the basis for MöjoR's scoped "Numba-style hot loop"
positioning.

## Function Shape

Supported:

- Sequential `for` loops and supported nested loops
- Expression-only functions (for supported reductions/math)
- Implicit vectorized expressions over typed array args (single-expression or
  multi-statement let-binding bodies with automatic inlining)
- Output as preallocated vector/matrix/array, or scalar accumulator
- Optional explicit `return()`

## Loop Ranges

Supported forms include:

- `seq_along(x)`
- `seq_len(n)`
- `seq_len(dim(arg)[k])` where `arg` is a matrix/fixed-rank ND function
  argument and `k` is a positive integer-valued scalar lane (positive integer
  literal or supported scalar expression coercible to integer)
- `seq.int(...)`
- `1:n`
- constructor-driven ranges (`c(...)`, `rep(...)`, `rep_len(...)`, `rep.int(...)`) under supported scalar constraints
- `for (v in x)` value iteration

## Types and Casts

Vectors, matrices, and higher-dimensional arrays are all **ndarrays**
distinguished only by their rank (number of dimensions):

| Rank | Notation             | R analogy |
|------|----------------------|-----------|
| 0    | `f64`                | scalar    |
| 1    | `f64[]` or `f64[1d]` | vector    |
| 2    | `f64[,]` or `f64[2d]`| matrix    |
| N    | `f64[Nd]`            | array     |

`[1d]` is an alias for `[]`; both forms are accepted everywhere.

Supported element-type hints:

- `f64`, `f64[]` / `f64[1d]`
- `f32`, `f32[]` / `f32[1d]`
- `i32`, `i32[]` / `i32[1d]`
- `lgl`, `lgl[]` / `lgl[1d]`

Supported explicit casts:

- `as.integer()`
- `as.double()`
- `as.single()`
- `as.float()` (compatibility alias for `as.single()`)
- `as.logical()`

## Expressions

Supported core operators/functions:

- Arithmetic: `+ - * / %/% %%`
- Comparisons: `== != < <= > >=`
- Logical: `&& || ! & | xor()`
- `ifelse()`, expression-form `if/else`
- Type checks: `is.na`, `is.nan`, `is.finite`, `is.infinite`
- Type casts: `as.integer()`, `as.double()`, `as.single()`, `as.float()`,
  `as.logical()`
- Reductions/wrappers: `sum`, `mean`, `min`, `max`, `prod`, `sd`, `var`, `any()`, `all()`, `which.min`, `which.max`
- Elementwise: `pmin`, `pmax`
- Math functions: `abs`, `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`,
  `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `log`, `log10`, `log2`,
  `log1p`, `exp`, `expm1`, `floor`, `ceiling`, `trunc`, `round`, `sign`,
  `cbrt`, `lgamma`, `erf`, `atan2`, `hypot`

## Indexing and Shapes

Supported:

- 1-D indexing and common affine index expressions
- Vector-index writes via direct `i32[]`/`chr[]` vars and covered `i32[]`
  constructor/arithmetic forms (`c()`, `rep()`, `rep_len()`, `seq()`, strict
  `+`/`-`/`*`/`%/%`/`%%` expressions over supported `i32[]` lanes, and strict
  `ifelse(cond, yes, no)` over supported logical + `i32[]` branch lanes, plus
  covered call-composed `pmin`/`pmax` (`min`/`max`) `i32[]` index expressions)
- Matrix indexing (`x[i, j]`) with column-major semantics
- Matrix missing-index slice forms (`x[i, ]`, `x[, j]`) on covered canonical
  assignment/read routes
- Local matrix/array allocation (`matrix(0, nrow=N, ncol=M)`,
  `array(0, dim=c(...))`) with slice assignment (`mat[i, ] <- c(...)`)
- Higher-dimensional array read/write for supported output declarations
- Supported slice assignment forms and logical mask writes
- Negative indexing: scalar (`x[-k]`), dynamic (`x[-idx]`), vector
  (`x[-c(1,2)]`, `x[-c(a,b)]`), range (`x[-(1:3)]`), N-d scalar
  (`mat[-1, j]`), N-d subscript with any combination of exclusion,
  vector selection, scalar, and missing dims (`out <- mat[c(1,3), -2]`,
  `out <- arr[1:3, , ]`, `out[c(1,3), ] <- val`)
  for any rank (2-D, 3-D+)
- Character dimname indexing on matrix/fixed-rank ND routes (including direct
  `chr[]` argument index vectors on covered strict lanes)
- Named vector character indexing (`c(a=1, b=2)["a"]`) via compile-time
  `names()` tracking
- Local character variable element-wise loop indexing
  (`rows <- c("r1", "r2"); mat[rows[i], "c1"]`) via compile-time position
  resolution against target dimnames

## Caveats

- Parallel loops currently require subprocess/executable execution path for reliability.
- Mixed-length arrays require explicit broadcast policy (`broadcast = "recycle"` or `"recycle_warn"`).
- NA behavior is policy-driven (`na_mode`), defaulting to strict checks.
- Advanced non-canonical vector-indexed ND gather/scatter writes remain
  subset-limited; see `docs/KNOWN_ISSUES.md`.
- Chained positive-selector indexing remains subset-limited in strict lanes
  (`x[i1][i2]`, matrix/ND chained gather forms).
- Some long-running 3D runtime dispatch paths remain partial under stress; keep
  high-rank workloads on covered canonical routes and verify with focused tests.

## Loop Body Expression Restrictions

**Supported inside loop bodies:**
- All operators and math functions listed in Expressions above
- Scalar local temporaries (`tmp <- expr`) and local array allocation
  (`temp <- numeric(K)` with indexed writes `temp[1] <- ...`)
- `if`/`else` statements and `ifelse()` ternary
- `break`, `next`, early `return()`
- Nested loops (`for`, `while`, `repeat`)
- Indexing: scalar `x[i]`, offset `x[i+1]`, multi-dim `mat[i,j]`,
  slices `x[1:n]`, logical masks `x[mask]` (1D, matrix, and ND array masks),
  negative `x[-k]` (scalar, dynamic, vector `x[-c(1,2)]`/`x[-c(a,b)]`,
  range `x[-(1:3)]`, N-d `mat[-1, j]`) on covered strict routes
- Array metadata: `length()`, `nrow()`, `ncol()`, `dim()`
- RNG calls: `runif()`, `rnorm()`, `sample.int()`

**Not supported inside loop bodies:**
- User-defined function calls (no closures, no call graph)
- Local array mutation without prior allocation (use `temp <- numeric(K)` first)
- List, data.frame, factor, or S3/S4 object operations
- `switch()`, `tryCatch()`, or error handling
- Recursive calls
- String manipulation beyond basic `nchar`/`substr`/`paste`
- Whole-vector slice allocation (`y <- x[1:k]`) or slice as reduction
  argument (`sum(x[1:i])`)

This is standard for an array-oriented compiler: no heap allocation, no dynamic
dispatch, no user-defined functions inside compiled kernels.

## Not Supported in Strict Mode

- Arbitrary R helper/function calls outside supported set
- NSE and full-language semantics
- Bytecode as a compilation target

## References

- `docs/COOKBOOK.md`
- `docs/FEATURES.md`
- `BUILD_AND_TEST.md`
- `packages/mojor/SUBSET_V1.md` (legacy detailed mirror)
