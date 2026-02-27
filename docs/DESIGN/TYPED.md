# Typed IR Pass Design

## Goal
Add type information to IR nodes and insert explicit casts to centralize type checking, coercion rules, and ensure type-safe Mojo code generation.

**Status:** Implemented as the standard pre-emission typing stage.

## Plain-Language Mental Model

Typed IR answers "what type is this expression here?" at every step.

- It annotates nodes with inferred types.
- It inserts explicit casts where needed.
- It enforces compatibility before emission.

This prevents late codegen surprises and keeps type rules in one place.

## Mini Example

If one side is `i32` and the other is `f64`:

- Untyped IR only sees a binary operation.
- Typed pass infers target type (`f64`) and inserts cast on `i32` side.

Now later stages no longer guess conversion behavior.

## Beginner Debug Checklist

1. Check inferred types on both operands before debugging operator behavior.
2. Confirm cast insertion happened where promotion rules require it.
3. For condition failures, verify boolean coercion path was applied.
4. If type stays `unknown`, trace where env/type metadata was missing.

## Overview
The typed IR pass is a **transformation** that takes untyped IR and returns the same IR structure with:
1. Type annotations on expression nodes
2. Explicit `cast` nodes inserted where conversions are needed
3. Boolean coercion for conditions centralized

## Type System

### Basic Types

Vectors, matrices, and higher-dimensional arrays are all **ndarrays**
distinguished only by their rank (number of dimensions).

```r
# Scalar types (rank 0)
"f64"     # Float64
"f32"     # Float32
"i32"     # Int32
"bool"    # Bool
"unknown" # Type inference failed

# 1D / vector types (rank 1) — [1d] is an alias for []
"f64[]", "f32[]", "i32[]"        # canonical form
"f64[1d]", "f32[1d]", "i32[1d]"  # alias (accepted everywhere)

# 2D / matrix types (rank 2)
"f64[,]", "f64[2d]"

# ND / array types (rank N)
"f64[3d]", "f64[4d]", ...
```

### Type Lattice
```
 unknown
 |
 ----------------
 | | |
 f64 f32 i32 bool
```

Promotion rules:
- `i32` + `f64` → `f64` (insert cast on i32)
- `i32` + `f32` → `f32` (insert cast on i32)
- `f64` + `f32` → `f64` (insert cast on f32)
- `bool` + numeric → numeric (insert cast on bool to i32, then to target)

## IR Node Changes

### Expression Nodes (Enhanced)
All expression nodes get a `type` field:

```r
# Before typed pass:
list(kind = "const", value = "5", src = ...)

# After typed pass:
list(kind = "const", value = "5", type = "i32", src = ...)

# Before typed pass:
list(kind = "binop", op = "+", lhs = ..., rhs = ..., src = ...)

# After typed pass:
list(kind = "binop", op = "+", lhs = ..., rhs = ..., type = "f64", src = ...)
```

### No New Node Types
We already have `cast` nodes:
```r
list(kind = "cast", to = "f64", expr = ..., type = "f64", src = ...)
```

## Type Inference Algorithm

### Step 1: Bottom-Up Type Inference

```r
.mojor_ir_infer_type <- function(node, type_env) {
 # type_env: named list mapping variable names to types

 if (node$kind == "const") {
 # Infer from value
 if (is.logical(node$value)) return("bool")
 if (is.integer(node$value)) return("i32")
 if (is.numeric(node$value)) return("f64")
 return("unknown")
 }

 if (node$kind == "var") {
 # Look up in environment
 return(type_env[[node$name]] %||% "unknown")
 }

 if (node$kind == "cast") {
 # Cast node specifies its type
 return(node$to)
 }

 if (node$kind == "unop") {
 arg_type <- .mojor_ir_infer_type(node$expr, type_env)
 if (node$op == "!") return("bool")
 if (node$op == "-") return(arg_type) # Preserve numeric type
 return("unknown")
 }

 if (node$kind == "binop") {
 # Infer from operands
 lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
 rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)
 return(.mojor_ir_promote_types(lhs_type, rhs_type))
 }

 # ... other node types
}
```

### Step 2: Cast Insertion

```r
.mojor_ir_insert_casts <- function(node, target_type) {
 # Insert cast nodes where type conversions are needed
 if (node$type != target_type) {
 return(list(kind = "cast", to = target_type, expr = node, type = target_type))
 }
 return(node)
}
```

## Representative Typing Diagnostics

Common strict-typing failures include:

- `mojor_transpile: strict IR type check failed: compiled subset node '<kind>' has unknown result type in <context>`
- `mojor_transpile: strict IR type check failed: compiled subset node '<kind>' type contract failed in <context>: <verify message>`
- `mojor_transpile: integer output requires explicit cast from f64 (use as.integer())`
- `mojor_transpile: unsupported subscript assignment index type; expected scalar index or supported vector index in assignment`

These failures indicate type contract issues before emission.

## Implementation Mapping

- Type inference/cast insertion: `packages/mojor/R/ir/typing.R`
- Typed annotation helpers: `packages/mojor/R/ir/type_guards.R`
- Pipeline integration: `packages/mojor/R/transpile/ir_helpers.R` (`.mojor_transpile_type_and_emit_stmt`)

---

## Related Documents

- [IMPLEMENTATION.md](../IR/IMPLEMENTATION.md) - Implementation Details
- [CONTRACT.md](../IR/CONTRACT.md) - IR Contracts
