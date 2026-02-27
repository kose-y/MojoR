# Type Check and Typed Annotation

Typing enforces type correctness before emission.

## Plain-Language Mental Model

Typing is the "are these operations type-safe?" checkpoint.

- It determines types for expressions.
- It inserts casts where needed.
- It rejects combinations that cannot be made safe.

No code emission should happen before this gate succeeds.

## Goals

- Infer/validate expression and statement types.
- Insert explicit casts/coercions where required.
- Attach typed metadata used by emitter and diagnostics.

## Key Behaviors

- Assignment compatibility enforcement.
- Boolean-context checks.
- Scalar/array shape-aware type handling for supported subset forms.
- Type annotation normalization canonicalizes whitespace/layout variants
  (for example `f64[, ]` and `f64[,]` are treated identically).
- Numeric promotion keeps `f32` arithmetic lanes stable when mixed with float
  literals, avoiding incompatible `f64` upcasts in `f32` tensor writes.

## Mini Example

If a statement needs an explicit cast:

- Before typing: `y <- x + 1` where `y` expects a different numeric type.
- After typing: cast node inserted so assignment becomes type-compatible.

For control flow, `if (cond)` requires a legal boolean condition after typing/coercion rules.

## Output Contract

- Typed Tree IR suitable for final emission.
- In strict mode, typing failures are hard errors.

## Interaction with Earlier Stages

- Uses normalized/lowered/scheduled IR shape.
- Depends on consistent node forms from lower/schedule.

## Code Mapping

- Type inference/check: `packages/mojor/R/ir/typing.R`
- Pipeline wrappers: `packages/mojor/R/transpile/ir_helpers.R`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Type inference | `.mojor_ir_infer_type(...)` in `packages/mojor/R/ir/typing.R` | Computes expression-level type predictions from node structure and `type_env`. |
| Cast insertion/coercion | `.mojor_ir_insert_casts(...)` in `packages/mojor/R/ir/typing.R` | Inserts explicit casts to satisfy assignment and operator compatibility contracts. |
| Statement type checking | `.mojor_ir_type_check_stmt(...)` in `packages/mojor/R/ir/typing.R` | Validates statement legality (assignment targets, conditions, returns). |
| Typed annotation pass | `.mojor_ir_annotate_typed(...)` in `packages/mojor/R/ir/type_guards.R` | Attaches inferred `type` metadata on relevant nodes for emission/diagnostics. |
| Pipeline integration | `.mojor_transpile_type_and_emit_stmt(...)` in `packages/mojor/R/transpile/ir_helpers.R` | Executes type check + typed annotation immediately before statement emission. |

## Typed IR Contract

- Typed IR must be emission-ready: explicit casts are inserted where required.
- Boolean contexts (`if`, `while`, guard expressions) must type-check as legal boolean conditions.
- In strict mode, typing failures stop pipeline execution rather than returning partial output.

## Typical Typing Failures

- Assignment type mismatch (including scalar/array target incompatibility).
- Non-boolean conditions in control flow without legal coercion.
- Unsupported operator/type combinations in strict subset paths.

## Representative Typing Diagnostics

Common strict-typing messages include:

- `mojor_transpile: strict IR type check failed: compiled subset node '<kind>' has unknown result type in <context>`
- `mojor_transpile: strict IR type check failed: compiled subset node '<kind>' type contract failed in <context>: <verify message>`
- `mojor_transpile: integer output requires explicit cast from f64 (use as.integer())`
- `mojor_transpile: unsupported subscript assignment index type; expected scalar index or supported vector index in assignment`

Supported vector-index assignment lanes include direct `i32[]`/`chr[]` vars,
covered `i32[]` constructors/arithmetic forms, and supported `ifelse(...)`
`i32[]` index expressions.

These are usually contract errors, not optimizer bugs.

## Beginner Debug Checklist

1. Identify the first typing error, not the final cascade error.
2. Check assignment target type versus RHS inferred type.
3. Check `if`/`while` conditions are boolean-coercible.
4. If strict mode fails, test a minimal reduced expression to isolate unsupported operator/type pairs.

## Internal Symbol Index

`Typing core`
- `.mojor_ir_infer_type`
- `.mojor_ir_insert_casts`
- `.mojor_ir_type_check_stmt`
- `.mojor_ir_type_check_block`

`Typed annotation and guard helpers`
- `.mojor_ir_annotate_type`
- `.mojor_ir_annotate_typed`
- `.mojor_ir_bool_coerce`
- `.mojor_ir_reduction_acc_type`
