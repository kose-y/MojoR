# Emission (Tree IR to Mojo Source)

Emission converts typed Tree IR into Mojo source lines.

## Plain-Language Mental Model

Emission is the "write final code text" stage.

- Earlier stages decide legality and types.
- Emission turns that validated IR into concrete Mojo syntax.

If unresolved sugar reaches emission, that is usually a lowering gap, not an emission policy choice.

## Inputs

- Typed Tree IR.
- Layout/index context.
- Guard policies (`na_mode`, `bounds_check`).
- Schedule metadata (unroll/reduction routing information).

## Main Responsibilities

- Emit statement structure (`loop`, `if`, assignment, returns).
- Emit expression syntax in scalar/index contexts.
- Maintain R-facing indexing semantics while generating Mojo indexing.
- Insert guard logic for NA/bounds where policy requires.
- Emit deterministic constructor-loop lookup logic for literal `c(...)` ranges
  without a small fixed literal-count cap.

## Mini Example

A typed loop-assignment IR node becomes emitted loop code lines.

- Input: typed `loop` + `index` + `assign` nodes.
- Output: concrete Mojo loop syntax with index math and any required guard checks.

The emitted text should reflect policy choices like bounds checks and NA handling.

## Guard Handling

- Bounds guards use current layout length/dimension maps.
- NA guards are emitted according to mode.
- Guard CSE avoids repeated identical guard code in statement scope.

## IR Emitter Boundary (Unified)

Emission routes through one unified internal emitter lane.

- `MOJOR_IR_SERVICE` and `MOJOR_IR_EMIT_SHADOW` are deprecated and ignored.
- When either env var is set, transpile emits a deterministic warning:
  `mojor_transpile: MOJOR_IR_SERVICE and MOJOR_IR_EMIT_SHADOW are deprecated and ignored; unified IR emitter is always used`
- Unsupported IR kinds hard-fail deterministically in the unified emitter path.
- Transpile outputs no longer expose `trans_out$ir_service`.

Typed-LIR payload serialization remains internally available behind
`MOJOR_EMIT_TYPED_LIR_PAYLOAD=1`, but it is not part of public transpile output.

## Output

- Mojo source text for kernel body and helpers.
- Metadata used by build/wrapper packaging steps.

## Code Mapping

- Statement emitter: `packages/mojor/R/ir/stmt_emit.R`
- Expression emitter: `packages/mojor/R/ir/expr_emit.R`
- Bounds/NA helpers: `packages/mojor/R/ir/bounds_emit.R`

## Implementation Ownership

| Concern | Primary owners | Responsibility |
|---|---|---|
| Statement emission | `.mojor_ir_stmt_emit(...)` in `packages/mojor/R/ir/stmt_emit.R` | Emits control flow, assignments, reductions, and statement-level helper calls. |
| Expression emission | `.mojor_ir_expr_emit(...)` in `packages/mojor/R/ir/expr_emit.R` | Emits scalar/index expression strings with typed coercion behavior, including vector-index `ifelse(...)` per-loop element reads on strict index lanes. |
| Index/range emission | `.mojor_ir_index_emit(...)` in `packages/mojor/R/ir/index_emit.R`; range helpers in `packages/mojor/R/ir/stmt_range_emit.R` | Preserves index-base semantics while generating legal Mojo indexing/range syntax, and routes assignment-LHS indexes through write-context emission (no read-helper lvalue). |
| Bounds guards | `.mojor_ir_emit_bounds_guards(...)` in `packages/mojor/R/ir/bounds_emit.R` | Emits and caches bounds checks for index-sensitive expressions. |
| NA guards | `.mojor_ir_emit_na_guard(...)` in `packages/mojor/R/ir/type_guards.R` | Emits policy-controlled NA checks (`forbid`, tolerant modes). |

## Emission Contract

- Input must already be typed/lowered/scheduled Tree IR.
- Emitted code must preserve R-facing indexing and NA semantics according to current policy knobs.
- Emission failures are treated as strict hard errors when `ir_only=TRUE`.

## Practical Failure Surfaces

- Remaining unresolved sugar node reaching emitter without lowering support.
- Incomplete layout/type metadata for bounds/index helper generation.
- Unsupported function/operator forms on strict emission routes.
- Assignment-lhs index expressions accidentally emitted through read helpers (non-mutable lvalue in Mojo).

## Representative Emission Diagnostics

Common emission-time messages include:

- `.mojor_ir_expr_emit: c_call function '<name>' not declared`
- `.mojor_ir_expr_emit: c_call returns must be a non-empty character string`
- `IR emit [vapply]: FUN.VALUE type must be numeric(1), integer(1), or logical(1)`
- `IR emit [mapply]: requires at least two vector arguments`
- `IR emit [<op>]: FUN body is not inlineable`
- `error: expression must be mutable in assignment` (usually points to non-write-context index emission on assignment lhs)
- `IR emit [dim]: unresolved runtime dim pointer for '<arg>' (ndim=<n>)`
- `IR emit [dim]: unsupported dim() source; use direct array variables with available dim metadata`

When these appear, the usual fix is to either:
- lower unsupported sugar before emission, or
- simplify to the currently supported compiled-subset form.

## Emission vs Earlier-Stage Bug Triage

- If the failing node is still high-level sugar, it is usually a lowering ownership gap (`LIR.md`), not an emitter bug.
- If the node is lowered but lacks type/layout metadata, it is usually a typing or context wiring issue (`TYPING.md` / transpile helpers).
- If emitted code is syntactically valid but runtime fails, continue in `RUNTIME_AND_ABI.md`.

## Beginner Debug Checklist

1. If emission fails on a node kind, confirm that kind should reach emitter in LIR.
2. Check typed metadata exists on the failing node and its operands.
3. For bounds/NA issues, verify policy knobs (`bounds_check`, `na_mode`) and required layout metadata.
4. For assignment-lhs index failures, confirm `.mojor_ir_index_emit(..., write_context=TRUE)` is used on the lhs route.
5. If strict route fails, isolate whether issue is unsupported form versus missing lower/type preparation.

## Internal Symbol Index

`Core emit entrypoints`
- `.mojor_ir_stmt_emit`
- `.mojor_ir_expr_emit`
- `.mojor_ir_loop_emit`
- `.mojor_ir_scheduled_reduce_emit`
- `.mojor_ir_emit_stmt_with_service`
- `.mojor_ir_service_mode`
- `.mojor_ir_service_type_and_emit_stmt`

`Index/range/guard emit helpers`
- `.mojor_ir_index_emit`
- `.mojor_ir_simple_range_emit`
- `.mojor_ir_emit_bounds_guards`
- `.mojor_ir_emit_na_guard`
- `.mojor_ir_collect_na_sources`
- `.mojor_ir_na_checks`
- `.mojor_ir_emit_len_lookup`
- `.mojor_ir_emit_ptr_lookup`

`N-d subscript emit helpers`
- `.mojor_classify_nd_indices` — classifies N-d index nodes into 5 dim kinds (missing, scalar, neg-scalar-excl, neg-vec-excl, pos-vec-select)
- `.mojor_emit_nd_loops` — generates nested loop structure from dim classification
- `.mojor_build_ternary_chain` — builds ternary expression for set-type vector selection indices
- `.mojor_ir_nd_exclusion_subset_emit` — read-side N-d subscript emission (`out <- mat[c(1,3), -2, ]`)
- `.mojor_ir_nd_exclusion_write_emit` — write-side N-d subscript emission (`out[c(1,3), -2] <- val`)
- `.mojor_ir_exclusion_subset_emit` — 1D scalar exclusion emission (`out <- x[-k]`)
- `.mojor_ir_vec_exclusion_subset_emit` — 1D vector exclusion emission (`out <- x[-c(1,2)]`)
- `.mojor_ir_vec_exclusion_write_emit` — 1D vector exclusion write emission (`out[-c(1,2)] <- val`)

`Specialized emission families`
- `.mojor_ir_apply_emit`
- `.mojor_ir_apply_assign_emit`
- `.mojor_ir_rng_vec_emit`
- `.mojor_ir_rng_vec_scalar_expr_emit`
- `.mojor_ir_sample_int_emit`
- `.mojor_ir_sample_emit`
- `.mojor_ir_sample_index_assign_emit`
- `.mojor_ir_tier7_vector_call_emit`
- `.mojor_ir_tier8_hof_assign_emit`
- `.mojor_ir_tier8_string_assign_emit`
