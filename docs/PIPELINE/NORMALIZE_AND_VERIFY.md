# Normalize and Verify

This stage makes IR canonical and rejects illegal structures before expensive passes.

## Plain-Language Mental Model

This stage is a "clean and safety-check" gate.

- Normalize: make equivalent shapes look the same.
- Verify: reject shapes that are invalid or unsafe.

If this stage fails, later optimization/emission stages are intentionally blocked.

## Normalize

Normalize rewrites equivalent structures into consistent forms.

Why this matters:
- Later passes rely on predictable node shapes.
- Fewer structural variants means simpler optimization/type logic.

Typical operations:
- Canonical node shape cleanup.
- Dim/type-adjacent normalization helpers used before strict checking.

## Mini Example

Two source patterns that mean the same thing can become one canonical shape:

- Pattern A and Pattern B both normalize into the same internal node form.
- Later passes then only need to handle one form, not both.

This reduces bugs and avoids pass duplication.

## Verify

Verify enforces structural and legality rules.

Checks include:
- Required fields and valid tokens.
- Scope and loop-control legality (`break`/`next` context).
- Assignment LHS legality.
- Strict-route checks that reject compatibility escapes.

## Strictness Behavior

- Strict mode (`ir_only = TRUE`): verify failures are hard errors.
- Compatibility mode: selected failures can route through guarded fallback behavior depending on context.

## Code Mapping

- Normalization helpers: `packages/mojor/R/transpile/ir_helpers.R`
- Tree IR verifier: `packages/mojor/R/ir/verify.R`
- Shared pipeline wrappers: `packages/mojor/R/transpile/ir_helpers.R`

## Implementation Ownership

| Sub-stage | Primary owners | Guarantee |
|---|---|---|
| Tree normalization | `.mojor_ir_normalize(...)` in `packages/mojor/R/ir/utils.R` | Canonical node form before strict verification and later passes. |
| Pipeline-wrapped prepare+verify | `.mojor_prepare_transpile_ir_stmt(...)` and `.mojor_verify_ir_stmt(...)` in `packages/mojor/R/transpile/ir_helpers.R` | Route-aware strict/compat error policy around normalize+verify. |
| Structural legality verification | `.mojor_ir_verify(...)` in `packages/mojor/R/ir/verify.R` | Enforces node shape, scope/loop legality, and strict compatibility boundaries. |

## Verifier Context Contract

The verifier context (`ctx`) controls policy for legality checks. Key fields include:
- `ir_only`: strict-mode toggle rejecting compatibility-only forms.
- `check_scope`, `defined_vars`, `loop_vars`, `in_loop`: scope and control-flow legality.
- `type_env`: optional type context for type-sensitive structural checks.

## Diagnostic Surfaces

- Missing required fields or invalid tokens fail at the first offending node.
- Invalid loop-control placement (`break`/`next` outside loop) is rejected early.
- Strict mode turns compatibility escapes into hard errors instead of route-dependent fallback.

## Representative Verifier Diagnostics

Common verifier messages include:

- `IR verify: node missing valid 'kind' field`
- `IR verify [break]: break statement outside loop`
- `IR verify [next]: next statement outside loop`
- `IR verify [range]: step cannot be zero`
- `IR verify [index]: indices list cannot be empty`
- `IR verify [scheduled_reduce]: mode must be one of tree/simd`

These point to IR shape/legality problems, not runtime toolchain issues.

## Beginner Debug Checklist

1. Check whether failure is normalize-time or verify-time.
2. If verify fails, find the first offending node and inspect its `kind` fields.
3. For loop-control errors, verify `break`/`next` are inside a legal loop context.
4. If strict mode is enabled, retry once in compatibility mode to confirm whether this is a strictness boundary.

## Internal Symbol Index

`Normalization and strict wrappers`
- `.mojor_ir_normalize`
- `.mojor_prepare_transpile_ir_stmt`
- `.mojor_transpile_try_stage`
- `.mojor_verify_ir_stmt`

`Tree verifier`
- `.mojor_ir_verify`

`Verifier policy context fields (runtime contract)`
- `ir_only`
- `check_scope`
- `defined_vars`
- `loop_vars`
- `in_loop`
- `type_env`
