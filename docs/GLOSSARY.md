# MöjoR Glossary (Plain Language)

This page defines common MöjoR codewords in plain language.

For detailed implementation docs by stage/concept, see [PIPELINE/README.md](./PIPELINE/README.md).

## Core Compiler Terms

- **AST (Abstract Syntax Tree):** The parsed tree form of your R code before MöjoR converts it to IR.
- **IR (Intermediate Representation):** The compiler's internal language between parsing and final code generation.
- **HIR (High-Level IR):** Richer IR with convenient forms that are easier to build from R syntax.
- **LIR (Low-Level IR):** Simplified IR after lowering; closer to final code emission.
- **SSA (Static Single Assignment):** IR style where each value is assigned once; helpful for strict verification and backend analysis.
- **CFG (Control-Flow Graph):** Blocks and branches that represent all control-flow paths.

## Pipeline Stages

- **Build:** Convert R AST into Tree IR nodes.
- **Normalize:** Canonicalize structure so equivalent code looks the same to later passes.
- **Verify:** Enforce structural and legality rules (for example loop/control-flow constraints).
- **Optimize:** Apply safe rewrites for speed/size.
- **Lower:** Rewrite high-level IR forms into lower-level core forms.
- **Schedule:** Attach/choose execution metadata (for example unroll/tile/reduction routing).
- **Type Check:** Infer/validate types and insert needed casts.
- **Emit:** Generate Mojo source code from typed IR.

## Optimization Terms

- **CSE (Common Subexpression Elimination):** Reuse the same pure computation instead of recalculating it.
- **LICM (Loop-Invariant Code Motion):** Move loop-invariant work outside loops.
- **DICM:** Common typo/shorthand in discussions; usually means LICM.
- **DCE (Dead Code Elimination):** Remove calculations whose results are never used.
- **Fusion (Loop Fusion):** Merge compatible adjacent loops to reduce overhead and improve locality.
- **SIMD (Single Instruction, Multiple Data):** Vectorized execution for data-parallel operations.
- **Unroll:** Duplicate loop body work per iteration to reduce loop overhead.
- **Tile:** Split large loop spaces into blocks/chunks for cache/locality.

## Effect and Legality Model

- **Effect class:** Describes behavior of an expression/statement for safety reasoning.
- **Legality class:** Optimizer-facing classification used to decide what rewrites are safe.

Effect classes (Tree IR):
- **Pure:** No side effects and no mutable-state dependency.
- **ReadsMem:** Reads state/memory but does not write.
- **RNG:** Uses random state; order matters.
- **Unknown:** Not enough proof to be safe; treat conservatively.

Legality classes (optimization):
- **None:** Safe for elimination/reuse/reordering in local legality scope.
- **Read:** Reads resources; may conflict with nearby writes.
- **Write:** Mutates resources; ordering and aliasing constraints apply.
- **RNG:** Random/stateful effect; not reorderable.
- **Unknown:** Hard barrier unless stronger proof exists.

## Runtime and Interface Terms

- **ABI (Application Binary Interface):** Binary contract for how compiled code calls/returns values.
- **FFI (Foreign Function Interface):** Mechanism for calling native code from R and vice versa.
- **`.Call` bridge:** R C-API entrypoint used to invoke compiled native wrappers.
- **Backend:** Native Mojo/C implementation layer that executes kernels.
- **Stub backend:** Compatibility backend used when full Mojo backend is unavailable.
- **Shared library (`.so`, `.dylib`, `.dll`):** Compiled artifact loaded by R for native execution.

## MöjoR Policy and Semantics Terms

- **`ir_only` (strict mode):** Require strict IR path; disallow compatibility raw escapes.
- **Compatible mode:** Allows guarded fallback behavior when strict IR path is unavailable.
- **Compatibility fallback (`object_mode` fallback):** Interpreted R execution used when the compiled path is unavailable and `object_mode` permits fallback.
- **Fallback:** Switching to an alternate supported path when a preferred path is not legal/available.
- **`object_mode`:** Build fallback policy in `mojor_build`/`mojor_fn`.
 - `"off"`: strict compiled subset only.
 - `"fallback"`: interpreted R fallback for fallback-class misses.
 - `"hybrid"`: mixed plan that keeps accelerable parts compiled where possible.
- **`na_mode` / `na_guard`:** Policy controlling NA handling and generated NA checks.
- **`bounds_check`:** Whether generated code includes bounds guards.
- **`index_base`:** Whether indexing is interpreted as one-based or zero-based.
- **`array_layout`:** Memory layout policy (for example column-major).
- **`layout_ctx`:** Threaded layout/index metadata used during lowering/emission.
- **`len_var_map` / `nrow_var_map` / `ncol_var_map`:** Maps used for safe dimension/bounds logic.
- **`semantics = "r"`:** R-safe semantics (includes copy-on-modify behavior where applicable).
- **`semantics = "raw"`:** Performance-oriented semantics; fewer safety semantics checks.
- **`gpu_jit_mode`:** GPU lowering policy.
 - `"auto"`: allow unified/helper-backed selection by lowering analysis.
 - `"unified_preview"`: reject helper-backed routes and require unified helper-free lowering.
- **Preview path:** Constrained compatibility/lowering lane for selected forms (especially
  no-loop rewrites in `mojor_fn` and unified GPU helper-free lowering). Not a product beta flag.
- **Copy-on-modify:** Duplicate shared R objects before in-place mutation to preserve R semantics.
- **`object_mode = "off"`:** Run in compiled subset mode with strict feature constraints.

## Project Workflow Terms

- **Package source tree:** `packages/mojor/` implementation source.
- **Package-authoritative workflow:** Development happens directly in `packages/mojor/`.
- **Mirror parity (historical):** Former requirement to keep packages/mojor/package trees aligned.
- **Route audit:** Check which execution routes were used (for example GPU vs CPU fallback paths).
