# SSA Backend Inspection Guide

This guide explains how to inspect `emit_ssa_backend = TRUE` output and map it to the backend-prep pipeline.

## Plain-Language Mental Model

`emit_ssa_backend = TRUE` exposes compiler artifacts after Tree IR scheduling and before backend codegen.
You can inspect:
- staged Tree IR snapshots,
- semantic SSA snapshots,
- backend CFG shape,
- stage trace (`trans$ssa_backend$pipeline`).

## Mini Example

```r
f <- function(x, n) {
  out <- numeric(n)
  for (i in seq_len(n)) out[i] <- x[i] + 1
  out
}

trans <- mojor_transpile(
  f,
  x = "f64[]",
  n = "i32",
  emit_ssa_backend = TRUE
)

stopifnot(is.null(trans$ssa_backend_error))
```

## Pipeline Trace

```r
trans$ssa_backend$pipeline
```

Typical order:
- `dump` (function/expression input)
- `normalize`
- `verify_ir`
- `optimize_ir`
- `verify_ir`
- `lower_ir`
- `verify_ir`
- `schedule_ir`
- `verify_ir`
- `to_ssa`
- `memory_canonicalize`
- `annotate_effects_resources`
- `loop_recovery`
- `recanonicalize_loop_cfg`
- `fusion_candidate_analysis`
- `typing`
- `verify_ssa`
- `optimize_ssa`
- `verify_ssa`
- `backend_cfg`

Checks:
- `schedule_ir` must occur before `to_ssa`.
- `fusion_candidate_analysis` is analysis-only in backend prep (no forced rewrite stage).

## Artifact Map

```r
names(trans$ssa_backend$ir)
# input, normalized, optimized, lowered, scheduled

names(trans$ssa_backend$ssa)
# raw, optimized

cfg <- trans$ssa_backend$backend_cfg
cfg$kind
cfg$entry
cfg$block_order
names(cfg$blocks)
```

Backend CFG blocks contain:
- `params`
- `stmts`
- `term` (`ret`, `br`, `condbr`)
- `successors`

## Common Inspections

### Control flow present?

```r
has_control_flow <- function(cfg) {
  any(vapply(cfg$blocks, function(b) {
    term_kind <- b$term$kind
    term_kind %in% c("condbr", "br")
  }, logical(1)))
}
```

### Back-edge/loop detection (CFG)

```r
has_loops <- function(cfg) {
  succ <- lapply(cfg$blocks, function(b) {
    s <- b$successors
    if (is.null(s)) character(0) else unname(unlist(s, use.names = FALSE))
  })

  visited <- setNames(as.list(rep(FALSE, length(succ))), names(succ))
  in_stack <- setNames(as.list(rep(FALSE, length(succ))), names(succ))

  dfs <- function(node) {
    visited[[node]] <<- TRUE
    in_stack[[node]] <<- TRUE
    for (nxt in succ[[node]]) {
      if (!isTRUE(visited[[nxt]]) && dfs(nxt)) return(TRUE)
      if (isTRUE(in_stack[[nxt]])) return(TRUE)
    }
    in_stack[[node]] <<- FALSE
    FALSE
  }

  for (node in names(succ)) {
    if (!isTRUE(visited[[node]]) && dfs(node)) return(TRUE)
  }
  FALSE
}
```

### Phi-like statement presence

```r
has_phi <- function(block) {
  any(vapply(block$stmts, function(s) identical(s$kind, "phi"), logical(1)))
}
```

## Limitations Snapshot

- `emit_ssa_backend = TRUE` is an inspection surface for backend-prep stages, not a guarantee of final backend codegen coverage.
- `fusion_candidate_analysis` is analysis-only in this path.
- Unsupported strict routes can fail before `backend_cfg`; compatibility mode can still route through guarded non-strict paths.
- Canonical current limitations are tracked in `docs/KNOWN_ISSUES.md`.

## Beginner Debug Checklist

1. Verify `trans$ssa_backend_error` is `NULL` before reading artifacts.
2. Inspect `trans$ssa_backend$pipeline` first; confirm stage order and boundaries.
3. If SSA verifier failures appear, map to stage boundary (`verify_ssa` after typing/optimize).
4. If CFG shape is suspect, inspect `cfg$entry`, `cfg$block_order`, and each block terminator.

## Related Documents

- [../PIPELINE/SSA.md](../PIPELINE/SSA.md) - SSA stage overview
- [SSA_OP_REFERENCE.md](./SSA_OP_REFERENCE.md) - Canonical SSA node/op schema reference
- [ABI.md](./ABI.md) - Backend ABI contracts
