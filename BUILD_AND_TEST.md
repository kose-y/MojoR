# MöjoR: Build and Test Guide

**Last Updated:** 2026-02-21  
**Scope:** Package-authoritative workflow (`packages/mojor/`)

This guide covers the current developer workflow after removing `packages/mojor/`.

## Prerequisites

- R (>= 4.0)
- C toolchain (Xcode CLT on macOS, GCC/Clang on Linux)
- Mojo toolchain `0.26.x` (required for backend builds)

Mojo preflight helper:

```bash
bash scripts/install_mojo.sh --check
```

Install common R deps for local development:

```r
install.packages(c("testthat", "float"))
```

## Quick Start

From repo root:

```bash
bash packages/mojor/build.sh
```

Then in R:

```r
source("packages/mojor/R/mojor.R")
mojor_load("packages/mojor/build")
```

Quick smoke test:

```r
f <- function(x) {
  out <- numeric(length(x))
  for (i in seq_along(x)) out[i] <- x[i] * 2
  out
}

b <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
b$func(c(1, 2, 3))
```

## Build Commands

### Build backend + bridge

```bash
bash packages/mojor/build.sh
```

Primary artifacts:

- `packages/mojor/build/libmojor_backend.*`
- `packages/mojor/build/libmojor_backend_rng.*`
- `packages/mojor/build/libmojor_backend_gamma.*`
- `packages/mojor/build/mojor_bridge.so`

### Rebuild from scratch

```bash
rm -rf packages/mojor/build
bash packages/mojor/build.sh
```

### GPU API selection

```bash
MOJOR_GPU_API=metal bash packages/mojor/build.sh
# or: MOJOR_GPU_API=cuda / MOJOR_GPU_API=amd
```

### Strict GPU binding

```bash
MOJOR_STRICT_GPU_BIND=1 bash packages/mojor/build.sh
```

## Test Commands

### Full package testthat suite

```bash
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat")'
```

On Linux, prefer isolated per-file execution to avoid same-process instability:

```bash
R_BIN=Rscript bash packages/mojor/tools/run_testthat_isolated.sh
```

### Focused tests

```bash
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_gpu_array.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_ir_verify.R")'
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_error_recovery.R")'
```

### Parallel file-level sweep (package only)

```bash
bash scripts/run_testthat_parallel_sweep.sh --target package --jobs 25
```

## Package Build/Install

```bash
R CMD build packages/mojor
R CMD INSTALL mojor_*.tar.gz
```

## Roxygen Docs

```bash
Rscript scripts/document_roxygen.R
```

## Troubleshooting

### Build artifacts not loading

```bash
# macOS
otool -L packages/mojor/build/mojor_bridge.so

# Linux
ldd packages/mojor/build/mojor_bridge.so
```

### Stale testthat state

```bash
rm -f packages/mojor/tests/testthat/testthat-problems.rds
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat")'
```

### Missing path errors mentioning `packages/mojor/`

If you still see `packages/mojor/...` in local notes/scripts, migrate the command to `packages/mojor/...`.

## Repository Layout (Current)

- `packages/mojor/`: canonical source tree (R, src, tests, tools, build)
- `docs/`: user/developer documentation
- `scripts/`: repo maintenance and release/check scripts
- `tools/`: auxiliary developer tooling

## Related Docs

- `docs/COOKBOOK.md`
- `docs/TROUBLESHOOTING.md`
- `docs/ARCHITECTURE.md`
- `docs/IR/PLAN.md` (and related files under `docs/IR/`)
