# MöjoR

MöjoR is a "Numba for R" system that transpiles a supported subset of R into Mojo and compiles native kernels for fast numeric loops.

## Problem This Solves

R has had a long-standing gap (for over a decade) between:

- ergonomic high-level code, and
- compiler-backed performance with practical GPU array workflows.

MöjoR targets that gap by combining:

- compilation of supported R kernels to fast native code, and
- a flexible `GPUArray` runtime/API surface for explicit GPU-oriented workflows.

## Repository Layout

- `packages/mojor/` is the authoritative source tree for R code, native/backend assets, tests, and package tools.
- `docs/` contains user, architecture, IR, pipeline, and release documentation.
- `scripts/` contains release and verification gates.

## Quick Start

- Build/test commands: [`BUILD_AND_TEST.md`](./BUILD_AND_TEST.md)
- Primary docs index: [`docs/README.md`](./docs/README.md)
- Package entrypoint docs: [`packages/mojor/README.md`](./packages/mojor/README.md)
- User guide and examples: [`docs/COOKBOOK.md`](./docs/COOKBOOK.md)
- Stable subset contract: [`docs/SUBSET.md`](./docs/SUBSET.md)
- API boundary (exported vs dev): [`docs/API_SURFACE.md`](./docs/API_SURFACE.md)
- Troubleshooting: [`docs/TROUBLESHOOTING.md`](./docs/TROUBLESHOOTING.md)

## Install

Mojo toolchain `0.26.x` is required for backend builds.

Quick check:

```bash
bash scripts/install_mojo.sh --check
```

Install Mojo first (recommended: `pixi`, from Modular docs):

```bash
curl -fsSL https://pixi.sh/install.sh | sh
pixi init mojor-env -c https://conda.modular.com/max/ -c conda-forge && cd mojor-env
pixi add "mojo==0.26.1"
pixi shell
mojo --version
```

If `mojo --version` does not start with `0.26`, stop and pin the version before building MöjoR.

Alternative install (if you already use `uv`):

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv && source .venv/bin/activate
uv pip install "mojo==0.26.1" --extra-index-url https://modular.gateway.scarf.sh/simple/
mojo --version
```

From repo root:

```bash
bash packages/mojor/build.sh
R CMD build packages/mojor
R CMD INSTALL mojor_*.tar.gz
```

R package dependencies for development:

```r
install.packages(c("testthat", "float"))
```

## Platform and GPU Support

- Host platforms: Linux and macOS (Apple Silicon) are supported.
- GPU backends: Apple Metal and CUDA routes are supported (backend capability dependent).
- GPU validation: release/test lanes include Apple GPU and CUDA GPU coverage when matching hardware is available.
- Canonical GPU support matrix: [`docs/GPU_FUNCTION_SUPPORT_MATRIX.md`](./docs/GPU_FUNCTION_SUPPORT_MATRIX.md)

## Common Gates

- Continuous verification:
  - `bash scripts/check_continuous_verification.sh --allow-mismatch --allow-artifacts --allow-drift`
- Release readiness:
  - `bash scripts/check_release_readiness.sh`
- Release candidate:
  - `bash scripts/check_release_candidate.sh`
