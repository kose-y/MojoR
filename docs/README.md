# MöjoR Documentation

Welcome to the MöjoR documentation. MöjoR is a "Numba for R" system that transpiles a supported subset of R into Mojo and compiles native kernels for fast numeric loops.

## CI

GitHub Actions CI:
- `![CI](https://github.com/kose-y/MojoR/actions/workflows/ci.yml/badge.svg)`
- [Workflow](https://github.com/kose-y/MojoR/actions/workflows/ci.yml)

Local CI (manual):
- `bash scripts/check_continuous_verification.sh --allow-mismatch --allow-artifacts --allow-drift`
- `bash scripts/check_continuous_verification.sh --allow-mismatch --allow-artifacts --allow-drift --skip-package-tarball` (fast local pass)
- `bash scripts/check_release_candidate.sh --profile release-freeze --perf-baseline docs/BASELINES/PERF_BASELINE_SAMPLE.csv --perf-current docs/BASELINES/PERF_BASELINE_SAMPLE.csv --without-gpuarray-f64-routes` (strict package-only release-freeze gate)

## Quick Start

- **[COOKBOOK.md](./COOKBOOK.md)** - Primary user guide (examples + workflows)
- **[TROUBLESHOOTING.md](./TROUBLESHOOTING.md)** - Canonical error diagnosis and fixes
- **[KNOWN_ISSUES.md](./KNOWN_ISSUES.md)** - Consolidated known limitations and constraints
- **[DEV_ISSUES.md](./DEV_ISSUES.md)** - Contributor-facing development constraints and workflow sharp edges
- **[../BUILD_AND_TEST.md](../BUILD_AND_TEST.md)** - Build and test commands

Supporting guides:
- **[ROLES/README.md](./ROLES/README.md)** - Role-based "first 2 hours" onboarding guides
- **[GLOSSARY.md](./GLOSSARY.md)** - Plain-language definitions for compiler/runtime codewords
- **[API_SURFACE.md](./API_SURFACE.md)** - Canonical exported API vs development-only helper boundary
- **[DEBUG_MODE_GUIDE.md](./DEBUG_MODE_GUIDE.md)** - Debug mode user guide
- **[ERROR_RECOVERY_GUIDE.md](./ERROR_RECOVERY_GUIDE.md)** - Error recovery user guide
- **[DIAGNOSTICS_INDEX.md](./DIAGNOSTICS_INDEX.md)** - Canonical diagnostic text -> boundary -> owner map
- **[DIAGNOSTICS_API.md](./DIAGNOSTICS_API.md)** - Callable diagnostics helper reference
- **[GPUARRAY_GUIDE.md](./GPUARRAY_GUIDE.md)** - GPUArray user guide
- **[GPUARRAY_CLASS_SUPPORT.md](./GPUARRAY_CLASS_SUPPORT.md)** - GPUArray class API, support levels, and fallback routes
- **[GPU_FUNCTION_SUPPORT_MATRIX.md](./GPU_FUNCTION_SUPPORT_MATRIX.md)** - Canonical GPU function support notation matrix
- **[FEATURES.md](./FEATURES.md)** - Overview of supported features
- **[SUBSET.md](./SUBSET.md)** - Stable R subset contract

## Core Documentation

### [IR/](./IR/) - Intermediate Representation

- **[SPEC.md](./IR/SPEC.md)** - IR Specification
- **[CONTRACT.md](./IR/CONTRACT.md)** - IR Contracts
- **[EFFECT_SYSTEM.md](./IR/EFFECT_SYSTEM.md)** - Effect System
- **[IMPLEMENTATION.md](./IR/IMPLEMENTATION.md)** - Implementation Details
- **[PLAN.md](./IR/PLAN.md)** - IR Direction and Architecture
- **[COVERAGE.md](./IR/COVERAGE.md)** - Coverage Matrix
- **[DSL_FUNCTION_SUPPORT.md](./IR/DSL_FUNCTION_SUPPORT.md)** - DSL constructor support levels (L4/L3/L2/L1)
- **[BASIC_FUNCTION_SUPPORT.md](./IR/BASIC_FUNCTION_SUPPORT.md)** - Basic operators/math function support (`+`, `-`, `sin`, `cos`, ...)
- **[FUNCTION_SUPPORT_BREADTH.md](./IR/FUNCTION_SUPPORT_BREADTH.md)** - HOF/set-match/data-frame/string/GPU/reduction breadth signatures and mode constraints
- **[TREE_NODE_REFERENCE.md](./IR/TREE_NODE_REFERENCE.md)** - Complete Tree IR node reference

### [PIPELINE/](./PIPELINE/) - Step-by-Step Deep Dives

- **[README.md](./PIPELINE/README.md)** - Pipeline index and map
- **[API.md](./PIPELINE/API.md)** - API entrypoints and pipeline wiring
- **[AST_AND_BUILD.md](./PIPELINE/AST_AND_BUILD.md)** - R AST to Tree IR build
- **[HIR.md](./PIPELINE/HIR.md)** - High-level Tree IR model
- **[NORMALIZE_AND_VERIFY.md](./PIPELINE/NORMALIZE_AND_VERIFY.md)** - Canonicalization and legality checks
- **[OPTIMIZATION.md](./PIPELINE/OPTIMIZATION.md)** - CSE/LICM/DCE and pass order
- **[CSE.md](./PIPELINE/CSE.md)** - Common subexpression elimination
- **[LICM.md](./PIPELINE/LICM.md)** - Loop-invariant code motion
- **[DCE.md](./PIPELINE/DCE.md)** - Dead code elimination
- **[LIR.md](./PIPELINE/LIR.md)** - Lowered Tree IR contract
- **[SCHEDULING.md](./PIPELINE/SCHEDULING.md)** - Scheduling metadata and policy
- **[TYPING.md](./PIPELINE/TYPING.md)** - Type check and cast insertion
- **[EMISSION.md](./PIPELINE/EMISSION.md)** - Typed IR to Mojo source
- **[SSA.md](./PIPELINE/SSA.md)** - SSA boundary and downstream passes
- **[CFG.md](./PIPELINE/CFG.md)** - Backend CFG and loop recovery
- **[EFFECTS_LEGALITY.md](./PIPELINE/EFFECTS_LEGALITY.md)** - Effect/resource/legality model
- **[RUNTIME_AND_ABI.md](./PIPELINE/RUNTIME_AND_ABI.md)** - Runtime bridge and ABI surface

### [BASELINES/](./BASELINES/) - Baseline Artifacts

- **[README.md](./BASELINES/README.md)** - Baseline artifact location and usage notes

### [DESIGN/](./DESIGN/) - Design Documents

- **[FUSION.md](./DESIGN/FUSION.md)** - Loop Fusion Design
- **[GUARDS.md](./DESIGN/GUARDS.md)** - Guard Pass Design
- **[TYPED.md](./DESIGN/TYPED.md)** - Typed IR Pass
- **[VERIFIER.md](./DESIGN/VERIFIER.md)** - Verifier Strengthening
- **[LOGICAL_INDEX.md](./DESIGN/LOGICAL_INDEX.md)** - Logical Index Design
- **[MATRIX_OUTPUT.md](./DESIGN/MATRIX_OUTPUT.md)** - Matrix Output Design
- **[CODEGEN.md](./DESIGN/CODEGEN.md)** - Codegen Design

### [BREADTH/](./BREADTH/) - Breadth Expansion

- **[PLAN.md](./BREADTH/PLAN.md)** - Breadth Expansion Plan
- **[BROADCAST_ND.md](./BREADTH/BROADCAST_ND.md)** - broadcast_nd
- **[STRING_OPS.md](./BREADTH/STRING_OPS.md)** - String Operations

### [BACKEND/](./BACKEND/) - Backend Documentation

- **[SSA_GUIDE.md](./BACKEND/SSA_GUIDE.md)** - SSA Backend Guide
- **[SSA_OP_REFERENCE.md](./BACKEND/SSA_OP_REFERENCE.md)** - Complete SSA node/op schema reference
- **[ABI.md](./BACKEND/ABI.md)** - ABI Documentation

## Additional Resources

- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - Architecture Overview
- **[CHANGELOG.md](./CHANGELOG.md)** - Changelog

## Package Documentation

- [`packages/mojor/README.md`](../packages/mojor/README.md) - Canonical package README

## Getting Help

For questions or issues, check [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) or open an issue on GitHub.
