# Backend/Runtime Track: First 2 Hours

## Goal
Understand Mojo backend, bridge ABI, and runtime loading behavior.

## 0-30 min: Build and inspect artifacts

```bash
cd .
bash packages/mojor/build.sh
ls -lh packages/mojor/build
```

On macOS:

```bash
otool -L packages/mojor/build/mojor_bridge.so
```

## 30-70 min: Read backend and bridge code

1. `docs/GLOSSARY.md`
2. `packages/mojor/src/ABI.md`
3. `packages/mojor/src/bridge.c`
4. `packages/mojor/src/backend.mojo`
5. `packages/mojor/src/rng_helpers.mojo`
6. `packages/mojor/src/na_helpers.mojo`
7. `packages/mojor/src/set_match_helpers.mojo`
8. `packages/mojor/src/quantile_helpers.mojo`

## 70-105 min: Run runtime-facing tests

```bash
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_debug_runtime_simple.R")'
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_ffi.R")'
```

## 105-120 min: Optional GPU strict-binding check

```bash
MOJOR_STRICT_GPU_BIND=1 bash packages/mojor/build.sh
MOJOR_TEST_GPU_STRICT_BIND=1 Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_gpu_bridge_binding.R")'
```

Optional f64 GPU array coverage (for f64-capable GPU backends):

```bash
MOJOR_TEST_GPU_F64=1 Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
```
