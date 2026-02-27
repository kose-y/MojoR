# Release/Maintainer Track: First 2 Hours

## Goal
Validate package docs, metadata, build/install flow, and basic release readiness.

## 0-30 min: Regenerate package docs

```bash
cd .
Rscript scripts/document_roxygen.R
```

Review:

1. `packages/mojor/DESCRIPTION`
2. `packages/mojor/NAMESPACE`
3. `packages/mojor/man/`

## 30-70 min: Build and install package

```bash
R CMD build packages/mojor
R CMD INSTALL mojor_*.tar.gz
```

## 70-105 min: Run package tests

```bash
Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat")'
```

Optional f64 GPU coverage (only on f64-capable GPU backends):

```bash
MOJOR_TEST_GPU_F64=1 Rscript -e 'testthat::test_dir("packages/mojor/tests/testthat", filter = "^test_gpu_array_[0-9]{2}[.]R$")'
```

## 105-120 min: Review touched package areas

```bash
git diff -- packages/mojor
```

Read:

1. `BUILD_AND_TEST.md`
2. `packages/mojor/README.md`
3. `docs/COOKBOOK.md`
4. `docs/GLOSSARY.md`
