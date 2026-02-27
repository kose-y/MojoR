# Compiler Dev Track: First 2 Hours

## Goal
Understand the transpilation and IR pipeline, then validate with focused tests.

## 0-35 min: Read architecture and IR contracts

1. `AGENTS.md`
2. `docs/ARCHITECTURE.md`
3. `docs/GLOSSARY.md`
4. `docs/IR/SPEC.md`
5. `docs/IR/CONTRACT.md`
6. `docs/IR/IMPLEMENTATION.md`

## 35-70 min: Open core pipeline files

1. `packages/mojor/R/transpile/entry_stages.R`
2. `packages/mojor/R/transpile/transpile.R`
3. `packages/mojor/R/transpile/ir_helpers.R`
4. `packages/mojor/R/ir/verify.R`
5. `packages/mojor/R/ir/stmt_emit.R`

Quick pass discovery:

```bash
rg -n "normalize|verify|opt|lower|schedule|type|emit" packages/mojor/R/transpile/*.R packages/mojor/R/ir/*.R
```

## 70-105 min: Run focused IR tests

```bash
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_ir_verify.R")'
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_ir_guard_cse.R")'
Rscript -e 'source("packages/mojor/R/mojor.R"); testthat::test_file("packages/mojor/tests/testthat/test_ir_effect_legality.R")'
```

## 105-120 min: Dump and inspect one IR sample

```r
source("packages/mojor/R/mojor.R")
ir <- mojor_ir_dump(quote({
 y <- 0
 for (i in 1:length(x)) y <- y + x[i]
 y
}))
mojor_ir_print(ir)
```
