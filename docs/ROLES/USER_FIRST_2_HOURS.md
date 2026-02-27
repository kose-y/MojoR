# User Track: First 2 Hours

## Goal
Get a working MöjoR environment and run real kernels from R.

## 0-30 min: Build and smoke test

```bash
cd .
bash packages/mojor/build.sh
R -q
```

In R:

```r
source("packages/mojor/R/mojor.R")
mojor_load("packages/mojor/build")
mojor_has_gpu()
```

## 30-70 min: Run your first compiled kernel

```r
f <- mojor_fn(
 function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2
 out
 },
 x = "f64[]"
)

f(c(1, 2, 3))
```

## 70-100 min: Try preview/object-mode-off examples

```r
u <- mojor_fn(function(x) unique(x), x = "f64[]", object_mode = "off")
u(c(1, 2, 2, 3))

qf <- mojor_fn(function(x) quantile(x, c(0.25, 0.5, 0.75)), x = "f64[]", object_mode = "off")
qf(1:10)
```

## 100-120 min: Read key docs

1. `packages/mojor/README.md`
2. `docs/COOKBOOK.md`
3. `docs/SUBSET.md`
4. `docs/TROUBLESHOOTING.md`
5. `docs/GLOSSARY.md`
