# mojor

`mojor` transpiles numeric R loops to Mojo-backed kernels with CPU and GPU routes.

## Core API

- `mojor_fn()`: compile a loop-style R function and return a callable object.
- `mojor_transpile()`: transpile and inspect generated code.
- `mojor_jit()` / `mojor_njit()`: JIT entry points.
- `mojor_has_gpu()`: detect runtime GPU availability.

## Minimal example

```r
library(mojor)

f <- function(x, y, n) {
  for (i in 1:n) {
    y[i] <- x[i] * 2
  }
}

jf <- mojor_fn(f, x = "f64[]", y = "f64[]", n = "i32")
```

## Notes

- This package ships prebuilt backend bundles for supported platforms and validates symbol contracts during `configure`.
- Source installs require `mojo` on PATH with version `0.26.x` (see repo helper: `scripts/install_mojo.sh`).
- In a source checkout, deeper project docs and implementation plans live under `docs/`.
