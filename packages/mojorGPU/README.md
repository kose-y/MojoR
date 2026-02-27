# mojorGPU

Direction note: we are introducing an intermediate IR layer between the R AST and Mojo codegen (see `IR_PLAN.md`).

GPU helpers and array wrappers for MöjoR. Requires `mojor`.

## Quick start

```r
library(mojorGPU)

# Ensure the backend is loaded first
mojor::mojor_load()

x <- runif(1e6)
# float32 GPU buffer
xg <- mojor_gpu_array(x, dtype = "f32")

# Elementwise kernel (GPU)
yg <- mojor_gpu_sigmoid(xg)

y <- mojor_gpu_array_read(yg)
```

## Notes

- GPU buffers are managed by Mojo backend symbols exposed by the `mojor` package.
- Default GPU memory cap is 16GB (see `mojor_options(gpu_max_bytes=...)`).
