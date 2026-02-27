#' mojorGPU package namespace directives.
#'
#' @keywords internal
#' @import mojor
"_PACKAGE"

#' MojoR GPU Bridge and Context Utilities
#'
#' Utilities for loading the MojoR backend, querying GPU availability, and managing
#' the GPU context.
#'
#' @name mojor_gpu_bridge
#' @aliases mojor_is_loaded mojor_load mojor_has_gpu mojor_gpu_ctx_free
#' @aliases mojor_gpu_ctx_reset mojor_gpu_meminfo mojor_gpu_ctx_smoke
#' @export mojor_is_loaded
#' @export mojor_load
#' @export mojor_has_gpu
#' @export mojor_gpu_ctx_free
#' @export mojor_gpu_ctx_reset
#' @export mojor_gpu_meminfo
#' @export mojor_gpu_ctx_smoke
#' @usage
#' mojor_is_loaded()
#'
#' mojor_load(...)
#'
#' mojor_has_gpu()
#'
#' mojor_gpu_ctx_free()
#'
#' mojor_gpu_ctx_reset(mode = c("soft", "hard"))
#'
#' mojor_gpu_meminfo()
#'
#' mojor_gpu_ctx_smoke(script = NULL, strict = FALSE)
#' @param ... Arguments forwarded to `mojor::mojor_load()`.
#' @param mode Reset mode. `"soft"` clears cached context reference; `"hard"` is not supported in-process.
#' @param script Optional path to a Mojo smoke-test script.
#' @param strict If `TRUE`, smoke-test failures raise an error. Otherwise they produce a warning.
#' @return
#' - `mojor_is_loaded()`, `mojor_has_gpu()`: logical scalar.
#' - `mojor_load()`: `TRUE` invisibly on successful load/bind.
#' - `mojor_gpu_ctx_free()`, `mojor_gpu_ctx_reset()`: logical success flag invisibly.
#' - `mojor_gpu_meminfo()`: list with `free_bytes`, `total_bytes`, `free_gb`, `total_gb`, and `status`.
#' - `mojor_gpu_ctx_smoke()`: list (`ok`, `output`, `script`) invisibly.
#' @examples
#' \dontrun{
#' mojor_load()
#' if (mojor_has_gpu()) {
#'   print(mojor_gpu_meminfo())
#' }
#' }
NULL

#' Low-Level GPU Kernel Wrappers
#'
#' Direct wrappers around MojoR GPU kernels for sigmoid/affine transformation pipelines.
#'
#' @name mojor_gpu_lowlevel_kernels
#' @aliases mojor_sigmoid_f32_gpu mojor_sigmoid_f64_gpu mojor_sigmoid_f32_gpu_iters
#' @aliases mojor_sigmoid_affine_f32_gpu mojor_sigmoid_affine_f32_gpu_iters
#' @aliases mojor_sigmoid_affine_f32_gpu_chain mojor_sigmoid_affine_f32_gpu_chain_sum
#' @aliases mojor_gpu_chain
#' @export mojor_sigmoid_f32_gpu
#' @export mojor_sigmoid_f64_gpu
#' @export mojor_sigmoid_f32_gpu_iters
#' @export mojor_sigmoid_affine_f32_gpu
#' @export mojor_sigmoid_affine_f32_gpu_iters
#' @export mojor_sigmoid_affine_f32_gpu_chain
#' @export mojor_sigmoid_affine_f32_gpu_chain_sum
#' @export mojor_gpu_chain
#' @usage
#' mojor_sigmoid_f32_gpu(x)
#'
#' mojor_sigmoid_f64_gpu(x)
#'
#' mojor_sigmoid_f32_gpu_iters(x, iters = 5L)
#'
#' mojor_sigmoid_affine_f32_gpu(x, scale = 1, bias = 0)
#'
#' mojor_sigmoid_affine_f32_gpu_iters(x, iters = 5L, scale = 1, bias = 0)
#'
#' mojor_sigmoid_affine_f32_gpu_chain(
#'   x,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L
#' )
#'
#' mojor_sigmoid_affine_f32_gpu_chain_sum(
#'   x,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L,
#'   reduction = c("block", "warp")
#' )
#'
#' mojor_gpu_chain(
#'   x,
#'   iters = 5L,
#'   op = "sigmoid",
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L
#' )
#' @param x Input vector (numeric or float32-compatible where applicable).
#' @param iters Iteration count for repeated transform application.
#' @param scale Affine scale parameter.
#' @param bias Affine bias parameter.
#' @param post_scale Second-stage affine scale for chained variants.
#' @param post_bias Second-stage affine bias for chained variants.
#' @param post_iters Second-stage iteration count for chained variants.
#' @param reduction Reduction backend used by `mojor_sigmoid_affine_f32_gpu_chain_sum()`.
#' @param op Operation family for `mojor_gpu_chain()` (currently `"sigmoid"`).
#' @return
#' - f32 kernels return float32 vectors (class from package `float`).
#' - `mojor_sigmoid_f64_gpu()` returns a numeric vector.
#' - `mojor_sigmoid_affine_f32_gpu_chain_sum()` returns a numeric scalar sum.
#' @examples
#' \dontrun{
#' x <- runif(1024)
#' y <- mojor_sigmoid_affine_f32_gpu_chain(x, iters = 3, scale = 1.2, bias = 0.1)
#' s <- mojor_sigmoid_affine_f32_gpu_chain_sum(x, reduction = "block")
#' }
NULL

#' GPU Buffer Constructors and Accessors
#'
#' Allocate, read, write, and free raw GPU buffers for float32/float64 payloads.
#'
#' @name mojor_gpu_buffers
#' @aliases mojor_gpu_buf_f32 mojor_gpu_buf_f32_write mojor_gpu_buf_f32_read
#' @aliases mojor_gpu_buf_f32_free mojor_gpu_buf_f32_live_count
#' @aliases print.mojor_gpu_buf_f32 as.numeric.mojor_gpu_buf_f32
#' @aliases mojor_gpu_buf_f64 mojor_gpu_buf_f64_write mojor_gpu_buf_f64_read
#' @aliases mojor_gpu_buf_f64_free print.mojor_gpu_buf_f64 as.numeric.mojor_gpu_buf_f64
#' @export mojor_gpu_buf_f32
#' @export mojor_gpu_buf_f32_write
#' @export mojor_gpu_buf_f32_read
#' @export mojor_gpu_buf_f32_free
#' @export mojor_gpu_buf_f32_live_count
#' @export print.mojor_gpu_buf_f32
#' @export as.numeric.mojor_gpu_buf_f32
#' @export mojor_gpu_buf_f64
#' @export mojor_gpu_buf_f64_write
#' @export mojor_gpu_buf_f64_read
#' @export mojor_gpu_buf_f64_free
#' @export print.mojor_gpu_buf_f64
#' @export as.numeric.mojor_gpu_buf_f64
#' @usage
#' mojor_gpu_buf_f32(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"))
#'
#' mojor_gpu_buf_f32_write(buf, values)
#'
#' mojor_gpu_buf_f32_read(buf)
#'
#' mojor_gpu_buf_f32_free(buf)
#'
#' mojor_gpu_buf_f32_live_count()
#'
#' \method{print}{mojor_gpu_buf_f32}(x, ...)
#'
#' \method{as.numeric}{mojor_gpu_buf_f32}(x, ...)
#'
#' mojor_gpu_buf_f64(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"))
#'
#' mojor_gpu_buf_f64_write(buf, values)
#'
#' mojor_gpu_buf_f64_read(buf)
#'
#' mojor_gpu_buf_f64_free(buf)
#'
#' \method{print}{mojor_gpu_buf_f64}(x, ...)
#'
#' \method{as.numeric}{mojor_gpu_buf_f64}(x, ...)
#' @param n Buffer length when `x` is not provided.
#' @param api Requested GPU API.
#' @param buf GPU buffer object of the matching class.
#' @param values Numeric values to copy into `buf`.
#' @return
#' - Constructors return buffer handles of class `mojor_gpu_buf_f32`/`mojor_gpu_buf_f64`.
#' - `*_write()` returns the input buffer invisibly.
#' - `*_read()`/`as.numeric()` return numeric host values.
#' - `*_free()` returns `TRUE` invisibly.
#' - `mojor_gpu_buf_f32_live_count()` returns the current live f32 buffer count.
#' @examples
#' \dontrun{
#' buf <- mojor_gpu_buf_f32(n = 16)
#' mojor_gpu_buf_f32_write(buf, rnorm(16))
#' host <- as.numeric(buf)
#' mojor_gpu_buf_f32_free(buf)
#' }
NULL

#' GPU Array Wrapper
#'
#' High-level GPU array container that tracks handle, dtype, and optional shape metadata.
#' Instances are R6-style objects (class `mojor_gpu_array`) with mutable payload semantics.
#'
#' @name mojor_gpu_array
#' @aliases mojor_gpu_array mojor_gpu_array_free mojor_gpu_array_write mojor_gpu_array_read
#' @export mojor_gpu_array
#' @export mojor_gpu_array_free
#' @export mojor_gpu_array_write
#' @export mojor_gpu_array_read
#' @usage
#' mojor_gpu_array(
#'   x = NULL,
#'   n = NULL,
#'   api = c("auto", "metal", "cuda", "amd"),
#'   dtype = c("auto", "f32", "f64")
#' )
#'
#' mojor_gpu_array_free(buf)
#'
#' mojor_gpu_array_write(buf, values)
#'
#' mojor_gpu_array_read(buf)
#'
#' show(object)
#'
#' length(x)
#'
#' dim(x)
#'
#' dimnames(x)
#'
#' as.array(x, ...)
#'
#' as.matrix(x, ...)
#'
#' as.numeric(x, ...)
#' @param dtype Storage type (`"f32"` or `"f64"`), or `"auto"`.
#' @return
#' - `mojor_gpu_array()` returns an R6-style object of class `mojor_gpu_array`.
#' - `mojor_gpu_array_write()` returns the array invisibly.
#' - `mojor_gpu_array_read()` and `as.numeric()` return host numeric data.
#' - `mojor_gpu_array_free()` returns the array object with cleared handle/metadata.
#' @examples
#' \dontrun{
#' ga <- mojor_gpu_array(matrix(runif(12), nrow = 3), dtype = "f32")
#' print(ga)
#' host <- mojor_gpu_array_read(ga)
#' ga <- mojor_gpu_array_free(ga)
#' }
NULL

#' High-Level GPU Array Operations
#'
#' Apply GPU transformations and reductions over numeric inputs or `mojor_gpu_array` objects.
#'
#' @name mojor_gpu_ops
#' @aliases mojor_gpu_sigmoid mojor_gpu_affine mojor_gpu_linear mojor_gpu_chain_array mojor_gpu_sum
#' @export mojor_gpu_sigmoid
#' @export mojor_gpu_affine
#' @export mojor_gpu_linear
#' @export mojor_gpu_chain_array
#' @export mojor_gpu_sum
#' @usage
#' mojor_gpu_sigmoid(x, api = c("auto", "metal", "cuda", "amd"))
#'
#' mojor_gpu_affine(x, scale = 1, bias = 0, api = c("auto", "metal", "cuda", "amd"))
#'
#' mojor_gpu_linear(x, scale = 1, bias = 0, api = c("auto", "metal", "cuda", "amd"))
#'
#' mojor_gpu_chain_array(
#'   x,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L,
#'   api = c("auto", "metal", "cuda", "amd")
#' )
#'
#' mojor_gpu_sum(
#'   x,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L
#' )
#' @return
#' - `mojor_gpu_sigmoid()`, `mojor_gpu_affine()`, `mojor_gpu_linear()`, and
#'   `mojor_gpu_chain_array()` return `mojor_gpu_array`.
#' - `mojor_gpu_sum()` returns a numeric scalar.
#' @examples
#' \dontrun{
#' x <- runif(2048)
#' y <- mojor_gpu_linear(x, scale = 0.5, bias = 0.1)
#' s <- mojor_gpu_sum(y, iters = 2)
#' }
NULL

#' GPU Session API
#'
#' Stateful GPU session helpers for repeated chained operations over float32 payloads.
#'
#' @name mojor_gpu_session
#' @aliases mojor_gpu_session mojor_gpu_session_run mojor_gpu_session_sum
#' @aliases mojor_gpu_session_free
#' @export mojor_gpu_session
#' @export mojor_gpu_session_run
#' @export mojor_gpu_session_sum
#' @export mojor_gpu_session_free
#' @usage
#' mojor_gpu_session(x)
#'
#' mojor_gpu_session_run(
#'   session,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L
#' )
#'
#' mojor_gpu_session_sum(
#'   session,
#'   iters = 5L,
#'   scale = 1,
#'   bias = 0,
#'   post_scale = 1,
#'   post_bias = 0,
#'   post_iters = 0L
#' )
#'
#' mojor_gpu_session_free(session)
#'
#' show(object)
#' @param session A `mojor_gpu_session` object.
#' @return
#' - `mojor_gpu_session()` and `mojor_gpu_session_run()` return formal S4 `mojor_gpu_session` objects.
#' - `mojor_gpu_session_sum()` returns a numeric scalar.
#' - `mojor_gpu_session_free()` returns the session object invisibly after release.
#' @examples
#' \dontrun{
#' s <- mojor_gpu_session(runif(1024))
#' s <- mojor_gpu_session_run(s, iters = 3)
#' mojor_gpu_session_sum(s)
#' mojor_gpu_session_free(s)
#' }
NULL

#' Validate GPU Buffer Release Behavior
#'
#' Run a function twice and verify that live GPU f32 buffer count is unchanged between runs.
#'
#' @name mojor_gpu_check_release
#' @aliases mojor_gpu_check_release
#' @export mojor_gpu_check_release
#' @usage
#' mojor_gpu_check_release(fn, ...)
#' @param fn Function to run and verify.
#' @return
#' An invisible list with elements `first` and `second`, containing outputs from the two invocations.
#' @examples
#' \dontrun{
#' mojor_gpu_check_release(function(x) mojor_gpu_sigmoid(x), runif(256))
#' }
NULL
