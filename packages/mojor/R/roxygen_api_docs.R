#' MojoR package namespace directives.
#'
#' @keywords internal
#' @useDynLib mojor, .registration = TRUE, .fixes = "C_"
#' @importFrom R6 R6Class
#' @importFrom stats sd setNames var
#' @importFrom methods is new
#' @importFrom utils capture.output getFromNamespace modifyList tail
"_PACKAGE"

#' Transpile Supported R Functions to Mojo Source
#'
#' Transpile a supported subset of R into Mojo source without compiling.
#'
#' @name mojor_transpile
#' @aliases mojor_transpile
#' @export mojor_transpile
#' @usage
#' mojor_transpile(
#' fn,
#' ...,
#' name = "mojor_kernel",
#' na_mode = NULL,
#' fusion_debug = FALSE,
#' assume_aligned = NULL,
#' simd_mode = NULL,
#' elementwise = FALSE,
#' elementwise_target = c("cpu", "gpu"),
#' elementwise_size = NULL,
#' elementwise_cpu = NULL,
#' elementwise_gpu_layouttensor = NULL,
#' gpu_jit_mode = NULL,
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' ),
#' parallel = FALSE,
#' allow_in_place = FALSE,
#' semantics = c("r", "raw"),
#' unroll = NULL,
#' reduction = NULL,
#' bounds_check = NULL,
#' index_base = NULL,
#' array_layout = NULL,
#' tile = NULL,
#' emit_ir = FALSE,
#' emit_ssa_backend = FALSE,
#' opt_level = NULL,
#' fusion = TRUE,
#' debug = FALSE,
#' trace = FALSE,
#' memory_check = FALSE,
#' df_schema = NULL,
#' profile = FALSE
#' )
#' @param fn Function to transpile.
#' @param ... Named type annotations for arguments, e.g. `x = "f64[]"`, `n = "i32"`.
#'   Vectors, matrices, and higher-dimensional arrays are all ndarrays distinguished
#'   by rank: `"f64[]"` (or `"f64[1d]"`) for 1-D, `"f64[,]"` (or `"f64[2d]"`) for 2-D,
#'   `"f64[3d]"` for 3-D, etc.
#' @param name Kernel symbol name used in generated Mojo/C ABI glue.
#' @param na_mode NA policy override. `NULL` uses `mojor_options("na_mode")`.
#' @param fusion_debug If `TRUE`, emit fusion/SIMD routing diagnostics.
#' @param assume_aligned Optional positive integer alignment hint (bytes) for SIMD codegen.
#' @param simd_mode SIMD mode override (typically `"explicit"` or `"auto"`).
#' @param elementwise Enable elementwise lowering path when eligible.
#' @param elementwise_target Elementwise target, `"cpu"` or `"gpu"`.
#' @param elementwise_size Optional elementwise launch/work size hint.
#' @param elementwise_cpu Optional CPU-only elementwise override.
#' @param elementwise_gpu_layouttensor Optional GPU elementwise layout-tensor override.
#' @param gpu_jit_mode Optional GPU JIT mode override for elementwise GPU routing.
#' @param broadcast Broadcast/recycling policy.
#' @param parallel Enable parallel routing when legal.
#' @param allow_in_place Accept in-place intent metadata (currently conservative).
#' @param semantics Execution semantics mode: `"r"` or `"raw"`.
#' @param unroll Optional loop-unroll scheduling hint.
#' @param reduction Optional reduction scheduling mode.
#' @param bounds_check Optional bounds-check policy override.
#' @param index_base Optional index base override (`"one_based"` or `"zero_based"`).
#' @param array_layout Optional layout override (`"col_major"` or `"row_major"`).
#' @param tile Optional tiling metadata.
#' @param emit_ir If `TRUE`, include IR dump payload in output.
#' @param emit_ssa_backend If `TRUE`, include SSA-backend artifacts when available.
#' @param opt_level Optimization level override (`0..3` or `NULL`).
#' @param fusion Enable fusion analysis/routing hooks.
#' @param debug Enable debug-oriented code generation paths.
#' @param trace Enable verbose trace diagnostics.
#' @param memory_check Enable additional memory safety checks in generated wrapper paths.
#' @param df_schema Optional data.frame schema metadata for rewrite paths.
#' @param profile Attach profiling wrappers/metadata to results.
#' @return
#' A list describing transpilation output. Core fields include:
#' - `mojo`: generated Mojo source text.
#' - `name`: kernel symbol name.
#' - `types`: normalized argument type map.
#' - `out_kind`, `out_name`, `out_type`: output contract metadata.
#' - `n_source`: loop-length/source metadata.
#' - `ir`: IR payload (when requested).
#' - `ssa_backend`, `ssa_backend_status`: SSA backend artifacts and fallback status.
#' - `simd`, `elementwise`, `parallel`, `fusion`: scheduling metadata.
#' - `compatibility`: release/API/transpile versioning metadata for consumers.
#' - `rng_needed`, `na_mode`, `diagnostics`: runtime requirement flags and diagnostics.
#' @details
#' The accepted R subset is intentionally constrained; unsupported constructs fail fast
#' with explicit diagnostics. Use [mojor_build()] to compile the returned Mojo source.
#' @examples
#' f <- function(x) {
#' out <- numeric(length(x))
#' for (i in 1:length(x)) out[i] <- x[i] * 2
#' out
#' }
#'
#' \dontrun{
#' tr <- mojor_transpile(f, x = "f64[]")
#' cat(substr(tr$mojo, 1, 120), "...\n")
#' }
NULL

#' Build a Callable MojoR Kernel
#'
#' Transpile and compile a supported R function into a callable native kernel.
#'
#' @name mojor_build
#' @aliases mojor_build
#' @export mojor_build
#' @usage
#' mojor_build(
#' fn,
#' ...,
#' name = "mojor_kernel",
#' build_dir = tempdir(),
#' load = TRUE,
#' verbose = FALSE,
#' cache = TRUE,
#' cache_dir = NULL,
#' na_mode = NULL,
#' fusion_debug = FALSE,
#' assume_aligned = NULL,
#' simd_mode = NULL,
#' elementwise = FALSE,
#' elementwise_target = c("cpu", "gpu"),
#' elementwise_size = NULL,
#' elementwise_cpu = NULL,
#' elementwise_gpu_layouttensor = NULL,
#' gpu_jit_mode = NULL,
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' ),
#' fast_math = NULL,
#' parallel = FALSE,
#' semantics = c("r", "raw"),
#' unroll = NULL,
#' reduction = NULL,
#' bounds_check = NULL,
#' index_base = NULL,
#' array_layout = NULL,
#' tile = NULL,
#' opt_level = NULL,
#' r_jit_level = NULL,
#' debug = FALSE,
#' trace = FALSE,
#' memory_check = FALSE,
#' profile = FALSE,
#' error_mode = c("stop", "partial", "retry"),
#' max_retries = 3,
#' retry_delay = 0.1,
#' object_mode = c("off", "fallback", "hybrid"),
#' df_schema = NULL
#' )
#' @param fn Function to transpile and compile.
#' @param ... Named type annotations for arguments, e.g. `x = "f64[]"`, `n = "i32"`.
#'   Vectors, matrices, and higher-dimensional arrays are all ndarrays distinguished
#'   by rank: `"f64[]"` (or `"f64[1d]"`) for 1-D, `"f64[,]"` (or `"f64[2d]"`) for 2-D,
#'   `"f64[3d]"` for 3-D, etc.
#' @param name Kernel symbol name used in generated Mojo/C ABI glue.
#' @param na_mode NA policy override. `NULL` uses `mojor_options("na_mode")`.
#' @param fusion_debug If `TRUE`, emit fusion/SIMD routing diagnostics.
#' @param assume_aligned Optional positive integer alignment hint (bytes) for SIMD codegen.
#' @param simd_mode SIMD mode override (typically `"explicit"` or `"auto"`).
#' @param elementwise Enable elementwise lowering path when eligible.
#' @param elementwise_target Elementwise target, `"cpu"` or `"gpu"`.
#' @param elementwise_size Optional elementwise launch/work size hint.
#' @param elementwise_cpu Optional CPU-only elementwise override.
#' @param elementwise_gpu_layouttensor Optional GPU elementwise layout-tensor override.
#' @param gpu_jit_mode Optional GPU JIT mode override for elementwise GPU routing.
#' @param broadcast Broadcast/recycling policy.
#' @param parallel Enable parallel routing when legal.
#' @param semantics Execution semantics mode: `"r"` or `"raw"`.
#' @param unroll Optional loop-unroll scheduling hint.
#' @param reduction Optional reduction scheduling mode.
#' @param bounds_check Optional bounds-check policy override.
#' @param index_base Optional index base override (`"one_based"` or `"zero_based"`).
#' @param array_layout Optional layout override (`"col_major"` or `"row_major"`).
#' @param tile Optional tiling metadata.
#' @param opt_level Optimization level override (`0..3` or `NULL`).
#' @param r_jit_level R bytecode JIT level (`0..3`) used during transpile; `NULL` uses `mojor_options("r_jit_level")`.
#' @param debug Enable debug-oriented code generation paths.
#' @param trace Enable verbose trace diagnostics.
#' @param memory_check Enable additional memory safety checks in generated wrapper paths.
#' @param df_schema Optional data.frame schema metadata for rewrite paths.
#' @param build_dir Directory used for non-cached build artifacts.
#' @param load If `TRUE`, load compiled wrapper and return callable closures.
#' @param verbose If `TRUE`, print build commands.
#' @param cache If `TRUE`, reuse cached build artifacts by content hash.
#' @param cache_dir Optional cache root directory.
#' @param fast_math Optional fast-math flag toggle/override.
#' @param profile Attach profiling wrappers/metadata to results.
#' @param error_mode Error handling mode, one of `"stop"`, `"partial"`, or `"retry"`.
#' @param max_retries Maximum retries when `error_mode = "retry"`.
#' @param retry_delay Delay (seconds) between retries for retry mode.
#' @param object_mode Object fallback mode: `"off"`, `"fallback"`, or `"hybrid"`.
#' @return
#' A build result list. Common fields:
#' - `func`: callable R wrapper for compiled kernel (`load = TRUE`).
#' - `gpu_func`, `gpu_func_raw`: optional GPU wrappers when emitted.
#' - `build_dir`: artifact directory.
#' - `kernel`, `cache_key`, `wrapper_so`: build identifiers/paths.
#' - `trans`: transpilation payload from [mojor_transpile()].
#' - `success`: build success flag.
#' @examples
#' f <- function(x) {
#' s <- 0
#' for (i in 1:length(x)) s <- s + x[i]
#' s
#' }
#'
#' \dontrun{
#' b <- mojor_build(f, x = "f64[]")
#' b$func(c(1, 2, 3))
#' }
NULL

#' Function-First Convenience API for Building Kernels
#'
#' Convenience wrapper around [mojor_build()] for function-centric and signature-string workflows.
#'
#' @name mojor_fn
#' @aliases mojor_fn
#' @export mojor_fn
#' @usage
#' mojor_fn(signature, body, ..., name = NULL, cache = TRUE, load = TRUE, env = parent.frame())
#' @param signature Either an R function or a signature string such as `"sum1(x: f64[]) -> f64"`.
#' @param body Body expression/string when `signature` is a signature string.
#' @param ... Named type annotations passed through to [mojor_build()] for function signatures.
#' @param name Optional kernel symbol name override.
#' @param cache If `TRUE`, reuse build cache entries where possible.
#' @param load If `TRUE`, return a callable compiled function; otherwise return build metadata.
#' @param env Environment used when parsing signature-string bodies.
#' @return
#' If `load = TRUE`, returns a callable R function. Otherwise returns the list
#' produced by [mojor_build()].
#' @details
#' When `signature` is an R function, all formal arguments must receive explicit type
#' hints via `...`.
#' @examples
#' f <- function(x) {
#' y <- numeric(length(x))
#' for (i in 1:length(x)) y[i] <- x[i] + 1
#' y
#' }
#'
#' \dontrun{
#' g <- mojor_fn(f, x = "f64[]")
#' g(c(1, 2, 3))
#' }
NULL

#' Inspect MojoR Tree IR
#'
#' Build and inspect MojoR Tree IR for supported expressions/functions.
#'
#' @name mojor_ir_dump
#' @aliases mojor_ir_dump mojor_ir_print
#' @export mojor_ir_dump
#' @export mojor_ir_print
#' @usage
#' mojor_ir_dump(expr, env = parent.frame())
#'
#' mojor_ir_print(expr, env = parent.frame())
#' @param expr Expression, function body, function object, or previously built IR node list for `mojor_ir_print()`.
#' @param env Environment used to resolve symbols in expression/signature parsing paths.
#' @return
#' `mojor_ir_dump()` returns an IR node/list. `mojor_ir_print()` prints a formatted
#' view and returns the IR object invisibly.
#' @examples
#' f <- function(x) {
#' y <- 0
#' for (i in 1:length(x)) y <- y + x[i]
#' y
#' }
#'
#' ir <- mojor_ir_dump(body(f))
#' mojor_ir_print(ir)
NULL

#' Create a Runtime-Specializing JIT Wrapper
#'
#' Create a callable wrapper that infers argument types at runtime, builds kernels on demand,
#' and caches compiled call paths by signature.
#'
#' @name mojor_jit
#' @aliases mojor_jit
#' @export mojor_jit
#' @usage
#' mojor_jit(
#' fn,
#' ...,
#' name = "mojor_jit_kernel",
#' elementwise = FALSE,
#' elementwise_target = c("cpu", "gpu"),
#' elementwise_size = NULL,
#' elementwise_cpu = NULL,
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' ),
#' disk_cache = NULL,
#' cache_dir = NULL,
#' signatures = NULL,
#' eager = FALSE,
#' strict_signatures = NULL,
#' parallel = FALSE,
#' object_mode = c("off", "fallback", "hybrid"),
#' semantics = c("r", "raw"),
#' fast_math = NULL,
#' bounds_check = NULL
#' )
#' @param fn Function to specialize and compile.
#' @param ... Optional runtime type hints forwarded to [mojor_build()].
#' @param name Kernel name prefix.
#' @param elementwise Enable elementwise lowering when legal.
#' @param elementwise_target Elementwise target backend (`"cpu"` or `"gpu"`).
#' @param elementwise_size Optional elementwise launch/work size hint.
#' @param elementwise_cpu Optional CPU-only elementwise override.
#' @param broadcast Broadcast/recycling policy.
#' @param disk_cache If `TRUE`, persist JIT signature index on disk. If `NULL`,
#' uses `mojor_options("jit_disk_cache")`.
#' @param cache_dir Optional persistent cache directory used when disk cache is enabled.
#' @param signatures Optional declared signatures for eager precompile (named type-list signatures or signature strings).
#' @param eager If `TRUE`, compile all declared `signatures` at wrapper creation.
#' @param strict_signatures If `TRUE`, runtime signatures must match declared `signatures`.
#' If `NULL`, uses `mojor_options("jit_strict_signatures")`.
#' @param parallel Enable parallel loop routing when legal.
#' @param object_mode Object fallback mode: `"off"`, `"fallback"`, or `"hybrid"`.
#' @param semantics Semantics mode forwarded to [mojor_build()] (`"r"` or `"raw"`).
#' @param fast_math Fast-math toggle forwarded to [mojor_build()] (`TRUE`, `FALSE`, or `NULL`).
#' @param bounds_check Bounds-check toggle forwarded to [mojor_build()] (`TRUE`, `FALSE`, or `NULL`).
#' @return
#' A function. On each call it infers argument types, reuses/creates a compiled kernel
#' for that signature, and returns the kernel result.
#' @examples
#' f <- function(x) {
#' out <- numeric(length(x))
#' for (i in 1:length(x)) out[i] <- x[i] * 3
#' out
#' }
#'
#' \dontrun{
#' jf <- mojor_jit(f)
#' jf(c(1, 2, 3))
#' }
NULL

#' Strict Nopython-Style JIT Convenience Wrapper
#'
#' Convenience wrapper over [mojor_jit()] that forces `object_mode = "off"`.
#'
#' @name mojor_njit
#' @aliases mojor_njit
#' @export mojor_njit
#' @usage
#' mojor_njit(
#' fn,
#' ...,
#' name = "mojor_njit_kernel",
#' elementwise = FALSE,
#' elementwise_target = c("cpu", "gpu"),
#' elementwise_size = NULL,
#' elementwise_cpu = NULL,
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' ),
#' disk_cache = NULL,
#' cache_dir = NULL,
#' signatures = NULL,
#' eager = FALSE,
#' strict_signatures = NULL,
#' parallel = FALSE,
#' semantics = c("r", "raw"),
#' fast_math = NULL,
#' bounds_check = NULL
#' )
#' @param fn Function to specialize and compile.
#' @param ... Optional runtime type hints forwarded to [mojor_jit()].
#' @param name Kernel name prefix.
#' @param elementwise Enable elementwise lowering.
#' @param elementwise_target Elementwise target backend.
#' @param elementwise_size Optional elementwise launch/work size hint.
#' @param elementwise_cpu Optional CPU-only elementwise override.
#' @param broadcast Broadcast/recycling policy.
#' @param disk_cache If `TRUE`, persist JIT signature index on disk. If `NULL`,
#' uses `mojor_options("jit_disk_cache")`.
#' @param cache_dir Optional persistent cache directory.
#' @param signatures Optional declared signatures for eager precompile (named type-list signatures or signature strings).
#' @param eager If `TRUE`, compile all declared `signatures` at wrapper creation.
#' @param strict_signatures If `TRUE`, runtime signatures must match declared `signatures`.
#' If `NULL`, uses `mojor_options("jit_strict_signatures")`.
#' @param parallel Enable parallel loop routing when legal.
#' @param semantics Semantics mode forwarded to [mojor_jit()].
#' @param fast_math Fast-math toggle forwarded to [mojor_jit()].
#' @param bounds_check Bounds-check toggle forwarded to [mojor_jit()].
#' @return A runtime-specializing callable function.
NULL

#' Elementwise Vectorize Convenience Wrapper
#'
#' Convenience wrapper over [mojor_jit()] with `elementwise = TRUE`
#' and strict object mode disabled.
#'
#' @name mojor_vectorize
#' @aliases mojor_vectorize
#' @export mojor_vectorize
#' @usage
#' mojor_vectorize(
#' fn,
#' ...,
#' name = "mojor_vectorize_kernel",
#' target = c("cpu", "gpu"),
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' ),
#' disk_cache = NULL,
#' cache_dir = NULL,
#' signatures = NULL,
#' eager = FALSE,
#' strict_signatures = NULL,
#' parallel = FALSE,
#' elementwise_size = NULL,
#' elementwise_cpu = NULL,
#' semantics = c("r", "raw"),
#' fast_math = NULL,
#' bounds_check = NULL
#' )
#' @param fn Function to specialize and compile.
#' @param ... Optional runtime type hints forwarded to [mojor_jit()].
#' @param name Kernel name prefix.
#' @param target Elementwise target backend (`"cpu"` or `"gpu"`).
#' @param broadcast Broadcast/recycling policy.
#' @param disk_cache If `TRUE`, persist JIT signature index on disk. If `NULL`,
#' uses `mojor_options("jit_disk_cache")`.
#' @param cache_dir Optional persistent cache directory.
#' @param signatures Optional declared signatures for eager precompile (named type-list signatures or signature strings).
#' @param eager If `TRUE`, compile all declared `signatures` at wrapper creation.
#' @param strict_signatures If `TRUE`, runtime signatures must match declared `signatures`.
#' If `NULL`, uses `mojor_options("jit_strict_signatures")`.
#' @param parallel Enable parallel loop routing when legal.
#' @param elementwise_size Optional elementwise launch/work size hint.
#' @param elementwise_cpu Optional CPU-only elementwise override.
#' @param semantics Semantics mode forwarded to [mojor_jit()].
#' @param fast_math Fast-math toggle forwarded to [mojor_jit()].
#' @param bounds_check Bounds-check toggle forwarded to [mojor_jit()].
#' @return A runtime-specializing callable function.
NULL

#' Inspect Runtime JIT Dispatcher Metadata
#'
#' Inspect runtime counters and per-signature events from a JIT wrapper returned by
#' [mojor_jit()], [mojor_njit()], or [mojor_vectorize()].
#'
#' @name mojor_jit_info
#' @aliases mojor_jit_info
#' @export mojor_jit_info
#' @usage
#' mojor_jit_info(jit_fn)
#' @param jit_fn Function returned by [mojor_jit()], [mojor_njit()], or [mojor_vectorize()].
#' @return Named list with `name`, `created_at`, aggregate `stats` (including strict-signature
#' reject counters plus API compile counters), `last_error`, and `signatures`.
NULL

#' Explicitly Compile JIT Signatures
#'
#' Force compilation (or cache resolution) for explicit signatures on an existing JIT wrapper.
#'
#' @name mojor_jit_compile
#' @aliases mojor_jit_compile
#' @export mojor_jit_compile
#' @usage
#' mojor_jit_compile(jit_fn, signatures, stop_on_error = TRUE)
#' @param jit_fn Function returned by [mojor_jit()], [mojor_njit()], or [mojor_vectorize()].
#' @param signatures Signature declarations in named type-list form or signature-string form.
#' @param stop_on_error If `TRUE`, stop on first compile failure; otherwise continue and return all results.
#' @return Named list containing `attempted`, `succeeded`, `failed`, and per-signature `results`.
NULL

#' List JIT Dispatcher Signatures
#'
#' Return declared and/or compiled signatures from a JIT dispatcher in deterministic order.
#'
#' @name mojor_jit_signatures
#' @aliases mojor_jit_signatures
#' @export mojor_jit_signatures
#' @usage
#' mojor_jit_signatures(
#' jit_fn,
#' kind = c("compiled", "declared", "all"),
#' format = c("key", "string", "typed_list")
#' )
#' @param jit_fn Function returned by [mojor_jit()], [mojor_njit()], or [mojor_vectorize()].
#' @param kind Signature set to return (`"compiled"`, `"declared"`, or `"all"`).
#' @param format Output format (`"key"`, `"string"`, or `"typed_list"`).
#' @return Signature set in requested format.
NULL

#' Generalized Vectorize Build Wrapper
#'
#' Build a compiled callable (or build object) from an explicit type signature.
#'
#' @name mojor_guvectorize
#' @aliases mojor_guvectorize
#' @export mojor_guvectorize
#' @usage
#' mojor_guvectorize(
#' fn,
#' signature,
#' name = "mojor_guvectorize_kernel",
#' target = c("cpu", "gpu"),
#' core_dims = NULL,
#' output_shape = NULL,
#' load = TRUE,
#' cache = TRUE,
#' cache_dir = NULL,
#' parallel = FALSE,
#' broadcast = c(
#' "none",
#' "scalar",
#' "recycle",
#' "recycle_warn",
#' "broadcast_nd",
#' "broadcast_nd_warn"
#' )
#' )
#' @param fn Function to compile.
#' @param signature Named signature specification list or signature string.
#' @param name Kernel name prefix.
#' @param target Vectorize target backend (`"cpu"` or `"gpu"`).
#' @param core_dims Optional core-dimension signature string (for example `"(n),(n)->(n)"`).
#'   Tuple rank is unbounded. Signature rank forms accepted by `mojor_guvectorize`
#'   include comma-bracket syntax (`[]`, `[,]`, `[,,]`, ...) and dimensional-tag
#'   syntax (`[1d]`, `[2d]`, `[3d]`, ...). Core-dimension tokens accept
#'   symbolic names (`[A-Za-z_][A-Za-z0-9_]*`) and fixed positive integer
#'   literals (`1`, `2`, ...).
#' @param output_shape Optional output metadata. For single-output wrappers provide
#'   an integer shape vector; for multi-output wrappers provide a list of integer
#'   shape vectors (one per output tuple).
#' @param load If `TRUE`, return callable function; otherwise return build result list.
#' @param cache If `TRUE`, reuse build cache entries where possible.
#' @param cache_dir Optional cache directory.
#' @param parallel Enable parallel loop routing when legal.
#' @param broadcast Broadcast/recycling policy.
#' @return
#' If `load = TRUE`, returns a callable function; otherwise returns the
#' complete build result list.
NULL

#' Parallel Range Alias for Transpiled Loops
#'
#' Alias for parallel sequence routing in transpiled loop syntax.
#'
#' @name mojor_prange
#' @aliases mojor_prange
#' @export mojor_prange
#' @usage
#' mojor_prange(n)
#' @param n Loop bound (non-negative scalar integer).
#' @return A sequence equivalent to `seq_len(n)` in interpreted mode.
NULL

#' Load MojoR Bridge Library
#'
#' Load and bind the MojoR bridge shared library used by compiled kernels and bridge calls.
#'
#' @name mojor_load
#' @aliases mojor_load
#' @export mojor_load
#' @usage
#' mojor_load(path = NULL)
#' @param path Directory containing `mojor_bridge.so`. If `NULL`, MojoR probes default locations.
#' @return `TRUE` invisibly when a compatible bridge is loaded and bound.
#' @examples
#' \dontrun{
#' mojor_load()
#' }
NULL

#' Query GPU Availability
#'
#' Report whether the loaded MojoR bridge can access a supported GPU runtime/device.
#'
#' @name mojor_has_gpu
#' @aliases mojor_has_gpu
#' @export mojor_has_gpu
#' @usage
#' mojor_has_gpu()
#' @return `TRUE` if GPU support is available through the current bridge runtime, otherwise `FALSE`.
#' @examples
#' \dontrun{
#' mojor_has_gpu()
#' }
NULL

#' Get or Set MojoR Runtime/Compile Options
#'
#' Read or update package-level MojoR options that control transpilation, scheduling,
#' and runtime behavior.
#'
#' @name mojor_options
#' @aliases mojor_options
#' @export mojor_options
#' @usage
#' mojor_options(...)
#' @param ... Option query/set arguments passed through to the option helper.
#' @return
#' If called with no arguments, returns the full options list. If queried by name,
#' returns a sub-list of values. If setting named options, returns previous values invisibly.
#' @examples
#' mojor_options()
#' mojor_options(c("na_mode", "simd_mode"))
#' old <- mojor_options(na_mode = "forbid", simd_mode = "explicit")
#' mojor_options(r_jit_level = 0L)
#' mojor_options(jit_profile = "bench")
#' mojor_options(jit_strict_signatures = TRUE)
NULL

#' Run Independent Chains in Parallel Workers
#'
#' Execute independent chains with deterministic per-chain seeding and optional fork-based
#' parallelism.
#'
#' @name mojor_run_chains_parallel
#' @aliases mojor_run_chains_parallel
#' @export mojor_run_chains_parallel
#' @usage
#' mojor_run_chains_parallel(
#' chain_fun,
#' n_chains,
#' chain_args = list(),
#' seed = 1L,
#' workers = getOption("mc.cores", 2L),
#' backend = c("auto", "fork", "sequential"),
#' use_mojor_rng = TRUE
#' )
#' @param chain_fun Function that runs one chain.
#' @param n_chains Positive integer number of chains.
#' @param chain_args Named list of arguments passed to `chain_fun`.
#' @param seed Base integer seed. Chain `i` uses `seed + i - 1`.
#' @param workers Positive integer worker count hint.
#' @param backend One of `"auto"`, `"fork"`, or `"sequential"`.
#' @param use_mojor_rng If `TRUE`, seed with `mojor_rng_seed()`; otherwise use `set.seed()`.
#' @return List of per-chain outputs, ordered by chain id.
#' @examples
#' chain_fun <- function(n) {
#' x <- numeric(n)
#' for (i in seq_len(n)) x[i] <- i
#' sum(x)
#' }
#'
#' mojor_run_chains_parallel(
#' chain_fun = chain_fun,
#' n_chains = 2,
#' chain_args = list(n = 5),
#' backend = "sequential",
#' use_mojor_rng = FALSE
#' )
NULL
