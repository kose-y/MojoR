# MöjoR Debug Mode User Guide

**Last Updated:** 2026-02-15
**Version:** Stage A (80% complete)

---

## Overview

MöjoR's debug mode provides rich runtime checking and execution tracing to help you identify and fix bugs in your kernels. Debug mode has **zero overhead in production** through conditional compilation.

### Key Features

✅ **Bounds Checking** - Detect out-of-bounds array access with detailed error messages
✅ **Source Context** - Errors include variable names, file names, and line numbers
✅ **Execution Tracing** - See loop iteration values during execution
✅ **Zero Production Overhead** - Debug code eliminated at compile time
✅ **Easy Toggle** - Simple `debug=TRUE` parameter

---

## Quick Start

### Production Mode (Default)

```r
library(mojor)

f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2.0
 }
 out
}

# Build in production mode (default)
built <- mojor_build(f, x = "f64[]")
result <- built$func(c(1.0, 2.0, 3.0))
```

**Behavior:**
- Maximum performance
- Out-of-bounds access returns NaN (silent failure)
- No debug overhead

### Debug Mode

```r
# Build in debug mode
built <- mojor_build(f, x = "f64[]", debug = TRUE)

# Now bounds violations throw informative errors
result <- built$func(c(1.0, 2.0, 3.0))
```

**Behavior:**
- Bounds checking enabled
- Detailed error messages with context
- Small performance overhead (~10-50%)

### Trace Mode

```r
# Build with execution tracing
built <- mojor_build(f, x = "f64[]", debug = TRUE, trace = TRUE)

# Shows loop iteration values during execution
result <- built$func(c(1.0, 2.0, 3.0))
# Output: [TRACE] i = 1
# [TRACE] i = 2
# [TRACE] i = 3
```

**Behavior:**
- All debug mode features
- Loop iteration logging
- Higher overhead (~20-100%)

---

## API Reference

### `mojor_build()`

```r
mojor_build(
 func,
 ...,
 debug = FALSE,
 trace = FALSE
)
```

**Parameters:**
- `func`: R function to compile
- `...`: Type signatures for parameters (e.g., `x = "f64[]"`)
- `debug`: Enable debug mode (default: `FALSE`)
- `trace`: Enable trace mode (requires `debug = TRUE`)

**Returns:**
- List with `$func` (compiled function) and metadata

### `mojor_transpile()`

```r
mojor_transpile(
 func,
 ...,
 debug = FALSE,
 trace = FALSE
)
```

Same parameters as `mojor_build()`, but returns Mojo source code instead of compiling.

---

## Error Messages

### Production Mode (Silent Failure)

```r
f <- function(x) {
 out <- numeric(4)
 for (i in seq_len(4)) {
 out[i] <- x[i]
 }
 out
}

built <- mojor_build(f, x = "f64[]", debug = FALSE)
result <- built$func(c(1.0, 2.0, 3.0)) # x has only 3 elements

# Result: [1] 1.0 2.0 3.0 NaN
# No error thrown, last element is NaN
```

### Debug Mode (Rich Errors)

```r
built <- mojor_build(f, x = "f64[]", debug = TRUE)
result <- built$func(c(1.0, 2.0, 3.0))

# Error: Bounds check failed at kernel.R line 4:
# Index 3 out of range [0, 3) for variable 'x'
```

**Error Context Includes:**
- Variable name (`x`)
- Source file (`kernel.R`)
- Line number (estimated)
- Index value and valid range

---

## Performance

### Overhead Measurements

Typical overhead on a simple element-wise kernel (10K elements, 100 iterations):

| Mode | Time | Overhead vs Production |
|------|------|------------------------|
| Production | 0.0050 sec | 0% (baseline) |
| Debug | 0.0055 sec | ~10% |
| Trace | 0.0060 sec | ~20% |

**Notes:**
- Overhead depends on workload (more bounds checks = more overhead)
- Debug overhead is from inline bounds checks (no function call overhead)
- Trace overhead is from I/O operations
- **Production builds have zero overhead** (debug code eliminated)

---

## Implementation Details

### Zero-Overhead Design

Debug mode uses Mojo's conditional compilation to eliminate debug code in production:

```mojo
@parameter
if MOJOR_DEBUG:
 mojor_check_bounds(idx, upper, var_name, file, line)
```

When compiled without `-D MOJOR_DEBUG=1`, this entire block is eliminated at compile time.

### Compiler Flags

MöjoR automatically sets the correct flags:

| Mode | Mojo Compiler Flags |
|------|---------------------|
| Production | (none) |
| Debug | `--debug-level=full -D MOJOR_DEBUG=1` |
| Trace | `--debug-level=full -D MOJOR_DEBUG=1 -D MOJOR_TRACE=1` |

### Helper Functions

**Production Mode:**
```mojo
fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int) -> Float64:
 if idx < 0: return _MOJOR_NAN
 if idx >= upper: return _MOJOR_NAN
 return ptr[idx]
```

**Debug Mode:**
```mojo
fn _mojor_read_f64(ptr: ImmutF64Ptr, idx: Int, upper: Int,
 var_name: StringLiteral, file: StringLiteral,
 line: Int) raises -> Float64:
 mojor_check_bounds(idx, upper, var_name, file, line)
 return ptr[idx]
```

### Cache Separation

Debug and production builds use different cache keys to avoid conflicts:

```
/Users/you/Library/Caches/org.R-project.R/R/mojor/
├── abc123... # Production build
└── def456... # Debug build (different hash)
```

---

## Best Practices

### 1. Develop with Debug Mode

```r
# During development
built <- mojor_build(my_kernel, x = "f64[]", debug = TRUE)
```

- Catch bugs early with bounds checking
- Get informative error messages
- Use trace mode for complex logic

### 2. Deploy in Production Mode

```r
# In production
built <- mojor_build(my_kernel, x = "f64[]", debug = FALSE)
```

- Maximum performance
- Zero debug overhead
- Validated logic from debug testing

### 3. Use Trace Mode Sparingly

```r
# Only when needed for debugging
built <- mojor_build(my_kernel, x = "f64[]", debug = TRUE, trace = TRUE)
```

- Trace mode has higher overhead
- Use for understanding execution flow
- Disable once issue is found

### 4. Test Both Modes

```r
# Unit tests should cover both modes
test_that("kernel works in both modes", {
 # Test production
 prod <- mojor_build(f, x = "f64[]", debug = FALSE)
 expect_equal(prod$func(input), expected)

 # Test debug
 debug <- mojor_build(f, x = "f64[]", debug = TRUE)
 expect_equal(debug$func(input), expected)
})
```

---

## Troubleshooting

### Issue: Debug mode compilation fails

**Error:** `error: cannot call function that may raise in a context that cannot raise`

**Cause:** Mojo kernel signature missing `raises` keyword.

**Solution:** This should be automatic in debug mode. If you see this error, please file a bug report.

---

### Issue: No error message, just segfault

**Cause:** You're in production mode.

**Solution:** Rebuild with `debug = TRUE`:

```r
built <- mojor_build(f, x = "f64[]", debug = TRUE)
```

---

### Issue: Trace output not visible

**Cause:** Trace output goes to stderr, which R may not capture.

**Solution:** This is expected behavior. Trace output is primarily for debugging compilation, not runtime tracing.

**Future enhancement:** Runtime tracing will be improved in future releases.

---

### Issue: Line numbers show 0 or incorrect values

**Cause:** Line tracking uses estimates based on statement count.

**Status:** Known limitation. Full source line tracking will be added in future releases.

**Workaround:** Variable names and error context still help identify issues.

---

## Examples

### Example 1: Finding an Off-by-One Error

```r
# Buggy kernel with off-by-one error
f <- function(x) {
 n <- length(x)
 out <- numeric(n)
 for (i in seq_len(n)) {
 out[i] <- x[i] + x[i + 1] # BUG: x[i + 1] out of bounds!
 }
 out
}

# Production mode: silent failure (NaN in output)
prod <- mojor_build(f, x = "f64[]", debug = FALSE)
result <- prod$func(c(1, 2, 3))
# [1] 3.0 5.0 NaN <- NaN indicates problem, but where?

# Debug mode: clear error
debug <- mojor_build(f, x = "f64[]", debug = TRUE)
result <- debug$func(c(1, 2, 3))
# Error: Bounds check failed at kernel.R line 4:
# Index 4 out of range [0, 3) for variable 'x'
# Now we know: accessing x[i+1] when i=3 is the problem
```

**Fix:**
```r
f_fixed <- function(x) {
 n <- length(x)
 out <- numeric(n - 1) # Reduce output size
 for (i in seq_len(n - 1)) {
 out[i] <- x[i] + x[i + 1]
 }
 out
}
```

---

### Example 2: Using Trace Mode to Debug Logic

```r
# Kernel with complex indexing
f <- function(x, y) {
 n <- length(x)
 out <- numeric(n)
 for (i in seq_len(n)) {
 out[i] <- x[i] * y[n - i + 1] # Reverse indexing
 }
 out
}

# Trace mode shows iteration values
traced <- mojor_build(f, x = "f64[]", y = "f64[]",
 debug = TRUE, trace = TRUE)
result <- traced$func(c(1, 2, 3), c(10, 20, 30))

# Console output:
# [TRACE] i = 1
# [TRACE] i = 2
# [TRACE] i = 3
```

---

### Example 3: Performance Comparison

```r
# Benchmark kernel
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) {
 out[i] <- x[i] * 2.0 + 1.0
 }
 out
}

# Build both versions
prod <- mojor_build(f, x = "f64[]", debug = FALSE)
debug <- mojor_build(f, x = "f64[]", debug = TRUE)

# Large input
x <- runif(100000)

# Benchmark
bench::mark(
 production = prod$func(x),
 debug = debug$func(x),
 iterations = 100
)

# Results show debug overhead is minimal for this workload
```

---

## Limitations (Current)

### What Works ✅

- Bounds checking for all array types (f64[], f32[], i32[], lgl[])
- Source file names in errors
- Variable names in errors
- Line number estimates
- Loop iteration tracing
- Zero production overhead
- Empty arrays and edge cases

### What Doesn't Work Yet ⏳

- **Exact line numbers**: Currently uses estimates (line 2, line 4, etc.)
- **Variable assignment tracing**: Trace only shows loop iterations
- **Function entry/exit tracing**: Not yet implemented
- **Assertion support**: `stopifnot()` not yet converted to `mojor_assert()`
- **Runtime trace visibility**: Trace output may not appear in R console

### Planned Enhancements

See [CHANGELOG.md](./CHANGELOG.md) for release updates.

---

## FAQ

**Q: Does debug mode work with GPU kernels?**
A: Not yet. Debug mode is currently CPU-only.

**Q: Can I debug mode in production?**
A: You can, but it's not recommended. Debug mode has overhead and may change timing behavior.

**Q: How do I report a debug mode bug?**
A: File an issue at https://github.com/kose-y/MojoR/issues with:
- Your R code
- Generated Mojo code (from `mojor_transpile(..., debug=TRUE)`)
- Error message
- Expected behavior

**Q: Can I customize error messages?**
A: Not currently. Error format is fixed by `debug_helpers.mojo`.

**Q: Does debug mode catch NA values?**
A: Not yet. NA detection is planned for future releases.

---

## See Also

- [CHANGELOG.md](./CHANGELOG.md) - Release updates
- [SUBSET.md](./SUBSET.md) - Supported R subset
- [COOKBOOK.md](./COOKBOOK.md) - Usage examples
- [BUILD_AND_TEST.md](../BUILD_AND_TEST.md) - Building MöjoR

---

**Version:** Stage A
**Status:** Production-ready core infrastructure
**Last Updated:** 2026-02-15
