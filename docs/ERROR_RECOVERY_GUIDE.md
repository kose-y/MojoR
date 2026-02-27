# Error Recovery ( Stage C)

**Status:** ✅ Complete **Date:** 2026-02-15 **Priority:** P1

---

## Overview

 Stage C implements graceful error handling with retry mechanisms and partial result support. This allows MöjoR kernels to handle transient failures gracefully and return partial results when possible.

---

## Features

### 1. Error Recovery Modes

Three error handling modes are available:

| Mode | Behavior |
|------|----------|
| `stop` | Default behavior - errors propagate immediately |
| `partial` | Errors are caught and a partial result is returned |
| `retry` | Errors trigger automatic retries with exponential backoff |

### 2. Result Wrapper

All error recovery operations return a `mojor_error_result` object with:

- `status`: "ok", "partial", or "failed"
- `error`: The error object (if any)
- `data`: The computed data (full or partial)
- `retries_used`: Number of retry attempts made

### 3. Retry Logic

- Automatic retry for transient failures
- Configurable `max_retries` parameter
- Exponential backoff with configurable base delay
- Optional error pattern matching for selective retries

---

## API Reference

### `mojor_error_result()`

Create an error recovery result object.

```r
mojor_error_result(status = "ok", error = NULL, data = NULL, retries_used = 0)
```

**Arguments:**
- `status`: Character - "ok", "partial", or "failed"
- `error`: Optional error object or message
- `data`: The computed data (full or partial)
- `retries_used`: Number of retries used (default: 0)

**Returns:** A list with class "mojor_error_result"

### `result_status()`

Extract status from a result object.

```r
result_status(result)
```

### `result_error()`

Extract error from a result object.

```r
result_error(result)
```

### `result_data()`

Extract data from a result object.

```r
result_data(result)
```

### `result_is_success()`

Check if result indicates success.

```r
result_is_success(result)
```

### `result_is_partial()`

Check if result indicates partial success.

```r
result_is_partial(result)
```

### `result_is_failed()`

Check if result indicates failure.

```r
result_is_failed(result)
```

### `with_cleanup()`

RAII-style cleanup for resource management.

```r
with_cleanup(cleanup_fn, code)
```

**Arguments:**
- `cleanup_fn`: Function to call for cleanup
- `code`: Code to execute with cleanup

---

## Usage Examples

### Basic Error Recovery

```r
f <- function(x) {
 out <- numeric(length(x))
 for (i in seq_along(x)) out[i] <- x[i] * 2
 out
}

# Enable error recovery with retry mode
built <- mojor_build(f, x = "f64[]", error_mode = "retry", max_retries = 3)

# Execute with error recovery
result <- built$func(1:5)

# Check result
result_status(result) # "ok"
result_data(result) # c(2, 4, 6, 8, 10)
```

### Partial Results

```r
# Enable partial result mode
built <- mojor_build(f, x = "f64[]", error_mode = "partial")

# Execute - returns result object even on error
result <- built$func(1:5)

if (result_is_success(result)) {
 print(result_data(result))
} else if (result_is_partial(result)) {
 print("Partial result available:")
 print(result_data(result))
 print("Error:", result_error(result))
} else {
 print("Full failure:", result_error(result))
}
```

### Retry with Exponential Backoff

```r
# Configure retry behavior
built <- mojor_build(
 f, x = "f64[]",
 error_mode = "retry",
 max_retries = 3,
 retry_delay = 0.1 # Base delay in seconds
)

result <- built$func(1:5)
print(paste("Retries used:", result$retries_used))
```

### Cleanup on Error

```r
# Use with_cleanup for resource management
result <- with_cleanup(
 function() {
 # Cleanup code (e.g., close file handles, release resources)
 message("Cleanup called")
 },
 {
 # Your code here
 42
 }
)
```

---

## Integration with Existing Features

Error recovery works seamlessly with all existing MöjoR features:

```r
# With debug mode
built <- mojor_build(f, x = "f64[]", debug = TRUE, error_mode = "partial")

# With trace mode
built <- mojor_build(f, x = "f64[]", trace = TRUE, error_mode = "retry")

# With memory checking
built <- mojor_build(f, x = "f64[]", memory_check = TRUE, error_mode = "partial")

# With tiling
built <- mojor_build(f, x = "f64[]", tile = 32, error_mode = "retry")
```

---

## Error Recovery State

Error recovery state is managed internally in `.mojor_state$error_recovery`:

```r
# Access current error recovery configuration
state <- .mojor_state$error_recovery
print(state$error_mode) # "stop", "partial", or "retry"
print(state$max_retries) # Maximum retry attempts
print(state$retry_delay) # Base delay for exponential backoff
```

---

## Performance Considerations

- **Stop mode**: No overhead (default, recommended for production)
- **Partial mode**: Minimal overhead (error handling only)
- **Retry mode**: Additional overhead from retry logic and exponential backoff

For production use, consider using `error_mode = "stop"` (default) and handle errors at a higher level if needed.

---

## Migration Guide

### From Legacy Error Handling

**Before (no error recovery):**
```r
built <- mojor_build(f, x = "f64[]")
result <- built$func(1:5) # Errors propagate immediately
```

**After (with error recovery):**
```r
built <- mojor_build(f, x = "f64[]", error_mode = "partial")
result <- built$func(1:5) # Returns result object

if (result_is_success(result)) {
 print(result_data(result))
} else {
 print("Error:", result_error(result))
}
```

---

## Implementation Details

### Error Recovery Pipeline

1. **Function Execution**: The kernel function is executed
2. **Error Detection**: If an error occurs, it's caught by `tryCatch`
3. **Retry Decision**: Based on `error_mode` and retry configuration
4. **Exponential Backoff**: Delay before retry (if applicable)
5. **Result Return**: Success, partial, or failed result

### Exponential Backoff Formula

```
delay = retry_delay * (2 ^ retries_used)
```

Where:
- `retry_delay`: Base delay (default: 0.1 seconds)
- `retries_used`: Number of retries already performed

### Result Object Structure

```r
list(
 status = "ok" | "partial" | "failed",
 error = NULL | <error_object>,
 data = NULL | <computed_data>,
 retries_used = <integer>
)
```

---

## Testing

Run the error recovery tests:

```bash
Rscript -e 'testthat::test_file("packages/mojor/tests/testthat/test_error_recovery.R")'
```

---

## Future Enhancements

Potential future improvements:

- Configurable retry policies (e.g., linear backoff, jitter)
- Error type filtering (retry only on specific error types)
- Circuit breaker pattern for persistent failures
- Metrics and monitoring for retry behavior
- Partial result aggregation for distributed execution

---

## See Also

- [ Stage A: Debug Mode](./DEBUG_MODE_GUIDE.md)
- [ Changelog](./CHANGELOG.md)
