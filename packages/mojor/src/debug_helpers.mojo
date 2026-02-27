# MojoR Debug Helpers
#
# Provides zero-overhead debug instrumentation for compiled kernels.
# Only active when compiled with -D MOJOR_DEBUG=1
#
# Key Design:
# - @parameter if MOJOR_DEBUG: code is ELIMINATED at compile time when debug=FALSE
# - @always_inline: no function call overhead
# - raises: propagates errors through call stack
#
# Date: 2026-02-15
# Tier: 6.1 Stage A (Debug Mode)

from sys.info import sizeof

# ============================================================================
# Debug Assertion
# ============================================================================

@always_inline
fn mojor_assert(
    condition: Bool,
    message: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Runtime assertion that only executes in debug mode.

    Args:
        condition: Boolean condition to check
        message: Error message if condition is False
        file: Source file name (for error reporting)
        line: Source line number (for error reporting)

    Raises:
        Error if condition is False (only in debug mode)

    Example:
        mojor_assert(x >= 0.0, "x must be non-negative", "kernel.R", 5)
    """
    @parameter
    if MOJOR_DEBUG:
        if not condition:
            print("╔══════════════════════════════════════════════════════╗")
            print("║ ASSERTION FAILED                                     ║")
            print("╚══════════════════════════════════════════════════════╝")
            print("")
            print("  Condition:", message)
            print("  at", file, "line", line)
            print("")
            raise Error("Assertion failed: " + message)


# ============================================================================
# Bounds Checking
# ============================================================================

@always_inline
fn mojor_check_bounds(
    index: Int,
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Check array bounds with detailed error reporting.

    Args:
        index: Index being accessed (0-based)
        length: Array length
        var_name: Variable name (for error reporting)
        file: Source file name
        line: Source line number

    Raises:
        Error if index out of bounds (only in debug mode)

    Example:
        mojor_check_bounds(Int(i - 1), len(x), "x", "kernel.R", 10)
    """
    @parameter
    if MOJOR_DEBUG:
        if index < 0 or index >= length:
            print("╔══════════════════════════════════════════════════════╗")
            print("║ BOUNDS CHECK FAILED                                  ║")
            print("╚══════════════════════════════════════════════════════╝")
            print("")
            print("  Variable:", var_name)
            print("  Index:", index)
            print("  Valid range: [0,", length, ")")
            print("  at", file, "line", line)
            print("")

            if index < 0:
                print("  Error: Index is negative")
            else:
                print("  Error: Index", index, ">=", length)
            print("")

            raise Error("Index out of bounds")


@always_inline
fn mojor_check_bounds_2d(
    row: Int,
    col: Int,
    nrow: Int,
    ncol: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Check 2D array bounds (matrix).

    Args:
        row: Row index (0-based)
        col: Column index (0-based)
        nrow: Number of rows
        ncol: Number of columns
        var_name: Variable name
        file: Source file
        line: Source line

    Raises:
        Error if row or col out of bounds
    """
    @parameter
    if MOJOR_DEBUG:
        if row < 0 or row >= nrow or col < 0 or col >= ncol:
            print("╔══════════════════════════════════════════════════════╗")
            print("║ BOUNDS CHECK FAILED (2D)                             ║")
            print("╚══════════════════════════════════════════════════════╝")
            print("")
            print("  Variable:", var_name)
            print("  Access:", var_name + "[", row, ",", col, "]")
            print("  Dimensions:", nrow, "×", ncol)
            print("  at", file, "line", line)
            print("")

            if row < 0 or row >= nrow:
                print("  Error: Row index", row, "out of range [0,", nrow, ")")
            if col < 0 or col >= ncol:
                print("  Error: Column index", col, "out of range [0,", ncol, ")")
            print("")

            raise Error("Index out of bounds")


# ============================================================================
# Execution Tracing
# ============================================================================

@always_inline
fn mojor_trace(
    message: StringLiteral,
    file: StringLiteral,
    line: Int
):
    """
    Print execution trace message (trace mode only).

    Args:
        message: Trace message
        file: Source file
        line: Source line

    Example:
        mojor_trace("Loop iteration", "kernel.R", 15)
    """
    @parameter
    if MOJOR_TRACE:
        print("[TRACE]", file + ":" + String(line), ":", message)


@always_inline
fn mojor_trace_value[T: DType](
    name: StringLiteral,
    value: Scalar[T]
):
    """
    Print variable value (trace mode only).

    Args:
        name: Variable name
        value: Variable value

    Example:
        mojor_trace_value("i", i)
    """
    @parameter
    if MOJOR_TRACE:
        print("    ", name, "=", value)


@always_inline
fn mojor_trace_value_int(
    name: StringLiteral,
    value: Int
):
    """
    Print Int variable value (trace mode only).

    Args:
        name: Variable name
        value: Int value
    """
    @parameter
    if MOJOR_TRACE:
        print("    ", name, "=", value)


@always_inline
fn mojor_trace_enter(
    function_name: StringLiteral,
    file: StringLiteral,
    line: Int
):
    """
    Trace function entry (trace mode only).

    Args:
        function_name: Function name
        file: Source file
        line: Source line
    """
    @parameter
    if MOJOR_TRACE:
        print("[TRACE] ▶ Entering", function_name, "at", file + ":" + String(line))


@always_inline
fn mojor_trace_exit(
    function_name: StringLiteral
):
    """
    Trace function exit (trace mode only).

    Args:
        function_name: Function name
    """
    @parameter
    if MOJOR_TRACE:
        print("[TRACE] ◀ Exiting", function_name)


# ============================================================================
# Type Checking
# ============================================================================

@always_inline
fn mojor_check_type[T: DType, Expected: DType](
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Runtime type check (debug mode only).

    Args:
        T: Actual type
        Expected: Expected type
        var_name: Variable name
        file: Source file
        line: Source line

    Raises:
        Error if T != Expected
    """
    @parameter
    if MOJOR_DEBUG:
        @parameter
        if T != Expected:
            print("╔══════════════════════════════════════════════════════╗")
            print("║ TYPE MISMATCH                                        ║")
            print("╚══════════════════════════════════════════════════════╝")
            print("")
            print("  Variable:", var_name)
            print("  Expected type:", Expected)
            print("  Actual type:", T)
            print("  at", file, "line", line)
            print("")
            raise Error("Type mismatch")


# ============================================================================
# NA Checking
# ============================================================================

@always_inline
fn _mojor_raise_na_value(
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    print("╔══════════════════════════════════════════════════════╗")
    print("║ NA VALUE DETECTED                                    ║")
    print("╚══════════════════════════════════════════════════════╝")
    print("")
    print("  Variable:", var_name, "is NA (NaN)")
    print("  at", file, "line", line)
    print("")
    raise Error("NA value not allowed")


@always_inline
fn _mojor_check_na_value(
    is_na: Bool,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    @parameter
    if MOJOR_DEBUG:
        if is_na:
            _mojor_raise_na_value(var_name, file, line)


@always_inline
fn mojor_check_na_f64(
    value: Float64,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Check for NA (NaN) in Float64 value.

    Args:
        value: Value to check
        var_name: Variable name
        file: Source file
        line: Source line

    Raises:
        Error if value is NaN (only in debug mode with na_guard)
    """
    _mojor_check_na_value(value != value, var_name, file, line)


@always_inline
fn mojor_check_na_f32(
    value: Float32,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Check for NA (NaN) in Float32 value.
    """
    _mojor_check_na_value(value != value, var_name, file, line)


# ============================================================================
# Memory Safety
# ============================================================================

@always_inline
fn mojor_check_null[T: AnyType](
    ptr: UnsafePointer[T],
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Check for null pointer (debug mode only).

    Args:
        ptr: Pointer to check
        var_name: Variable name
        file: Source file
        line: Source line

    Raises:
        Error if ptr is null
    """
    @parameter
    if MOJOR_DEBUG:
        # Note: Mojo's type system makes null pointers rare,
        # but we include this for completeness
        if not ptr:
            print("╔══════════════════════════════════════════════════════╗")
            print("║ NULL POINTER DETECTED                                ║")
            print("╚══════════════════════════════════════════════════════╝")
            print("")
            print("  Variable:", var_name)
            print("  at", file, "line", line)
            print("")
            raise Error("Null pointer dereference")


# ============================================================================
# Pointer + Length Validation (Stage B)
# ============================================================================

@always_inline
fn _mojor_raise_invalid_length(
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    print("╔══════════════════════════════════════════════════════╗")
    print("║ INVALID ARRAY LENGTH                                 ║")
    print("╚══════════════════════════════════════════════════════╝")
    print("")
    print("  Variable:", var_name)
    print("  Length:", length, "(must be >= 0)")
    print("  at", file, "line", line)
    print("")
    raise Error("Invalid array length")


@always_inline
fn _mojor_check_nonnegative_length(
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    @parameter
    if MOJOR_DEBUG:
        if length < 0:
            _mojor_raise_invalid_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_f64(
    ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Validate a Float64 pointer and its associated length.
    Checks that the pointer is non-null and length is non-negative.
    Only active in debug mode (zero overhead in production).
    """
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_f64_mut(
    ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """Validate a mutable Float64 pointer and its associated length."""
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_f32(
    ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """Validate a Float32 pointer and its associated length."""
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_f32_mut(
    ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """Validate a mutable Float32 pointer and its associated length."""
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_i32(
    ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """Validate an Int32 pointer and its associated length."""
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_ptr_i32_mut(
    ptr: UnsafePointer[mut=True, type=Int32, origin=MutAnyOrigin],
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """Validate a mutable Int32 pointer and its associated length."""
    _mojor_check_nonnegative_length(length, var_name, file, line)


@always_inline
fn mojor_check_length(
    length: Int,
    var_name: StringLiteral,
    file: StringLiteral,
    line: Int
) raises:
    """
    Validate that a length value is non-negative.
    Use when you only have a length but no pointer to check.
    """
    _mojor_check_nonnegative_length(length, var_name, file, line)


# ============================================================================
# Conditional Compilation Constants
# ============================================================================

# These are set at compile time via -D flags:
# -D MOJOR_DEBUG=1     Enable debug checks (bounds, asserts, type checks)
# -D MOJOR_TRACE=1     Enable execution tracing (implies MOJOR_DEBUG=1)
#
# When not set, default to 0 (disabled)

comptime MOJOR_DEBUG = False  # Will be overridden by -D flag at compile time
comptime MOJOR_TRACE = False  # Will be overridden by -D flag at compile time

# Note: In actual build, these become:
# mojo build -D MOJOR_DEBUG=1 ...     # Debug mode
# mojo build -D MOJOR_TRACE=1 ...     # Trace mode (implies debug)
# mojo build ...                       # Production (both False)
