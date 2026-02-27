# MojoR Quantiles & Robust Stats Helpers
#
# Provides quantile, median, IQR, and MAD computations.
# Used for Tier 7.4 Quantiles & Robust Stats.
#
# Key Design:
# - Uses in-place quicksort to sort temporary buffers for quantile computation
# - Supports NA handling via na_rm mode
# - Uses linear interpolation for quantile types 1-8
#
# Date: 2026-02-15
# Tier: 7.4 Quantiles & Robust Stats

from math import floor, ceil
from memory import alloc
from abi_types import ImmutF64Ptr, ImmutF32Ptr, ImmutI32Ptr, MutF64Ptr, MutF32Ptr, MutI32Ptr

# ============================================================================
# Helper: Swap two elements in an array
# ============================================================================

@always_inline
fn _mojor_swap_f64(arr: MutF64Ptr, i: Int, j: Int) -> None:
    """Swap two elements in a Float64 array."""
    tmp = arr[i]
    arr[i] = arr[j]
    arr[j] = tmp


@always_inline
fn _mojor_swap_i32(arr: MutI32Ptr, i: Int, j: Int) -> None:
    """Swap two elements in an Int32 array."""
    tmp = arr[i]
    arr[i] = arr[j]
    arr[j] = tmp


# ============================================================================
# Helper: In-place quicksort (Float64)
# ============================================================================

@always_inline
fn _mojor_sort_f64(arr: MutF64Ptr, n: Int) -> None:
    """Sort a Float64 array in place (ascending)."""
    if n <= 1:
        return

    # Iterative quicksort using an explicit stack to avoid recursion.
    var stack_left = alloc[Int](n)
    var stack_right = alloc[Int](n)
    var top: Int = 0
    stack_left[0] = 0
    stack_right[0] = n - 1

    while top >= 0:
        var left: Int = stack_left[top]
        var right: Int = stack_right[top]
        top -= 1

        while left < right:
            pivot = arr[(left + right) // 2]
            var i: Int = left
            var j: Int = right

            while i <= j:
                while arr[i] < pivot:
                    i += 1
                while arr[j] > pivot:
                    j -= 1
                if i <= j:
                    _mojor_swap_f64(arr, i, j)
                    i += 1
                    j -= 1

            # Sort the smaller partition immediately; push the larger onto the stack.
            if (j - left) < (right - i):
                if i < right:
                    top += 1
                    stack_left[top] = i
                    stack_right[top] = right
                right = j
            else:
                if left < j:
                    top += 1
                    stack_left[top] = left
                    stack_right[top] = j
                left = i
    stack_left.free()
    stack_right.free()


# ============================================================================
# Helper: Quickselect for k-th smallest element (0-indexed)
# ============================================================================

@always_inline
fn _mojor_quickselect_f64(
    arr: MutF64Ptr,
    left: Int,
    right: Int,
    k: Int
) -> Float64:
    """
    Quickselect algorithm to find the k-th smallest element (0-indexed).
    
    This is a partial sort - it only ensures the k-th element is in its
    correct position, with smaller elements to the left and larger to the right.
    
    Args:
        arr: Mutable array of Float64 values
        left: Left boundary (inclusive)
        right: Right boundary (inclusive)
        k: Index of desired element (0-indexed)
    
    Returns:
        The k-th smallest element
    """
    var left_i = left
    var right_i = right
    while True:
        if left_i == right_i:
            return arr[left_i]
        
        # Choose pivot (middle element)
        pivot_idx = (left_i + right_i) // 2
        pivot_val = arr[pivot_idx]
        
        # Partition
        i = left_i
        j = right_i
        while i <= j:
            while arr[i] < pivot_val:
                i += 1
            while arr[j] > pivot_val:
                j -= 1
            if i <= j:
                _mojor_swap_f64(arr, i, j)
                i += 1
                j -= 1
        
        # Recurse on appropriate side
        if k <= j:
            right_i = j
        elif k >= i:
            left_i = i
        else:
            return arr[k]


@always_inline
fn _mojor_quickselect_i32(
    arr: MutI32Ptr,
    left: Int,
    right: Int,
    k: Int
) -> Int32:
    """
    Quickselect algorithm to find the k-th smallest element (0-indexed).
    
    Args:
        arr: Mutable array of Int32 values
        left: Left boundary (inclusive)
        right: Right boundary (inclusive)
        k: Index of desired element (0-indexed)
    
    Returns:
        The k-th smallest element
    """
    var left_i = left
    var right_i = right
    while True:
        if left_i == right_i:
            return arr[left_i]
        
        # Choose pivot (middle element)
        pivot_idx = (left_i + right_i) // 2
        pivot_val = arr[pivot_idx]
        
        # Partition
        i = left_i
        j = right_i
        while i <= j:
            while arr[i] < pivot_val:
                i += 1
            while arr[j] > pivot_val:
                j -= 1
            if i <= j:
                _mojor_swap_i32(arr, i, j)
                i += 1
                j -= 1
        
        # Recurse on appropriate side
        if k <= j:
            right_i = j
        elif k >= i:
            left_i = i
        else:
            return arr[k]


# ============================================================================
# Helper: Check if value is NA
# ============================================================================

@always_inline
fn _mojor_is_na_f64(val: Float64) -> Bool:
    """Check if a Float64 value is NA (NaN)."""
    return val != val


@always_inline
fn _mojor_is_na_f32(val: Float32) -> Bool:
    """Check if a Float32 value is NA (NaN)."""
    return val != val


@always_inline
fn _mojor_is_na_i32(val: Int32) -> Bool:
    """Check if an Int32 value is NA (INT_MIN)."""
    return val == Int32(-2147483648)


# ============================================================================
# Helper: Filter NA values and return count of valid values
# ============================================================================

@always_inline
fn _mojor_filter_na_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    out_ptr: MutF64Ptr
) -> Int:
    """
    Filter NA values from Float64 array.
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        out_ptr: Output array pointer (must be at least x_len)
    
    Returns:
        Number of non-NA values
    """
    out_idx: Int = 0
    for i in range(0, x_len):
        val = x_ptr[i]
        if not _mojor_is_na_f64(val):
            out_ptr[out_idx] = val
            out_idx += 1
    return out_idx


@always_inline
fn _mojor_filter_na_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    out_ptr: MutI32Ptr
) -> Int:
    """
    Filter NA values from Int32 array.
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        out_ptr: Output array pointer (must be at least x_len)
    
    Returns:
        Number of non-NA values
    """
    out_idx: Int = 0
    for i in range(0, x_len):
        val = x_ptr[i]
        if not _mojor_is_na_i32(val):
            out_ptr[out_idx] = val
            out_idx += 1
    return out_idx


@always_inline
fn _mojor_filter_na_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    out_ptr: MutF64Ptr
) -> Int:
    """
    Filter NA values from Float32 array.
    Writes Float64 values into out_ptr.
    """
    out_idx: Int = 0
    for i in range(0, x_len):
        val = x_ptr[i]
        if not _mojor_is_na_f32(val):
            out_ptr[out_idx] = Float64(val)
            out_idx += 1
    return out_idx


# ============================================================================
# Helper: Linear interpolation
# ============================================================================

@always_inline
fn _mojor_lerp(a: Float64, b: Float64, t: Float64) -> Float64:
    """Linear interpolation between a and b with weight t."""
    return a + t * (b - a)


@always_inline
fn _mojor_abs_f64(x: Float64) -> Float64:
    if x < 0.0:
        return -x
    return x


@always_inline
fn _mojor_nan_f64() -> Float64:
    """Canonical NaN value for Float64."""
    return Float64(0) / Float64(0)


# ============================================================================
# Helper: Quantile types 1-8 interpolation
# ============================================================================

@always_inline
fn _mojor_quantile_interp(
    sorted: MutF64Ptr,
    n: Int,
    p: Float64,
    type: Int
) -> Float64:
    """
    Compute quantile using types 1-8 interpolation methods.
    
    Args:
        sorted: Sorted array (modified in place)
        n: Number of valid values
        p: Quantile probability in [0, 1]
        type: Interpolation type (1-8)
    
    Returns:
        The quantile value
    """
    if n == 0:
        return _mojor_nan_f64()
    
    if n == 1:
        return sorted[0]
    
    # Adjust p for 0-indexed array
    # Types 1-3: Discrete methods
    # Types 4-8: Continuous methods with different interpolation
    
    if type <= 3:
        # Discrete methods
        if type == 1:
            # Type 1: Inverse of empirical CDF
            h = n * p
            if h <= 0:
                return sorted[0]
            elif h < 1:
                return sorted[0]
            else:
                return sorted[Int(floor(h)) - 1]
        
        elif type == 2:
            # Type 2: Average of empirical CDF
            h = n * p
            if h <= 0:
                return sorted[0]
            elif h < 1:
                return (sorted[0] + sorted[0]) / 2.0
            else:
                idx = Int(floor(h)) - 1
                if floor(h) == h:
                    return (sorted[idx] + sorted[idx + 1]) / 2.0
                else:
                    return sorted[idx]
        
        elif type == 3:
            # Type 3: Nearest even
            h = n * p + 0.5
            if h <= 0.5:
                return sorted[0]
            else:
                idx = Int(floor(h))
                if floor(h + 0.5) == h + 0.5:
                    # Even rule
                    if idx % 2 == 0:
                        return sorted[idx - 1]
                    else:
                        return sorted[idx - 2]
                else:
                    return sorted[idx - 1]
    
    else:
        # Continuous methods (types 4-8)
        # General R/Hyndman-Fan form:
        # h = a + p * (n + 1 - a - b)
        # q = x[floor(h)] + (h - floor(h)) * (x[ceil(h)] - x[floor(h)])
        
        if type == 4:
            # Type 4: linear interpolation of empirical CDF
            a = 0.0
            b = 1.0
        elif type == 5:
            # Type 5: Hazen
            a = 0.5
            b = 0.5
        elif type == 6:
            # Type 6: Weibull
            a = 0.0
            b = 0.0
        elif type == 7:
            # Type 7: Default in R
            a = 1.0
            b = 1.0
        elif type == 8:
            # Type 8: Median unbiased
            a = 1.0 / 3.0
            b = 1.0 / 3.0
        else:
            # Default to type 7
            a = 1.0
            b = 1.0
        
        h = a + p * (n + 1.0 - a - b)
        
        if h <= 1:
            return sorted[0]
        elif h >= n:
            return sorted[n - 1]
        else:
            lower = Int(floor(h)) - 1
            upper = Int(ceil(h)) - 1
            if lower == upper:
                return sorted[lower]
            else:
                t = h - floor(h)
                return _mojor_lerp(sorted[lower], sorted[upper], t)
    
    # Fallback
    return sorted[n - 1]


# ============================================================================
# Exported Functions
# ============================================================================

@export("mojor_quantile_f64", ABI="C")
fn mojor_quantile_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    probs_ptr: ImmutF64Ptr,
    probs_len: Int,
    type: Int,
    na_rm: Bool,
    out_ptr: MutF64Ptr
) -> Int32:
    """
    Compute quantiles for a Float64 array.
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        probs_ptr: Quantile probabilities array (e.g., [0.25, 0.5, 0.75])
        probs_len: Number of probabilities
        type: Interpolation type (1-8)
        na_rm: Remove NA values if True
        out_ptr: Output array for quantile values
    
    Returns:
        0 on success, -1 on error
    """
    if x_len == 0 or probs_len == 0:
        return Int32(-1)
    
    # Allocate temporary array for sorting
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        n = _mojor_filter_na_f64(x_ptr, x_len, tmp)
        if n == 0:
            # All values were NA
            for i in range(0, probs_len):
                out_ptr[i] = _mojor_nan_f64()
            tmp.free()
            return 0
    else:
        # Copy all values
        for i in range(0, x_len):
            tmp[i] = x_ptr[i]
        n = x_len
    
    # Sort once
    _mojor_sort_f64(tmp, n)

    # Compute each quantile
    for i in range(0, probs_len):
        p = probs_ptr[i]

        # Validate probability
        if p < 0.0 or p > 1.0:
            out_ptr[i] = _mojor_nan_f64()
            continue

        # Compute quantile
        out_ptr[i] = _mojor_quantile_interp(tmp, n, p, type)
    tmp.free()
    return 0


@export("mojor_quantile_f32", ABI="C")
fn mojor_quantile_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    probs_ptr: ImmutF64Ptr,
    probs_len: Int,
    type: Int,
    na_rm: Bool,
    out_ptr: MutF64Ptr
) -> Int32:
    """
    Compute quantiles for a Float32 array (output as Float64).
    """
    if x_len == 0 or probs_len == 0:
        return Int32(-1)

    var tmp = alloc[Float64](x_len)

    n: Int
    if na_rm:
        n = _mojor_filter_na_f32(x_ptr, x_len, tmp)
        if n == 0:
            for i in range(0, probs_len):
                out_ptr[i] = _mojor_nan_f64()
            tmp.free()
            return 0
    else:
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len

    _mojor_sort_f64(tmp, n)

    for i in range(0, probs_len):
        p = probs_ptr[i]
        if p < 0.0 or p > 1.0:
            out_ptr[i] = _mojor_nan_f64()
            continue
        out_ptr[i] = _mojor_quantile_interp(tmp, n, p, type)
    tmp.free()
    return 0


@export("mojor_quantile_i32", ABI="C")
fn mojor_quantile_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    probs_ptr: ImmutF64Ptr,
    probs_len: Int,
    type: Int,
    na_rm: Bool,
    out_ptr: MutF64Ptr
) -> Int32:
    """
    Compute quantiles for an Int32 array (output as Float64).
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        probs_ptr: Quantile probabilities array
        probs_len: Number of probabilities
        type: Interpolation type (1-8)
        na_rm: Remove NA values if True
        out_ptr: Output array for quantile values (Float64)
    
    Returns:
        0 on success, -1 on error
    """
    if x_len == 0 or probs_len == 0:
        return Int32(-1)
    
    # Allocate temporary array for sorting
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        out_n: Int = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_i32(val):
                tmp[out_n] = Float64(val)
                out_n += 1
        n = out_n
        if n == 0:
            # All values were NA
            for i in range(0, probs_len):
                out_ptr[i] = _mojor_nan_f64()
            tmp.free()
            return 0
    else:
        # Copy all values
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len
    
    # Sort once
    _mojor_sort_f64(tmp, n)

    # Compute each quantile
    for i in range(0, probs_len):
        p = probs_ptr[i]

        # Validate probability
        if p < 0.0 or p > 1.0:
            out_ptr[i] = _mojor_nan_f64()
            continue

        # Compute quantile
        out_ptr[i] = _mojor_quantile_interp(tmp, n, p, type)
    tmp.free()
    return 0


@export("mojor_median_f64", ABI="C")
fn mojor_median_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute median for a Float64 array.
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        na_rm: Remove NA values if True
    
    Returns:
        Median value
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Allocate temporary array
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        n = _mojor_filter_na_f64(x_ptr, x_len, tmp)
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = x_ptr[i]
        n = x_len

    # Sort in place
    _mojor_sort_f64(tmp, n)

    # Compute median (quantile 0.5)
    result = _mojor_quantile_interp(tmp, n, 0.5, 7)  # Type 7 = R default
    tmp.free()
    return result


@export("mojor_median_f32", ABI="C")
fn mojor_median_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute median for a Float32 array (output as Float64).
    """
    if x_len == 0:
        return _mojor_nan_f64()

    var tmp = alloc[Float64](x_len)

    n: Int
    if na_rm:
        n = _mojor_filter_na_f32(x_ptr, x_len, tmp)
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len

    _mojor_sort_f64(tmp, n)
    result = _mojor_quantile_interp(tmp, n, 0.5, 7)
    tmp.free()
    return result


@export("mojor_median_i32", ABI="C")
fn mojor_median_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute median for an Int32 array (output as Float64).
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        na_rm: Remove NA values if True
    
    Returns:
        Median value as Float64
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Allocate temporary array
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        out_n: Int = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_i32(val):
                tmp[out_n] = Float64(val)
                out_n += 1
        n = out_n
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len

    # Sort in place
    _mojor_sort_f64(tmp, n)

    # Compute median (quantile 0.5)
    result = _mojor_quantile_interp(tmp, n, 0.5, 7)
    tmp.free()
    return result


@export("mojor_iqr_f64", ABI="C")
fn mojor_iqr_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute IQR (Interquartile Range) for a Float64 array.
    IQR = Q3 - Q1 = quantile(0.75) - quantile(0.25)
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        na_rm: Remove NA values if True
    
    Returns:
        IQR value
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Allocate temporary array
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        n = _mojor_filter_na_f64(x_ptr, x_len, tmp)
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = x_ptr[i]
        n = x_len

    # Sort in place
    _mojor_sort_f64(tmp, n)

    # Compute Q1 and Q3
    q1 = _mojor_quantile_interp(tmp, n, 0.25, 7)
    q3 = _mojor_quantile_interp(tmp, n, 0.75, 7)
    result = q3 - q1
    tmp.free()
    return result


@export("mojor_iqr_f32", ABI="C")
fn mojor_iqr_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute IQR for a Float32 array (output as Float64).
    """
    if x_len == 0:
        return _mojor_nan_f64()

    var tmp = alloc[Float64](x_len)

    n: Int
    if na_rm:
        n = _mojor_filter_na_f32(x_ptr, x_len, tmp)
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len

    _mojor_sort_f64(tmp, n)
    q1 = _mojor_quantile_interp(tmp, n, 0.25, 7)
    q3 = _mojor_quantile_interp(tmp, n, 0.75, 7)
    result = q3 - q1
    tmp.free()
    return result


@export("mojor_iqr_i32", ABI="C")
fn mojor_iqr_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    na_rm: Bool
) -> Float64:
    """
    Compute IQR for an Int32 array (output as Float64).
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        na_rm: Remove NA values if True
    
    Returns:
        IQR value as Float64
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Allocate temporary array
    var tmp = alloc[Float64](x_len)
    
    # Filter NA if needed
    n: Int
    if na_rm:
        out_n: Int = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_i32(val):
                tmp[out_n] = Float64(val)
                out_n += 1
        n = out_n
        if n == 0:
            tmp.free()
            return _mojor_nan_f64()
    else:
        for i in range(0, x_len):
            tmp[i] = Float64(x_ptr[i])
        n = x_len

    # Sort in place
    _mojor_sort_f64(tmp, n)

    # Compute Q1 and Q3
    q1 = _mojor_quantile_interp(tmp, n, 0.25, 7)
    q3 = _mojor_quantile_interp(tmp, n, 0.75, 7)
    result = q3 - q1
    tmp.free()
    return result


@export("mojor_mad_f64", ABI="C")
fn mojor_mad_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    center: Float64,
    na_rm: Bool,
    constant: Float64,
    type: Int
) -> Float64:
    """
    Compute MAD (Median Absolute Deviation) for a Float64 array.
    MAD = median(|x - center|) * constant
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        center: Center value (usually median of x)
        na_rm: Remove NA values if True
        constant: Scaling constant (default 1.4826 for normal distribution)
        type: Type parameter for median computation (1-8)
    
    Returns:
        MAD value
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Compute absolute deviations
    var devs = alloc[Float64](x_len)
    n: Int
    
    if na_rm:
        n = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_f64(val):
                devs[n] = _mojor_abs_f64(val - center)
                n += 1
    else:
        n = x_len
        for i in range(0, x_len):
            val = x_ptr[i]
            devs[i] = _mojor_abs_f64(val - center)
    
    if n == 0:
        devs.free()
        return _mojor_nan_f64()

    # Sort deviations in place
    _mojor_sort_f64(devs, n)

    # Compute median of deviations
    median_dev = _mojor_quantile_interp(devs, n, 0.5, type)
    result = median_dev * constant
    devs.free()
    return result


@export("mojor_mad_f32", ABI="C")
fn mojor_mad_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    center: Float64,
    na_rm: Bool,
    constant: Float64,
    type: Int
) -> Float64:
    """
    Compute MAD for a Float32 array (output as Float64).
    """
    if x_len == 0:
        return _mojor_nan_f64()

    var devs = alloc[Float64](x_len)
    n: Int

    if na_rm:
        n = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_f32(val):
                devs[n] = _mojor_abs_f64(Float64(val) - center)
                n += 1
    else:
        n = x_len
        for i in range(0, x_len):
            val = x_ptr[i]
            devs[i] = _mojor_abs_f64(Float64(val) - center)

    if n == 0:
        devs.free()
        return _mojor_nan_f64()

    _mojor_sort_f64(devs, n)
    median_dev = _mojor_quantile_interp(devs, n, 0.5, type)
    result = median_dev * constant
    devs.free()
    return result


@export("mojor_mad_i32", ABI="C")
fn mojor_mad_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    center: Float64,
    na_rm: Bool,
    constant: Float64,
    type: Int
) -> Float64:
    """
    Compute MAD for an Int32 array.
    
    Args:
        x_ptr: Input array pointer
        x_len: Input array length
        center: Center value (usually median of x)
        na_rm: Remove NA values if True
        constant: Scaling constant
        type: Type parameter for median computation
    
    Returns:
        MAD value as Float64
    """
    if x_len == 0:
        return _mojor_nan_f64()
    
    # Compute absolute deviations
    var devs = alloc[Float64](x_len)
    n: Int
    
    if na_rm:
        n = 0
        for i in range(0, x_len):
            val = x_ptr[i]
            if not _mojor_is_na_i32(val):
                devs[n] = _mojor_abs_f64(Float64(val) - center)
                n += 1
    else:
        n = x_len
        for i in range(0, x_len):
            val = x_ptr[i]
            devs[i] = _mojor_abs_f64(Float64(val) - center)
    
    if n == 0:
        devs.free()
        return _mojor_nan_f64()

    # Sort deviations in place
    _mojor_sort_f64(devs, n)

    # Compute median of deviations
    median_dev = _mojor_quantile_interp(devs, n, 0.5, type)
    result = median_dev * constant
    devs.free()
    return result
