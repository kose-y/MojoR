# MojoR NA Helpers
#
# Provides NA-aware arithmetic and comparison operations.
# Used when na_mode="propagate" or na_mode="na_rm"
#
# Key Design:
# - @always_inline: no function call overhead
# - Propagate mode: NA propagates through operations, sets flag
# - Na_rm mode: NA values are skipped in reductions
#
# Date: 2026-02-15
# Tier: 7.1 NA Semantics

from memory import stack_allocation

# Pointer type alias matching kernel ABI
comptime MutInt32Ptr = UnsafePointer[mut=True, type=Int32, origin=MutAnyOrigin]
comptime _MOJOR_R_NA_BITS_F64 = UInt64(0x7ff00000000007a2)
comptime _MOJOR_R_NA_BITS_F32 = UInt32(0x7f8007a2)

# ============================================================================
# NA Detection Helpers
# ============================================================================

@always_inline
fn _mojor_bits_f64(val: Float64) -> UInt64:
    """Reinterpret Float64 bits as UInt64."""
    var buf = stack_allocation[1, Float64]()
    buf[0] = val
    return buf.bitcast[UInt64]()[0]


@always_inline
fn _mojor_bits_f32(val: Float32) -> UInt32:
    """Reinterpret Float32 bits as UInt32."""
    var buf = stack_allocation[1, Float32]()
    buf[0] = val
    return buf.bitcast[UInt32]()[0]


@always_inline
fn _mojor_is_na_f64(val: Float64) -> Bool:
    """Check if a Float64 value is R NA (payload-specific NaN)."""
    return _mojor_bits_f64(val) == _MOJOR_R_NA_BITS_F64


@always_inline
fn _mojor_is_missing_f64(val: Float64) -> Bool:
    """Check if a Float64 value is any NaN payload (NA or NaN)."""
    return val != val


@always_inline
fn _mojor_is_nan_f64(val: Float64) -> Bool:
    """Check if a Float64 value is NaN but not R NA."""
    return _mojor_is_missing_f64(val) and (not _mojor_is_na_f64(val))


@always_inline
fn _mojor_is_na_f32(val: Float32) -> Bool:
    """Check if a Float32 value is R NA payload."""
    return _mojor_bits_f32(val) == _MOJOR_R_NA_BITS_F32


@always_inline
fn _mojor_is_missing_f32(val: Float32) -> Bool:
    """Check if a Float32 value is any NaN payload (NA or NaN)."""
    return val != val


@always_inline
fn _mojor_is_nan_f32(val: Float32) -> Bool:
    """Check if a Float32 value is NaN but not R NA."""
    return _mojor_is_missing_f32(val) and (not _mojor_is_na_f32(val))


@always_inline
fn _mojor_is_na_i32(val: Int32) -> Bool:
    """Check if an Int32 value is NA (INT_MIN = -2147483648)."""
    return val == Int32(-2147483648)


@always_inline
fn _mojor_is_na_lgl(val: Int32) -> Bool:
    """Check if a logical (Int32) value is NA (INT_MIN = -2147483648)."""
    return val == Int32(-2147483648)


@always_inline
fn _mojor_has_na_f64(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """Returns True and marks na_flag when either operand is NA/NaN."""
    if _mojor_is_missing_f64(a) or _mojor_is_missing_f64(b):
        na_flag[0] = 1
        return True
    return False


@always_inline
fn _mojor_has_na_f32(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """Returns True and marks na_flag when either operand is NA/NaN."""
    if _mojor_is_missing_f32(a) or _mojor_is_missing_f32(b):
        na_flag[0] = 1
        return True
    return False


@always_inline
fn _mojor_nan_f64() -> Float64:
    """Canonical NaN value for Float64 NA propagation."""
    return Float64(0) / Float64(0)


@always_inline
fn _mojor_nan_f32() -> Float32:
    """Canonical NaN value for Float32 NA propagation."""
    return Float32(0) / Float32(0)


@always_inline
fn _mojor_r_na_f64() -> Float64:
    """R NA payload value for Float64."""
    var bits = stack_allocation[1, UInt64]()
    bits[0] = _MOJOR_R_NA_BITS_F64
    return bits.bitcast[Float64]()[0]


@always_inline
fn _mojor_r_na_f32() -> Float32:
    """R NA payload value for Float32."""
    var bits = stack_allocation[1, UInt32]()
    bits[0] = _MOJOR_R_NA_BITS_F32
    return bits.bitcast[Float32]()[0]


# ============================================================================
# NA-Skipping Helpers (na_mode="na_rm")
# ============================================================================

@always_inline
fn _mojor_skip_na_f64(val: Float64) -> Bool:
    """Returns True if val is NA/NaN and should be skipped."""
    return _mojor_is_missing_f64(val)


@always_inline
fn _mojor_skip_na_f32(val: Float32) -> Bool:
    """Returns True if val is NA/NaN and should be skipped."""
    return _mojor_is_missing_f32(val)


@always_inline
fn _mojor_skip_na_i32(val: Int32) -> Bool:
    """Returns True if val is NA and should be skipped."""
    return _mojor_is_na_i32(val)


@always_inline
fn _mojor_skip_na_lgl(val: Int32) -> Bool:
    """Returns True if val is NA and should be skipped."""
    return _mojor_is_na_lgl(val)


# ============================================================================
# NA-Aware Binary Arithmetic (na_mode="propagate")
# na_flag: MutInt32Ptr — kernel sets flag[0] = 1 if any NA encountered
# ============================================================================

@always_inline
fn _mojor_add_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Float64:
    """NA-aware addition for Float64 preserving NA vs NaN payloads."""
    if _mojor_is_na_f64(a) or _mojor_is_na_f64(b):
        na_flag[0] = 1
        return _mojor_r_na_f64()
    if _mojor_has_na_f64(a, b, na_flag):
        return _mojor_nan_f64()
    return a + b


@always_inline
fn _mojor_sub_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Float64:
    """NA-aware subtraction for Float64 preserving NA vs NaN payloads."""
    if _mojor_is_na_f64(a) or _mojor_is_na_f64(b):
        na_flag[0] = 1
        return _mojor_r_na_f64()
    if _mojor_has_na_f64(a, b, na_flag):
        return _mojor_nan_f64()
    return a - b


@always_inline
fn _mojor_mul_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Float64:
    """NA-aware multiplication for Float64 preserving NA vs NaN payloads."""
    if _mojor_is_na_f64(a) or _mojor_is_na_f64(b):
        na_flag[0] = 1
        return _mojor_r_na_f64()
    if _mojor_has_na_f64(a, b, na_flag):
        return _mojor_nan_f64()
    return a * b


@always_inline
fn _mojor_div_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Float64:
    """NA-aware division for Float64 preserving NA vs NaN payloads."""
    if _mojor_is_na_f64(a) or _mojor_is_na_f64(b):
        na_flag[0] = 1
        return _mojor_r_na_f64()
    if _mojor_has_na_f64(a, b, na_flag):
        return _mojor_nan_f64()
    return a / b


@always_inline
fn _mojor_add_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Float32:
    """NA-aware addition for Float32 preserving NA vs NaN payloads."""
    if _mojor_is_na_f32(a) or _mojor_is_na_f32(b):
        na_flag[0] = 1
        return _mojor_r_na_f32()
    if _mojor_has_na_f32(a, b, na_flag):
        return _mojor_nan_f32()
    return a + b


@always_inline
fn _mojor_sub_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Float32:
    """NA-aware subtraction for Float32 preserving NA vs NaN payloads."""
    if _mojor_is_na_f32(a) or _mojor_is_na_f32(b):
        na_flag[0] = 1
        return _mojor_r_na_f32()
    if _mojor_has_na_f32(a, b, na_flag):
        return _mojor_nan_f32()
    return a - b


@always_inline
fn _mojor_mul_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Float32:
    """NA-aware multiplication for Float32 preserving NA vs NaN payloads."""
    if _mojor_is_na_f32(a) or _mojor_is_na_f32(b):
        na_flag[0] = 1
        return _mojor_r_na_f32()
    if _mojor_has_na_f32(a, b, na_flag):
        return _mojor_nan_f32()
    return a * b


@always_inline
fn _mojor_div_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Float32:
    """NA-aware division for Float32 preserving NA vs NaN payloads."""
    if _mojor_is_na_f32(a) or _mojor_is_na_f32(b):
        na_flag[0] = 1
        return _mojor_r_na_f32()
    if _mojor_has_na_f32(a, b, na_flag):
        return _mojor_nan_f32()
    return a / b


# ============================================================================
# NA-Aware Comparison Operations (na_mode="propagate")
# ============================================================================

@always_inline
fn _mojor_lt_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware less-than. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a < b


@always_inline
fn _mojor_gt_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware greater-than. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a > b


@always_inline
fn _mojor_le_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware less-or-equal. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a <= b


@always_inline
fn _mojor_ge_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware greater-or-equal. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a >= b


@always_inline
fn _mojor_eq_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware equality. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a == b


@always_inline
fn _mojor_ne_f64_na(a: Float64, b: Float64, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware not-equal. Returns False if either is NA."""
    if _mojor_has_na_f64(a, b, na_flag):
        return False
    return a != b


@always_inline
fn _mojor_lt_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware less-than. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a < b


@always_inline
fn _mojor_gt_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware greater-than. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a > b


@always_inline
fn _mojor_le_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware less-or-equal. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a <= b


@always_inline
fn _mojor_ge_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware greater-or-equal. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a >= b


@always_inline
fn _mojor_eq_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware equality. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a == b


@always_inline
fn _mojor_ne_f32_na(a: Float32, b: Float32, na_flag: MutInt32Ptr) -> Bool:
    """NA-aware not-equal. Returns False if either is NA."""
    if _mojor_has_na_f32(a, b, na_flag):
        return False
    return a != b
