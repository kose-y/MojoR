# MojoR Set/Match Helpers
#
# Provides hash table-based set and match operations using Mojo dictionaries.
# Supports numeric/integer vectors for unique, duplicated, match, and %in%.
# NaN handling: NaN values are tracked separately (NaN != NaN breaks Dict keys).
#
# Date: 2026-02-15
# Tier: 7.3 Set/Match Primitives

from abi_types import ImmutF64Ptr, ImmutF32Ptr, ImmutI32Ptr, MutF64Ptr, MutF32Ptr, MutI32Ptr

@always_inline
fn _is_nan_f64(val: Float64) -> Bool:
    return val != val


@always_inline
fn _is_nan_f32(val: Float32) -> Bool:
    return val != val


@always_inline
fn _is_na_i32(val: Int32) -> Bool:
    return val == Int32(-2147483648)

# ============================================================================
# unique(x) - Returns unique values from a vector
# ============================================================================

@export("mojor_unique_f64", ABI="C")
fn mojor_unique_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    out_ptr: MutF64Ptr
) -> Int32:
    if x_len <= 0:
        return Int32(0)

    var seen = Dict[Float64, Bool]()
    var out_idx: Int = 0
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f64(val):
            if not seen_nan:
                seen_nan = True
                out_ptr[out_idx] = val
                out_idx += 1
        elif val not in seen:
            seen[val] = True
            out_ptr[out_idx] = val
            out_idx += 1

    return Int32(out_idx)


@export("mojor_unique_i32", ABI="C")
fn mojor_unique_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return Int32(0)

    var seen = Dict[Int32, Bool]()
    var out_idx: Int = 0

    for i in range(x_len):
        var val = x_ptr[i]
        if val not in seen:
            seen[val] = True
            out_ptr[out_idx] = val
            out_idx += 1

    return Int32(out_idx)


@export("mojor_unique_f32", ABI="C")
fn mojor_unique_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    out_ptr: MutF32Ptr
) -> Int32:
    if x_len <= 0:
        return Int32(0)

    var seen = Dict[Float32, Bool]()
    var out_idx: Int = 0
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f32(val):
            if not seen_nan:
                seen_nan = True
                out_ptr[out_idx] = val
                out_idx += 1
        elif val not in seen:
            seen[val] = True
            out_ptr[out_idx] = val
            out_idx += 1

    return Int32(out_idx)


# ============================================================================
# duplicated(x) - Returns logical vector indicating duplicated values
# ============================================================================

@export("mojor_duplicated_f64", ABI="C")
fn mojor_duplicated_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Float64, Bool]()
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f64(val):
            if seen_nan:
                out_ptr[i] = 1
            else:
                seen_nan = True
                out_ptr[i] = 0
        elif val in seen:
            out_ptr[i] = 1
        else:
            seen[val] = True
            out_ptr[i] = 0

    return 0


@export("mojor_duplicated_f32", ABI="C")
fn mojor_duplicated_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Float32, Bool]()
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f32(val):
            if seen_nan:
                out_ptr[i] = 1
            else:
                seen_nan = True
                out_ptr[i] = 0
        elif val in seen:
            out_ptr[i] = 1
        else:
            seen[val] = True
            out_ptr[i] = 0

    return 0


@export("mojor_duplicated_i32", ABI="C")
fn mojor_duplicated_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Int32, Bool]()

    for i in range(x_len):
        var val = x_ptr[i]
        if val in seen:
            out_ptr[i] = 1
        else:
            seen[val] = True
            out_ptr[i] = 0

    return 0


# ============================================================================
# anyDuplicated(x) - Returns index of first duplicate or 0 if none
# ============================================================================

@export("mojor_any_duplicated_f64", ABI="C")
fn mojor_any_duplicated_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Float64, Bool]()
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f64(val):
            if seen_nan:
                return Int32(i + 1)
            seen_nan = True
        elif val in seen:
            return Int32(i + 1)
        else:
            seen[val] = True

    return 0


@export("mojor_any_duplicated_f32", ABI="C")
fn mojor_any_duplicated_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Float32, Bool]()
    var seen_nan: Bool = False

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f32(val):
            if seen_nan:
                return Int32(i + 1)
            seen_nan = True
        elif val in seen:
            return Int32(i + 1)
        else:
            seen[val] = True

    return 0


@export("mojor_any_duplicated_i32", ABI="C")
fn mojor_any_duplicated_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int
) -> Int32:
    if x_len <= 0:
        return 0

    var seen = Dict[Int32, Bool]()

    for i in range(x_len):
        var val = x_ptr[i]
        if val in seen:
            return Int32(i + 1)
        seen[val] = True

    return 0


# ============================================================================
# match(x, table) - Returns positions of first matches (1-based, 0 if not found)
# ============================================================================

@export("mojor_match_f64", ABI="C")
fn mojor_match_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    table_ptr: ImmutF64Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var na_val: Int32 = Int32(-2147483648)
    var nan_pos: Int32 = na_val  # NA if not in table

    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_nan_f64(tv):
            if nan_pos == na_val:
                nan_pos = Int32(j + 1)

    # Look up each value in x
    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f64(val):
            out_ptr[i] = nan_pos
        else:
            out_ptr[i] = na_val
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_nan_f64(tv)) and tv == val:
                    out_ptr[i] = Int32(j + 1)
                    break

    return 0


@export("mojor_match_f32", ABI="C")
fn mojor_match_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    table_ptr: ImmutF32Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var na_val: Int32 = Int32(-2147483648)
    var nan_pos: Int32 = na_val

    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_nan_f32(tv):
            if nan_pos == na_val:
                nan_pos = Int32(j + 1)

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f32(val):
            out_ptr[i] = nan_pos
        else:
            out_ptr[i] = na_val
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_nan_f32(tv)) and tv == val:
                    out_ptr[i] = Int32(j + 1)
                    break

    return 0


@export("mojor_match_i32", ABI="C")
fn mojor_match_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    table_ptr: ImmutI32Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var na_val: Int32 = Int32(-2147483648)
    var na_pos: Int32 = na_val
    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_na_i32(tv):
            if na_pos == na_val:
                na_pos = Int32(j + 1)

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_na_i32(val):
            out_ptr[i] = na_pos
        else:
            out_ptr[i] = na_val
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_na_i32(tv)) and tv == val:
                    out_ptr[i] = Int32(j + 1)
                    break

    return 0


# ============================================================================
# %in% operator - Returns logical vector indicating matches
# ============================================================================

@export("mojor_in_f64", ABI="C")
fn mojor_in_f64(
    x_ptr: ImmutF64Ptr,
    x_len: Int,
    table_ptr: ImmutF64Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var has_nan: Bool = False

    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_nan_f64(tv):
            has_nan = True

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f64(val):
            # R `%in%` uses match(..., nomatch = 0) > 0, so missing unmatched -> FALSE.
            out_ptr[i] = 1 if has_nan else 0
        else:
            var present: Bool = False
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_nan_f64(tv)) and tv == val:
                    present = True
                    break
            out_ptr[i] = 1 if present else 0

    return 0


@export("mojor_in_f32", ABI="C")
fn mojor_in_f32(
    x_ptr: ImmutF32Ptr,
    x_len: Int,
    table_ptr: ImmutF32Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var has_nan: Bool = False

    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_nan_f32(tv):
            has_nan = True

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_nan_f32(val):
            # R `%in%` uses match(..., nomatch = 0) > 0, so missing unmatched -> FALSE.
            out_ptr[i] = 1 if has_nan else 0
        else:
            var present: Bool = False
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_nan_f32(tv)) and tv == val:
                    present = True
                    break
            out_ptr[i] = 1 if present else 0

    return 0


@export("mojor_in_i32", ABI="C")
fn mojor_in_i32(
    x_ptr: ImmutI32Ptr,
    x_len: Int,
    table_ptr: ImmutI32Ptr,
    table_len: Int,
    out_ptr: MutI32Ptr
) -> Int32:
    if x_len <= 0:
        return 0

    var has_na: Bool = False
    for j in range(table_len):
        var tv = table_ptr[j]
        if _is_na_i32(tv):
            has_na = True

    for i in range(x_len):
        var val = x_ptr[i]
        if _is_na_i32(val):
            # R `%in%` uses match(..., nomatch = 0) > 0, so NA unmatched -> FALSE.
            out_ptr[i] = 1 if has_na else 0
        else:
            var present: Bool = False
            for j in range(table_len):
                var tv = table_ptr[j]
                if (not _is_na_i32(tv)) and tv == val:
                    present = True
                    break
            out_ptr[i] = 1 if present else 0

    return 0
