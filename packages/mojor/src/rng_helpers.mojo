from math import exp, log, log1p, sqrt, tan
from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE, _ZIGGURAT_NOR_R, _ZIGGURAT_NOR_INV_R
from abi_types import MutU64Ptr, MutOpaqueAny, MutF64ScalarPtr

@always_inline
fn _rng_rotl(x: UInt64, k: Int) -> UInt64:
    return (x << k) | (x >> (64 - k))

@always_inline
fn _rng_next_u64(state: MutU64Ptr) -> UInt64:
    var s0 = state[0]
    var s1 = state[1]
    var s2 = state[2]
    var s3 = state[3]

    var result = _rng_rotl(s0 + s3, 23) + s0
    var t = s1 << 17

    s2 ^= s0
    s3 ^= s1
    s1 ^= s2
    s0 ^= s3
    s2 ^= t
    s3 = _rng_rotl(s3, 45)

    state[0] = s0
    state[1] = s1
    state[2] = s2
    state[3] = s3
    return result

@always_inline
fn _rng_next_f64(state: MutU64Ptr) -> Float64:
    return Float64(_rng_next_u64(state) >> 11) * 1.1102230246251565e-16

fn _mojor_sample_pick_index(
    state: MutU64Ptr,
    n_total: Int,
    size: Int,
    pick_pos: Int,
    replace: Bool
) -> Int:
    if n_total <= 0 or size <= 0:
        return 0
    if pick_pos < 1 or pick_pos > size:
        return 0
    if (not replace) and size > n_total:
        return 0

    var chosen: Int = 0
    if replace:
        for draw_i in range(1, size + 1):
            var idx = Int((_rng_next_f64(state) * Float64(n_total)) + 1.0)
            if idx > n_total:
                idx = n_total
            if draw_i == pick_pos:
                chosen = idx
        return chosen

    var used = Dict[Int, Bool]()
    for draw_i in range(1, size + 1):
        while True:
            var idx = Int((_rng_next_f64(state) * Float64(n_total)) + 1.0)
            if idx > n_total:
                idx = n_total
            if idx not in used:
                used[idx] = True
                if draw_i == pick_pos:
                    chosen = idx
                break
    return chosen

@always_inline
fn _rng_out_f64_ptr(out_ptr: MutOpaqueAny) -> MutF64ScalarPtr:
    return out_ptr.bitcast[Scalar[DType.float64]]()

@always_inline
fn _rng_state_ptr(state_ptr: MutOpaqueAny) -> MutU64Ptr:
    return state_ptr.bitcast[UInt64]()

fn _rng_fill_runif(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    var out = _rng_out_f64_ptr(out_ptr)
    var n_i = Int(n)
    var state = _rng_state_ptr(state_ptr)
    for i in range(n_i):
        out[i] = _rng_next_f64(state)

fn _rng_fill_runif_range(
    out_ptr: MutOpaqueAny,
    n: Int32,
    min_val: Float64,
    max_val: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out = _rng_out_f64_ptr(out_ptr)
    var n_i = Int(n)
    var range_val = max_val - min_val
    var state = _rng_state_ptr(state_ptr)
    for i in range(n_i):
        out[i] = min_val + range_val * _rng_next_f64(state)

fn _rng_fill_rnorm(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    var out = _rng_out_f64_ptr(out_ptr)
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state = _rng_state_ptr(state_ptr)
    for i in range(n_i):
        out[i] = _random_standard_normal(state, ki, wi, fi)

fn _rng_fill_rnorm_mean_sd(
    out_ptr: MutOpaqueAny,
    n: Int32,
    mean: Float64,
    sd: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out = _rng_out_f64_ptr(out_ptr)
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state = _rng_state_ptr(state_ptr)
    for i in range(n_i):
        out[i] = mean + sd * _random_standard_normal(state, ki, wi, fi)

fn _rng_fill_rgamma(
    out_ptr: MutOpaqueAny,
    n: Int32,
    shape: Float64,
    rate: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out = _rng_out_f64_ptr(out_ptr)
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state = _rng_state_ptr(state_ptr)
    var scale = 1.0 / rate
    for i in range(n_i):
        out[i] = _random_standard_gamma(state, ki, wi, fi, shape) * scale

@always_inline
fn _random_standard_exponential(state: MutU64Ptr) -> Float64:
    return -log1p(-_rng_next_f64(state))

fn _random_standard_normal(state: MutU64Ptr, ki: List[UInt64], wi: List[Float64], fi: List[Float64]) -> Float64:
    while True:
        var r = _rng_next_u64(state)
        var idx = Int(r & UInt64(0xff))
        r = r >> 8
        var sign = r & UInt64(0x1)
        var rabs = (r >> 1) & UInt64(0x000fffffffffffff)
        var x = Float64(rabs) * wi[idx]
        if (sign & UInt64(0x1)) != UInt64(0):
            x = -x
        if rabs < ki[idx]:
            return x
        if idx == 0:
            while True:
                var xx = -_ZIGGURAT_NOR_INV_R * log1p(-_rng_next_f64(state))
                var yy = -log1p(-_rng_next_f64(state))
                if (yy + yy > xx * xx):
                    if ((rabs >> 8) & UInt64(0x1)) != UInt64(0):
                        return -(_ZIGGURAT_NOR_R + xx)
                    else:
                        return _ZIGGURAT_NOR_R + xx
        else:
            if (((fi[idx - 1] - fi[idx]) * _rng_next_f64(state) + fi[idx]) < exp(-0.5 * x * x)):
                return x

fn _random_standard_gamma(state: MutU64Ptr, ki: List[UInt64], wi: List[Float64], fi: List[Float64], shape: Float64) -> Float64:
    if shape == 1.0:
        return _random_standard_exponential(state)
    elif shape == 0.0:
        return 0.0
    elif shape < 1.0:
        while True:
            var u = _rng_next_f64(state)
            var v = _random_standard_exponential(state)
            if u <= 1.0 - shape:
                var x = u ** (1.0 / shape)
                if x <= v:
                    return x
            else:
                var y = -log((1.0 - u) / shape)
                var x = (1.0 - shape + shape * y) ** (1.0 / shape)
                if x <= (v + y):
                    return x
    else:
        var b = shape - 1.0 / 3.0
        var c = 1.0 / sqrt(9.0 * b)
        while True:
            var x: Float64
            var v: Float64
            while True:
                x = _random_standard_normal(state, ki, wi, fi)
                v = 1.0 + c * x
                if v > 0.0:
                    break
            v = v * v * v
            var u = _rng_next_f64(state)
            if u < 1.0 - 0.0331 * x * x * x * x:
                return b * v
            if log(u) < 0.5 * x * x + b * (1.0 - v + log(v)):
                return b * v

fn _random_poisson(state: MutU64Ptr, ki: List[UInt64], wi: List[Float64], fi: List[Float64], lam: Float64) -> Int:
    if lam <= 0.0:
        return 0
    if lam < 30.0:
        var l = exp(-lam)
        var p = 1.0
        var k: Int = 0
        while p > l:
            k += 1
            p *= _rng_next_f64(state)
        return k - 1

    # Fast fallback for large lambda: rounded normal approximation.
    while True:
        var z = _random_standard_normal(state, ki, wi, fi)
        var x = lam + sqrt(lam) * z
        if x >= 0.0:
            return Int(x + 0.5)

@always_inline
fn _random_chisq(state: MutU64Ptr, ki: List[UInt64], wi: List[Float64], fi: List[Float64], df: Float64) -> Float64:
    return 2.0 * _random_standard_gamma(state, ki, wi, fi, 0.5 * df)

@always_inline
fn _random_beta(state: MutU64Ptr, ki: List[UInt64], wi: List[Float64], fi: List[Float64], shape1: Float64, shape2: Float64) -> Float64:
    var x = _random_standard_gamma(state, ki, wi, fi, shape1)
    var y = _random_standard_gamma(state, ki, wi, fi, shape2)
    var denom = x + y
    if denom <= 0.0:
        return 0.0
    return x / denom

@always_inline
fn _random_binomial(state: MutU64Ptr, size: Int, prob: Float64) -> Int:
    var count: Int = 0
    for _ in range(size):
        if _rng_next_f64(state) < prob:
            count += 1
    return count

@always_inline
fn _random_weibull(state: MutU64Ptr, shape: Float64, scale: Float64) -> Float64:
    var e = -log1p(-_rng_next_f64(state))
    return scale * (e ** (1.0 / shape))

@always_inline
fn _random_logistic(state: MutU64Ptr, location: Float64, scale: Float64) -> Float64:
    var u = _rng_next_f64(state)
    if u <= 0.0:
        u = 1e-16
    if u >= 1.0:
        u = 1.0 - 1e-16
    return location + scale * log(u / (1.0 - u))

@always_inline
fn _random_cauchy(state: MutU64Ptr, location: Float64, scale: Float64) -> Float64:
    var u = _rng_next_f64(state)
    return location + scale * tan(3.14159265358979323846 * (u - 0.5))

@always_inline
fn _random_geometric(state: MutU64Ptr, prob: Float64) -> Int:
    var u = _rng_next_f64(state)
    return Int(log1p(-u) / log1p(-prob))

fn _random_hypergeometric(state: MutU64Ptr, m: Int, n: Int, k: Int) -> Int:
    var white = m
    var black = n
    var draws = k
    var success: Int = 0
    for _ in range(draws):
        var total = white + black
        if total <= 0:
            break
        if white <= 0:
            black -= 1
            continue
        var p_white = Float64(white) / Float64(total)
        if _rng_next_f64(state) < p_white:
            success += 1
            white -= 1
        else:
            black -= 1
    return success

fn _random_signrank(state: MutU64Ptr, n: Int) -> Int:
    var stat: Int = 0
    for rank in range(1, n + 1):
        if _rng_next_f64(state) < 0.5:
            stat += rank
    return stat

fn _random_wilcox(state: MutU64Ptr, m: Int, n: Int) -> Int:
    var total = m + n
    if m <= 0 or n <= 0 or total <= 0:
        return 0

    var need = m
    var rank_sum: Int = 0
    for rank in range(1, total + 1):
        var remaining = total - rank + 1
        if need <= 0:
            break
        if need >= remaining:
            rank_sum += rank
            need -= 1
            continue
        var p_pick = Float64(need) / Float64(remaining)
        if _rng_next_f64(state) < p_pick:
            rank_sum += rank
            need -= 1

    return rank_sum - ((m * (m + 1)) // 2)
