# RNG module for MojoR
from memory import OpaquePointer, UnsafePointer
from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE
from rng_helpers import _rng_next_f64, _random_standard_normal, _random_binomial

comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]
comptime MutF64Ptr = UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin]
comptime MutU64Ptr = UnsafePointer[mut=True, type=UInt64, origin=MutAnyOrigin]


@export("mojor_rng_seed", ABI="C")
fn mojor_rng_seed(seed_val: Int32) -> None:
    # Seed state is managed by the C bridge.
    _ = seed_val


@export("mojor_runif", ABI="C")
fn mojor_runif(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(Int(n)):
        out[i] = _rng_next_f64(state)


@export("mojor_runif_range", ABI="C")
fn mojor_runif_range(
    out_ptr: MutOpaqueAny,
    n: Int32,
    min_val: Float64,
    max_val: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var range_val = max_val - min_val
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(Int(n)):
        out[i] = min_val + range_val * _rng_next_f64(state)


@export("mojor_rnorm", ABI="C")
fn mojor_rnorm(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(Int(n)):
        out[i] = _random_standard_normal(state, ki, wi, fi)


@export("mojor_rnorm_mean_sd", ABI="C")
fn mojor_rnorm_mean_sd(
    out_ptr: MutOpaqueAny,
    n: Int32,
    mean: Float64,
    sd: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(Int(n)):
        out[i] = mean + sd * _random_standard_normal(state, ki, wi, fi)


@export("mojor_rbinom", ABI="C")
fn mojor_rbinom(
    out_ptr: MutOpaqueAny,
    n: Int32,
    size: Int32,
    prob: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(Int(n)):
        out[i] = Float64(_random_binomial(state, Int(size), prob))
