# RNG module for MojoR
from abi_types import MutOpaqueAny, MutF64ScalarPtr, MutU64Ptr
from rng_helpers import _rng_fill_runif, _rng_fill_runif_range, _rng_fill_rnorm, _rng_fill_rnorm_mean_sd, _rng_state_ptr, _rng_out_f64_ptr, _random_binomial


@export("mojor_rng_seed", ABI="C")
fn mojor_rng_seed(seed_val: Int32) -> None:
    # Seed state is managed by the C bridge.
    _ = seed_val


@export("mojor_runif", ABI="C")
fn mojor_runif(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    _rng_fill_runif(out_ptr, n, state_ptr)


@export("mojor_runif_range", ABI="C")
fn mojor_runif_range(
    out_ptr: MutOpaqueAny,
    n: Int32,
    min_val: Float64,
    max_val: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    _rng_fill_runif_range(out_ptr, n, min_val, max_val, state_ptr)


@export("mojor_rnorm", ABI="C")
fn mojor_rnorm(out_ptr: MutOpaqueAny, n: Int32, state_ptr: MutOpaqueAny) -> None:
    _rng_fill_rnorm(out_ptr, n, state_ptr)


@export("mojor_rnorm_mean_sd", ABI="C")
fn mojor_rnorm_mean_sd(
    out_ptr: MutOpaqueAny,
    n: Int32,
    mean: Float64,
    sd: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    _rng_fill_rnorm_mean_sd(out_ptr, n, mean, sd, state_ptr)


@export("mojor_rbinom", ABI="C")
fn mojor_rbinom(
    out_ptr: MutOpaqueAny,
    n: Int32,
    size: Int32,
    prob: Float64,
    state_ptr: MutOpaqueAny
) -> None:
    var out: MutF64ScalarPtr = _rng_out_f64_ptr(out_ptr)
    var state: MutU64Ptr = _rng_state_ptr(state_ptr)
    for i in range(Int(n)):
        out[i] = Float64(_random_binomial(state, Int(size), prob))
