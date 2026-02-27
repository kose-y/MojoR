# Gamma distribution module for MojoR
from abi_types import MutOpaqueAny
from rng_helpers import _rng_fill_rgamma

@export("mojor_rgamma", ABI="C")
fn mojor_rgamma(out_ptr: MutOpaqueAny, n: Int32, shape: Float64, rate: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n gamma random numbers Gamma(shape, rate)."""
    _rng_fill_rgamma(out_ptr, n, shape, rate, state_ptr)
