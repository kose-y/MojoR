# Gamma distribution module for MojoR
from memory import OpaquePointer, UnsafePointer
from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE
from rng_helpers import _random_standard_gamma

comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]
comptime MutF64Ptr = UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin]
comptime MutU64Ptr = UnsafePointer[mut=True, type=UInt64, origin=MutAnyOrigin]

@export("mojor_rgamma", ABI="C")
fn mojor_rgamma(out_ptr: MutOpaqueAny, n: Int32, shape: Float64, rate: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n gamma random numbers Gamma(shape, rate)."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    var scale = 1.0 / rate
    for i in range(Int(n)):
        out[i] = _random_standard_gamma(state, ki, wi, fi, shape) * scale
