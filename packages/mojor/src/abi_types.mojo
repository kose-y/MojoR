# Shared ABI pointer aliases for Mojo helper modules.

from memory import OpaquePointer, UnsafePointer

comptime ImmutOpaqueAny = OpaquePointer[mut=False, origin=ImmutAnyOrigin]
comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]

# Raw scalar element pointers (used by helper modules).
comptime ImmutF64Ptr = UnsafePointer[mut=False, type=Float64, origin=ImmutAnyOrigin]
comptime ImmutF32Ptr = UnsafePointer[mut=False, type=Float32, origin=ImmutAnyOrigin]
comptime ImmutI32Ptr = UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin]
comptime MutF64Ptr = UnsafePointer[mut=True, type=Float64, origin=MutAnyOrigin]
comptime MutF32Ptr = UnsafePointer[mut=True, type=Float32, origin=MutAnyOrigin]
comptime MutI32Ptr = UnsafePointer[mut=True, type=Int32, origin=MutAnyOrigin]

# Scalar[DType.*] pointers used by backend ABI wrappers.
comptime ImmutF64ScalarPtr = UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin]
comptime ImmutF32ScalarPtr = UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin]
comptime MutF64ScalarPtr = UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin]
comptime MutF32ScalarPtr = UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin]

comptime MutU64Ptr = UnsafePointer[mut=True, type=UInt64, origin=MutAnyOrigin]
