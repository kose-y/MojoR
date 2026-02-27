# Mojo backend implementing the C ABI for MojoR.
# These symbols are called from the R bridge via .Call().

from memory import OpaquePointer, UnsafePointer, Span, alloc
from algorithm import sum as algo_sum, min as algo_min, max as algo_max, mean as algo_mean, product as algo_product
from buffer.buffer import NDBuffer
from math import exp, ceildiv, sqrt
from sys import has_accelerator
from sys.info import simd_width_of
from gpu.host import DeviceContext, HostBuffer, DeviceBuffer
from gpu import block_idx, block_dim, thread_idx, grid_dim
from gpu.primitives import block, warp
from gpu.primitives.id import lane_id, warp_id
from layout import Layout, LayoutTensor, RuntimeLayout
from utils import Index, IndexList

comptime MOJOR_GPU_API: String = "metal"
comptime MOJOR_GPU_F64_REDUCE_FASTPATH: Bool = False

# Concrete pointer aliases (no parametric origins)
comptime ImmutOpaqueAny = OpaquePointer[mut=False, origin=ImmutAnyOrigin]
comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]
comptime ImmutF64Ptr = UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin]
comptime MutF64Ptr = UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin]
comptime MutI32Ptr = UnsafePointer[mut=True, type=Int32, origin=MutAnyOrigin]
comptime ImmutU8Ptr = UnsafePointer[mut=False, type=UInt8, origin=ImmutAnyOrigin]
comptime MutU8Ptr = UnsafePointer[mut=True, type=UInt8, origin=MutAnyOrigin]
comptime ImmutF32Ptr = UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin]
comptime MutF32Ptr = UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin]
# Fixed SIMD width (Float64 on NEON is typically 2 lanes).
# Adjust if you target wider SIMD (e.g., AVX2/AVX-512).
comptime f64_simd_width = simd_width_of[DType.float64]()
comptime f32_simd_width = simd_width_of[DType.float32]()
comptime GPU_MAX_BYTES: Int64 = 16 * 1024 * 1024 * 1024
comptime _MOJOR_ARGEXT_LAYOUT = Layout.col_major(IndexList[1](0))

comptime MutCtxPtr = UnsafePointer[mut=True, type=DeviceContext, origin=MutAnyOrigin]
comptime MutBufF32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin]
comptime MutBufF64Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin]
comptime MutBufI32Ptr = UnsafePointer[mut=True, type=DeviceBuffer[DType.int32], origin=MutAnyOrigin]
comptime MutU64Ptr = UnsafePointer[mut=True, type=UInt64, origin=MutAnyOrigin]
comptime NULL_CTX = MutCtxPtr(unsafe_from_address=0)
comptime NULL_BUF_F32 = MutBufF32Ptr(unsafe_from_address=0)
comptime NULL_BUF_F64 = MutBufF64Ptr(unsafe_from_address=0)
comptime NULL_BUF_I32 = MutBufI32Ptr(unsafe_from_address=0)
comptime NULL_U8 = MutU8Ptr(unsafe_from_address=0)
comptime NULL_I32 = MutI32Ptr(unsafe_from_address=0)

@fieldwise_init
struct GpuBuf[dtype: DType](Movable):
    var buf: UnsafePointer[mut=True, type=DeviceBuffer[Self.dtype], origin=MutAnyOrigin]
    var len: Int

comptime GpuBufF32 = GpuBuf[DType.float32]
comptime GpuBufF64 = GpuBuf[DType.float64]
comptime GpuBufI32 = GpuBuf[DType.int32]

@fieldwise_init
struct GpuIdxPlanF32(Movable):
    var idx_data: DeviceBuffer[DType.int32]
    var idx_offsets: DeviceBuffer[DType.int32]
    var idx_lens: DeviceBuffer[DType.int32]
    var dims: DeviceBuffer[DType.int32]
    var ndim: Int
    var idx_data_n: Int
    var out_n: Int
    var full_n: Int

@fieldwise_init
struct GpuIdxPlanF64(Movable):
    var idx_data: DeviceBuffer[DType.int32]
    var idx_offsets: DeviceBuffer[DType.int32]
    var idx_lens: DeviceBuffer[DType.int32]
    var dims: DeviceBuffer[DType.int32]
    var ndim: Int
    var idx_data_n: Int
    var out_n: Int
    var full_n: Int

@fieldwise_init
struct GpuIdxPlanI32(Movable):
    var idx_data: DeviceBuffer[DType.int32]
    var idx_offsets: DeviceBuffer[DType.int32]
    var idx_lens: DeviceBuffer[DType.int32]
    var dims: DeviceBuffer[DType.int32]
    var ndim: Int
    var idx_data_n: Int
    var out_n: Int
    var full_n: Int

@export("mojor_gpu_ctx_create", ABI="C")
fn mojor_gpu_ctx_create() -> MutCtxPtr:
    if not has_accelerator():
        return NULL_CTX
    try:
        var ctxp: MutCtxPtr = alloc[DeviceContext](1)
        if ctxp == NULL_CTX:
            return NULL_CTX
        try:
            ctxp[0] = DeviceContext(api=MOJOR_GPU_API)
        except:
            ctxp.free()
            return NULL_CTX
        return ctxp
    except:
        return NULL_CTX


@export("mojor_gpu_ctx_free", ABI="C")
fn mojor_gpu_ctx_free(ctxp: MutCtxPtr) -> Int32:
    if ctxp == NULL_CTX:
        return 0
    ctxp.free()
    return 1

@export("mojor_has_gpu", ABI="C")
fn mojor_has_gpu() -> Int32:
    if not has_accelerator():
        return 0
    return 1

@export("mojor_gpu_meminfo", ABI="C")
fn mojor_gpu_meminfo(ctxp: MutCtxPtr, out_ptr: MutOpaqueAny) -> Int32:
    if ctxp == NULL_CTX:
        return -1
    var out: MutU64Ptr = out_ptr.bitcast[UInt64]()
    out[0] = 0
    out[1] = 0
    return -1

comptime MutGpuBufF32Ptr = UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin]
comptime NULL_GPUBUF = MutGpuBufF32Ptr(unsafe_from_address=0)
comptime MutGpuBufF64Ptr = UnsafePointer[mut=True, type=GpuBufF64, origin=MutAnyOrigin]
comptime NULL_GPUBUF_F64 = MutGpuBufF64Ptr(unsafe_from_address=0)
comptime MutGpuBufI32Ptr = UnsafePointer[mut=True, type=GpuBufI32, origin=MutAnyOrigin]
comptime NULL_GPUBUF_I32 = MutGpuBufI32Ptr(unsafe_from_address=0)
comptime MutGpuIdxPlanF32Ptr = UnsafePointer[mut=True, type=GpuIdxPlanF32, origin=MutAnyOrigin]
comptime NULL_GPU_IDXPLAN_F32 = MutGpuIdxPlanF32Ptr(unsafe_from_address=0)
comptime MutGpuIdxPlanF64Ptr = UnsafePointer[mut=True, type=GpuIdxPlanF64, origin=MutAnyOrigin]
comptime NULL_GPU_IDXPLAN_F64 = MutGpuIdxPlanF64Ptr(unsafe_from_address=0)
comptime MutGpuIdxPlanI32Ptr = UnsafePointer[mut=True, type=GpuIdxPlanI32, origin=MutAnyOrigin]
comptime NULL_GPU_IDXPLAN_I32 = MutGpuIdxPlanI32Ptr(unsafe_from_address=0)

fn _null_buf[dtype: DType]() -> UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin]:
    return UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin](unsafe_from_address=0)

fn _null_gpubuf[dtype: DType]() -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    return UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin](unsafe_from_address=0)

fn _null_idxplan_f32() -> MutGpuIdxPlanF32Ptr:
    return NULL_GPU_IDXPLAN_F32

fn _null_idxplan_f64() -> MutGpuIdxPlanF64Ptr:
    return NULL_GPU_IDXPLAN_F64

fn _null_idxplan_i32() -> MutGpuIdxPlanI32Ptr:
    return NULL_GPU_IDXPLAN_I32

fn set_status(ptr: MutOpaqueAny, value: Int32) -> None:
    var out: MutI32Ptr = ptr.bitcast[Int32]()
    out[0] = value

comptime GPU_STATUS_OK: Int32 = 1
comptime GPU_STATUS_EMPTY: Int32 = 0
comptime GPU_STATUS_INVALID: Int32 = -1
comptime GPU_STATUS_ALLOC: Int32 = -2
comptime GPU_STATUS_DEVICE: Int32 = -3
comptime GPU_STATUS_WARP: Int32 = -4
comptime GPU_STATUS_HOST: Int32 = -5
comptime GPU_STATUS_LIMIT: Int32 = -8

fn _set_status_ok(status_ptr: MutOpaqueAny) -> None:
    set_status(status_ptr, GPU_STATUS_OK)


fn _set_status_invalid(status_ptr: MutOpaqueAny) -> None:
    set_status(status_ptr, GPU_STATUS_INVALID)


fn _gpu_transform_precheck[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    status_ptr: MutOpaqueAny,
    bytes_per: Int,
    buffers: Int,
) -> Bool:
    if handle == _null_gpubuf[dtype]() or not gpu_ready(ctxp):
        _set_status_invalid(status_ptr)
        return False
    var n_i = handle[0].len
    if n_i <= 0:
        set_status(status_ptr, GPU_STATUS_EMPTY)
        return False
    if not gpu_limit_ok(n_i, buffers, bytes_per):
        set_status(status_ptr, GPU_STATUS_LIMIT)
        return False
    return True


fn _gpu_status_from_stage(stage: Int32) -> Int32:
    if stage == 3:
        return GPU_STATUS_DEVICE
    return GPU_STATUS_ALLOC


fn _gpu_status_from_chain_sum_stage(stage: Int32) -> Int32:
    if stage == 3:
        return GPU_STATUS_DEVICE
    if stage == 4:
        return GPU_STATUS_WARP
    if stage == 5:
        return GPU_STATUS_HOST
    return GPU_STATUS_ALLOC


@fieldwise_init
struct GpuBufCopyInfo[dtype: DType](Movable):
    var ok: Bool
    var n_i: Int
    var bufp: UnsafePointer[mut=True, type=DeviceBuffer[Self.dtype], origin=MutAnyOrigin]


fn _gpu_buf_copy_precheck[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    n: Int32
) -> GpuBufCopyInfo[dtype]:
    if handle == _null_gpubuf[dtype]() or not gpu_ready(ctxp):
        return GpuBufCopyInfo[dtype](ok=False, n_i=0, bufp=_null_buf[dtype]())
    var n_i = Int(n)
    if n_i != handle[0].len:
        return GpuBufCopyInfo[dtype](ok=False, n_i=0, bufp=_null_buf[dtype]())
    var bufp = handle[0].buf
    if bufp == _null_buf[dtype]():
        return GpuBufCopyInfo[dtype](ok=False, n_i=0, bufp=_null_buf[dtype]())
    return GpuBufCopyInfo[dtype](ok=True, n_i=n_i, bufp=bufp)


fn _gpu_copy_host_to_device[dtype: DType](
    ctx: DeviceContext,
    dev_buf: DeviceBuffer[dtype],
    host_ptr: ImmutOpaqueAny,
    n_i: Int
) raises:
    var host = ctx.enqueue_create_host_buffer[dtype](n_i)
    var src: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin] = host_ptr.bitcast[Scalar[dtype]]()
    for i in range(n_i):
        host[i] = src[i]
    ctx.enqueue_copy(dst_buf=dev_buf, src_buf=host)
    ctx.synchronize()


fn _gpu_copy_device_to_host[dtype: DType](
    ctx: DeviceContext,
    dev_buf: DeviceBuffer[dtype],
    host_ptr: MutOpaqueAny,
    n_i: Int
) raises:
    var host = ctx.enqueue_create_host_buffer[dtype](n_i)
    ctx.enqueue_copy(dst_buf=host, src_buf=dev_buf)
    ctx.synchronize()
    var dst: UnsafePointer[mut=True, type=Scalar[dtype], origin=MutAnyOrigin] = host_ptr.bitcast[Scalar[dtype]]()
    for i in range(n_i):
        dst[i] = host[i]


fn gpu_ready(ctxp: MutCtxPtr) -> Bool:
    if ctxp == NULL_CTX:
        return False
    return True

fn _gpu_input_buf_or_invalid[dtype: DType](
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    status_ptr: MutOpaqueAny
) -> UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin]:
    var in_bufp = handle[0].buf
    if in_bufp == _null_buf[dtype]():
        _set_status_invalid(status_ptr)
        return _null_buf[dtype]()
    return in_bufp

fn _gpu_clone_device_buffer[dtype: DType](
    ctx: DeviceContext,
    src_buf: DeviceBuffer[dtype],
    n_i: Int
) raises -> DeviceBuffer[dtype]:
    var dev_out = ctx.enqueue_create_buffer[dtype](n_i)
    ctx.enqueue_copy(dst_buf=dev_out, src_buf=src_buf)
    return dev_out

fn _gpu_box_device_buffer[dtype: DType](
    dev_buf: DeviceBuffer[dtype]
) -> UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin]:
    var bufp: UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin] = alloc[DeviceBuffer[dtype]](1)
    bufp[0] = dev_buf
    return bufp

fn _gpu_wrap_gpubuf[dtype: DType](
    bufp: UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin],
    n_i: Int
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    var ptr: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin] = alloc[GpuBuf[dtype]](1)
    ptr[0] = GpuBuf[dtype](bufp, n_i)
    return ptr

fn _gpu_buf_alloc[dtype: DType](ctxp: MutCtxPtr, n: Int32, bytes_per: Int) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    if not gpu_ready(ctxp):
        return _null_gpubuf[dtype]()
    var n_i = Int(n)
    if n_i < 0:
        return _null_gpubuf[dtype]()
    if not gpu_limit_ok(n_i, 1, bytes_per):
        return _null_gpubuf[dtype]()
    try:
        var dev_buf = ctxp[0].enqueue_create_buffer[dtype](n_i)
        var bufp = _gpu_box_device_buffer[dtype](dev_buf)
        return _gpu_wrap_gpubuf[dtype](bufp, n_i)
    except:
        return _null_gpubuf[dtype]()

fn _gpu_buf_free[dtype: DType](handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]) -> Int32:
    if handle == _null_gpubuf[dtype]():
        return 0
    var bufp = handle[0].buf
    if bufp != _null_buf[dtype]():
        bufp.free()
    handle.free()
    return 1

fn _gpu_buf_len[dtype: DType](handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]) -> Int32:
    if handle == _null_gpubuf[dtype]():
        return -1
    return Int32(handle[0].len)

fn _gpu_buf_ptr[dtype: DType](handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]) -> UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin]:
    if handle == _null_gpubuf[dtype]():
        return _null_buf[dtype]()
    var bufp = handle[0].buf
    if bufp == _null_buf[dtype]():
        return _null_buf[dtype]()
    return bufp

fn _gpu_buf_write[dtype: DType](ctxp: MutCtxPtr, handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin], host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    var info = _gpu_buf_copy_precheck[dtype](ctxp, handle, n)
    if not info.ok:
        return 0
    try:
        _gpu_copy_host_to_device[dtype](ctxp[0], info.bufp[0], host_ptr, info.n_i)
        return 1
    except:
        return 0

fn _gpu_buf_read[dtype: DType](ctxp: MutCtxPtr, handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin], host_ptr: MutOpaqueAny, n: Int32) -> Int32:
    var info = _gpu_buf_copy_precheck[dtype](ctxp, handle, n)
    if not info.ok:
        return 0
    try:
        _gpu_copy_device_to_host[dtype](ctxp[0], info.bufp[0], host_ptr, info.n_i)
        return 1
    except:
        return 0

fn _gpu_buf_cast_limit_ok(n_i: Int, in_bytes: Int, out_bytes: Int) -> Bool:
    if n_i <= 0:
        return True
    var total: Int64 = Int64(n_i) * (Int64(in_bytes) + Int64(out_bytes))
    return total <= GPU_MAX_BYTES

fn _gpu_buf_cast[in_dtype: DType, out_dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[in_dtype], origin=MutAnyOrigin],
    in_bytes: Int,
    out_bytes: Int
) -> UnsafePointer[mut=True, type=GpuBuf[out_dtype], origin=MutAnyOrigin]:
    if not gpu_ready(ctxp) or handle == _null_gpubuf[in_dtype]():
        return _null_gpubuf[out_dtype]()
    var n_i = handle[0].len
    if n_i < 0:
        return _null_gpubuf[out_dtype]()
    if not _gpu_buf_cast_limit_ok(n_i, in_bytes, out_bytes):
        return _null_gpubuf[out_dtype]()

    var in_bufp = handle[0].buf
    if in_bufp == _null_buf[in_dtype]():
        return _null_gpubuf[out_dtype]()

    try:
        var ctx = ctxp[0]
        var src_host = ctx.enqueue_create_host_buffer[in_dtype](n_i)
        ctx.enqueue_copy(dst_buf=src_host, src_buf=in_bufp[0])
        ctx.synchronize()

        var dst_host = ctx.enqueue_create_host_buffer[out_dtype](n_i)
        for i in range(n_i):
            dst_host[i] = Scalar[out_dtype](src_host[i])

        var dst_dev = ctx.enqueue_create_buffer[out_dtype](n_i)
        ctx.enqueue_copy(dst_buf=dst_dev, src_buf=dst_host)
        ctx.synchronize()

        var out_bufp = _gpu_box_device_buffer[out_dtype](dst_dev)
        return _gpu_wrap_gpubuf[out_dtype](out_bufp, n_i)
    except:
        return _null_gpubuf[out_dtype]()

fn _gpu_buf_affine[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    status_ptr: MutOpaqueAny,
    bytes_per: Int,
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    if not _gpu_transform_precheck[dtype](ctxp, handle, status_ptr, bytes_per, 2):
        return _null_gpubuf[dtype]()
    var n_i = handle[0].len
    var stage: Int32 = 0
    try:
        var ctx = ctxp[0]
        var in_bufp = _gpu_input_buf_or_invalid[dtype](handle, status_ptr)
        if in_bufp == _null_buf[dtype]():
            return _null_gpubuf[dtype]()
        stage = 2
        var dev_out = _gpu_clone_device_buffer[dtype](ctx, in_bufp[0], n_i)
        stage = 3
        var out_bufp = _gpu_box_device_buffer[dtype](dev_out)
        _enqueue_affine[dtype](ctx, out_bufp[0], n_i, scale, bias)
        ctx.synchronize()
        _set_status_ok(status_ptr)
        return _gpu_wrap_gpubuf[dtype](out_bufp, n_i)
    except:
        set_status(status_ptr, _gpu_status_from_stage(stage))
        return _null_gpubuf[dtype]()

fn _gpu_buf_chain[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    iters: Int32,
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    post_scale: Scalar[dtype],
    post_bias: Scalar[dtype],
    post_iters: Int32,
    status_ptr: MutOpaqueAny,
    bytes_per: Int,
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    if not _gpu_transform_precheck[dtype](ctxp, handle, status_ptr, bytes_per, 2):
        return _null_gpubuf[dtype]()
    var n_i = handle[0].len
    var stage: Int32 = 0
    try:
        var ctx = ctxp[0]
        var in_bufp = _gpu_input_buf_or_invalid[dtype](handle, status_ptr)
        if in_bufp == _null_buf[dtype]():
            return _null_gpubuf[dtype]()
        stage = 2
        var dev_out = _gpu_clone_device_buffer[dtype](ctx, in_bufp[0], n_i)
        stage = 3
        var out_bufp = _gpu_box_device_buffer[dtype](dev_out)
        gpu_chain_inplace[dtype](ctx, out_bufp, n_i, iters, scale, bias, post_scale, post_bias, post_iters)
        ctx.synchronize()
        _set_status_ok(status_ptr)
        return _gpu_wrap_gpubuf[dtype](out_bufp, n_i)
    except:
        set_status(status_ptr, _gpu_status_from_stage(stage))
        return _null_gpubuf[dtype]()

fn _gpu_buf_chain_sum[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    iters: Int32,
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    post_scale: Scalar[dtype],
    post_bias: Scalar[dtype],
    post_iters: Int32,
    status_ptr: MutOpaqueAny,
    bytes_per: Int,
    use_warp: Bool,
) -> Scalar[dtype]:
    _ = use_warp
    if not _gpu_transform_precheck[dtype](ctxp, handle, status_ptr, bytes_per, 3):
        return _zero[dtype]()
    var n_i = handle[0].len
    var stage: Int32 = 0
    try:
        var ctx = ctxp[0]
        var in_bufp = _gpu_input_buf_or_invalid[dtype](handle, status_ptr)
        if in_bufp == _null_buf[dtype]():
            return _zero[dtype]()
        stage = 2
        var dev_work = _gpu_clone_device_buffer[dtype](ctx, in_bufp[0], n_i)
        var work_bufp = _gpu_box_device_buffer[dtype](dev_work)
        stage = 3
        gpu_chain_inplace[dtype](ctx, work_bufp, n_i, iters, scale, bias, post_scale, post_bias, post_iters)
        stage = 4
        var host_out = ctx.enqueue_create_host_buffer[dtype](n_i)
        ctx.enqueue_copy(dst_buf=host_out, src_buf=work_bufp[0])
        ctx.synchronize()
        var total = _zero[dtype]()
        for i in range(n_i):
            total += host_out[i]
        _set_status_ok(status_ptr)
        return total
    except:
        set_status(status_ptr, _gpu_status_from_chain_sum_stage(stage))
        return _zero[dtype]()

fn gpu_limit_ok(n_i: Int, buffers: Int, bytes_per: Int) -> Bool:
    if n_i <= 0:
        return True
    var total: Int64 = Int64(n_i) * Int64(buffers) * Int64(bytes_per)
    return total <= GPU_MAX_BYTES

fn gpu_limit_ok_matmul(m: Int, k: Int, n: Int, bytes_per: Int) -> Bool:
    if m <= 0 or n <= 0 or k <= 0:
        return False
    var total: Int64 = (
        Int64(m) * Int64(k)
        + Int64(k) * Int64(n)
        + Int64(m) * Int64(n)
    ) * Int64(bytes_per)
    return total <= GPU_MAX_BYTES

fn _zero[dtype: DType]() -> Scalar[dtype]:
    var out: Scalar[dtype] = 0
    return out

fn _sigmoid_f64(v: Float64) -> Float64:
    return 1.0 / (1.0 + exp(-v))

fn _sigmoid_f32(v: Float32) -> Float32:
    var one: Float32 = 1
    return one / (one + exp(-v))

fn _chain_f32_value(
    v: Float32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32
) -> Float32:
    var y = v
    var rounds = iters
    if rounds < 1:
        rounds = 1
    for _ in range(Int(rounds)):
        y = _sigmoid_f32(y) * scale + bias
    var post_rounds = post_iters
    if post_rounds < 1:
        post_rounds = 0
    for _ in range(Int(post_rounds)):
        y = y * post_scale + post_bias
    return y

fn _enqueue_affine[dtype: DType](ctx: DeviceContext, buf: DeviceBuffer[dtype], n_i: Int, scale: Scalar[dtype], bias: Scalar[dtype]) raises:
    var host = ctx.enqueue_create_host_buffer[dtype](n_i)
    ctx.enqueue_copy(dst_buf=host, src_buf=buf)
    ctx.synchronize()
    for i in range(n_i):
        host[i] = host[i] * scale + bias
    ctx.enqueue_copy(dst_buf=buf, src_buf=host)
    ctx.synchronize()

fn _enqueue_sigmoid_affine[dtype: DType](ctx: DeviceContext, buf: DeviceBuffer[dtype], n_i: Int, scale: Scalar[dtype], bias: Scalar[dtype]) raises:
    var host = ctx.enqueue_create_host_buffer[dtype](n_i)
    ctx.enqueue_copy(dst_buf=host, src_buf=buf)
    ctx.synchronize()
    for i in range(n_i):
        var v = host[i]
        var one: Scalar[dtype] = 1
        var s = one / (one + exp(-v))
        host[i] = s * scale + bias
    ctx.enqueue_copy(dst_buf=buf, src_buf=host)
    ctx.synchronize()

fn gpu_chain_inplace[dtype: DType](
    ctx: DeviceContext,
    bufp: UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin],
    n_i: Int,
    iters: Int32,
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    post_scale: Scalar[dtype],
    post_bias: Scalar[dtype],
    post_iters: Int32,
) raises:
    if bufp == _null_buf[dtype]():
        return
    var rounds = Int(iters)
    if rounds < 1:
        rounds = 1
    for _ in range(rounds):
        _enqueue_sigmoid_affine[dtype](ctx, bufp[0], n_i, scale, bias)
    var post_rounds = Int(post_iters)
    if post_rounds < 1:
        post_rounds = 0
    if post_rounds > 0:
        for _ in range(post_rounds):
            _enqueue_affine[dtype](ctx, bufp[0], n_i, post_scale, post_bias)

fn sum_simd_width[width: Int](data: ImmutF64Ptr, n_i: Int) -> Float64:
    var total: Float64 = 0.0
    var simd_end = n_i - (n_i % width)
    for i in range(0, simd_end, width):
        var chunk = data.load[width=width](i)
        total += chunk.reduce_add()

    for i in range(simd_end, n_i):
        total += data[i]

    return total

fn pairwise_sum[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], start: Int, end: Int) -> Scalar[dtype]:
    var n = end - start
    if n <= 0:
        return 0.0
    if n <= 1024:
        var acc: Scalar[dtype] = 0.0
        for i in range(start, end):
            acc += data[i]
        return acc
    var mid = start + n // 2
    return pairwise_sum[dtype](data, start, mid) + pairwise_sum[dtype](data, mid, end)


@export("mojor_sum_f64_pairwise", ABI="C")
fn mojor_sum_f64_pairwise(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    return pairwise_sum[DType.float64](data, 0, n_i)

@export("mojor_sum_f32_pairwise", ABI="C")
fn mojor_sum_f32_pairwise(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    return pairwise_sum[DType.float32](data, 0, n_i)

fn sum_simd_unrolled4[width: Int](data: ImmutF64Ptr, n_i: Int) -> Float64:
    var total: Float64 = 0.0
    if n_i < width:
        for i in range(n_i):
            total += data[i]
        return total
    var acc0 = data.load[width=width](0)
    acc0 = acc0 - acc0
    var acc1 = acc0
    var acc2 = acc0
    var acc3 = acc0
    var step = width * 4
    var simd_end = n_i - (n_i % step)
    for i in range(0, simd_end, step):
        acc0 += data.load[width=width](i)
        acc1 += data.load[width=width](i + width)
        acc2 += data.load[width=width](i + 2 * width)
        acc3 += data.load[width=width](i + 3 * width)
    total = (acc0 + acc1 + acc2 + acc3).reduce_add()
    for i in range(simd_end, n_i):
        total += data[i]
    return total

fn sum_simd_unrolled8[width: Int](data: ImmutF64Ptr, n_i: Int) -> Float64:
    var total: Float64 = 0.0
    if n_i < width:
        for i in range(n_i):
            total += data[i]
        return total
    var acc0 = data.load[width=width](0)
    acc0 = acc0 - acc0
    var acc1 = acc0
    var acc2 = acc0
    var acc3 = acc0
    var acc4 = acc0
    var acc5 = acc0
    var acc6 = acc0
    var acc7 = acc0
    var step = width * 8
    var simd_end = n_i - (n_i % step)
    for i in range(0, simd_end, step):
        acc0 += data.load[width=width](i)
        acc1 += data.load[width=width](i + width)
        acc2 += data.load[width=width](i + 2 * width)
        acc3 += data.load[width=width](i + 3 * width)
        acc4 += data.load[width=width](i + 4 * width)
        acc5 += data.load[width=width](i + 5 * width)
        acc6 += data.load[width=width](i + 6 * width)
        acc7 += data.load[width=width](i + 7 * width)
    total = (acc0 + acc1 + acc2 + acc3 + acc4 + acc5 + acc6 + acc7).reduce_add()
    for i in range(simd_end, n_i):
        total += data[i]
    return total

fn _nan_value[dtype: DType]() -> Scalar[dtype]:
    @parameter
    if dtype == DType.float64:
        return Scalar[dtype](Float64(0.0/0.0))
    return Scalar[dtype](Float32(0.0/0.0))

fn sum_manual[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    var total: Scalar[dtype] = 0.0
    for i in range(n_i):
        total += data[i]
    return total

fn sum_fallback[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    @parameter
    if dtype == DType.float64:
        var data64: ImmutF64Ptr = data.bitcast[Scalar[DType.float64]]()
        return Scalar[dtype](sum_simd_unrolled8[f64_simd_width](data64, n_i))
    return sum_manual[dtype](data, n_i)

fn prod_fallback[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    var prod: Scalar[dtype] = 1.0
    for i in range(n_i):
        prod *= data[i]
    return prod

fn min_fallback[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    if n_i <= 0:
        return _nan_value[dtype]()
    var v = data[0]
    for i in range(1, n_i):
        if data[i] < v:
            v = data[i]
    return v

fn max_fallback[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    if n_i <= 0:
        return _nan_value[dtype]()
    var v = data[0]
    for i in range(1, n_i):
        if data[i] > v:
            v = data[i]
    return v

fn mean_fallback[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    if n_i <= 0:
        return _nan_value[dtype]()
    return sum_fallback[dtype](data, n_i) / Scalar[dtype](n_i)

comptime REDUCE_SUM = 0
comptime REDUCE_MIN = 1
comptime REDUCE_MAX = 2
comptime REDUCE_MEAN = 3
comptime REDUCE_PROD = 4
comptime REDUCE_ARGMIN = 5
comptime REDUCE_ARGMAX = 6

fn reduce_nomiss[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int, op: Int32, use_manual_sum: Bool) -> Scalar[dtype]:
    var span = Span[Scalar[dtype], origin=ImmutAnyOrigin](ptr=data, length=n_i)
    if op == REDUCE_SUM:
        if use_manual_sum:
            return sum_manual[dtype](data, n_i)
        try:
            return algo_sum[dtype](span)
        except:
            return sum_fallback[dtype](data, n_i)
    if op == REDUCE_MIN:
        try:
            return algo_min[dtype](span)
        except:
            return min_fallback[dtype](data, n_i)
    if op == REDUCE_MAX:
        try:
            return algo_max[dtype](span)
        except:
            return max_fallback[dtype](data, n_i)
    if op == REDUCE_PROD:
        try:
            return algo_product[dtype](span)
        except:
            return prod_fallback[dtype](data, n_i)
    try:
        return algo_mean[dtype](span)
    except:
        return mean_fallback[dtype](data, n_i)

fn which_nomiss[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int, want_min: Bool) -> Int32:
    if n_i <= 0:
        return Int32(0)
    var best = data[0]
    var best_idx = Int32(1)
    for i in range(1, n_i):
        var v = data[i]
        if (want_min and v < best) or ((not want_min) and v > best):
            best = v
            best_idx = Int32(i + 1)
    return best_idx

fn var_nomiss[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    if n_i < 2:
        return _nan_value[dtype]()
    var mean: Scalar[dtype] = 0.0
    var m2: Scalar[dtype] = 0.0
    var count = Int(0)
    for i in range(n_i):
        var v = data[i]
        count += 1
        var delta = v - mean
        mean += delta / Scalar[dtype](count)
        var delta2 = v - mean
        m2 += delta * delta2
    return m2 / Scalar[dtype](count - 1)

fn var_nomiss_twopass[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    if n_i < 2:
        return _nan_value[dtype]()
    var mean = reduce_nomiss[dtype](data, n_i, REDUCE_SUM, False) / Scalar[dtype](n_i)
    var acc: Scalar[dtype] = 0.0
    for i in range(n_i):
        var d = data[i] - mean
        acc += d * d
    return acc / Scalar[dtype](n_i - 1)

fn sd_nomiss[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    return sqrt(var_nomiss[dtype](data, n_i))

fn sd_nomiss_twopass[dtype: DType](data: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin], n_i: Int) -> Scalar[dtype]:
    return sqrt(var_nomiss_twopass[dtype](data, n_i))

# BEGIN GENERATED REDUCE_EXPORTS
# NOTE: generated by tools/gen_reduce_exports.mojo
@export("mojor_sum_f64", ABI="C")
fn mojor_sum_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_SUM, False)

@export("mojor_sum_f64_std", ABI="C")
fn mojor_sum_f64_std(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_SUM, False)

@export("mojor_sum_f64_nomiss", ABI="C")
fn mojor_sum_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_SUM, False)

@export("mojor_sum_f64_nomiss_manual", ABI="C")
fn mojor_sum_f64_nomiss_manual(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_SUM, True)

@export("mojor_abs_f64", ABI="C")
fn mojor_abs_f64(ptr: MutOpaqueAny, n: Int32) -> None:
    var data: MutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    for i in range(n_i):
        var v = data[i]
        if v < 0.0:
            data[i] = -v
        else:
            data[i] = v

@export("mojor_prod_f64", ABI="C")
fn mojor_prod_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_PROD, False)

@export("mojor_prod_f64_nomiss", ABI="C")
fn mojor_prod_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_PROD, False)

@export("mojor_min_f64", ABI="C")
fn mojor_min_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MIN, False)

@export("mojor_min_f64_nomiss", ABI="C")
fn mojor_min_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MIN, False)

@export("mojor_max_f64", ABI="C")
fn mojor_max_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MAX, False)

@export("mojor_max_f64_nomiss", ABI="C")
fn mojor_max_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MAX, False)

@export("mojor_mean_f64", ABI="C")
fn mojor_mean_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MEAN, False)

@export("mojor_mean_f64_nomiss", ABI="C")
fn mojor_mean_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return reduce_nomiss[DType.float64](data, Int(n), REDUCE_MEAN, False)

@export("mojor_which_min_f64_nomiss", ABI="C")
fn mojor_which_min_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return which_nomiss[DType.float64](data, Int(n), True)

@export("mojor_which_max_f64_nomiss", ABI="C")
fn mojor_which_max_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return which_nomiss[DType.float64](data, Int(n), False)

@export("mojor_sd_f64_nomiss", ABI="C")
fn mojor_sd_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return sd_nomiss[DType.float64](data, Int(n))

@export("mojor_sd_f64_nomiss_twopass", ABI="C")
fn mojor_sd_f64_nomiss_twopass(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return sd_nomiss_twopass[DType.float64](data, Int(n))

@export("mojor_var_f64_nomiss", ABI="C")
fn mojor_var_f64_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return var_nomiss[DType.float64](data, Int(n))

@export("mojor_var_f64_nomiss_twopass", ABI="C")
fn mojor_var_f64_nomiss_twopass(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    var data: ImmutF64Ptr = ptr.bitcast[Scalar[DType.float64]]()
    return var_nomiss_twopass[DType.float64](data, Int(n))

@export("mojor_sum_f32", ABI="C")
fn mojor_sum_f32(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_SUM, False)

@export("mojor_sum_f32_std", ABI="C")
fn mojor_sum_f32_std(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_SUM, False)

@export("mojor_sum_f32_nomiss", ABI="C")
fn mojor_sum_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_SUM, False)

@export("mojor_prod_f32", ABI="C")
fn mojor_prod_f32(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_PROD, False)

@export("mojor_prod_f32_nomiss", ABI="C")
fn mojor_prod_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_PROD, False)

@export("mojor_min_f32", ABI="C")
fn mojor_min_f32(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MIN, False)

@export("mojor_min_f32_nomiss", ABI="C")
fn mojor_min_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MIN, False)

@export("mojor_max_f32", ABI="C")
fn mojor_max_f32(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MAX, False)

@export("mojor_max_f32_nomiss", ABI="C")
fn mojor_max_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MAX, False)

@export("mojor_mean_f32", ABI="C")
fn mojor_mean_f32(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MEAN, False)

@export("mojor_mean_f32_nomiss", ABI="C")
fn mojor_mean_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return reduce_nomiss[DType.float32](data, Int(n), REDUCE_MEAN, False)

@export("mojor_which_min_f32_nomiss", ABI="C")
fn mojor_which_min_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return which_nomiss[DType.float32](data, Int(n), True)

@export("mojor_which_max_f32_nomiss", ABI="C")
fn mojor_which_max_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return which_nomiss[DType.float32](data, Int(n), False)

@export("mojor_sd_f32_nomiss", ABI="C")
fn mojor_sd_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return sd_nomiss[DType.float32](data, Int(n))

@export("mojor_sd_f32_nomiss_twopass", ABI="C")
fn mojor_sd_f32_nomiss_twopass(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return sd_nomiss_twopass[DType.float32](data, Int(n))

@export("mojor_var_f32_nomiss", ABI="C")
fn mojor_var_f32_nomiss(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return var_nomiss[DType.float32](data, Int(n))

@export("mojor_var_f32_nomiss_twopass", ABI="C")
fn mojor_var_f32_nomiss_twopass(ptr: ImmutOpaqueAny, n: Int32) -> Float32:
    var data: ImmutF32Ptr = ptr.bitcast[Scalar[DType.float32]]()
    return var_nomiss_twopass[DType.float32](data, Int(n))

# END GENERATED REDUCE_EXPORTS

@export("mojor_scale_f64", ABI="C")
fn mojor_scale_f64(x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32, scalar: Float64) -> None:
    var x: ImmutF64Ptr = x_ptr.bitcast[Scalar[DType.float64]]()
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = x[i] * scalar

@export("mojor_running_max_f64", ABI="C")
fn mojor_running_max_f64(x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32) -> None:
    var x: ImmutF64Ptr = x_ptr.bitcast[Scalar[DType.float64]]()
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    if n_i <= 0:
        return
    var current = x[0]
    out[0] = current
    for i in range(1, n_i):
        var v = x[i]
        if v > current:
            current = v
        out[i] = current

@export("mojor_count_runs_f64", ABI="C")
fn mojor_count_runs_f64(x_ptr: ImmutOpaqueAny, n: Int32, threshold: Float64, out_ptr: MutOpaqueAny) -> None:
    var x: ImmutF64Ptr = x_ptr.bitcast[Scalar[DType.float64]]()
    var out: MutI32Ptr = out_ptr.bitcast[Int32]()
    var n_i = Int(n)
    var runs: Int32 = 0
    var longest: Int32 = 0
    var current: Int32 = 0
    for i in range(n_i):
        if x[i] > threshold:
            current += 1
        else:
            if current > 0:
                runs += 1
                if current > longest:
                    longest = current
                current = 0
    if current > 0:
        runs += 1
        if current > longest:
            longest = current
    out[0] = runs
    out[1] = longest

# BEGIN GENERATED GPU_SIGMOID_EXPORTS
# NOTE: generated by tools/gen_gpu_sigmoid_exports.mojo
@export("mojor_sigmoid_f64", ABI="C")
fn mojor_sigmoid_f64(x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32) -> None:
    var x: ImmutF64Ptr = x_ptr.bitcast[Scalar[DType.float64]]()
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = _sigmoid_f64(x[i])

@export("mojor_sigmoid_f64_gpu", ABI="C")
fn mojor_sigmoid_f64_gpu(ctxp: MutCtxPtr, x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32) -> Int32:
    if not gpu_ready(ctxp):
        return -1
    mojor_sigmoid_f64(x_ptr, out_ptr, n)
    return 1

@export("mojor_sigmoid_f32_gpu", ABI="C")
fn mojor_sigmoid_f32_gpu(ctxp: MutCtxPtr, x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32) -> Int32:
    if not gpu_ready(ctxp):
        return -1
    var x: ImmutF32Ptr = x_ptr.bitcast[Scalar[DType.float32]]()
    var out: MutF32Ptr = out_ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = _sigmoid_f32(x[i])
    return 1

@export("mojor_sigmoid_affine_f32_gpu", ABI="C")
fn mojor_sigmoid_affine_f32_gpu(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    out_ptr: MutOpaqueAny,
    n: Int32,
    scale: Float32,
    bias: Float32
) -> Int32:
    if not gpu_ready(ctxp):
        return -1
    var x: ImmutF32Ptr = x_ptr.bitcast[Scalar[DType.float32]]()
    var out: MutF32Ptr = out_ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = _sigmoid_f32(x[i]) * scale + bias
    return 1

@export("mojor_sigmoid_f32_gpu_iters", ABI="C")
fn mojor_sigmoid_f32_gpu_iters(ctxp: MutCtxPtr, x_ptr: ImmutOpaqueAny, out_ptr: MutOpaqueAny, n: Int32, iters: Int32) -> Int32:
    _ = iters
    return mojor_sigmoid_f32_gpu(ctxp, x_ptr, out_ptr, n)

@export("mojor_sigmoid_affine_f32_gpu_iters", ABI="C")
fn mojor_sigmoid_affine_f32_gpu_iters(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    out_ptr: MutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32
) -> Int32:
    _ = iters
    return mojor_sigmoid_affine_f32_gpu(ctxp, x_ptr, out_ptr, n, scale, bias)

@export("mojor_sigmoid_affine_f32_gpu_chain", ABI="C")
fn mojor_sigmoid_affine_f32_gpu_chain(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    out_ptr: MutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32
) -> Int32:
    if not gpu_ready(ctxp):
        return -1
    var x: ImmutF32Ptr = x_ptr.bitcast[Scalar[DType.float32]]()
    var out: MutF32Ptr = out_ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = _chain_f32_value(x[i], iters, scale, bias, post_scale, post_bias, post_iters)
    return 1

@export("mojor_sigmoid_affine_f32_gpu_chain_sum", ABI="C")
fn mojor_sigmoid_affine_f32_gpu_chain_sum(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float32:
    if not gpu_ready(ctxp):
        set_status(status_ptr, -1)
        return 0
    var x: ImmutF32Ptr = x_ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    var total: Float32 = 0
    for i in range(n_i):
        total += _chain_f32_value(x[i], iters, scale, bias, post_scale, post_bias, post_iters)
    set_status(status_ptr, 1)
    return total

@export("mojor_sigmoid_affine_f32_gpu_chain_sum_warp", ABI="C")
fn mojor_sigmoid_affine_f32_gpu_chain_sum_warp(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float32:
    return mojor_sigmoid_affine_f32_gpu_chain_sum(
        ctxp,
        x_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        status_ptr
    )
# END GENERATED GPU_SIGMOID_EXPORTS

@export("mojor_gpu_session_create", ABI="C")
fn mojor_gpu_session_create(ctxp: MutCtxPtr, n: Int32) -> Int32:
    _ = n
    if ctxp == NULL_CTX:
        return -1
    return 1

@export("mojor_gpu_session_free", ABI="C")
fn mojor_gpu_session_free(ctxp: MutCtxPtr) -> Int32:
    if ctxp == NULL_CTX:
        return 0
    return 1

@export("mojor_gpu_session_chain_f32", ABI="C")
fn mojor_gpu_session_chain_f32(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    out_ptr: MutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32
) -> Int32:
    if ctxp == NULL_CTX:
        return -1
    var x: ImmutF32Ptr = x_ptr.bitcast[Scalar[DType.float32]]()
    var out: MutF32Ptr = out_ptr.bitcast[Scalar[DType.float32]]()
    var n_i = Int(n)
    for i in range(n_i):
        out[i] = _chain_f32_value(x[i], iters, scale, bias, post_scale, post_bias, post_iters)
    return 1

@export("mojor_gpu_session_chain_sum_f32", ABI="C")
fn mojor_gpu_session_chain_sum_f32(
    ctxp: MutCtxPtr,
    x_ptr: ImmutOpaqueAny,
    n: Int32,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
    ) -> Float32:
    return mojor_sigmoid_affine_f32_gpu_chain_sum(
        ctxp,
        x_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        status_ptr
    )

comptime GPU_BUF_BYTES_F32: Int = 4
comptime GPU_BUF_BYTES_F64: Int = 8
comptime GPU_BUF_BYTES_I32: Int = 4

fn _gpu_buf_export_alloc[dtype: DType](
    ctxp: MutCtxPtr,
    n: Int32,
    bytes_per: Int
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    return _gpu_buf_alloc[dtype](ctxp, n, bytes_per)

fn _gpu_buf_export_free[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]
) -> Int32:
    _ = ctxp
    return _gpu_buf_free[dtype](handle)

fn _gpu_buf_export_len[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]
) -> Int32:
    _ = ctxp
    return _gpu_buf_len[dtype](handle)

fn _gpu_buf_export_write[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    host_ptr: ImmutOpaqueAny,
    n: Int32
) -> Int32:
    return _gpu_buf_write[dtype](ctxp, handle, host_ptr, n)

fn _gpu_buf_export_read[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    host_ptr: MutOpaqueAny,
    n: Int32
) -> Int32:
    return _gpu_buf_read[dtype](ctxp, handle, host_ptr, n)

fn _gpu_buf_export_ptr[dtype: DType](
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]
) -> UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin]:
    return _gpu_buf_ptr[dtype](handle)

fn _gpu_buf_export_affine[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    status_ptr: MutOpaqueAny,
    bytes_per: Int
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    return _gpu_buf_affine[dtype](ctxp, handle, scale, bias, status_ptr, bytes_per)

fn _gpu_buf_export_chain[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    iters: Int32,
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    post_scale: Scalar[dtype],
    post_bias: Scalar[dtype],
    post_iters: Int32,
    status_ptr: MutOpaqueAny,
    bytes_per: Int
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    return _gpu_buf_chain[dtype](
        ctxp,
        handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        status_ptr,
        bytes_per
    )

fn _gpu_buf_export_chain_sum[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    iters: Int32,
    scale: Scalar[dtype],
    bias: Scalar[dtype],
    post_scale: Scalar[dtype],
    post_bias: Scalar[dtype],
    post_iters: Int32,
    status_ptr: MutOpaqueAny,
    bytes_per: Int,
    use_warp: Bool
) -> Scalar[dtype]:
    return _gpu_buf_chain_sum[dtype](
        ctxp,
        handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        status_ptr,
        bytes_per,
        use_warp
    )

# BEGIN GENERATED GPU_BUF_EXPORTS
# NOTE: generated by tools/gen_gpu_buf_exports.mojo
@export("mojor_gpu_buf_f32_alloc", ABI="C")
fn mojor_gpu_buf_f32_alloc(ctxp: MutCtxPtr, n: Int32) -> MutGpuBufF32Ptr:
    return _gpu_buf_export_alloc[DType.float32](ctxp, n, GPU_BUF_BYTES_F32)

@export("mojor_gpu_buf_f32_free", ABI="C")
fn mojor_gpu_buf_f32_free(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr) -> Int32:
    return _gpu_buf_export_free[DType.float32](ctxp, handle)

@export("mojor_gpu_buf_f32_len", ABI="C")
fn mojor_gpu_buf_f32_len(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr) -> Int32:
    return _gpu_buf_export_len[DType.float32](ctxp, handle)

@export("mojor_gpu_buf_f32_write", ABI="C")
fn mojor_gpu_buf_f32_write(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr, host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_write[DType.float32](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_f32_read", ABI="C")
fn mojor_gpu_buf_f32_read(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr, host_ptr: MutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_read[DType.float32](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_f32_ptr", ABI="C")
fn mojor_gpu_buf_f32_ptr(handle: MutGpuBufF32Ptr) -> MutBufF32Ptr:
    return _gpu_buf_export_ptr[DType.float32](handle)

@export("mojor_gpu_buf_f32_affine", ABI="C")
fn mojor_gpu_buf_f32_affine(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    scale: Float32,
    bias: Float32,
    status_ptr: MutOpaqueAny
) -> MutGpuBufF32Ptr:
    return _gpu_buf_export_affine[DType.float32](ctxp, handle, scale, bias, status_ptr, GPU_BUF_BYTES_F32)

@export("mojor_gpu_buf_f32_chain", ABI="C")
fn mojor_gpu_buf_f32_chain(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> MutGpuBufF32Ptr:
    return _gpu_buf_export_chain[DType.float32](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F32)

@export("mojor_gpu_buf_f32_chain_sum", ABI="C")
fn mojor_gpu_buf_f32_chain_sum(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float32:
    return _gpu_buf_export_chain_sum[DType.float32](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F32, False)

@export("mojor_gpu_buf_f32_chain_sum_warp", ABI="C")
fn mojor_gpu_buf_f32_chain_sum_warp(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    iters: Int32,
    scale: Float32,
    bias: Float32,
    post_scale: Float32,
    post_bias: Float32,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float32:
    return _gpu_buf_export_chain_sum[DType.float32](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F32, True)

@export("mojor_gpu_buf_f64_alloc", ABI="C")
fn mojor_gpu_buf_f64_alloc(ctxp: MutCtxPtr, n: Int32) -> MutGpuBufF64Ptr:
    return _gpu_buf_export_alloc[DType.float64](ctxp, n, GPU_BUF_BYTES_F64)

@export("mojor_gpu_buf_f64_free", ABI="C")
fn mojor_gpu_buf_f64_free(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr) -> Int32:
    return _gpu_buf_export_free[DType.float64](ctxp, handle)

@export("mojor_gpu_buf_f64_len", ABI="C")
fn mojor_gpu_buf_f64_len(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr) -> Int32:
    return _gpu_buf_export_len[DType.float64](ctxp, handle)

@export("mojor_gpu_buf_f64_write", ABI="C")
fn mojor_gpu_buf_f64_write(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr, host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_write[DType.float64](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_f64_read", ABI="C")
fn mojor_gpu_buf_f64_read(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr, host_ptr: MutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_read[DType.float64](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_f64_ptr", ABI="C")
fn mojor_gpu_buf_f64_ptr(handle: MutGpuBufF64Ptr) -> MutBufF64Ptr:
    return _gpu_buf_export_ptr[DType.float64](handle)

@export("mojor_gpu_buf_f64_affine", ABI="C")
fn mojor_gpu_buf_f64_affine(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    scale: Float64,
    bias: Float64,
    status_ptr: MutOpaqueAny
) -> MutGpuBufF64Ptr:
    return _gpu_buf_export_affine[DType.float64](ctxp, handle, scale, bias, status_ptr, GPU_BUF_BYTES_F64)

@export("mojor_gpu_buf_f64_chain", ABI="C")
fn mojor_gpu_buf_f64_chain(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    iters: Int32,
    scale: Float64,
    bias: Float64,
    post_scale: Float64,
    post_bias: Float64,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> MutGpuBufF64Ptr:
    return _gpu_buf_export_chain[DType.float64](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F64)

@export("mojor_gpu_buf_f64_chain_sum", ABI="C")
fn mojor_gpu_buf_f64_chain_sum(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    iters: Int32,
    scale: Float64,
    bias: Float64,
    post_scale: Float64,
    post_bias: Float64,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float64:
    return _gpu_buf_export_chain_sum[DType.float64](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F64, False)

@export("mojor_gpu_buf_f64_chain_sum_warp", ABI="C")
fn mojor_gpu_buf_f64_chain_sum_warp(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    iters: Int32,
    scale: Float64,
    bias: Float64,
    post_scale: Float64,
    post_bias: Float64,
    post_iters: Int32,
    status_ptr: MutOpaqueAny
) -> Float64:
    return _gpu_buf_export_chain_sum[DType.float64](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, GPU_BUF_BYTES_F64, True)

@export("mojor_gpu_buf_i32_alloc", ABI="C")
fn mojor_gpu_buf_i32_alloc(ctxp: MutCtxPtr, n: Int32) -> MutGpuBufI32Ptr:
    return _gpu_buf_export_alloc[DType.int32](ctxp, n, GPU_BUF_BYTES_I32)

@export("mojor_gpu_buf_i32_free", ABI="C")
fn mojor_gpu_buf_i32_free(ctxp: MutCtxPtr, handle: MutGpuBufI32Ptr) -> Int32:
    return _gpu_buf_export_free[DType.int32](ctxp, handle)

@export("mojor_gpu_buf_i32_len", ABI="C")
fn mojor_gpu_buf_i32_len(ctxp: MutCtxPtr, handle: MutGpuBufI32Ptr) -> Int32:
    return _gpu_buf_export_len[DType.int32](ctxp, handle)

@export("mojor_gpu_buf_i32_write", ABI="C")
fn mojor_gpu_buf_i32_write(ctxp: MutCtxPtr, handle: MutGpuBufI32Ptr, host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_write[DType.int32](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_i32_read", ABI="C")
fn mojor_gpu_buf_i32_read(ctxp: MutCtxPtr, handle: MutGpuBufI32Ptr, host_ptr: MutOpaqueAny, n: Int32) -> Int32:
    return _gpu_buf_export_read[DType.int32](ctxp, handle, host_ptr, n)

@export("mojor_gpu_buf_i32_ptr", ABI="C")
fn mojor_gpu_buf_i32_ptr(handle: MutGpuBufI32Ptr) -> MutBufI32Ptr:
    return _gpu_buf_export_ptr[DType.int32](handle)

# END GENERATED GPU_BUF_EXPORTS

@export("mojor_gpu_buf_f32_cast_f64", ABI="C")
fn mojor_gpu_buf_f32_cast_f64(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr
) -> MutGpuBufF64Ptr:
    return _gpu_buf_cast[DType.float32, DType.float64](ctxp, handle, GPU_BUF_BYTES_F32, GPU_BUF_BYTES_F64)

@export("mojor_gpu_buf_f32_cast_i32", ABI="C")
fn mojor_gpu_buf_f32_cast_i32(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr
) -> MutGpuBufI32Ptr:
    return _gpu_buf_cast[DType.float32, DType.int32](ctxp, handle, GPU_BUF_BYTES_F32, GPU_BUF_BYTES_I32)

@export("mojor_gpu_buf_f64_cast_f32", ABI="C")
fn mojor_gpu_buf_f64_cast_f32(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr
) -> MutGpuBufF32Ptr:
    return _gpu_buf_cast[DType.float64, DType.float32](ctxp, handle, GPU_BUF_BYTES_F64, GPU_BUF_BYTES_F32)

@export("mojor_gpu_buf_f64_cast_i32", ABI="C")
fn mojor_gpu_buf_f64_cast_i32(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr
) -> MutGpuBufI32Ptr:
    return _gpu_buf_cast[DType.float64, DType.int32](ctxp, handle, GPU_BUF_BYTES_F64, GPU_BUF_BYTES_I32)

@export("mojor_gpu_buf_i32_cast_f32", ABI="C")
fn mojor_gpu_buf_i32_cast_f32(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr
) -> MutGpuBufF32Ptr:
    return _gpu_buf_cast[DType.int32, DType.float32](ctxp, handle, GPU_BUF_BYTES_I32, GPU_BUF_BYTES_F32)

@export("mojor_gpu_buf_i32_cast_f64", ABI="C")
fn mojor_gpu_buf_i32_cast_f64(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr
) -> MutGpuBufF64Ptr:
    return _gpu_buf_cast[DType.int32, DType.float64](ctxp, handle, GPU_BUF_BYTES_I32, GPU_BUF_BYTES_F64)

# ============================================================================
# Random Number Generation (RNG) support
# ============================================================================

from math import exp, log, log1p, sqrt
from ziggurat_constants import _KI_DOUBLE, _WI_DOUBLE, _FI_DOUBLE
from rng_helpers import _rng_next_f64, _random_standard_normal, _random_standard_gamma, _random_poisson, _random_chisq, _random_beta, _random_weibull, _random_logistic, _random_cauchy, _random_geometric, _random_hypergeometric, _random_signrank, _random_wilcox

@export("mojor_rexp", ABI="C")
fn mojor_rexp(out_ptr: MutOpaqueAny, n: Int32, rate: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n exponential random numbers with rate."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = -log1p(-_rng_next_f64(state)) / rate

@export("mojor_rpois", ABI="C")
fn mojor_rpois(out_ptr: MutOpaqueAny, n: Int32, lam: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n Poisson random numbers with mean lambda."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = Float64(_random_poisson(state, ki, wi, fi, lam))

@export("mojor_rlnorm", ABI="C")
fn mojor_rlnorm(out_ptr: MutOpaqueAny, n: Int32, meanlog: Float64, sdlog: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n log-normal random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        var z = meanlog + sdlog * _random_standard_normal(state, ki, wi, fi)
        out[i] = exp(z)

@export("mojor_rchisq", ABI="C")
fn mojor_rchisq(out_ptr: MutOpaqueAny, n: Int32, df: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n chi-squared random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = _random_chisq(state, ki, wi, fi, df)

@export("mojor_rt", ABI="C")
fn mojor_rt(out_ptr: MutOpaqueAny, n: Int32, df: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n Student's t random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        var z = _random_standard_normal(state, ki, wi, fi)
        var x = _random_chisq(state, ki, wi, fi, df)
        out[i] = z / sqrt(x / df)

@export("mojor_rf", ABI="C")
fn mojor_rf(out_ptr: MutOpaqueAny, n: Int32, df1: Float64, df2: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n F random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        var x1 = _random_chisq(state, ki, wi, fi, df1)
        var x2 = _random_chisq(state, ki, wi, fi, df2)
        out[i] = (x1 / df1) / (x2 / df2)

@export("mojor_rbeta", ABI="C")
fn mojor_rbeta(out_ptr: MutOpaqueAny, n: Int32, shape1: Float64, shape2: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n beta random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = _random_beta(state, ki, wi, fi, shape1, shape2)

@export("mojor_rweibull", ABI="C")
fn mojor_rweibull(out_ptr: MutOpaqueAny, n: Int32, shape: Float64, scale: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n Weibull random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = _random_weibull(state, shape, scale)

@export("mojor_rlogis", ABI="C")
fn mojor_rlogis(out_ptr: MutOpaqueAny, n: Int32, location: Float64, scale: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n logistic random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = _random_logistic(state, location, scale)

@export("mojor_rcauchy", ABI="C")
fn mojor_rcauchy(out_ptr: MutOpaqueAny, n: Int32, location: Float64, scale: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n Cauchy random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = _random_cauchy(state, location, scale)

@export("mojor_rgeom", ABI="C")
fn mojor_rgeom(out_ptr: MutOpaqueAny, n: Int32, prob: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n geometric random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = Float64(_random_geometric(state, prob))

@export("mojor_rnbinom", ABI="C")
fn mojor_rnbinom(out_ptr: MutOpaqueAny, n: Int32, size: Float64, prob: Float64, state_ptr: MutOpaqueAny) -> None:
    """Generate n negative-binomial random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var ki = materialize[_KI_DOUBLE]()
    var wi = materialize[_WI_DOUBLE]()
    var fi = materialize[_FI_DOUBLE]()
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    if prob >= 1.0:
        for i in range(n_i):
            out[i] = 0.0
        return
    var rate = prob / (1.0 - prob)
    for i in range(n_i):
        var lam = _random_standard_gamma(state, ki, wi, fi, size) / rate
        out[i] = Float64(_random_poisson(state, ki, wi, fi, lam))

@export("mojor_rhyper", ABI="C")
fn mojor_rhyper(out_ptr: MutOpaqueAny, n: Int32, m: Int32, n_bad: Int32, k: Int32, state_ptr: MutOpaqueAny) -> None:
    """Generate n hypergeometric random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var m_i = Int(m)
    var n_bad_i = Int(n_bad)
    var k_i = Int(k)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = Float64(_random_hypergeometric(state, m_i, n_bad_i, k_i))

@export("mojor_rsignrank", ABI="C")
fn mojor_rsignrank(out_ptr: MutOpaqueAny, n: Int32, n_sign: Int32, state_ptr: MutOpaqueAny) -> None:
    """Generate n Wilcoxon signed-rank random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var n_sign_i = Int(n_sign)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = Float64(_random_signrank(state, n_sign_i))

@export("mojor_rwilcox", ABI="C")
fn mojor_rwilcox(out_ptr: MutOpaqueAny, n: Int32, m: Int32, n_w: Int32, state_ptr: MutOpaqueAny) -> None:
    """Generate n Wilcoxon rank-sum random numbers."""
    var out: MutF64Ptr = out_ptr.bitcast[Scalar[DType.float64]]()
    var n_i = Int(n)
    var m_i = Int(m)
    var n_w_i = Int(n_w)
    var state: MutU64Ptr = state_ptr.bitcast[UInt64]()
    for i in range(n_i):
        out[i] = Float64(_random_wilcox(state, m_i, n_w_i))

# ============================================================================
# BinCount (histogram) implementation - C_BinCount equivalent
# ============================================================================

@export("mojor_bincount_f64", ABI="C")
fn mojor_bincount_f64(
    x_ptr: ImmutOpaqueAny,           # Input data (n elements)
    breaks_ptr: ImmutOpaqueAny,      # Break points (nbreaks elements, sorted)
    counts_ptr: MutOpaqueAny,        # Output counts (nbins = nbreaks - 1)
    n: Int32,                        # Length of x
    nbreaks: Int32,                  # Length of breaks
    right: Int32,                    # 1 for (a,b], 0 for [a,b)
    include_lowest: Int32            # 1 to include lowest value
) -> None:
    """
    BinCount implementation equivalent to R's C_BinCount.
    
    Counts elements of x into bins defined by breaks.
    breaks must be sorted in ascending order.
    
    Args:
        x_ptr: Pointer to f64 array of data.
        breaks_ptr: Pointer to f64 array of break points (sorted).
        counts_ptr: Pointer to i32 array for output counts (zeroed by caller).
        n: Number of data points.
        nbreaks: Number of break points.
        right: If 1, use (a,b] intervals; if 0, use [a,b).
        include_lowest: If 1, include the lowest value in first bin.
    """
    var x: ImmutF64Ptr = x_ptr.bitcast[Scalar[DType.float64]]()
    var breaks: ImmutF64Ptr = breaks_ptr.bitcast[Scalar[DType.float64]]()
    var counts: MutI32Ptr = counts_ptr.bitcast[Int32]()
    
    var n_i = Int(n)
    var nbreaks_i = Int(nbreaks)
    var nbins = nbreaks_i - 1
    
    if nbins <= 0:
        return
    
    var right_closed = right != 0
    var include_min = include_lowest != 0
    
    for i in range(n_i):
        var xi = x[i]
        var bin = -1
        
        # Find the bin for this value
        # breaks[0] < breaks[1] < ... < breaks[nbreaks-1]
        # bins are: [breaks[0], breaks[1]), [breaks[1], breaks[2]), ... or with (]
        
        # Check if value is below lowest break
        if xi < breaks[0]:
            continue  # Below range, not counted
        
        # Check if value is above highest break
        if xi > breaks[nbreaks_i - 1]:
            continue  # Above range, not counted
        
        # Linear search for bin (could be binary search for many bins)
        # For typical histograms with < 100 bins, linear is fine
        for j in range(nbins):
            var low = breaks[j]
            var high = breaks[j + 1]
            
            if right_closed:
                # (low, high] interval
                # Special case for first bin with include_lowest
                if j == 0 and include_min:
                    if xi >= low and xi <= high:
                        bin = j
                        break
                else:
                    if xi > low and xi <= high:
                        bin = j
                        break
            else:
                # [low, high) interval
                # Special case for last bin with include_lowest
                if j == nbins - 1 and include_min:
                    if xi >= low and xi <= high:
                        bin = j
                        break
                else:
                    if xi >= low and xi < high:
                        bin = j
                        break
        
        if bin >= 0:
            counts[bin] = counts[bin] + 1

@export("mojor_ir_emit_stmt_pilot", ABI="C")
fn mojor_ir_emit_stmt_pilot(
    payload_ptr: ImmutOpaqueAny,
    payload_len: Int32,
    legacy_ptr: ImmutOpaqueAny,
    legacy_len: Int32,
    mode_hint: Int32,
    out_ptr: MutOpaqueAny,
    out_cap: Int32,
    out_len_ptr: MutOpaqueAny
) -> Int32:
    # Phase-6 compat-first endpoint:
    # backend provides emitted text bytes via out buffer while keeping
    # legacy text as authoritative source during pilot rollout.
    if payload_len < 0 or legacy_len < 0 or out_cap < 0:
        return -2
    var payload_i = payload_ptr.bitcast[UInt8]()
    var legacy_i = legacy_ptr.bitcast[UInt8]()
    var out_i = out_ptr.bitcast[UInt8]()
    var out_len_i = out_len_ptr.bitcast[Int32]()
    if payload_i == ImmutU8Ptr(unsafe_from_address=0) or legacy_i == ImmutU8Ptr(unsafe_from_address=0) or out_len_i == NULL_I32:
        return -3
    var parsed_assign_const = False
    var lhs_start = 0
    var lhs_len = 0
    var val_start = 0
    var val_len = 0
    var parsed_return_var = False
    var ret_name_start = 0
    var ret_name_len = 0
    var return_plain_shape = False
    var return_scalar_shape = False
    var parsed_block_simple = False
    var block_lhs_start = 0
    var block_lhs_len = 0
    var block_val_start = 0
    var block_val_len = 0
    var parsed_if_simple = False
    var if_cond_start = 0
    var if_cond_len = 0
    var if_then_lhs_start = 0
    var if_then_lhs_len = 0
    var if_then_val_start = 0
    var if_then_val_len = 0
    var if_else_lhs_start = 0
    var if_else_lhs_len = 0
    var if_else_val_start = 0
    var if_else_val_len = 0
    var candidate_len = legacy_len
    if mode_hint == 1:
        var n_payload = Int(payload_len)
        var i = 0
        var found_name = -1
        while i + 8 <= n_payload:
            if payload_i[i] == UInt8(110) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(109) and payload_i[i + 3] == UInt8(101) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34):
                found_name = i + 8
                break
            i += 1
        if found_name >= 0:
            var ne = found_name
            while ne < n_payload and payload_i[ne] != UInt8(34):
                ne += 1
            if ne > found_name and ne < n_payload:
                lhs_start = found_name
                lhs_len = ne - found_name
                i = 0
                var found_value = -1
                while i + 8 <= n_payload:
                    if payload_i[i] == UInt8(118) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(108) and payload_i[i + 3] == UInt8(117) and payload_i[i + 4] == UInt8(101) and payload_i[i + 5] == UInt8(32) and payload_i[i + 6] == UInt8(61) and payload_i[i + 7] == UInt8(32):
                        found_value = i + 8
                        break
                    i += 1
                if found_value >= 0:
                    var vs = found_value
                    while vs < n_payload and (payload_i[vs] == UInt8(32) or payload_i[vs] == UInt8(9)):
                        vs += 1
                    if vs < n_payload:
                        var ve = vs
                        if payload_i[vs] == UInt8(34):
                            vs += 1
                            ve = vs
                            while ve < n_payload and payload_i[ve] != UInt8(34):
                                ve += 1
                            if ve > vs and ve < n_payload:
                                val_start = vs
                                val_len = ve - vs
                                parsed_assign_const = True
                        else:
                            while ve < n_payload:
                                var ch = payload_i[ve]
                                if ch == UInt8(44) or ch == UInt8(10) or ch == UInt8(13) or ch == UInt8(41) or ch == UInt8(32) or ch == UInt8(9):
                                    break
                                ve += 1
                            if ve > vs:
                                val_start = vs
                                val_len = ve - vs
                                parsed_assign_const = True
        if parsed_assign_const:
            candidate_len = Int32(4 + lhs_len + 3 + val_len)
        else:
            candidate_len = legacy_len
    elif mode_hint == 2:
        var n_payload = Int(payload_len)
        var i = 0
        var found_return = -1
        while i + 15 <= n_payload:
            if payload_i[i] == UInt8(107) and payload_i[i + 1] == UInt8(105) and payload_i[i + 2] == UInt8(110) and payload_i[i + 3] == UInt8(100) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34) and payload_i[i + 8] == UInt8(114) and payload_i[i + 9] == UInt8(101) and payload_i[i + 10] == UInt8(116) and payload_i[i + 11] == UInt8(117) and payload_i[i + 12] == UInt8(114) and payload_i[i + 13] == UInt8(110) and payload_i[i + 14] == UInt8(34):
                found_return = i
                break
            i += 1
        if found_return >= 0:
            i = found_return
            var found_name = -1
            while i + 8 <= n_payload:
                if payload_i[i] == UInt8(110) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(109) and payload_i[i + 3] == UInt8(101) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34):
                    found_name = i + 8
                    break
                i += 1
            if found_name >= 0:
                var ne = found_name
                while ne < n_payload and payload_i[ne] != UInt8(34):
                    ne += 1
                if ne > found_name and ne < n_payload:
                    ret_name_start = found_name
                    ret_name_len = ne - found_name
                    parsed_return_var = True
        if parsed_return_var:
            if legacy_len == 10 and legacy_i[0] == UInt8(32) and legacy_i[1] == UInt8(32) and legacy_i[2] == UInt8(32) and legacy_i[3] == UInt8(32) and legacy_i[4] == UInt8(114) and legacy_i[5] == UInt8(101) and legacy_i[6] == UInt8(116) and legacy_i[7] == UInt8(117) and legacy_i[8] == UInt8(114) and legacy_i[9] == UInt8(110):
                return_plain_shape = True
                candidate_len = 10
            else:
                var n_legacy = Int(legacy_len)
                var has_out_assign = False
                i = 0
                while i + 10 <= n_legacy:
                    if legacy_i[i] == UInt8(95) and legacy_i[i + 1] == UInt8(111) and legacy_i[i + 2] == UInt8(117) and legacy_i[i + 3] == UInt8(116) and legacy_i[i + 4] == UInt8(91) and legacy_i[i + 5] == UInt8(48) and legacy_i[i + 6] == UInt8(93) and legacy_i[i + 7] == UInt8(32) and legacy_i[i + 8] == UInt8(61) and legacy_i[i + 9] == UInt8(32):
                        has_out_assign = True
                        break
                    i += 1
                var has_return_tail = False
                i = 0
                while i + 11 <= n_legacy:
                    if legacy_i[i] == UInt8(10) and legacy_i[i + 1] == UInt8(32) and legacy_i[i + 2] == UInt8(32) and legacy_i[i + 3] == UInt8(32) and legacy_i[i + 4] == UInt8(32) and legacy_i[i + 5] == UInt8(114) and legacy_i[i + 6] == UInt8(101) and legacy_i[i + 7] == UInt8(116) and legacy_i[i + 8] == UInt8(117) and legacy_i[i + 9] == UInt8(114) and legacy_i[i + 10] == UInt8(110):
                        has_return_tail = True
                        break
                    i += 1
                if has_out_assign and has_return_tail:
                    return_scalar_shape = True
                    candidate_len = Int32(25 + ret_name_len * 2)
                else:
                    candidate_len = legacy_len
        else:
            candidate_len = legacy_len
    elif mode_hint == 3:
        var n_payload = Int(payload_len)
        var i = 0
        var found_assign = -1
        while i + 15 <= n_payload:
            if payload_i[i] == UInt8(107) and payload_i[i + 1] == UInt8(105) and payload_i[i + 2] == UInt8(110) and payload_i[i + 3] == UInt8(100) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34) and payload_i[i + 8] == UInt8(97) and payload_i[i + 9] == UInt8(115) and payload_i[i + 10] == UInt8(115) and payload_i[i + 11] == UInt8(105) and payload_i[i + 12] == UInt8(103) and payload_i[i + 13] == UInt8(110) and payload_i[i + 14] == UInt8(34):
                found_assign = i
                break
            i += 1
        if found_assign >= 0:
            i = found_assign
            var found_lhs_name = -1
            while i + 8 <= n_payload:
                if payload_i[i] == UInt8(110) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(109) and payload_i[i + 3] == UInt8(101) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34):
                    found_lhs_name = i + 8
                    break
                i += 1
            if found_lhs_name >= 0:
                var ne = found_lhs_name
                while ne < n_payload and payload_i[ne] != UInt8(34):
                    ne += 1
                if ne > found_lhs_name and ne < n_payload:
                    block_lhs_start = found_lhs_name
                    block_lhs_len = ne - found_lhs_name
                    i = found_assign
                    var found_value = -1
                    while i + 8 <= n_payload:
                        if payload_i[i] == UInt8(118) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(108) and payload_i[i + 3] == UInt8(117) and payload_i[i + 4] == UInt8(101) and payload_i[i + 5] == UInt8(32) and payload_i[i + 6] == UInt8(61) and payload_i[i + 7] == UInt8(32):
                            found_value = i + 8
                            break
                        i += 1
                    if found_value >= 0:
                        var vs = found_value
                        while vs < n_payload and (payload_i[vs] == UInt8(32) or payload_i[vs] == UInt8(9)):
                            vs += 1
                        if vs < n_payload:
                            var ve = vs
                            if payload_i[vs] == UInt8(34):
                                vs += 1
                                ve = vs
                                while ve < n_payload and payload_i[ve] != UInt8(34):
                                    ve += 1
                                if ve > vs and ve < n_payload:
                                    block_val_start = vs
                                    block_val_len = ve - vs
                            else:
                                while ve < n_payload:
                                    var ch = payload_i[ve]
                                    if ch == UInt8(44) or ch == UInt8(10) or ch == UInt8(13) or ch == UInt8(41) or ch == UInt8(32) or ch == UInt8(9):
                                        break
                                    ve += 1
                                if ve > vs:
                                    block_val_start = vs
                                    block_val_len = ve - vs
        var block_return_ok = False
        if block_lhs_len > 0 and block_val_len > 0:
            i = 0
            var found_return = -1
            while i + 15 <= n_payload:
                if payload_i[i] == UInt8(107) and payload_i[i + 1] == UInt8(105) and payload_i[i + 2] == UInt8(110) and payload_i[i + 3] == UInt8(100) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34) and payload_i[i + 8] == UInt8(114) and payload_i[i + 9] == UInt8(101) and payload_i[i + 10] == UInt8(116) and payload_i[i + 11] == UInt8(117) and payload_i[i + 12] == UInt8(114) and payload_i[i + 13] == UInt8(110) and payload_i[i + 14] == UInt8(34):
                    found_return = i
                    break
                i += 1
            if found_return >= 0:
                i = found_return
                var found_ret_name = -1
                while i + 8 <= n_payload:
                    if payload_i[i] == UInt8(110) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(109) and payload_i[i + 3] == UInt8(101) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34):
                        found_ret_name = i + 8
                        break
                    i += 1
                if found_ret_name >= 0:
                    var re = found_ret_name
                    while re < n_payload and payload_i[re] != UInt8(34):
                        re += 1
                    if re > found_ret_name and re < n_payload and (re - found_ret_name) == block_lhs_len:
                        block_return_ok = True
                        for j in range(block_lhs_len):
                            if payload_i[block_lhs_start + j] != payload_i[found_ret_name + j]:
                                block_return_ok = False
                                break
        var legacy_return_line_ok = False
        var n_legacy = Int(legacy_len)
        var newline_pos = -1
        i = 0
        while i < n_legacy:
            if legacy_i[i] == UInt8(10):
                newline_pos = i
                break
            i += 1
        if newline_pos > 0 and (n_legacy - newline_pos - 1) == 10:
            var tail = newline_pos + 1
            if legacy_i[tail + 0] == UInt8(32) and legacy_i[tail + 1] == UInt8(32) and legacy_i[tail + 2] == UInt8(32) and legacy_i[tail + 3] == UInt8(32) and legacy_i[tail + 4] == UInt8(114) and legacy_i[tail + 5] == UInt8(101) and legacy_i[tail + 6] == UInt8(116) and legacy_i[tail + 7] == UInt8(117) and legacy_i[tail + 8] == UInt8(114) and legacy_i[tail + 9] == UInt8(110):
                legacy_return_line_ok = True
        if block_lhs_len > 0 and block_val_len > 0 and block_return_ok and legacy_return_line_ok:
            parsed_block_simple = True
            candidate_len = Int32(18 + block_lhs_len + block_val_len)
        else:
            candidate_len = legacy_len
    elif mode_hint == 4:
        var n_payload = Int(payload_len)
        var i = 0
        var found_if = -1
        while i + 11 <= n_payload:
            if payload_i[i] == UInt8(107) and payload_i[i + 1] == UInt8(105) and payload_i[i + 2] == UInt8(110) and payload_i[i + 3] == UInt8(100) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34) and payload_i[i + 8] == UInt8(105) and payload_i[i + 9] == UInt8(102) and payload_i[i + 10] == UInt8(34):
                found_if = i
                break
            i += 1
        if found_if >= 0:
            var name_from = found_if
            var name_ok = True
            for slot in range(3):
                i = name_from
                var found_name = -1
                while i + 8 <= n_payload:
                    if payload_i[i] == UInt8(110) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(109) and payload_i[i + 3] == UInt8(101) and payload_i[i + 4] == UInt8(32) and payload_i[i + 5] == UInt8(61) and payload_i[i + 6] == UInt8(32) and payload_i[i + 7] == UInt8(34):
                        found_name = i + 8
                        break
                    i += 1
                if found_name < 0:
                    name_ok = False
                    break
                var ne = found_name
                while ne < n_payload and payload_i[ne] != UInt8(34):
                    ne += 1
                if ne <= found_name or ne >= n_payload:
                    name_ok = False
                    break
                if slot == 0:
                    if_cond_start = found_name
                    if_cond_len = ne - found_name
                elif slot == 1:
                    if_then_lhs_start = found_name
                    if_then_lhs_len = ne - found_name
                else:
                    if_else_lhs_start = found_name
                    if_else_lhs_len = ne - found_name
                name_from = ne + 1
            var value_from = found_if
            var value_ok = True
            for slot in range(2):
                i = value_from
                var found_value = -1
                while i + 8 <= n_payload:
                    if payload_i[i] == UInt8(118) and payload_i[i + 1] == UInt8(97) and payload_i[i + 2] == UInt8(108) and payload_i[i + 3] == UInt8(117) and payload_i[i + 4] == UInt8(101) and payload_i[i + 5] == UInt8(32) and payload_i[i + 6] == UInt8(61) and payload_i[i + 7] == UInt8(32):
                        found_value = i + 8
                        break
                    i += 1
                if found_value < 0:
                    value_ok = False
                    break
                var vs = found_value
                while vs < n_payload and (payload_i[vs] == UInt8(32) or payload_i[vs] == UInt8(9)):
                    vs += 1
                if vs >= n_payload:
                    value_ok = False
                    break
                var ve = vs
                if payload_i[vs] == UInt8(34):
                    vs += 1
                    ve = vs
                    while ve < n_payload and payload_i[ve] != UInt8(34):
                        ve += 1
                    if ve <= vs or ve >= n_payload:
                        value_ok = False
                        break
                else:
                    while ve < n_payload:
                        var ch = payload_i[ve]
                        if ch == UInt8(44) or ch == UInt8(10) or ch == UInt8(13) or ch == UInt8(41) or ch == UInt8(32) or ch == UInt8(9):
                            break
                        ve += 1
                    if ve <= vs:
                        value_ok = False
                        break
                if slot == 0:
                    if_then_val_start = vs
                    if_then_val_len = ve - vs
                else:
                    if_else_val_start = vs
                    if_else_val_len = ve - vs
                value_from = ve + 1
            if name_ok and value_ok and if_cond_len > 0 and if_then_lhs_len > 0 and if_then_val_len > 0 and if_else_lhs_len > 0 and if_else_val_len > 0:
                parsed_if_simple = True
                candidate_len = Int32(42 + if_cond_len + if_then_lhs_len + if_then_val_len + if_else_lhs_len + if_else_val_len)
            else:
                candidate_len = legacy_len
        else:
            candidate_len = legacy_len
    out_len_i[0] = candidate_len
    if out_i == NULL_U8 or Int(out_cap) <= Int(candidate_len):
        return -4
    if mode_hint == 1 and parsed_assign_const:
        var pos = 0
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(lhs_len):
            out_i[pos] = payload_i[lhs_start + j]
            pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(61); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(val_len):
            out_i[pos] = payload_i[val_start + j]
            pos += 1
        out_i[pos] = UInt8(0)
        return 0
    if mode_hint == 2 and return_scalar_shape:
        var pos = 0
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(ret_name_len):
            out_i[pos] = payload_i[ret_name_start + j]
            pos += 1
        out_i[pos] = UInt8(95); pos += 1
        out_i[pos] = UInt8(111); pos += 1
        out_i[pos] = UInt8(117); pos += 1
        out_i[pos] = UInt8(116); pos += 1
        out_i[pos] = UInt8(91); pos += 1
        out_i[pos] = UInt8(48); pos += 1
        out_i[pos] = UInt8(93); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(61); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(ret_name_len):
            out_i[pos] = payload_i[ret_name_start + j]
            pos += 1
        out_i[pos] = UInt8(10); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(114); pos += 1
        out_i[pos] = UInt8(101); pos += 1
        out_i[pos] = UInt8(116); pos += 1
        out_i[pos] = UInt8(117); pos += 1
        out_i[pos] = UInt8(114); pos += 1
        out_i[pos] = UInt8(110); pos += 1
        out_i[pos] = UInt8(0)
        return 0
    if mode_hint == 2 and return_plain_shape:
        out_i[0] = UInt8(32); out_i[1] = UInt8(32); out_i[2] = UInt8(32); out_i[3] = UInt8(32)
        out_i[4] = UInt8(114); out_i[5] = UInt8(101); out_i[6] = UInt8(116); out_i[7] = UInt8(117)
        out_i[8] = UInt8(114); out_i[9] = UInt8(110)
        out_i[10] = UInt8(0)
        return 0
    if mode_hint == 3 and parsed_block_simple:
        var pos = 0
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(block_lhs_len):
            out_i[pos] = payload_i[block_lhs_start + j]
            pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(61); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(block_val_len):
            out_i[pos] = payload_i[block_val_start + j]
            pos += 1
        out_i[pos] = UInt8(10); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(114); pos += 1
        out_i[pos] = UInt8(101); pos += 1
        out_i[pos] = UInt8(116); pos += 1
        out_i[pos] = UInt8(117); pos += 1
        out_i[pos] = UInt8(114); pos += 1
        out_i[pos] = UInt8(110); pos += 1
        out_i[pos] = UInt8(0)
        return 0
    if mode_hint == 4 and parsed_if_simple:
        var pos = 0
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(105); pos += 1
        out_i[pos] = UInt8(102); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(if_cond_len):
            out_i[pos] = payload_i[if_cond_start + j]
            pos += 1
        out_i[pos] = UInt8(58); pos += 1
        out_i[pos] = UInt8(10); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(if_then_lhs_len):
            out_i[pos] = payload_i[if_then_lhs_start + j]
            pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(61); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(if_then_val_len):
            out_i[pos] = payload_i[if_then_val_start + j]
            pos += 1
        out_i[pos] = UInt8(10); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(101); pos += 1
        out_i[pos] = UInt8(108); pos += 1
        out_i[pos] = UInt8(115); pos += 1
        out_i[pos] = UInt8(101); pos += 1
        out_i[pos] = UInt8(58); pos += 1
        out_i[pos] = UInt8(10); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(if_else_lhs_len):
            out_i[pos] = payload_i[if_else_lhs_start + j]
            pos += 1
        out_i[pos] = UInt8(32); pos += 1
        out_i[pos] = UInt8(61); pos += 1
        out_i[pos] = UInt8(32); pos += 1
        for j in range(if_else_val_len):
            out_i[pos] = payload_i[if_else_val_start + j]
            pos += 1
        out_i[pos] = UInt8(0)
        return 0
    var n = Int(legacy_len)
    for i in range(n):
        out_i[i] = legacy_i[i]
    out_i[n] = UInt8(0)
    return 0


# =============================================================================
# Step 24: GPU Array Backend Functions
# =============================================================================

# GPU Reduction - parallel reduction on GPU
# op: one of REDUCE_SUM, REDUCE_MIN, REDUCE_MAX, REDUCE_MEAN, REDUCE_PROD, REDUCE_ARGMIN, REDUCE_ARGMAX
# dims: encoded dimension vector; each axis stores abs(size), with reduced axes negative
# keepdims: ignored by backend (handled in R for shape metadata)

comptime REDUCE_BLOCK_SIZE = 256

fn _inf_value[dtype: DType]() -> Scalar[dtype]:
    @parameter
    if dtype == DType.int32:
        return Scalar[dtype](Int32(2147483647))
    @parameter
    if dtype == DType.float64:
        return Scalar[dtype](Float64(1.0 / 0.0))
    return Scalar[dtype](Float32(1.0 / 0.0))

fn _ninf_value[dtype: DType]() -> Scalar[dtype]:
    @parameter
    if dtype == DType.int32:
        return Scalar[dtype](Int32(-2147483648))
    @parameter
    if dtype == DType.float64:
        return Scalar[dtype](Float64(-1.0 / 0.0))
    return Scalar[dtype](Float32(-1.0 / 0.0))

fn _reduce_identity[dtype: DType](op: Int32) -> Scalar[dtype]:
    if op == REDUCE_MIN:
        return _inf_value[dtype]()
    if op == REDUCE_MAX:
        return _ninf_value[dtype]()
    if op == REDUCE_PROD:
        return Scalar[dtype](1)
    return Scalar[dtype](0)

fn _reduce_step[dtype: DType](acc: Scalar[dtype], x: Scalar[dtype], op: Int32) -> Scalar[dtype]:
    var out = acc
    if op == REDUCE_MIN:
        if x < out:
            out = x
    elif op == REDUCE_MAX:
        if x > out:
            out = x
    elif op == REDUCE_PROD:
        out *= x
    else:
        out += x
    return out


fn _reduce_is_arg_op(op: Int32) -> Bool:
    return op == REDUCE_ARGMIN or op == REDUCE_ARGMAX

fn _reduce_kernel_1d_gpu_f32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    n: Int,
    op: Int32
):
    var tid = Int(thread_idx.x)
    var block_id = Int(block_idx.x)
    var idx = block_id * REDUCE_BLOCK_SIZE + tid

    var val = _reduce_identity[DType.float32](op)
    if idx < n:
        val = in_ptr[idx]

    var simd_val = SIMD[DType.float32, 1](val)
    if op == REDUCE_MIN:
        simd_val = block.min[dtype=DType.float32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
    elif op == REDUCE_MAX:
        simd_val = block.max[dtype=DType.float32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
    else:
        simd_val = block.sum[dtype=DType.float32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)

    if tid == 0:
        out_ptr[block_id] = simd_val[0]

fn _reduce_kernel_1d_gpu_f64(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    n: Int,
    op: Int32
):
    @parameter
    if MOJOR_GPU_F64_REDUCE_FASTPATH:
        var tid = Int(thread_idx.x)
        var block_id = Int(block_idx.x)
        var idx = block_id * REDUCE_BLOCK_SIZE + tid

        var val = _reduce_identity[DType.float64](op)
        if idx < n:
            val = in_ptr[idx]

        var simd_val = SIMD[DType.float64, 1](val)
        if op == REDUCE_MIN:
            simd_val = block.min[dtype=DType.float64, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
        elif op == REDUCE_MAX:
            simd_val = block.max[dtype=DType.float64, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
        else:
            simd_val = block.sum[dtype=DType.float64, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)

        if tid == 0:
            out_ptr[block_id] = simd_val[0]
    else:
        var tid = Int(thread_idx.x)
        var block_id = Int(block_idx.x)
        if tid != 0:
            return

        # Linux/CUDA builds can reject f64 warp-shuffle paths in std.gpu reduction
        # primitives. Keep a conservative block-local scalar fallback for portability.
        var start = block_id * REDUCE_BLOCK_SIZE
        if start >= n:
            out_ptr[block_id] = _reduce_identity[DType.float64](op)
            return

        var end = start + REDUCE_BLOCK_SIZE
        if end > n:
            end = n

        var acc = _reduce_identity[DType.float64](op)
        var i = start
        if op == REDUCE_MIN or op == REDUCE_MAX:
            acc = in_ptr[start]
            i = start + 1

        while i < end:
            acc = _reduce_step[DType.float64](acc, in_ptr[i], op)
            i += 1

        out_ptr[block_id] = acc

fn _reduce_kernel_1d_gpu_i32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    n: Int,
    op: Int32
):
    var tid = Int(thread_idx.x)
    var block_id = Int(block_idx.x)
    var idx = block_id * REDUCE_BLOCK_SIZE + tid

    var val = _reduce_identity[DType.int32](op)
    if idx < n:
        val = in_ptr[idx]

    var simd_val = SIMD[DType.int32, 1](val)
    if op == REDUCE_MIN:
        simd_val = block.min[dtype=DType.int32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
    elif op == REDUCE_MAX:
        simd_val = block.max[dtype=DType.int32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)
    else:
        simd_val = block.sum[dtype=DType.int32, width=1, block_size=REDUCE_BLOCK_SIZE, broadcast=False](simd_val)

    if tid == 0:
        out_ptr[block_id] = simd_val[0]

fn _scale_out_f32(
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    scale: Scalar[DType.float32]
):
    if Int(block_idx.x) == 0 and Int(thread_idx.x) == 0:
        out_ptr[0] = out_ptr[0] * scale

fn _scale_out_f64(
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    scale: Scalar[DType.float64]
):
    if Int(block_idx.x) == 0 and Int(thread_idx.x) == 0:
        out_ptr[0] = out_ptr[0] * scale

fn _div_out_i32(
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    n: Int32
):
    if Int(block_idx.x) == 0 and Int(thread_idx.x) == 0:
        if n > 0:
            out_ptr[0] = out_ptr[0] / Scalar[DType.int32](n)

fn _reduce_kernel_nd_gpu[dtype: DType](
    in_ptr: UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[dtype], origin=MutAnyOrigin],
    out_n: Int,
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int,
    red_n: Int,
    op: Int32
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var base = 0
    var rem = tid
    var stride = 1

    for axis in range(ndim):
        var raw = Int(dims_ptr[axis])
        if raw == 0:
            return
        var size = raw
        if size < 0:
            size = -size
        if size <= 0:
            return
        if raw > 0:
            var coord = rem % size
            rem = rem // size
            base += coord * stride
        stride *= size

    if _reduce_is_arg_op(op):
        if red_n <= 0:
            out_ptr[tid] = Scalar[dtype](0)
            return
        var best_val = in_ptr[base]
        var best_idx = 1
        for i in range(1, red_n):
            var in_off = base
            var rr = i
            var stride2 = 1
            for axis in range(ndim):
                var raw = Int(dims_ptr[axis])
                var size = raw
                if size < 0:
                    size = -size
                elif size == 0:
                    return
                if size <= 0:
                    return
                if raw < 0:
                    var coord = rr % size
                    rr = rr // size
                    in_off += coord * stride2
                stride2 *= size
            var cand = in_ptr[in_off]
            if op == REDUCE_ARGMIN:
                if cand < best_val:
                    best_val = cand
                    best_idx = i + 1
            else:
                if cand > best_val:
                    best_val = cand
                    best_idx = i + 1
        out_ptr[tid] = Scalar[dtype](best_idx)
        return

    var out_val = _reduce_identity[dtype](op)
    var start_i = 0
    if op == REDUCE_MIN or op == REDUCE_MAX:
        # Seed min/max from the first reduced element to avoid device-specific
        # inf/-inf constant behavior in scalar compare paths.
        var in_off0 = base
        var rr0 = 0
        var stride0 = 1
        for axis in range(ndim):
            var raw = Int(dims_ptr[axis])
            var size = raw
            if size < 0:
                size = -size
            elif size == 0:
                return
            if size <= 0:
                return
            if raw < 0:
                var coord0 = rr0 % size
                rr0 = rr0 // size
                in_off0 += coord0 * stride0
            stride0 *= size
        out_val = in_ptr[in_off0]
        start_i = 1

    for i in range(start_i, red_n):
        var in_off = base
        var rr = i
        var stride2 = 1
        for axis in range(ndim):
            var raw = Int(dims_ptr[axis])
            var size = raw
            if size < 0:
                size = -size
            elif size == 0:
                return
            if size <= 0:
                return
            if raw < 0:
                var coord = rr % size
                rr = rr // size
                in_off += coord * stride2
            stride2 *= size
        out_val = _reduce_step[dtype](out_val, in_ptr[in_off], op)

    if op == REDUCE_MEAN:
        out_val = out_val / Scalar[dtype](red_n)

    out_ptr[tid] = out_val

fn reduction_kernel_nd[dtype: DType](
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[dtype],
    out_bufp: DeviceBuffer[dtype],
    out_n: Int,
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int,
    red_n: Int,
    op: Int32
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_reduce_kernel_nd_gpu[dtype], _reduce_kernel_nd_gpu[dtype]](
        in_bufp,
        out_bufp,
        out_n,
        dims_ptr,
        ndim,
        red_n,
        op,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn reduction_kernel_1d[dtype: DType](
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[dtype],
    out_bufp: DeviceBuffer[dtype],
    n: Int,
    op: Int32
) raises:
    var op_reduce = op
    if op_reduce == REDUCE_MEAN:
        op_reduce = REDUCE_SUM

    var cur_buf = in_bufp
    var cur_n = n
    while True:
        var grid = ceildiv(cur_n, REDUCE_BLOCK_SIZE)
        var out_buf = out_bufp
        if grid > 1:
            out_buf = ctx.enqueue_create_buffer[dtype](grid)
        @parameter
        if dtype == DType.float32:
            ctx.enqueue_function[_reduce_kernel_1d_gpu_f32, _reduce_kernel_1d_gpu_f32](
                cur_buf,
                out_buf,
                cur_n,
                op_reduce,
                grid_dim=grid,
                block_dim=REDUCE_BLOCK_SIZE
            )
        elif dtype == DType.int32:
            ctx.enqueue_function[_reduce_kernel_1d_gpu_i32, _reduce_kernel_1d_gpu_i32](
                cur_buf,
                out_buf,
                cur_n,
                op_reduce,
                grid_dim=grid,
                block_dim=REDUCE_BLOCK_SIZE
            )
        else:
            ctx.enqueue_function[_reduce_kernel_1d_gpu_f64, _reduce_kernel_1d_gpu_f64](
                cur_buf,
                out_buf,
                cur_n,
                op_reduce,
                grid_dim=grid,
                block_dim=REDUCE_BLOCK_SIZE
            )
        if grid <= 1:
            break
        cur_buf = out_buf
        cur_n = grid

    ctx.synchronize()

    if op == REDUCE_MEAN:
        @parameter
        if dtype == DType.float32:
            var scale = Scalar[dtype](1.0) / Scalar[dtype](n)
            ctx.enqueue_function[_scale_out_f32, _scale_out_f32](
                out_bufp,
                scale,
                grid_dim=1,
                block_dim=1
            )
        elif dtype == DType.int32:
            ctx.enqueue_function[_div_out_i32, _div_out_i32](
                out_bufp,
                Int32(n),
                grid_dim=1,
                block_dim=1
            )
        else:
            var scale = Scalar[dtype](1.0) / Scalar[dtype](n)
            ctx.enqueue_function[_scale_out_f64, _scale_out_f64](
                out_bufp,
                scale,
                grid_dim=1,
                block_dim=1
            )
        ctx.synchronize()

fn _slice_kernel_nd_gpu_f32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var start = Int(starts_ptr[axis])
        var size = Int(sizes_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return
        if start + size > dim:
            return
        var coord = rem % size
        rem = rem // size
        in_off += (start + coord) * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn slice_kernel_nd_f32(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.float32],
    out_bufp: DeviceBuffer[DType.float32],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_slice_kernel_nd_gpu_f32, _slice_kernel_nd_gpu_f32](
        in_bufp,
        out_bufp,
        out_n,
        starts_ptr,
        sizes_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _gather_kernel_nd_gpu_f32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        in_off += idx_val * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn gather_kernel_nd_f32(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.float32],
    out_bufp: DeviceBuffer[DType.float32],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gather_kernel_nd_gpu_f32, _gather_kernel_nd_gpu_f32](
        in_bufp,
        out_bufp,
        out_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _scatter_kernel_nd_gpu_f32(
    dst_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    values_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= values_n:
        return

    var rem = tid
    var dst_off = 0
    var dst_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        dst_off += idx_val * dst_stride
        dst_stride *= dim

    dst_ptr[dst_off] = values_ptr[tid]

fn scatter_kernel_nd_f32(
    ctx: DeviceContext,
    dst_bufp: DeviceBuffer[DType.float32],
    values_bufp: DeviceBuffer[DType.float32],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(values_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_scatter_kernel_nd_gpu_f32, _scatter_kernel_nd_gpu_f32](
        dst_bufp,
        values_bufp,
        values_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _slice_kernel_nd_gpu_f64(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var start = Int(starts_ptr[axis])
        var size = Int(sizes_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return
        if start + size > dim:
            return
        var coord = rem % size
        rem = rem // size
        in_off += (start + coord) * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn slice_kernel_nd_f64(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.float64],
    out_bufp: DeviceBuffer[DType.float64],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_slice_kernel_nd_gpu_f64, _slice_kernel_nd_gpu_f64](
        in_bufp,
        out_bufp,
        out_n,
        starts_ptr,
        sizes_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _gather_kernel_nd_gpu_f64(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        in_off += idx_val * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn gather_kernel_nd_f64(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.float64],
    out_bufp: DeviceBuffer[DType.float64],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gather_kernel_nd_gpu_f64, _gather_kernel_nd_gpu_f64](
        in_bufp,
        out_bufp,
        out_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _scatter_kernel_nd_gpu_f64(
    dst_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    values_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= values_n:
        return

    var rem = tid
    var dst_off = 0
    var dst_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        dst_off += idx_val * dst_stride
        dst_stride *= dim

    dst_ptr[dst_off] = values_ptr[tid]

fn scatter_kernel_nd_f64(
    ctx: DeviceContext,
    dst_bufp: DeviceBuffer[DType.float64],
    values_bufp: DeviceBuffer[DType.float64],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(values_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_scatter_kernel_nd_gpu_f64, _scatter_kernel_nd_gpu_f64](
        dst_bufp,
        values_bufp,
        values_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _slice_kernel_nd_gpu_i32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var start = Int(starts_ptr[axis])
        var size = Int(sizes_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return
        if start + size > dim:
            return
        var coord = rem % size
        rem = rem // size
        in_off += (start + coord) * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn slice_kernel_nd_i32(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.int32],
    out_bufp: DeviceBuffer[DType.int32],
    out_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_slice_kernel_nd_gpu_i32, _slice_kernel_nd_gpu_i32](
        in_bufp,
        out_bufp,
        out_n,
        starts_ptr,
        sizes_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _slice_assign_kernel_nd_gpu_i32(
    dst_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    values_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    values_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= values_n:
        return

    var rem = tid
    var dst_off = 0
    var dst_stride = 1

    for axis in range(ndim):
        var start = Int(starts_ptr[axis])
        var size = Int(sizes_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return
        if start + size > dim:
            return
        var coord = rem % size
        rem = rem // size
        dst_off += (start + coord) * dst_stride
        dst_stride *= dim

    dst_ptr[dst_off] = values_ptr[tid]

fn slice_assign_kernel_nd_i32(
    ctx: DeviceContext,
    dst_bufp: DeviceBuffer[DType.int32],
    values_bufp: DeviceBuffer[DType.int32],
    values_n: Int,
    starts_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    sizes_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(values_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_slice_assign_kernel_nd_gpu_i32, _slice_assign_kernel_nd_gpu_i32](
        dst_bufp,
        values_bufp,
        values_n,
        starts_ptr,
        sizes_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _gather_kernel_nd_gpu_i32(
    in_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    out_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= out_n:
        return

    var rem = tid
    var in_off = 0
    var in_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        in_off += idx_val * in_stride
        in_stride *= dim

    out_ptr[tid] = in_ptr[in_off]

fn gather_kernel_nd_i32(
    ctx: DeviceContext,
    in_bufp: DeviceBuffer[DType.int32],
    out_bufp: DeviceBuffer[DType.int32],
    out_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gather_kernel_nd_gpu_i32, _gather_kernel_nd_gpu_i32](
        in_bufp,
        out_bufp,
        out_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _scatter_kernel_nd_gpu_i32(
    dst_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    values_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if tid >= values_n:
        return

    var rem = tid
    var dst_off = 0
    var dst_stride = 1

    for axis in range(ndim):
        var idx_off = Int(idx_offsets_ptr[axis])
        var idx_len = Int(idx_lens_ptr[axis])
        var dim = Int(dims_ptr[axis])
        if idx_off < 0 or idx_len <= 0 or dim <= 0:
            return
        var coord = rem % idx_len
        rem = rem // idx_len
        var idx_val = Int(idx_data_ptr[idx_off + coord])
        if idx_val < 0 or idx_val >= dim:
            return
        dst_off += idx_val * dst_stride
        dst_stride *= dim

    dst_ptr[dst_off] = values_ptr[tid]

fn scatter_kernel_nd_i32(
    ctx: DeviceContext,
    dst_bufp: DeviceBuffer[DType.int32],
    values_bufp: DeviceBuffer[DType.int32],
    values_n: Int,
    idx_data_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_offsets_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    idx_lens_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    dims_ptr: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin],
    ndim: Int
) raises:
    var grid = max(1, ceildiv(values_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_scatter_kernel_nd_gpu_i32, _scatter_kernel_nd_gpu_i32](
        dst_bufp,
        values_bufp,
        values_n,
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE
    )
    ctx.synchronize()

fn _idxplan_f32_free_handle(plan_handle: MutGpuIdxPlanF32Ptr) -> Int32:
    if plan_handle == _null_idxplan_f32():
        return 0
    plan_handle.free()
    return 1

@fieldwise_init
struct IdxPlanShape(Movable):
    var valid: Bool
    var ndim_i: Int
    var idx_data_n: Int
    var out_n: Int
    var full_n: Int

fn _idxplan_validate_shape(
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> IdxPlanShape:
    var ndim_i = Int(ndim)
    if ndim_i <= 0:
        return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)

    var idx_data_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_data_ptr.bitcast[Int32]()
    var idx_offsets_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_offsets_ptr.bitcast[Int32]()
    var idx_lens_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_lens_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()

    var idx_data_n = 0
    var out_n = 1
    var full_n = 1
    for axis in range(ndim_i):
        var idx_off = Int(idx_offsets_raw[axis])
        var idx_len = Int(idx_lens_raw[axis])
        var dim = Int(dims_raw[axis])
        if idx_off != idx_data_n or idx_len <= 0 or dim <= 0:
            return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)
        idx_data_n += idx_len
        out_n *= idx_len
        full_n *= dim
        if out_n <= 0 or full_n <= 0:
            return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)
    if idx_data_n <= 0 or out_n <= 0 or full_n <= 0:
        return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)
    if not gpu_limit_ok(idx_data_n + (3 * ndim_i), 2, 4):
        return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)

    for axis in range(ndim_i):
        var idx_off = Int(idx_offsets_raw[axis])
        var idx_len = Int(idx_lens_raw[axis])
        var dim = Int(dims_raw[axis])
        for j in range(idx_len):
            var idx_val = Int(idx_data_raw[idx_off + j])
            if idx_val < 0 or idx_val >= dim:
                return IdxPlanShape(valid=False, ndim_i=0, idx_data_n=0, out_n=0, full_n=0)

    return IdxPlanShape(
        valid=True,
        ndim_i=ndim_i,
        idx_data_n=idx_data_n,
        out_n=out_n,
        full_n=full_n
    )

fn _idxplan_f32_new(
    ctxp: MutCtxPtr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuIdxPlanF32Ptr:
    if not gpu_ready(ctxp):
        return _null_idxplan_f32()

    var shape = _idxplan_validate_shape(
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim
    )
    if not shape.valid:
        return _null_idxplan_f32()
    var ndim_i = shape.ndim_i
    var idx_data_n = shape.idx_data_n
    var out_n = shape.out_n
    var full_n = shape.full_n

    var idx_data_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_data_ptr.bitcast[Int32]()
    var idx_offsets_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_offsets_ptr.bitcast[Int32]()
    var idx_lens_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_lens_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()
    try:
        var idx_data_host = ctxp[0].enqueue_create_host_buffer[DType.int32](idx_data_n)
        var idx_offsets_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var idx_lens_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)

        for i in range(idx_data_n):
            idx_data_host[i] = idx_data_raw[i]
        for axis in range(ndim_i):
            idx_offsets_host[axis] = idx_offsets_raw[axis]
            idx_lens_host[axis] = idx_lens_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var idx_data_dev = ctxp[0].enqueue_create_buffer[DType.int32](idx_data_n)
        var idx_offsets_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var idx_lens_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=idx_data_dev, src_buf=idx_data_host)
        ctxp[0].enqueue_copy(dst_buf=idx_offsets_dev, src_buf=idx_offsets_host)
        ctxp[0].enqueue_copy(dst_buf=idx_lens_dev, src_buf=idx_lens_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var planp = alloc[GpuIdxPlanF32](1)
        planp[0] = GpuIdxPlanF32(
            idx_data=idx_data_dev,
            idx_offsets=idx_offsets_dev,
            idx_lens=idx_lens_dev,
            dims=dims_dev,
            ndim=ndim_i,
            idx_data_n=idx_data_n,
            out_n=out_n,
            full_n=full_n
        )
        return planp
    except:
        return _null_idxplan_f32()

fn _gpu_buf_f32_gather_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    plan_handle: MutGpuIdxPlanF32Ptr
) -> MutGpuBufF32Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF or plan_handle == _null_idxplan_f32():
        return NULL_GPUBUF
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_F32:
        return NULL_GPUBUF
    if plan_handle[0].full_n != handle[0].len:
        return NULL_GPUBUF
    if plan_handle[0].out_n <= 0:
        return NULL_GPUBUF
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 4):
        return NULL_GPUBUF

    try:
        var out_buf = ctxp[0].enqueue_create_buffer[DType.float32](plan_handle[0].out_n)
        gather_kernel_nd_f32(
            ctxp[0],
            in_bufp[0],
            out_buf,
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )

        var out_bufp = alloc[DeviceBuffer[DType.float32]](1)
        out_bufp[0] = out_buf
        var out_handle = alloc[GpuBufF32](1)
        out_handle[0] = GpuBufF32(out_bufp, plan_handle[0].out_n)
        return out_handle
    except:
        return NULL_GPUBUF

fn _gpu_buf_f32_scatter_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    values_handle: MutGpuBufF32Ptr,
    plan_handle: MutGpuIdxPlanF32Ptr
) -> Int32:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF or values_handle == NULL_GPUBUF or plan_handle == _null_idxplan_f32():
        return 0
    var dst_bufp = handle[0].buf
    var values_bufp = values_handle[0].buf
    if dst_bufp == NULL_BUF_F32 or values_bufp == NULL_BUF_F32:
        return 0
    if plan_handle[0].full_n != handle[0].len:
        return 0
    if values_handle[0].len != plan_handle[0].out_n:
        return 0
    if plan_handle[0].out_n <= 0:
        return 0
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 4):
        return 0

    try:
        scatter_kernel_nd_f32(
            ctxp[0],
            dst_bufp[0],
            values_bufp[0],
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )
        return 1
    except:
        return 0

fn _idxplan_f64_free_handle(plan_handle: MutGpuIdxPlanF64Ptr) -> Int32:
    if plan_handle == _null_idxplan_f64():
        return 0
    plan_handle.free()
    return 1

fn _idxplan_f64_new(
    ctxp: MutCtxPtr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuIdxPlanF64Ptr:
    if not gpu_ready(ctxp):
        return _null_idxplan_f64()

    var shape = _idxplan_validate_shape(
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim
    )
    if not shape.valid:
        return _null_idxplan_f64()
    var ndim_i = shape.ndim_i
    var idx_data_n = shape.idx_data_n
    var out_n = shape.out_n
    var full_n = shape.full_n

    var idx_data_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_data_ptr.bitcast[Int32]()
    var idx_offsets_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_offsets_ptr.bitcast[Int32]()
    var idx_lens_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_lens_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()
    try:
        var idx_data_host = ctxp[0].enqueue_create_host_buffer[DType.int32](idx_data_n)
        var idx_offsets_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var idx_lens_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)

        for i in range(idx_data_n):
            idx_data_host[i] = idx_data_raw[i]
        for axis in range(ndim_i):
            idx_offsets_host[axis] = idx_offsets_raw[axis]
            idx_lens_host[axis] = idx_lens_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var idx_data_dev = ctxp[0].enqueue_create_buffer[DType.int32](idx_data_n)
        var idx_offsets_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var idx_lens_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=idx_data_dev, src_buf=idx_data_host)
        ctxp[0].enqueue_copy(dst_buf=idx_offsets_dev, src_buf=idx_offsets_host)
        ctxp[0].enqueue_copy(dst_buf=idx_lens_dev, src_buf=idx_lens_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var planp = alloc[GpuIdxPlanF64](1)
        planp[0] = GpuIdxPlanF64(
            idx_data=idx_data_dev,
            idx_offsets=idx_offsets_dev,
            idx_lens=idx_lens_dev,
            dims=dims_dev,
            ndim=ndim_i,
            idx_data_n=idx_data_n,
            out_n=out_n,
            full_n=full_n
        )
        return planp
    except:
        return _null_idxplan_f64()

fn _gpu_buf_f64_gather_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    plan_handle: MutGpuIdxPlanF64Ptr
) -> MutGpuBufF64Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_F64 or plan_handle == _null_idxplan_f64():
        return NULL_GPUBUF_F64
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_F64:
        return NULL_GPUBUF_F64
    if plan_handle[0].full_n != handle[0].len:
        return NULL_GPUBUF_F64
    if plan_handle[0].out_n <= 0:
        return NULL_GPUBUF_F64
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 8):
        return NULL_GPUBUF_F64

    try:
        var out_buf = ctxp[0].enqueue_create_buffer[DType.float64](plan_handle[0].out_n)
        gather_kernel_nd_f64(
            ctxp[0],
            in_bufp[0],
            out_buf,
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )

        var out_bufp = alloc[DeviceBuffer[DType.float64]](1)
        out_bufp[0] = out_buf
        var out_handle = alloc[GpuBufF64](1)
        out_handle[0] = GpuBufF64(out_bufp, plan_handle[0].out_n)
        return out_handle
    except:
        return NULL_GPUBUF_F64

fn _gpu_buf_f64_scatter_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    values_handle: MutGpuBufF64Ptr,
    plan_handle: MutGpuIdxPlanF64Ptr
) -> Int32:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_F64 or values_handle == NULL_GPUBUF_F64 or plan_handle == _null_idxplan_f64():
        return 0
    var dst_bufp = handle[0].buf
    var values_bufp = values_handle[0].buf
    if dst_bufp == NULL_BUF_F64 or values_bufp == NULL_BUF_F64:
        return 0
    if plan_handle[0].full_n != handle[0].len:
        return 0
    if values_handle[0].len != plan_handle[0].out_n:
        return 0
    if plan_handle[0].out_n <= 0:
        return 0
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 8):
        return 0

    try:
        scatter_kernel_nd_f64(
            ctxp[0],
            dst_bufp[0],
            values_bufp[0],
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )
        return 1
    except:
        return 0

fn _idxplan_i32_free_handle(plan_handle: MutGpuIdxPlanI32Ptr) -> Int32:
    if plan_handle == _null_idxplan_i32():
        return 0
    plan_handle.free()
    return 1

fn _idxplan_i32_new(
    ctxp: MutCtxPtr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuIdxPlanI32Ptr:
    if not gpu_ready(ctxp):
        return _null_idxplan_i32()

    var shape = _idxplan_validate_shape(
        idx_data_ptr,
        idx_offsets_ptr,
        idx_lens_ptr,
        dims_ptr,
        ndim
    )
    if not shape.valid:
        return _null_idxplan_i32()
    var ndim_i = shape.ndim_i
    var idx_data_n = shape.idx_data_n
    var out_n = shape.out_n
    var full_n = shape.full_n

    var idx_data_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_data_ptr.bitcast[Int32]()
    var idx_offsets_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_offsets_ptr.bitcast[Int32]()
    var idx_lens_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = idx_lens_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()
    try:
        var idx_data_host = ctxp[0].enqueue_create_host_buffer[DType.int32](idx_data_n)
        var idx_offsets_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var idx_lens_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)

        for i in range(idx_data_n):
            idx_data_host[i] = idx_data_raw[i]
        for axis in range(ndim_i):
            idx_offsets_host[axis] = idx_offsets_raw[axis]
            idx_lens_host[axis] = idx_lens_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var idx_data_dev = ctxp[0].enqueue_create_buffer[DType.int32](idx_data_n)
        var idx_offsets_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var idx_lens_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=idx_data_dev, src_buf=idx_data_host)
        ctxp[0].enqueue_copy(dst_buf=idx_offsets_dev, src_buf=idx_offsets_host)
        ctxp[0].enqueue_copy(dst_buf=idx_lens_dev, src_buf=idx_lens_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var planp = alloc[GpuIdxPlanI32](1)
        planp[0] = GpuIdxPlanI32(
            idx_data=idx_data_dev,
            idx_offsets=idx_offsets_dev,
            idx_lens=idx_lens_dev,
            dims=dims_dev,
            ndim=ndim_i,
            idx_data_n=idx_data_n,
            out_n=out_n,
            full_n=full_n
        )
        return planp
    except:
        return _null_idxplan_i32()

fn _gpu_buf_i32_gather_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    plan_handle: MutGpuIdxPlanI32Ptr
) -> MutGpuBufI32Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_I32 or plan_handle == _null_idxplan_i32():
        return NULL_GPUBUF_I32
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_I32:
        return NULL_GPUBUF_I32
    if plan_handle[0].full_n != handle[0].len:
        return NULL_GPUBUF_I32
    if plan_handle[0].out_n <= 0:
        return NULL_GPUBUF_I32
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 4):
        return NULL_GPUBUF_I32

    try:
        var out_buf = ctxp[0].enqueue_create_buffer[DType.int32](plan_handle[0].out_n)
        gather_kernel_nd_i32(
            ctxp[0],
            in_bufp[0],
            out_buf,
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )

        var out_bufp = alloc[DeviceBuffer[DType.int32]](1)
        out_bufp[0] = out_buf
        var out_handle = alloc[GpuBufI32](1)
        out_handle[0] = GpuBufI32(out_bufp, plan_handle[0].out_n)
        return out_handle
    except:
        return NULL_GPUBUF_I32

fn _gpu_buf_i32_scatter_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    values_handle: MutGpuBufI32Ptr,
    plan_handle: MutGpuIdxPlanI32Ptr
) -> Int32:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_I32 or values_handle == NULL_GPUBUF_I32 or plan_handle == _null_idxplan_i32():
        return 0
    var dst_bufp = handle[0].buf
    var values_bufp = values_handle[0].buf
    if dst_bufp == NULL_BUF_I32 or values_bufp == NULL_BUF_I32:
        return 0
    if plan_handle[0].full_n != handle[0].len:
        return 0
    if values_handle[0].len != plan_handle[0].out_n:
        return 0
    if plan_handle[0].out_n <= 0:
        return 0
    if not gpu_limit_ok(plan_handle[0].out_n, 2, 4):
        return 0

    try:
        scatter_kernel_nd_i32(
            ctxp[0],
            dst_bufp[0],
            values_bufp[0],
            plan_handle[0].out_n,
            plan_handle[0].idx_data.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_offsets.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].idx_lens.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].dims.unsafe_ptr().bitcast[Int32](),
            plan_handle[0].ndim
        )
        return 1
    except:
        return 0

fn _reduce_op_supported(op: Int32) -> Bool:
    return op == REDUCE_SUM or op == REDUCE_MIN or op == REDUCE_MAX or op == REDUCE_MEAN or op == REDUCE_PROD or op == REDUCE_ARGMIN or op == REDUCE_ARGMAX


@fieldwise_init
struct ReduceShapeInfo(Movable):
    var out_n: Int
    var red_n: Int
    var has_ndim: Bool
    var valid: Bool


fn _analyze_reduce_shape(n: Int, dims_ptr: ImmutOpaqueAny, ndim: Int32) -> ReduceShapeInfo:
    var dims_i = Int(ndim)
    if dims_i < 0 or n <= 0:
        return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=False, valid=False)

    var out_n = 1
    var red_n = 1
    var has_ndim = dims_i > 0
    if has_ndim:
        var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()
        for axis in range(dims_i):
            var raw = Int(dims_raw[axis])
            if raw == 0:
                return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=has_ndim, valid=False)
            var dim_abs = raw
            if dim_abs < 0:
                dim_abs = -dim_abs
                if dim_abs <= 0:
                    return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=has_ndim, valid=False)
                red_n *= dim_abs
            else:
                out_n *= dim_abs
            if dim_abs <= 0:
                return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=has_ndim, valid=False)
        if out_n <= 0 or red_n <= 0:
            return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=has_ndim, valid=False)
        if out_n * red_n != n:
            return ReduceShapeInfo(out_n=0, red_n=0, has_ndim=has_ndim, valid=False)

    return ReduceShapeInfo(out_n=out_n, red_n=red_n, has_ndim=has_ndim, valid=True)


fn _gpu_buf_reduce[dtype: DType](
    ctxp: MutCtxPtr,
    handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    op: Int32,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin]:
    if not gpu_ready(ctxp) or handle == _null_gpubuf[dtype]():
        return _null_gpubuf[dtype]()
    if not _reduce_op_supported(op):
        return _null_gpubuf[dtype]()

    var bufp = handle[0].buf
    if bufp == _null_buf[dtype]():
        return _null_gpubuf[dtype]()

    var n = handle[0].len
    var shape = _analyze_reduce_shape(n, dims_ptr, ndim)
    if not shape.valid:
        return _null_gpubuf[dtype]()

    var dims_i = Int(ndim)
    try:
        var out_buf = ctxp[0].enqueue_create_buffer[dtype](shape.out_n)
        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[dtype], origin=MutAnyOrigin] = alloc[DeviceBuffer[dtype]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin] = alloc[GpuBuf[dtype]](1)
        out_handle[0] = GpuBuf[dtype](out_bufp, shape.out_n)

        if shape.has_ndim:
            var dim_host = ctxp[0].enqueue_create_host_buffer[DType.int32](dims_i)
            var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()
            for axis in range(dims_i):
                dim_host[axis] = dims_raw[axis]
            var dim_buf = ctxp[0].enqueue_create_buffer[DType.int32](dims_i)
            ctxp[0].enqueue_copy(dst_buf=dim_buf, src_buf=dim_host)
            ctxp[0].synchronize()
            reduction_kernel_nd[dtype](
                ctxp[0],
                bufp[0],
                out_bufp[0],
                shape.out_n,
                dim_buf.unsafe_ptr().bitcast[Int32](),
                dims_i,
                shape.red_n,
                op
            )
        elif _reduce_is_arg_op(op) or op == REDUCE_PROD:
            var dim_host = ctxp[0].enqueue_create_host_buffer[DType.int32](1)
            dim_host[0] = Int32(-n)
            var dim_buf = ctxp[0].enqueue_create_buffer[DType.int32](1)
            ctxp[0].enqueue_copy(dst_buf=dim_buf, src_buf=dim_host)
            ctxp[0].synchronize()
            reduction_kernel_nd[dtype](
                ctxp[0],
                bufp[0],
                out_bufp[0],
                1,
                dim_buf.unsafe_ptr().bitcast[Int32](),
                1,
                n,
                op
            )
        else:
            reduction_kernel_1d[dtype](ctxp[0], bufp[0], out_bufp[0], n, op)
        return out_handle
    except:
        return _null_gpubuf[dtype]()


@export("mojor_gpu_buf_f64_reduce", ABI="C")
fn mojor_gpu_buf_f64_reduce(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    op: Int32,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32,
    keepdims: Int32
) -> MutGpuBufF64Ptr:
    _ = keepdims
    return _gpu_buf_reduce[DType.float64](ctxp, handle, op, dims_ptr, ndim)


# =============================================================================
# dim() - Return integer vector of array dimensions
# =============================================================================
# For vectors: returns c(length(x))
# For matrices: returns c(nrow(x), ncol(x))
# For arrays: returns c(dim1, dim2, ..., dimN)
# =============================================================================
# The dim() function is implemented as a runtime helper that returns
# an integer vector containing the dimensions of the input array.
# The dimensions are passed as separate parameters to the Mojo function.
# =============================================================================
# NOTE: The dim() function is currently implemented as a placeholder.
# The actual implementation requires access to the array's dimension metadata
# which is passed as __mojor_dim_x_ptr and __mojor_ndim_x parameters.
# For now, we emit a call to a helper that will be implemented in the
# transpiled code using the dimension variables that are already available.


@export("mojor_gpu_buf_f32_reduce", ABI="C")
fn mojor_gpu_buf_f32_reduce(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    op: Int32,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32,
    keepdims: Int32
) -> MutGpuBufF32Ptr:
    _ = keepdims
    return _gpu_buf_reduce[DType.float32](ctxp, handle, op, dims_ptr, ndim)

@export("mojor_gpu_buf_i32_reduce", ABI="C")
fn mojor_gpu_buf_i32_reduce(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    op: Int32,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32,
    keepdims: Int32
) -> MutGpuBufI32Ptr:
    _ = keepdims
    return _gpu_buf_reduce[DType.int32](ctxp, handle, op, dims_ptr, ndim)


fn _enqueue_matmul_col_major_f32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float32],
    b_buf: DeviceBuffer[DType.float32],
    c_buf: DeviceBuffer[DType.float32],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var out_n = m * n
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_matmul_kernel_col_major_f32, _matmul_kernel_col_major_f32](
        a_buf,
        b_buf,
        c_buf,
        m,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _matmul_kernel_col_major_f32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    var out_n = m * n
    if tid >= out_n:
        return
    var row = tid % m
    var col = tid // m

    var acc = Scalar[DType.float32](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[tid] = acc


fn _enqueue_matmul_col_major_f64(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float64],
    b_buf: DeviceBuffer[DType.float64],
    c_buf: DeviceBuffer[DType.float64],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var out_n = m * n
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_matmul_kernel_col_major_f64, _matmul_kernel_col_major_f64](
        a_buf,
        b_buf,
        c_buf,
        m,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _matmul_kernel_col_major_f64(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    var out_n = m * n
    if tid >= out_n:
        return
    var row = tid % m
    var col = tid // m

    var acc = Scalar[DType.float64](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[tid] = acc


fn _enqueue_gemv_col_major_f32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float32],
    b_buf: DeviceBuffer[DType.float32],
    c_buf: DeviceBuffer[DType.float32],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(m, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gemv_kernel_col_major_f32, _gemv_kernel_col_major_f32](
        a_buf,
        b_buf,
        c_buf,
        m,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gemv_kernel_col_major_f32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var row = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if row >= m:
        return

    var acc = Scalar[DType.float32](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = p * b_rows_orig
        else:
            b_idx = p
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[row] = acc


fn _enqueue_gemv_col_major_f64(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float64],
    b_buf: DeviceBuffer[DType.float64],
    c_buf: DeviceBuffer[DType.float64],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(m, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gemv_kernel_col_major_f64, _gemv_kernel_col_major_f64](
        a_buf,
        b_buf,
        c_buf,
        m,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gemv_kernel_col_major_f64(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var row = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if row >= m:
        return

    var acc = Scalar[DType.float64](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = p * b_rows_orig
        else:
            b_idx = p
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[row] = acc


fn _enqueue_gevm_col_major_f32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float32],
    b_buf: DeviceBuffer[DType.float32],
    c_buf: DeviceBuffer[DType.float32],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gevm_kernel_col_major_f32, _gevm_kernel_col_major_f32](
        a_buf,
        b_buf,
        c_buf,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gevm_kernel_col_major_f32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float32], origin=MutAnyOrigin],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var col = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if col >= n:
        return

    var acc = Scalar[DType.float32](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p
        else:
            a_idx = p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[col] = acc


fn _enqueue_gevm_col_major_f64(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.float64],
    b_buf: DeviceBuffer[DType.float64],
    c_buf: DeviceBuffer[DType.float64],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gevm_kernel_col_major_f64, _gevm_kernel_col_major_f64](
        a_buf,
        b_buf,
        c_buf,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gevm_kernel_col_major_f64(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.float64], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.float64], origin=MutAnyOrigin],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var col = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if col >= n:
        return

    var acc = Scalar[DType.float64](0.0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p
        else:
            a_idx = p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[col] = acc

fn _enqueue_matmul_col_major_i32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.int32],
    b_buf: DeviceBuffer[DType.int32],
    c_buf: DeviceBuffer[DType.int32],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var out_n = m * n
    var grid = max(1, ceildiv(out_n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_matmul_kernel_col_major_i32, _matmul_kernel_col_major_i32](
        a_buf,
        b_buf,
        c_buf,
        m,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _matmul_kernel_col_major_i32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    m: Int,
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var tid = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    var out_n = m * n
    if tid >= out_n:
        return
    var row = tid % m
    var col = tid // m

    var acc = Scalar[DType.int32](0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[tid] = acc


fn _enqueue_gemv_col_major_i32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.int32],
    b_buf: DeviceBuffer[DType.int32],
    c_buf: DeviceBuffer[DType.int32],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(m, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gemv_kernel_col_major_i32, _gemv_kernel_col_major_i32](
        a_buf,
        b_buf,
        c_buf,
        m,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gemv_kernel_col_major_i32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    m: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var row = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if row >= m:
        return

    var acc = Scalar[DType.int32](0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p + row * a_rows_orig
        else:
            a_idx = row + p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = p * b_rows_orig
        else:
            b_idx = p
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[row] = acc


fn _enqueue_gevm_col_major_i32(
    ctx: DeviceContext,
    a_buf: DeviceBuffer[DType.int32],
    b_buf: DeviceBuffer[DType.int32],
    c_buf: DeviceBuffer[DType.int32],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
) raises:
    var grid = max(1, ceildiv(n, REDUCE_BLOCK_SIZE))
    ctx.enqueue_function[_gevm_kernel_col_major_i32, _gevm_kernel_col_major_i32](
        a_buf,
        b_buf,
        c_buf,
        n,
        k,
        a_rows_orig,
        b_rows_orig,
        transpose_a,
        transpose_b,
        grid_dim=grid,
        block_dim=REDUCE_BLOCK_SIZE,
    )
    ctx.synchronize()


fn _gevm_kernel_col_major_i32(
    a_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    b_ptr: UnsafePointer[mut=False, type=Scalar[DType.int32], origin=ImmutAnyOrigin],
    c_ptr: UnsafePointer[mut=True, type=Scalar[DType.int32], origin=MutAnyOrigin],
    n: Int,
    k: Int,
    a_rows_orig: Int,
    b_rows_orig: Int,
    transpose_a: Int32,
    transpose_b: Int32
):
    var col = Int(block_idx.x) * Int(block_dim.x) + Int(thread_idx.x)
    if col >= n:
        return

    var acc = Scalar[DType.int32](0)
    for p in range(k):
        var a_idx = 0
        if transpose_a != 0:
            a_idx = p
        else:
            a_idx = p * a_rows_orig

        var b_idx = 0
        if transpose_b != 0:
            b_idx = col + p * b_rows_orig
        else:
            b_idx = p + col * b_rows_orig
        acc += a_ptr[a_idx] * b_ptr[b_idx]

    c_ptr[col] = acc


fn _gpu_matmul_supported[dtype: DType]() -> Bool:
    @parameter
    if dtype == DType.float32:
        return True
    if dtype == DType.float64:
        return True
    if dtype == DType.int32:
        return True
    return False

@export("mojor_gpu_cap_f64_matmul", ABI="C")
fn mojor_gpu_cap_f64_matmul(ctxp: MutCtxPtr) -> Int32:
    if not gpu_ready(ctxp):
        return 0
    if not _gpu_matmul_supported[DType.float64]():
        return 0
    if not gpu_limit_ok_matmul(1, 1, 1, _gpu_dtype_bytes[DType.float64]()):
        return 0
    return 1

@export("mojor_gpu_cap_f64_reduce", ABI="C")
fn mojor_gpu_cap_f64_reduce(ctxp: MutCtxPtr, op: Int32) -> Int32:
    if not gpu_ready(ctxp):
        return 0
    if not _reduce_op_supported(op):
        return 0
    if not gpu_limit_ok(1, 2, 8):
        return 0
    return 1

@export("mojor_gpu_cap_i32_scatter", ABI="C")
fn mojor_gpu_cap_i32_scatter(ctxp: MutCtxPtr) -> Int32:
    if not gpu_ready(ctxp):
        return 0
    if not gpu_limit_ok(1, 2, 4):
        return 0
    return 1


fn _gpu_dtype_bytes[dtype: DType]() -> Int:
    @parameter
    if dtype == DType.float32:
        return 4
    if dtype == DType.int32:
        return 4
    return 8


@fieldwise_init
struct GpuMatmulInfo[dtype: DType](Movable):
    var valid: Bool
    var m_i: Int
    var n_i: Int
    var k_i: Int
    var buf_a: UnsafePointer[mut=True, type=DeviceBuffer[Self.dtype], origin=MutAnyOrigin]
    var buf_b: UnsafePointer[mut=True, type=DeviceBuffer[Self.dtype], origin=MutAnyOrigin]


fn _gpu_matmul_inputs[dtype: DType](
    ctxp: MutCtxPtr,
    handle_a: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    handle_b: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    m: Int32,
    k: Int32,
    n: Int32
) -> GpuMatmulInfo[dtype]:
    if not _gpu_matmul_supported[dtype]():
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())
    if not gpu_ready(ctxp) or handle_a == _null_gpubuf[dtype]() or handle_b == _null_gpubuf[dtype]():
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())

    var m_i = Int(m)
    var n_i = Int(n)
    var k_i = Int(k)
    if m_i <= 0 or n_i <= 0 or k_i <= 0:
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())
    if not gpu_limit_ok_matmul(m_i, k_i, n_i, _gpu_dtype_bytes[dtype]()):
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())

    var buf_a = handle_a[0].buf
    var buf_b = handle_b[0].buf
    if buf_a == _null_buf[dtype]() or buf_b == _null_buf[dtype]():
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())
    if handle_a[0].len < m_i * k_i or handle_b[0].len < k_i * n_i:
        return GpuMatmulInfo[dtype](valid=False, m_i=0, n_i=0, k_i=0, buf_a=_null_buf[dtype](), buf_b=_null_buf[dtype]())

    return GpuMatmulInfo[dtype](valid=True, m_i=m_i, n_i=n_i, k_i=k_i, buf_a=buf_a, buf_b=buf_b)


fn _gpu_matmul_out_valid[dtype: DType](
    handle_out: UnsafePointer[mut=True, type=GpuBuf[dtype], origin=MutAnyOrigin],
    m_i: Int,
    n_i: Int
) -> Bool:
    if handle_out == _null_gpubuf[dtype]():
        return False
    var buf_out = handle_out[0].buf
    if buf_out == _null_buf[dtype]():
        return False
    return handle_out[0].len >= m_i * n_i


@export("mojor_gpu_buf_f32_matmul", ABI="C")
fn mojor_gpu_buf_f32_matmul(
    ctxp: MutCtxPtr,
    handle_a: MutGpuBufF32Ptr,
    handle_b: MutGpuBufF32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> MutGpuBufF32Ptr:
    var info = _gpu_matmul_inputs[DType.float32](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return NULL_GPUBUF
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var out_len = info.m_i * info.n_i
        var out_buf = ctxp[0].enqueue_create_buffer[DType.float32](out_len)
        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.float32]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin] = alloc[GpuBufF32](1)
        out_handle[0] = GpuBufF32(out_bufp, out_len)

        if info.n_i == 1:
            _enqueue_gemv_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return out_handle
    except:
        return NULL_GPUBUF


@export("mojor_gpu_buf_f32_matmul_into", ABI="C")
fn mojor_gpu_buf_f32_matmul_into(
    ctxp: MutCtxPtr,
    handle_out: MutGpuBufF32Ptr,
    handle_a: MutGpuBufF32Ptr,
    handle_b: MutGpuBufF32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> Int32:
    var info = _gpu_matmul_inputs[DType.float32](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return 0
    if not _gpu_matmul_out_valid[DType.float32](handle_out, info.m_i, info.n_i):
        return 0
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var buf_out = handle_out[0].buf
        if info.n_i == 1:
            _enqueue_gemv_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_f32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return 1
    except:
        return 0


@export("mojor_gpu_buf_f64_matmul", ABI="C")
fn mojor_gpu_buf_f64_matmul(
    ctxp: MutCtxPtr,
    handle_a: MutGpuBufF64Ptr,
    handle_b: MutGpuBufF64Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> MutGpuBufF64Ptr:
    var info = _gpu_matmul_inputs[DType.float64](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return NULL_GPUBUF_F64
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var out_len = info.m_i * info.n_i
        var out_buf = ctxp[0].enqueue_create_buffer[DType.float64](out_len)
        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.float64]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufF64, origin=MutAnyOrigin] = alloc[GpuBufF64](1)
        out_handle[0] = GpuBufF64(out_bufp, out_len)

        if info.n_i == 1:
            _enqueue_gemv_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return out_handle
    except:
        return NULL_GPUBUF_F64


@export("mojor_gpu_buf_f64_matmul_into", ABI="C")
fn mojor_gpu_buf_f64_matmul_into(
    ctxp: MutCtxPtr,
    handle_out: MutGpuBufF64Ptr,
    handle_a: MutGpuBufF64Ptr,
    handle_b: MutGpuBufF64Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> Int32:
    var info = _gpu_matmul_inputs[DType.float64](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return 0
    if not _gpu_matmul_out_valid[DType.float64](handle_out, info.m_i, info.n_i):
        return 0
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var buf_out = handle_out[0].buf
        if info.n_i == 1:
            _enqueue_gemv_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_f64(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return 1
    except:
        return 0

@export("mojor_gpu_buf_i32_matmul", ABI="C")
fn mojor_gpu_buf_i32_matmul(
    ctxp: MutCtxPtr,
    handle_a: MutGpuBufI32Ptr,
    handle_b: MutGpuBufI32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> MutGpuBufI32Ptr:
    var info = _gpu_matmul_inputs[DType.int32](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return NULL_GPUBUF_I32
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var out_len = info.m_i * info.n_i
        var out_buf = ctxp[0].enqueue_create_buffer[DType.int32](out_len)
        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.int32], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.int32]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufI32, origin=MutAnyOrigin] = alloc[GpuBufI32](1)
        out_handle[0] = GpuBufI32(out_bufp, out_len)

        if info.n_i == 1:
            _enqueue_gemv_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                out_bufp[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return out_handle
    except:
        return NULL_GPUBUF_I32

@export("mojor_gpu_buf_i32_matmul_into", ABI="C")
fn mojor_gpu_buf_i32_matmul_into(
    ctxp: MutCtxPtr,
    handle_out: MutGpuBufI32Ptr,
    handle_a: MutGpuBufI32Ptr,
    handle_b: MutGpuBufI32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> Int32:
    var info = _gpu_matmul_inputs[DType.int32](ctxp, handle_a, handle_b, m, k, n)
    if not info.valid:
        return 0
    if not _gpu_matmul_out_valid[DType.int32](handle_out, info.m_i, info.n_i):
        return 0
    var a_rows_orig = info.m_i
    if transpose_a != 0:
        a_rows_orig = info.k_i
    var b_rows_orig = info.k_i
    if transpose_b != 0:
        b_rows_orig = info.n_i

    try:
        var buf_out = handle_out[0].buf
        if info.n_i == 1:
            _enqueue_gemv_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        elif info.m_i == 1:
            _enqueue_gevm_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        else:
            _enqueue_matmul_col_major_i32(
                ctxp[0],
                info.buf_a[0],
                info.buf_b[0],
                buf_out[0],
                info.m_i,
                info.n_i,
                info.k_i,
                a_rows_orig,
                b_rows_orig,
                transpose_a,
                transpose_b
            )
        return 1
    except:
        return 0


@export("mojor_gpu_buf_f32_slice", ABI="C")
fn mojor_gpu_buf_f32_slice(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    starts_ptr: ImmutOpaqueAny,
    sizes_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufF32Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF:
        return NULL_GPUBUF
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_F32:
        return NULL_GPUBUF

    var ndim_i = Int(ndim)
    if ndim_i <= 0:
        return NULL_GPUBUF

    var starts_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = starts_ptr.bitcast[Int32]()
    var sizes_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = sizes_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()

    var out_n = 1
    for axis in range(ndim_i):
        var start = Int(starts_raw[axis])
        var size = Int(sizes_raw[axis])
        var dim = Int(dims_raw[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return NULL_GPUBUF
        if start + size > dim:
            return NULL_GPUBUF
        out_n *= size
    if out_n <= 0 or out_n > handle[0].len:
        return NULL_GPUBUF
    if not gpu_limit_ok(out_n, 1, 4):
        return NULL_GPUBUF

    try:
        var starts_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var sizes_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        for axis in range(ndim_i):
            starts_host[axis] = starts_raw[axis]
            sizes_host[axis] = sizes_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var starts_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var sizes_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=starts_dev, src_buf=starts_host)
        ctxp[0].enqueue_copy(dst_buf=sizes_dev, src_buf=sizes_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var out_buf = ctxp[0].enqueue_create_buffer[DType.float32](out_n)
        slice_kernel_nd_f32(
            ctxp[0],
            in_bufp[0],
            out_buf,
            out_n,
            starts_dev.unsafe_ptr().bitcast[Int32](),
            sizes_dev.unsafe_ptr().bitcast[Int32](),
            dims_dev.unsafe_ptr().bitcast[Int32](),
            ndim_i
        )

        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.float32], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.float32]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufF32, origin=MutAnyOrigin] = alloc[GpuBufF32](1)
        out_handle[0] = GpuBufF32(out_bufp, out_n)
        return out_handle
    except:
        return NULL_GPUBUF

@export("mojor_gpu_buf_f32_gather", ABI="C")
fn mojor_gpu_buf_f32_gather(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufF32Ptr:
    var plan = _idxplan_f32_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_f32():
        return NULL_GPUBUF
    var out = _gpu_buf_f32_gather_plan(ctxp, handle, plan)
    _ = _idxplan_f32_free_handle(plan)
    return out

@export("mojor_gpu_buf_f32_scatter", ABI="C")
fn mojor_gpu_buf_f32_scatter(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    values_handle: MutGpuBufF32Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> Int32:
    var plan = _idxplan_f32_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_f32():
        return 0
    var ok = _gpu_buf_f32_scatter_plan(ctxp, handle, values_handle, plan)
    _ = _idxplan_f32_free_handle(plan)
    return ok

@export("mojor_gpu_idx_plan_f32_create", ABI="C")
fn mojor_gpu_idx_plan_f32_create(
    ctxp: MutCtxPtr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuIdxPlanF32Ptr:
    return _idxplan_f32_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)

@export("mojor_gpu_idx_plan_f32_free", ABI="C")
fn mojor_gpu_idx_plan_f32_free(
    _ctxp: MutCtxPtr,
    plan_handle: MutGpuIdxPlanF32Ptr
) -> Int32:
    return _idxplan_f32_free_handle(plan_handle)

@export("mojor_gpu_buf_f32_gather_plan", ABI="C")
fn mojor_gpu_buf_f32_gather_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    plan_handle: MutGpuIdxPlanF32Ptr
) -> MutGpuBufF32Ptr:
    return _gpu_buf_f32_gather_plan(ctxp, handle, plan_handle)

@export("mojor_gpu_buf_f32_scatter_plan", ABI="C")
fn mojor_gpu_buf_f32_scatter_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    values_handle: MutGpuBufF32Ptr,
    plan_handle: MutGpuIdxPlanF32Ptr
) -> Int32:
    return _gpu_buf_f32_scatter_plan(ctxp, handle, values_handle, plan_handle)

@export("mojor_gpu_buf_f64_slice", ABI="C")
fn mojor_gpu_buf_f64_slice(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    starts_ptr: ImmutOpaqueAny,
    sizes_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufF64Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_F64:
        return NULL_GPUBUF_F64
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_F64:
        return NULL_GPUBUF_F64

    var ndim_i = Int(ndim)
    if ndim_i <= 0:
        return NULL_GPUBUF_F64

    var starts_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = starts_ptr.bitcast[Int32]()
    var sizes_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = sizes_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()

    var out_n = 1
    for axis in range(ndim_i):
        var start = Int(starts_raw[axis])
        var size = Int(sizes_raw[axis])
        var dim = Int(dims_raw[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return NULL_GPUBUF_F64
        if start + size > dim:
            return NULL_GPUBUF_F64
        out_n *= size
    if out_n <= 0 or out_n > handle[0].len:
        return NULL_GPUBUF_F64
    if not gpu_limit_ok(out_n, 1, 8):
        return NULL_GPUBUF_F64

    try:
        var starts_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var sizes_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        for axis in range(ndim_i):
            starts_host[axis] = starts_raw[axis]
            sizes_host[axis] = sizes_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var starts_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var sizes_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=starts_dev, src_buf=starts_host)
        ctxp[0].enqueue_copy(dst_buf=sizes_dev, src_buf=sizes_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var out_buf = ctxp[0].enqueue_create_buffer[DType.float64](out_n)
        slice_kernel_nd_f64(
            ctxp[0],
            in_bufp[0],
            out_buf,
            out_n,
            starts_dev.unsafe_ptr().bitcast[Int32](),
            sizes_dev.unsafe_ptr().bitcast[Int32](),
            dims_dev.unsafe_ptr().bitcast[Int32](),
            ndim_i
        )

        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.float64], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.float64]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufF64, origin=MutAnyOrigin] = alloc[GpuBufF64](1)
        out_handle[0] = GpuBufF64(out_bufp, out_n)
        return out_handle
    except:
        return NULL_GPUBUF_F64

@export("mojor_gpu_buf_f64_gather", ABI="C")
fn mojor_gpu_buf_f64_gather(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufF64Ptr:
    var plan = _idxplan_f64_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_f64():
        return NULL_GPUBUF_F64
    var out = _gpu_buf_f64_gather_plan(ctxp, handle, plan)
    _ = _idxplan_f64_free_handle(plan)
    return out

@export("mojor_gpu_buf_f64_scatter", ABI="C")
fn mojor_gpu_buf_f64_scatter(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    values_handle: MutGpuBufF64Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> Int32:
    var plan = _idxplan_f64_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_f64():
        return 0
    var ok = _gpu_buf_f64_scatter_plan(ctxp, handle, values_handle, plan)
    _ = _idxplan_f64_free_handle(plan)
    return ok

@export("mojor_gpu_idx_plan_f64_create", ABI="C")
fn mojor_gpu_idx_plan_f64_create(
    ctxp: MutCtxPtr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuIdxPlanF64Ptr:
    return _idxplan_f64_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)

@export("mojor_gpu_idx_plan_f64_free", ABI="C")
fn mojor_gpu_idx_plan_f64_free(
    _ctxp: MutCtxPtr,
    plan_handle: MutGpuIdxPlanF64Ptr
) -> Int32:
    return _idxplan_f64_free_handle(plan_handle)

@export("mojor_gpu_buf_f64_gather_plan", ABI="C")
fn mojor_gpu_buf_f64_gather_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    plan_handle: MutGpuIdxPlanF64Ptr
) -> MutGpuBufF64Ptr:
    return _gpu_buf_f64_gather_plan(ctxp, handle, plan_handle)

@export("mojor_gpu_buf_f64_scatter_plan", ABI="C")
fn mojor_gpu_buf_f64_scatter_plan(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    values_handle: MutGpuBufF64Ptr,
    plan_handle: MutGpuIdxPlanF64Ptr
) -> Int32:
    return _gpu_buf_f64_scatter_plan(ctxp, handle, values_handle, plan_handle)

@export("mojor_gpu_buf_i32_slice", ABI="C")
fn mojor_gpu_buf_i32_slice(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    starts_ptr: ImmutOpaqueAny,
    sizes_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufI32Ptr:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_I32:
        return NULL_GPUBUF_I32
    var in_bufp = handle[0].buf
    if in_bufp == NULL_BUF_I32:
        return NULL_GPUBUF_I32

    var ndim_i = Int(ndim)
    if ndim_i <= 0:
        return NULL_GPUBUF_I32

    var starts_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = starts_ptr.bitcast[Int32]()
    var sizes_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = sizes_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()

    var out_n = 1
    for axis in range(ndim_i):
        var start = Int(starts_raw[axis])
        var size = Int(sizes_raw[axis])
        var dim = Int(dims_raw[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return NULL_GPUBUF_I32
        if start + size > dim:
            return NULL_GPUBUF_I32
        out_n *= size
    if out_n <= 0 or out_n > handle[0].len:
        return NULL_GPUBUF_I32
    if not gpu_limit_ok(out_n, 1, 4):
        return NULL_GPUBUF_I32

    try:
        var starts_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var sizes_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        for axis in range(ndim_i):
            starts_host[axis] = starts_raw[axis]
            sizes_host[axis] = sizes_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var starts_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var sizes_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=starts_dev, src_buf=starts_host)
        ctxp[0].enqueue_copy(dst_buf=sizes_dev, src_buf=sizes_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        var out_buf = ctxp[0].enqueue_create_buffer[DType.int32](out_n)
        slice_kernel_nd_i32(
            ctxp[0],
            in_bufp[0],
            out_buf,
            out_n,
            starts_dev.unsafe_ptr().bitcast[Int32](),
            sizes_dev.unsafe_ptr().bitcast[Int32](),
            dims_dev.unsafe_ptr().bitcast[Int32](),
            ndim_i
        )

        var out_bufp: UnsafePointer[mut=True, type=DeviceBuffer[DType.int32], origin=MutAnyOrigin] = alloc[DeviceBuffer[DType.int32]](1)
        out_bufp[0] = out_buf
        var out_handle: UnsafePointer[mut=True, type=GpuBufI32, origin=MutAnyOrigin] = alloc[GpuBufI32](1)
        out_handle[0] = GpuBufI32(out_bufp, out_n)
        return out_handle
    except:
        return NULL_GPUBUF_I32

@export("mojor_gpu_buf_i32_slice_assign", ABI="C")
fn mojor_gpu_buf_i32_slice_assign(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    values_handle: MutGpuBufI32Ptr,
    starts_ptr: ImmutOpaqueAny,
    sizes_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> Int32:
    if not gpu_ready(ctxp) or handle == NULL_GPUBUF_I32 or values_handle == NULL_GPUBUF_I32:
        return 0
    var dst_bufp = handle[0].buf
    var values_bufp = values_handle[0].buf
    if dst_bufp == NULL_BUF_I32 or values_bufp == NULL_BUF_I32:
        return 0

    var ndim_i = Int(ndim)
    if ndim_i <= 0:
        return 0

    var starts_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = starts_ptr.bitcast[Int32]()
    var sizes_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = sizes_ptr.bitcast[Int32]()
    var dims_raw: UnsafePointer[mut=False, type=Int32, origin=ImmutAnyOrigin] = dims_ptr.bitcast[Int32]()

    var values_n = 1
    for axis in range(ndim_i):
        var start = Int(starts_raw[axis])
        var size = Int(sizes_raw[axis])
        var dim = Int(dims_raw[axis])
        if start < 0 or size <= 0 or dim <= 0:
            return 0
        if start + size > dim:
            return 0
        values_n *= size
    if values_n <= 0 or values_n > handle[0].len:
        return 0
    if values_handle[0].len != values_n:
        return 0
    if not gpu_limit_ok(values_n, 2, 4):
        return 0

    try:
        var starts_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var sizes_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        var dims_host = ctxp[0].enqueue_create_host_buffer[DType.int32](ndim_i)
        for axis in range(ndim_i):
            starts_host[axis] = starts_raw[axis]
            sizes_host[axis] = sizes_raw[axis]
            dims_host[axis] = dims_raw[axis]

        var starts_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var sizes_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        var dims_dev = ctxp[0].enqueue_create_buffer[DType.int32](ndim_i)
        ctxp[0].enqueue_copy(dst_buf=starts_dev, src_buf=starts_host)
        ctxp[0].enqueue_copy(dst_buf=sizes_dev, src_buf=sizes_host)
        ctxp[0].enqueue_copy(dst_buf=dims_dev, src_buf=dims_host)
        ctxp[0].synchronize()

        slice_assign_kernel_nd_i32(
            ctxp[0],
            dst_bufp[0],
            values_bufp[0],
            values_n,
            starts_dev.unsafe_ptr().bitcast[Int32](),
            sizes_dev.unsafe_ptr().bitcast[Int32](),
            dims_dev.unsafe_ptr().bitcast[Int32](),
            ndim_i
        )
        return 1
    except:
        return 0

@export("mojor_gpu_buf_i32_gather", ABI="C")
fn mojor_gpu_buf_i32_gather(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> MutGpuBufI32Ptr:
    var plan = _idxplan_i32_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_i32():
        return NULL_GPUBUF_I32
    var out = _gpu_buf_i32_gather_plan(ctxp, handle, plan)
    _ = _idxplan_i32_free_handle(plan)
    return out

@export("mojor_gpu_buf_i32_scatter", ABI="C")
fn mojor_gpu_buf_i32_scatter(
    ctxp: MutCtxPtr,
    handle: MutGpuBufI32Ptr,
    values_handle: MutGpuBufI32Ptr,
    idx_data_ptr: ImmutOpaqueAny,
    idx_offsets_ptr: ImmutOpaqueAny,
    idx_lens_ptr: ImmutOpaqueAny,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32
) -> Int32:
    var plan = _idxplan_i32_new(ctxp, idx_data_ptr, idx_offsets_ptr, idx_lens_ptr, dims_ptr, ndim)
    if plan == _null_idxplan_i32():
        return 0
    var ok = _gpu_buf_i32_scatter_plan(ctxp, handle, values_handle, plan)
    _ = _idxplan_i32_free_handle(plan)
    return ok


# Runtime v1 compatibility exports (forwarders).
@export("mojor_rt_v1_sum_f64", ABI="C")
fn mojor_rt_v1_sum_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    return mojor_sum_f64(ptr, n)

@export("mojor_rt_v1_prod_f64", ABI="C")
fn mojor_rt_v1_prod_f64(ptr: ImmutOpaqueAny, n: Int32) -> Float64:
    return mojor_prod_f64(ptr, n)

@export("mojor_rt_v1_gpu_ctx_create", ABI="C")
fn mojor_rt_v1_gpu_ctx_create() -> MutCtxPtr:
    return mojor_gpu_ctx_create()

@export("mojor_rt_v1_gpu_ctx_free", ABI="C")
fn mojor_rt_v1_gpu_ctx_free(ctxp: MutCtxPtr) -> Int32:
    return mojor_gpu_ctx_free(ctxp)

@export("mojor_rt_v1_has_gpu", ABI="C")
fn mojor_rt_v1_has_gpu() -> Int32:
    return mojor_has_gpu()

@export("mojor_rt_v1_gpu_meminfo", ABI="C")
fn mojor_rt_v1_gpu_meminfo(ctxp: MutCtxPtr, out_ptr: MutOpaqueAny) -> Int32:
    return mojor_gpu_meminfo(ctxp, out_ptr)

@export("mojor_rt_v1_gpu_buf_f32_alloc", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_alloc(ctxp: MutCtxPtr, n: Int32) -> MutGpuBufF32Ptr:
    return mojor_gpu_buf_f32_alloc(ctxp, n)

@export("mojor_rt_v1_gpu_buf_f32_free", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_free(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr) -> Int32:
    return mojor_gpu_buf_f32_free(ctxp, handle)

@export("mojor_rt_v1_gpu_buf_f32_len", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_len(ctxp: MutCtxPtr, handle: MutGpuBufF32Ptr) -> Int32:
    return mojor_gpu_buf_f32_len(ctxp, handle)

@export("mojor_rt_v1_gpu_buf_f32_write", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_write(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    host_ptr: ImmutOpaqueAny,
    n: Int32
) -> Int32:
    return mojor_gpu_buf_f32_write(ctxp, handle, host_ptr, n)

@export("mojor_rt_v1_gpu_buf_f32_read", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_read(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF32Ptr,
    host_ptr: MutOpaqueAny,
    n: Int32
) -> Int32:
    return mojor_gpu_buf_f32_read(ctxp, handle, host_ptr, n)

@export("mojor_rt_v1_gpu_buf_f64_alloc", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_alloc(ctxp: MutCtxPtr, n: Int32) -> MutGpuBufF64Ptr:
    return mojor_gpu_buf_f64_alloc(ctxp, n)

@export("mojor_rt_v1_gpu_buf_f64_free", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_free(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr) -> Int32:
    return mojor_gpu_buf_f64_free(ctxp, handle)

@export("mojor_rt_v1_gpu_buf_f64_len", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_len(ctxp: MutCtxPtr, handle: MutGpuBufF64Ptr) -> Int32:
    return mojor_gpu_buf_f64_len(ctxp, handle)

@export("mojor_rt_v1_gpu_buf_f64_write", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_write(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    host_ptr: ImmutOpaqueAny,
    n: Int32
) -> Int32:
    return mojor_gpu_buf_f64_write(ctxp, handle, host_ptr, n)

@export("mojor_rt_v1_gpu_buf_f64_read", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_read(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    host_ptr: MutOpaqueAny,
    n: Int32
) -> Int32:
    return mojor_gpu_buf_f64_read(ctxp, handle, host_ptr, n)

@export("mojor_rt_v1_gpu_buf_f64_reduce", ABI="C")
fn mojor_rt_v1_gpu_buf_f64_reduce(
    ctxp: MutCtxPtr,
    handle: MutGpuBufF64Ptr,
    op: Int32,
    dims_ptr: ImmutOpaqueAny,
    ndim: Int32,
    keepdims: Int32
) -> MutGpuBufF64Ptr:
    return mojor_gpu_buf_f64_reduce(ctxp, handle, op, dims_ptr, ndim, keepdims)

@export("mojor_rt_v1_gpu_buf_f32_matmul", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_matmul(
    ctxp: MutCtxPtr,
    handle_a: MutGpuBufF32Ptr,
    handle_b: MutGpuBufF32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> MutGpuBufF32Ptr:
    return mojor_gpu_buf_f32_matmul(ctxp, handle_a, handle_b, m, k, n, transpose_a, transpose_b)

@export("mojor_rt_v1_gpu_buf_f32_matmul_into", ABI="C")
fn mojor_rt_v1_gpu_buf_f32_matmul_into(
    ctxp: MutCtxPtr,
    handle_out: MutGpuBufF32Ptr,
    handle_a: MutGpuBufF32Ptr,
    handle_b: MutGpuBufF32Ptr,
    m: Int32,
    k: Int32,
    n: Int32,
    transpose_a: Int32,
    transpose_b: Int32
) -> Int32:
    return mojor_gpu_buf_f32_matmul_into(ctxp, handle_out, handle_a, handle_b, m, k, n, transpose_a, transpose_b)
