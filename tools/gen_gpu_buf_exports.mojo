from pathlib import Path
from sys import argv

fn gen_for(suffix: String, dtype: String, scalar: String, ptr: String, buf_ptr: String, bytes_name: String) -> String:
    var out = ""
    out += "@export(\"mojor_gpu_buf_" + suffix + "_alloc\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_alloc(ctxp: MutCtxPtr, n: Int32) -> " + ptr + ":\n"
    out += "    return _gpu_buf_export_alloc[" + dtype + "](ctxp, n, " + bytes_name + ")\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_free\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_free(ctxp: MutCtxPtr, handle: " + ptr + ") -> Int32:\n"
    out += "    return _gpu_buf_export_free[" + dtype + "](ctxp, handle)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_len\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_len(ctxp: MutCtxPtr, handle: " + ptr + ") -> Int32:\n"
    out += "    return _gpu_buf_export_len[" + dtype + "](ctxp, handle)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_write\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_write(ctxp: MutCtxPtr, handle: " + ptr + ", host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:\n"
    out += "    return _gpu_buf_export_write[" + dtype + "](ctxp, handle, host_ptr, n)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_read\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_read(ctxp: MutCtxPtr, handle: " + ptr + ", host_ptr: MutOpaqueAny, n: Int32) -> Int32:\n"
    out += "    return _gpu_buf_export_read[" + dtype + "](ctxp, handle, host_ptr, n)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_ptr\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_ptr(handle: " + ptr + ") -> " + buf_ptr + ":\n"
    out += "    return _gpu_buf_export_ptr[" + dtype + "](handle)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_affine\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_affine(\n"
    out += "    ctxp: MutCtxPtr,\n"
    out += "    handle: " + ptr + ",\n"
    out += "    scale: " + scalar + ",\n"
    out += "    bias: " + scalar + ",\n"
    out += "    status_ptr: MutOpaqueAny\n"
    out += ") -> " + ptr + ":\n"
    out += "    return _gpu_buf_export_affine[" + dtype + "](ctxp, handle, scale, bias, status_ptr, " + bytes_name + ")\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_chain\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_chain(\n"
    out += "    ctxp: MutCtxPtr,\n"
    out += "    handle: " + ptr + ",\n"
    out += "    iters: Int32,\n"
    out += "    scale: " + scalar + ",\n"
    out += "    bias: " + scalar + ",\n"
    out += "    post_scale: " + scalar + ",\n"
    out += "    post_bias: " + scalar + ",\n"
    out += "    post_iters: Int32,\n"
    out += "    status_ptr: MutOpaqueAny\n"
    out += ") -> " + ptr + ":\n"
    out += "    return _gpu_buf_export_chain[" + dtype + "](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, " + bytes_name + ")\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_chain_sum\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_chain_sum(\n"
    out += "    ctxp: MutCtxPtr,\n"
    out += "    handle: " + ptr + ",\n"
    out += "    iters: Int32,\n"
    out += "    scale: " + scalar + ",\n"
    out += "    bias: " + scalar + ",\n"
    out += "    post_scale: " + scalar + ",\n"
    out += "    post_bias: " + scalar + ",\n"
    out += "    post_iters: Int32,\n"
    out += "    status_ptr: MutOpaqueAny\n"
    out += ") -> " + scalar + ":\n"
    out += "    return _gpu_buf_export_chain_sum[" + dtype + "](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, " + bytes_name + ", False)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_chain_sum_warp\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_chain_sum_warp(\n"
    out += "    ctxp: MutCtxPtr,\n"
    out += "    handle: " + ptr + ",\n"
    out += "    iters: Int32,\n"
    out += "    scale: " + scalar + ",\n"
    out += "    bias: " + scalar + ",\n"
    out += "    post_scale: " + scalar + ",\n"
    out += "    post_bias: " + scalar + ",\n"
    out += "    post_iters: Int32,\n"
    out += "    status_ptr: MutOpaqueAny\n"
    out += ") -> " + scalar + ":\n"
    out += "    return _gpu_buf_export_chain_sum[" + dtype + "](ctxp, handle, iters, scale, bias, post_scale, post_bias, post_iters, status_ptr, " + bytes_name + ", True)\n\n"
    return out

fn gen_basic_for(suffix: String, dtype: String, ptr: String, buf_ptr: String, bytes_name: String) -> String:
    var out = ""
    out += "@export(\"mojor_gpu_buf_" + suffix + "_alloc\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_alloc(ctxp: MutCtxPtr, n: Int32) -> " + ptr + ":\n"
    out += "    return _gpu_buf_export_alloc[" + dtype + "](ctxp, n, " + bytes_name + ")\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_free\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_free(ctxp: MutCtxPtr, handle: " + ptr + ") -> Int32:\n"
    out += "    return _gpu_buf_export_free[" + dtype + "](ctxp, handle)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_len\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_len(ctxp: MutCtxPtr, handle: " + ptr + ") -> Int32:\n"
    out += "    return _gpu_buf_export_len[" + dtype + "](ctxp, handle)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_write\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_write(ctxp: MutCtxPtr, handle: " + ptr + ", host_ptr: ImmutOpaqueAny, n: Int32) -> Int32:\n"
    out += "    return _gpu_buf_export_write[" + dtype + "](ctxp, handle, host_ptr, n)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_read\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_read(ctxp: MutCtxPtr, handle: " + ptr + ", host_ptr: MutOpaqueAny, n: Int32) -> Int32:\n"
    out += "    return _gpu_buf_export_read[" + dtype + "](ctxp, handle, host_ptr, n)\n\n"

    out += "@export(\"mojor_gpu_buf_" + suffix + "_ptr\", ABI=\"C\")\n"
    out += "fn mojor_gpu_buf_" + suffix + "_ptr(handle: " + ptr + ") -> " + buf_ptr + ":\n"
    out += "    return _gpu_buf_export_ptr[" + dtype + "](handle)\n\n"
    return out


fn gen_buf_block() -> String:
    var begin = "# BEGIN GENERATED GPU_BUF_EXPORTS"
    var end_marker = "# END GENERATED GPU_BUF_EXPORTS"
    var out = ""
    out += begin + "\n"
    out += "# NOTE: generated by tools/gen_gpu_buf_exports.mojo\n"
    out += gen_for("f32", "DType.float32", "Float32", "MutGpuBufF32Ptr", "MutBufF32Ptr", "GPU_BUF_BYTES_F32")
    out += gen_for("f64", "DType.float64", "Float64", "MutGpuBufF64Ptr", "MutBufF64Ptr", "GPU_BUF_BYTES_F64")
    out += gen_basic_for("i32", "DType.int32", "MutGpuBufI32Ptr", "MutBufI32Ptr", "GPU_BUF_BYTES_I32")
    out += end_marker + "\n"
    return out


fn replace_block(text: String, begin: String, end_marker: String, block: String) -> String:
    var start = text.find(begin)
    var end = text.find(end_marker)
    if start < 0 or end < 0 or end < start:
        return text
    var pre = text[0:start]
    var after = text[end + len(end_marker):]
    if after.startswith("\n"):
        after = after[1:]
    return pre + block + after


fn main():
    var args = argv()
    var path = "packages/mojor/src/backend.mojo"
    if len(args) > 1:
        path = String(args[1])

    try:
        var file = Path(path)
        if not file.exists():
            print("gen_gpu_buf_exports: file not found: " + path)
            return
        var text = file.read_text()
        var buf_block = gen_buf_block()
        var out = replace_block(text, "# BEGIN GENERATED GPU_BUF_EXPORTS", "# END GENERATED GPU_BUF_EXPORTS", buf_block)
        if out == text:
            print("gen_gpu_buf_exports: markers not found, no changes")
            return
        file.write_text(out)
        print("gen_gpu_buf_exports: updated " + path)
    except:
        print("gen_gpu_buf_exports: failed")
