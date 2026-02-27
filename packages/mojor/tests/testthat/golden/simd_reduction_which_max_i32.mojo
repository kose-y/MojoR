    # SIMD arg-reduction for which.max(x) (tie-stable: first index)
    from sys.info import simd_width_of
    comptime simd_width = simd_width_of[DType.int32]()
    if n_i == Int(0):
        idx = Int32(0)
    else:
        var __mojor_best_v: Int32 = x[0]
        var __mojor_best_idx: Int32 = Int32(1)
        var n_chunks = n_i // simd_width
        for __mojor_sr_i in range(n_chunks):
            var __mojor_chunk_base = __mojor_sr_i * simd_width
            var vec = x.load[width=simd_width](__mojor_chunk_base)
            var __mojor_chunk_best_lane = Int(0)
            var __mojor_chunk_best_v: Int32 = vec[0]
            for __mojor_sr_lane in range(1, simd_width):
                var __mojor_lane_v: Int32 = vec[__mojor_sr_lane]
                if __mojor_lane_v > __mojor_chunk_best_v:
                    __mojor_chunk_best_v = __mojor_lane_v
                    __mojor_chunk_best_lane = __mojor_sr_lane
                elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:
                    __mojor_chunk_best_lane = __mojor_sr_lane
            var __mojor_chunk_best_idx = Int32(__mojor_chunk_base + __mojor_chunk_best_lane + 1)
            if __mojor_chunk_best_v > __mojor_best_v:
                __mojor_best_v = __mojor_chunk_best_v
                __mojor_best_idx = __mojor_chunk_best_idx
            elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:
                __mojor_best_idx = __mojor_chunk_best_idx
        for __mojor_sr_rem in range(n_chunks * simd_width, n_i):
            var __mojor_rem_v: Int32 = x[__mojor_sr_rem]
            var __mojor_rem_idx = Int32(__mojor_sr_rem + 1)
            if __mojor_rem_v > __mojor_best_v:
                __mojor_best_v = __mojor_rem_v
                __mojor_best_idx = __mojor_rem_idx
            elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:
                __mojor_best_idx = __mojor_rem_idx
        idx = __mojor_best_idx
    idx_out[0] = idx
