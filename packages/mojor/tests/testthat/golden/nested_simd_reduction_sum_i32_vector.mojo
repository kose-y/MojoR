        # SIMD reduction for sum(x)
        from sys.info import simd_width_of
        comptime simd_width = simd_width_of[DType.int32]()
        var vec_acc = SIMD[DType.int32, simd_width](Int32(0))
        if __mojor_red_n == Int(0):
            acc = Int32(0)
        else:
        
        # Phase 1: SIMD accumulation
            var n_chunks = __mojor_red_n // simd_width
            for __mojor_sr_i in range(n_chunks):
                var vec = x.load[width=simd_width](__mojor_sr_i * simd_width)
                vec_acc = (vec_acc + vec)
        
        # Phase 2: Horizontal reduction
            acc = vec_acc[0]
            for __mojor_sr_lane in range(1, simd_width):
                acc = (acc + vec_acc[__mojor_sr_lane])
        
        # Phase 3: Handle remainder
            for __mojor_sr_rem in range(n_chunks * simd_width, __mojor_red_n):
                acc = (acc + x[__mojor_sr_rem])
        out[Int((j - 1))] = acc
