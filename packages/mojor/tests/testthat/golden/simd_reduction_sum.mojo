    # SIMD reduction for sum(x)
    from sys.info import simd_width_of
    comptime simd_width = simd_width_of[DType.float64]()
    var vec_acc = SIMD[DType.float64, simd_width](0.0)
    if n_i == Int(0):
        acc = 0.0
    else:
    
    # Phase 1: SIMD accumulation
        var n_chunks = n_i // simd_width
        for __mojor_sr_i in range(n_chunks):
            var vec = x.load[width=simd_width](__mojor_sr_i * simd_width)
            vec_acc = (vec_acc + vec)
    
    # Phase 2: Horizontal reduction
        acc = vec_acc[0]
        for __mojor_sr_lane in range(1, simd_width):
            acc = (acc + vec_acc[__mojor_sr_lane])
    
    # Phase 3: Handle remainder
        for __mojor_sr_rem in range(n_chunks * simd_width, n_i):
            acc = (acc + x[__mojor_sr_rem])
    acc_out[0] = acc
