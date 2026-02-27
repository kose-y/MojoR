    # Tree arg-reduction for which.max(x) (tie-stable: first index)
    var temp_val = alloc[Float64](n_i)
    var temp_idx = alloc[Int32](n_i)
    if n_i == Int(0):
        idx = Int32(0)
    else:
    
    # Phase 1: Pairwise argmax reduction
        var n_pairs = n_i // 2
        for __mojor_tr_i in range(n_pairs):
            var __mojor_left_i = 2*__mojor_tr_i
            var __mojor_right_i = __mojor_left_i + 1
            var __mojor_left_v: Float64 = x[__mojor_left_i]
            var __mojor_right_v: Float64 = x[__mojor_right_i]
            if __mojor_right_v > __mojor_left_v:
                temp_val[__mojor_tr_i] = __mojor_right_v
                temp_idx[__mojor_tr_i] = Int32(__mojor_right_i + 1)
            else:
                temp_val[__mojor_tr_i] = __mojor_left_v
                temp_idx[__mojor_tr_i] = Int32(__mojor_left_i + 1)
    
    # Handle odd length
        if n_i % 2 == 1:
            temp_val[n_pairs] = x[n_i - 1]
            temp_idx[n_pairs] = Int32(n_i)
            n_pairs += 1
    
    # Phase 2+: Recursive pairwise (stable ties by lower index)
        while n_pairs > 1:
            var next_pairs = n_pairs // 2
            for __mojor_tr_j in range(next_pairs):
                var __mojor_l = 2*__mojor_tr_j
                var __mojor_r = __mojor_l + 1
                var __mojor_lv: Float64 = temp_val[__mojor_l]
                var __mojor_rv: Float64 = temp_val[__mojor_r]
                var __mojor_li: Int32 = temp_idx[__mojor_l]
                var __mojor_ri: Int32 = temp_idx[__mojor_r]
                if __mojor_rv > __mojor_lv:
                    temp_val[__mojor_tr_j] = __mojor_rv
                    temp_idx[__mojor_tr_j] = __mojor_ri
                elif __mojor_rv < __mojor_lv:
                    temp_val[__mojor_tr_j] = __mojor_lv
                    temp_idx[__mojor_tr_j] = __mojor_li
                else:
                    temp_val[__mojor_tr_j] = __mojor_lv
                    temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)
            if n_pairs % 2 == 1:
                temp_val[next_pairs] = temp_val[n_pairs - 1]
                temp_idx[next_pairs] = temp_idx[n_pairs - 1]
                next_pairs += 1
            n_pairs = next_pairs
    
        idx = temp_idx[0]
    temp_idx.free()
    temp_val.free()
    idx_out[0] = idx
