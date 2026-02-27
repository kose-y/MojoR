    # Tree reduction for min(x)
    var temp = alloc[Float64](n_i)
    if n_i == Int(0):
        acc = _MOJOR_NAN
    else:
    
    # Phase 1: Pairwise reduction
        var n_pairs = n_i // 2
        for __mojor_tr_i in range(n_pairs):
            temp[__mojor_tr_i] = min(x[2*__mojor_tr_i], x[2*__mojor_tr_i + 1])
    
    # Handle odd length
        if n_i % 2 == 1:
            temp[n_pairs] = x[n_i - 1]
            n_pairs += 1
    
    # Phase 2+: Recursive pairwise
        while n_pairs > 1:
            var next_pairs = n_pairs // 2
            for __mojor_tr_j in range(next_pairs):
                temp[__mojor_tr_j] = min(temp[2*__mojor_tr_j], temp[2*__mojor_tr_j + 1])
            if n_pairs % 2 == 1:
                temp[next_pairs] = temp[n_pairs - 1]
                next_pairs += 1
            n_pairs = next_pairs
    
        acc = temp[0]
    temp.free()
    acc_out[0] = acc
