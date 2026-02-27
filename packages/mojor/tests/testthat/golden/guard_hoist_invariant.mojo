    if (Int(1) <= Int(n)):
        if j < 1 or j > Int(n_x_i):
            __mojor_na_flag[0] = Int32(2)
    if Int(1) <= Int(n):
        var _mojor_unroll_span = (Int(n) - Int(1) + 1)
        var _mojor_unroll_main_count = _mojor_unroll_span - (_mojor_unroll_span % 4)
        var _mojor_unroll_tail_start = Int(1) + _mojor_unroll_main_count
        for _mojor_i in range(Int(1), _mojor_unroll_tail_start, 4):
