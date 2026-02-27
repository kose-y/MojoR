        var _mojor_unroll_span = (Int(n) - Int(1) + 1)
        var _mojor_unroll_main_count = _mojor_unroll_span - (_mojor_unroll_span % 4)
        var _mojor_unroll_tail_start = Int(1) + _mojor_unroll_main_count
        for _mojor_i in range(Int(1), _mojor_unroll_tail_start, 4):
            var __mojor_unroll_lane_guard_0 = True
            if __mojor_unroll_lane_guard_0:
                out[Int(((_mojor_i) - 1))] = (_mojor_read_f64(x, Int(((_mojor_i) - 1)), Int(n_x_i)) + _mojor_read_f64(y, Int(((_mojor_i) - 1)), Int(n_y_i)))
            var __mojor_unroll_lane_guard_1 = True
            if __mojor_unroll_lane_guard_1:
                out[Int(((_mojor_i + 1) - 1))] = (_mojor_read_f64(x, Int(((_mojor_i + 1) - 1)), Int(n_x_i)) + _mojor_read_f64(y, Int(((_mojor_i + 1) - 1)), Int(n_y_i)))
            var __mojor_unroll_lane_guard_2 = True
            if __mojor_unroll_lane_guard_2:
                out[Int(((_mojor_i + 2) - 1))] = (_mojor_read_f64(x, Int(((_mojor_i + 2) - 1)), Int(n_x_i)) + _mojor_read_f64(y, Int(((_mojor_i + 2) - 1)), Int(n_y_i)))
            var __mojor_unroll_lane_guard_3 = True
            if __mojor_unroll_lane_guard_3:
                out[Int(((_mojor_i + 3) - 1))] = (_mojor_read_f64(x, Int(((_mojor_i + 3) - 1)), Int(n_x_i)) + _mojor_read_f64(y, Int(((_mojor_i + 3) - 1)), Int(n_y_i)))
        for i in range(_mojor_unroll_tail_start, Int(n) + 1):
