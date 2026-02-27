comptime _MOJOR_LAYOUT_mat = Layout.row_major(IndexList[2](0, 0))
comptime _MOJOR_MATRIX_LAYOUT = Layout.row_major(IndexList[2](0, 0))
    var mat_layout = RuntimeLayout[_MOJOR_MATRIX_LAYOUT].row_major(IndexList[2](Int(nrow_out_i), Int(ncol_out_i)))
                mat_tensor[(i - 1), __mojor_i2] = Float64((_mojor_read_f64(x, Int((i - 1)), Int(n_x_i)) if ((__mojor_i2) % 2) < 1 else _mojor_read_f64(y, Int((i - 1)), Int(n_y_i))))
