    if n_y != n_x:
        return Int32(-1)
    for i in range(n_x):
        var a = Float64(x[i])
        var b = Float64(y[i])
        out[i] = Float64((a + b))
    return Int32(n_x)
