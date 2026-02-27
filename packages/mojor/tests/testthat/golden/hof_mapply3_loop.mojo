    if n_y != n_x:
        return Int32(-1)
    if n_z != n_x:
        return Int32(-1)
    for i in range(n_x):
        var a = Float64(x[i])
        var b = Float64(y[i])
        var c = Float64(z[i])
        out[i] = Float64(((a + b) - c))
    return Int32(n_x)
