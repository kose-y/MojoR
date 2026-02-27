# Test what expression-only Mojo code should look like
# For: function(x) sum(x)

# Expected Mojo output:
cat("Expected Mojo for sum(x):\n")
cat("
@export(\"mojor_kernel\", ABI=\"C\")
fn mojor_kernel(
    x_ptr: ImmutOpaqueAny,
    __mojor_len_x: Int32
) -> Float64:
    var x: ImmutFloat64Ptr = x_ptr.bitcast[Float64]()
    var n_x = Int(__mojor_len_x)
    return mojor_sum_f64(x, n_x)
\n")

# For: function(x, y) x + y
cat("\nExpected Mojo for x + y:\n")
cat("
@export(\"mojor_kernel\", ABI=\"C\")
fn mojor_kernel(
    x: Float64,
    y: Float64
) -> Float64:
    return x + y
\n")

# For: function(x) sum(x) + 1
cat("\nExpected Mojo for sum(x) + 1:\n")
cat("
@export(\"mojor_kernel\", ABI=\"C\")
fn mojor_kernel(
    x_ptr: ImmutOpaqueAny,
    __mojor_len_x: Int32
) -> Float64:
    var x: ImmutFloat64Ptr = x_ptr.bitcast[Float64]()
    var n_x = Int(__mojor_len_x)
    return mojor_sum_f64(x, n_x) + 1.0
\n")
