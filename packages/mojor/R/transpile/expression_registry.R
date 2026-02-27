# Shared expression/transpile operation registry.

.mojor_expr_registry <- local({
    registry <- list(
        standalone_reductions = c("sum", "mean", "min", "max", "prod", "sd", "var"),
        vectorized_math_unary = c(
            "abs", "abs2", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan",
            "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
            "log", "log10", "log2", "log1p", "exp", "expm1",
            "floor", "ceiling", "trunc", "round", "sign", "cbrt", "lgamma", "erf",
            "gamma", "factorial"
        ),
        vectorized_type_checks = c("is.na", "is.nan", "is.finite", "is.infinite"),
        vectorized_type_casts = c("as.double", "as.integer", "as.single", "as.logical"),
        vectorized_binary_calls = c("atan2", "hypot", "xor", "beta", "choose"),
        vectorized_multiary_calls = c("min", "max", "pmin", "pmax"),
        mojo_math_imports = c(
            "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
            "tanh", "log", "log1p", "exp", "expm1", "sqrt", "floor", "ceil",
            "trunc", "hypot", "cbrt", "lgamma", "erf", "tgamma"
        )
    )
    function() registry
})

.mojor_expr_registry_standalone_reductions <- function() {
    .mojor_expr_registry()$standalone_reductions
}

.mojor_expr_registry_standalone_reductions_label <- function() {
    paste(.mojor_expr_registry_standalone_reductions(), collapse = ", ")
}

.mojor_expr_registry_vectorized_math_unary <- function() {
    .mojor_expr_registry()$vectorized_math_unary
}

.mojor_expr_registry_vectorized_type_checks <- function() {
    .mojor_expr_registry()$vectorized_type_checks
}

.mojor_expr_registry_vectorized_type_casts <- function() {
    .mojor_expr_registry()$vectorized_type_casts
}

.mojor_expr_registry_vectorized_binary_calls <- function() {
    .mojor_expr_registry()$vectorized_binary_calls
}

.mojor_expr_registry_vectorized_multiary_calls <- function() {
    .mojor_expr_registry()$vectorized_multiary_calls
}

.mojor_expr_registry_mojo_math_imports <- function() {
    .mojor_expr_registry()$mojo_math_imports
}
