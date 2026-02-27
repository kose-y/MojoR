# ============================================================================= MojoR IR
# Type System
# ============================================================================= Type
# system utilities for MojoR's Tree IR Organization: 1. Type Constants 2. Type Predicates
# 3. Type Inference 4. Type Promotion 5. Type Coercion 6. Type Checking
# =============================================================================

# ============================================================================= Section
# 1: Type Constants
# =============================================================================

# Scalar types
MOJOR_TYPE_I32 <- "i32"
MOJOR_TYPE_F64 <- "f64"
MOJOR_TYPE_F32 <- "f32"
MOJOR_TYPE_BOOL <- "bool"
MOJOR_TYPE_LGL <- "lgl"

# Array types
MOJOR_TYPE_I32_ARR <- "i32[]"
MOJOR_TYPE_F64_ARR <- "f64[]"
MOJOR_TYPE_F32_ARR <- "f32[]"
MOJOR_TYPE_BOOL_ARR <- "bool[]"
MOJOR_TYPE_LGL_ARR <- "lgl[]"

# All valid types
MOJOR_VALID_TYPES <- c(
    MOJOR_TYPE_I32, MOJOR_TYPE_F64, MOJOR_TYPE_F32, MOJOR_TYPE_BOOL, MOJOR_TYPE_LGL, MOJOR_TYPE_I32_ARR,
    MOJOR_TYPE_F64_ARR, MOJOR_TYPE_F32_ARR, MOJOR_TYPE_BOOL_ARR, MOJOR_TYPE_LGL_ARR
)

# ============================================================================= Section
# 2: Type Predicates
# =============================================================================

#' Check if a type is an array type
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_array <- function(type) {
    !is.null(type) &&
        grepl("\\[", type)
}

#' Check if a type is a scalar type
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_scalar <- function(type) {
    !is.null(type) &&
        !grepl("\\[", type) &&
        type %in% c(MOJOR_TYPE_I32, MOJOR_TYPE_F64, MOJOR_TYPE_F32, MOJOR_TYPE_BOOL, MOJOR_TYPE_LGL)
}

#' Check if a type is numeric
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_numeric <- function(type) {
    !is.null(type) &&
        type %in% c(MOJOR_TYPE_I32, MOJOR_TYPE_F64, MOJOR_TYPE_F32)
}

#' Check if a type is floating point
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_float <- function(type) {
    !is.null(type) &&
        type %in% c(MOJOR_TYPE_F64, MOJOR_TYPE_F32)
}

#' Check if a type is integer
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_integer <- function(type) {
    !is.null(type) &&
        type %in% c(MOJOR_TYPE_I32)
}

#' Check if a type is logical
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_logical <- function(type) {
    !is.null(type) &&
        type %in% c(MOJOR_TYPE_BOOL, MOJOR_TYPE_LGL)
}

#' Check if a type is valid
#'
#' @param type Type string
#'
#' @noRd
mojor_type_is_valid <- function(type) {
    !is.null(type) &&
        type %in% MOJOR_VALID_TYPES
}

# ============================================================================= Section
# 3: Type Inference
# =============================================================================

#' Infer type from IR node
#'
#' @param node IR node
#' @param type_env Type environment (optional)
#'
#' @noRd
mojor_ir_infer_type <- function(node, type_env = list()) {
    if (is.null(node) ||
        is.null(node$kind)) {
        return("unknown")
    }

 # Cache hit: return pre-annotated type
    if (!is.null(node$type)) {
        return(node$type)
    }

    switch(
        node$kind, const = {
            val <- node$value
            if (is.null(val)) {
                return("unknown")
            }

            if (val %in% c("TRUE", "FALSE", "True", "False")) {
                return("bool")
            }

            if (tolower(val) %in%
                c("inf", "-inf", "nan")) {
                return("f64")
            }

            if (grepl("^-?[0-9]+$", val)) {
                return("i32")
            }

            if (grepl("^-?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$", val) ||
                grepl("^-?[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?$", val)) {
                return("f64")
            }

            if (grepl("[eE]", val)) {
                return("f64")
            }

            "unknown"
        }, var = {
            var_type <- type_env[[node$name]]
            if (is.null(var_type)) {
                return("unknown")
            }
            var_type
        }, cast = {
            node$to
        }, unop = {
            arg_type <- mojor_ir_infer_type(node$expr, type_env)
            if (node$op == "!") {
                return("bool")
            }
            if (node$op == "-") {
                return(arg_type)
            }
            "unknown"
        }, binop = {
            lhs_type <- mojor_ir_infer_type(node$lhs, type_env)
            rhs_type <- mojor_ir_infer_type(node$rhs, type_env)

            if (node$op %in% c(">", "<", ">=", "<=", "==", "!=")) {
                return("bool")
            }

            if (node$op %in% c("&&", "||")) {
                return("bool")
            }

            if (node$op %in% c("+", "-", "*", "/", "%%", "%/%")) {
                return(mojor_type_promote(lhs_type, rhs_type))
            }

            "unknown"
        }, index = {
            base_type <- mojor_ir_infer_type(node$base, type_env)
            if (is.null(base_type) ||
                base_type == "unknown") {
                return("unknown")
            }

            if (mojor_type_is_array(base_type)) {
                elem_type <- mojor_type_elem(base_type)
                if (elem_type == "lgl") elem_type <- "bool"
                return(elem_type)
            }

            "unknown"
        }, call = {
            if (node$fn %in% c("len", "length")) {
                return("i32")
            }
            if (node$fn %in% c("is.na", "is.nan", "is.finite", "is.infinite", "as.logical")) {
                return("bool")
            }

            if (node$fn %in% c(
                "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh",
                "acosh", "atanh", "log", "log10", "log1p", "log2", "exp", "expm1", "sqrt",
                "floor", "ceil", "trunc", "round", "abs", "sign", "cbrt", "lgamma", "erf", "gamma",
                "atan2", "hypot", "pow"
            )) {
                return("f64")
            }

            if (node$fn == "abs2") {
                return("f64")
            }

            if (node$fn %in% c("min", "max", "pmin", "pmax") &&
                length(node$args) ==
                  2) {
                lhs_type <- mojor_ir_infer_type(node$args[[1]], type_env)
                rhs_type <- mojor_ir_infer_type(node$args[[2]], type_env)
                return(mojor_type_promote(lhs_type, rhs_type))
            }

            "unknown"
        }, ifelse = {
            yes_type <- mojor_ir_infer_type(node$yes, type_env)
            no_type <- mojor_ir_infer_type(node$no, type_env)
            if (identical(yes_type, "bool") &&
                identical(no_type, "bool")) {
                return("bool")
            }
            mojor_type_promote(yes_type, no_type)
        }, raw = {
            if (!is.null(node$expr) &&
                mojor_ir_expr_has_constructor(node$expr)) {
                kind <- mojor_expr_kind(node$expr, type_env)
                if (kind == "float") {
                  return("f64")
                }
                if (kind == "int") {
                  return("i32")
                }
                if (kind == "bool") {
                  return("bool")
                }
            }
            "unknown"
        }, "unknown"
    )
}

#' Get element type from array type
#'
#' @param type Array type string
#'
#' @noRd
mojor_type_elem <- function(type) {
    if (is.null(type) ||
        !grepl("\\[", type)) {
        return(type)
    }
    sub("\\[.*\\]$", "", type)
}

# ============================================================================= Section
# 4: Type Promotion
# =============================================================================

#' Promote two types to common type
#'
#' @param type1 First type
#' @param type2 Second type
#'
#' @noRd
mojor_type_promote <- function(type1, type2) {
    if (is.null(type1) ||
        type1 == "unknown") {
        return(type2)
    }
    if (is.null(type2) ||
        type2 == "unknown") {
        return(type1)
    }

 # Same type
    if (type1 == type2) {
        return(type1)
    }

 # Array vs scalar
    if (mojor_type_is_array(type1) &&
        !mojor_type_is_array(type2)) {
        elem1 <- mojor_type_elem(type1)
        return(
            paste0(
                mojor_type_promote(elem1, type2),
                "[]"
            )
        )
    }
    if (!mojor_type_is_array(type1) &&
        mojor_type_is_array(type2)) {
        elem2 <- mojor_type_elem(type2)
        return(
            paste0(
                mojor_type_promote(type1, elem2),
                "[]"
            )
        )
    }

 # Both arrays - promote element types
    if (mojor_type_is_array(type1) &&
        mojor_type_is_array(type2)) {
        elem1 <- mojor_type_elem(type1)
        elem2 <- mojor_type_elem(type2)
        return(
            paste0(
                mojor_type_promote(elem1, elem2),
                "[]"
            )
        )
    }

 # Numeric promotion hierarchy: f64 > f32 > i32
    if (type1 == "f64" || type2 == "f64") {
        return("f64")
    }
    if (type1 == "f32" || type2 == "f32") {
        return("f32")
    }
    if (type1 == "i32" && type2 == "i32") {
        return("i32")
    }

 # Logical promotion
    if (type1 == "bool" && type2 == "bool") {
        return("bool")
    }
    if (type1 == "lgl" && type2 == "lgl") {
        return("lgl")
    }
    if (type1 %in% c("bool", "lgl") &&
        type2 %in% c("bool", "lgl")) {
        return("bool")
    }

 # Default to first type
    type1
}

# ============================================================================= Section
# 5: Type Coercion
# =============================================================================

#' Check if type coercion is needed
#'
#' @param from_type Source type
#' @param to_type Target type
#'
#' @noRd
mojor_type_needs_coercion <- function(from_type, to_type) {
    if (is.null(from_type) ||
        is.null(to_type)) {
        return(FALSE)
    }
    if (from_type == to_type) {
        return(FALSE)
    }

 # Coercion needed if types are different
    !identical(from_type, to_type)
}

#' Get coercion expression
#'
#' @param expr Expression
#' @param from_type Source type
#' @param to_type Target type
#'
#' @noRd
mojor_type_coercion_expr <- function(expr, from_type, to_type) {
    if (!mojor_type_needs_coercion(from_type, to_type)) {
        return(expr)
    }

 # Generate appropriate coercion
    if (to_type == "f64") {
        call("as.double", expr)
    } else if (to_type == "f32") {
        call("as.single", expr)
    } else if (to_type == "i32") {
        call("as.integer", expr)
    } else if (to_type == "bool") {
        call("as.logical", expr)
    } else {
        expr
    }
}

# ============================================================================= Section
# 6: Type Checking
# =============================================================================

#' Check if two types are compatible
#'
#' @param type1 First type
#' @param type2 Second type
#'
#' @noRd
mojor_type_compatible <- function(type1, type2) {
    if (is.null(type1) ||
        is.null(type2)) {
        return(FALSE)
    }

 # Same type is compatible
    if (type1 == type2) {
        return(TRUE)
    }

 # Array vs scalar compatibility
    if (mojor_type_is_array(type1) &&
        !mojor_type_is_array(type2)) {
        elem1 <- mojor_type_elem(type1)
        return(mojor_type_compatible(elem1, type2))
    }
    if (!mojor_type_is_array(type1) &&
        mojor_type_is_array(type2)) {
        elem2 <- mojor_type_elem(type2)
        return(mojor_type_compatible(type1, elem2))
    }

 # Both arrays - check element compatibility
    if (mojor_type_is_array(type1) &&
        mojor_type_is_array(type2)) {
        elem1 <- mojor_type_elem(type1)
        elem2 <- mojor_type_elem(type2)
        return(mojor_type_compatible(elem1, elem2))
    }

 # Numeric compatibility
    if (type1 %in% c("f64", "f32", "i32") &&
        type2 %in% c("f64", "f32", "i32")) {
        return(TRUE)
    }

 # Logical compatibility
    if (type1 %in% c("bool", "lgl") &&
        type2 %in% c("bool", "lgl")) {
        return(TRUE)
    }

    FALSE
}
