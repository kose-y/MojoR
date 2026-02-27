.mojor_ir_eval_const <- function(expr) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(NULL)
  }

  switch(expr$kind,
    const = {
 # Parse the constant value
      val <- expr$value
      if (is.character(val)) {
 # Try to parse as number
        if (tolower(val) == "true") {
          return(TRUE)
        }
        if (tolower(val) == "false") {
          return(FALSE)
        }
        if (tolower(val) == "inf") {
          return(Inf)
        }
        if (tolower(val) == "-inf") {
          return(-Inf)
        }
        if (tolower(val) == "nan") {
          return(NaN)
        }

 # Try numeric
        num <- suppressWarnings(as.numeric(val))
        if (!is.na(num)) {
          return(num)
        }
      }
      return(val)
    },
    unop = {
      operand <- .mojor_ir_eval_const(expr$expr)
      if (is.null(operand)) {
        return(NULL)
      }

      result <- switch(expr$op,
        "-" = -operand,
        "!" = !operand,
        NULL
      )
 # Convert logical to numeric for consistency
      if (is.logical(result)) as.numeric(result) else result
    },
    binop = {
      lhs <- .mojor_ir_eval_const(expr$lhs)
      rhs <- .mojor_ir_eval_const(expr$rhs)
      if (is.null(lhs) || is.null(rhs)) {
        return(NULL)
      }

      tryCatch(
        {
          result <- switch(expr$op,
            "+" = lhs + rhs,
            "-" = lhs - rhs,
            "*" = lhs * rhs,
            "/" = lhs / rhs,
            "%%" = lhs %% rhs,
            "%/%" = lhs %/% rhs,
            "<" = lhs < rhs,
            ">" = lhs > rhs,
            "<=" = lhs <= rhs,
            ">=" = lhs >= rhs,
            "==" = lhs == rhs,
            "!=" = lhs != rhs,
            "&&" = lhs && rhs,
            "||" = lhs || rhs,
            "&" = lhs & rhs,
            "|" = lhs | rhs,
            "^" = xor(lhs, rhs),
            NULL
          )
 # Convert logical to numeric for consistency
          if (is.logical(result)) as.numeric(result) else result
        },
        error = function(e) NULL
      )
    },
    cast = {
      operand <- .mojor_ir_eval_const(expr$expr)
      if (is.null(operand)) {
        return(NULL)
      }

      tryCatch(
        {
          switch(expr$to,
            "f64" = as.numeric(operand),
            "f32" = as.numeric(operand), # R doesn't distinguish, but return numeric
            "i32" = as.integer(operand),
            "bool" = as.logical(operand),
            NULL
          )
        },
        error = function(e) NULL
      )
    },
    call = {
 # Evaluate math functions with constant arguments
      args <- lapply(expr$args, .mojor_ir_eval_const)
      if (any(sapply(args, is.null))) {
        return(NULL)
      }

      tryCatch(
        {
          switch(expr$fn,
            "sin" = sin(args[[1]]),
            "cos" = cos(args[[1]]),
            "tan" = tan(args[[1]]),
            "asin" = asin(args[[1]]),
            "acos" = acos(args[[1]]),
            "atan" = atan(args[[1]]),
            "log" = log(args[[1]]),
            "log10" = log10(args[[1]]),
            "log1p" = log1p(args[[1]]),
            "log2" = log2(args[[1]]),
            "exp" = exp(args[[1]]),
            "expm1" = expm1(args[[1]]),
            "sqrt" = sqrt(args[[1]]),
            "abs" = abs(args[[1]]),
            "floor" = floor(args[[1]]),
            "ceiling" = ceiling(args[[1]]),
            "trunc" = trunc(args[[1]]),
            "round" = round(args[[1]]),
            "min" = do.call(min, args),
            "max" = do.call(max, args),
            "as.logical" = as.logical(args[[1]]),
            "is.na" = is.na(args[[1]]),
            "is.nan" = is.nan(args[[1]]),
            "is.finite" = is.finite(args[[1]]),
            "is.infinite" = is.infinite(args[[1]]),
            NULL
          )
        },
        error = function(e) NULL
      )
    },
    ifelse = {
      cond <- .mojor_ir_eval_const(expr$cond)
      if (is.null(cond)) {
        return(NULL)
      }

      if (isTRUE(cond)) {
        .mojor_ir_eval_const(expr$yes)
      } else {
        .mojor_ir_eval_const(expr$no)
      }
    },
 # Default: cannot evaluate
    NULL
  )
}

# Convert R value to IR constant node
