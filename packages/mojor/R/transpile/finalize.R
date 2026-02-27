# Transpile scalar output finalization helper.

.mojor_finalize_transpile_scalar_output <- function(ctx) {
    if (!is.environment(ctx)) {
        stop(".mojor_finalize_transpile_scalar_output: ctx must be an environment")
    }
    evalq(
        {
            if (out_kind == "scalar") {
                if (!is.null(return_num_expr_ast)) {
                  num_mojo <- .mojor_expr_to_mojo(
                    return_num_expr_ast, character(0),
                    local_types, in_cond = FALSE
                  )
                  mojo_lines <- c(
                    mojo_lines, paste0("    ", scalar_init, " = ", num_mojo)
                  )
                }
                if (!is.null(return_div)) {
                  denom_ctor <- if (!is.null(scalar_type) && scalar_type == "f32") "Float32" else "Float64"
                  if (return_div == "count") {
                    mojo_lines <- c(mojo_lines, paste0("    if ", scalar_init, "_count == 0:"))
                    mojo_lines <- c(mojo_lines, paste0("        ", scalar_init, " = _MOJOR_NAN"))
                    mojo_lines <- c(mojo_lines, "    else:")
                    mojo_lines <- c(
                      mojo_lines, paste0(
                        "        ", scalar_init, " = ", scalar_init, " / ", denom_ctor, "(", scalar_init,
                        "_count)"
                    )
                  )
                  } else if (return_div == "var_count") {
                    mojo_lines <- c(mojo_lines, paste0("    if ", scalar_init, "_count <= 1:"))
                    mojo_lines <- c(mojo_lines, paste0("        ", scalar_init, " = _MOJOR_NAN"))
                    mojo_lines <- c(mojo_lines, "    else:")
                    mojo_lines <- c(
                      mojo_lines, paste0(
                        "        ", scalar_init, " = ", scalar_init, " / ", denom_ctor, "(", scalar_init,
                        "_count - 1)"
                    )
                  )
                  } else if (return_div == "sd_count") {
                    mojo_lines <- c(mojo_lines, paste0("    if ", scalar_init, "_count <= 1:"))
                    mojo_lines <- c(mojo_lines, paste0("        ", scalar_init, " = _MOJOR_NAN"))
                    mojo_lines <- c(mojo_lines, "    else:")
                    mojo_lines <- c(
                      mojo_lines, paste0(
                        "        ", scalar_init, " = sqrt(", scalar_init, " / ", denom_ctor, "(", scalar_init,
                        "_count - 1))"
                    )
                  )
                  } else if (return_div == "n_i") {
                    mojo_lines <- c(mojo_lines, paste0("    ", scalar_init, " = ", scalar_init, " / ", denom_ctor, "(n_i)"))
                  } else {
                    mojo_lines <- c(
                      mojo_lines, paste0(
                        "    ", scalar_init, " = ", scalar_init, " / ", denom_ctor, "(", return_div,
                        ")"
                    )
                  )
                  }
                }
                mojo_lines <- c(mojo_lines, paste0("    ", scalar_init, "_out[0] = ", scalar_init))
            }
        }, envir = ctx
    )
}
