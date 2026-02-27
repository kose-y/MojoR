.mojor_truthy_env <- function(name) {
    val <- tolower(Sys.getenv(name, unset = ""))
    val %in% c("1", "true", "yes", "y", "on")
}

.mojor_parse_mojo_version <- function(raw) {
    if (!is.character(raw) || length(raw) == 0L) {
        return("")
    }
    txt <- paste(raw, collapse = "\n")
    m <- regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", txt, perl = TRUE)
    if (m[[1L]] > 0L) {
        return(regmatches(txt, m)[[1L]])
    }
    m2 <- regexpr("[0-9]+\\.[0-9]+", txt, perl = TRUE)
    if (m2[[1L]] > 0L) {
        return(regmatches(txt, m2)[[1L]])
    }
    ""
}

.mojor_detect_mojo_version <- function() {
    mojo_bin <- Sys.which("mojo")
    if (!nzchar(mojo_bin)) {
        return(list(ok = FALSE, version = "", raw = "", reason = "missing"))
    }
    raw <- tryCatch(
        system2(mojo_bin, "--version", stdout = TRUE, stderr = TRUE),
        error = function(e) character(0)
    )
    ver <- .mojor_parse_mojo_version(raw)
    list(ok = TRUE, version = ver, raw = paste(raw, collapse = " "), reason = "")
}

.mojor_assert_mojo_toolchain <- function(context = "package load") {
    if (isTRUE(.mojor_truthy_env("MOJOR_SKIP_MOJO_CHECK"))) {
        return(invisible(TRUE))
    }
    required_prefix <- Sys.getenv("MOJOR_REQUIRED_MOJO_PREFIX", unset = "0.26")
    info <- .mojor_detect_mojo_version()
    if (!isTRUE(info$ok)) {
        stop(
            sprintf(
                "mojor: mojo binary not found during %s. Install Mojo %s.x before using mojor. Source checkout helper: bash scripts/install_mojo.sh",
                context,
                required_prefix
            ),
            call. = FALSE
        )
    }
    ver <- as.character(info$version[[1L]])
    ok <- identical(ver, required_prefix) || startsWith(ver, paste0(required_prefix, "."))
    if (!isTRUE(ok)) {
        stop(
            sprintf(
                "mojor: unsupported mojo version '%s' during %s (need %s.x). Run: bash scripts/install_mojo.sh",
                if (nzchar(ver)) ver else "<unknown>",
                context,
                required_prefix
            ),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
    .mojor_assert_mojo_toolchain("package load")
}
