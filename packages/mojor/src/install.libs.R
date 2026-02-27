files <- c(Sys.glob("*.so"), Sys.glob("*.dll"), Sys.glob("*.dylib"))
files <- unique(files[file.exists(files)])

if (length(files) == 0L) {
  stop("install.libs.R: no shared libraries found to install")
}

dest <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

ok <- file.copy(files, dest, overwrite = TRUE)
if (!all(ok)) {
  missing <- files[!ok]
  stop(
    sprintf(
      "install.libs.R: failed to copy shared libraries: %s",
      paste(missing, collapse = ", ")
    )
  )
}
