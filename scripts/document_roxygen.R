#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(desc)
  library(roxygen2)
})

load_local <- function(path) {
  env <- new.env(parent = globalenv())
  pkg <- desc_get_field("Package", file = path)
  methods::setPackageName(pkg, env)
  deps <- desc_get_deps(path)
  pkgs <- deps$package[deps$type %in% c("Depends", "Imports") & deps$package != "R"]
  for (p in pkgs) {
    suppressWarnings(require(p, character.only = TRUE))
  }
  lapply(roxygen2:::package_files(path), sys.source, envir = env)
  env
}

load_dummy <- function(path) {
  env <- new.env(parent = asNamespace("methods"))
  pkg <- desc_get_field("Package", file = path)
  methods::setPackageName(pkg, env)
  env
}

message("Documenting packages/mojor (Rd only)...")
roxygenise(
  package.dir = "packages/mojor",
  roclets = c("rd"),
  load_code = load_local
)

message("Documenting packages/mojorGPU (Rd + NAMESPACE)...")
roxygenise(
  package.dir = "packages/mojorGPU",
  roclets = c("rd", "namespace"),
  load_code = load_dummy
)

message("Roxygen documentation complete.")
