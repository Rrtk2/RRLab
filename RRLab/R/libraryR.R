#' Load or Install One or More R Packages
#'
#' Attempts to load package(s). If not installed, tries CRAN then Bioconductor.
#' Silently suppresses install messages. Supports both quoted and unquoted names.
#'
#' @param pkg A single package name or a character vector of package names.
#' @return Invisibly returns a named logical vector indicating success per package.
#' @examples
#' # Load multiple packages
#' libraryR(c("limma", "missMethyl"))
#'
#' # Also works with unquoted names (non-standard evaluation)
#' libraryR(dplyr)
#'
#' # Package names can also be stored in a vector
#' pkgs <- c("ggplot2", "dplyr")
#' libraryR(pkgs)
#'
#' @export
libraryR = function(pkg) {
  if (!requireNamespace("cli", quietly = TRUE)) install.packages("cli", quiet = TRUE)
  if (!requireNamespace("rlang", quietly = TRUE)) install.packages("rlang", quiet = TRUE)
  
  suppress_all = function(expr) {
    invisible(capture.output(suppressMessages(suppressWarnings(try(expr, silent = TRUE))), type = "output"))
  }
  
  pkg = substitute(pkg)
  pkg = if (is.name(pkg)) as.character(pkg) else eval(pkg)
  
  results = setNames(logical(length(pkg)), pkg)
  
  for (p in pkg) {
    installed_now = FALSE
    
    spinner = cli::cli_status(paste0("Loading {.pkg ", p, "}..."))
    
    if (!suppressMessages(suppressWarnings(requireNamespace(p, quietly = TRUE)))) {
      spinner = cli::cli_status(paste0("Installing {.pkg ", p, "} from CRAN"))
      suppress_all(install.packages(p, dependencies = TRUE, quiet = TRUE))
      installed_now = TRUE
    }
    
    if (!suppressMessages(suppressWarnings(requireNamespace(p, quietly = TRUE)))) {
      spinner = cli::cli_status(paste0("Installing {.pkg ", p, "} from Bioconductor"))
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        suppress_all(install.packages("BiocManager", quiet = TRUE))
      }
      suppress_all(BiocManager::install(p, ask = FALSE, update = FALSE, quiet = TRUE))
      installed_now = TRUE
      
      
      if (!suppressMessages(suppressWarnings(requireNamespace(p, quietly = TRUE)))) {
        cli::cli_status_clear(spinner)
        cli::cli_alert_danger("Could not install {.pkg {p}} via CRAN or Bioconductor.")
        results[p] = FALSE
        next
      }
    }
    
    suppressPackageStartupMessages(library(p, character.only = TRUE))
    cli::cli_alert_success("Loaded {.pkg {p}}{if (installed_now) ' after install' else ''}.")
    results[p] = TRUE
  }
  
  invisible(results)
}
