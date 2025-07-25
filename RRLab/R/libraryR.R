#' Load or Install One or More R Packages
#'
#' Attempts to load package(s). If not installed, tries CRAN then Bioconductor.
#' Silently suppresses install messages. Supports both quoted and unquoted names.
#' If a package is already loaded it simply notifies the user.
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
#' # Calling it on RRLab itself triggers a colourful message
#' libraryR("RRLab")
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
  
  pkg_expr <- substitute(pkg)
  pkg_val <- tryCatch(eval(pkg_expr, parent.frame()), error = function(e) NULL)

  if (is.character(pkg_val)) {
    pkg <- pkg_val
  } else if (is.name(pkg_expr)) {
    pkg <- as.character(pkg_expr)
  } else {
    pkg <- pkg_val
  }
  
  results = setNames(logical(length(pkg)), pkg)

  for (p in pkg) {
    installed_now = FALSE

    spinner = cli::cli_status(paste0("Loading {.pkg ", p, "}..."))
    already_loaded <- paste0("package:", p) %in% search()
    
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

    cli::cli_status_clear(spinner)
    if (already_loaded) {
      cli::cli_alert_info(" In-use {.pkg {p}}.")
    } else {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      cli::cli_alert_success("Loaded {.pkg {p}}{if (installed_now) ' after install' else ''}.")
    }

    if (identical(p, "RRLab")) {
      msg <- "~ Achievement unlocked: Super user of libraryR()! ~"
      color_set <- c(32, 33, 35, 36, 31, 34)
      n <- nchar(msg)

      for (iter in rep(seq_along(color_set),3)) {
        cat("\r")
        cat(paste0("\033[", color_set[iter], "m", msg, "\033[0m"))
        
        Sys.sleep(0.1)
        
        flush.console()
    }
      cat("\n")
    }

    results[p] = TRUE
  }
  
  invisible(results)
}
