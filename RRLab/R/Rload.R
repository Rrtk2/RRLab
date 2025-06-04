#' Rload
#'
#' Wrapper around `qs::qload` to read objects saved with `Rsave`.
#' If `name` is an object, its name will be used to construct the
#' file name using the supplied prefix and postfix. Alternatively a
#' character string can be provided to directly specify the base file
#' name. If `file_location` is not provided, the function attempts to use
#' `s_saveloc_qc` from the calling environment. If neither is available an
#' error is thrown.
#'
#' @param name Object or character string identifying the file to load.
#' @param prefix Optional prefix used during saving.
#' @param postfix Optional postfix used during saving.
#' @param file_location Directory of the saved file. If `NULL`, `s_saveloc_qc`
#'   is used when available.
#' @param envir Environment to load the object into. Defaults to the caller's
#'   environment.
#' @param ... Additional arguments passed to `qs::qload`.
#'
#' @return Invisibly returns the result of `qs::qload`.
#' @examples
#' \dontrun{
#' Rload(a)
#' Rload("08A_a_cohort")
#' }
#' @export
Rload <- function(name, prefix = "", postfix = "", file_location = NULL,
                  envir = parent.frame(), ...) {
  name_expr <- substitute(name)

  if (is.null(file_location)) {
    if (exists("s_saveloc_qc", inherits = TRUE)) {
      file_location <- get("s_saveloc_qc", inherits = TRUE)
    } else {
      stop("file_location must be provided or 's_saveloc_qc' must exist")
    }
  }

  if (is.character(name)) {
    base <- name
  } else {
    base <- deparse(name_expr)
    pre <- if (nzchar(prefix)) paste0(prefix, "_") else ""
    post <- if (nzchar(postfix)) paste0("_", postfix) else ""
    base <- paste0(pre, base, post)
  }

  file_path <- file.path(file_location, paste0(base, ".qs"))
  qs::qload(file_path, env = envir, ...)
}
