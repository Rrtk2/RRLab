#' Rload
#'
#' Wrapper around `qs::qload` to read objects saved with `Rsave`.
#' If `name` is an object, its name will be used to construct the
#' file name using the supplied prefix and postfix. Alternatively a
#' character string can be provided to directly specify the base file
#' name. Files are loaded from `s_saveloc_qc` by default.
#'
#' @param name Object or character string identifying the file to load.
#' @param prefix Optional prefix used during saving.
#' @param postfix Optional postfix used during saving.
#' @param file_location Directory of the saved file. Defaults to `s_saveloc_qc`.
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
Rload <- function(name, prefix = "", postfix = "", file_location = s_saveloc_qc,
                  envir = parent.frame(), ...) {
  name_expr <- substitute(name)

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
