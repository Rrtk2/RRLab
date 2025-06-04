#' Rsave
#'
#' Convenience wrapper around `qs::qsavem` that saves an object using its
#' name in the calling environment. A prefix and postfix can be supplied to
#' modify the file name and `s_saveloc_qc` is used as the default directory.
#'
#' @param object Object to save or the name of the object.
#' @param prefix Optional prefix for the file name.
#' @param postfix Optional postfix for the file name.
#' @param file_location Directory where the file will be saved. Defaults to
#'   `s_saveloc_qc`.
#' @param ... Additional arguments passed to `qs::qsavem`.
#'
#' @return Invisibly returns the file path used.
#' @examples
#' \dontrun{
#' a <- 1:5
#' Rsave(a)
#' Rsave(a, prefix = "08A", postfix = "cohort")
#' }
#' @export
Rsave <- function(object, prefix = "", postfix = "", file_location = s_saveloc_qc, ...) {
  obj_expr <- substitute(object)
  obj_name <- deparse(obj_expr)

  # If a character string is provided, treat it as a variable name
  if (is.character(object) && length(object) == 1 && !exists(obj_name, envir = parent.frame())) {
    obj_name <- object
    object <- get(obj_name, envir = parent.frame())
  }

  pre <- if (nzchar(prefix)) paste0(prefix, "_") else ""
  post <- if (nzchar(postfix)) paste0("_", postfix) else ""

  file_path <- file.path(file_location, paste0(pre, obj_name, post, ".qs"))

  args <- c(setNames(list(object), obj_name), list(file = file_path), list(...))
  do.call(qs::qsavem, args)

  invisible(file_path)
}
