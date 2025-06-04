#' Rsave
#'
#' Convenience wrapper around `qs::qsavem` that saves an object using its
#' name in the calling environment. A prefix and postfix can be supplied to
#' modify the file name. If `file_location` is not supplied, `s_saveloc_qc`
#' will be used when available.
#'
#' @param object Object to save or the name of the object.
#' @param prefix Optional prefix for the file name.
#' @param postfix Optional postfix for the file name.
#' @param file_location Directory where the file will be saved. If `NULL`,
#'   `s_saveloc_qc` will be used when available.
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
Rsave <- function(object_name, prefix = "", postfix = "", file_location = NULL, ...) {
  obj_expr <- substitute(object_name)
  obj_name <- deparse(obj_expr)

  if (is.null(file_location)) {
    if (exists("s_saveloc_qc", inherits = TRUE)) {
      file_location <- get("s_saveloc_qc", inherits = TRUE)
    } else {
      stop("file_location must be provided or 's_saveloc_qc' must exist")
    }
  }

  # If a character string is provided, treat it as a variable name
  if (is.character(object_name) && length(object_name) == 1 && !exists(obj_name, envir = parent.frame())) {
    obj_name <- object_name
    object_name <- get(obj_name, envir = parent.frame())
  }

  pre <- if (nzchar(prefix)) paste0(prefix, "_") else ""
  post <- if (nzchar(postfix)) paste0("_", postfix) else ""

  file_path <- paste0(file_location, paste0(pre, obj_name, post, ".qs"))

  #args <- c(setNames(list(object), obj_name), list(file = file_path), list(...))
  #do.call(qs::qsavem, args, ...)
  qs::qsave(setNames(object = list(object_name), nm = as.character(obj_expr)), file = file_path, ...)

  invisible(file_path)
}
