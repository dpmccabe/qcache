#' Cache the results of a function call
#'
#' Cache the results of a function call as an .Rds file with a specified
#' basename, unless there is already a cached version stored, in which case load
#' the cached version.
#'
#' Remove a cached object by deleting the file manually or calling
#' \code{\link{unqcache}}.
#'
#' @param cache_dir Full path to directory where .Rds file will be stored.
#' @param base_name Basename of .Rds file (i.e. ".Rds" will be appended).
#' @param fn A function or name of function.
#' @param ... Additional parameters passed to \code{fn}.
#'
#' @return The result of the function call with parameters \code{...}
#' @export
#'
#' @examples
#' # cache the result of this call
#' qcache("~/cache", "rnorm_result", rnorm, n = 100, mean = 0, sd = 1)
#'
#' # load the cached version
#' qcache("~/cache", "rnorm_result", rnorm, n = 100, mean = 0, sd = 1)
#' # alternatively,
#' qcache("~/cache", "rnorm_result")
qcache <- function(cache_dir, base_name, fn = NULL, ...) {
  dir.create(cache_dir, recursive = T, showWarnings = F)

  filename <- paste0(base_name, ".Rds", collapse = "")
  fqfn <- file.path(cache_dir, filename)

  if (file.exists(fqfn)) {
    message("Loading cached object: ", base_name)
    obj <- readRDS(fqfn)
    message("Done loading cached object: ", base_name)
  } else {
    if (!is.null(fn)) {
      message("Building object for cache: ", base_name)
      obj <- fn(...)
      message("Done building object for cache: ", base_name)

      message("Caching object: ", base_name)
      saveRDS(obj, file = fqfn)
      message("Done caching object: ", base_name)
    } else {
      stop("fn not defined")
    }
  }

  return(obj)
}
