#' Read cached function call results
#'
#' Read cached function call results, i.e. an .Rds file in the specified
#' \code{cache_dir} created by \code{\link{qcache}}.
#'
#' @param cache_dir Full path to directory where .Rds file will be stored
#' @param base_name Vector of .Rds basenames
#'
#' @return Cached object
#' @export
#'
#' @examples
#' rqcache("~/cache", "rnorm_result")
rqcache <- function(cache_dir, base_name) {
  filename <- paste0(base_name, ".Rds", collapse = "")
  fqfn <- file.path(cache_dir, filename)

  if (file.exists(fqfn)) {
    message("Loading cached object: ", base_name)
    obj <- readRDS(fqfn)
    message("Done loading cached object: ", base_name)
  } else {
    stop(fqfn, " not found")
  }

  return(obj)
}
