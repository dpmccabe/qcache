#' Remove cached function call results
#'
#' Remove cached function call results, i.e. .Rds files in the specified
#' \code{cache_dir} created by \code{\link{qcache}}.
#'
#' If \code{base_names} is NULL, all cached files in \code{cache_dir} are
#' removed, otherwise only those matching the vector of basenames are removed.
#'
#' @param cache_dir Full path to directory where .Rds file will be stored
#' @param base_names Vector of .Rds basenames
#'
#' @return Vector of basenames successfully deleted from file system
#' @export
#'
#' @examples
#' unqcache("~/cache", c("rnorm_result", "runif_result"))
unqcache <- function(cache_dir, base_names = NULL) {
  cache_dir <- gsub("\\/+$", "", cache_dir)
  if (!dir.exists(cache_dir)) stop("Cache directory ", cache_dir, " does not exist!")

  cached_fqfns <- list.files(cache_dir, "\\.Rds$", full.names = T, ignore.case = T)
  cached_filenames <- basename(cached_fqfns)
  cached_basenames <- tools::file_path_sans_ext(cached_filenames)

  if (is.null(base_names)) {
    matched_ix <- 1:length(cached_fqfns)
    missing_ix <- NA
  } else {
    matched_ix <- which(cached_basenames %in% base_names)
    missing_ix <- which(!(base_names %in% cached_basenames))
  }

  if (length(matched_ix) > 0) {
    obj_names <- paste0(cached_basenames[matched_ix], collapse = ", ")
    message("Deleting cached objects: ", obj_names)
    file.remove(cached_fqfns[matched_ix])
    message("Done deleting cached objects: ", obj_names)
  }

  if (length(missing_ix) > 0) {
    message("Cached objects not found: ", paste0(base_names[missing_ix], collapse = ", "))
  }

  return(obj_names)
}
