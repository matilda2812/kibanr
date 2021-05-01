#' Group url paths
#' 
#' 
#' @export classify_url_paths

classify_url_paths <- function(path) {
  if (grepl("/image/", path)) {
    return("/image/")
  } else if (grepl("/intersect/batch/", path)) {
    return("intersect_batch")
  } else if (grepl("fieldguide", path)) {
    return("fieldguide")
  }
  return(path)
}