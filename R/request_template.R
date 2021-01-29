#' Return a nested list containing the base request template
#' 
#' @param path string: path specifiying where the json template is stored
#' @return list containing base request information
#' @export request_template

request_template <- function(path) {
  fromJSON(path, simplifyVector = FALSE)  
}