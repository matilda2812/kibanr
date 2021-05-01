#' Write results to csv
#'
#' @export write_result

write_result <- function(result, date, path) {
  result$date <- date
  write.csv(result, path, row.names = FALSE)
}