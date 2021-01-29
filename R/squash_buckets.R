#' Turn nested json into 2D dataframe
#' 
#' Handles case of one or two aggregations
#' @param resp string: response object to convert
#' @return data.frame of data
#' @export squash_buckets

squash_buckets <- function(result) {
  resp <- content(result)
  # check how nested the data is
  if ("3" %in% names(resp$aggregations$`2`$buckets[[1]])) {
    # two aggs
    rows <- data.table::rbindlist(lapply(resp$aggregations$`2`$buckets, function(b) {
      key1 <- b$key
      df <- data.table::rbindlist(b$`3`$buckets)
      df$key2 <- b$key
      df
    }))
  } else {
    # one agg
    rows <- data.table::rbindlist(resp$aggregations$`2`$buckets)
  }
  rows
}