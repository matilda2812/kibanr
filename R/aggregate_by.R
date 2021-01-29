#' Select a field to aggregate results by
#' 
#' @param request string: request to modify
#' @param field string: name of field to aggregate results by
#' @param limit number: number of results to return. 1000 by default
#' @return modified request as a list
#' @export aggregate_by

aggregate_by <- function(request, field, limit = 1000) {
  request$aggs <- list("2" = agg(field[[1]], limit))
  if (length(field) > 1) {
    request$aggs$`2`$aggs <- list("3" = agg(field[[2]], limit))
  }
  request
}

agg <- function(field, limit) {
  list(
    terms = list(
      field = field,
      order = list(
        "_count" = "desc"
      ),
      size = limit,
      "missing" = "__missing__"
    )
  )
}

