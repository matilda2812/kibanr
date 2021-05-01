#' Add filter to query
#' 
#' @export add_filter

add_filter <- function(body, filter) {
  num_filters <- length(body$query$bool$filter)
  
  if (length(filter[[names(filter)]]) > 1) {
    filter_value <- add_multi_filter(filter)
  } else {
    filter_value <- add_single_filter(filter)
  }
  num_filters <- length(body$query$bool$filter)
  body$query$bool$filter[[num_filters + 1]] <- filter_value
  body
}


add_single_filter <- function(filter) {
  if (grepl("*", filter)) {
    return(list(wildcard = filter))
  }
  list(match_phrase = filter)
}

add_multi_filter <- function(filter) {
  name <- names(filter)
  filters <- lapply(filter[[name]], function(x) {
    mp <- list(match_phrase = list())
    mp$match_phrase[[name]] <- x
    mp
  })
  list(bool = list(should = filters,
                   minimum_should_match = 1))
}

#' @export add_negative_filter
add_negative_filter <- function(body, filter) {
  num_filters <- length(body$query$bool$filter)
  
  if (length(filter[[names(filter)]]) > 1) {
    filter_value <- add_multi_filter(filter)
  } else {
    filter_value <- add_single_filter(filter)
  }
  num_filters <- length(body$query$bool$must_not)
  body$query$bool$must_not[[num_filters + 1]] <- filter_value
  body
}

# exclude travis ip addresses
#' @export add_travis_filter
add_travis_filter <- function(body) {
  num_filters <- length(body$query$bool$must_not)
  not_match <- list()
  ips <- travis_ips()
  for (i in seq_len(length(ips))) {
    not_match[[i]] <- list(match_phrase = list(
      "httpd.access.remote_ip.keyword" = ips[[i]]
    ))
  }
  body$query$bool$must_not[[num_filters + 1]] <- list(bool = list(
    should = not_match,
    minimum_should_match = 1
  ))
  body
}


#' @export add_date_filter
add_date_filter <- function(body, start, end) {
  
  if (!class(start) == "Date") {
    stop("start argument must be of class 'Date'")
  }
  if (!class(end) == "Date") {
    stop("end argument must be of class 'Date'")
  }
  # put date into correct format
  date_format <- "%Y-%m-%dT%H:%M:%S"
  end <- format(end, date_format)
  start <- format(start, date_format)
  
  filter <- list(range = list(
    `@timestamp` = list(
      gte = start, lte = end, format = "strict_date_optional_time"
    )
  ))
  num_filters <- length(body$query$bool$filter)
  body$query$bool$filter[[num_filters + 1]] <- filter
  body
}

email_exists_filter <- function(body) {
  num_filters <- length(body$query$bool$filter)
  body$query$bool$filter[[num_filters + 1]] <-
    list(exists = list(field = "httpd.access.url_params.email.keyword"))
  body
}