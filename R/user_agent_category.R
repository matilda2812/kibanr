#' Classify user agent for aggregated stats
#' 
#' @export user_agent_category

user_agent_category <- function(ua) {
  if (grepl("Safari", ua) | grepl("Mozilla", ua) | grepl("Opera", ua)) {
    return("web-browser")
  } else if (grepl("r-curl", ua)) {
    return("R")
  } else if (grepl("python", ua, ignore.case = TRUE)) {
    return("python")
  } else if (grepl("insomnia", ua, ignore.case = TRUE)) {
    return("insomnia-client")
  } else if (grepl("java", ua, ignore.case = TRUE)) {
    return("Java")
  } else {
    return(ua)
  }
}