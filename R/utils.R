# set config for kibana server- store in env
set_config <- function(url, user, pwd) {
  Sys.setenv(KIBANA_BASE_URL = url)
  Sys.setenv(KIBANA_USER = user)
  Sys.setenv(KIBANA_PWD = pwd)
}


classify_user_agent <- function(ua) {
  if (grepl("Mozilla/5.0", ua, fixed = TRUE)) {
    return("browser")
  } 
  else if (grepl("python-requests", ua, fixed = TRUE)) {
    return("python-requests")
   } else if (grepl("Python-urllib", ua, fixed = TRUE)) {
     return("Python-urllib")
  } else if (grepl("insomnia", ua, fixed = TRUE)) {
   return("insomnia")
  } else if (grepl("libcurl", ua, fixed = TRUE)) {
    return("libcurl")
  } 
  else {
    return(ua)
  }
}

# extract useful data and convert to df
result_df <- function(result) {
  data.table::rbindlist(content(result)$aggregations$`2`$buckets)
}

travis_ips <- function() {
  c("34.73.34.132", "34.73.65.1", "34.73.66.97", "34.74.16.120", "34.74.79.111",
    "34.74.91.53", "34.74.253.255", "35.185.97.135", "35.196.72.151",
    "35.196.82.30", "35.196.99.99", "35.196.158.85", "35.227.58.83",
    "35.227.97.188", "35.229.115.143", "35.231.58.0", "35.237.8.208",
    "35.237.56.208", "35.237.212.185", "104.196.53.161", "104.196.57.92",
    "104.196.213.122")
}