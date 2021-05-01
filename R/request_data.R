#' Send a request for data
#' 
#' @param user string: username to authenticate request
#' @param pwd string: password to authenticate request
#' @param body string: list containing request
#' @export request_data

request_data <- function(body) {
  # for now write body to file until we work out how to send it without this 
  write(toJSON(body, auto_unbox = TRUE), file = '../temp_body.json')
  res <- POST(
    url = paste0(Sys.getenv("KIBANA_BASE_URL"),
                 "/api/console/proxy?path=_search&method=POST"),
    config = authenticate(Sys.getenv("KIBANA_USER"),
                          Sys.getenv("KIBANA_PWD"), type = "basic"),
    body = upload_file('../temp_body.json'),
    add_headers("kbn-xsrf" = "true")
  )
  file.remove('../temp_body.json')
  res
}