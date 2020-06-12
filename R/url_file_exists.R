url_file_exists <- function(url){
  HTTP_STATUS_OK <- c(200,302)
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status %in% HTTP_STATUS_OK, status = status)
}
