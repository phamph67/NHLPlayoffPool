require(httr)
require(jsonlite)
require(dplyr)
require(stringr)
get_standings = function(date=NA){
  if(is.na(date)){
    cat("Using 'now' as current date\n")
    date = "now"
  }
  headers = add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `Accept` = "application/json"
  )
  # curl -L -X GET "https://api-web.nhle.com/v1/standings/now"
  url = str_glue("https://api-web.nhle.com/v1/standings/{date}")
  response = GET(url, headers)
  
  if(status_code(response) >= 300){
    stop("Request failed: ", status_code(response))
  }
  # parse response
  content_json = content(response, as = "text", encoding = "UTF-8")
  parsed_data = fromJSON(content_json, flatten = TRUE)
  standings = parsed_data$standings
  return(standings)
}
