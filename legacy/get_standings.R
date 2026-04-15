require(httr)
require(jsonlite)
require(dplyr)
require(tidyr)
require(stringr)
require(rvest)
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

get_playoff_rounds = function(){
  # Specify the URL of the NHL Playoff Series page
  url = "https://www.hockey-reference.com/playoffs/NHL_2025.html"
  page = read_html(url)
  tables = html_nodes(page, "table")
  first_table = html_table(tables[[1]])
  head(first_table)
    
  table1_parsed = first_table %>% 
    dplyr::filter(str_detect(X1, "Round|Final")) %>% 
    dplyr::select(X1,X2,X3) %>%
    tidyr::separate(
      col = X3,
      into = c("team1_full", "Remaining"),
      sep = "\\s(over|lead)\\s", # Split at " over " or " lead " (with spaces)
      remove = FALSE
    ) %>%
    tidyr::separate(
      col = Remaining,
      into = c("team2_full", "Extra"),
      sep = "(?=\\s[A-Z]{3}$)", # Split before the last group of 3 uppercase letters
      remove = TRUE,
      fill = "right" # In case of unexpected formats
    ) %>%
    dplyr::mutate(
      team1_short = case_when(
        grepl("Edmonton", team1_full) ~ "EDM",
        grepl("Los Angeles", team1_full) ~ "LAK",
        grepl("Vegas", team1_full) ~ "VGK",
        grepl("Minnesota", team1_full) ~ "MIN",
        grepl("Dallas", team1_full) ~ "DAL",
        grepl("Colorado", team1_full) ~ "COL",
        grepl("Winnipeg", team1_full) ~ "WPG",
        grepl("St. Louis", team1_full) ~ "STL",
        grepl("Carolina", team1_full) ~ "CAR",
        grepl("New Jersey", team1_full) ~ "NJD",
        grepl("Washington", team1_full) ~ "WSH",
        grepl("Montreal", team1_full) ~ "MTL",
        grepl("Florida", team1_full) ~ "FLA",
        grepl("Tampa Bay", team1_full) ~ "TBL",
        grepl("Toronto", team1_full) ~ "TOR",
        grepl("Ottawa", team1_full) ~ "OTT",
        TRUE ~ NA_character_
      ),
      team2_short = case_when(
        grepl("Kings", team2_full) ~ "LAK",
        grepl("Oilers", team2_full) ~ "EDM",
        grepl("Golden Knights", team2_full) ~ "VGK",
        grepl("Wild", team2_full) ~ "MIN",
        grepl("Stars", team2_full) ~ "DAL",
        grepl("Avalanche", team2_full) ~ "COL",
        grepl("Jets", team2_full) ~ "WPG",
        grepl("Blues", team2_full) ~ "STL",
        grepl("Hurricanes", team2_full) ~ "CAR",
        grepl("Devils", team2_full) ~ "NJD",
        grepl("Capitals", team2_full) ~ "WSH",
        grepl("Canadiens", team2_full) ~ "MTL",
        grepl("Panthers", team2_full) ~ "FLA",
        grepl("Lightning", team2_full) ~ "TBL",
        grepl("Maple Leafs", team2_full) ~ "TOR",
        grepl("Senators", team2_full) ~ "OTT",
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::separate(
      col = X2,
      into = c("team1_wins", "team2_wins"),
      sep = "-",
      remove = FALSE,
      convert = TRUE # Automatically convert to numeric
    ) %>% 
    dplyr::rename(round = X1) %>% 
    dplyr::select(round, team1_wins, team2_wins, team1_short, team2_short) %>% 
    dplyr::mutate(date = Sys.Date(),
                  round = str_to_lower(round) %>%  str_replace(., ' ', '_'))
  
  return(table1_parsed)
}
