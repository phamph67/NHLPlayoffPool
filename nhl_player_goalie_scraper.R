# data scraper
library(tidyverse)
library(rvest)

# Importing data ----------------------------------------------------------

print("scraping goalies")
goalies = rvest::read_html(x = "https://www.hockey-reference.com/playoffs/NHL_2025_goalies.html") %>% 
  html_elements("table") %>%  
  html_table(header = T) %>% 
  pluck(1) %>%
  (\(tbl) {
    colnames(tbl) <- make.names(tbl[1, ])
    tbl %>% slice(2:nrow(tbl))
  })()
goalies$date_retrieved = Sys.Date()

print("scraping players")
players = rvest::read_html(x = "https://www.hockey-reference.com/playoffs/NHL_2025_skaters.html") %>% 
  html_elements("table") %>%
  html_table() %>% 
  pluck(1) %>%
  (\(tbl) {
    colnames(tbl) <- make.names(tbl[1, ], unique = TRUE)
    tbl %>% slice(2:nrow(tbl))
  })()
players$date_retrieved = Sys.Date()

if(!dir.exists('data')){
  dir.create('data')
} else {
  print("skipping, directory already exists")
}


print("writing data")

fname_goalie = str_glue("data/goalies_{Sys.Date()}.csv")
readr::write_csv(x = goalies, file = fname_goalie)

fname_player = str_glue("data/players_{Sys.Date()}.csv")
readr::write_csv(x = goalies, file = fname_player)