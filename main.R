library(tidyverse)
library(rvest)
library(googlesheets4)



# Importing data ----------------------------------------------------------

pool_picks = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BC_FybBRPVaG5H-LmA5hcPsYlvQT-VhEtZbMThUFqHc/edit?gid=728233770#gid=728233770")

goalies_raw = rvest::read_html(x = "https://www.hockey-reference.com/playoffs/NHL_2025_goalies.html") %>% 
  html_elements("table") %>%  
  html_table(header = T) %>% 
  pluck(1) %>%
  (\(tbl) {
    colnames(tbl) <- make.names(tbl[1, ])
    tbl %>% slice(2:nrow(tbl))
  })()

players_raw = rvest::read_html(x = "https://www.hockey-reference.com/playoffs/NHL_2025_skaters.html") %>% 
  html_elements("table") %>%
  html_table() %>% 
  pluck(1) %>%
  (\(tbl) {
    colnames(tbl) <- make.names(tbl[1, ], unique = TRUE)
    tbl %>% slice(2:nrow(tbl))
  })()


# Data Wrangling ----------------------------------------------------------

goalies <- goalies_raw %>%
  dplyr::rename(rank = Rk,
                team = Tm,
                games_played = GP,
                games_started = GS,
                wins = W,
                losses = L,
                goals_against = GA,
                shots_against = SA,
                saves = SV,
                saves_pct = `SV.`,
                goals_against_avg = GAA,
                shutouts = SO,
                goalie_point_shares = GPS,
                mins_played = MIN,
                quality_starts = QS,
                quality_starts_rate = `QS.`,
                really_bad_starts = RBS,
                goals_against_avgleague = `GA..`,
                goals_saved_above_avg = GSAA,
                goals = G,
                assists = A,
                points = PTS,
                penality_mins = PIM
                
  )
