# data scraper
library(tidyverse)
library(rvest)
source("get_standings.R")
# Importing data ----------------------------------------------------------

print("scraping goalies")
page_goalies = read_html("https://www.hockey-reference.com/playoffs/NHL_2025_goalies.html")
table_nodes = page_goalies %>%  html_node("table#stats")
goalies = table_nodes %>% 
  html_table(fill=TRUE, header = T)
colnames(goalies) = goalies[1,]
goalies = goalies %>% filter(Rk != "Rk")
goalies = goalies %>% 
  dplyr::mutate(date_retrieved = Sys.Date()) %>% 
  dplyr::rename(rank = Rk,
                team = Tm,
                games_played = GP,
                games_started = GS,
                wins = W,
                losses = L,
                goals_against = GA,
                shots_against = SA,
                saves = SV,
                saves_pct = `SV%`,
                goals_against_avg = GAA,
                shutouts = SO,
                goalie_point_shares = GPS,
                mins_played = MIN,
                quality_starts = QS,
                quality_starts_rate = `QS%`,
                really_bad_starts = RBS,
                goals_against_avgleague = `GA%-`,
                goals_saved_above_avg = GSAA,
                goals = G,
                assists = A,
                points = PTS,
                penality_mins = PIM)

print("scraping players")
page_skaters = read_html("https://www.hockey-reference.com/playoffs/NHL_2025_skaters.html")
table_nodes = page_skaters %>%  html_node("table#stats")
skaters = table_nodes %>% 
  html_table(fill=TRUE, header = T)
colnames(skaters) = skaters[1,]
skaters = repair_names(skaters)
skaters = skaters %>% filter(Rk != "Rk")
skaters = skaters %>%
  dplyr::mutate(date_retrieved = Sys.Date()) %>%
  dplyr::rename(rank = Rk,
                team = Tm,
                position = Pos,
                goals = G,
                assists = A,
                points = PTS,
                plus_minus = `+/-`,
                penality_mins = PIM,
                even_strength_goals = EV,
                power_play_goals = PP,
                short_handed_goals = SH,
                game_winning_goals = GW,
                even_strength_assists = EV1,
                power_play_assists = PP1,
                short_handed_assists = SH1,
                shots_on_goal = S,
                shooting_percentage = `S%`,
                total_ice_time = TOI,
                avg_total_ice_time = ATOI,
                blocks = BLK,
                hits = HIT,
                faceoff_wins = FOW,
                faceoff_losses = FOL,
                faceoff_pct = `FO%`)

# fetch standings
print("Fetching team standings")
standings = get_standings()

# getch playoff results
print("Fetching Playoff results")
playoff_results = get_playoff_rounds()

print("writing data")

fname_goalie = str_glue("data/goalies_{Sys.Date()}.csv")
readr::write_csv(x = goalies, file = fname_goalie)

fname_skaters = str_glue("data/skaters_{Sys.Date()}.csv")
readr::write_csv(x = skaters, file = fname_skaters)

fname_standings = str_glue("data/standings_{Sys.Date()}.csv")
readr::write_csv(x = standings, file = fname_standings)

fname_playoff_results = str_glue("data/playoff_results_{Sys.Date()}.csv")
readr::write_csv(x = playoff_results, file = fname_playoff_results)
