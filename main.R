library(tidyverse)
library(rvest)
library(googlesheets4)

# Importing data ----------------------------------------------------------

pool_picks_raw = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BC_FybBRPVaG5H-LmA5hcPsYlvQT-VhEtZbMThUFqHc/edit?gid=728233770#gid=728233770")


# Data Wrangling ----------------------------------------------------------
pool_picks = pool_picks_raw[,colSums(is.na(pool_picks_raw)) == 0] %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(across(-c(timestamp, email_address), ~ gsub("\\s+", " ", .x)))
new_names = colnames(pool_picks) %>% 
  str_remove("pick_a_") %>% 
  str_remove("group_") 
colnames(pool_picks) = new_names

# get all unique players from pool pick list
pool_picks_players = pool_picks %>% 
  tidyr::pivot_longer(cols = 3:29, names_to = "position", values_to = 'player') %>% 
  select(player) %>% 
  unlist() %>% 
  gsub("\\s+", " ", .) %>% 
  unique()

# load dictionary
player_dictdf = readr::read_csv(file = "hockey_player_key.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::rename(name = form_in_google_forms,
                ref = form_in_hockey_reference) %>% 
  dplyr::select(-html) %>% 
  dplyr::mutate(name = gsub("\\s+", " ", name))
player_dict = setNames(player_dictdf$name, player_dictdf$ref)

# sanity check - player dict is good
pool_picks_players %in% player_dict

# gather goalies
goalie_fnames = dir("data") %>% grep(pattern = 'goalie', x = ., value = T)
goalie_fnames = paste0("data/", goalie_fnames)
goalies = vector(mode='list', length=length(goalie_fnames))
for(f in goalie_fnames){
  goalies[[f]] = readr::read_csv(file = f)
}
goalies = dplyr::bind_rows(goalies) %>% 
  dplyr::rename(player = Player) %>% 
  dplyr::filter(player %in% names(player_dict)) %>% 
  dplyr::mutate(player = player_dict[player]) %>% 
  dplyr::mutate(score = wins + assists + goals + shutouts) %>% 
  dplyr::select(player, team, date_retrieved, score)


# gather skaters
skater_fnames = dir("data") %>% grep(pattern = 'skater', x = ., value = T)
skater_fnames = paste0("data/", skater_fnames)
skaters = vector(mode='list', length=length(skater_fnames))
for(f in skater_fnames){
  skaters[[f]] = readr::read_csv(file = f)
}
skaters = dplyr::bind_rows(skaters) %>% 
  dplyr::rename(player = Player) %>%
  dplyr::filter(player %in% names(player_dict)) %>%
  dplyr::mutate(player = player_dict[player]) %>% 
  dplyr::mutate(score = goals + assists + game_winning_goals) %>% 
  dplyr::select(player, team, date_retrieved, score)

goalies_skaters = dplyr::bind_rows(goalies, skaters)

## NOTE ##
# not all pool pick players seem to be posted on the skater/goalies list
# the following are pool pick players which do not exist in skater+goalies list
player_dictdf$name[!player_dictdf$name %in% skaters_goalies$player]
# > player_dictdf$name[!player_dictdf$name %in% skaters_goalies$player]
# [1] "DAWS NJD"               "FLEURY MIN"             "KOTCHETKOV CAR"         "LINDGREN WSH"           "MARKSTROM NJD"          "RITTICH LAK"           
# [7] "SAMSONOV VGK"           "WOLL TOR"               "AARON EKBLAD FLA"       "ALIAKSEI PROTAS WSH"    "DYLAN HOLLOWAY STL"     "GABRIEL VILARDI WPG"   
# [13] "JACK HUGHES NJD"        "JASON ROBERTSON DAL"    "MARTIN NECAS COL"       "MATTIAS EKHOLM EDM"     "NIKOLAJ EHLERS WPG"     "OLIVER BJORKSTRAND TBL"
# [19] "SCOTT MORROW CAR"   



# Calculating poolpick team scores ----------------------------------------

dates = unique(goalies_skaters$date_retrieved)
results = vector(mode = 'list', length = length(dates))
for(i in 1:length(dates)){
  # calculate each poolpick score by date
  date_subset = goalies_skaters %>% 
    dplyr::filter(date_retrieved == dates[i])
  dict = setNames(date_subset$score, date_subset$player)
  
  results[[i]] = pool_picks %>% 
    dplyr::mutate(across(.cols = -c(timestamp, email_address), ~ dict[.x])) %>% 
    dplyr::mutate(across(.cols = -c(timestamp, email_address), ~ifelse(is.na(.x), yes = 0, no = .x))) %>% 
    dplyr::mutate(score = rowSums(across(-c(timestamp, email_address)), na.rm = TRUE),
                  date = dates[i])
}
results = dplyr::bind_rows(results)


# Plot results ------------------------------------------------------------

#' Plotting Function
#'
#' @param results, data frame with cols email_address, date, score
#'
#' @returns ggplot
plot_function = function(results){
  
  
  
  plt = ggplot2::ggplot(data = results,
                        mapping = aes(x = date, y = score, colour = email_address)) +
    geom_point()
  
}

plt = results %>% 
  ggplot2::ggplot(data = ., mapping = aes(x = ))