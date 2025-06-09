library(tidyverse)
library(rvest)
library(googlesheets4)
library(viridis)
library(ggrepel)
library(ggthemes)

# Importing data ----------------------------------------------------------

pool_picks_raw = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BC_FybBRPVaG5H-LmA5hcPsYlvQT-VhEtZbMThUFqHc/edit?gid=728233770#gid=728233770")


# Data Wrangling ----------------------------------------------------------
pool_picks = pool_picks_raw[,colSums(is.na(pool_picks_raw)) == 0] %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(across(-c(timestamp, email_address), ~ gsub("\\s+", " ", .x))) %>% 
  dplyr::rename(name = your_name_this_is_the_name_that_will_show_up_on_the_graph)
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


# Determine team status ---------------------------------------------------

team_fnames = grep("playoff_results", x = dir("data"), value = TRUE) %>% 
  paste0("data/", .)
team_status = vector(mode = 'list', length = length(team_fnames))

for(i in 1:length(team_fnames)){
  team_status[[i]] = readr::read_csv(file = team_fnames[i])
}
team_status = dplyr::bind_rows(team_status)
team_status = team_status %>% dplyr::filter(complete.cases(.))
team_status = team_status %>% 
  dplyr::mutate(eliminated = ifelse(team1_wins >= 4, yes = team2_short, NA))


## NOTE ##
# not all pool pick players seem to be posted on the skater/goalies list
# the following are pool pick players which do not exist in skater+goalies list
player_dictdf$name[!player_dictdf$name %in% goalies_skaters$player]
# > player_dictdf$name[!player_dictdf$name %in% skaters_goalies$player]
# [1] "DAWS NJD"               "FLEURY MIN"             "KOTCHETKOV CAR"         "LINDGREN WSH"           "MARKSTROM NJD"          "RITTICH LAK"           
# [7] "SAMSONOV VGK"           "WOLL TOR"               "AARON EKBLAD FLA"       "ALIAKSEI PROTAS WSH"    "DYLAN HOLLOWAY STL"     "GABRIEL VILARDI WPG"   
# [13] "JACK HUGHES NJD"        "JASON ROBERTSON DAL"    "MARTIN NECAS COL"       "MATTIAS EKHOLM EDM"     "NIKOLAJ EHLERS WPG"     "OLIVER BJORKSTRAND TBL"
# [19] "SCOTT MORROW CAR"   



# Calculating poolpick team scores ----------------------------------------

dates = unique(goalies_skaters$date_retrieved)
results = vector(mode = 'list', length = length(dates))
player_cols = c(
  "center_1", "center_2", "center_3", "center_4", "center_5", "center_6", "center_7", "center_8",
  "winger_1", "winger_2", "winger_3", "winger_4", "winger_5", "winger_6", "winger_7", "winger_8", "winger_9", "winger_10",
  "defenseman_1", "defenseman_2", "defenseman_3", "defenseman_4", "defenseman_5", "defenseman_6",
  "goalie_1", "goalie_2", "goalie_3"
)

skater_cols = c(
  "center_1", "center_2", "center_3", "center_4", "center_5", "center_6", "center_7", "center_8",
  "winger_1", "winger_2", "winger_3", "winger_4", "winger_5", "winger_6", "winger_7", "winger_8", "winger_9", "winger_10",
  "defenseman_1", "defenseman_2", "defenseman_3", "defenseman_4", "defenseman_5", "defenseman_6"
)

goalie_cols = c("goalie_1", "goalie_2", "goalie_3")

picked_skaters = unique(unlist(pool_picks[,skater_cols]))
picked_goalies = unique(unlist(pool_picks[, goalie_cols]))

for(i in 1:length(dates)){
  # gather eliminated teams
  eliminated_teams = team_status %>%
    dplyr::filter(date == dates[i]) %>% 
    dplyr::select(eliminated) %>% 
    filter(!is.na(eliminated)) %>% 
    as.vector() %>% 
    unlist()
  
  # calculate each poolpick score by date
  # date_subset = goalies_skaters %>% 
  #   dplyr::filter(date_retrieved == dates[i]) %>% 
  #   # eliminated teams = NA points
  #   dplyr::mutate(score = ifelse(team %in% eliminated_teams, NA, score))
  
  # scores accumulate and will plateau if teams are eliminated - no more score loss due to eliminated players
  date_subset = goalies_skaters %>% 
    dplyr::filter(date_retrieved == dates[i])
  
  skaters_subset = date_subset %>% 
    dplyr::filter(player %in% picked_skaters) %>% 
    dplyr::mutate(remaining = ifelse(team %in% eliminated_teams, 0, 1))
  goalies_subset = date_subset %>% 
    dplyr::filter(player %in% picked_goalies) %>% 
    dplyr::mutate(remaining = ifelse(team %in% eliminated_teams, 0, 1))
  
  dict = setNames(date_subset$score, date_subset$player)
  skater_remaining = setNames(skaters_subset$remaining, skaters_subset$player)
  goalie_remaining = setNames(goalies_subset$remaining, goalies_subset$player)
  remaining = pool_picks %>% 
    dplyr::mutate(across(.cols = all_of(skater_cols), ~skater_remaining[.x])) %>% 
    dplyr::mutate(across(.cols = all_of(goalie_cols), ~goalie_remaining[.x])) %>% 
    dplyr::mutate(skaters_remaining = rowSums(across(all_of(skater_cols)), na.rm = TRUE),
                  goalies_remaining = rowSums(across(all_of(goalie_cols)), na.rm = TRUE)) %>% 
    dplyr::mutate(players_remaining = skaters_remaining + goalies_remaining) %>% 
    dplyr::select(email_address, players_remaining, skaters_remaining, goalies_remaining)
  
  
  
  results[[i]] = pool_picks %>% 
    dplyr::mutate(across(.cols = -c(timestamp, email_address, name), ~ dict[.x])) %>% 
    # dplyr::mutate(across(.cols = -c(timestamp, email_address, name), ~ifelse(is.na(.x), yes = 0, no = .x))) %>% 
    dplyr::mutate(score = rowSums(across(all_of(player_cols)), na.rm = TRUE), 
                  # players_remaining = length(player_cols) - rowSums(is.na(across(all_of(player_cols)))),
                  date = dates[i]) %>% 
    dplyr::left_join(., remaining, by = "email_address")
  
  
  
  
}
results = dplyr::bind_rows(results)

# gather latest score and names to generate graph ranking + score display
results_maxscore = results %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::arrange(desc(score)) %>% 
  dplyr::mutate(name_labels = str_glue("{name} : {score}"))

# set y position for plotting
results_maxscore$y_position = rev(seq(min(results$score), max(results$score), length.out = length(results_maxscore$name)))

results$name = factor(results$name, levels = results_maxscore$name)

max_players = max(results$players_remaining)

latest_players_remaining = results %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::arrange(desc(players_remaining)) %>% 
  dplyr::mutate(name_labels = str_glue("{name} : Players: {players_remaining}, Skaters: {skaters_remaining}, Goalies {goalies_remaining}"),
                y_position = rev(seq(min(players_remaining), max_players, length.out = length(players_remaining))))
latest_players_remaining$name = factor(latest_players_remaining$name, levels = latest_players_remaining$name)

# set colours
n_lines = length(results_maxscore$name)
colours = viridisLite::viridis(n = n_lines)
pal = viridis::viridis(n = 10, option = "D")
pal_rep = rep(pal, length.out = n_lines) 

pal2 = colorspace::qualitative_hcl(n = n_lines)

# set name labels
name_labels = str_glue("{results_maxscore$name} : {results_maxscore$score}")

# Plot results ------------------------------------------------------------

#' Plotting Function
#'
#' @param results, data frame with cols email_address, date, score
#'
#' @returns ggplot
plot_function = function(data, name_labs, name_colours, results_maxscore){
  plt = ggplot2::ggplot(data = data,
                        mapping = aes(x = date, y = score, colour = name)) +
    geom_point() +
    geom_line(alpha = 0.35, linetype = 2) +
    scale_colour_manual(name = "Name (By descending latest score)", values = name_colours, labels = name_labs) +
    scale_y_continuous(name = "Score", 
                       breaks = seq(0,500,20), 
                       minor_breaks = seq(0, 500, 10)) +
    scale_x_date(name = "Date", 
                 expand = expansion(mult = c(0.1, 0.3)),
                 date_labels = "%b %d",
                 date_breaks = "1 day")  +
    geom_segment(data = results_maxscore,
                 mapping = aes(x = date,
                     xend = (date + 0.5),
                     y = score, 
                     yend = y_position),
                 alpha = 0.35) +
    geom_text(data = results_maxscore,
              aes(x = date + 0.5,
                  y = y_position, 
                  label = name_labels),
              hjust = -0.2,
              size = 4) +
    ggthemes::theme_clean() +
    theme(legend.position = "None",
          axis.text.x=element_text(angle=45, hjust=1))
    # ggrepel::geom_label_repel(data = results_maxscore,
    #         mapping = aes(label = name_labels, y = y_position),
    #         direction = 'y',
    #         force = 2,
    #         max.overlaps = 10,
    #         size = 3,
    #         hjust = -0.2,
    #         nudge_x = 0.5,
    #         label.padding = 0.1)
  return(plt)
}

plt = plot_function(data = results, name_labs = name_labels, name_colours=pal2, results_maxscore =results_maxscore)


players_remaining_labels = str_glue("{results_maxscore$name} : {results_maxscore$score}")
players_remaining_plot = function(data, name_colours, results_maxscore){
  plt = ggplot2::ggplot(data = data,
                        mapping = aes(x = date, y = players_remaining, colour = name)) +
    geom_point() +
    geom_line(alpha = 0.35, linetype = 2) +
    scale_colour_manual(name = "Name (By descending latest score)", values = name_colours, labels = names) +
    scale_y_continuous(name = "Players remaining", 
                       breaks = seq(0,100,4), 
                       minor_breaks = seq(0, 100, 2)) +
    scale_x_date(name = "Date", 
                 expand = expansion(mult = c(0.1, 0.3)),
                 date_labels = "%b %d",
                 date_breaks = "1 day")  +
    geom_segment(data = results_maxscore,
                 mapping = aes(x = date,
                               xend = (date + 1.5),
                               y = players_remaining, 
                               yend = y_position),
                 alpha = 0.35) +
    geom_text(data = results_maxscore,
              aes(x = date + 0.5,
                  y = y_position, 
                  label = name_labels),
              hjust = -0.2,
              size = 4) +
    ggthemes::theme_clean() +
    theme(legend.position = "None",
          axis.text.x=element_text(angle=45, hjust=1))
  return(plt)
}

plt2 = players_remaining_plot(data = results, name_colours=pal2, results_maxscore=latest_players_remaining)
plt2
if(!dir.exists("output")){
  dir.create("output")
}

png(filename = str_glue("output/pool_pick_visualization_{Sys.Date()}.png"), width = 16, height = 10, units = 'in', res = 300)
plt
dev.off()

png(filename = str_glue("output/pool_pick_visualization_players_{Sys.Date()}.png"), width = 18, height = 10, units = 'in', res = 300)
plt2
dev.off()
