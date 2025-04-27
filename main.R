library(tidyverse)
library(rvest)
library(googlesheets4)

# Importing data ----------------------------------------------------------

pool_picks = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1BC_FybBRPVaG5H-LmA5hcPsYlvQT-VhEtZbMThUFqHc/edit?gid=728233770#gid=728233770")


# Data Wrangling ----------------------------------------------------------

# gather goalies
goalie_fnames = dir("data") %>% grep(pattern = 'goalie', x = ., value = T)
goalie_fnames = paste0("data/", goalie_fnames)
goalies = vector(mode='list', length=length(goalie_fnames))
for(f in goalie_fnames){
  goalies[[f]] = readr::read_csv(file = f)
}
goalies = dplyr::bind_rows(goalies) 

# gather skaters
skater_fnames = dir("data") %>% grep(pattern = 'skater', x = ., value = T)
skater_fnames = paste0("data/", skater_fnames)
skaters = vector(mode='list', length=length(skater_fnames))
for(f in skater_fnames){
  skaters[[f]] = readr::read_csv(file = f)
}
skaters = dplyr::bind_rows(skaters)

# 