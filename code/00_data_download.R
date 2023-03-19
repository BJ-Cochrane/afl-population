### Player data load and save

library(tidyverse)
library(ggplot2)
library(fitzRoy)
library(glue)
library(patchwork)
library(lubridate)


player_details <- fitzRoy::fetch_player_details(current = FALSE)
years <- c(2014:2023)

player_stats <- map_dfr(years,fetch_player_stats_afl)

player_stats <-
  player_stats %>%
  mutate(season = year(utcStartTime)) %>%
  mutate(name = str_c(player.givenName,player.surname, sep = " "))


saveRDS(player_details, file = glue("data/player_details_year(Sys.time()).rds"))
saveRDS(player_stats, file = glue("data/player_details_year(Sys.time()).rds"))
