### Player data load and save

library(tidyverse)
library(fitzRoy)
library(glue)
library(lubridate)


### AFLM

player_details <- fitzRoy::fetch_player_details(current = FALSE)
years <- c(2014:2023)

player_stats <- map_dfr(years,fetch_player_stats_afl)

player_stats <-
  player_stats %>%
  mutate(season = year(utcStartTime)) %>%
  mutate(name = str_c(player.givenName,player.surname, sep = " "))


saveRDS(player_details, file = glue("data/player_aflm_details_year(Sys.time()).rds"))
saveRDS(player_stats, file = glue("data/player_aflm_details_year(Sys.time()).rds"))

### AFLW


player_details <- fitzRoy::fetch_player_details(current = FALSE,
                                                comp = "AFLW")
years <- c(2014:2023)

player_stats <- map_dfr(years,fetch_player_stats_afl, comp = "AFLW")

player_stats <-
  player_stats %>%
  mutate(season = year(utcStartTime)) %>%
  mutate(name = str_c(player.givenName,player.surname, sep = " "))


saveRDS(player_details, file = "data/player_aflw_details_2023.rds")
saveRDS(player_stats, file = "data/player_aflw_details_2023.rds")
