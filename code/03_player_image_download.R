## download player images (to HDD)

library(tidyverse)
library(purrr)
library(fitzRoy)
library(glue)
library(lubridate)
library(rasterImage)
library(png)
library(RCurl)
library(EBImage)


player_details <- readRDS("data/player_details_2022.rds")
player_stats <- readRDS("data/player_stats_2022.rds")

seasons <- c(2013:2023)


download_function <- function(season_no){


  all_players<-
    player_stats %>%
    select(season,name,team.name,player.photoURL) %>%
    unique() %>%
    mutate(player_id = str_c(season,team.name,name, sep = "_")) %>%
    unique() %>%
    filter(season == season_no)



  download_afl_images <- function(player_id,image_url){

    download.file(image_url,destfile=glue("D:/data/player_photos/{player_id}.png"), mode="wb")

  }

  walk2(all_players$player_id, all_players$player.photoURL, download_afl_images)



}


walk(seasons,download_function)

















