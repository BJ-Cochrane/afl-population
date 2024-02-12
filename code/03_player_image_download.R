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

# seasons <- c(2013:2023)
seasons <- c(2023:2019)

download_function <- function(season_no = 2023){


  all_players<-
    player_stats %>%
    mutate(season = year(utcStartTime),
           name = str_c(player.player.player.givenName,
                        player.player.player.surname,
                        sep = " ")) %>%
    select(season,name,team.name,player.photoURL) %>%
    unique() %>%
    mutate(player_id = str_c(season,team.name,name, sep = "_")) %>%
    unique() %>%
    filter(season == season_no) %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2019264/1007157.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2020264/1016956.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2023264/1025164.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2023264/1009869.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2023264/1012547.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2022264/1012700.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2021264/1017027.png?im=Scale,width=0.6,height=0.6") %>%


    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2023264/1012964.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2022264/1012719.png?im=Scale,width=0.6,height=0.6") %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2021264/1006666.png?im=Scale,width=0.6,height=0.6") %>%

    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFLW/2020264/1011091.png?im=Scale,width=0.6,height=0.6")



  download_afl_images <- function(player_id,image_url){

    download.file(image_url,destfile=glue("D:/data/player_photos_aflw/{player_id}.png"), mode="wb")

  }

  walk2(all_players$player_id, all_players$player.photoURL, download_afl_images)



}


walk(seasons,safely(download_function))

















