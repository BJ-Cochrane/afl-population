library(pacman)

p_load(
  tidyverse,
  purrr,
  fitzRoy,
  glue,
  lubridate,
  rasterImage,
  png,
  RCurl,
  EBImage,
  magick
)

# install.packages("BiocManager")
# BiocManager::install("EBImage")
# library(EBImage)

player_details <- readRDS("data/player_details_2022.rds")
player_stats <- readRDS("data/player_aflw_stats_2023.rds")

# seasons <- c(2013:2023)
seasons <- c(2017:2023)


# all_players<-
# player_stats_new %>%
#   mutate(season = year(utcStartTime),
#          name = str_c(player.player.player.givenName,
#                       player.player.player.surname,
#                       sep = " ")) %>%
#   dplyr::select(season,name,team.name,player.photoURL) %>%
#   unique() %>%
#   mutate(player_id = str_c(season,team.name,name, sep = "_")) %>%
#   unique()

download_afl_images <- function(player_id,image_url){

  download.file(image_url,destfile=glue("D:/data/player_photos/{player_id}.png"), mode="wb")

}

walk2(all_players$player_id, all_players$player.photoURL, download_afl_images)


download_function <- function(season_no){


  all_players<-
    player_stats %>%
    mutate(season = year(utcStartTime),
           name = str_c(player.player.player.givenName,
                        player.player.player.surname,
                        sep = " ")) %>%
    dplyr::select(season,name,team.name,player.photoURL) %>%
    unique() %>%
    mutate(player_id = str_c(season,team.name,name, sep = "_")) %>%
    unique() %>%
    filter(season == season_no) %>%
    filter(player.photoURL != "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2023014/1031801.png?im=Scale,width=0.6,height=0.6")



  download_afl_images <- function(player_id,image_url){

    download.file(image_url,destfile=glue("D:/data/player_photos/{player_id}.png"), mode="wb")

  }

  walk2(all_players$player_id, all_players$player.photoURL, download_afl_images)



}



walk(2023,download_function)

walk(seasons,download_function)

