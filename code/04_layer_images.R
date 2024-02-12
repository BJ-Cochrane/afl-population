library(tidyverse)
library(purrr)
library(fitzRoy)
library(glue)
library(lubridate)
library(rasterImage)
library(png)
library(magick)


player_details <- readRDS("data/player_details_2022.rds")
player_stats <- readRDS("data/player_stats_2022.rds")

team_names <-
  player_stats %>%
  select(team.name) %>%
  unique() %>%
  na.omit() %>%
  unlist()

load_and_raster <- function(img_url, alpha){

  png_load <- readPNG(source = img_url)

  png_load[,,4] = alpha

  rasterImage(png_load, 0, 0, 1, 1)

}


raster_teams <- function(team_name,
                         alpha_amount = 0.1){

if(team_name == "AFL"){
  imgpaths <- list.files(path="D:/data/player_photos_aflw/",
                         pattern = glue("^2023_"), full.names = T)

} else{


  imgpaths <- list.files(path="D:/data/player_photos_aflw/",
                         pattern = glue("^2023_{team_name}"), full.names = T)
}

  ## Shuffle order

  imgpaths_shuffle <- imgpaths %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    sample_n(nrow(.)) %>%
    select(value)%>%
    unlist()

  png(glue('output/faces/aflw/bulk/teams/{team_name}_shuffle_{alpha_amount}.png'), width = 2, height = 2, units = 'in', res = 300)
  par(mai=c(0,0,0,0))
  plot.new()

  walk(imgpaths_shuffle, load_and_raster, alpha = alpha_amount)

  dev.off()


}

walk(team_names, raster_teams, alpha_amount = 0.04)

walk("AFL",raster_teams, alpha_amount = 0.01)


walk2("AFL",raster_teams)

### Custom images

custom_raster <- function(pattern_name){


  imgpaths_custom <- list.files(path="D:/data/player_photos_aflw/",
                                pattern = glue("(2022|{pattern_name})(?:.+)(2022|{pattern_name})"), full.names = T)

  imgpaths_shuffle <- imgpaths_custom %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    sample_n(nrow(.)) %>%
    select(value)%>%
    unlist()

  alpha_set <- 1/length(imgpaths_shuffle)

  png(glue('output/faces/custom/{pattern_name}_{alpha_set}.png'), width = 2, height = 2, units = 'in', res = 150)
  par(mai=c(0,0,0,0))
  plot.new()

  walk(imgpaths_shuffle, load_and_raster, alpha = alpha_set)

  dev.off()

}


bulk <-
  player_stats %>%
  filter(season == 2022)%>%
  select(player.surname) %>%
  group_by(player.surname)%>%
  add_count() %>%
  unique() %>%
  arrange(desc(n)) %>%
  filter(n > 30) %>%
  select(player.surname) %>%
  unlist(0)


walk(bulk,custom_raster)



