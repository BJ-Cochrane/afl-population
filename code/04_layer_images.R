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
                         alpha_amount = 0.05){

imgpaths <- list.files(path="D:/data/player_photos/",
                  pattern = glue("^2022_{team_name}"), full.names = T)

## Shuffle order

imgpaths_shuffle <- imgpaths %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  sample_n(nrow(.)) %>%
  select(value)%>%
  unlist()

png(glue('output/faces/{team_name}_shuffle.png'), width = 2, height = 2, units = 'in', res = 150)
par(mai=c(0,0,0,0))
plot.new()

walk(imgpaths_shuffle, load_and_raster, alpha = alpha_amount)

dev.off()


}


walk(team_names, raster_teams)




### Custom images



imgpaths_custom <- list.files(path="D:/data/player_photos/",
                       pattern = glue("(202\\d|Smith)(?:.+)(202\\d|Smith)"), full.names = T)



imgpaths_custom <- list.files(path="D:/data/player_photos/",
                              pattern = glue("(2022|De Koning)(?:.+)(2022|De Koning)"), full.names = T)

  imgpaths_shuffle <- imgpaths_custom %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    sample_n(nrow(.)) %>%
    select(value)%>%
    unlist()

  png(glue('output/faces/custom/DK_shuffle.png'), width = 2, height = 2, units = 'in', res = 150)
  par(mai=c(0,0,0,0))
  plot.new()

  walk(imgpaths_shuffle, load_and_raster, alpha = 0.3)

  dev.off()


custom_raster <- function(pattern_name, alpha_set = 0.25){


  imgpaths_custom <- list.files(path="D:/data/player_photos/",
                                pattern = glue("(2022|{pattern_name})(?:.+)(2022|{pattern_name})"), full.names = T)

  imgpaths_shuffle <- imgpaths_custom %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    sample_n(nrow(.)) %>%
    select(value)%>%
    unlist()

  png(glue('output/faces/custom/{pattern_name}.png'), width = 2, height = 2, units = 'in', res = 150)
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



