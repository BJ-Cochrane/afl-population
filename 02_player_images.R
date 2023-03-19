library(tidyverse)
library(purrr)
library(fitzRoy)
library(lubridate)
library(rasterImage)
library(png)
library(RCurl)
library(EBImage)
library(magick)
library(glue)
library(raster)
library(rgdal)
library(rasterVis)

player_details <- readRDS("data/player_details_2022.rds")
player_stats <- readRDS("data/player_stats_2022.rds")

pies2020<-
player_stats %>%
  select(season,name,team.name,player.photoURL) %>%
  unique() %>%
  filter(season == 2020, team.name == "Collingwood")


# tay14 <- readPNG(getURLContent("https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2022014/291776.png?im=Scale,width=0.6,height=0.6"))
#
tay22<-download.file("https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2020014/291776.png?im=Scale,width=0.6,height=0.6",
              destfile="tay22.png", mode="wb")

tay21<-readPNG(download.file("https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2020014/291790.png?im=Scale,width=0.6,height=0.6",
                     destfile="tay21.png", mode="wb"))


x <- "https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2022014/291776.png?im=Scale,width=0.6,height=0.6"



download_afl_images <- function(player_name,image_url){

  download.file(image_url,destfile=glue("data/pies2020/{player_name}_2020.png"), mode="wb")

}

walk2(pies2020$name, pies2020$player.photoURL, download_afl_images)


imgpaths <- list.files(path="data/pies2020/", full.names=T)

load_and_raster <- function(img_url, alpha){

  png_load <- readPNG(source = img_url)

  png_load[,,4] = alpha

  rasterImage(png_load, 0, 0, 1, 1)

}

imgpaths_rev <- imgpaths %>% sort(decreasing = TRUE)

png('testbulk.png', width = 2, height = 2, units = 'in', res = 150)
par(mai=c(0,0,0,0))
plot.new()

walk(imgpaths, load_and_raster, alpha = 0.2)

dev.off()

png('testbulkrev.png', width = 2, height = 2, units = 'in', res = 150)
par(mai=c(0,0,0,0))
plot.new()

walk(imgpaths_rev, load_and_raster, alpha = 0.1)

dev.off()



#
# raw <- readBin("https://s.afl.com.au/staticfile/AFL Tenant/AFL/Players/ChampIDImages/AFL/2022014/291776.png?im=Scale,width=0.6,height=0.6",
#                what = 'raw', n=50000)

# png <- readPNG(raw)
#
# readPNG("tmp.png")

tay22[,,4] = 0.5  # set alpha to semi-transparent
