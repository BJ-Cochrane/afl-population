## Cohort construction

library(tidyverse)
library(ggplot2)
library(fitzRoy)
library(glue)
library(patchwork)
library(lubridate)
library(gganimate)
library(transformr)


player_details <- readRDS(file = "data/player_details_2022.rds")
player_stats <- readRDS(file = "data/player_stats_2022.rds")

today <- lubridate::as_date(Sys.Date())

population <- player_details %>%
  mutate(age = trunc((dateOfBirth %--% today) / years(1))) %>%
  mutate(age = age - (year(today) - season))

population_year_club <-
population %>%
  group_by(team,season)%>%
  add_count(age)%>%
  unique() %>%
  group_by(team)%>%
  complete(age = seq(min(population$age), max(population$age)),fill = list(n = 0)) %>%
  ungroup() %>%
  select(team,season,age,n)


population_year <-
  population %>%
  group_by(season)%>%
  add_count(age)%>%
  group_by(season)%>%
  complete(age = seq(min(population$age), max(population$age)),fill = list(n = 0)) %>%
  unique() %>%
  select(season,age,n) %>%
  unique()

ggplot(population_year)+
  geom_col(aes(x=age, y = n), size = 1, alpha = 0.5) +
  geom_hline(aes(yintercept=mean(age)),size = 1 , colour = "red")+
  labs(title = 'Year: {frame_time}')+
  theme_classic()+
  transition_time(season)+
  ease_aes()





population_club <-
population %>%
  select(team,age)%>%
  group_by(team,age)%>%
  add_count(age) %>%
  unique() %>%
  group_by(team)%>%
  complete(age = seq(min(population$age), max(population$age)),fill = list(n = 0)) %>%
  ungroup()

population_comp <-
population %>%
  select(age)%>%
  group_by(age)%>%
  add_count(age) %>%
  mutate(pct_teams = n/18)%>%
  unique() %>%
  ungroup()


population_ranges <-
population %>%
  select(age)%>%
  group_by(age)%>%
  add_count(age) %>%
  unique() %>%
  mutate(ranges = as.character(cut(age, seq(18,36, by = 2)))) %>%
  mutate(ranges = str_replace(ranges, ",","-"),
         ranges = str_remove(ranges,"\\("),
         ranges = str_remove(ranges,"\\]")) %>%
  mutate(ranges = case_when(age == 18 ~ "18-20",
                            age != 18 ~ ranges)) %>%
  group_by(ranges) %>%
  summarise(n = sum(n)) %>%
  ungroup()

ggplot(population_ranges)+
  geom_line(aes(x=ranges, y = n, group = 1), size = 1, alpha = 0.5)+
  scale_y_continuous(sec.axis = dup_axis())







### Player data load and save




