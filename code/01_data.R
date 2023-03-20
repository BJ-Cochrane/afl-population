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

player_stats_afltables <- fitzRoy::fetch_player_details_afltables() %>%
  select(Player,Games,Debut,Last) %>%
  rename(name = Player) %>%
  group_by(name) %>%
  filter(Games == max(Games))

population_merged <-
  population %>%
  mutate(name = str_c(firstName,surname,sep = " "))%>%
  left_join(player_stats_afltables, by = "name") %>%
  mutate(Debut = gsub('[d|y]','',Debut),
         Last = gsub('[d|y]','',Last))%>%
  separate(Debut, c("debut_y","debut_d"))%>%
  separate(Last, c("last_y","last_d")) %>%
  mutate(across(starts_with('debut'), ~ as.numeric(.)),
         across(starts_with('last'), ~ as.numeric(.)),
         across(ends_with("_d"), ~ ./365)) %>%
  mutate(debut = debut_y + debut_d,
         last = last_y + last_d) %>%
  select(-debut_y,-debut_d,-last_y,-last_d)

population_life <-
population %>%
  mutate(name = str_c(firstName,surname,sep = " "))%>%
  group_by(id)%>%
  arrange(season)%>%
  mutate(duplicated = duplicated(id)) %>%
  mutate(status =
           case_when(row_number()==1 ~ 'birth',
                     row_number()==n() & season != "2023" ~ "death",
                     TRUE ~ 'alive')) %>%
  mutate(status = case_when(n()==1 & season != "2023"  ~ "birth-death",
                            TRUE ~ status)) %>%
  ungroup() %>%
  group_by(season,age,status) %>%
  summarise(n = n()) %>%
  ungroup()%>%
  complete(season,age,status,fill = list(n = 0)) %>%
  select(age,season,status,n)%>%
  unique() %>%
  filter(season != "2014") %>%
  ungroup()


cbbPalette <- c("#0072B2", "#009E73","#E69F00", "#D55E00")

anim <- ggplot(population_life)+
  geom_col(aes(x=age, y = n, fill = status)) +
  scale_fill_manual(values = cbbPalette)+
  labs(title = 'Year: {frame_time}')+
  transition_time(season)+
  ease_aes()

animate(anim, fps = 25, duration = 25)

anim_save('test.gif')

population_life %>%
  select(season,age,status)%>%
  group_by(season)%>%
  add_count(age)%>%
  add_count(status) %>%
  unique() %>%
ggplot()+
  geom_col(aes(x=age, y = n, fill = status))+
  labs(title = 'Year: {frame_time}')+
  theme_classic()+
  transition_time(season)+
  ease_aes()


population_selwood <-
population %>%
  ungroup()%>%
  mutate(selwood = case_when(firstName == "Joel" & surname == "Selwood" ~ 0,
                              TRUE ~ 1)) %>%

  group_by(season)%>%
  add_count(age,selwood)%>%
  group_by(season)%>%
  complete(age = seq(min(population$age), max(population$age)),fill = list(n = 0)) %>%
  select(season,age,n,selwood) %>%
  unique() %>%
  mutate(selwood = as.character(selwood)) %>%
  mutate(selwood = str_replace_na(selwood,"1"))


ggplot(population_selwood, aes(fill=selwood, y=n, x=age)) +
  geom_bar(position="stack", stat="identity")



ggplot(population_year)+
  geom_col(aes(x=age, y = n), size = 1, alpha = 0.5) +
  geom_line(aes(x=age, y = n), colour = 'red', size = 2)+
  labs(title = 'Year: {frame_time}')+
  theme_classic()+
  transition_time(season)+
  ease_aes()

population_year %>%
  group_by(season) %>%
  summarise(n = sum(n)) %>%

ggplot()+
  geom_col(aes(x=season, y = n, fill = n)) +
  labs(title = 'Year: {frame_time}')+
  theme_classic()+
  guides(fill = 'none') +
  transition_time(season)+
  shadow_mark()
  ease_aes()



