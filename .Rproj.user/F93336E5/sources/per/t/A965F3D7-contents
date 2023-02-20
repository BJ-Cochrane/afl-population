## Cohort construction

library(tidyverse)
library(ggplot2)
library(fitzRoy)
library(glue)
library(patchwork)
library(lubridate)


aflplayers_afltables <- fitzRoy::fetch_player_details(source = 'afltables',
                              current = FALSE)



today <- lubridate::as_date(Sys.Date())


pop2023 <- fitzRoy::fetch_player_details()%>%
  mutate(age = trunc((dateOfBirth %--% today) / years(1)))



pop2023club <-
pop2023 %>%
  select(team,age)%>%
  group_by(team,age)%>%
  add_count(age) %>%
  unique() %>%
  group_by(team)%>%
  complete(age = seq(min(pop2023$age), max(pop2023$age)),fill = list(n = 0)) %>%
  ungroup()

pop2023comp <-
pop2023 %>%
  select(age)%>%
  group_by(age)%>%
  add_count(age) %>%
  mutate(pct_teams = n/18)%>%
  unique() %>%
  ungroup()


pop2023_ranges <-
pop2023 %>%
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

ggplot(pop2023_ranges)+
  geom_line(aes(x=ranges, y = n, group = 1), size = 1, alpha = 0.5)+
  scale_y_continuous(sec.axis = dup_axis())


ggplot(pop2023club)+
  geom_line(aes(x=age, y = n, colour = team), size = 1, alpha = 0.5)+
  geom_line(data = pop2023comp, aes(x=age,y=n/10),colour = 'red', size = 2)+
  scale_y_continuous(
    breaks = c(0,2,4,6,8,10),
    name = "First Axis",
    sec.axis = sec_axis(~.*10)
  )






