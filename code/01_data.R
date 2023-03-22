## Cohort construction

library(tidyverse)
library(ggplot2)
library(fitzRoy)
library(glue)
library(patchwork)
library(lubridate)
library(gganimate)
library(transformr)
library(zoo)
library(magick)
library(scales)


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
           case_when(row_number()==1 ~ 'Birth',
                     row_number()==n() & season != "2023" ~ "Death",
                     TRUE ~ 'Alive')) %>%
  mutate(status = case_when(n()==1 & season != "2023"  ~ "BirthDeath",
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


fill_colours <- c(
          Alive = "#0072B2",
          Birth = "#009E73",
          BirthDeath = "#E69F00",
          Death = "#D55E00")



ggplot(population_life)+
  geom_col(aes(x=age, y = n, fill = status)) +
  scale_fill_manual(values = fill_colours)+
  labs(title = 'AFL Population - Year: {frame_time}',
       x = "Age",
       y = "Population",
       fill = "Population status")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text = element_text(size = 11,
                                 colour = "black"),
        plot.title  = element_text(size = 14, face = "bold"),
        title = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(18:40), expand = c(0.025,0))




anim1 <-
  ggplot(population_life)+
  geom_col(aes(x=age, y = n, fill = status)) +
  scale_fill_manual(values = fill_colours)+
  labs(title = 'AFL Population - Year: {frame_time}',
       x = "Age",
       y = "Population",
       fill = "Population status",
       caption = "@BenCochraneR")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text = element_text(size = 13,
                                 colour = "black"),
        plot.title  = element_text(size = 16, face = "bold"),
        title = element_text(size = 15),
        plot.caption =  element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_y_continuous(expand = c(0,0), breaks = breaks_pretty(8))+
  scale_x_continuous(breaks = c(18:40), expand = c(0.025,0))+
  transition_time(season)+
  ease_aes()


a1 <- animate(anim1, fps = 40, duration = 15, width = 750, height = 600, end_pause = 75)

anim_save('output/pop1.gif', animation = a1)


rolling_births_deaths <-
  population_life %>% arrange(season)%>%
  group_by(season,status)%>%
  summarise(n = sum(n)) %>%
  group_by(status) %>%
  filter(status %in% c("Birth","Death")) %>%
  mutate(nsum = cumsum(n))

anim2 <-
ggplot(rolling_births_deaths)+
  geom_col(aes(x=status,y=nsum,fill = status))+
  scale_fill_manual(values = fill_colours[c(2,4)])+
  labs(x = "Population status",
       y = "Cumulative number")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text = element_text(size = 13,
                                 colour = "black"),
        plot.title  = element_text(size = 16, face = "bold"),
        title = element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_y_continuous(expand = c(0,0),
                     breaks = breaks_pretty(10))+
  transition_time(season)+
  ease_aes()




a2 <- animate(anim2, fps = 30, duration = 15,width = 250, height = 500, end_pause = 75)

anim_save('output/pop2.gif', animation = a2)


a_mgif <- image_read(a1)
b_mgif <- image_read(a2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 1:600){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif,path = "output/pop4.gif", format = 'gif')


rolling_population <-
  population %>%
  arrange(season)%>%
  group_by(season)%>%
  add_count(id)%>%
  summarise(n = sum(n))

anim3 <-
ggplot(rolling_population)+
  geom_line(aes(x=season, y =n), size = 1.5, colour = "#0072B2") +
  geom_point(aes(x=season, y =n), color = "#D55E00", size = 4)+
  labs(title = 'AFL population 2014-2023',
       x = "Season",
       y = "Population")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text = element_text(size = 11,
                                 colour = "black"),
        plot.title  = element_text(size = 14, face = "bold"),
        title = element_text(size = 12))+
  scale_y_continuous(breaks = breaks_pretty(6))+
  scale_x_continuous(breaks = c(2014:2023))+
  transition_reveal(season)+
  shadow_mark(colour = "#D55E00", size = 4, exclude_layer = 1)+
  ease_aes()

a3 <- animate(anim3, fps = 35, duration = 15,width = 500, height = 500, end_pause = 15)


anim_save('output/pop3.gif', animation = a3)



club_pops <-
population %>%
  filter(season == 2023) %>%
  arrange(season)%>%
  group_by(season,team)%>%
  add_count(id) %>%
  summarise(n = sum(n))%>%
  select(team,n)

# Team colours from @crow_data_sci
team_colours <- c(
  'Carlton'= '#0e1e2d',
  'Geelong Cats'= '#1c3c63',
  'Adelaide Crows'= '#002b5c',
  'Sydney Swans' = '#ed171f',
  'St Kilda' = '#ed0f05',
  'Essendon' = '#cc2031',
  'Richmond'= '#D8D800',
  'Melbourne'= '#0f1131',
  'Hawthorn' = '#4d2004',
  'Brisbane Lions' = '#a30046',
  'Gold Coast Suns' = '#d93e39',
  'Fremantle'= '#2a1a54',
  'Collingwood'= '#000000',
  'Port Adelaide'  = '#01b5b6',
  'Western Bulldogs'  = '#014896',
  'West Coast Eagles' = '#062ee2',
  'North Melbourne'= '#013b9f',
  'GWS Giants' = '#f15c22')

club_pops %>%
  mutate(label_y = n - 1) %>%
  mutate(team = fct_reorder(team, n)) %>%
  ggplot(aes(y=team,x=n,fill=team))+
  scale_fill_manual(values=team_colours)+
  geom_col()+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,50))+
  labs(
    x="List size",
    y="Club",
    title = "AFL teams list sizes (population)",
    subtitle = "2023"
  )+
  geom_text(aes(x = label_y, label = n),colour = "white", fontface = "bold")+
  guides(fill = "none")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text = element_text(size = 11,
                                 colour = "black"),
        plot.title  = element_text(size = 14, face = "bold"),
        title = element_text(size = 12))





