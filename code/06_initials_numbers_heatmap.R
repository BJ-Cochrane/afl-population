## Name action

library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(ggplot2)
library(gapminder)
library(gganimate)
library(gifski)
library(forcats)
library(readxl)
library(mosaic)
library(stringr)
library(janitor)

afltables <- fitzRoy::fetch_player_stats_afltables(season = 1800:2023)

number<-afltables %>%
  select(Jumper.No.,First.name,Surname)


player_numbers <- number %>%
  unique() %>%
  filter(Jumper.No. != 0) %>%
  mutate(jumper = str_remove_all(Jumper.No.," ↑")) %>%
  mutate(jumper = str_remove_all(jumper," ↓")) %>%
  select(-Jumper.No.) %>%
  mutate(jumper_length = str_length(jumper))%>%
  clean_names() %>%
  mutate(first_number = ifelse(jumper_length == 1, 0, substr(jumper,1,1)),
         second_number = ifelse(jumper_length == 1, substr(jumper,1,1), substr(jumper,2,2))) %>%
  select(-jumper_length) %>%
  unique()



numbers_count <- player_numbers %>%
  group_by(first_number,second_number) %>%
  # filter(season == 2022) %>%
  mutate(jumpercount = count(jumper))%>%
  ungroup()

my_breaks <- c(1,300,400,500,600)

ggplot(numbers_count, aes(x=second_number, y=fct_rev(first_number), fill = jumpercount))+
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1)+
  labs(
    x="Second digit",
    y="First digit",
    title = "AFL jumper numbers numbers 1897 - 2023",
    caption = "@BenCochraneR"
  )+
  scale_fill_gradientn(colors = hcl.colors(10,"YlOrRd", rev = T), name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)+
  theme_classic()+
  geom_text(aes(label = jumpercount), color = "Black", size = 4)+
  scale_x_discrete(position = "top")+
  theme(legend.position = "none")




name<-afltables %>%
  select(First.name,Surname)

player_list <- name %>%
  unique()

p_fn <- substring(player_list$First.name,1,1)
p_sn <- substring(player_list$Surname,1,1)
p_id <- player_list$ID

p_initials <- data.frame(p_fn,p_sn)%>%
  mutate_each(funs(toupper))


initials_count <- p_initials %>%
  group_by(p_fn,p_sn)%>%
  mutate(surcount = count(p_sn))%>%
  ungroup()


initials_count%>%
  unique()%>%
  mutate(median = median(surcount))%>%
  arrange(desc(surcount))


initials_count%>%
  reorder(p_fn = fct_reorder(p_fn))

my_breaks <- c(1,10,25,100,250)

ggplot(initials_count, aes(x=p_sn, y=fct_rev(p_fn), fill = surcount))+
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1)+
  labs(
    x="Surname initial",
    y="First name initial",
    title = "AFL players initials 1897 - 2022",
    caption = "*Initial from first surname taken from hyphenated names

    @BenCochraneR"
  )+
  scale_fill_gradientn(colors = hcl.colors(100,"YlOrRd", rev = T), name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)+
  theme_classic()+
  geom_text(aes(label = surcount), color = "Black", size = 4)+
  scale_x_discrete(position = "top")+
  theme(legend.position = "none")


ggsave('output/afl-initials.png', dp = 1000)

######### AFLW Version #############################################
library(stringr)


AFLW <- fitzRoy::get_aflw_player_stats()

AFLW <- AFLW %>%
  separate(player_name,c("First.name", "Surname"), ", ")


name<-AFLW %>%
  select(player_id,First.name,Surname)


player_list <- name %>%
  unique()

p_fn <- substring(player_list$First.name,1,1)
p_sn <- substring(player_list$Surname,1,1)
p_id <- player_list$ID

p_initials <- data.frame(p_fn,p_sn)%>%
  mutate_each(funs(toupper))


initials_count <- p_initials %>%
  group_by(p_fn,p_sn)%>%
  mutate(surcount = count(p_sn))%>%
  ungroup()


initials_count%>%
  unique()%>%
  mutate(median = median(surcount))%>%
  arrange(desc(surcount))


initials_count%>%
  reorder(p_fn = fct_reorder(p_fn))

initials_count <- initials_count%>%
  add_row(p_fn = "F", p_sn = "Q", surcount = 0)%>%
  add_row(p_fn = "U", p_sn = "Z", surcount = 0)%>%
  add_row(p_fn = "X", p_sn = "Z", surcount = 0)


my_breaks <- c(1,10,25,100,250)

ggplot(initials_count, aes(x=p_sn, y=fct_rev(p_fn), fill = surcount))+
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1)+
  labs(
    x="Surname initial",
    y="First name initial",
    title = "AFLW players initials since 2017",
    caption = "*Initial from first surname taken from hyphenated names"
  )+
  scale_fill_gradientn(colors = hcl.colors(100,"YlOrRd", rev = T), name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)+
  theme_classic()+
  geom_text(aes(label = surcount), color = "Black", size = 4)+
  scale_x_discrete(position = "top", drop = FALSE)+
  scale_y_discrete(drop = FALSE)+
  theme(legend.position = "none")

