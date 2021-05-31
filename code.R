## In Season analysis following closely to Learn
## Python with Fantasy Football using sql
library(RSQLite)
library(DBI)
library(dplyr)
library(nflfastR)
library(tidyverse)
# Create a database to access. I've never done this but I am trying it out just
#to see it for future reference.

read_pbp_rds <- function(year){
  readRDS(url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{year}.rds")))
}

all_pbp <- 2019:2020 %>% 
  purrr::map_dfr(read_pbp_rds)

all_pbp %>% 
  write_rds("data/2000-2019_pbp_raw.rds")

mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/pbp_db.sqlite")
mydb

DBI::dbWriteTable(mydb, "2000-2019_pbp_raw.rds",all_pbp)
library(dbplyr)
rm(all_pbp)
# Open a queryable connection with the database
pbp_db <- tbl(mydb, "2000-2019_pbp_raw.rds")
pass_df <- pbp_db %>% filter(pass_attempt==1) %>%
  select(receiver_id,receiver_player_name,posteam,air_yards,season) %>% 
  collect()
# check na's
pass_df %>%
  summarise_all(funs(sum(is.na(.))))
# remove id's and air yards with na's-----
# two point conversions and plays were players were not
# targeted
pass_df <- pass_df %>% drop_na(.)

top_air <-  pass_df %>% group_by(receiver_id,receiver_player_name,season) %>%
  summarise(air_yards_total = sum(air_yards,na.rm = T)) %>%group_by(season) %>% arrange(desc(air_yards_total)) %>%
  top_n(10) %>% ungroup()

top_air %>% inner_join(pass_df,by = "receiver_id") %>% filter(season.x==2019) %>% 
  ggplot(aes(x=air_yards,fill = receiver_player_name.x,colour = receiver_player_name.x))+
  geom_density(alpha=0.1)+labs(title=("2019 Top Ten Recievers by Total Air Yards"),fill="Reciever Name",colour="Reciever Name")
  

top_air %>% inner_join(pass_df,by = "receiver_id") %>% filter(season.x==2020) %>% 
  ggplot(aes(x=air_yards,fill = receiver_player_name.x,colour = receiver_player_name.x))+
  geom_density(alpha=0.1)+facet_wrap("season.x")+
  labs(title=("2020 Top Ten Recievers by Total Air Yards"),fill="Reciever Name",colour="Reciever Name")
# Target Shares-----
# shares per player per game
#note yet to merge these together
pbp_db %>%
  select(game_id, receiver_player_id,receiver_player_name,posteam,season,pass_attempt) %>% filter(season == 2020) %>%
  collect() %>% 
  group_by(game_id, receiver_player_id,receiver_player_name,posteam) %>%
  summarise(attempts = sum(pass_attempt,na.rm = T))  

pbp_db %>%
  select(game_id, receiver_player_id,receiver_player_name,posteam,season,pass_attempt) %>%
  filter(season == 2020) %>% 
  collect() %>% 
  drop_na(receiver_player_id) %>%
  group_by(game_id,posteam) %>%
  summarise(pass_attempts= sum(pass_attempt,na.rm = T)) 

