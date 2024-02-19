library(rsconnect)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)

bear_coords_df <- read.csv("bear_attacks.csv")
NA_fatal_df <- read.csv("north_america_bear_killings.csv")

true_df <- left_join(NA_fatal_df, bear_coords_df, by = "Name")
Coordinate_and_person_type <- true_df %>% select(Name, Year, Latitude, Longitude, Location.x, Type.of.bear, Hunter, Hikers) %>% filter(!is.na(Latitude)) 

Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(across(c('Latitude', 'Longitude'), round, 1))
Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(region = str_replace(Location.x, ".*?,\\s*", ""))

state_counts <- Coordinate_and_person_type %>% group_by(region) %>% summarise(Number_of_attacks = n())

ggplot(Coordinate_and_person_type) + geom_point(aes(x = Latitude, y = Longitude))

ggplot(state_counts, aes(x = region, y = Number_of_attacks)) + geom_bar(stat = "identity") + labs(title = "Numer of fatal attacks in each states")
