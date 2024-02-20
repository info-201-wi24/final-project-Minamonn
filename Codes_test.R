# Libraries
library(rsconnect)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(maps)
library(mapproj)
library(shiny)
library(ggplot2)

# longitudinal locations for world
# note the only other option is to use map data for just entire world since our coordinates
# include more then just the united states
world_shape <- map_data("world")
# Longitudinal locations for bear attacks and descriptions
bear_coords_df <- read.csv("bear_attacks.csv")
# NA Region locations for bear attacks and descriptions
NA_fatal_df <- read.csv("north_america_bear_killings.csv")

# combination of both bear data frames (raw data)
true_df <- left_join(NA_fatal_df, bear_coords_df, by = "Name")

# cleaned libraries includes only includes basic information
# excludes data without longitudinal coordinates
Coordinate_and_person_type <- true_df %>% 
  select(Name, Year, Latitude, Longitude, Location.x, Type.of.bear, Hunter, Hikers) %>% 
  filter(!is.na(Latitude)) 

# rounds latitude and longitude 
Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(across(c('Latitude', 'Longitude'), round, 1))

# removes unwanted descriptions of region and keeps only region name
Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(region = str_replace(Location.x, ".*?,\\s*", ""))

# creates a data frame with frequency of bear attacks per region
# note for future (data frame is not states but regions in NA)
state_counts <- Coordinate_and_person_type %>% group_by(region) %>% summarize(Number_of_attacks = n())

# scatter plot of latitude vs longitude
ggplot(Coordinate_and_person_type) + geom_point(aes(x = Latitude, y = Longitude))

# bar graph of frequency of bar attacks per region (title needs to be updated)
ggplot(state_counts, aes(x = region, y = Number_of_attacks)) + 
  geom_bar(stat = "identity") + labs(title = "Numer of fatal attacks in each states")

# creates a scatter plot of the latitude and longitude of fatal bear attacks on a map
# note while this works like intended more data wrangling is required to make it look better
ggplot(data = world_shape) +
  
  geom_polygon(aes(x = long, y = lat, group = group)) +
  
  
  geom_point(data = Coordinate_and_person_type, aes(x = Longitude, y = Latitude, color = "red")) +
  
  coord_map()

write.csv(Coordinate_and_person_type, file = "Unified and clean CSV dataset File.csv")
