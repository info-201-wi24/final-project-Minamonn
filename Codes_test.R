# Libraries
library(rsconnect)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(maps)
library(mapproj)
library(scales)
library(plotly)


# longitudinal locations for world
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
  select(Name, gender, Year, Latitude, Longitude, Location.x, Type.of.bear, Hunter, Hikers) %>% 
  filter(!is.na(Latitude)) 

# rounds latitude and longitude 
Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(across(c('Latitude', 'Longitude'), round, 1))

# removes unwanted descriptions of region and keeps only region name
Coordinate_and_person_type <- Coordinate_and_person_type %>% mutate(region = str_replace(Location.x, ".*?,\\s*", ""))

# creates a data frame with frequency of bear attacks per region
region_counts <- Coordinate_and_person_type %>% group_by(region) %>% summarise(Number_of_attacks = n())

# scatter plot of latitude vs longitude
ggplot(Coordinate_and_person_type) + geom_point(aes(x = Latitude, y = Longitude))

# bar graph of frequency of bar attacks per region (title needs to be updated)
ggplot(region_counts, aes(x = region, y = Number_of_attacks)) + 
  geom_bar(stat = "identity") + labs(title = "Numer of fatal attacks in each Region")

# creates a scatter plot of the latitude and longitude of fatal bear attacks on a map
# note while this works like intended more data wrangling is required to make it look better
ggplot(data = world_shape) +
  
  geom_polygon(aes(x = long, y = lat, group = group)) +
  
  
  geom_point(data = Coordinate_and_person_type, aes(x = Longitude, y = Latitude, color = "deaths" )) +
  
  coord_map()

##<<<<<<< HEAD
##write.csv(Coordinate_and_person_type, file = "Unified and clean CSV dataset File.csv")
#=======
# number of deaths per region per year data frame
region_female_male_count <- Coordinate_and_person_type %>%
  group_by(region) %>%
  filter(gender == "female") %>% 
  reframe(Female_deaths = n(), Coordinate_and_person_type %>%
              group_by(Year) %>%
              filter(gender == "male") %>% summarise(Male_deaths = n())
  )

# bar graph of deaths per region per year (males vs female deaths)
female_male_deaths_by_region <- ggplot(data = region_female_male_count) +
  geom_col(mapping = aes(Year, Female_deaths, fill = "Female")) +
  geom_col(mapping = aes(Year, Male_deaths, fill = "Male")) +
  labs(title = "Female and Males Bear Deaths by Year", x = "Year", y = "Number of Deaths", fill = "Gender") +
  scale_x_continuous(breaks = seq(1901, 2018, 39)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  facet_wrap(~region)

ggplotly(female_male_deaths_by_region)
<<<<<<< HEAD
#>>>>>>> fa8e931b8fdaa99ee0550f45923dd2947527fbef
=======


#number of female deaths
female_deaths <- Coordinate_and_person_type %>%
  filter(gender == "female") %>%
  nrow()
#number of male deaths
male_deaths <- Coordinate_and_person_type %>%
  filter(gender == "male") %>%
  nrow()

#total deaths
total_deaths <- Coordinate_and_person_type %>%
  nrow()

#percent of male deaths
male_perc <- round(male_deaths / total_deaths * 100)

#percent of male deaths
female_perc <- round(female_deaths / total_deaths * 100)
>>>>>>> 02864a7226ba3745aa13ba761cfbd3455a60202f
