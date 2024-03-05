
library(dplyr)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(stringr)
library(scales)
library(plotly)
library(maps)
library(mapproj)
library(tidyverse)

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

region_female_male_count <- Coordinate_and_person_type %>%
  group_by(region) %>%
  filter(gender == "female") %>% 
  reframe(Female_deaths = n(), Coordinate_and_person_type %>%
            group_by(Year) %>%
            filter(gender == "male") %>% summarise(Male_deaths = n())
  )
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
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  
                  # Application title
                  "Bear Attacks in North America",
                  
                  tabPanel("Home", 
                           h2 ("Weelcome to statistical Data exploring Bear attacks"),
                           p("Through thorough analysis of historical bear attack data, we aim to pinpoint the locations where attacks are most likely, informing the public of potential dangers. Identifying the bear species with the highest
             fatality rates allows for targeted warnings, mitigating the risk of future incidents. This initiative underscores a pressing need to balance human safety with wildlife conservation, particularly concerning bear encounters that can result in severe consequences for individuals and communities alike. The narratives of those affected by bear attacks underscore the urgency of our mission. By contributing to this cause, we seek not only to prevent future
             tragedies but also to honor the memories of those impacted. 
 
We envision this project as a means to humanize bear attack statistics, fostering empathy and understanding of the risks involved. We aim to educate on the necessity of caution in bear habitats and inspire action toward safer human-bear coexistence. Our project is a step towards a future where humans and bears coexist more safely, emphasizing empowerment and preparedness for those navigating bear territories.", style = "font-size: 18px"),
                           mainPanel(
                             img(src = 'brownbear.jpg', height = 650, width = "auto", align = "left")
                           ),
                           h2("This investigation is conducted with key questions to help us obtain information and avoid fatal events:"),
                           p(strong("· Which region has the most amount of bear attacks?", style = "font-size: 21px")),
                           p(strong("· Are there specific geographic location that they attack in?", style = "font-size: 21px")),
                           p(strong("· Who are the usual targets of these attacks?", style = "font-size: 21px"))
                           
                  ),
                  
                  
                  # Panel 2
                  tabPanel("Attacks Across Regions", h2("Bear attacks in Each Regions of North America"), 
                           sidebarPanel(img(src = 'bear.png', height = 100, width = "auto", alight = "left"),
                                        selectInput("region", "Select Multiple Regions:", region_counts$region, selected = region_counts$region[c(1,24,4)], multiple = TRUE)),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotlyOutput("barChart")
                             
                           ),
                           h2("Summary of Attacks"), 
                           p("This chart intends to answer the question of which regions are most likely to have bear attacks since the 1900s. After evaluating the data shown in the chart, a correlation was found between geographical location and the incidence of bear attacks in North America. Northern locations, particularly Alaska and Alberta, have the greatest number rate of reported bear assaults. This pattern indicates that the risk of such interactions is higher in locations with extensive wilderness and dense bear populations. The presence of grizzly and polar bears, who are noted for their size and strength, may contribute to the increased frequency of assaults in these areas.", style = "font-size: 22px"),
                           
                           p("In contrast, records reveal that locations further south, such as Tennessee, West Virginia, and New Jersey, had less number of bear attacks. This could be due to factors such as smaller or distinct species of bear populations, fewer expansive wilderness areas, or more developed terrain, which reduce the frequency of human-bear encounters. Bears in these places are often smaller, with black bears being the most common, and may be less likely to interact with humans", style = "font-size: 22px"),
                           
                           p("This trend of bear attacks demonstrates the importance of focused wildlife management and public safety education. Areas with a higher assault rate may benefit from specific risk-mitigation techniques, such as bear-proofing measures, public awareness campaigns, and resident and tourist preparedness training. Understanding the interplay between bear behavior, habitat, and human activity is crucial for developing effective policies to protect both wildlife and people.", style = "font-size: 22px")),
                  
                  #Panel 3 about the Lat and Long of data
                  tabPanel("North America Trends", 
                           mainPanel(
                             plotlyOutput("scatterPlot",  height = "900px", width = "100%")
                             
                           ),
                           h2("Summary and Purpose"),
                           p("The dot map precisely plots the latitude and longitude of bear attacks across North America, highlighting a spatial pattern that is closely associated with areas of major wilderness and bear populations. The visualization clearly shows a denser concentration of attacks along specific latitudinal and longitudinal regions. Specifically, events occur more frequently at northern latitudes.", style = "font-size: 22px"),
                           p("This mapping of precise locations where bear attacks have occurred adds to the historical data collected from 1901 to 2022, demonstrating that areas such as Alaska, Alberta, British Columbia, and Montana are not only historically significant in bear attack incidences but also spatially different. Higher latitudes are associated with a higher number of bear attacks, which can be linked to the presence of larger bear species such as grizzlies and polar bears, as well as interactions with human populations who visit or live in these wilderness areas.
The goal of combining this thorough geographical study with over a century of data is to increase public awareness of the risks that exist in these high-latitude places. It aims to inform and maybe guide the development of preventive measures and safety practices for anyone who may find themselves in bear territory. The project's ability to pinpoint the exact latitudes and longitudes of bear encounters makes it a vital tool for wildlife management and public safety programs, allowing resources and education efforts to be focused in the most affected areas.
", style = "font-size: 22px")
                  ),
                  tabPanel("Victim Types & Findings",
                           h2("Statistics and Victim Reports"),
                           mainPanel(
                             plotOutput("facetChart",  height = 700, width = "auto")
                             
                           ),
                           p("Most deaths occur on the western part of North America, especially in the northwest of the continent.
                The regions with the highest frequency of bear attacks are located in Canadian and Alaskan regions (Alaska 27, Alberta 17, British Columbia 17, Montana 17).
                Females are most likely to die in Alaska, Alberta, British Columbia, and Montana.
                Males are most likely to die in Alaska, Alberta, British Columbia, Montana, and Wyoming.
                The lowest frequency of bear attacks is in the middle of North America, particularly in the United States (Arizona, Michigan, Minnesota, New Jersey, New Mexico, New York, Utah, Vermont, Washington, Yellowstone, all with only 1 death).
                Males are much more likely to get killed.
                10 out of 18 regions only have male deaths.
                About 72% of deaths are male deaths.
                About 28% of deaths are female deaths.", style = "font-size: 22px")
                           ),
                  tabPanel("Takeaways",
                           h2("Conclusion:"),
                           p("Throughout our analysis of data gathered on bear fatalities, a line can be drawn towards the complex dynamics of interactions between bears and humans. It shows the importance of informed decision-making and precautionary measures that must be taken in regions with high rates of fatalities. By acknowledging these patterns and addressing them through proactive measures, we can walk towards a more safe and environment free of bear-related fatalities.", style = "font-size: 22px"),
                           
                           p(strong("Region Patterns of Bear Attacks:"), "The data compiled highlights clear regional patterns related to bear attacks and fatalities. Through our analysis, we discovered that most deaths occur in the western part of North America, particularly in the northwest region of the continent. Alaska, Alberta, British Columbia, Montana, and Wyoming are hotspots for bear-related fatalities, which shows the prevalence of bear and human interaction in this area.", style = "font-size: 22px"),             
                           
                           p(strong("Gender Disparities in Fatalies:"), "Our data shows that there is a clear and notable gender disparity in bear-related fatalities. While it is shown that males and females are both at risk for encountering bear attacks, males are significantly more likely to die/encounter an attack. The data shows that out of the 18 regions recorded 10 of them recorded only male deaths, which emphasizes the vulnerability of males to such incidents.", style = "font-size: 22px"),
                           
                           p(strong("Frequency of Bear Attacks for Regions:"), "The frequency of bear attacks across regions varies significantly across them. In Canadian and Alaskan regions such as Alaska, Alberta, and British Columbia, they experience the highest frequency of bear attacks, which is evident by the number of reports in these areas. Conversely, in the middle regions of North America, more significantly in the United States, there are lower frequencies of bear attacks.", style = "font-size: 22px"),           
                           
                           p(strong("Implications for Safety Measures:"),  "These findings have significant implications for implementing safety measures and raising awareness in regard to bear attacks. Understanding how the distribution of bear attacks, demographic vulnerabilities, and the frequency of such patterns, can help to inform us where safety measures, should be targeted, in order to mitigate the risks and overall enhance the safety for both humans and bears.", style = "font-size: 22px"))
                )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$scatterPlot <- renderPlotly({
    ggplotly(ggplot(data = world_shape) +
      
      geom_polygon(aes(x = long, y = lat, group = group)) +
       
      
      geom_point(data = Coordinate_and_person_type, aes(x = Longitude, y = Latitude, color = "red", text = paste0(region, ", Latitude: ", Latitude, ", Longitude: ", Longitude))) +
      
      coord_fixed(ratio = 1.3 ) + xlim(-169,-50)+ ylim(5,83)+labs(title = "Latitude and Longitude of attacks", x = "Longitude", y = "Latitude", color = "Attack Locations") + theme_minimal(), tooltip = "text"
  
   ) })
  
  output$barChart <- renderPlotly({
    filtered_data <- region_counts %>% filter(region %in% input$region)
    
    ggplotly(ggplot(filtered_data, aes(x = region, y = Number_of_attacks, fill = region, text = paste(region, "has", Number_of_attacks, "attacks"))) + 
      geom_bar(stat = "identity") + labs(title = "Numer of Fatal Attacks in Each States from 1901 - 2022", x = "Selected Region", y = "Number of Attacks") + 
      theme(axis.title = element_text(size = 16), axis.text = element_text(size = 10))+ scale_fill_brewer(palette = "Set1") + scale_y_continuous(breaks = seq(0,40)), tooltip = "text"
  
   ) })
  
  output$facetChart <- renderPlot({
      ggplot(data = region_female_male_count) +
      geom_col(mapping = aes(Year, Female_deaths, fill = "Female")) +
      geom_col(mapping = aes(Year, Male_deaths, fill = "Male")) +
      labs(title = "Female and Males Bear Deaths by Year", x = "Year", y = "Number of Deaths", fill = "Gender") +
      scale_x_continuous(breaks = seq(1901, 2018, 39)) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      facet_wrap(~region)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
