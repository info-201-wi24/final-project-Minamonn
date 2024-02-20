
library(dplyr)
library(ggplot2)
library(shinythemes)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage(

    # Application title
    "Bear Attacks in North America",

    tabPanel("Home", 
             h2 ("Welcome to statistical Data exploring Bear attacks"),
             p("Through thorough analysis of historical bear attack data, we aim to pinpoint the locations where attacks are most likely, informing the public of potential dangers. Identifying the bear species with the highest
             fatality rates allows for targeted warnings, mitigating the risk of future incidents. This initiative underscores a pressing need to balance human safety with wildlife conservation, particularly concerning bear encounters that can result in severe consequences for individuals and communities alike. The narratives of those affected by bear attacks underscore the urgency of our mission. By contributing to this cause, we seek not only to prevent future
             tragedies but also to honor the memories of those impacted. 
 
We envision this project as a means to humanize bear attack statistics, fostering empathy and understanding of the risks involved. We aim to educate on the necessity of caution in bear habitats and inspire action toward safer human-bear coexistence. Our project is a step towards a future where humans and bears coexist more safely, emphasizing empowerment and preparedness for those navigating bear territories.", style = "font-size: 18px"),
             mainPanel(
               img(src = 'brownbear.jpg', height = 650, width = "auto", align = "left")
             ),
             h2("This investigation is conducted with key questions to help us obtain information and avoid fatal events:"),
             p(strong("· Which region has the most amount of bear attacks?", style = "font-size: 22px")),
             p(strong("· Are there specific geographic location that they attack in?", style = "font-size: 22px")),
             p(strong("· Who are the usual targets of these attacks?", style = "font-size: 22px")),
             
             ),
            
             
    # Panel 2
    tabPanel("Attacks Across Regions", h2("Bear attacks in Each Regions of North America"), 
        sidebarPanel(img(src = 'bear.png', height = 100, width = "auto", alight = "left"),
            selectInput("region", "Select Multiple Regions:", region_counts$region, selected = region_counts$region[c(1,24,4)], multiple = TRUE)),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("barChart")
          
        ),
        h2("Summary of Attacks"), 
        p("This chart intends to answer the question of which regions are most likely to have bear attacks since the 1900s. After evaluating the data shown in the chart, a correlation was found between geographical location and the incidence of bear attacks in North America. Northern locations, particularly Alaska and Alberta, have the greatest number rate of reported bear assaults. This pattern indicates that the risk of such interactions is higher in locations with extensive wilderness and dense bear populations. The presence of grizzly and polar bears, who are noted for their size and strength, may contribute to the increased frequency of assaults in these areas.", style = "font-size: 22px"),

p("In contrast, records reveal that locations further south, such as Tennessee, West Virginia, and New Jersey, had less number of bear attacks. This could be due to factors such as smaller or distinct species of bear populations, fewer expansive wilderness areas, or more developed terrain, which reduce the frequency of human-bear encounters. Bears in these places are often smaller, with black bears being the most common, and may be less likely to interact with humans", style = "font-size: 22px"),

p("This trend of bear attacks demonstrates the importance of focused wildlife management and public safety education. Areas with a higher assault rate may benefit from specific risk-mitigation techniques, such as bear-proofing measures, public awareness campaigns, and resident and tourist preparedness training. Understanding the interplay between bear behavior, habitat, and human activity is crucial for developing effective policies to protect both wildlife and people.", style = "font-size: 22px")),
    
    #Panel 3 about the Lat and Long of data
    tabPanel("North America Trends", 
             mainPanel(
                plotOutput("scatterPlot",  height = 900, width = "auto")
                 
               ),
             h2("Summary and Purpose"),
             p("The dot map precisely plots the latitude and longitude of bear attacks across North America, highlighting a spatial pattern that is closely associated with areas of major wilderness and bear populations. The visualization clearly shows a denser concentration of attacks along specific latitudinal and longitudinal regions. Specifically, events occur more frequently at northern latitudes.", style = "font-size: 22px"),
p("This mapping of precise locations where bear attacks have occurred adds to the historical data collected from 1901 to 2022, demonstrating that areas such as Alaska, Alberta, British Columbia, and Montana are not only historically significant in bear attack incidences but also spatially different. Higher latitudes are associated with a higher number of bear attacks, which can be linked to the presence of larger bear species such as grizzlies and polar bears, as well as interactions with human populations who visit or live in these wilderness areas.
The goal of combining this thorough geographical study with over a century of data is to increase public awareness of the risks that exist in these high-latitude places. It aims to inform and maybe guide the development of preventive measures and safety practices for anyone who may find themselves in bear territory. The project's ability to pinpoint the exact latitudes and longitudes of bear encounters makes it a vital tool for wildlife management and public safety programs, allowing resources and education efforts to be focused in the most affected areas.
", style = "font-size: 22px"),
             ),
    tabPanel("Victim Types"),
    tabPanel("Findings and Takeaways")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
  
  output$scatterPlot <- renderPlot({
      ggplot(data = world_shape) +
        
        geom_polygon(aes(x = long, y = lat, group = group)) +
        
        
        geom_point(data = Coordinate_and_person_type, aes(x = Longitude, y = Latitude, color = "red")) +
        
        coord_fixed(ratio = 1.3 ) + xlim(-169,-50)+ ylim(5,83)+labs(title = "Latitude and Longitude of attacks", x = "Longitude", y = "Latitude") + theme_minimal()
    })
    
    output$barChart <- renderPlot({
      filtered_data <- region_counts %>% filter(region %in% input$region)
      
      ggplot(filtered_data, aes(x = region, y = Number_of_attacks, fill = region)) + 
        geom_bar(stat = "identity") + labs(title = "Numer of Fatal Attacks in Each States from 1901 - 2022", x = "Selected Region", y = "Number of Attacks") + 
        theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))+ scale_fill_brewer(palette = "Set1") + scale_y_continuous(breaks = seq(0,40))
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

