#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
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
        sidebarPanel(
            selectInput("region", "Select Region:", state_counts$region, selected = state_counts$region[1], multiple = TRUE)),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("barChart")
          
        ),
        h2("Summary of Attacks"), 
        p("This chart intends to answer the question of which regions are most prominent to have bear attacks since the 1900s. After analyzing the number of bear attacks in each region, it is clear that there is a pattern. Regions in the northern part 
                                    of North America is more prominant to fatal bear attacks. Example of this is shown by
                                    regions such as Alaska or Alberta where the most common occurances of attacks take place.", style = "font-size: 18px")),
    
    #Panel 3 about the Lat and Long of data
    tabPanel("North America Trends", 
             mainPanel(
                plotOutput("scatterPlot")
                 
               )
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
      filtered_data <- state_counts %>% filter(region %in% input$region)
      
      ggplot(filtered_data, aes(x = region, y = Number_of_attacks, fill = region)) + 
        geom_bar(stat = "identity") + labs(title = "Numer of Fatal Attacks in Each States from 1901 - 2022", x = "Selected Region", y = "Number of Attacks") + scale_fill_brewer(palette = "Paste1") + scale_y_continuous(breaks = seq(0,40))
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
