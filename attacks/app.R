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

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage(

    # Application title
    "Bear Attacks in North America",

    
    # Sidebar with a slider input for number of bins 
    tabPanel("Attacks Across Regions",
        sidebarPanel(
            selectInput("region", "Select Region:", state_counts$region, selected = state_counts$region[1], multiple = TRUE)),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("barChart")
          
        )
    ),
    tabPanel("North America Trends", "Not yet")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$selected_var <- renderText({
      paste("You have selected", input$region_name)
    })
    
    output$barChart <- renderPlot({
      filtered_data <- state_counts %>% filter(region %in% input$region)
      
      ggplot(filtered_data, aes(x = region, y = Number_of_attacks, fill = region)) + 
        geom_bar(stat = "identity") + labs(title = "Numer of Fatal Attacks in Each States from 1901 - 2022", x = "Selected Region", y = "Number of Attacks") + scale_fill_brewer(palette = "Paste2") + scale_y_continuous(breaks = seq(0,40))
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
