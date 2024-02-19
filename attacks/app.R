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
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bear Attacks in North America"),
    
    #Select region
    selectInput("region", label = "Region 1", choices = state_counts$region, selected = state_counts$region),
    selectInput("region2", label = "Region 2", choices = state_counts$region, selected = state_counts$region),
    selectInput("region3", label = "Region 3", choices = state_counts$region, selected = state_counts$region),
    
    #output text
    textOutput("selected_var"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$selected_var <- renderText({
      paste("You have selected", input$region, ",", input$region2, "and", input$region3)
    })
    
    output$plot <- renderPlot({
      filtered_data <- state_counts %>%
        filter(region %in% input$region)

      ggplot(state_counts, aes(x = region, y = Number_of_attacks)) + 
        geom_bar(stat = "identity") + labs(title = "Numer of fatal attacks in each states")
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
