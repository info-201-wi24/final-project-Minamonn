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
    
    #output text
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "Select Region:", state_counts$region, selected = state_counts$region[1], multiple = TRUE)),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("barChart")
        )
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
        geom_bar(stat = "identity") + labs(title = "Numer of fatal attacks in each states", x = "Selected Region", y = "Number of Attacks") + scale_fill_brewer(palette = "Paste1")
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
