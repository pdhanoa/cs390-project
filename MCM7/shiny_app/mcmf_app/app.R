#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library("RColorBrewer")

data <- read_delim("../../data/convert_MCMF_ALL_TIME_DATA.csv")

data <- data %>% rename("program_price" = "Program Price")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MCMF Analysis Dashboard"),

    mainPanel(
      plotOutput("distPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data, aes(x=program_price)) + geom_bar()

      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
