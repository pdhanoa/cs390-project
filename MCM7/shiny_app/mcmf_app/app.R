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
library(sf)
library(ggthemes)
library("RColorBrewer")

data <- read_delim("../../data/convert_MCMF_ALL_TIME_DATA.csv")
data <- data %>% rename("program_price" = "Program Price",
                        "geographic_cname" = "Geographic Cluster Name")
counties <- read_sf("../../data/comm_areas.shp")
county_list <- unique(counties$community)

grouped_data <- data %>% group_by(geographic_cname)  %>% 
  filter(program_price == "Free") %>%
  summarise(num_free_programs=n(),
            .groups = 'drop')

merge_data <- counties %>% 
  # return all rows for candidate Biden and all columns from x and y
  left_join(
    grouped_data,
    by = c("community" ="geographic_cname")
  )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MCMF Analysis Dashboard"),

    mainPanel(
      plotOutput("map")
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderPlot({
        # generate bins based on input$bins from ui.R
      ggplot(data=merge_data ) + 
        geom_sf(aes(fill=num_free_programs))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
