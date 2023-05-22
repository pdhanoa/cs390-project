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
                        "geographic_cname" = "Geographic Cluster Name",
                        "modality" = "Meeting Type",
                        "provides_transport" = "Program Provides Transportation")
data <- data %>% mutate(geographic_cname = recode(geographic_cname, 'Back of the Yards' = 'NEW CITY', 'Little Village' = 'SOUTH LAWNDALE'))
counties <- read_sf("../../data/comm_areas.shp")
county_list <- unique(counties$community)

perc_free <- data %>%
  group_by(geographic_cname, program_price) %>%
  summarise(num_f = n()) %>%
  mutate(freq_free = num_f/sum(num_f)) %>%
  filter(program_price == 'Free')

perc_online <- data %>% 
  group_by(geographic_cname, modality) %>%
  summarise(num_o = n()) %>%
  mutate(freq_online = 1-num_o/sum(num_o)) %>%
  filter(modality == 'face_to_face')

perc_transport <- data %>% 
  group_by(geographic_cname, provides_transport, .drop=FALSE) %>%
  summarise(num_t = n()) %>%
  mutate(freq_transport = 1-num_t /sum(num_t)) %>%
  filter(provides_transport == 'NO') 

perc_data = list(perc_free, perc_online, perc_transport)
perc_data <- perc_data %>% reduce(inner_join, by='geographic_cname')
perc_data <- subset(perc_data, select = -c(num_f, num_o, num_t, program_price, modality, provides_transport))


merge_data <- counties %>%
  # return all rows for candidate Biden and all columns from x and y
  left_join(
    perc_data,
    by = c("community" ="geographic_cname")
  )




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MCMF Analysis Dashboard"),
    # selectInput("Here's some stuff", "Whoa:",
    #             c("Cylinders" = "cyl")),
    # mainPanel(
    #   plotOutput("map")
    # )
    sidebarLayout(
      sidebarPanel(
        selectInput("feature_dropdown",
                    "Please select a feature to view on the map:",
                    choices = c("freq_free", "freq_online", "freq_transport")),
        plotOutput("map")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # plotOutput("map")
      )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    fill_var <- reactive({ input$feature_dropdown })
    output$map <- renderPlot({
        # generate bins based on input$bins from ui.R
      merge_data %>%
        ggplot(aes_string(fill=fill_var())) + 
          geom_sf()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
