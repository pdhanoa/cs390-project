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
data <- data %>% filter(`Min Age` < 25)
counties <- read_sf("../../data/comm_areas.shp")
county_list <- unique(counties$community)

type_data <- data %>%
  group_by(`Category Name`) %>%
  summarise(Percent = n()/nrow(.) * 100)

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

keyword_list = list('AUBURN GRESHAM', 'AUSTIN', 'BELMONT CRAGIN', 'NEW CITY', 'CHICAGO LAWN', 'GAGE PARK','ENGLEWOOD', 'WEST ENGLEWOOD',
                    'GREATER GRAND CROSSING', 'EAST GARFIELD PARK', 'WEST GARFIELD PARK', 'PULLMAN', 'ROSELAND','WEST PULLMAN',
                    'SOUTH LAWNDALE', 'NORTH LAWNDALE', 'HUMBOLDT PARK') 
# merge_data$keywords <- merge_data$community %in% keyword_list

merge_data$key_neighborhood <- ifelse(merge_data$community %in% keyword_list, "focus", "non-focus")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MCMF Analysis Dashboard"),
    tabsetPanel(
      tabPanel("panel 1",
        sidebarLayout(
          sidebarPanel(
            selectInput("feature_dropdown",
                        "Please select a feature to view on the map:",
                        choices = c("freq_free", "freq_online", "freq_transport")),
            checkboxInput("key_focus",
                          label = "Check to view the focus neighborhoods",
                          value = FALSE)
            # plotOutput("map")
          ),
          
          
          # Show a plot of the generated distribution
          mainPanel(
                plotOutput("map")
              )
            )
          ),
      tabPanel("panel 2",
            sidebarLayout(
              sidebarPanel(),
               mainPanel(
               plotOutput("pie_chart")
               )
            )
          ),
      tabPanel("panel 3")
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  

    fill_var <- reactive({ input$feature_dropdown })
    output$map <- renderPlot({
        # generate bins based on input$bins from ui.R
      p = merge_data %>%
        ggplot(aes_string(fill=fill_var())) + 
          geom_sf(linewidth = 0.5)
          
      
      if(input$key_focus){
        p = p + geom_sf(fill = "gray", linewidth = 0.5, data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
      }
      
      p
    })
    
    output$pie_chart <- renderPlot({
      ggplot(data = type_data)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
