#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shiny.fluent)
library(ggplot2)
library(scales)
library(tidyverse)
library(dplyr)
library(sf)
library(ggthemes)
library("RColorBrewer")

data <- read_delim("data/convert_MCMF_ALL_TIME_DATA.csv")
data <- data %>% rename("program_price" = "Program Price",
                        "geographic_cname" = "Geographic Cluster Name",
                        "modality" = "Meeting Type",
                        "provides_transport" = "Program Provides Transportation")
neigh_data <- read_delim(file = "data/neighborhoods.csv")
neigh_data$Name <- toupper(neigh_data$Name)
neigh_data <- neigh_data %>% rename("median_income" = " Median_Income ")
neigh_data$median_income <- gsub("\\$", "", 
                                     gsub(",", "", neigh_data$median_income))
neigh_data$median_income <- trimws(neigh_data$median_income)

neigh_data$median_income <- as.double(neigh_data$median_income)

neigh_data$Internet <- as.double(gsub("%$", "", neigh_data$Internet))

neigh_data$Vehicle <- as.double(gsub("%$", "", neigh_data$Vehicle))

counties <- read_sf("data/comm_areas.shp")

neigh_geodata <- counties %>%
  # return all rows for candidate Biden and all columns from x and y
  left_join(
    neigh_data,
    by = c("community" ="Name")
  )

data <- data %>% mutate(geographic_cname = recode(geographic_cname, 'Back of the Yards' = 'NEW CITY', 'Little Village' = 'SOUTH LAWNDALE'))
data <- data %>% filter(`Min Age` < 25)
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
neigh_geodata$key_neighborhood <- ifelse(merge_data$community %in% keyword_list, "focus", "non-focus")

merge_data <- merge(merge_data, neigh_data, by.x = "community", by.y = "Name")

programs_per_youth <- data %>% 
  group_by(geographic_cname) %>%
  summarise(num = n())

merge_data <- merge(merge_data, programs_per_youth, by.x = "community", by.y = "geographic_cname")

colnames(merge_data)[14] <- "num_programs"

merge_data$pro_per_youth <- merge_data$num_programs / merge_data$Youth

merge_data$Youth_Perc <- as.double(sapply(str_split(merge_data$Youth_Perc, "%"), function(x) x[[1]])) / 100
merge_data$Computing <- as.double(sapply(str_split(merge_data$Computing, "%"), function(x) x[[1]])) / 100

data$Age_Range <- data$`Max Age` - data$`Min Age`
data$Duration <- as.Date(data$`End Date`, format = "%m/%d/%y") - as.Date(data$`Start Date`, format = "%m/%d/%y")

age_duration <- data %>% 
  group_by(geographic_cname) %>%
  summarise(avg_age = mean(Age_Range),
            avg_dur = mean(Duration))

online_activities <- data %>% filter(data$modality == 'online') %>%
  group_by(`Category Name`) %>%
  summarise(count = n())

merge_data <- merge(merge_data, age_duration, by.x = "community", by.y = "geographic_cname")
merge_data$avg_dur <- as.double(merge_data$avg_dur)

test <- data %>%
filter(`Min Age` <= 20) %>%
filter(`Max Age` > 50)




# Define UI for application that draws a histogram
ui <- dashboardPage(
  ## skin = c("blue"),
  dashboardHeader(title = "My CHI. My Future"),
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem("About", icon = icon("list"), tabName = "about"),
      menuItem("Affordability", icon = icon("money-bill"), tabName = "aff"),
      menuItem("Transportability", icon = icon("car"), tabName = "trans"),
      menuItem("Modality", icon = icon("wifi"), tabName = "mod"),
      menuItem("Accessibility", icon = icon("universal-access"), tabName = "acc"),
      menuItem("Diversity of Programs", icon = icon("handshake"), tabName = "dp"),
      menuItem('Age Range & Duration', icon = icon("business-time"), tabName = "ad")
    )),
  dashboardBody(
    tabItems(
      tabItem("about",
              fluidRow(
                  mainPanel(
                    box(title = "The Neighborhoods in Focus",
                        tags$img(src = "focus_neighborhoods.png",
                                 width = "100%", height = "auto"),
                        solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 6
                        ),
                    box(title = "Our Approach to Assess Equity",
                        solidHeader = TRUE, collapsible = TRUE, status = "warning", width = 6,
                        list(
                          tags$ul(
                            tags$li("First, we broke down ", tags$b("equity"), "into 5 areas"),
                            tags$ul(
                              tags$li("Affordability"),
                              tags$li('Transportability'),
                              tags$li('Modality'),
                              tags$li('Accessibility'),
                              tags$li('Program Types')
                            ),
                            tags$li("Then, we found proxy metrics from the dataset for these respective areas across the 77 neighborhoods of Chicago"),
                            tags$ul(
                              tags$li("Percentage of", tags$b("Free Programs"), "and Median", tags$b("Household Income")),
                              tags$li("Percentage of Programs that Offer", tags$b("Transportation"),"and Percentage of Households", tags$b("without Vehicle")),
                              tags$li("Percentage of Households without", tags$b("Internet or Computing Device")),
                              tags$li(tags$b("Programs per Youth and Youth Density")),
                              tags$li(tags$b("Diversity"), "of Programs", tags$b("Age Range & Duration"))
                            ),
                            tags$li("Lastly, we effectively visualized these characteristics across different neighborhoods (especially those in focus) to capture", tags$b("discrepancies and inequities"), "across Chicago"),
                          )
                        )
                        ),
                    box(title = "Our Data",
                        solidHeader = TRUE, collapsible = TRUE, status = "success",
                        tags$a(href = "https://data.cityofchicago.org/Events/My-CHI-My-Future-Programs/w22p-bfyb/data", "MCMF (My CHI. My Future) Dataset"), br(),
                        tags$a(href = "https://www.cmap.illinois.gov/documents/10180/126764/_Combined_AllCCAs.pdf/", "CMAP (Chicago Metropolitan Agency for Planning) Dataset")
                    )
                )
              )
            ),
      tabItem("aff",
              box(plotOutput("aff_map"), title = "Percentage of Free Programs",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(plotOutput("aff_companion_map"), title = "Median Household Income",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(checkboxInput("key_focus_aff",
                              label = "Check to view the focus neighborhoods",
                              value = FALSE), width = 3)),
      tabItem("trans",
              box(plotOutput("trans_map"), title = "Percentage of Programs that Provide Transporation",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(plotOutput("trans_companion_map"), title = "Percentage of Household Without Vehicle",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(checkboxInput("key_focus_trans",
                              label = "Check to view the focus neighborhoods",
                              value = FALSE), width = 3)),
      tabItem("mod",
                fluidRow(
                  box(width =6, plotOutput("mod_map"), title = "Percentage of Households Without Internet Access",
                      solidHeader = TRUE, collapsible = TRUE),
                  box(width=6, plotOutput("mod_companion_map"), title = "Percentage of Household Without Computing Device",
                      solidHeader = TRUE, collapsible = TRUE),
                  box(checkboxInput("key_focus_mod",
                                    label = "Check to view the focus neighborhoods",
                                    value = FALSE), width = 3)
                ),
              fluidRow(
                box(width=5, plotOutput("online_pie_chart")),
                box(width=5, plotOutput("online_activities_pie_chart"))
              
              )
            ),
      tabItem("acc",
              box(plotOutput("acc_map"), title = "Programs Per Youth",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(plotOutput("acc_companion_map"), title = "Percentage of Youth",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(
                checkboxInput("key_focus_acc",
                              label = "Check to view the focus neighborhoods",
                              value = FALSE), width = 3)),
      tabItem("dp",
              box(selectInput("area_dropdown",
                            "Please select a neighborhood:",
                            choices = append(c("ALL NEIGHBORHOODS"), merge_data$community)
                ),
                checkboxInput("age_filter_check",
                              label = "Check this to filter by age",
                              value = FALSE), width = 3),
              box(
                sliderInput("chosen_age_slider", "Age:",
                            min = 0, max = 30,
                            value = 0)
                ),
              box(plotOutput("pie_chart"), width=8)),
      tabItem("ad",
              box(plotOutput("ad_map"), title = "Average Age Range of Programs",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(plotOutput("ad_companion_map"), title = "Average Duration of Programs",
                  width = 6, solidHeader = TRUE, collapsible = TRUE),
              box(
                checkboxInput("key_focus_ad",
                              label = "Check to view the focus neighborhoods",
                              value = FALSE), width = 3, height = "50px")
              )
      )
    ),
  tags$head(
    tags$style(HTML(".logo {background-color: #21b3ed !important;}
                      .navbar {background-color: #21b3ed !important;}
                     .skin_blue .main-sidebar .sidebar .sidebar-menu .active a {background-color: black;}
                     .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {background-color : #d90023;}"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # fill_var <- reactive({ input$feature_dropdown })
  # switch_var <- reactive({
  #   switch(input$feature_dropdown, 
  #          "freq_free" = "median_income",
  #          "freq_online" = "Internet",
  #          "freq_transport" = "Vehicle")}
  # )
  
  neighborhood_initial <- reactive({
    data %>%
      filter( if (input$age_filter_check) (`Min Age` <= input$chosen_age_slider) else TRUE) %>%
      filter ( if (input$age_filter_check) (`Max Age` > input$chosen_age_slider) else TRUE )
    
  })
  
  neighborhood_select <- reactive({
    data2 <- neighborhood_initial() %>%
      group_by(`Category Name`) %>%
      summarise(count = n())
    data2 %>% filter(count %in% tail(sort(data2$count),5))
  })
  
  
  
  chosen_neighborhood <- reactive({
    input$area_dropdown
  })
  
  chosen_age <- reactive({
    ifelse(input$age_filter_check, paste("Age", input$chosen_age_slider), "All Ages")
   
  })
  
  output$aff_map <- renderPlot({
    p1 = merge_data %>% 
      ggplot(aes(fill = freq_free)) + 
      geom_sf() + 
      labs(fill = NULL) + 
      theme_map() +
      scale_fill_continuous(labels = percent)
    
    if (input$key_focus_aff){
      p1 = p1 + geom_sf(fill = "gray", 
                                data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }
    
    p1
  })
  
  
  output$aff_companion_map <- renderPlot({
    # generate bins based on input$bins from ui.R
    comp_p1 = merge_data %>%
      ggplot(aes(fill=median_income)) + 
      geom_sf() +
      labs(fill = NULL) + 
      theme_map()
    
    if (input$key_focus_aff) {
      comp_p1 = comp_p1 + geom_sf(fill = "gray", 
                                  data = neigh_geodata %>% filter(neigh_geodata$key_neighborhood == "non-focus"))
    }

    comp_p1
  })
  
  output$trans_map <- renderPlot({
    p2 = merge_data %>% 
      ggplot(aes(fill = freq_transport)) + 
      geom_sf() + 
      labs(fill = NULL) + 
      theme_map() + 
      scale_fill_continuous(labels = percent)
    
    if (input$key_focus_trans) {
      p2 = p2 + geom_sf(fill = "gray", 
                                data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }
    
    p2
  })
  
  
  output$trans_companion_map <- renderPlot({
    comp_p2 = merge_data %>%
      ggplot(aes(fill=Vehicle/100)) + 
      geom_sf() +
      labs(fill = NULL) + 
      theme_map() + 
      scale_fill_continuous(labels = percent)
    
    if (input$key_focus_trans) {
      comp_p2 = comp_p2 + geom_sf(fill = "gray", 
                                  data = neigh_geodata %>% filter(neigh_geodata$key_neighborhood == "non-focus"))
    }
    
    comp_p2
  })
  
  output$mod_map <- renderPlot({
    p3 = merge_data %>% 
      ggplot(aes(fill = Internet/100)) + 
      geom_sf() + 
      labs(fill = NULL) + 
      theme_map() + 
      scale_fill_continuous(labels = percent)
    
    if (input$key_focus_mod) {
      p3 = p3 + geom_sf(fill = "gray", 
                        data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }
    
    p3
  })
  
  
  output$mod_companion_map <- renderPlot({
    comp_p3 = merge_data %>%
      ggplot(aes(fill=Computing)) + 
      geom_sf() +
      labs(fill = NULL) + 
      theme_map() + 
      scale_fill_continuous(labels = percent)
    
    if (input$key_focus_mod) {
      comp_p3 = comp_p3 + geom_sf(fill = "gray", 
                                  data = neigh_geodata %>% filter(neigh_geodata$key_neighborhood == "non-focus"))
    }
    
    comp_p3
  })


  output$online_pie_chart <- renderPlot({
    ggplot(data = data %>% group_by(modality) %>%
             summarise(count = n()), aes(x="", y=count, fill=modality)) +
    geom_bar(stat="identity", width=1) +
    labs(title = "Modality of Programs Across Chicago", fill = "Modality") + 
    coord_polar("y", start=0) + 
    theme_void()
  })
  
  
  
  
  output$acc_map <- renderPlot({
    p4 = ggplot(data = merge_data, aes(fill= pro_per_youth)) + 
      geom_sf() +
      scale_fill_viridis_c(option = "B", begin = 0.15) + 
      theme_map()
    
    if (input$key_focus_acc) {
      p4 = p4 + geom_sf(fill = "gray", 
                        data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }
    
    p4
  })
  
  
  output$acc_companion_map <- renderPlot({
    comp_p4 = ggplot(data = merge_data, aes(fill= Youth_Perc)) + 
      geom_sf() +
      labs(fill = NULL) +
      scale_fill_viridis_c(option = "B", begin = 0.15) +
      scale_fill_continuous(labels = percent) + 
      theme_map()
    
    if (input$key_focus_acc) {
      comp_p4 = comp_p4 + geom_sf(fill = "gray", 
                                  data = neigh_geodata %>% filter(neigh_geodata$key_neighborhood == "non-focus"))
    }
    
    comp_p4
  })
  
  output$acc_map <- renderPlot({
    p5 = ggplot(data = merge_data, aes(fill= pro_per_youth)) + 
      geom_sf() +
      labs(fill = NULL) +
      scale_fill_viridis_c(option = "B", begin = 0.15) + 
      theme_map()
    
    if (input$key_focus_acc) {
      p5 = p5 + geom_sf(fill = "gray", 
                        data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }
    
    p5
  })
  
  output$pie_chart <- renderPlot({
    ggplot(neighborhood_select(), aes(x="", y=count, fill=`Category Name`)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)  +
      labs(title = paste("Top Types of Programs In ", chosen_neighborhood(), "for", chosen_age()), fill = "Program Type") + 
      theme_void()
  })
  
  
  output$online_activities_pie_chart <- renderPlot({
    ggplot(data =  online_activities %>% filter(count %in% tail(sort(online_activities$count),5)), aes(x="", y=count, fill=`Category Name`)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + 
      labs(title="Types of Online Programs Across Chicago") + 
      theme_void()
  })
  
  
  output$ad_map <- renderPlot({
    p6 = ggplot(data = merge_data, aes(fill = avg_age)) + 
      geom_sf() +
      scale_fill_viridis_c(option = "B", begin = 0.15)
    
    if (input$key_focus_ad) {
      p6 = p6 + geom_sf(fill = "gray", 
                        data = merge_data %>% filter(merge_data$key_neighborhood == "non-focus"))
    }

    p6
  })
  
  
  output$ad_companion_map <- renderPlot({
    comp_p6 = ggplot(data = merge_data, aes(fill = avg_dur)) + 
      geom_sf() +
      scale_fill_viridis_c(option = "B", begin = 0.15)
    
    if (input$key_focus_ad){
      comp_p6 = comp_p6 + geom_sf(fill = "gray", 
                                  data = neigh_geodata %>% filter(neigh_geodata$key_neighborhood == "non-focus"))
    }
    comp_p6
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
