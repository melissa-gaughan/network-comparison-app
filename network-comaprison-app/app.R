#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(sf)
library(rsconnect)
library(here)
block_groups <- readRDS( here::here(  "input","block_groups.RDS"))
                            
load(file =here::here( "input","route_and_block_group_equity_data.RDS"))
                            
network_data <- read_csv(here::here( "input","block_group_trips_and_capacity_summary.csv")) %>% 
         select(-c(Name:`Acs Year`)) %>% 
         pivot_longer(cols = !c(Geoid, `Analysis Period`, `Day Type`, `Routes in Geo Baseline`, 
         `Routes in Geo Proposed`), 
           names_to = "Metric", 
            values_to = "Value")
                            
block_group_need_scores<- block_group_need_scores %>% 
                             mutate(Geoid = as.numeric(geoid))
                            
epa_hatch <- block_groups %>%
             left_join(block_group_need_scores) %>% 
               mutate(equity_priority_area = ifelse(final_score >= 4, TRUE, FALSE)) %>%
             filter(equity_priority_area) %>% # filter for tracts that I want to receive a cross-hatch
            # sample_n(40) %>%
            HatchedPolygons::hatched.SpatialPolygons(density = 500, angle = 45) %>%
              st_set_crs(4326) %>%
              mutate(col = 1) 
                            

metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"


day_type_choices <- unique(network_data$`Day Type`)
period_choices <- unique(network_data$`Analysis Period`)
metric_choices <- unique(network_data$Metric)

#there's a weird UI think because of the small periods not being in the data for 
# Sat/SUN
# test<- network_data %>% 
#   distinct(`Analysis Period`, `Day Type`)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
    # Application title
    titlePanel("Pre/Post Network Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("metric",
                        "Metric to Display:",
                        choices = metric_choices, 
                        multiple = FALSE, 
                        selected = "Change in Capacity"),
        
        selectInput("day_type",
                    "Day of Week",
                    choices = day_type_choices, 
                    multiple = FALSE, 
                    selected = "weekday"
                    ), 
        
        selectInput("period",
                    "Day Period:",
                    choices = period_choices, 
                    multiple = FALSE, 
                    selected = "AM"), 
       
        selectInput("colors", "Color Scheme",
                    rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), 
                    selected = "Spectral"
        ), 
        checkboxInput("legend", "Show legend", TRUE),
        actionButton("recalc", "Update Map"), 
        hr(), 
        h6("This app shows the difference in vehicle trips and vehicle capacity for Lynnwood Link Phase 2. This tool is for planning purposes only and does not show final data. Please contact Melissa Gaughan with questions. Last updated 2022.08.11.")
    
    ),
 
        # Show a plot of the generated distribution
        mainPanel(
          leaflet::leafletOutput("metric_map", height = "100vh")
        )
    )
)

server <- function(input, output) {
  
  metric_data <- eventReactive(input$recalc, {
    if(input$metric %in% c("Percent Change in Capacity" ,"Percent Change in Trips" )){
    network_data %>% 
      filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type) %>% 
      drop_na() %>% 
        mutate(Value = Value*100)
    } else {
      network_data %>% 
        filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type) %>% 
        drop_na() 
    }
     }, ignoreNULL = FALSE)
  
  metric_data_sf <- eventReactive(input$recalc,{
    block_groups %>% 
      left_join(metric_data()) %>% 
      drop_na(Value) %>% 
      filter(Value != 0)
  },  ignoreNULL = FALSE)
  
  colorpal <- reactive({
    colorBin(input$colors, metric_data_sf()$Value)
  })

  
  output$metric_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::setView(lng = -122.3321, lat = 47.6062, zoom = 11 ) %>%
      leaflet.extras::addSearchOSM() %>%
      leafem::addLogo( img =   metro,
                       src="remote", 
                       position = "bottomright",
                       offset.x = 30,
                       offset.y = 75,
                       height = 30, 
                       width = 80) %>% 
      leaflet::addScaleBar(position = "topright")
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observeEvent(input$recalc, {
    pal <- colorpal()
    
    leafletProxy("metric_map", data = metric_data_sf()) %>%
      clearShapes() %>%
      addPolygons( weight = 2, opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   highlightOptions = highlightOptions(
                               weight = 5,
                               color = "#666",
                               dashArray = "",
                               fillOpacity = 0.7,
                               bringToFront = TRUE),
                   label = ~Value,
                   # labelOptions = labelOptions(
                   #         style = list("font-weight" = "normal", padding = "3px 8px"),
                   #         textsize = "15px",
                   #         direction = "auto"),
                 fillColor = ~pal(Value), 
                 popup = ~paste0(input$metric, ": ", Value, 
                                 "<br>Routes in Baseline Network: ", `Routes in Geo Baseline`,
                                 "<br>Routes in Proposed Network: ", `Routes in Geo Proposed`
                                 )
      ) %>% 
      addPolylines(
        data = epa_hatch, 
        color = "black",
        weight = 0.6, 
        group = "EPA Overlay"
      ) %>% 
      addLayersControl(
        overlayGroups = c( "EPA Overlay"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("EPA Overlay")
    
  } ,ignoreNULL = FALSE)
  
  observeEvent(input$recalc,{
    proxy <- leafletProxy("metric_map", data = metric_data_sf())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend ) {
      pal <- colorpal()
      proxy %>% addLegend(position = "topright",
                          pal = pal, values = ~metric_data_sf()$Value, 
                          title = input$metric
      )
    }
  }, ignoreNULL = FALSE)
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
