#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://github.com/IBM-DSE/Shiny-Examples-with-Blog/blob/master/1%20-%20Leaflet%20-%20Center%20Diverging%20Colors/app.R

#NOTE Need to handle the centroid call outside of the reactive function
library(shiny)
library(shinydashboard)
library(shinyjqui)
#library(shinydashboardPlus)
library(tidyverse) 
library(RColorBrewer)
library(leaflet)
library(sf)
library(rsconnect)
library(here)
library(leafem)
# LOAD IN DATA ####
source("utils.R")
 
files_list <- list.files(here::here("input", "r-objects"), full.names = T)
files_short_names <- list.files(here::here("input", "r-objects"), full.names = F)
file_names <-  gsub(pattern = "\\.RDS$", replacement = "", x = basename(files_short_names))


files <- map(files_list, readRDS)

names(files) <- file_names
                            

# UI Choices #####
metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"


day_type_choices <- unique(files$network_data$`Day Type`)
period_choices <- unique(files$network_data$`Analysis Period`)
metric_choices <- unique(files$network_data$Metric)

network_choices <-  c("Baseline", "Phase 2")

                                         
                                         # look at folder, read in folder names, remove.zip from name

#UI #####
body <- dashboardBody(
  
  fluidRow(
  column(width = 6,
         jqui_resizable(
         box(title = "Metric Filters", 
             width = NULL, 
             solidHeader = TRUE,
             background = "navy",
             collapsible = T,
           selectInput("metric",
                       "Metric",
                       choices = metric_choices, 
                       multiple = FALSE, 
                       selected = "Change in Trips"),
           
           selectInput("day_type",
                       "Day",
                       choices = day_type_choices, 
                       multiple = FALSE, 
                       selected = "week"
           ),
           selectInput("period",
                       "Period",
                       choices = NULL, 
                       multiple = FALSE)
         )
         )
         ),
 
  column(width = 6,
         jqui_resizable(
   box( title = "Route Filters",
        width = NULL,
        solidHeader = TRUE, 
        background = "navy",
        collapsible = T,
        selectInput("network",
                "Network",
                choices = c("Baseline", "Final Proposal"), 
                multiple = FALSE, 
                selected = "Baseline"), 
    
    selectInput("routes",
                "Routes",
                choices = NULL, 
                multiple = TRUE)
   )
    ) 
   )#,  
  #column(width = 4,
  ),
  
  #  fluidRow(
  column(width = 12,  
     
         box(height = "100%",
             id = "map_container",
           width = NULL, solidHeader = TRUE,
           jqui_resizable( leaflet::leafletOutput("metric_map")#, 
                           #options = list(minWidth = 300)
                           )),
    
           box(width = NULL, solidHeader = TRUE,            
               jqui_resizable( tableOutput("click_info" ) 
          #   tableOutput("breaks"),
            # textOutput("geography")
             )))
          
        #)
)
    # )
#), 
#NOTES UI####
# tabPanel("Notes", 
#          fluidPage(
#            mainPanel(
#              h6(textOutput("note" ))
#            ))
# ),
# # HEADWAY AND TRIP TABLE UI ####
# tabPanel("Headways", 
#          fluidPage(
#            
#            sidebarLayout(
#              sidebarPanel(
#                selectInput("network_1",
#                            "First Service Change:",
#                            choices = network_choices,
#                            multiple = FALSE,
#                            selected = "213"),
#                selectInput("network_2",
#                            "Second Service Change:",
#                            choices = network_choices,
#                            multiple = FALSE,
#                            selected = "221"),
#                selectInput("table_contents",
#                            "Headways or Trips:",
#                            choices = c("Headways", "Trips"),
#                            multiple = FALSE,
#                            selected = "Headways")
#                ),
#         
#            mainPanel(
#              tabsetPanel(
#                tabPanel("First Service Change", h6(textOutput("note1" ))),
#                tabPanel("Second Service Change",  h6(textOutput("note2" ))),
#                tabPanel("Change Between Selected Networks",  h6(textOutput("note3" )))
#              )
#            )))
# ),
# 
# )

ui <- dashboardPage(
  dashboardHeader(title = "Network Comparison App"),
 dashboardSidebar(box( title = "Execute Map", width = NULL, 
                       background= 'navy', 
                       solidHeader = FALSE,     collapsible = T,
                       sliderInput("metric_range", 
                                   label = "Filter Data Range", 
                                   min = -100, 
                                   max = 100,
                                   value = c(-100, 100)),
                       
                       selectInput("geography", "Geography", 
                                   choices = c("Block Groups" = "block_group",
                                               "1/4 Mile Hex" = "quarter_mile_hex", 
                                               "1/8 Mile Hex" = "eigth_mile_hex"), 
                                   selected = "quarter_mile_hex"),
                     
                       checkboxInput("legend", "Show legend", TRUE),
                       actionButton("recalc", "Load Map & Filters"))),
  body
)
# SERVER#####
server <- function(input, output) {
  #MAP FUNCTIONS #####
      #handle route reactivity####
  
  #network selections
 network<- reactive({
    if (input$network == "baseline"){
      network <-  files$baseline_network 
    } else if(input$network == "Final Proposal"){
      network <- files$proposed_network
    } else{
      network <- files$baseline_network
    }

  })

 
  # update routes to correspond to network selected
  observeEvent(network(), {
    #req(input$network)
    #freezeReactiveValue(input, "routes")
    choices <- unique(network()$route_short_name)
    updateSelectInput( inputId = "routes", choices = choices)
  })
  
  

  
  
  #handle period/day combos ####
  
  period_reactive<- reactive({
    if (input$day_type == "week"){
       "week" 
    } else if(input$day_type == "weekday"){
      c("PM")
      # c("weekday" , "AM", "MID", "PM", "XEV")
    } else if(input$day_type == "saturday"){
      c("MID", "XEV")
    # }  else if(input$day_type == "sunday"){
    #   c("sunday")
    } else{
        ""
    }
    
  })
  
  observeEvent(period_reactive(), {
    #req(input$network)
    #freezeReactiveValue(input, "routes")
    choices <- unique( period_reactive())
    updateSelectInput( inputId = "period", choices = choices)
  })
  
  
conditional <- function(condition, success){
    if(condition) success else TRUE
  }  
  #handle route selections. Add in reset button?
routes <- eventReactive(input$recalc,{

#files$baseline_network

  if (input$network == "Baseline"){
route <- files$baseline_network  %>%
     filter( conditional(isTruthy(input$routes), route_short_name %in% input$routes )) %>%
      sf::st_as_sf()
  } else if(input$network == "Final Proposal"){
   route <- files$proposed_network %>%
      filter( conditional(isTruthy(input$routes),route_short_name %in% input$routes ))%>%
      sf::st_as_sf()
  }

  })

epa_hatch_reactive <- reactive({
  epa <- files$epa_hatch %>% 
    sf::st_as_sf()
})

#
# update range of filter for user controls
observe( {
 
  if(input$metric %in% c("Percent Change in Capacity" ,"Percent Change in Trips" )){
   data <- files$network_data %>% 
      filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type &
               Geography  == input$geography) %>% 
      drop_na() %>% 
      mutate(Value = Value*100)
   min_range <- min(data$Value, na.rm = T)
   max_range <- max(data$Value, na.rm = T)
   updateSliderInput( inputId = "metric_range",
                      min =min_range, 
                      max = max_range, 
                      value = c(min_range, max_range))
  } else {
  data <-   files$network_data %>% 
      filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type &
               Geography  == input$geography) %>% 
      drop_na() 
  
  min_range <- min(data$Value, na.rm = T)
  max_range <- max(data$Value, na.rm = T)
  updateSliderInput( inputId = "metric_range",
                     min =min_range, 
                     max = max_range, 
                     value = c(min_range, max_range))
  }

})

      #filter data for user input on metrics#####
  metric_data <- eventReactive(input$recalc, {
    req(input$recalc)
    if(input$metric %in% c("Percent Change in Capacity" ,"Percent Change in Trips" )){
      filtered_hex_data <-  files$network_data %>% 
      filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type &
               Geography == input$geography ) %>% 
      drop_na(Value)  %>% 
        mutate(Value = Value*100) %>% 
        filter( Value >= input$metric_range[1] &
                  Value <= input$metric_range[2] )
     
     minVal <- min(filtered_hex_data$Value)
     maxVal <- max(filtered_hex_data$Value)
     domain <- c(minVal,maxVal)
     values_df <-  filtered_hex_data$Value
     
     center <- as.numeric(0)
     interval <- ifelse((maxVal - minVal)>10,10,
                        ifelse((maxVal - minVal)>5,1,0.2))
     
     
     color_bucket <- calculateBucket(min_val = minVal,max_val = maxVal,values_df = values_df,
                                     max_bin=7,interval=10,interval_options=seq(10,5000,10),
                                     center=100,floor_at=NULL,ceil_at=NULL)
     df_pal <- inferColor(color_bucket,
                          color_below = "#b2182b",
                          color_above = "#2166ac",
                          interval=interval,
                          center=center)
     
     
     filtered_hex_data <- filtered_hex_data %>%
       mutate(metric_color_label = cut(Value, breaks = color_bucket$breaks,
                                       labels = color_bucket$breaks_label,
                                       include.lowest = TRUE)) %>%
       mutate(metric_color_label = as.factor(metric_color_label)) %>%
       dplyr::left_join(df_pal) %>%
       arrange(metric_color_label)
    } else {
      filtered_hex_data <- files$network_data %>% 
        filter(Metric == input$metric &
               `Analysis Period` == input$period &
               `Day Type` == input$day_type &
                 Geography == input$geography ) %>% 
        drop_na(Value) %>% 
        filter( Value >= input$metric_range[1] &
                  Value <= input$metric_range[2])
      
      minVal <- min(filtered_hex_data$Value)
      maxVal <- max(filtered_hex_data$Value)
      domain <- c(minVal,maxVal)
      values_df <- filtered_hex_data$Value
      
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(min_val = minVal,max_val = maxVal,values_df = values_df,
                                      max_bin=7,interval=10,interval_options=seq(10,5000,10),
                                      center=100,floor_at=NULL,ceil_at=NULL)
      df_pal <- inferColor(color_bucket,
                           color_below = "#b2182b",
                           color_above = "#2166ac",
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(Value, breaks = color_bucket$breaks,
                                        labels = color_bucket$breaks_label,
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>%
        arrange(metric_color_label)
      
    }
     }, ignoreNULL = FALSE)

  
  metric_data_sf <- eventReactive(input$recalc,{
    if(input$geography == "block_group"){
    block_groups <- files$block_groups %>% 
      left_join(metric_data(), by = "Geoid") %>% 
     drop_na(Value) %>% 
     # filter(Value != 0) %>% 
      sf::st_as_sf() #added because R was making this a table not a spatial object
    } else if (input$geography == "quarter_mile_hex" ){
      quarter_mile <- files$quarter_mile_hex_grid %>% 
        left_join(metric_data(), by = "Geoid") %>% 
       drop_na(Value) %>% 
      #  filter(Value != 0) %>% 
        sf::st_as_sf()
      
    } else if (input$geography == "eigth_mile_hex" ){
      eigth_mile <- files$eigth_mile_hex_grid %>% 
        left_join(metric_data(), by = "Geoid") %>% 
      drop_na(Value) %>% 
      #  filter(Value != 0) %>% 
        sf::st_as_sf()
    }
  },  ignoreNULL = FALSE)
  
# responsive labels for multiple geos
  metric_data_labels <- eventReactive(input$recalc,{
    
    
    if(input$geography == "block_group"){
    files$block_group_centroids %>%
      left_join(metric_data()) %>%
      drop_na(Value) %>%
      filter(Value != 0) %>%
      sf::st_as_sf() #added because R was making this a table not a spatial object
      } else if (input$geography == "quarter_mile_hex" ){
        files$quarter_mile_hex_grid %>% 
          left_join(metric_data()) %>%
          drop_na(Value) %>%
          filter(Value != 0) %>%
          sf::st_as_sf()
      } else if (input$geography == "eigth_mile_hex" ){
        files$eigth_mile_hex_grid %>% 
          left_join(metric_data()) %>%
          drop_na(Value) %>%
          filter(Value != 0) %>%
          sf::st_as_sf()
      }
  },  ignoreNULL = FALSE)
  
  #recalc legend to respond to new breaks
  
  reactive_legend <- reactive({
    label_data <-  metric_data_sf() %>%
      select(metric_color_label, metric_color_group) %>% 
      distinct(metric_color_label, metric_color_group) %>% 
      arrange(metric_color_label)
  })

rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)

observeEvent(input$metric_map_shape_click, {
  map_land_shape_click_info <- input$metric_map_shape_click
  # map_land_click_info <- input$map_land_click
  
  rv_location$id <-  map_land_shape_click_info$id #str_split_fixed(map_land_shape_click_info$id,'\\|',2)[2] # take the second part which is county name
  # rv_location$lat <- round(map_land_click_info$lat,4)
   #rv_location$lng <- round(map_land_click_info$lng,4)
})


metric_data_detail <- eventReactive(input$metric_map_shape_click, {
  files$network_data_details %>% 
    filter(Geoid== input$metric_map_shape_click$id &
             `Analysis Period` == input$period &
             `Day Type` == input$day_type &
             Geography == input$geography) %>% 
    select(Route, 
           `Trips per Rte Baseline`, 
           `Trips per Rte Proposed`, 
           `Change in Trips`, 
           `Percent Change in Trips`) %>% 
    arrange(`Change in Trips`)
  
  
})

output$click_info <- renderTable(metric_data_detail())


  # Map reactives ####
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
                       offset.y = 100,
                       height = 30, 
                       width = 80) %>% 
      leaflet::addScaleBar(position = "topright")   %>% 
      addLayersControl(
        overlayGroups = c( "EPA Overlay", "Labels", "Routes"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) %>% 
      hideGroup(c("EPA Overlay", "Routes", "Labels") )
  })
  
  
  
   observeEvent(input$recalc, {
   
   proxy <- leafletProxy("metric_map")   %>%
     clearShapes() %>%
     clearGroup("Labels") %>% #"Labels", "EPA Overlay", "Routes"
    
      addPolygons( data = metric_data_sf() ,
                   weight = 2, opacity = 1,
                   color = "white",
                   dashArray = "3",
                   layerId = metric_data_sf()$Geoid,
                   fillOpacity = 0.7 ,
                   highlightOptions = highlightOptions(
                               weight = 5,
                               color = "#666",
                               dashArray = "",
                               fillOpacity = 0.7,
                               bringToFront = FALSE) ,   #)#,
                 label = ~paste0(Value,""),
                   labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"),
                 fillColor = ~metric_data_sf()$metric_color_group,
                 popup = ~paste0(input$metric, ": ", Value,
                                 "<br>Routes in Baseline Network: ", `Routes in Geo Baseline`,
                                 "<br>Routes in Proposed Network: ", `Routes in Geo Proposed`
                                 )
      ) %>%
      addPolylines(
        data = epa_hatch_reactive(),
        color = "black",
        weight = 0.6,
        group = "EPA Overlay"
      ) %>%
      addPolylines(
        data = routes(),
        color = "black",
        weight = 3 ,
        group = "Routes",
        label = ~route_short_name,
        popup = ~paste0("<br>Route: ", route_short_name,
                        "<br>Description: ", description  ) ) %>%
     leafem::addStaticLabels(
    # addLabelOnlyMarkers(
                   data = metric_data_labels(),
                  # lat = metric_data_labels()$Y,
                    #lng = metric_data_labels()$X,
                    label = metric_data_labels()$Value,
                     group = "Labels") %>%
      addLayersControl(
        overlayGroups = c( "EPA Overlay", "Labels", "Routes"), #
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) #%>%

    # hideGroup(c("EPA Overlay", "Routes") ) #
     # myVariable <<- proxy
  } ,ignoreNULL = FALSE)


  
  #recreate legend if needed ####
  observeEvent(input$recalc,{
    proxy <- leafletProxy("metric_map", data = metric_data_sf())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend ) {
      #pal <- colorpal()
      proxy %>% addLegend(position = "topright",
                          colors = reactive_legend()$metric_color_group,
                          labels = reactive_legend()$metric_color_label,
                          opacity =  0.9,
                          title = input$metric)
    }
  }, ignoreNULL = FALSE)
   
   


  
  
    
  # NOTES SERVER #####
  
  output$note <- renderText("This app shows the difference in vehicle trips and vehicle capacity for the Madison Street Area Final Proposal.
This tool is for planning purposes only and does not show final data.
Please contact Melissa Gaughan with questions. Last updated 2023.10.25.")
  
 
  #TABLE FUNCTIONS #####  
  
  # Ok time for some dev work here. 
  # If user input == headways, go to GTFS folder, grab specified GTFS 
  #Calculate avg headways for weekdays, weekends by period
  #display by route, else calculate trips by time period
  
  #If user is on change tab, use results from network 1 and 2 to find differences. 
  # Routes that are not in baseline network get flagged as new. Rotues that are 
  # not in second network get flagged as deleted. Both new/deleted routes sent to second table on change tab
  

 
  # 
  # output$click_info <- renderText({  
  #   location_info <- reactiveValuesToList(rv_location)
  #   
  #   HTML(paste(h3(rv_location$id)))
  #   })
    
   
  
  output$network_1 <- eventReactive(input$table_contents, {
    if (input$table_contents == "Headways" ){
      # read file
  
      
      #file_name <- paste0( input$network_1, "_gtfs.zip")
      file_name <- paste0(213, "_gtfs.zip")
      
      gtfs_1 <- tidytransit::read_gtfs(here::here("input","gtfs", file_name))
      
      
      #calculate headways
    }
  })
  
  output$note2 <- renderText("note2")
  
  output$note3 <- renderText("note3")
  
 # output$geography <- renderText(paste0(input$geography))

 
}

# Run the application 
shinyApp(ui = ui, server = server)
