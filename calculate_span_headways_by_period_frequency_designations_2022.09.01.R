 # # # # # # # # # # # # # # # # # # # # # # # # 

# Load packages and set working directory ###############
library(hms)
library(rstudioapi)
library(sf)
library(tidycensus)
library(tidytransit)
library(tidyverse)
library(tigris)
library(reshape2)

# Set working directory to folder where R script is saved
# wd <- dirname(getActiveDocumentContext()$path)
# setwd(wd)
# rm(wd)
# Prevent scientific notation 
options(scipen=999)

# As requested by Metro
# CRS used in script is HARN WA State North
CRS <- 2926

# # # # # # # # # # # # # # # # # # # # # # # # 
# Load input files and select network         ################     
# # # # # # # # # # # # # # # # # # # # # # # # 

# Save gtfs files in the "1_inputs" folder of the R project
# write the full name of the current and future GTFS files to be used
# The rest of the code should run from here to bottom

#if/when this becomes a shiny app, service change will be input$service_change
service_change <- "213_gtfs"
service_change_num <- "213"
name_current_gtfs_file <- paste0(service_change , ".zip")


gtfs_selected = "current"

# Code below here can be run fully for the Current or Future Network
# Recall that the future Network code does some adjustments to the 
# name of the routes in the GTFS file, because those GTFS come from Remix
# If GTFS for future network from other sources than remix, the future code sections
# will require updates

# # # # # # # # # # # # # # # # # # # # # # # # 
# General GTFS processing section             #     
# # # # # # # # # # # # # # # # # # # # # # # # 

#Read selected GTFS file

#import data from NetPlan combination gtfs created in "0_combine_netplan_gtfs"

kcm <- gtfs

  kcm <- read_gtfs(paste0("gtfs/",name_current_gtfs_file))

  kcm$routes <- kcm$routes %>% 
    mutate(route_short_name = ifelse(is.na(route_short_name), route_long_name,
                                     paste0(route_short_name,
                                            ' ',
                                            route_long_name))) %>% 
    mutate(route_short_name = trimws(route_short_name))

# # # # # # # # # # # # # # # # # # # # # # # # 
# Step 1 - Process Main Bulk Metrics          ###############
# # # # # # # # # # # # # # # # # # # # # # # # 

# Identify service ID by type of day (weekday, sat or sun)
service_days <- kcm$calendar %>% 
  # Make sure service IDs are actually in use in the trip tables
  filter(service_id %in% unique(kcm$trips$service_id)) %>% 
  # Define daytype of service ID to filter in next steps 
  mutate(daytype = ifelse(saturday == 1, 'sat',
                          ifelse(sunday == 1, 'sun', 'wkd')))
  day_type<- 'wkd'


create_trips_table <- function( day_type){ 
if (day_type == "wkd"){
# Filter weekday only service IDs
serv_wkd <- service_days %>% 
  filter(daytype == 'wkd') 
} else if (day_type == "sat"){
  # Filter weekday only service IDs
  serv_wkd <- service_days %>% 
    filter(daytype == 'sat') 
} else if (day_type == "sun"){
  serv_wkd <- service_days %>% 
    filter(daytype == 'sun') 
}else{
  print("Day type incorrect. Options are 'wkd', 'sat', sun'. Case sensistive.")
}

# count_trips_by_service_id <-  kcm$trips %>% 
# 
#   left_join(kcm$routes) %>% 
#   left_join(kcm$calendar) %>% 
#   mutate(route_short_name = trimws(route_short_name), 
#          service_change_num = expr(!!service_change_num)) %>% 
#   filter(service_id %in% serv_wkd$service_id) %>% 
#   group_by()
# 
# # Clean up
#rm(service_days)

# trips_schedule_wkd <- kcm$stop_times %>% 
#   # Filter stop-trips of the relevant routes and only weekday
#   filter(trip_id %in% trips$trip_id) %>% 
#   select(trip_id, arrival_time, departure_time, stop_id, stop_sequence) %>% 
#   # Sort the dataframe
#   arrange(trip_id, arrival_time, stop_sequence) %>% 
#   # Group by unique trip (Recall this dataframe is already sorted by time)
#   group_by(trip_id) %>% 
#   # Create two new columns for each trip (group)
#   # One with the start time (string type) of every trip
#   # The other with the end time (string type) of every trip
#   summarise(start_time_str = as.numeric(min(arrival_time)),
#             end_time_str = as.numeric(max(arrival_time))) %>%
#   
#   mutate(start_time_str = hms::hms(seconds=start_time_str),
#          end_time_str = hms::hms(seconds=end_time_str)) %>% 
#   left_join(kcm$trips %>% select(trip_id, route_id, trip_headsign) )
# 
# 
# 
# route_direction <- trips_schedule_wkd %>% 
#   ungroup() %>% 
#   distinct(route_id, route_short_name, direction_id) #trip_headsign,
# 
# route_direction_list <- as.list(route_direction)

# Weekdays
# Filter only trips in the routes in the corridors selected
trips <- kcm$trips %>% 
  #filter(route_id == "223-1988") %>% 
  # Filter only weekday trips
  filter(service_id %in% serv_wkd$service_id) %>%
  # Keep only relevant variables
  select(route_id, service_id, trip_id, direction_id)  #trip_headsign,


# Create a dataframe with the stops and times of desired trips
stop_times <- kcm$stop_times %>% 
  # Filter stop-trips of the relevant routes and only weekday
  filter(trip_id %in% trips_wkd$trip_id) %>% 
  select(trip_id, arrival_time, departure_time, stop_id, stop_sequence) %>% 
  # Sort the dataframe
  arrange(trip_id, arrival_time, stop_sequence) %>% 
  # Group by unique trip (Recall this dataframe is already sorted by time)
  group_by(trip_id) %>% 
  # Create two new columns for each trip (group)
  # One with the start time (string type) of every trip
  # The other with the end time (string type) of every trip
  summarise(start_time_str = min(arrival_time),
            end_time_str = max(arrival_time))

# # This snippet process the time fields from character type into more manageable type and values
# # The raw time in the GTFS contained midnight and past-midnight trips hours as
# # 24, 25, 26, 27 etc.
# # First, join the trip dataframe with the stop times computed previously 
# trips_wkd <- left_join(trips_wkd, stop_times) %>%
#   select(-service_id, -trip_id) %>% 
#   filter(!(is.na(start_time_str) | is.na(end_time_str) )) %>% 
#   # Remove potential duplicates to avoid errors in the calculations
#   distinct(., route_id, direction_id, start_time_str, .keep_all = TRUE) %>%
#   # Extract only the hour integer from the character time and set it as numeric
#   mutate(start_time = as.numeric(substr(start_time_str,1,2)),
#          end_time = as.numeric(substr(end_time_str,1,2))) %>%
#   # Convert midnight and past-midnight hours to equivalent hours
#   # e.g. 24 to 00, 25 to 01, 26 to 02, etc.
#   #added this line to preserve the 30 hour clock for later processing of night minutes. 
#   mutate(metro_clock_start_time =  start_time, 
#          metro_clock_end_time = end_time) %>% 
#   mutate(start_time = ifelse(start_time > 23, start_time - 24, start_time),
#          end_time = ifelse(end_time > 23, end_time - 24, end_time)) %>% 
#   # Add back the minutes and seconds to the cleaned numeric trip hours
#   # This will reverse back the type od the data to character
#   mutate(start_time = paste0(str_pad(start_time,2, pad = 0),substr(start_time_str,3,8)),
#          end_time = paste0(str_pad(end_time,2, pad = 0),substr(end_time_str,3,8))) %>%
#   # Change start and end time type from character to difftime type (hms)
#   # This will allow to perform calculations such as sums and subtractions
#   mutate(start_time = hms::as_hms(start_time),
#          end_time = hms::as_hms(end_time)) 


trips_wkd <- left_join(trips, stop_times) %>%
  select( -trip_id) %>% 
  filter(!(is.na(start_time_str) | is.na(end_time_str) )) %>%
   mutate(start_time = hms::as_hms(start_time_str), 
         end_time = hms::as_hms( end_time_str)) %>% 

mutate(start_time_str = as.character(start_time), 
       end_time_str = as.character(end_time)) %>% 
#create the hour info needed for period assignment  
  # Extract only the hour integer from the character time and set it as numeric
  mutate(metro_clock_start_time = as.numeric(substr(start_time_str,1,2))) %>%
  # Calculate service hours for every trip
  # A conditional statement is used for those trips that start before midnight and end after it
  mutate(service_hr = ifelse(start_time <  end_time,
                             difftime(end_time, start_time, unit = 'hours'),
                             difftime(end_time, start_time, unit = 'hours') + 24),
         # Define period of day based on the integer hour the trip starts
         period = ifelse( metro_clock_start_time %in% c(4:5), '0.early_am',
                    ifelse(metro_clock_start_time %in% c(6:8), '1.am_peak',
                    ifelse(metro_clock_start_time %in% c(9:14), '2.midday', 
                         ifelse(metro_clock_start_time %in% c(15:18), '3.pm_peak',
                                ifelse(metro_clock_start_time %in% c(19:21), '4.evening',
                                       ifelse(metro_clock_start_time %in% c(22:24), '5.night', '6.owl'))))))) %>%
  # Sort dataframe, the relevant variable is start time of the trip
  # Note that we use the string start time, to ensure that a trip starting at 25:00:00 
  # is sorted at the end of the day an not earlier as a 01:00:00 trip
  arrange(route_id, direction_id, start_time_str) %>% 
  group_by(route_id, direction_id, period) %>%
  #filter(route_id == 100001) %>% 
  # For every corridor calculate the start time of the first trip and the
  # starting time of the last trip
  mutate(start_first_trip_period = first(metro_clock_start_time),
         start_last_trip_period = last(metro_clock_start_time), 
         #this is calculating the total hours in the period served by the route. I added a one hour period
         #to solve the issue of both trips in a period starting in the same hour
    
         period_length = start_last_trip_period - start_first_trip_period+1) %>% 
 # filter(route_id == 100228 & period == "0.early_am")
         # period_length = ifelse(start_last_trip_period - start_first_trip_period > 0, start_last_trip_period - start_first_trip_period, 
         #                        start_last_trip_period - start_first_trip_period +24), 
         #period_length = hms(period_length))

  ungroup() %>% 
  group_by(route_id, direction_id) %>% 
  
  # Calculate the start of first trip and end of last trip 
  
  mutate(first = first(start_time),
         last = last(start_time)) %>% 
  
  mutate(first= as.character(first), 
         last = as.character(last)) %>% 
  #create the hour info needed for period assignment  
  # Extract only the hour integer from the character time and set it as numeric
  mutate(first = as.numeric(substr(first,1,2)), 
         last = as.numeric(substr(last,1,2))) %>% 
  # Compute the span of service for all day for every corridor
  # This all day span of service was not used for any further calculation
  # It can be removed in final version
  mutate(span_gtfs_hrs = ifelse(first <  last,
                                last - first,
                                last - first + 24)) 
  # Calculate how many hours before 5am a trip starts (Upper)
  # Calulate how many hours after 7pm (19:00) a trip starts (Lower) 
  
  #####################################################

# Aggregate dataframe by route and period


first_last_trips <- trips_wkd %>% 
  group_by(route_id, direction_id, period) %>% 
  arrange(start_time, by_group = TRUE) %>% 
 mutate(first_period_trip =  first(start_time), 
        last_period_trip = last(start_time)) %>% 
  select(route_id, direction_id, period, first_period_trip, 
         last_period_trip) %>% 
  distinct() %>% 
  #so, to deal with instances where the evening period stretches across the midnight
  #mark, I am FLIPPING the first/last trips previously assigned. 
  mutate(first_period_trip_final = case_when( period == "5.night"
                                         & (last_period_trip - first_period_trip) > 2*60*60 ~ last_period_trip, 
                                         TRUE ~ first_period_trip), 
         last_period_trip_final = case_when( period == "5.night"
                                         & (last_period_trip - first_period_trip) > 2*60*60 ~ first_period_trip, 
                                         TRUE ~ last_period_trip))
# as_hms('22:00:00') - as_hms('01:00:00')
#   distinct(start_first_trip_period, start_last_trip_period)

route_wkd <- trips_wkd %>%
  group_by(route_id, period, direction_id,  period_length, span_gtfs_hrs  #trip_headsign,
           ) %>%
  # Calculate trips in period and service hours in period
  summarise(num_trips = n()) %>%
  # Calculate service hour per trip and adjust minutes of operation in period based on the
  # adjustment values calculated earlier
  # Calcualte average headways based on GTFS processed data   
  mutate(avg_headway_mins = round(period_length*60/num_trips, 0)) %>% 
  ungroup() 

 
 weekday_trips <- kcm$routes %>% 
   
      select(route_id, route_short_name ) %>%
  
  # filter(route_id == "223-1988") %>% 
  left_join(route_wkd) %>% 
   mutate(day_type = day_type , 
          period = stringr::str_remove_all(period, "[:digit:]"), 
          period = stringr::str_remove_all(period, '[\\.,]')) %>% 
   drop_na()
}

 
 
   
    # mutate(period = stringr::str_sub(period, start = 3))
 

  test <- map_dfr(c('wkd', 'sat', 'sun'), create_trips_table) %>% 
    #this is a step to handle NetPlan schedules that concatenate route id and scenario id
    separate(route_id, into = c("route", "schedule"), sep = "-", 
             extra = "merge") 
  
  final_trips_table <- test %>% 
    select(-schedule) %>% 
    mutate(route= str_replace_all(route, "R", "")) %>% 
    pivot_wider(id_cols = route:direction_id, # #trip_headsign, 
                names_from = c(day_type, period), 
                values_from =  num_trips, 
                names_glue = "{day_type}_{period}_{.value}", 
                names_sort = FALSE) %>% 
    select( route, route_short_name,   direction_id, everything()) %>%  #trip_headsign,
    # separate(route_short_name, into = c("route", "description"), sep = "\\s", 
    #          extra = "merge") %>% 
   mutate(route= str_replace_all(route, "S", "")) %>% 
    mutate(route= str_replace_all(route, "E", ""))%>% 
    filter( !(str_detect (route_short_name, "CT" ) | str_detect (route_short_name, "PT" ))) %>% 
    mutate(route = as.numeric(route))
 
# 2021.08.05 TASKS
  # 1. Make another function for the headways table. DONE
  #2. Add in the option to summarize/average for bi-directional service
  #3. Investigate the NA Period issues. Seems to be affecting some Saturday and Sunday service. DONE
  
  

  weekday_headways <- test %>%

    select(-schedule) %>% 
    mutate(route= str_replace_all(route, "R", "")) %>% 
    pivot_wider(id_cols = route:direction_id,  #trip_headsign, 
                names_from = c(day_type, period), 
                values_from =  avg_headway_mins, 
                names_glue = "{day_type}_{period}_{.value}", 
                names_sort = FALSE) %>% 
    select( route, route_short_name,   direction_id, everything()) %>%  #trip_headsign,
    # separate(route_short_name, into = c("route", "description"), sep = "\\s", 
    #          extra = "merge") %>% 
    mutate(route= str_replace_all(route, "S", "")) %>% 
    mutate(route= str_replace_all(route, "E", ""))%>% 
    filter( !(str_detect (route_short_name, "CT" ) | str_detect (route_short_name, "PT" ))) %>% 
    mutate(route = as.numeric(route)) 
  

bidirectional_headways <-   test %>%
  
  select(-schedule) %>% 
  mutate(route= str_replace_all(route, "R", ""))

setwd("C:/Users/mgaughan/OneDrive - King County/Projects/East-Link/output")

write_csv(weekday_headways, paste("headways", service_change, ".csv", 
                                  sep ="_" ))
write_csv(final_trips_table, paste("period_trips",  service_change, ".csv", 
          sep ="_" ))


