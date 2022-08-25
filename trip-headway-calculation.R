create_trips_headways_table <- function( day_type){ 

  gtfs <- gtfs_1
  
  service_days <- gtfs$calendar %>% 
    # Make sure service IDs are actually in use in the trip tables
    filter(service_id %in% unique(gtfs$trips$service_id)) %>% 
    # Define daytype of service ID to filter in next steps 
    mutate(daytype = ifelse(saturday == 1, 'sat',
                            ifelse(sunday == 1, 'sun', 'wkd')))


  
  trips_schedule <- gtfs$trips %>% 
    #filter(route_id == "223-1989") %>% 
    left_join( gtfs$stop_times) %>%
    left_join(gtfs$routes) %>% 
    #mutate(route_short_name = trimws(route_short_name)) %>% 
    select( trip_id, route_id, route_short_name, 
           direction_id,  arrival_time, departure_time, #trip_headsign,
           stop_id, stop_sequence) %>% 
    # Sort the dataframe
    arrange(trip_id, arrival_time, stop_sequence) %>% 
    # Group by unique trip (Recall this dataframe is already sorted by time)
    group_by(trip_id) %>% 
    # Create two new columns for each trip (group)
    # One with the start time (string type) of every trip
    # The other with the end time (string type) of every trip
    mutate(start_time_str = min(arrival_time),
           end_time_str = max(arrival_time)) %>% 
    # Extract only the hour integer from the character time and set it as numeric
    mutate(start_time = as.numeric(substr(start_time_str,1,2)),
           end_time = as.numeric(substr(end_time_str,1,2)), 
           start_time_min = as.numeric(substr(start_time_str,4,5)),
           end_time_min = as.numeric(substr(end_time_str,4,5)),
           start_time_sec = as.numeric(substr(start_time_str,7,8)),
           end_time_sec = as.numeric(substr(end_time_str,7,8))) %>% 
    mutate(       
      start_secs_after_midnight = start_time*60*60 + start_time_min * 60 + start_time_sec, 
      end_secs_after_midnight = end_time*60*60 + end_time_min * 60 + end_time_sec  ) %>%
    select(-c(start_time:end_time_sec) ) %>% 
    mutate(start_time = as.numeric(substr(arrival_time,1,2)),
           end_time = as.numeric(substr(departure_time,1,2)), 
           start_time_min = as.numeric(substr(arrival_time,4,5)),
           end_time_min = as.numeric(substr(departure_time,4,5)),
           start_time_sec = as.numeric(substr(arrival_time,7,8)),
           end_time_sec = as.numeric(substr(departure_time,7,8))) %>% 
    mutate(       
      arrival_secs_after_midnight = start_time*60*60 + start_time_min * 60 + start_time_sec, 
      departure_secs_after_midnight = end_time*60*60 + end_time_min * 60 + end_time_sec
    ) %>%
    select(-c(start_time:end_time_sec) ) %>% 
    left_join(gtfs$stops %>% select(stop_id, stop_name) )

  trips <- gtfs$trips %>% 
    #filter(route_id == "223-1988") %>% 
    # Filter only weekday trips
    
    # Keep only relevant variables
    select(route_id, service_id, trip_id, direction_id)
  
  
  
  # Create a dataframe with the stops and times of desired trips
  stop_times <- gtfs$stop_times %>% 
    # Filter stop-trips of the relevant routes and only weekday
    filter(trip_id %in% trips$trip_id) %>% 
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence) %>% 
    # Sort the dataframe
    arrange(trip_id, arrival_time, stop_sequence) %>% 
    # Group by unique trip (Recall this dataframe is already sorted by time)
    group_by(trip_id) %>% 
    # Create two new columns for each trip (group)
    # One with the start time (string type) of every trip
    # The other with the end time (string type) of every trip
    summarise(start_time_str = as.numeric(min(arrival_time)),
              end_time_str = as.numeric(max(arrival_time))) %>%
    
    mutate(start_time_str = hms::hms(seconds=start_time_str),
           end_time_str = hms::hms(seconds=end_time_str)
    )
  
  
 
  # First, join the trip dataframe with the stop times computed previously 
  trips_1 <- left_join(trips, stop_times) %>%
    select(-service_id, -trip_id) %>% 
    filter(!(is.na(start_time_str) | is.na(end_time_str) )) %>% 
    # Remove potential duplicates to avoid errors in the calculations
    distinct(., route_id, direction_id, start_time_str, .keep_all = TRUE) %>%
    # Extract only the hour integer from the character time and set it as numeric
   
    mutate(start_time = lubridate::hms(start_time_str),
           end_time = lubridate::hms(end_time_str)) 
  
  trips <- trips %>% 
    # Extract only the hour integer from the character time and set it as numeric
    mutate(hour = as.numeric(substr(start_time, 1, 2))) %>%
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
    
    # Calculate the start of first trip and end of last trip in numeric hours
    # (this has the hours as integer and the minutes and seconds in decimals)
    
    mutate(first = as.difftime(first(as.character(start_time)), format = '%H:%M:%S', units = 'hours'),
           last = as.difftime(last(as.character(start_time)), format = '%H:%M:%S', units = 'hours')) %>% 
    # Compute the span of service for all day for every corridor
    # This all day span of service was not used for any further calculation
    # It can be removed in final version
    mutate(span_gtfs_hrs = ifelse(first <  last,
                                  last - first,
                                  last - first + 24)) 
  # Calculate how many hours before 5am a trip starts (Upper)
  # Calulate how many hours after 7pm (19:00) a trip starts (Lower) 
  
  #####################################################
  
  # Aggregate dataframe by corridor and period
  
  
  first_last_trips <- trips %>% 
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
  
  route <- trips %>%
    group_by(route_id, period, direction_id,  period_length, span_gtfs_hrs  #trip_headsign,
    ) %>%
    # Calculate trips in period and service hours in period
    summarise(num_trips = n()) %>%
    # Calculate service hour per trip and adjust minutes of operation in period based on the
    # adjustment values calculated earlier
    # Calcualte average headways based on GTFS processed data   
    mutate(avg_headway_mins = round(period_length*60/num_trips, 0)) %>% 
    ungroup() 
  
  
  weekday_trips <- gtfs$routes %>% 
    
    select(route_id, route_short_name ) %>%
    
    # filter(route_id == "223-1988") %>% 
    left_join(route) %>% 
    mutate(day_type = day_type , 
           period = stringr::str_remove_all(period, "[:digit:]"), 
           period = stringr::str_remove_all(period, '[\\.,]')) %>% 
    drop_na()
}
