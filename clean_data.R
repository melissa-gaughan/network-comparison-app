
#library(shinydashboardPlus)
library(tidyverse)

library(sf)

library(here)
# LOAD IN DATA ####
block_groups <- sf::read_sf(here::here("input", "2020_block_groups", "blkgrp20_shore.shp"))





block_group_centroids <- block_groups %>% 
  st_centroid() %>% 
  st_transform(4326)

block_groups <- block_groups %>% 
  st_transform(4326) %>% 
  rmapshaper::ms_simplify()

load(file =here::here( "input","route_and_block_group_equity_data.RDS"))

#route shapefiles ####
proposed_network <- sf::read_sf("input/Lynnwood_Link_Phase_2_Proposal/planner_var_shape.shp") %>% 
  rename(route_short_name = VAR_ROUTE, 
         variant = VAR_IDENT, 
         direction = VAR_DIREC, 
         description = VAR_DESCR) %>% 
  st_set_crs(2926) %>% 
  st_transform(4326) %>% 
  rmapshaper::ms_simplify(keep = .2)

baseline_network <- sf::read_sf("input/Lynnwood_Link_Phase_2_Baseline/planner_var_shape.shp") %>% 
  rename(route_short_name = VAR_ROUTE, 
         variant = VAR_IDENT, 
         direction = VAR_DIREC, 
         description = VAR_DESCR)%>% 
  st_set_crs(2926) %>% 
  st_transform(4326)%>% 
  rmapshaper::ms_simplify(keep = .2)
# block group metrics #####
network_data <- read_csv(here::here( "input","block_group_trips_and_capacity_summary.csv")) %>% 
  select(-c(Name:`Acs Year`)) %>% 
  pivot_longer(cols = !c(Geoid, `Analysis Period`, `Day Type`, `Routes in Geo Baseline`, 
                         `Routes in Geo Proposed`), 
               names_to = "Metric", 
               values_to = "Value")


network_data_details <- read_csv(here::here( "input","route_level_trips_and_capacity_summary.csv")) %>% 
  mutate(across(.cols = -c(Geoid ,`Percent Change in Trips`), .fns = as.character))

block_group_need_scores<- block_group_need_scores %>% 
  mutate(Geoid = as.numeric(geoid))

epa_hatch <- block_groups %>%
  left_join(block_group_need_scores) %>% 
  mutate(equity_priority_area = ifelse(final_score >= 4, TRUE, FALSE)) %>%
  filter(equity_priority_area) %>% # filter for tracts that I want to receive a cross-hatch
  # sample_n(40) %>%
  HatchedPolygons::hatched.SpatialPolygons(density = 500, angle = 45) %>%
  st_transform(4326) %>%
  mutate(col = 1) 