#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(tigris)
library(sf)
library(mapview)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

primary_secondary_roads = tigris::primary_secondary_roads(state = "OR") %>%  
  st_transform(crs = 4326)

counties = tigris::counties(state = "OR")
index_remove = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  pull(COUNTYFP)

states_reduced = tigris::states() %>%  
  st_transform(crs = 4326) %>%  
  filter(NAME == "Washington") %>%  
  mutate(id = STATEFP,type = "county") %>%  
  select(id, type)

tracts_reduced = tigris::tracts(state = "OR", year = 2021) %>%  
  st_transform(crs = 4326) %>%  
  filter(COUNTYFP %in% index_remove) %>% 
  mutate(id = GEOID,type = "tract") %>%  
  select(id, type)

counties_reduced = counties %>%  
  filter(COUNTYFP %notin% index_remove) %>%  
  st_transform(crs = 4326) %>%  
  mutate(id = COUNTYFP,type = "county") %>%  
  select(id, type)

  mapview::mapview(states_reduced) + 
    mapview::mapview(tracts_reduced) + 
    mapview(counties_reduced)
  
 replica_data = rbind(states_reduced, 
        tracts_reduced,
        counties_reduced) 
 
 replica_data %>% 
   st_write(here::here("data/gis/replica_data_portland.shp"))


##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































