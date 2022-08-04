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
library(tidyverse) #- this one has ALL functions
library(gauntlet)
# library(tigris)
library(sf)
library(mapview)
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
`%notin%` = Negate(`%in%`)

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

network = here("data/replica_20220801/gis/network.gpkg") %>% 
  read_sf() %>% 
  st_transform(4326)

mapview(network)

network_sa = network %>%
  st_filter(zip_codes_sa)

network_sa %>%  
  head()

network_sa$highway %>%  
  unique()

network_sa %>%  
  filter(highway %in% c("motorway")) %>%  
  mapview()

portland_buffer = data.frame(lon = -122.67720, lat = 45.5) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  
  quick_buffer(radius = 10000)

network_pb = network_sa %>%  
  st_filter(portland_buffer)



network_pb %>%  
  filter(highway %notin% c("footway", "residential", "tertiary", "path"
                           ,"service", "track", "pedestrian", "cycleway", "other"
                           ,"living_street", "platform", "unclassified"
                           ,"motorway_link", "primary_link", "trunk_link", "tertiary_link", "secondary_link")) %>% 
  filter(highway != "secondary") %>% 
  mapview(zcol = "highway")

network_cleaned = network %>%  
  filter(highway %notin% c("footway", "residential", "tertiary", "path"
                           ,"service", "track", "pedestrian", "cycleway", "other"
                           ,"living_street", "platform", "unclassified"
                           ,"motorway_link", "primary_link", "trunk_link", "tertiary_link", "secondary_link")) %>% 
  filter(highway != "secondary") 


network_cleaned %>%  
  saveRDS(here("data/replica_20220801/gis/network_cleaned.RDS"))

network_cleaned %>% 
  sf::write_sf(here("data/replica_20220801/gis/network_cleaned.gpkg"))


# sample_n(network_cleaned, here("data/replica_20220801/gis/network_sa_cleaned.RDS"))
  


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































