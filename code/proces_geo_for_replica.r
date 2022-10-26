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
`%not_in%` = Negate(`%in%`)

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##replica import raw network====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

replica_bulk_network_south_central = here("data/replica_networks/replica-bulk-network-south-central/network.gpkg") %>% 
  read_sf()

##create_new_dir================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_dir_relative = Sys.Date() %>%  
  str_remove_all("[:punct:]") %>% 
  paste0("data/geo_", .) 

new_dir_relative %>% 
  here::here() %>% 
  dir.create()

new_dir_relative %>% 
  here::here("geo") %>% 
  dir.create()

##get all base shapes===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
counties = tigris::counties(state = c("OR", "WA")) %>% 
  st_transform(4326)

states = tigris::states() %>%  
  st_transform(crs = 4326) %>%  
  filter(NAME %in% c("Washington", "Oregon")) %>%  
  mutate(id = STATEFP,type = "county") %>%  
  st_transform(4326) %>% 
  select(id, type, NAME)

tracts = c("OR", "WA") %>%  
  map(~tigris::tracts(state = .x, year = 2021) %>%  
        st_transform(crs = 4326) %>%  
        select(STATEFP:NAME) %>% 
        mutate(type = "tract"))  %>%  
  reduce(rbind)
  
  

##make construction shapes=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

index_sa = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  pull(COUNTYFP)

tracts_sa = tracts %>%  
  mutate(flag_sa = case_when(COUNTYFP %in% index_sa & 
                               STATEFP == 41 ~"study area"
                             ,T~"external area"))   

combined_replica_geos = counties %>%  
  filter(NAME %not_in% c("Multnomah", "Washington", "Clackamas")) %>%  
  mutate(flag_sa = "external area"
         ,type = "county") %>% 
  select(STATEFP, COUNTYFP, GEOID, NAME, type, flag_sa) %>% 
  rbind(.,   tracts_sa %>%  
          mutate(type = "tracts") %>% 
          select(STATEFP, COUNTYFP, GEOID, NAME, type, flag_sa) %>%   
          filter(flag_sa == "study area") 
        ) %>%  
  rename(name = "NAME"
         ,id = "GEOID")  %>%  
  mutate(name = paste0(name, "_", id))



##make construction shapes=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combined_replica_geos %>% 
  filter(flag_sa != "external area") %>%  
  st_write(here::here(new_dir_relative, "geo", "internal_tracts_20220810.shp"))

combined_replica_geos %>% 
  filter(flag_sa == "external area") %>%  
  st_write(here::here(new_dir_relative, "geo", "external_counties_20220810.shp"))

combined_replica_geos %>% 
  # filter(flag_sa == "external area") %>%  
  st_write(here::here(new_dir_relative, "geo", "all_polys_20220810.shp"))


##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































