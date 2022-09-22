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
library(sf)

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
org_agg_by_top_dest_sf = read_rds(here("data/geo_tracts_analysis_20220810/org_agg_by_top_dest_sf.rds"))

od_basic_agg_byDest_sf = read_rds(here("data/geo_tracts_analysis_20220810/od_basic_agg_byDest_sf.rds"))

dest_agg_by_top_org_sf = read_rds(here("data/geo_tracts_analysis_20220810/dest_agg_by_top_org_sf.rds"))

od_basic_agg_byOrigin_sf = read_rds(here("data/geo_tracts_analysis_20220810/od_basic_agg_byOrigin_sf.rds"))

selected_geoms = read_rds(here("data/geo_tracts_analysis_20220810/selected_geoms_4.RDS"))

block_groups_raw = c("WA","OR") %>%
  map(~tigris::block_groups(state = .x, year = 2010) %>%
        st_transform(crs = 4326) %>%
        mutate(area_km2 = ALAND10/(1000^2))) %>%
  reduce(rbind) %>% 
  rename(GEOID = GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID))


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



function(states, year){
  block_groups_raw =  %>%
    map(~tigris::block_groups(state = .x, year = 2010) %>%
          st_transform(crs = 4326) %>%
          mutate(area_km2 = ALAND10/(1000^2))) %>%
    reduce(rbind) %>% 
    rename(GEOID = GEOID10) %>%
    mutate(GEOID = as.numeric(GEOID))
}

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































