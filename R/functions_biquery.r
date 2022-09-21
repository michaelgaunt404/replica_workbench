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
# library(tidyverse)
# library(sf)
# library(tigris)
# library(gauntlet)

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

get_manual_cluster = function(file){
  read_rds(file)
}

get_block_groups = function(states = c("WA", "OR"), year = 2010){
  #gets block groups for states and year - only works for 2010 
  block_groups_raw = states %>%
    map(~tigris::block_groups(state = .x, year = year) %>%
          st_transform(crs = 4326) %>%
          mutate(area_km2 = ALAND10/(1000^2))) %>%
    reduce(rbind) %>% 
    rename(GEOID = GEOID10) %>%
    mutate(GEOID = as.numeric(GEOID))
}


get_block_groups_manual = function(){
  #gets subset of OD block groups given threshold 
  org_agg_by_top_dest_sf = read_rds(here("data/geo_tracts_analysis_20220810/org_agg_by_top_dest_sf.rds"))
  
  od_basic_agg_byDest_sf = read_rds(here("data/geo_tracts_analysis_20220810/od_basic_agg_byDest_sf.rds"))
  
  dest_agg_by_top_org_sf = read_rds(here("data/geo_tracts_analysis_20220810/dest_agg_by_top_org_sf.rds"))
  
  od_basic_agg_byOrigin_sf = read_rds(here("data/geo_tracts_analysis_20220810/od_basic_agg_byOrigin_sf.rds"))
  
  data_object = rbind(
    od_basic_agg_byDest_sf %>%  
      mutate(source = "Top Destinations") %>%  
      select(GEOID, contains("count"), source)
    ,dest_agg_by_top_org_sf %>%  
      mutate(source = "Top Destinations by Top Gen.") %>% 
      select(GEOID, contains("count"), source)
    ,od_basic_agg_byOrigin_sf %>%  
      mutate(source = "Top Generators") %>% 
      select(GEOID, contains("count"), source)
    ,org_agg_by_top_dest_sf %>%  
      mutate(source = "Top Generators by Top Dest.") %>% 
      select(GEOID, contains("count"), source)
  ) %>% 
    filter(count_adj_area_rnk_bin >= .9) 
  
  data_object %>%  
    select(GEOID) %>%  
    unique()
}

data_bg_clust = function(block_groups, clusters){
  bg_clust = block_groups %>%
    st_join(clusters$all %>%  
              mutate(index_cluster = dplyr::row_number() ))
  
  bg_clust %>%
    filter(!is.na(X_leaflet_id)) 
  
}

query_trip_info = function(data){
  
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "replica-customer",
    dataset = "northwest.northwest_2019_Q4_saturday_trip"
  )
  
  data_bg_clust_df = data %>%  
    st_drop_geometry()
  
  index_cluster = data_bg_clust_df %>%  
    pull(index_cluster)
  
  query_combinations = crossing(origin = index_cluster
                                ,destination = index_cluster) %>%  
    filter(origin != destination)
  
  
  query_strings = list(query_combinations$origin #%>% sample(2)
                       ,query_combinations$destination #%>% sample(2)
  ) %>% 
    pmap(~{
      
      origin_bg = data_bg_clust_df %>%  
        filter(index_cluster == .x) %>%  
        pull(GEOID) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ")
      
      destination_bg = data_bg_clust_df %>%  
        filter(index_cluster == .y) %>%  
        pull(GEOID) %>%  
        paste0("'", ., "'") %>% 
        paste(collapse = ", ")
      
      query_string = str_glue("SELECT * FROM
  `northwest.northwest_2019_Q4_saturday_trip`
 WHERE origin_bgrp IN ({origin_bg}) AND destination_bgrp IN ({destination_bg}) AND mode = 'COMMERCIAL' --LIMIT 20
                              ;")
      
      queried_data =  dbGetQuery(con, query_string) %>%  
        mutate(origin_cluster = .x
               ,destination_cluster = .y)
      
    })
}

make_spatial_networks = function(data){
  
  network = "data/gis/network_reduced_link_types_spatial_portland.rds" %>%  
    here() %>%  
    read_rds() 
  
  data_queried_trips_comb = data %>%  
    reduce(bind_rows)
  
  link_unnest = data_queried_trips_comb %>%  
    filter(vehicle_type == "HEAVY_COMMERCIAL") %>% 
    select(network_link_ids, origin_cluster, destination_cluster) %>%  
    unnest(cols = network_link_ids)
  
  network_agg_od = link_unnest %>%  
    count(origin_cluster, destination_cluster, network_link_ids)
  
  network_agg = link_unnest %>%  
    count(network_link_ids) 
  
  list(network_agg_od = network_agg_od
       ,network_agg = network_agg) %>%  
    map(~merge(network, .x
               ,by.x = "stableEdgeId", by.y = "network_link_ids", all.y = T) %>%  
          mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
          filter(sf_geom_typ == "LINESTRING"))
  
}

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































