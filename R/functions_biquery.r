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


#BigQuery Data Functions========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_manual_cluster = function(file){
  read_rds(file) %>% 
    .$all %>% 
    mutate(index_cluster = row_number())
}

get_block_groups = function(states = c("WA", "OR"), year = 2010){
  #gets block groups for states and year - only works for 2010 
  #uses counties to get FIPs and make study area flag 
  block_groups_raw = states %>%
    map(~tigris::block_groups(state = .x, year = year) %>%
          st_transform(crs = 4326) %>%
          mutate(area_km2 = ALAND10/(1000^2))) %>%
    reduce(rbind) %>% 
    rename(GEOID = GEOID10) %>%
    mutate(GEOID = as.numeric(GEOID))
  
  counties = tigris::counties(state = c("OR", "WA")) %>% 
    st_transform(4326)
  
  index_sa = counties %>%  
    filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
    pull(COUNTYFP)
  
  block_groups_with_internal_flag = block_groups_raw %>%
    mutate(flag_sa = case_when(COUNTYFP %in% index_sa & 
                                 STATEFP == 41~"internal", T~"external"))
}

get_block_groups_selected_od = function(){
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
  # block_groups = tar_read("data_block_group")
  # clusters = tar_read("data_manual_cluster")
  
  bg_clust = block_groups %>%
    st_join(clusters)
  
  bg_clust %>%
    filter(!is.na(X_leaflet_id)) 
  
}

query_trip_info = function(data_bgclust, data_bgsa, schema_table){
  
  # schema_table = "northwest.northwest_2019_Q4_thursday_trip"
  # data_bgclust = tar_read("data_bg_clust")
  # data_bgsa = tar_read("data_block_groups_sa")

  #connect to google
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "replica-customer",
    dataset = schema_table
  )
  
  #query prep
  data_bg_clust_df = data_bgclust %>%  
    st_drop_geometry()
  
  data_comb = data_bgsa %>%  
    select(GEOID, flag_sa) %>%
    merge(data_bg_clust_df, all = T) %>%  
    mutate(index_cluster = case_when(!is.na(index_cluster)~as.character(index_cluster)
                     ,T~"External to Study Area")) %>% 
    filter(!(flag_sa == "internal" & is.na(X_leaflet_id))) 
  
  index_cluster = data_comb %>%  
    filter(index_cluster != "External to Study Area") %>% 
    pull(index_cluster) %>%  
    unique() %>%  
    sort()
    
  query_combinations = crossing(origin = index_cluster
                                ,destination = index_cluster) %>%  
    filter(origin != destination) 
  
  # %>%  
  #   filter((origin == "1" | origin == "External to Study Area") & 
  #            (destination == "1" | destination == "External to Study Area"))
  # x = query_combinations$origin[1]
  # y = query_combinations$destination[1]
  
  
  query_strings = list(query_combinations$origin #%>% sample(2)
                       ,query_combinations$destination #%>% sample(2)
  ) %>% 
    pmap(~{
      
      origin_bg = data_comb %>%  
        filter(index_cluster == .x) %>%  
        pull(GEOID) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ")
      
      destination_bg = data_comb %>%  
        filter(index_cluster == .y) %>%  
        pull(GEOID) %>%  
        paste0("'", ., "'") %>% 
        paste(collapse = ", ")
      
      query_string = str_glue("SELECT * FROM
  `{schema_table}`
 WHERE origin_bgrp IN ({origin_bg}) AND destination_bgrp IN ({destination_bg}) AND mode = 'COMMERCIAL' --LIMIT 20
                              ;")
      
      queried_data =  dbGetQuery(con, query_string) %>%  
        mutate(origin_cluster = .x
               ,destination_cluster = .y
               ,dataset = schema_table)
      
    })
}

make_spatial_networks = function(data21, data19){
  # data = tar_read("data_queried_trips")
  # data21 = tar_read("data_queried_trips_2021")
  # data19 = tar_read("data_queried_trips_2019")
  
  network = "data/gis/network_reduced_link_types_spatial_portland.rds" %>%  
    here() %>%  
    read_rds() 
  
  data_queried_trips_comb = list(data21
                                 ,data19) %>%  
    map(~reduce(.x, bind_rows)) %>% 
    reduce(bind_rows)
  
  data_queried_trips_comb_hc = data_queried_trips_comb %>%  
    filter(vehicle_type == "HEAVY_COMMERCIAL") 
  
  trips_agg_od = data_queried_trips_comb_hc %>%  
    count(origin_cluster, destination_cluster, dataset) %>%  
    data.frame()
  # %>% 
  # ggplot() + 
  #   geom_tile(aes(origin_cluster, destination_cluster, fill = n)) + 
  #   facet_grid(rows = vars(dataset))
  
  link_unnest = data_queried_trips_comb %>%  
    filter(vehicle_type == "HEAVY_COMMERCIAL") %>% 
    select(activity_id, network_link_ids, origin_cluster, destination_cluster, dataset) %>%  
    unnest(cols = network_link_ids)
  
  # link_unnest %>%  
  #   count(activity_id, network_link_ids) %>%  
  #   filter(n != 1)
  
  network_agg_od = link_unnest %>%  
    count(origin_cluster, destination_cluster, network_link_ids, dataset)
  
  # network_agg_od %>%  
  #   filter(n>2) 
  
  # network_agg = link_unnest %>%  
  #   count(network_link_ids) 
  
  network_agg = link_unnest %>% 
    mutate(count = 1) %>% 
    count_percent_zscore(grp_c = c(dataset, network_link_ids, origin_cluster)
                         ,grp_p = c(dataset, network_link_ids)
                         ,col = count, rnd = 2) %>%  
    group_by(dataset, network_link_ids) %>%  
    mutate(count_tot = sum(count)) %>%  
    arrange(origin_cluster) %>% 
    select(!count) %>% 
    mutate(origin_cluster = str_glue("% from Cluster: {origin_cluster}")) %>% 
    pivot_wider(names_from = origin_cluster
                ,values_from = percent)
  
  networks = list(network_agg_od = network_agg_od
       ,network_agg = network_agg) %>%  
    map(~merge(network, .x
               ,by.x = "stableEdgeId", by.y = "network_link_ids", all.y = T) %>%  
          mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
          filter(sf_geom_typ == "LINESTRING")) 
  
  networks %>%  
    c(trips_agg_od = list(trips_agg_od))
  
}

##Mapping Functions=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

make_network_maps = function(data, cluster_object){
  # data = tar_read("data_processed_queries")
  # cluster_object = tar_read("data_manual_cluster")
  
  #global vars/objects 
  
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  cluster = cluster_object
  
  #OD map
  
  ##prepping data----
  
  network_agg_od = data$network_agg_od %>%  
    mutate(across(c(origin_cluster, destination_cluster), as.numeric))

  network_agg_od_mp = network_agg_od %>%  
    st_true_midpoint()
  
  network_agg_od_mp_sd = SharedData$new(network_agg_od_mp)
  
  ##make map----
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,network_agg_od_mp$n
    ,reverse = T)
  
  map_network_od = bscols(widths = c(3, 9)
                          ,list(
                            crosstalk::filter_select("dataset", "Choose Dataset: "
                                                     ,network_agg_od_mp_sd, ~dataset)
                            ,crosstalk::filter_select("origin_cluster", "Choose Origin Cluster:"
                                                      ,network_agg_od_mp_sd, ~origin_cluster)
                            ,crosstalk::filter_select("destination_cluster", "Choose Destination Cluster:"
                                                      ,network_agg_od_mp_sd, ~destination_cluster)
                            ,crosstalk::filter_slider("n", "Link Count Slider:"
                                                      ,network_agg_od_mp_sd, ~n)
                          )
                          ,leaflet(height = 800) %>% 
                            leaflet_default_tiles() %>% 
                            addCircleMarkers(data = network_agg_od_mp_sd
                                             ,fillColor = ~pal_centroids_od(network_agg_od_mp$n)
                                             ,color = "black"
                                             ,opacity = .8
                                             ,fillOpacity  = .5
                                             ,weight = 1
                                             ,radius = 5
                                             ,group = "Network Links (mid-points)"
                                             ,label = network_agg_od_mp$n
                                             ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>% 
                            addPolygons(data = cluster
                                        ,color = "black"
                                        ,opacity = .8
                                        ,fillOpacity = .1
                                        ,weight = 1
                                        ,group = "OD Clusters"
                                        ,label = cluster$index_cluster) %>%  
                            ##layer control----
                          addLayersControl(
                            baseGroups = leaflet_default_tiles_index,
                            overlayGroups =
                              c("Network Links (mid-points)", "OD Clusters"),
                            options = layersControlOptions(collapsed = F, sortLayers = F)) %>%  
                            setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
                            leafem::addMouseCoordinates() %>%  
                            ###legends----
                          addLegend(
                            position = "bottomleft"
                            ,title = HTML("Link Truck Counts")
                            ,group = "Network Links (mid-points)"
                            ,pal = pal_centroids_od
                            ,opacity = 0.7
                            ,values = network_agg_od_mp$n)     
  )
  
  #total map----
  
  ##prepping data
  
  network_agg = data$network_agg
  
  network_agg_mp = network_agg %>%  
    st_true_midpoint()
  
  network_agg_mp_sd = SharedData$new(network_agg_mp)
  
  ##make map
  
  pal_centroids_tot = colorNumeric(
    palette = "magma"
    ,network_agg_mp$count_tot
    ,reverse = T)
  
  map_network = bscols(widths = c(3, 9)
                       ,list(
                         crosstalk::filter_select("dataset2", "Choose Dataset: "
                                                  ,network_agg_mp_sd, ~dataset)
                         ,crosstalk::filter_slider("n2", "Link Count Slider:"
                                                   ,network_agg_mp_sd, ~count_tot)
                       )
                       ,leaflet(height = 800) %>% 
                         leaflet_default_tiles() %>% 
                         addCircleMarkers(data = network_agg_mp_sd
                                          ,fillColor = ~pal_centroids_tot(network_agg_mp$count_tot)
                                          ,color = "black"
                                          ,opacity = .8
                                          ,fillOpacity  = .5
                                          ,weight = 1
                                          ,radius = 5
                                          ,group = "Network Links (mid-points)"
                                          ,label = network_agg_mp$count_tot
                                          ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>% 
                         addPolygons(data = cluster
                                     ,color = "black"
                                     ,opacity = .8
                                     ,fillOpacity = .1
                                     ,weight = 1
                                     ,group = "OD Clusters"
                                     ,label = cluster$index_cluster) %>%  
                         ##layer control----
                       addLayersControl(
                         baseGroups = leaflet_default_tiles_index,
                         overlayGroups =
                           c("Network Links (mid-points)", "OD Clusters"),
                         options = layersControlOptions(collapsed = F, sortLayers = F)) %>%  
                         setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
                         leafem::addMouseCoordinates() %>% 
                         ###legends----
                       addLegend(
                         position = "bottomleft"
                         ,title = HTML("Link Truck Counts")
                         ,group = "Network Links (mid-points)"
                         ,pal = pal_centroids_tot
                         ,opacity = 0.7
                         ,values = network_agg_mp$count_tot)     
  )
  
  list(map_network_od, map_network)
  
}

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































