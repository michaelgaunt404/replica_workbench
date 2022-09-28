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
  
  #get network
  network = "data/gis/network_reduced_link_types_spatial_portland.rds" %>%  
    here() %>%  
    read_rds() 
  
  #reduce and combine data
  data_queried_trips_comb = list(data21
                                 ,data19) %>%  
    map(~reduce(.x, bind_rows)) %>% 
    reduce(bind_rows) %>% 
    mutate(across(c(origin_cluster, destination_cluster), as.numeric))
  
  #this should be a an input or at least and option to do so 
  data_queried_trips_comb_hc = data_queried_trips_comb %>%  
    filter(vehicle_type == "HEAVY_COMMERCIAL") 
  
  #aggregation for OD matrix
  trips_agg_od = data_queried_trips_comb_hc %>%  
    count(origin_cluster, destination_cluster, dataset) %>%  
    data.frame()
  
  cluster_pair_xwalk = trips_agg_od %>%  
    select(ends_with("cluster")) %>%  
    unique() %>%  
    mutate(index = row_number()
           ,) %>% 
    pivot_longer(cols = ends_with("cluster")) %>%  
    mutate(value_duplicate = value) %>%  
    arrange(index, value_duplicate) %>% 
    group_by(index) %>% 
    mutate(cluster_pair = str_glue("{value_duplicate}_{lead(value_duplicate)}")
           ,cluster_pair = case_when(str_detect(cluster_pair, "_NA")~lag(cluster_pair),T~cluster_pair)) %>%  
    ungroup() %>% 
    arrange(index ) %>%  
    select(!value_duplicate) %>% 
    pivot_wider(names_from = "name", values_from = "value") %>%  
    select(contains("cluster"))
  
  link_unnest = data_queried_trips_comb %>%  
    filter(vehicle_type == "HEAVY_COMMERCIAL") %>% 
    select(activity_id, network_link_ids, origin_cluster, destination_cluster, dataset) %>%  
    unnest(cols = network_link_ids)
  
  #create network data
  network_agg_od = link_unnest %>%
    # sample_n(1000) %>% 
    count(origin_cluster, destination_cluster, network_link_ids, dataset) %>%  
    merge(cluster_pair_xwalk) %>%  
    group_by(origin_cluster, destination_cluster) %>%  
    mutate(pr_od = percent_rank(n)
           ,cp_od = (n/sum(n))*1000) %>%  
    ungroup() %>%  
    group_by(cluster_pair) %>%  
    mutate(pr_od_pair = percent_rank(n)
           ,cp_od_pair = (n/sum(n))*1000
           ,cp_pr_od_pair = percent_rank(cp_od_pair)) %>%  
    ungroup()
  
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
  
  
}

##Mapping Functions=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

make_network_maps = function(data, cluster_object){
  # data = tar_read("data_processed_queries")
  # cluster_object = tar_read("data_manual_cluster")
  
  #global vars/objects
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  cluster = cluster_object %>%  
    mutate(index_cluster = as.factor(index_cluster))
  
  index_cluster_pair = c("1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_8", "1_9", "1_10"  
    ,"2_3", "2_4", "2_6", "2_7", "2_8", "2_9", "2_10"  
    ,"3_4", "3_5", "3_6", "3_7", "3_8", "3_9", "3_10", "3_11", "3_12"
    ,"4_5", "4_6", "4_7", "4_8", "4_9","4_10", "4_11", "4_12"  
    ,"5_6", "5_7", "5_8", "5_9", "5_10", "5_11", "5_12"
    ,"6_7", "6_8", "6_9", "6_10", "6_11", "6_12"  
    ,"7_8", "7_9","7_10", "7_11", "7_12"  
    ,"8_9","8_10", "8_11", "8_12"
    ,"9_10", "9_11", "9_12"
    ,"10_11", "10_12"
    ,"11_12" )
  
  
  
  #OD map
  
  ##prepping data----
  
  network_agg_od = data$network_agg_od %>%  
    mutate(across(c(origin_cluster, destination_cluster), as.numeric)
           ,cluster_pair = fct_relevel(cluster_pair
                                       ,index_cluster_pair)
           ,cp_pr_od_pair = dgt2(cp_pr_od_pair))
  
  # levels(network_agg_od$cluster_pair)
  
  network_agg_od_mp = network_agg_od %>%  
    # sample_n(20000) %>%
    select(origin_cluster, destination_cluster, cluster_pair, highway
           # ,n, pr_od, pr_od_pair, dataset
           ,n, pr_od, cp_od, pr_od_pair, cp_od_pair, cp_pr_od_pair, dataset
    ) %>%  
    rename(highway_class = highway, count = n, dataset = dataset
           # ,precent_rank_od = pr_od, precent_rank_od_pair = pr_od_pair
           ,precent_rank_od = pr_od, count_percent = cp_od
           ,precent_rank_od_pair = pr_od_pair, count_percent_od = cp_od_pair, count_percent_od_pair = cp_pr_od_pair
    ) %>%  
    mutate(dataset = gsub(".*(2019|2021)", "\\1", dataset)
           ,across(starts_with("precent"), dgt2)
           ,label = str_glue(
             "{cluster_pair} from {origin_cluster} <br> {count} - {precent_rank_od} - {precent_rank_od_pair}")
           ,text = str_glue(
             "Origin Pair: {cluster_pair} \n Origin Cluster: {origin_cluster} \n
           Truck Count: {count} \n Percent Rank: {precent_rank_od} \n Percent Rank for OD Pair: {precent_rank_od_pair}")) %>% 
    st_true_midpoint()
  
  network_agg_od_mp_sm = network_agg_od_mp %>%  
    filter(count> 3) 
  
  network_agg_od_mp_sd = SharedData$new(network_agg_od_mp_sm)
  
  ##make map----
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,network_agg_od_mp_sm$count
    ,reverse = T)
  
  pal_clusters = colorFactor(
    rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                             length(levels(cluster$index_cluster)),
    )),
    cluster$index_cluster)
  
  # map_network_od = 
  bscols(widths = c(3, 9)
         ,list(
           bscols(
             widths = c(6, 6)
             ,filter_select("dataset", "Choose Dataset: "
                            ,network_agg_od_mp_sd, ~dataset)
             ,filter_select("cluster_pair", "Choose Origin Pairs:"
                            ,network_agg_od_mp_sd, ~cluster_pair))
           ,bscols(
             widths = c(6, 6)
             ,filter_select("origin_cluster", "Origin Cluster:"
                            ,network_agg_od_mp_sd, ~origin_cluster)
             ,filter_select("destination_cluster", "Destination Cluster:"
                            ,network_agg_od_mp_sd, ~destination_cluster))
           ,HTML("Truck Count Filters") %>%  strong()
           ,shiny::hr()
           # ,shiny::hr()
           ,filter_slider("count", "Link Count Slider:"
                          ,network_agg_od_mp_sd, ~count)
           ,HTML("Truck Count Percent Rank per OD")
           ,shiny::hr()
           ,filter_slider("precent_rank_od", "Unidirectional:"
                          ,network_agg_od_mp_sd, ~precent_rank_od)
           ,filter_slider("precent_rank_od_pair", "Bidirectional:"
                          ,network_agg_od_mp_sd, ~precent_rank_od_pair)
           ,HTML("Link Percent of Total OD Pair Volume")
           ,shiny::hr()
           ,filter_slider("count_percent_od_pair", "Percent Rank (link count/total OD count):"
                          ,network_agg_od_mp_sd, ~count_percent_od_pair)
           # ,filter_slider("count_percent_od", "count_percent_od:"
           #                ,network_agg_od_mp_sd, ~count_percent_od)
         )
         ,leaflet(height = 700) %>% 
           addTiles(group = "OSM (default)") %>% 
           # leaflet_default_tiles() %>% 
           addCircleMarkers(data = network_agg_od_mp_sd
                            ,fillColor = ~pal_centroids_od(network_agg_od_mp_sm$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
                                                        select(!c(text, label)))
                            ,label =
                              network_agg_od_mp_sm$label %>%
                              map(htmltools::HTML)
                            ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>% 
           addPolygons(data = cluster
                       ,fillColor = ~pal_clusters(cluster$index_cluster)
                       ,opacity = .8
                       ,fillOpacity = .4
                       ,weight = 1
                       ,group = "OD Clusters"
                       ,label = cluster$index_cluster) %>%
           #layer control----
         addLayersControl(
           baseGroups = "OSM (default)", #leaflet_default_tiles_index,
           overlayGroups =
             c("Network Links (mid-points)", "OD Clusters"),
           options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = network_agg_od_mp$count) %>%  
           addLegend(
             position = "bottomleft"
             ,title = HTML("OD Clusters")
             ,group = "OD Clusters"
             ,pal = pal_clusters
             ,opacity = 0.7
             ,values = cluster$index_cluster)
         
         
  )
  
  #total map----
  
  ##prepping data
  # network_agg = data$network_agg
  # 
  # network_agg_mp = network_agg %>%  
  #   st_true_midpoint()
  # 
  # network_agg_mp_sd = SharedData$new(network_agg_mp)
  
  ##make map
  
  # pal_centroids_tot = colorNumeric(
  #   palette = "magma"
  #   ,network_agg_mp$count_tot
  #   ,reverse = T)
  # 
  # map_network = bscols(widths = c(3, 9)
  #                      ,list(
  #                        filter_select("dataset2", "Choose Dataset: "
  #                                      ,network_agg_mp_sd, ~dataset)
  #                        ,filter_slider("n2", "Link Count Slider:"
  #                                       ,network_agg_mp_sd, ~count_tot)
  #                      )
  #                      ,leaflet(height = 800) %>% 
  #                        leaflet_default_tiles() %>% 
  #                        addCircleMarkers(data = network_agg_mp_sd
  #                                         ,fillColor = ~pal_centroids_tot(network_agg_mp$count_tot)
  #                                         ,color = "black"
  #                                         ,opacity = .8
  #                                         ,fillOpacity  = .5
  #                                         ,weight = 1
  #                                         ,radius = 5
  #                                         ,group = "Network Links (mid-points)"
  #                                         ,label = network_agg_mp$count_tot
  #                                         ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>% 
  #                        addPolygons(data = cluster
  #                                    ,color = "black"
  #                                    ,opacity = .8
  #                                    ,fillOpacity = .1
  #                                    ,weight = 1
  #                                    ,group = "OD Clusters"
  #                                    ,label = cluster$index_cluster) %>%  
  #                        ##layer control----
  #                      addLayersControl(
  #                        baseGroups = leaflet_default_tiles_index,
  #                        overlayGroups =
  #                          c("Network Links (mid-points)", "OD Clusters"),
  #                        options = layersControlOptions(collapsed = F, sortLayers = F)) %>%  
  #                        setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
  #                        addMouseCoordinates() %>% 
  #                        ###legends----
  #                      addLegend(
  #                        position = "bottomleft"
  #                        ,title = HTML("Link Truck Counts")
  #                        ,group = "Network Links (mid-points)"
  #                        ,pal = pal_centroids_tot
  #                        ,opacity = 0.7
  #                        ,values = network_agg_mp$count_tot)     
  # )
  # 
  # list(map_network_od, map_network)
  
}

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================











                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 