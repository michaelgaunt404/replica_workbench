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

#functions======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data prep and query============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
query_poi_to_everything = function(data, db_connection, schema_table, limit = 25){
  
  limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(flag_poi == "poi") %>%  
    select(group, id) 
  
  message("Begin query..")
  queried_data = poly_query_df$group %>%  
    unique() %>%  
    map(~{
      
      message(.x)
      
      query_focus_id = poly_query_df %>%  
        filter(group == .x) %>%  
        pull(id) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ") 
      
      query_string = str_glue("SELECT * FROM
  `{schema_table}`
 WHERE start_taz IN ({query_focus_id}) AND mode = 'COMMERCIAL' {limit_query}")
      
      message(query_string)
      
      temp_data = dbGetQuery(db_connection, query_string)
      
      message(nrow(temp_data))
      
      temp_data %>% 
        mutate(queried_group = .x
               ,queried_poi = query_focus_id
               ,dataset = schema_table)
      
    })
  
  queried_data
}

query_poi_to_everything_safe = purrr::safely(query_poi_to_everything)

purrr_get_safe_results = function(object, type = "result"){
  object %>%
    map(~.x[[type]])
  }

query_replica = function(data, schema_table, limit = 50){
  # schema_table = "south_central.south_central_2019_Q4_thursday_trip"
  # schema_table = "wsp.south_central_2021_Q4_thursday_trip_taz"
  # data = read_rds(here("data", "memphis_req/processed_taz_for_query.rds"))
  # data = here("data/memphis_req/data_for_query"
  #             ,"split_taz_polys_pro_comb_20221031.shp") %>%
  #   read_sf()
  
  # browser()
  
  data_pro = read_sf(data)
  
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "replica-customer",
    dataset = schema_table
  )
  
  # message(con)
  
  queried_data =  query_poi_to_everything_safe(data = data_pro
                                               ,db_connection = con
                                               ,schema_table = schema_table
                                               ,limit = limit)  %>% 
   .[["result"]] %>%  
    reduce(bind_rows)
 
  queried_data
  
}

get_replica_network = function(query_data, schema_table, limit = 25
                               ,street_type_exclude = c("path", "other", "track", "service","pedestrian", "unclassified"
                                                        ,"living_street","cycleway","proposed ", "trunk",  "footway"
                                                        ,"construction ","abandoned ","platform")){
  street_type_exclude_pro = street_type_exclude %>%  
    paste0("'", .,"'", collapse = ",") 
  
  bbox = query_data %>%  
    sf::st_bbox()
  
  
  limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))
  
  query_string = str_glue("SELECT * FROM
  `{schema_table}`
 WHERE ((startLon > {bbox$xmin} AND startLon < {bbox$xmax}) AND
 (endLon > {bbox$xmin} AND endLon < {bbox$xmax})) AND
  ((startLat > {bbox$ymin} AND startLat < {bbox$ymax}) AND
  (endLat > {bbox$ymin} AND endLat < {bbox$ymax})) AND
  highway NOT IN ({street_type_exclude_pro}) {limit_query}")
  
  temp_query_gis = dbGetQuery(con, 
                              query_string)
  
  sf::st_as_sf(temp_query_gis, wkt = "geometry", crs = 4326) %>%  
    st_filter(query_data)
}

make_spatial_networks = function(network, data, query_poly, rm_self = T){
  # data = tar_read("data_queried_trips")
  # data21 = tar_read("data_queried_trips_2021")
  # data19 = tar_read("data_queried_trips_2019")
  # query_poly = data_gis
  # network = network_memphis
  
  message("Preping data...")
  
  query_poly_df = query_poly %>%  
    read_sf() %>% 
    st_drop_geometry() %>%  
    filter(flag_poi == "poi") %>%  
    rename(start_taz_group = group)
  
  network_pro = network %>%  
    read_sf() %>% 
    mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", ""))
  
  data_pro = data %>%  
    mutate(count = 1) %>% 
    select(c('dataset', 'activity_id', 'start_taz'
             ,'end_taz', 'vehicle_type', 'network_link_ids', 'count')) %>% 
    {if (rm_self) (.) %>%  filter(start_taz != end_taz) else .} %>% 
    merge(., query_poly_df %>%
            select(id, start_taz_group)  
          ,by.x = "start_taz", by.y = "id") 
  
  link_unnest = data_pro %>%  
    unnest(cols = network_link_ids) %>% 
    mutate(network_link_ids_trunc = str_trunc(network_link_ids, 14, "right", "")) 
  
  message("Perfroming aggregations...")
  
  #agg by network link
  agg_network_link = link_unnest %>%  
    count_percent_zscore(
      grp_c = c('dataset', 'network_link_ids_trunc')
      ,grp_p = c('dataset', 'network_link_ids_trunc')
      ,col = count)
  
  #agg by network link
  agg_network_link_type = link_unnest %>%  
    count_percent_zscore(
      grp_c = c('dataset', 'network_link_ids_trunc', 'vehicle_type')
      ,grp_p = c('dataset', 'network_link_ids_trunc')
      ,col = count)
  
  #agg by network link
  agg_network_link_type_origin = link_unnest %>%  
    count_percent_zscore(
      grp_c = c('dataset', 'start_taz', 'start_taz_group', 'network_link_ids_trunc', 'vehicle_type')
      ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'network_link_ids_trunc')
      ,col = count)
  
  #agg by network link
  # agg_od = link_unnest %>%  
  #   count_percent_zscore(
  #     grp_c = c('dataset', 'start_taz', 'start_taz_group', 'end_taz', 'vehicle_type')
  #     ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'end_taz')
  #     ,col = count)
  
  message("Merging networks...")
  
  network_merged = list(agg_network_link = agg_network_link
                        ,agg_network_link_type = agg_network_link_type
                        ,agg_network_link_type_origin = agg_network_link_type_origin
  ) %>%  
    map(~merge(network_pro 
               ,.x 
               ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_trunc", all = T) %>%  
          mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
          filter(sf_geom_typ == "LINESTRING")) 
  
  return(network_merged)
  
}



#map functions==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_network_map_anl = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
   
  query_poly = file_query_poly %>%  
    read_sf() %>%  
    mutate(across(c(starts_with("flag"), group), as.factor))
  
  net_anl = network_objects$agg_network_link %>%  
    filter(!is.na(count) & !is.na(startLon)) %>% 
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                     ,T~NA_character_)) %>% 
    mutate(label = str_glue("Network Link: {streetName} <br> Type: {highway} <br> Link Volume {count}"))
  
  net_anl_cntrd = net_anl %>%  
    gauntlet::st_true_midpoint()
  
  #global vars/objects
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  #OD map

  ##prepping data----

  net_anl_cntrd_sd = SharedData$new(net_anl_cntrd)

  ##make map----

  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_anl_cntrd$count
    ,reverse = T)

  pal_group = colorFactor(
    rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = 1,
                             length(levels(query_poly$flag_poi)),
    )),
    query_poly$flag_poi)

  bscols(widths = c(3, 9)
         ,list(
           filter_select("net_anl_cntrd_sd_dataset", "Choose Dataset:"
                         ,net_anl_cntrd_sd, ~dataset)
           ,filter_select("net_anl_cntrd_sd_flag_NHS", "Choose Specific Rdwy:"
                          ,net_anl_cntrd_sd, ~flag_NHS)
           ,bscols(
             widths = c(12)
             ,filter_select("net_anl_cntrd_sd_highway", "Choose Street Type:"
                            ,net_anl_cntrd_sd, ~highway)
           )
           ,HTML("Truck Count Filters") %>%  strong()
           ,shiny::hr()
           ,filter_slider("net_anl_cntrd_sd_count", "Trip Count:"
                          ,net_anl_cntrd_sd, ~count)
           ,filter_slider("net_anl_cntrd_sd_countlg10", "Trip Count (log10):"
                          ,net_anl_cntrd_sd, ~dgt2(log10(count)))
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           # leaflet_default_tiles() %>%
           addCircleMarkers(data = net_anl_cntrd_sd
                            ,fillColor = ~pal_centroids_od(net_anl_cntrd$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            # ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
                            #                             select(!c(text, label)))
                            ,label =
                              net_anl_cntrd$label %>%
                              map(htmltools::HTML)
                            # ,labelOptions = labelOptions(noHide = F, textOnly = F)
                            ) %>%
           addPolygons(data = query_poly
                       ,fillColor = ~pal_group(query_poly$flag_poi)
                       ,opacity = .4
                       ,fillOpacity = .4
                       ,weight = 1
                       ,group = "Origin Poly Groups"
                       ,label = query_poly$group) %>%
           #layer control----
         addLayersControl(
           baseGroups = "OSM (default)", #leaflet_default_tiles_index,
           overlayGroups =
             c("Network Links (mid-points)", "Origin Poly Groups"),
           options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           # setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net_anl_cntrd$count) %>% 
           addLegend(
             position = "bottomright"
             ,title = HTML("Poly Groups")
             ,group = "Origin Poly Groups"
             ,pal = pal_group
             ,opacity = 0.7
             ,values = query_poly$flag_poi)
         )

}

make_network_map_anlt = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
  
  query_poly = file_query_poly %>%  
    read_sf() %>%  
    mutate(across(c(starts_with("flag"), group), as.factor))
  
  net_anlt = network_objects$agg_network_link_type %>%  
    filter(!is.na(count) & !is.na(startLon)) %>% 
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                                ,T~NA_character_)) %>% 
    mutate(label = str_glue("Network Link: {streetName} <br> Type: {highway} <br> Vehicle: Type {vehicle_type} <br> Link Volume {count}"))
  
  net_anlt_cntrd = net_anlt %>%  
    gauntlet::st_true_midpoint()
  
  #global vars/objects
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  #OD map
  
  ##prepping data----
  
  net_anlt_cntrd_sd = SharedData$new(net_anlt_cntrd)
  
  ##make map----
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_anlt_cntrd$count
    ,reverse = T)
  
  pal_group = colorFactor(
    rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = 1,
                             length(levels(query_poly$flag_poi)),
    )),
    query_poly$flag_poi)
  
  bscols(widths = c(3, 9)
         ,list(
           filter_select("net_anlt_cntrd_sd_dataset", "Choose Dataset:"
                         ,net_anlt_cntrd_sd, ~dataset)
           ,filter_select("net_anlt_cntrd_sd_flag_NHS", "Choose Specific Rdwy:"
                          ,net_anlt_cntrd_sd, ~flag_NHS)
           ,bscols(
             widths = c(6, 6)
             ,filter_select("net_anlt_cntrd_sd_highway", "Choose Street Type:"
                            ,net_anlt_cntrd_sd, ~highway)
             ,filter_select("net_anlt_cntrd_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_anlt_cntrd_sd, ~vehicle_type)
           )
           ,HTML("Truck Count Filters") %>%  strong()
           ,shiny::hr()
           ,filter_slider("net_anlt_cntrd_sd_count", "Trip Count:"
                          ,net_anlt_cntrd_sd, ~count)
           ,filter_slider("net_anlt_cntrd_sd_countlg10", "Trip Count (log10):"
                          ,net_anlt_cntrd_sd, ~dgt2(log10(count)))
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           # leaflet_default_tiles() %>%
           addCircleMarkers(data = net_anlt_cntrd_sd
                            ,fillColor = ~pal_centroids_od(net_anlt_cntrd$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            # ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
                            #                             select(!c(text, label)))
                            ,label =
                              net_anlt_cntrd$label %>%
                              map(htmltools::HTML)
                            # ,labelOptions = labelOptions(noHide = F, textOnly = F)
           ) %>%
           addPolygons(data = query_poly
                       ,fillColor = ~pal_group(query_poly$flag_poi)
                       ,opacity = .4
                       ,fillOpacity = .4
                       ,weight = 1
                       ,group = "Origin Poly Groups"
                       ,label = query_poly$group) %>%
           #layer control----
         addLayersControl(
           baseGroups = "OSM (default)", #leaflet_default_tiles_index,
           overlayGroups =
             c("Network Links (mid-points)", "Origin Poly Groups"),
           options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           # setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net_anlt_cntrd$count) %>% 
           addLegend(
             position = "bottomright"
             ,title = HTML("Poly Groups")
             ,group = "Origin Poly Groups"
             ,pal = pal_group
             ,opacity = 0.7
             ,values = query_poly$flag_poi)
  )
  
}

make_network_map_anlto = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
  
  #global vars/objects~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")
  
  #prep poi polygons~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  query_poly = file_query_poly %>%  
    read_sf() %>%  
    mutate(across(c(starts_with("flag"), group), as.factor))
  
  query_poly_poi = query_poly %>%  
    filter(flag_poi == "poi")
  
  query_poly_nonpoi = query_poly %>%  
    filter(flag_poi != "poi")
  
  
  map_elemet_poi = function(base_map){
    base_map %>%  
      addPolygons(data = query_poly_poi
                  ,fillColor = "blue",fillOpacity = .2
                  ,color = "black", opacity = .4,weight = 1
                  ,group = "POI Polygons", label = query_poly_poi$group) %>% 
      addPolygons(data = query_poly_nonpoi
                  ,fillColor = "tan"
                  ,opacity = .2,fillOpacity = .2,weight = 1
                  ,group = "Non-POI Study Area Polygons")
  }
  
  
  #prep data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  net_anlto = network_objects$agg_network_link_type_origin %>%  
    filter(!is.na(count) & !is.na(startLon)) %>% 
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                                ,T~NA_character_)) %>% 
    mutate(label = str_glue("Origin Group: {start_taz_group} <br> Network Link: {streetName} <br> Type: {highway} <br> Vehicle: Type {vehicle_type} <br> Link Volume {count}"))
  
  net_anlto_cntrd = net_anlto %>%  
    gauntlet::st_true_midpoint()
  
  net_anlto_cntrd_sd = SharedData$new(net_anlto_cntrd)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_anlto_cntrd$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           filter_select("net_anlto_cntrd_sd_dataset", "Choose Dataset:"
                         ,net_anlto_cntrd_sd, ~dataset)
           ,filter_select("net_anlto_cntrd_sd_flag_NHS", "Choose Specific Rdwy:"
                          ,net_anlto_cntrd_sd, ~flag_NHS)
           ,bscols(
             widths = c(6, 6)
             ,filter_select("net_anlto_cntrd_sd_highway", "Choose Street Type:"
                            ,net_anlto_cntrd_sd, ~highway)
             ,filter_select("net_anlto_cntrd_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_anlto_cntrd_sd, ~vehicle_type)
           )
           ,filter_select("net_anlto_cntrd_sd_grp", "Choose Specific Rdwy:"
                          ,net_anlto_cntrd_sd, ~start_taz_group)
           ,HTML("Truck Count Filters") %>%  strong()
           ,shiny::hr()
           ,filter_slider("net_anlto_cntrd_sd_count", "Trip Count:"
                          ,net_anlto_cntrd_sd, ~count)
           ,filter_slider("net_anlto_cntrd_sd_countlg10", "Trip Count (log10):"
                          ,net_anlto_cntrd_sd, ~dgt2(log10(count)))
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
           addCircleMarkers(data = net_anlto_cntrd_sd
                            ,fillColor = ~pal_centroids_od(net_anlto_cntrd$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            # ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
                            #                             select(!c(text, label)))
                            ,label =
                              net_anlto_cntrd$label %>%
                              map(htmltools::HTML)
                            # ,labelOptions = labelOptions(noHide = F, textOnly = F)
           ) %>%
           map_elemet_poi() %>% 
           #layer control----
         addLayersControl(
           baseGroups = leaflet_default_tiles_index
           ,overlayGroups =
             c("Network Links (mid-points)"
               ,"POI Polygons", "Non-POI Study Area Polygons")
           ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net_anlto_cntrd$count) %>% 
           
  )

}















# #getting the data===============================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ##get trips=====================================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# temp_2 = here("data/memphis_req/data_for_query","split_taz_polys_pro_comb_20221031.shp") %>% read_sf()
# schema_table_2 = "wsp.south_central_2021_Q4_thursday_trip_taz"
# queried_data_2 = query_replica(temp_2, schema_table_2, limit = 5000)
# data = queried_data_2
# saveRDS(queried_data_2, 
#         here("data/memphis_req/data_queried", "memphis_request_trip_20221101.rds"))
# 
# ##get network===================================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_gis = here("data/memphis_req/data_for_query", "split_taz_polys_pro_comb_20221031.shp") %>%
#   read_sf()
# schema_table = "south_central.south_central_2021_Q4_network_segments"
# # 
# network_memphis = get_replica_network(
#   data_gis
#   ,schema_table
#   ,limit = NA
# )
# 
# here("data/memphis_req/data_queried", "network_memphis_pro_20221101.gpkg") %>%  
#   write_sf(network_memphis, .)
# 
# network_mem = here("data/memphis_req/data_queried", "network_memphis_pro_20221101.gpkg") %>%  
#   read_sf(.)
# # network = network_memphis
# 
# ##get network===================================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# test = make_spatial_networks(
#   network = network_mem
#   ,data = queried_data_2
#   ,query_poly = temp_2
# )







































##Mapping Functions=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# make_network_maps = function(data, cluster_object){
#   # data = tar_read("data_processed_queries")
#   # cluster_object = tar_read("data_manual_cluster")
#   
#   #global vars/objects
#   leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
#   
#   cluster = cluster_object %>%  
#     mutate(index_cluster = as.factor(index_cluster))
#   
#   index_cluster_pair = c("1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_8", "1_9", "1_10"  
#     ,"2_3", "2_4", "2_6", "2_7", "2_8", "2_9", "2_10"  
#     ,"3_4", "3_5", "3_6", "3_7", "3_8", "3_9", "3_10", "3_11", "3_12"
#     ,"4_5", "4_6", "4_7", "4_8", "4_9","4_10", "4_11", "4_12"  
#     ,"5_6", "5_7", "5_8", "5_9", "5_10", "5_11", "5_12"
#     ,"6_7", "6_8", "6_9", "6_10", "6_11", "6_12"  
#     ,"7_8", "7_9","7_10", "7_11", "7_12"  
#     ,"8_9","8_10", "8_11", "8_12"
#     ,"9_10", "9_11", "9_12"
#     ,"10_11", "10_12"
#     ,"11_12" )
#   
#   
#   
#   #OD map
#   
#   ##prepping data----
#   
#   network_agg_od = data$network_agg_od %>%  
#     mutate(across(c(origin_cluster, destination_cluster), as.numeric)
#            ,cluster_pair = fct_relevel(cluster_pair
#                                        ,index_cluster_pair)
#            ,cp_pr_od_pair = dgt2(cp_pr_od_pair))
#   
#   # levels(network_agg_od$cluster_pair)
#   
#   network_agg_od_mp = network_agg_od %>%  
#     # sample_n(20000) %>%
#     select(origin_cluster, destination_cluster, cluster_pair, highway
#            # ,n, pr_od, pr_od_pair, dataset
#            ,n, pr_od, cp_od, pr_od_pair, cp_od_pair, cp_pr_od_pair, dataset
#     ) %>%  
#     rename(highway_class = highway, count = n, dataset = dataset
#            # ,precent_rank_od = pr_od, precent_rank_od_pair = pr_od_pair
#            ,precent_rank_od = pr_od, count_percent = cp_od
#            ,precent_rank_od_pair = pr_od_pair, count_percent_od = cp_od_pair, count_percent_od_pair = cp_pr_od_pair
#     ) %>%  
#     mutate(dataset = gsub(".*(2019|2021)", "\\1", dataset)
#            ,across(starts_with("precent"), dgt2)
#            ,label = str_glue(
#              "{cluster_pair} from {origin_cluster} <br> {count} - {precent_rank_od} - {precent_rank_od_pair}")
#            ,text = str_glue(
#              "Origin Pair: {cluster_pair} \n Origin Cluster: {origin_cluster} \n
#            Truck Count: {count} \n Percent Rank: {precent_rank_od} \n Percent Rank for OD Pair: {precent_rank_od_pair}")) %>% 
#     st_true_midpoint()
#   
#   network_agg_od_mp_sm = network_agg_od_mp %>%  
#     filter(count> 3) 
#   
#   network_agg_od_mp_sd = SharedData$new(network_agg_od_mp_sm)
#   
#   ##make map----
#   
#   pal_centroids_od = colorNumeric(
#     palette = "magma"
#     ,network_agg_od_mp_sm$count
#     ,reverse = T)
#   
#   pal_clusters = colorFactor(
#     rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
#                              length(levels(cluster$index_cluster)),
#     )),
#     cluster$index_cluster)
#   
#   # map_network_od = 
#   bscols(widths = c(3, 9)
#          ,list(
#            bscols(
#              widths = c(6, 6)
#              ,filter_select("dataset", "Choose Dataset: "
#                             ,network_agg_od_mp_sd, ~dataset)
#              ,filter_select("cluster_pair", "Choose Origin Pairs:"
#                             ,network_agg_od_mp_sd, ~cluster_pair))
#            ,bscols(
#              widths = c(6, 6)
#              ,filter_select("origin_cluster", "Origin Cluster:"
#                             ,network_agg_od_mp_sd, ~origin_cluster)
#              ,filter_select("destination_cluster", "Destination Cluster:"
#                             ,network_agg_od_mp_sd, ~destination_cluster))
#            ,HTML("Truck Count Filters") %>%  strong()
#            ,shiny::hr()
#            # ,shiny::hr()
#            ,filter_slider("count", "Truck Counts per link per OD Pair:"
#                           ,network_agg_od_mp_sd, ~count)
#            # ,HTML("Truck Count Percent Rank per OD")
#            # ,shiny::hr()
#            ,filter_slider("precent_rank_od", "Truck Counts Percent Rank (per unidirectional OD Pair):"
#                           ,network_agg_od_mp_sd, ~precent_rank_od)
#            # ,filter_slider("precent_rank_od_pair", "Bidirectional:"
#            #                ,network_agg_od_mp_sd, ~precent_rank_od_pair)
#            # ,HTML("Link Percent of Total OD Pair Volume")
#            # ,shiny::hr()
#            # ,filter_slider("count_percent_od_pair", "Percent Rank (link count/total OD count):"
#            #                ,network_agg_od_mp_sd, ~count_percent_od_pair)
#            # ,filter_slider("count_percent_od", "count_percent_od:"
#            #                ,network_agg_od_mp_sd, ~count_percent_od)
#          )
#          ,leaflet(height = 700) %>% 
#            addTiles(group = "OSM (default)") %>% 
#            # leaflet_default_tiles() %>% 
#            addCircleMarkers(data = network_agg_od_mp_sd
#                             ,fillColor = ~pal_centroids_od(network_agg_od_mp_sm$count)
#                             ,color = "black"
#                             ,opacity = .8
#                             ,fillOpacity  = .5
#                             ,weight = 1
#                             ,radius = 5
#                             ,group = "Network Links (mid-points)"
#                             ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
#                                                         select(!c(text, label)))
#                             ,label =
#                               network_agg_od_mp_sm$label %>%
#                               map(htmltools::HTML)
#                             ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>% 
#            addPolygons(data = cluster
#                        ,fillColor = ~pal_clusters(cluster$index_cluster)
#                        ,opacity = .8
#                        ,fillOpacity = .4
#                        ,weight = 1
#                        ,group = "OD Clusters"
#                        ,label = cluster$index_cluster) %>%
#            #layer control----
#          addLayersControl(
#            baseGroups = "OSM (default)", #leaflet_default_tiles_index,
#            overlayGroups =
#              c("Network Links (mid-points)", "OD Clusters"),
#            options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
#            setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
#            addMouseCoordinates() %>%
#            ##legends----
#          addLegend(
#            position = "bottomleft"
#            ,title = HTML("Link Truck Counts")
#            ,group = "Network Links (mid-points)"
#            ,pal = pal_centroids_od
#            ,opacity = 0.7
#            ,values = network_agg_od_mp$count) %>%  
#            addLegend(
#              position = "bottomleft"
#              ,title = HTML("OD Clusters")
#              ,group = "OD Clusters"
#              ,pal = pal_clusters
#              ,opacity = 0.7
#              ,values = cluster$index_cluster)
#          
#          
#   )
  
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
  
# }

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================











                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 