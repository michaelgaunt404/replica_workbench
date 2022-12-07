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

count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                   col, prefix = NULL, rnd = NULL, cntr_scl = FALSE){
  
  tmp = data %>%
    data.table::data.table() %>%
    .[,.(count = sum(.SD)), .SDcols = col, by = grp_c] %>%
    .[,`:=`(percent = (count/sum(count)) %>%
              { if (!is.null(rnd)) round(., rnd) else .}), by = grp_p] %>%
    { if (cntr_scl) (.) %>%
        .[,`:=`(zscore = as.vector(scale(count))), by = grp_z]
      else .}
  
  if (is.null(prefix)){
    tmp = tmp
  } else {
    newname1 = str_glue("{prefix}_count")
    newname2 = str_glue("{prefix}_percent")
    rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
  
  return(tmp)
  
}

#data prep and query============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

make_network_points = function(network){
  # network = tar_read('mem_network')
  
  network %>%  
    read_sf() %>% 
    mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>%  
    st_true_midpoint()
}

make_network_links = function(network){
  # network = tar_read('mem_network')
  
  network %>%  
    read_sf() %>% 
    mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) 
}

data_converage = function(data){
  # query_poly = tar_read('mem_query_poly')
  # query_poly = tar_read('mem_query_poly_custom')
  # data = tar_read('mem_data_trip_custom')
  
  data_coords = data %>%
    select(contains(c("lat", "lng"))) %>%  
    mutate(count = 1)
  x= "origin"
  c("origin"
    ,"destination") %>%  
    map(~{
      
      data_coords %>%  
        select(contains(.x)) %>%  
        set_names(c("lat", "lng")) %>%  
        count(lat, lng) %>%  
        st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%  
        mapview()
      
    })
  
  data_coords = data %>%
    select(contains(c("lat", "lng"))) %>%  
    select
}

process_trip_data = function(network, data, query_poly
                                  ,rm_self = T, rm_ext = T){
  #NOTE: this will return more rows on purpose 
  #filter na counts to return to original size
  message("Preping data...")
  
  query_poly = query_poly %>%
    read_sf()
  
  query_poly_df = query_poly %>%
    st_drop_geometry()
  
  query_poly_df_poi = query_poly_df %>%
    filter(flag_poi == "poi") %>%
    rename(start_taz_group = group)
  
  query_poly_df_notpoi = query_poly_df %>%
    rename(end_taz_group = group)
  
  data %>%
    mutate(count = 1) %>%
    mutate(dataset = "south_central_2021_Q4_thursday") %>%
    select(c('dataset', 'activity_id', 'start_taz'
             ,'end_taz', 'vehicle_type', 'network_link_ids', 'count')) %>%
    merge(., query_poly_df_poi %>%
            select(id, start_taz_group)
          ,by.x = "start_taz", by.y = "id", all = T) %>% 
    merge(., query_poly_df_notpoi %>%
            select(id, end_taz_group, flag_poi) %>%
            rename(flag_poi_end = flag_poi)
          ,by.x = "end_taz", by.y = "id", all = T) %>%  
    mutate(flag_sa_end = case_when(end_taz %in% unique(query_poly$id)~"internal"
                                   ,T~"external")
           ,end_taz = case_when(end_taz %in% unique(query_poly$id)~end_taz
                                ,T~"out of study area")) %>% 
    {if (rm_self) (.) %>%  filter(start_taz != end_taz) else .} %>%  
    {if (rm_ext) (.) %>%  filter(flag_sa_end != 'external') else .}
}

process_data_aggregate = function(network, data, query_poly
                                  ,rm_self = T, rm_ext = T){
  # query_poly = tar_read('mem_query_poly')
  # query_poly = tar_read('mem_query_poly_custom')
  # data = tar_read('mem_data_trip_custom')
  
  #TODO: function should be able to process data to external locations
  #---ideally should tell what percentage of link trips area to external locations
  #---i really don't know what that will get you but we should think about it in the future
  #---assume taz locations on the edge would have more percent of external trips

  message("Preping data...")

  query_poly = query_poly %>%
    read_sf()
  
  query_poly_df = query_poly %>%
    st_drop_geometry()

  query_poly_df_poi = query_poly_df %>%
    filter(flag_poi == "poi") %>%
    rename(start_taz_group = group)
  
  query_poly_df_notpoi = query_poly_df %>%
    rename(end_taz_group = group)

  data_pro = data %>%
    mutate(count = 1) %>%
    mutate(dataset = "south_central_2021_Q4_thursday") %>%
    select(c('dataset', 'activity_id', 'start_taz'
             ,'end_taz', 'vehicle_type', 'network_link_ids', 'count')) %>%
    merge(., query_poly_df_poi %>%
            select(id, start_taz_group)
          ,by.x = "start_taz", by.y = "id") %>% 
    merge(., query_poly_df_notpoi %>%
            select(id, end_taz_group, flag_poi) %>%
            rename(flag_poi_end = flag_poi)
          ,by.x = "end_taz", by.y = "id", all = T) %>%  
    mutate(flag_sa_end = case_when(end_taz %in% unique(query_poly$id)~"internal"
                                   ,T~"external")
           ,end_taz = case_when(end_taz %in% unique(query_poly$id)~end_taz
                                ,T~"out of study area")) %>% 
    {if (rm_self) (.) %>%  filter(start_taz != end_taz) else .} %>%  
    {if (rm_ext) (.) %>%  filter(flag_sa_end != 'external') else .}
  
  link_unnest = data_pro %>%
    unnest(cols = network_link_ids) %>%  
    data.table() %>% 
    .[,`:=`(network_link_ids_trunc = str_trunc(network_link_ids, 14, "right", ""))] %>% 
    data.table()
   
  message("Perfroming aggregations...")

  #agg by network link
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_network_link = link_unnest %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'network_link_ids_trunc', 'flag_sa_end')
      ,grp_p = c('dataset', 'network_link_ids_trunc')
      ,col = 'count') %>%  
    .[order(-count)] 
  
  message("Link aggregations complete...")

  #agg by network link and vehicle type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_network_link_type = link_unnest %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'network_link_ids_trunc', 'vehicle_type')
      ,grp_p = c('dataset', 'network_link_ids_trunc')
      ,col = 'count') %>%
    .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
            ,count_adj_max = dgt2(count/max(count)))
      ,by = .(dataset, vehicle_type)] %>% 
    .[,`:=`(ttl_count_link = sum(count)), by = .(network_link_ids_trunc)] %>% 
    .[,`:=`(ttl_count_link_type = sum(count)), by = .(vehicle_type)] %>%  
    data.frame() %>%  
    mutate(label = str_glue(
      "Link: {network_link_ids_trunc}
    <br>Total Link Volume: {ttl_count_link}
    <hr>
    Metrics  Adj for Vehicle Type
    <br>Vehicle Type: {ttl_count_link_type}
    <br>Link Volume: {count} ({100*dgt2(percent)}% of link total)
    <br>Link Volume/max(Link Volume): {100*count_adj_max}%")) %>%  
    data.table() %>%  
    .[count >= 5]
  
  message("Link and vehicle type aggregations complete...")

  #agg by network link, vehicle type, and origin
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_network_link_type_origin = link_unnest %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'start_taz', 'start_taz_group', 'network_link_ids_trunc', 'vehicle_type')
      ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'network_link_ids_trunc')
      ,col = 'count') %>%
    .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
            ,count_adj_max = dgt2(count/max(count)))
      ,by = .(dataset, start_taz_group, vehicle_type)] %>% 
    .[,`:=`(ttl_count_orgin = sum(count)), by = .(start_taz_group)] %>% 
    .[,`:=`(ttl_count_orgin_type = sum(count)), by = .(start_taz_group, vehicle_type)] %>% 
    .[,`:=`(ttl_count_orgin_type_other = ttl_count_orgin-ttl_count_orgin_type
            ,ttl_count_orgin_type_per = ttl_count_orgin_type/ttl_count_orgin)] %>%  
    data.frame() %>% 
    mutate(label = str_glue(
      "Origin Group: {start_taz_group}
    <br>Total Origin Trips: {ttl_count_orgin}
    <hr>
    Metrics  Adj for Vehicle Type
    <br>Vehicle Type: {vehicle_type}
    <br>Total Origin Trips: {ttl_count_orgin_type} ({100*dgt2(ttl_count_orgin_type_per)}% of total)
    <br>Link Volume: {count} ({100*dgt2(percent)}% of link total)
    <br>Link Volume/max(Link Volume): {100*count_adj_max}%")) %>% 
    data.table() %>% 
    .[count >= 5]
  
  message("Link, vehicle type, and origin aggregations complete...")

  # agg by network link, vehicle type, origin and destination
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_network_link_type_origin_dest = link_unnest %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'start_taz_group', 'end_taz', 'network_link_ids_trunc', 'vehicle_type')
      ,grp_p = c('dataset', 'start_taz_group', 'end_taz', 'network_link_ids_trunc')
      ,col = 'count'
      ,rnd = 2) %>% 
    .[count > 5,] 
  
  message("Link, vehicle type, origin, and dest aggregations complete...")

  #agg by OD
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_od = data_pro %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'start_taz', 'start_taz_group', 'vehicle_type', 'end_taz')
      ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'end_taz')
      # ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'vehicle_type')
      ,col = 'count'
      ,rnd = 2) %>%
    .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
            ,count_adj_max = dgt2(count/max(count)))
      ,by = .(dataset, start_taz_group, vehicle_type)] %>% 
    .[,`:=`(ttl_count_orgin = sum(count)), by = .(start_taz_group)] %>% 
    .[,`:=`(ttl_count_orgin_type = sum(count)), by = .(start_taz_group, vehicle_type)] %>% 
    data.frame() %>%  
    rename(percent_origin_trips = percent) %>% 
    mutate(label = str_glue(
      "Origin Group: {start_taz_group} 
    <br> Total Origin Trips: {ttl_count_orgin}
    <br> Destination Poly: {end_taz}
    <hr>
    Metrics  Adj for Vehicle Type 
    <br> Vehicle Type: {vehicle_type}
    <br> Total Origin Trips: {ttl_count_orgin_type} ({100*dgt2(ttl_count_orgin_type/ttl_count_orgin)}% of total)
    <br> Trips to Destination: {count} ({100*dgt2(percent_origin_trips)}% of link total)
    <br> Destination count/max(count): {100*count_adj_max}%
    <br> % of Origin Total: {100*percent_origin_trips}%")) %>% 
    data.table()
  
  message("Origin/destination aggregations complete...")
  
  #agg by O
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_o = agg_od %>% 
    count_percent_zscore_dt(grp_c = c("start_taz", "vehicle_type")
                            ,grp_p = c("start_taz")
                            ,col = 'count'
                            ,rnd = 2) %>%  
    .[order(start_taz, vehicle_type)] %>% 
    .[,`:=`(count_ttl_veh = sum(count)), by = .(vehicle_type)] %>% 
    .[,`:=`(count_tll_origin = sum(count)), by = .(start_taz)] %>% 
    .[,`:=`(vehicle_type = ifelse(str_detect(vehicle_type, "HEAVY"), "heavy", "medium"))] %>% 
    data.frame() %>% 
    pivot_wider(names_from = vehicle_type
                ,values_from = c(count, percent, count_ttl_veh)) %>%  
    mutate(label = str_glue(
      "Origin TAZ: {start_taz} 
    <br>Total Origin Trips: {count_tll_origin}
    <br>Heavy Duty Trips: {count_heavy} ({100*percent_heavy}%)
    <br>Medium Duty Trips: {count_medium} ({100*percent_medium}%)"))
  
  message("Origin aggregations complete...")
  
  list_objects = list(agg_network_link = agg_network_link
                      ,agg_network_link_type = agg_network_link_type
                      ,agg_network_link_type_origin = agg_network_link_type_origin
                      ,agg_od = agg_od
                      ,agg_o = agg_o)
  
  return(list_objects)
  
}

make_spatial_networks = function(network, data){
  # network = tar_read('mem_network_pnt')
  # data = tar_read('mem_data_pro_agg')
  
  network_merged = list(agg_network_link = data$agg_network_link
                        ,agg_network_link_type = data$agg_network_link_type
                        ,agg_network_link_type_origin = data$agg_network_link_type_origin
  ) %>%  
    map(~merge(network
               ,.x 
               ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_trunc", all = T) %>%  
          mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
          filter(sf_geom_typ == "POINT") %>%  
          filter(!is.na(count) & !is.na(startLon))) 
  
  # list_objects = network_merged %>%  c(query_poly_od = list(query_poly_od)) 
  
  return(network_merged)
  
}

make_spatial_od_polys = function(query_poly, data){
  # data = tar_read('mem_data_pro_agg_custom')
  # query_poly = tar_read('mem_query_poly_custom')
  
  query_poly = query_poly %>%  
    read_sf() 
  
  list_data = list(agg_od = merge(query_poly, select(data$agg_od, !dataset)
                                  ,by.x = c('id'), by.y = c("end_taz"))
                   ,agg_o = merge(query_poly, data$agg_o
                                  ,by.x = c('id'), by.y = c("start_taz")))
  
  return(list_data)
}

#script end=====================================================================



