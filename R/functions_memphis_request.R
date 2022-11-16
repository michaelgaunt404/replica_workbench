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
purrr_get_safe_results = function(object, type = "result"){
  object %>%
    map(~.x[[type]])
}

dbGetQuery_safe = purrr::safely(DBI::dbGetQuery)

query_database = function(db_connection, query_string, query_item, dataset){
  temp_data = dbGetQuery(db_connection, query_string)
  
  message(paste0(nrow(temp_data), " rows returned..."))
  
  temp_data %>% 
    mutate(queried_group = query_item
           ,dataset = dataset)
}

query_database_safe = purrr::safely(query_database)

query_database_count = function(db_connection, query_string){
  temp = dbGetQuery(db_connection, query_string)

  temp[[1]]
}

query_poi_to_everything = function(data, db_connection, schema_table, limit = 50){
  
  limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(group != "no_group_id") %>%  
    select(group, id) 
  
  message("Begin query..")
  
  query_index = poly_query_df$group %>%  
    unique() %>%  
    sort() 
  
  queried_data = query_index %>% 
    map(~{
      
      message(paste0("Querying for ", .x))
      
      query_focus_id = poly_query_df %>%  
        filter(group == .x) %>%  
        pull(id) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ") 
      
      query_string = str_glue("SELECT * FROM
  `{schema_table}`
 WHERE start_taz IN ({query_focus_id}) AND mode = 'COMMERCIAL' {limit_query}")
      
      temp_data = query_database_safe(db_connection, query_string, .x,  schema_table)
      
    })
  
  message("++++\n++++\nQuery complete....")
  
  queried_data_results = queried_data %>%  
    purrr_get_safe_results() %>% 
    reduce(bind_rows)
  
  bad_groups = query_index[!(query_index %in% 
                               queried_data_results$queried_group)]
  
  if (length(bad_groups)>0){
    message(str_glue('Unsuccessfully queried groups: \n{paste(bad_groups,collapse = "\n")}'))
  } else {
    message("All groups successfully queried!")
  }
  queried_data_results
}

query_poi_to_everything_k_lmt = function(data, db_connection, schema_table, limit = 50){
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(group != "no_group_id") %>%  
    select(group, id) 
  
  message("Begining query...")
  
  query_index = poly_query_df$group %>%  
    unique() %>%  
    sort()
  
  queried_data = query_index %>% 
    map(~{
      
      query_focus_id = poly_query_df %>%  
        filter(group == .x) %>%  
        pull(id) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ") 
      
      temp_table_name = "replica-customer._script455c3f9d5ccc9ec914f73e8a4f3cdd38d3335e74.temp_table_k_lmt_dl"
      
      query_string = str_glue("SELECT count(*) as count FROM `{temp_table_name}`
    WHERE start_taz IN ({query_focus_id})")
      
      limit = query_database_count(db_connection, query_string)
      limit_k_adj = ceiling(limit/1000)
      
      if (limit == 0){
        message(paste0("''''\nQuerying for ", .x))
        message(str_glue("There are {limit} records for this group, skipping..."))
        
      } else {
        
        message(paste0("''''\nQuerying for ", .x))
        message(str_glue("There are {limit} records for this group..."))
        message(str_glue("Will make {limit_k_adj} queries of 1000 records...\n\n''''"))
        
        temp_data = list(rep(.x, limit_k_adj)
                         ,seq(1, limit_k_adj, 1)
        ) %>% 
          pmap(~{
            
            lim_bttm = ((.y-1)*1000)
            lim_uppr = ((.y)*1000)
            
            query_string = str_glue("SELECT * FROM `{temp_table_name}`
                                WHERE row >= {lim_bttm} AND 
                                row < {lim_uppr};")
            
            data = dbGetQuery_safe(
              db_connection
              ,query_string)
            
            if ((!is.null(data$result) & is.null(data$error))==TRUE){
              message(str_glue("''''\nQuery {.y} of {limit_k_adj} successful -- {nrow(data$result)} records received..."))
            } else {
              message("Query {.y} of {limit_k_adj} unsuccessful...")
            }
            
            data
          })
        
        temp_data %>%  
          purrr_get_safe_results() %>%  
          reduce(bind_rows) %>%  
          arrange(row)
        
      }
    })
  
  message("++++\n++++\nQuery complete....")
  
  queried_data_results = queried_data %>%  
    reduce(bind_rows)
  
  bad_groups = query_index[!(query_index %in% 
                               queried_data_results$start_taz)]
  
  if (length(bad_groups)>0){
    message(str_glue('Groups unsuccessfully queried: \n{paste(bad_groups,collapse = "\n")}'))
  } else {
    message("All groups successfully queried!")
  }
  queried_data_results
}

query_replica = function(data, schema_table, limit = 50){
  # data = tar_read('mem_query_poly')
  # schema_table ="wsp.south_central_2021_Q4_thursday_trip_taz"
  # data = tar_read("mem_query_poly_custom")
  # schema_table = "wsp.south_central_2021_Q4_thursday_trip_custom_taz"
  
  # browser()

  data_pro = read_sf(data)
  
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "replica-customer",
    dataset = schema_table
  )
  
  queried_data =  query_poi_to_everything_k_lmt(data = data_pro
                                          ,db_connection = con
                                          ,schema_table = schema_table
                                          ,limit = limit) 
  
  queried_data
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
  network = tar_read('mem_network')
  
  network %>%  
    read_sf() %>% 
    mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>%  
    st_true_midpoint()
}

process_data_aggregate = function(network, data, query_poly, rm_self = T){
  # query_poly = tar_read('mem_query_poly')
  # query_poly = tar_read('mem_query_poly_custom')
  # data = tar_read('mem_data_trip_custom')

  message("Preping data...")

  query_poly = query_poly %>%
    read_sf()

  query_poly_df = query_poly %>%
    st_drop_geometry() %>%
    filter(flag_poi == "poi") %>%
    rename(start_taz_group = group)

  data_pro = data %>%
    mutate(count = 1) %>%
    mutate(dataset = "wsp.south_central_2021_Q4_thursday_trip_custom_taz") %>%
    select(c('dataset', 'activity_id', 'start_taz'
             ,'end_taz', 'vehicle_type', 'network_link_ids', 'count')) %>%
    {if (rm_self) (.) %>%  filter(start_taz != end_taz) else .} %>%
    merge(., query_poly_df %>%
            select(id, start_taz_group)
          ,by.x = "start_taz", by.y = "id") %>%
    merge(., query_poly_df %>%
            select(id, start_taz_group) %>%
            rename(end_taz_group = start_taz_group)
          ,by.x = "end_taz", by.y = "id", all = T)

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
      grp_c = c('dataset', 'network_link_ids_trunc')
      ,grp_p = c('dataset')
      ,col = 'count')
  
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

  #agg by network link
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  agg_od = data_pro %>%
    count_percent_zscore_dt(
      grp_c = c('dataset', 'start_taz', 'start_taz_group', 'vehicle_type', 'end_taz', 'end_taz_group')
      ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'end_taz', 'end_taz_group')
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
    <br> Total Origin Trips: {ttl_count_orgin_type}
    <br> Trips to Destination: {count}
    <br> % of Origin Total: {100*percent_origin_trips}%
    # <br> Max Destination Count: {count}
    <br> % of Max OD Count: {100*count_adj_max}%
    <br> Link Volume Percent Rank: {count_pRank_adj}")) %>% 
    data.table()
  
  message("Link, vehicle type, origin, and dest aggregations complete...")
  
  list_objects = list(agg_network_link = agg_network_link
                      ,agg_network_link_type = agg_network_link_type
                      ,agg_network_link_type_origin = agg_network_link_type_origin
                      ,agg_od = agg_od)
  
  return(list_objects)
  
}

#map functions==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_spatial_networks = function(network, data){
  # network = tar_read('mem_network_pnt')
  # data = tar_read('mem_data_pro_agg')
  # # query_poly = tar_read('mem_query_poly')
  # query_poly = tar_read('mem_query_poly_custom')
  
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

make_spatial_od_polys = function(network, data){
  # network = tar_read('mem_network_pnt')
  # data = tar_read('mem_data_pro_agg')
  # # query_poly = tar_read('mem_query_poly')
  # query_poly = tar_read('mem_query_poly_custom')
  
  network_merged = list(agg_network_link = data$agg_network_link
                        ,agg_network_link_type = data$agg_network_link_type
                        ,agg_network_link_type_origin = data$agg_network_link_type_origin
  ) %>%  
    map(~merge(network
               , 
               ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_trunc", all = T) %>%  
          mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
          filter(sf_geom_typ == "POINT") %>%  
          filter(!is.na(count) & !is.na(startLon))) 
  
  data$agg_od
  
  # list_objects = network_merged %>%  c(query_poly_od = list(query_poly_od)) 
  
  return(network_merged)
  
}

make_network_map_anl = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_network_objects_custom')
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
  net = network_objects$agg_network_link %>%  
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                     ,T~NA_character_)) %>% 
    mutate(label = str_glue("Network Link: {streetName} 
                            <br>Type: {highway} 
                            <br>Link Volume {count}"))
  
  net_sd = SharedData$new(net)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_sd$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           filter_select("net_sd_dataset", "Choose Dataset:"
                         ,net_sd, ~dataset)
           ,filter_select("net_sd_flag_NHS", "Choose Specific Rdwy:"
                          ,net_sd, ~flag_NHS)
           ,bscols(
             widths = c(12)
             ,filter_select("net_sd_highway", "Choose Street Type:"
                            ,net_sd, ~highway)
           )
           ,HTML("Network Link Volume Filters") %>%  strong()
           ,shiny::hr()
           ,filter_slider("net_sd_count", "Link Volume (vehicle counts):"
                          ,net_sd, ~count)
           ,filter_slider("net_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
                          ,net_sd, ~dgt2(log10(count)))
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
           addCircleMarkers(data = net_sd
                            ,fillColor = ~pal_centroids_od(net$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,label =
                              net$label %>%
                              map(htmltools::HTML)
           ) %>%
           map_elemet_poi() %>% 
           #layer control----
         addLayersControl(
           baseGroups = leaflet_default_tiles_index
           ,overlayGroups =
             c("Network Links (mid-points)"
               ,"POI Polygons", "Non-POI Study Area Polygons")
           ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
           setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net$count) 
         )

}

make_network_map_anlt = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
  # network_objects = tar_read('mem_data_network_objects_custom')
  # file_query_poly = tar_read("mem_query_poly_custom")
  
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
  net = network_objects$agg_network_link_type %>%  
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                                ,T~NA_character_))  
  
  net_sd = SharedData$new(net)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           bscols(
             widths = c(12, 12, 6, 6, 12, 12, 12, 12)
             ,filter_select("net_sd_dataset", "Choose Dataset:"
                            ,net_sd, ~dataset)
             ,filter_select("net_sd_flag_NHS", "Choose Specific Rdwy:"
                            ,net_sd, ~flag_NHS)
             ,filter_select("net_sd_highway", "Choose Street Type:"
                            ,net_sd, ~highway)
             ,filter_select("net_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_sd, ~vehicle_type)
             ,HTML("Network Link Volume Filters") %>%  strong()
             ,shiny::hr()
             ,filter_slider("net_sd_count", "Link Volume (vehicle counts):"
                            ,net_sd, ~count)
             ,filter_slider("net_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
                            ,net_sd, ~dgt2(log10(count)))
             ,filter_slider("net_sd_count_std_hwy", "Link Volume / Max Link Volume:"
                            ,net_sd, ~count_adj_max)
             ,filter_slider("net_sd_per_util", "Link Utilization by Vehicle Type:"
                            ,net_sd, ~100*dgt2(percent))
           )
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
           addCircleMarkers(data = net_sd
                            ,fillColor = ~pal_centroids_od(net$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,label =
                              net$label %>%
                              map(htmltools::HTML)
           ) %>%
           map_elemet_poi() %>% 
           #layer control----
         addLayersControl(
           baseGroups = leaflet_default_tiles_index
           ,overlayGroups =
             c("Network Links (mid-points)"
               ,"POI Polygons", "Non-POI Study Area Polygons")
           ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
           setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net$count) 
  )
  
}

make_network_map_anlto = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
  # network_objects = tar_read('mem_data_network_objects_custom')
  # file_query_poly = tar_read("mem_query_poly_custom")
  
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
  net = network_objects$agg_network_link_type_origin %>%  
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                                ,T~NA_character_)) 
  
  net_sd = SharedData$new(net)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           bscols(
             widths = c(12, 12, 6, 6, 12, 12, 12, 12, 12, 12, 12)
             ,filter_select("net_sd_dataset", "Choose Dataset:"
                            ,net_sd, ~dataset)
             ,filter_select("net_sd_flag_NHS", "Choose Specific Rdwy:"
                            ,net_sd, ~flag_NHS)
             ,filter_select("net_sd_highway", "Choose Street Type:"
                            ,net_sd, ~highway)
             ,filter_select("net_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_sd, ~vehicle_type)
             ,filter_select("net_sd_grp", "Choose Freight Terminal:"
                            ,net_sd, ~start_taz_group)
             
             ,HTML("Network Link Volume Filters") %>%  strong()
             ,br()
             ,"(adj for origin & veh type)"
             ,shiny::hr()
             
             ,filter_slider("net_sd_count", "Link Volume (vehicle counts):"
                            ,net_sd, ~count)
             ,filter_slider("net_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
                            ,net_sd, ~dgt2(log10(count)))
             ,filter_slider("net_sd_count_adj_max", "Link Volume / Max Link Volume:"
                            ,net_sd, ~count_adj_max)
             ,filter_slider("net_sd_per_util", "Link Utilization by Vehicle Type:"
                            ,net_sd, ~100*dgt2(percent))
           )
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
           addCircleMarkers(data = net_sd
                            ,fillColor = ~pal_centroids_od(net$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,label =
                              net$label %>%
                              map(htmltools::HTML)
           ) %>%
           map_elemet_poi() %>% 
           #layer control----
         addLayersControl(
           baseGroups = leaflet_default_tiles_index
           ,overlayGroups =
             c("Network Links (mid-points)"
               ,"POI Polygons", "Non-POI Study Area Polygons")
           ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
           setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("Link Truck Counts")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_od
           ,opacity = 0.7
           ,values = net$count) 
         
  )
  
}

make_map_origin = function(network_objects, file_query_poly){
  # network_objects = tar_read('mem_data_network_objects')
  # file_query_poly = tar_read("mem_query_poly")
  # network_objects = tar_read('mem_data_network_objects_custom')
  # file_query_poly = tar_read("mem_query_poly_custom")
  
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
  query_poly_od = network_objects$query_poly_od 
  
  query_poly_od_cntrd = query_poly_od %>%  
    st_make_valid() %>% 
    st_centroid() 
  
  query_poly_od_cntrd_sd = SharedData$new(query_poly_od_cntrd)
  
  pal_centroids_count = colorNumeric(
    palette = "magma"
    ,query_poly_od_cntrd$count_adj_max
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           bscols(
             widths = c(12)
             ,filter_select("query_poly_od_cntrd_sd_vehicle_type", "Choose Vehicle Type:"
                            ,query_poly_od_cntrd_sd, ~vehicle_type)
             ,filter_select("query_poly_od_cntrd_sd_grp", "Choose Freight Terminal:"
                            ,query_poly_od_cntrd_sd, ~start_taz_group)
           )
           ,HTML("Network Link Volume Filters") %>%  strong()
           ,shiny::hr()
           ,bscols(
             widths = c(12)
             ,filter_slider("query_poly_od_cntrd_sd_count", "Trips to Destination:"
                            ,query_poly_od_cntrd_sd, ~count)
             ,filter_slider("query_poly_od_cntrd_sd_percent", "% of Ttl. Origin Trips:"
                            ,query_poly_od_cntrd_sd, ~100*percent_origin_trips)
             ,filter_slider("query_poly_od_cntrd_sd_percent", "% of Max OD Count:"
                            ,query_poly_od_cntrd_sd, ~100*count_adj_max)
             ,filter_slider("query_poly_od_cntrd_sd_count_std", "Trips to Destination: (adj for orgin/vehicle):"
                            ,query_poly_od_cntrd_sd, ~count_pRank_adj)
           )
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
           addCircleMarkers(data = query_poly_od_cntrd_sd
                            ,fillColor = ~pal_centroids_count(query_poly_od_cntrd$count_adj_max)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            # ,radius = ~log2(query_poly_od_cntrd$count)
                            ,group = "Destination TAZs (centroids)"
                            # ,popup = popup_tbl_pretty(network_agg_od_mp_sm %>%
                            #                             select(!c(text, label)))
                            ,label =
                              query_poly_od_cntrd$label %>%
                              map(htmltools::HTML)
                            ,labelOptions = labelOptions(noHide = F, textOnly = F)
           ) %>%
           map_elemet_poi() %>%
           #layer control----
         addLayersControl(
           baseGroups = leaflet_default_tiles_index
           ,overlayGroups =
             c("Destination TAZs (centroids)"
               ,"POI Polygons", "Non-POI Study Area Polygons")
           ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
           hideGroup(c("Destination TAZs (centroids)", "Non-POI Study Area Polygons")) %>% 
           setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
           addMouseCoordinates() %>%
           ##legends----
         addLegend(
           position = "bottomleft"
           ,title = HTML("% ")
           ,group = "Network Links (mid-points)"
           ,pal = pal_centroids_count
           ,opacity = 0.7
           ,values = query_poly_od_cntrd$count_adj_max) 
  )
  
}

#script end=====================================================================



