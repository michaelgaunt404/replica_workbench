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
query_poi_to_everything = function(data, db_connection, schema_table, limit = 50){
  
  limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(group != "no_group_id") %>%  
    select(group, id) 
  
  message("Begin query..")
  queried_data = poly_query_df$group %>%  
    unique() %>%  
    sort() %>% 
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
      
      temp_data = dbGetQuery(db_connection, query_string)
      
      message(paste0(nrow(temp_data), " rows returned..."))
      
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
  # data = tar_read('mem_query_poly')
  # schema_table ="wsp.south_central_2021_Q4_thursday_trip_taz"

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
  # network = tar_read('mem_network')
  # query_poly = tar_read('mem_query_poly')
  # data = tar_read('mem_data_trip')
  
  message("Preping data...")
  
  query_poly = query_poly %>%  
    read_sf() 
  
  query_poly_df = query_poly %>% 
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
  agg_od = data_pro %>% 
    count_percent_zscore(
      grp_c = c('dataset', 'start_taz_group', 'end_taz', 'vehicle_type')
      ,grp_p = c('dataset', 'start_taz_group', 'vehicle_type')
      ,col = count
      ,rnd = 2) %>%  
    arrange(start_taz_group, vehicle_type, desc(count)) %>%  
    group_by(dataset, start_taz_group, vehicle_type) %>%  
    mutate(count_pRank_adj = dgt2(percent_rank(count))) %>%
    mutate(count_adj_max = dgt2(count/max(count))) %>%
    ungroup() %>% 
    group_by(start_taz_group) %>%  
    mutate(ttl_count_orgin = sum(count)) %>%  
    ungroup() %>% 
    group_by(start_taz_group, vehicle_type) %>%  
    mutate(ttl_count_orgin_type = sum(count)) %>%  
    ungroup() %>% 
    rename(percent_origin_trips = percent)
  
  query_poly_od = query_poly %>%  
    merge(agg_od %>%  
            select(!dataset)
          ,by.x = c('id'), by.y = c("end_taz")) 

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
  
  list_objects = network_merged %>%  c(query_poly_od = list(query_poly_od)) 
  
  return(list_objects)
  
}

#map functions==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_network_map_anl = function(network_objects, file_query_poly){
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
  net_anl = network_objects$agg_network_link %>%  
    filter(!is.na(count) & !is.na(startLon)) %>% 
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                     ,T~NA_character_)) %>% 
    mutate(label = str_glue("Network Link: {streetName} <br> Type: {highway} <br> Link Volume {count}"))
  
  net_anl_cntrd = net_anl %>%  
    gauntlet::st_true_midpoint()
  
  net_anl_cntrd_sd = SharedData$new(net_anl_cntrd)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_anl_cntrd$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
           ,HTML("Network Link Volume Filters") %>%  strong()
           ,shiny::hr()
           ,filter_slider("net_anl_cntrd_sd_count", "Link Volume (vehicle counts):"
                          ,net_anl_cntrd_sd, ~count)
           ,filter_slider("net_anl_cntrd_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
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
           ,values = net_anl_cntrd$count) 
         )

}

make_network_map_anlt = function(network_objects, file_query_poly){
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
  net_anlt = network_objects$agg_network_link_type %>%  
    filter(!is.na(count) & !is.na(startLon)) %>% 
    mutate(flag_NHS = case_when((str_detect(streetName, "^US") |
                                   str_detect(streetName, "^I"))~streetName
                                ,T~NA_character_)) %>% 
    group_by(highway) %>% 
    mutate(count_std_hwy = dgt2(percent_rank(count))) %>%  
    ungroup() %>% 
    mutate(label = str_glue("Network Link: {streetName} <br> Type: {highway} <br> Vehicle: Type {vehicle_type} <br> Link Volume {count}"))
  
  net_anlt_cntrd = net_anlt %>%  
    gauntlet::st_true_midpoint()
  
  net_anlt_cntrd_sd = SharedData$new(net_anlt_cntrd)
  
  pal_centroids_od = colorNumeric(
    palette = "magma"
    ,net_anlt_cntrd$count
    ,reverse = T)
  
  #make map~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bscols(widths = c(3, 9)
         ,list(
           bscols(
             widths = c(12)
           ,filter_select("net_anlt_cntrd_sd_dataset", "Choose Dataset:"
                         ,net_anlt_cntrd_sd, ~dataset)
           ,filter_select("net_anlt_cntrd_sd_flag_NHS", "Choose Specific Rdwy:"
                          ,net_anlt_cntrd_sd, ~flag_NHS))
           ,bscols(
             widths = c(6, 6)
             ,filter_select("net_anlt_cntrd_sd_highway", "Choose Street Type:"
                            ,net_anlt_cntrd_sd, ~highway)
             ,filter_select("net_anlt_cntrd_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_anlt_cntrd_sd, ~vehicle_type)
           )
           ,HTML("Network Link Volume Filters") %>%  strong()
           ,shiny::hr()
           ,bscols(
             widths = c(12)
             ,filter_slider("net_anlt_cntrd_sd_count", "Link Volume (vehicle counts):"
                            ,net_anlt_cntrd_sd, ~count)
             ,filter_slider("net_anlt_cntrd_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
                            ,net_anlt_cntrd_sd, ~dgt2(log10(count)))
             ,filter_slider("net_anlt_cntrd_sd_count_std_hwy", "Link Volume % Rank (adj for road type):"
                            ,net_anlt_cntrd_sd, ~count_std_hwy))
         )
         ,leaflet(height = 700) %>%
           addTiles(group = "OSM (default)") %>%
           leaflet_default_tiles() %>%
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
           ,values = net_anlt_cntrd$count) 
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
    group_by(start_taz_group, vehicle_type) %>% 
    mutate(count_std = dgt2(percent_rank(count))) %>%  
    ungroup() %>% 
    group_by(highway) %>% 
    mutate(count_std_hwy = dgt2(percent_rank(count))) %>%  
    ungroup() %>% 
    mutate(label = str_glue("Origin Group: {start_taz_group} <br> Network Link: {streetName} 
                            <br> Type: {highway} <br> Vehicle: Type {vehicle_type} 
                            <br> Link Volume {count} <br> Link Volume {count_std} <br>(adj origin & veh)"))
  
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
           bscols(
             widths = c(12)
             ,filter_select("net_anlto_cntrd_sd_dataset", "Choose Dataset:"
                            ,net_anlto_cntrd_sd, ~dataset)
             ,filter_select("net_anlto_cntrd_sd_flag_NHS", "Choose Specific Rdwy:"
                            ,net_anlto_cntrd_sd, ~flag_NHS))
           ,bscols(
             widths = c(6, 6, 12)
             ,filter_select("net_anlto_cntrd_sd_highway", "Choose Street Type:"
                            ,net_anlto_cntrd_sd, ~highway)
             ,filter_select("net_anlto_cntrd_sd_vehicle_type", "Choose Vehicle Type:"
                            ,net_anlto_cntrd_sd, ~vehicle_type)
             ,filter_select("net_anlto_cntrd_sd_grp", "Choose Freight Terminal:"
                            ,net_anlto_cntrd_sd, ~start_taz_group)
           )
           ,HTML("Network Link Volume Filters") %>%  strong()
           ,shiny::hr()
           ,bscols(
             widths = c(12)
             ,filter_slider("net_anlto_cntrd_sd_count", "Link Volume (vehicle counts):"
                            ,net_anlto_cntrd_sd, ~count)
             ,filter_slider("net_anlto_cntrd_sd_countlg10", "Link Volume (vehicle counts - log10 adj):"
                            ,net_anlto_cntrd_sd, ~dgt2(log10(count)))
             ,filter_slider("net_anlto_cntrd_sd_count_std", "Link Volume % Rank (adj for orgin/vehicle):"
                            ,net_anlto_cntrd_sd, ~count_std)
             ,filter_slider("net_anlto_cntrd_sd_count_std_hwy", "Link Volume % Rank (adj for road type):"
                            ,net_anlto_cntrd_sd, ~count_std_hwy)
           )
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
           ,values = net_anlto_cntrd$count) 
         
  )
  
}

make_map_origin = function(network_objects, file_query_poly){
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
  query_poly_od = network_objects$query_poly_od %>% 
    group_by(start_taz_group, vehicle_type) %>%
    mutate(count_max = max(count)) %>%
    ungroup() %>%
    mutate(label = str_glue(
    "Origin Group: {start_taz_group} 
    <br> Total Origin Trips: {ttl_count_orgin}
    <br> Destination Poly: {id}
    <hr>
    Metrics  Adj for Vehicle Type 
    <br> Vehicle Type: {vehicle_type}
    <br> Total Origin Trips: {ttl_count_orgin_type}
    <br> Trips to Destination: {count}
    <br> % of Origin Total: {100*percent_origin_trips}%
    <br> Max Destination Count: {count_max}
    <br> % of Max OD Count: {100*count_adj_max}%
    <br> Count Percent Rank: {count_pRank_adj}"))   
  
  query_poly_od_cntrd = query_poly_od %>%  
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











                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 