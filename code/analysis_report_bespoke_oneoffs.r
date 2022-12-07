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

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

##network data----
file_location = "data/memphis_req/data_queried_network"
sa_motorway_network = here(str_glue("{file_location}/sa_motorway_network.rds")) %>%  readRDS()
sa_primary_network = here(str_glue("{file_location}/sa_primary_network.rds")) %>%  readRDS()
sa_secondary_network = here(str_glue("{file_location}/sa_secondary_network.rds")) %>%  readRDS()
sa_tertiary_network = here(str_glue("{file_location}/sa_tertiary_network.rds")) %>%  readRDS()
sa_trunk_network = here(str_glue("{file_location}/sa_trunk_network.rds")) %>%  readRDS()

network_lnk = list(
  sa_motorway_network
  ,sa_primary_network
  ,sa_secondary_network
  ,sa_tertiary_network
  ,sa_trunk_network
) %>%  
  reduce(bind_rows) %>% 
  mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>% 
  st_as_sf(wkt = "geometry", crs = 4326) 

##normal data----
# mem_data_trip_split
# mem_query_poly_split
# mem_data_trip_custom
# mem_query_poly_custom
data_pro = process_trip_data(
  data = tar_read('mem_data_trip_split')
  ,query_poly = tar_read('mem_query_poly_split')
  ,rm_self = T, rm_ext = F
)

index_start_taz = DescTools::SortMixed(unique(data_pro$start_taz))

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

unnest_network_links = function(data, network_col = 'network_link_ids'){
  data %>%
    unnest(cols = network_col) %>%  
    data.table() %>% 
    .[,`:=`(network_link_ids_trunc = str_trunc(network_link_ids, 14, "right", ""))] %>% 
    data.table()
}

merge_processed_links = function(data, network, type){
  
  network_processed = network %>%  
    filter(stableEdgeId_trunc %in% unique(data$network_link_ids_trunc))
  
  merge(network_processed
        ,data 
        ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_trunc", all = T) %>%  
    mutate(sf_geom_typ = st_geometry_type(geometry)) %>% 
    filter(sf_geom_typ == type) %>%  
    filter(!is.na(count) & !is.na(startLon))
}

leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")

##prep poi polygon===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_query_poly = tar_read("mem_query_poly_split")
# file_query_poly = tar_read("mem_query_poly_custom")

query_poly = file_query_poly %>%  
  read_sf() %>%  
  mutate(across(c(starts_with("flag"), group), as.factor))

query_poly_poi = query_poly %>%  
  select(id, flag_poi, group#, count_tll_origin, label
         ) %>% 
  filter(flag_poi == "poi") 

query_poly_nonpoi = query_poly %>%  
  filter(flag_poi != "poi")

map_elemet_poi = function(base_map){
  base_map %>%  
    addPolygons(data = query_poly_poi
                # ,fillColor = ~pal_poi(query_poly_poi$count_tll_origin)
                ,fillOpacity = .4
                ,color = "black", opacity = .5,weight = 2
                ,group = "POI Polygons"
                ,label = ~group
                ) %>% 
    addPolygons(data = query_poly_nonpoi
                ,fillColor = "green", fillOpacity = .1
                ,opacity = .4, weight = 1, color = "grey"
                ,group = "Non-POI Study Area Polygons"
    ) #%>%  
    # addLegend(
    #   position = "bottomright"
    #   ,title = HTML("Total Origin Counts")
    #   ,group = "POI Polygons"
    #   ,pal = pal_poi
    #   ,opacity = 0.7
    #   ,values = query_poly_poi$count_tll_origin) 
} 

#connectivity====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##freight clusters==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#poi-to-poi links counts
processed_data = data_pro %>%  
  filter(start_taz != end_taz) %>% #removes self-to-self
  filter(flag_sa_end != 'external') %>%  #removes external trips
  filter(end_taz %in% index_start_taz) %>%  #keeps only poi-to-poi
  unnest_network_links(.) %>%  
  count_percent_zscore_dt(
    grp_c = c('network_link_ids_trunc')
    ,grp_p = c()
    ,col = 'count') %>% 
  .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
          ,count_adj_max = dgt2(count/max(count)))] 

spatial_data = merge_processed_links(data = processed_data
                             ,network = network_pnt) 

spatial_data_sd = SharedData$new(spatial_data)

color_variable = "count_adj_max"
color_index = spatial_data[[color_variable]]

pal_centroids_od = colorNumeric(
  palette = "magma",color_index,reverse = T)

bscols(widths = c(12)
       ,filter_slider("net_sd_count", "Link Volume (vehicle counts):"
                      ,spatial_data_sd, ~count)
       ,filter_slider("net_sd_count_adj_max", "Link Volume (min/max normalized)"
                      ,spatial_data_sd, ~count_adj_max)
       ,leaflet(height = 700) %>%
         addTiles(group = "OSM (default)") %>%
         leaflet_default_tiles() %>%
         addCircleMarkers(data = spatial_data_sd
                        ,fillColor = ~pal_centroids_od(color_index)
                        ,color = "black"
                        ,opacity = .8
                        ,fillOpacity  = .5
                        ,weight = 1
                        ,radius = ~(count_adj_max*10)
                        ,group = "Network Links (mid-points)"
                        ,label = color_index) %>%
         map_elemet_poi() %>% 
         #layer control--
       addLayersControl(
         baseGroups = leaflet_default_tiles_index
         ,overlayGroups =
           c("Network Links (mid-points)"
             ,"POI Polygons", "Non-POI Study Area Polygons")
         ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
         hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
         setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
         addMouseCoordinates() %>%
         ##legends~~~~
       addLegend(
         position = "bottomleft"
         ,title = HTML("Link Volume")
         ,group = "Network Links (mid-points)"
         ,pal = pal_centroids_od
         ,opacity = 0.7
         ,values = color_index) 
)    

spatial_data_for_map = merge_processed_links(data = processed_data
                                     ,network = network) %>%  
  filter(count > 9) %>%  
  filter(count_adj_max >= .1)

spatial_data_for_map %>%  
  summarise(min(count))

##Terminals=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(data_pro %>% select(start_taz_group, start_taz, flag_poi_end, flag_sa_end))

filtered_data = data_pro %>% 
###filter_poi to internal----
filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
filter(flag_sa_end != 'external')  #only internal-to-external
###filter_fedex----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
#   filter(str_detect(start_taz_group, "Fed"))
###filter_airport----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
#   filter(str_detect(start_taz_group, "MEM Airport"))
###filter_rail----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
#   filter(str_detect(start_taz_group, "BNSF") |
#            str_detect(start_taz_group, "UP") |
#            str_detect(start_taz_group, "NS") |
#            str_detect(start_taz_group, "CSX"))
###filter_port of memphis----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
# filter(start_taz_group %in% c("Port of West Memphis"))
###filter_POI-to-POI----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
#   filter(end_taz %in% index_start_taz)   #keeps only poi-to-poi
###filter_POI to everything----
# filter(start_taz != end_taz)
###end----

unique(filtered_data %>% select(start_taz_group, start_taz, flag_poi_end, flag_sa_end))

processed_data = filtered_data %>%  
  unnest_network_links(.) %>%  
  count_percent_zscore_dt(
    grp_c = c('network_link_ids_trunc', "flag_sa_end")
    ,grp_p = c('network_link_ids_trunc')
    ,col = 'count') %>% 
  .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
          ,count_adj_max = dgt2(count/max(count)))
    ,by = .(flag_sa_end)] 

processed_data_ie = processed_data %>%  
  filter(flag_sa_end != "external")


##static polyline map============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  spatial_data = merge_processed_links(data = processed_data_ie
                                       ,network = network_lnk
                                       ,type = "LINESTRING")
  
  # min_old = min(spatial_data$count)
  
  spatial_data_filtered = spatial_data %>%  
    # filter(count >= 10)
    filter(count_adj_max >= .01)
  
  # min_filtered = min(spatial_data_filtered$count)
  
}

mapview(spatial_data_filtered
        ,lwd)

leaflet(height = 700) %>%
  addTiles(group = "OSM (default)") %>%
  leaflet_default_tiles() %>%
  addPolylines(data = spatial_data_filtered
               # ,fillColor = ~pal_centroids_od(color_index)
               # ,color = ~pal_centroids_od(color_index)
               # ,width = 5
               ,opacity = 1
               ,fillOpacity  = .5
               ,weight = rescale_to(spatial_data_filtered$count, 20)
               # ,radius = ~(count_adj_max*10)
               ,group = "Network Links (mid-points)"
               ,label = ~count
  ) %>%
  map_elemet_poi() %>% 
  #layer control---
addLayersControl(
  baseGroups = leaflet_default_tiles_index
  ,overlayGroups =
    c("Network Links (mid-points)"
      ,"POI Polygons", "Non-POI Study Area Polygons")
  ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
  hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
  setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
  addMouseCoordinates() 

# list(
#   min_old = min_old
#   ,min_filtered = min_filtered) %>%  print()

# file_name = "terminal_links_fedEx_to_internal.gpkg"
# file_name = "terminal_links_airport_to_internal.gpkg"
# file_name = "terminal_links_memPortwithFedEx_to_internal.gpkg"
# file_name = "terminal_links_rail_to_internal.gpkg"
# file_name = "terminal_links_poi_to_poi.gpkg"
# file_name = "terminal_links_poi_to_internal.gpkg"
# file_name = "clusters_links_poi_to_poi.gpkg"
# file_name = "clusters_links_poi_to_internal.gpkg"
# file_name = "clusters_links_poi_to_external.gpkg"

here("data/memphis_req/data_for_report", file_name) %>%  
  write_sf(spatial_data_filtered, .)

##static poly map============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  processed_data_poly = filtered_data %>%  
    select(!network_link_ids) %>%  
    count(end_taz, name = "count", sort = T) %>%  
    mutate(count_adj_max = count/max(count))
  
  
  spatial_data_poly = merge(query_poly
                            ,processed_data_poly
                            ,by.x = c('id'), by.y = c("end_taz")
                            ,all = T)
  
  # min_old = min(spatial_data_poly$count)
  
  spatial_data_poly_filtered = spatial_data_poly  
  # filter(count >= 10)
  # filter(count_adj_max >= .01)
  
  # min_filtered = min(spatial_data_filtered$count)
  
}

mapview(spatial_data_poly, zcol = "count")
{
  (spatial_data_poly %>%  
     filter(count >= 5) %>%
     na.omit() %>% 
     mapview(zcol = "count", layer.name = "od") +
(spatial_data_filtered %>%  
    mutate(lwd = rescale_to(count, 25)) %>% 
    mapview(
      # color = "blue"
      # ,zcol = "count"
      label = "count"
      ,alpha = 1
      ,lwd = "lwd") ) )

}
# file_name = "terminal_polyspatial_data_polys_airportwithFedEx_to_internal.gpkg"
# file_name = "terminal_polys_rail_to_internal.gpkg"
file_name = "terminal_polys_airportwithFedEx_to_internal.gpkg"

mapview(spatial_data_poly, zcol = "count")

here("data/memphis_req/data_for_report", file_name) %>%  
  write_sf(spatial_data_poly, .)

#network by location============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_pro %>%  
  select(start_taz, start_taz_group) %>%  
  unique()

data_pro_net = data_pro %>%  
  mutate(start_taz_group = str_remove_all(start_taz_group, "[:punct:]"))
  # mutate(start_taz = gsub(".*_", "\\1", start_taz))

unique(data_pro_net$start_taz_group) %>%  
  map(~{
    print(.x)
    temp_data = data_pro_net %>%  
      filter(start_taz_group == .x) %>%  
      unnest_network_links(.) %>%  
      .[,.(count = sum(count))
        ,by = .(start_taz_group, network_link_ids_trunc)] %>%  
      .[,`:=`(count_adj_max = count/max(count))]
    
    print(nrow(temp_data))
    
    temp_gis = merge_processed_links(data = temp_data
                          ,network = network_lnk
                          ,type = "LINESTRING")
    print(nrow(temp_gis))
    here("data/memphis_req/terminal_networks"
         ,str_glue("network_{.x}.gpkg")) %>%  
      write_sf(temp_gis, .)
    
      # mapview(temp_gis, lwd = "count", zcol = "count_adj_max", layer.name = .x)
    
  })

test = data_pro %>%  
  sample_n(1e3) %>%
  unnest_network_links(.) %>%  
  .[,.(count = sum(count)), by = .(start_taz, network_link_ids_trunc)]
 
{
yolo = test %>%  
  .[start_taz == "6_Lamar",] %>%  
  .[count >= 3]

 bolo =  merge_processed_links(data = yolo
                        ,network = network_lnk
                        ,type = "LINESTRING")
 
 bolo %>%  
   mapview(lwd = "count")
}

tmppp = here("data/memphis_req/terminal_networks"
     ,str_glue("network_BNSF  Memphis Shelby IMX.gpkg")) %>%  
  read_sf(.)

mapview(tmppp, lwd = "count", zcol = "count_adj_max")


##Polygons=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# filtered_data = data_pro %>% 
#   ###filter_airport----
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
#   filter(str_detect(start_taz_group, "MEM Airport - General"))

###end----

unique(filtered_data %>%  
         select(start_taz_group, start_taz))

processed_data = filtered_data %>%  
  select(!network_link_ids) %>%  
  count(end_taz, name = "count", sort = T) %>%  
  mutate(count_adj_max = count/max(count))

# processed_data_ie = processed_data %>%  
#   filter(flag_sa_end == "external")















