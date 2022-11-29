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

network_pnt = tar_read('mem_network_pnt') 

network_lnk = tar_read('mem_network_lnk') 

network_objects_poly = tar_read("mem_spatial_od_polys")

network_objects = tar_read('mem_network_objects_custom')

mem_data_trip_split
mem_query_poly_split
mem_data_trip_custom
mem_query_poly_custom
data_pro = process_trip_data(
  data = tar_read('mem_data_trip_split')
  ,query_poly = tar_read('mem_query_poly_split')
  ,rm_self = T, rm_ext = F
)

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

index_start_taz = DescTools::SortMixed(unique(data_pro$start_taz))

unnest_network_links = function(data, network_col = 'network_link_ids'){
  data %>%
    unnest(cols = network_col) %>%  
    data.table() %>% 
    .[,`:=`(network_link_ids_trunc = str_trunc(network_link_ids, 14, "right", ""))] %>% 
    data.table()
}

# unnest_network_links(head(data_pro), network_col = 'network_link_ids')

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

#prep poi polygon===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_query_poly = tar_read("mem_query_poly_custom")

query_poly = file_query_poly %>%  
  read_sf() %>%  
  mutate(across(c(starts_with("flag"), group), as.factor))

query_poly_poi = network_objects_poly$agg_o %>%  
  select(id, flag_poi, count_tll_origin, label) %>% 
  filter(flag_poi == "poi") 

pal_poi = colorNumeric(
  palette = "magma"
  ,query_poly_poi$count_tll_origin
  ,reverse = T)

query_poly_nonpoi = query_poly %>%  
  filter(flag_poi != "poi")

map_elemet_poi = function(base_map){
  base_map %>%  
    addPolygons(data = query_poly_poi
                ,fillColor = ~pal_poi(query_poly_poi$count_tll_origin)
                ,fillOpacity = .4
                ,color = "black", opacity = .5,weight = 2
                ,group = "POI Polygons"
                ,label = query_poly_poi$label %>%
                  map(htmltools::HTML)) %>% 
    addPolygons(data = query_poly_nonpoi
                ,fillColor = "green", fillOpacity = .1
                ,opacity = .4, weight = 1, color = "grey"
                ,group = "Non-POI Study Area Polygons"
    ) %>%  
    addLegend(
      position = "bottomright"
      ,title = HTML("Total Origin Counts")
      ,group = "POI Polygons"
      ,pal = pal_poi
      ,opacity = 0.7
      ,values = query_poly_poi$count_tll_origin) 
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

##internal-external links=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filtered_data = data_pro %>% 
  #filter_internal/external~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
  # filter(flag_sa_efilter_internal/internalnd == 'external') %>%  #only internal-to-external
  #filter_internal/internal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
  # filter(flag_sa_end != 'external') %>%  #only internal-to-external
  #filter_airpot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all filters
  # filter(start_taz %in% c("10_MEM Airport & FedEx")) %>% 
  #filter_rail~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
# filter(start_taz %in% c("1_Annesdale","4_Central South","6_Lamar"
#                         ,"8_Marion I-55","9_Marion Intermodal","11_Memphis Depot"
# )) 
#filter_port of memphis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
  # filter(start_taz %in% c("13_New Chicago","16_Presidents Island"
  #                         ,"17_River - West Memphis")) %>%
  filter(start_taz_group %in% c("Port of West Memphis"))
#filter_POI-to-POI~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# filter(start_taz != end_taz) %>%  #removes self-to-self - should be on for all
# filter(end_taz %in% index_start_taz) %>%  #keeps only poi-to-poi

  # unique(filtered_data$start_taz)

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

##editable map===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spatial_data = merge_processed_links(data = processed_data_ie
                                     ,network = network_pnt
                                     ,type = "POINT") 

spatial_data_sd = SharedData$new(spatial_data)

# color_variable = "count_adj_max"
# color_index = spatial_data[[color_variable]]
# pal_centroids_od = colorNumeric(
#   palette = "magma", color_index,reverse = T)

bscols(widths = c(12)
       ,filter_slider("net_sd_count", "Link Volume (vehicle counts):"
                      ,spatial_data_sd, ~count)
       ,filter_slider("net_sd_count_adj_max", "Link Volume (min/max normalized)"
                      ,spatial_data_sd, ~count_adj_max)
       ,leaflet(height = 700) %>%
         addTiles(group = "OSM (default)") %>%
         leaflet_default_tiles() %>%
         addCircleMarkers(data = spatial_data_sd
                          ,fillColor = "blue"
                          ,color = "blue"
                          ,opacity = .9
                          ,fillOpacity  = .9
                          ,weight = 1
                          ,radius = ~(count_adj_max*10)
                          ,group = "Network Links (mid-points)"
                          ,label = color_index) %>%
         map_elemet_poi() %>% 
         ###layer control----
       addLayersControl(
         baseGroups = leaflet_default_tiles_index
         ,overlayGroups =
           c("Network Links (mid-points)"
             ,"POI Polygons", "Non-POI Study Area Polygons")
         ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
         hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
         setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
         addMouseCoordinates() %>%
         ###legends----
       addLegend(
         position = "bottomleft"
         ,title = HTML("Link Volume")
         ,group = "Network Links (mid-points)"
         ,pal = pal_centroids_od
         ,opacity = 0.7
         ,values = color_index) 
)  

##static polyline map============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{
spatial_data = merge_processed_links(data = processed_data_ie
                                     ,network = network_lnk
                                     ,type = "LINESTRING")

min_old = min(spatial_data$count)

spatial_data_filtered = spatial_data #%>%  
  # filter(count >= 10)
  # filter(count_adj_max > .05)

min_filtered = min(spatial_data_filtered$count)

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
               ,label = color_index
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
  addMouseCoordinates() 

list(
  min_old = min_old
  ,min_filtered = min_filtered) %>%  print()

}

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# potential tables 

data_pro %>%  
  filter(start_taz != end_taz) %>%  
  count_percent_zscore_dt(
    grp_c = c('flag_sa_end')
    ,grp_p = c()
    ,col = 'count') %>%  
  print()

data_pro %>%  
  filter(start_taz != end_taz) %>%  
  count_percent_zscore_dt(
    grp_c = c('start_taz', 'flag_sa_end')
    ,grp_p = c('start_taz')
    ,col = 'count') %>%
  select(!percent) %>% 
  pivot_wider(names_from = 'flag_sa_end'
              ,values_from = 'count'
              ,names_glue = "{names}_ssss") %>%  
  mutate(total_trips = int)
  print()
  

#script end=====================================================================










































