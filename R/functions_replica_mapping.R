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

#map functions==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_network_map_anl = function(network_objects
                                ,network_objects_poly
                                ,file_query_poly){
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

make_network_map_anlt = function(network_objects
                                 ,network_objects_poly
                                 ,file_query_poly){
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

make_network_map_anlto = function(network_objects
                                  ,network_objects_poly
                                  ,file_query_poly){
  
  # network_objects = tar_read('mem_network_objects_custom')
  # network_objects_poly = tar_read('mem_spatial_od_polys')
  # file_query_poly = tar_read("mem_query_poly_custom")

  #global vars/objects~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")
  
  #prep poi polygons~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

make_map_origin = function(network_objects_poly
                           ,file_query_poly){
  # network_objects_poly = tar_read('mem_spatial_od_polys')
  # file_query_poly = tar_read("mem_query_poly_custom")
  
  #global vars/objects~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")
  
  #prep poi polygons~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  #prep data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  query_poly_od = network_objects_poly$agg_od
  
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
                            ,group = "Destination TAZs (centroids)"
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



