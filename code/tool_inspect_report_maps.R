leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")

##get trip data=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data = tar_read("mem_data_trip_custom_0219")
data_21 = tar_read("mem_data_trip_custom")
data_use = "mem_data_trip_custom"
data_use = "mem_data_trip_custom_0219"
poly_use = "mem_query_poly_custom"
# data_use = "mem_data_trip_split"
# poly_use = "mem_query_poly_split"

data_pro = process_trip_data(
  data = tar_read("mem_data_trip_custom")
  ,query_poly = tar_read("mem_query_poly_custom")
  ,rm_self = T, rm_ext = F
)

data_pro %>% count(start_taz)
# data_21 %>% count(start_taz)

identical(data_pro_21, data_pro_19)

# data_pro_21 = data_pro_19
# data_pro_19 = data_pro

{
  poly_table = data_pro %>%  
    count(
      start_taz
      # start_taz_group
      ,flag_sa_end, flag_poi_end, name = "count") %>%  
    filter(flag_sa_end != "external") %>%  
    count_percent_zscore(
      grp_c = c(
        start_taz
        # start_taz_group
        ,flag_poi_end
      )
      ,grp_p = c(
        # start_taz_group
        start_taz
        )
      ,col = count, rnd = 2
    )

poly_table_sd = SharedData$new(poly_table)

data_agg = data_pro %>%  
  count(start_taz, flag_sa_end, name = "count") %>%  
  pivot_wider(names_from = "flag_sa_end"
              ,values_from = "count"
              ,names_glue = "count_{flag_sa_end}") %>% 
#BELOW CODE NOT RELEVANT FOR FREIGHT TERMINALS
# %>%
  mutate(across(
    c(starts_with("count"))
    ,~100*dgt2(.x/(count_external+count_internal))
    ,.names = "{str_replace(.col, 'count', 'percent')}"))
}

##generate tables for cluster===================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_pro %>% janitor::tabyl(flag_sa_end)
data_pro %>% janitor::tabyl(start_taz, flag_sa_end)
data_pro %>% janitor::tabyl(flag_poi_end)
data_pro %>% janitor::tabyl(start_taz_group, start_taz, flag_poi_end) %>% clipr::write_clip()
data_pro %>% 
  count_percent_zscore(
    grp_c = c('start_taz_group', 'start_taz', 'flag_poi_end')
    ,grp_p = c('start_taz_group', 'start_taz')
    ,col = count) %>%  
  arrange(desc(count))
# data_pro %>% 
#   filter(flag_sa_end == "internal") %>%  
#   count(start_taz, name = "count", sort = T) %>% 
#   mutate(start_taz = gsub(".*_", "\\1", start_taz)) %>% clipr::write_clip()
# 
# data_pro %>% 
#   filter(flag_sa_end != "internal") %>%  
#   count(start_taz, name = "count", sort = T) %>% 
#   mutate(start_taz = gsub(".*_", "\\1", start_taz)) %>% clipr::write_clip()
# 
data_pro %>%
filter(flag_poi_end == "poi") %>%
  count(start_taz, name = "count", sort = T) %>%
  mutate(start_taz = gsub(".*_", "\\1", start_taz)) %>% clipr::write_clip()
# 
# data_pro %>% 
#   filter(str_detect(start_taz, "general")) %>%  
#   count(start_taz, end_taz, name = "count", sort = T) %>% 
#   mutate(start_taz = gsub(".*_", "\\1", start_taz)
#          ,end_taz = gsub(".*_", "\\1", end_taz))

###vehicle type=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_pro %>% janitor::tabyl(vehicle_type)
data_pro %>% 
  count_percent_zscore(
    grp_c = c('start_taz', 'vehicle_type')
    ,grp_p = c('start_taz')
    ,col = count) %>%  
  arrange(desc(count)) %>%  
  mutate(start_taz = gsub(".*_", "\\1", start_taz)) %>% 
  pivot_wider(id_cols = "start_taz", names_from = "vehicle_type", values_from = "count") %>%  
  clipr::write_clip()

###split_data_spec==========================r=====================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_pro %>% 
  count_percent_zscore(
    grp_c = c('start_taz_group', 'start_taz')
    ,grp_p = c()
    ,col = count) %>%  
  arrange(desc(count)) %>% clipr::write_clip()

##prep poi polygon==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_query_poly = tar_read("mem_query_poly_split")
# file_query_poly = tar_read("mem_query_poly_custom")

{
query_poly = file_query_poly %>%  
  read_sf() %>%  
  mutate(across(c(starts_with("flag"), group), as.factor))

query_poly_poi = query_poly %>%  
  select(id, flag_poi, group) %>% 
  filter(flag_poi == "poi") %>%  
  merge(data_agg
        ,by.x = "id", by.y = 'start_taz') %>%  
  merge(poly_table %>%  
          filter(flag_poi_end == "poi") %>%  
          select(start_taz, percent) %>%  
          rename(percent_to_poi = percent)
        ,by.x = "id", by.y = 'start_taz') 
  
pal_poi = colorNumeric(
  palette = "magma"
  ,query_poly_poi$count_internal
  ,reverse = T)

pal_poi_to_poi = colorNumeric(
  palette = "magma"
  ,query_poly_poi$percent_to_poi
  ,reverse = F)

query_poly_nonpoi = query_poly %>%  
  filter(flag_poi != "poi")

map_elemet_poi = function(base_map){
  base_map %>%  
    addPolygons(data = query_poly_poi
                ,fillColor = ~pal_poi(query_poly_poi$count_internal)
                ,fillOpacity = .4
                ,color = "black", opacity = .5,weight = 2
                ,group = "POI Polygons (count)"
                ,label = ~paste0(group, " - ", count_internal)
    ) %>% 
    addPolygons(data = query_poly_poi
                ,fillColor = ~pal_poi_to_poi(query_poly_poi$percent_to_poi)
                ,fillOpacity = .4
                ,color = "black", opacity = .5,weight = 2
                ,group = "POI Polygons (% poi)"
                ,label = ~paste0(group, " - ", percent_to_poi)
    ) %>% 
    addPolygons(data = query_poly_nonpoi
                ,fillColor = "green", fillOpacity = .1
                ,opacity = .4, weight = 1, color = "grey"
                ,group = "Non-POI Study Area Polygons"
    ) %>%
    addLegend(
      position = "bottomright"
      ,title = HTML("Total Origin Counts")
      ,group = "POI Polygons (count)"
      ,pal = pal_poi
      ,opacity = 0.7
      ,values = query_poly_poi$count_internal) %>%
    addLegend(
      position = "bottomright"
      ,title = HTML("% to POIs")
      ,group = "POI Polygons (% poi)"
      ,pal = pal_poi_to_poi
      ,opacity = 0.7
      ,values = query_poly_poi$percent_to_poi)
} 
}

#prep poi polygon==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_name = "terminal_links_fedEx_to_internal.gpkg"
# file_name = "terminal_links_airport_to_internal.gpkg"
# file_name = "terminal_links_memPort_to_internal.gpkg"
# file_name = "terminal_links_rail_to_internal.gpkg"
# file_name = "terminal_links_poi_to_poi.gpkg"
# file_name = 'terminal_links_poi_to_internal.gpkg'
# file_name = "clusters_links_poi_to_poi.gpkg"
# file_name = "clusters_links_poi_to_external.gpkg"

#prepping for map
{
spatial_data_filtered = here("data/memphis_req/data_for_report", file_name) %>%  
  read_sf() %>%  
  mutate(label = str_glue("{streetName} ---- {count} ---- {count_adj_max}"))

color_variable = "count"
color_index = spatial_data_filtered[[color_variable]]

pal_centroids_od = colorNumeric(
  palette = "magma",color_index,reverse = T)
}

#making map
bscols(
  widths = c(8, 4)
  ,leaflet(height = 700) %>%
    addTiles(group = "OSM (default)") %>%
    leaflet_default_tiles() %>%
    addPolylines(data = spatial_data_filtered
                 ,color = ~pal_centroids_od(color_index)
                 # ,width = 5
                 ,opacity = 1
                 ,fillOpacity  = .5
                 ,weight = rescale_to(spatial_data_filtered$count, 20)
                 # ,radius = ~(count_adj_max*10)
                 ,group = "Network Links (mid-points)"
                 ,label = ~label 
    ) %>%
    map_elemet_poi() %>% 
    #layer control---
    addLayersControl(
      baseGroups = leaflet_default_tiles_index
      ,overlayGroups =
        c("Network Links (mid-points)", "POI Polygons (count)"
          ,"POI Polygons (% poi)", "Non-POI Study Area Polygons")
      ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
    hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>% 
    setView(lng= -89.96, lat = 35.11, zoom = 11) %>%
    addMouseCoordinates()
  ,bscols(
    widths = c(12, 12)
    ,crosstalk::filter_slider("percent", "percent select", poly_table_sd, ~percent)
    ,reactable::reactable(
      poly_table_sd
      ,filterable = T, highlight = TRUE
      ,compact = TRUE, fullWidth = T
      ,wrap = FALSE, resizable = TRUE
      ,height = 700, pagination = FALSE)
  )
)

##existing area manual calcualtion==============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#really used once for the cluster analysis
#make circle for external trips
# yolo = mapedit::drawFeatures() %>%  
#   st_transform(4326)
# 
# spatial_data_filtered %>%  
#   st_filter(yolo) %>% 
#   mutate(count_p = dgt2(count/sum(count))) %>% 
#   select(streetName, count, count_p) %>% 
#   mapview(zcol = "count_p")
# 
# 4900 + 3300
