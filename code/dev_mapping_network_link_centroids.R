


data_spatial_networks_object = tar_read("data_processed_queries")


network_agg_od = data_spatial_networks$network_agg_od

network_agg_od_mp = network_agg_od %>%  
  st_true_midpoint()

network_agg_od$n

pal_centroids_od = colorNumeric(
  "Blues",
  network_agg_od_mp$n)

network_agg_od_mp_sd = SharedData$new(network_agg_od_mp)


network_agg_od$destination_cluster
bscols(widths = c(3, 9)
       ,list(
         crosstalk::filter_select("origin_cluster", "origin_cluster"
                                  ,network_agg_od_mp_sd, ~origin_cluster)
         ,crosstalk::filter_select("destination_cluster", "destination_cluster"
                                   ,network_agg_od_mp_sd, ~destination_cluster)
         ,crosstalk::filter_slider("n", "n"
                                   ,network_agg_od_mp_sd, ~n)
       )
       ,leaflet() %>% 
         addTiles() %>%
         addCircleMarkers(data = network_agg_od_mp_sd
                          ,fillColor = ~pal_centroids(network_agg_od_mp$n)
                          ,color = "black"
                          ,opacity = .8
                          ,fillOpacity  = .5
                          ,weight = 1
                          ,radius = 5
                          ,group = "test"
                          # ,group = "All Collisions (filterable points)<hr><strong>Combined Tier Layers:</strong>"
                          # ,popup = popup_tbl_pretty(crash_links_points %>%
                          #                             select(!text))
                          ,label = network_agg_od_mp$n
                          ,labelOptions = labelOptions(noHide = F, textOnly = F)
         ) 
       
       )





























# cluster_object = tar_read("data_manual_cluster")
# 
# network = data_spatial_networks_object
# data_spatial_networks

data = tar_read("data_processed_queries")
cluster_object = tar_read("data_manual_cluster")

data$trips_agg_od %>% 
  mutate(across(c(origin_cluster, destination_cluster), as.numeric)) %>% 
  ggplot() + 
  geom_tile(aes(origin_cluster, destination_cluster, fill = n)) + 
  facet_grid(rows = vars(dataset))

# data_processed_queries




make_netowrk_map_od = function(data, cluster_object){
  
  # cluster = cluster_object$all %>%  
  #   mutate(index_cluster = row_number())
  # 
  # leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  network_agg = data$network_agg
  
  network_agg_mp = network_agg %>%  
    st_true_midpoint()
  
  pal_centroids = colorNumeric(
    "Blues",
    network_agg_mp$count_tot)
  
  network_agg_mp_sd = SharedData$new(network_agg_mp)
  
  bscols(widths = c(3, 9)
         ,list(
           crosstalk::filter_select("dataset2", "Choose Dataset: "
                                    ,network_agg_mp_sd, ~dataset)
           ,crosstalk::filter_slider("n2", "Link Count Slider:"
                                     ,network_agg_mp_sd, ~count_tot)
         )
         ,leaflet(height = 800) %>% 
           leaflet_default_tiles() %>% 
           addCircleMarkers(data = network_agg_mp_sd
                            ,fillColor = ~pal_centroids(network_agg_mp$count_tot)
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
           leafem::addMouseCoordinates()
         )
}




























