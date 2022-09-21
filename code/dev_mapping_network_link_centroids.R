


data_spatial_networks_object = tar_read("data_spatial_networks")


network_agg_od = data_spatial_networks$network_agg_od

network_agg_od_mp = network_agg_od %>%  
  st_true_midpoint()

network_agg_od$n

pal_centroids = colorNumeric(
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


cluster_object = tar_read("data_manual_cluster")

network = data_spatial_networks_object
data_spatial_networks

network = tar_read("data_spatial_networks")
cluster_object = tar_read("data_manual_cluster")





































make_netowrk_map_od = function(network, cluster_object){
  
  cluster = cluster_object$all %>%  
    mutate(index_cluster = row_number())
  
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  network_agg_od = network$network_agg_od
  
  network_agg_od_mp = network_agg_od %>%  
    st_true_midpoint()
  
  pal_centroids = colorNumeric(
    "Blues",
    network_agg_od_mp$n)
  
  network_agg_od_mp_sd = SharedData$new(network_agg_od_mp)
  
  bscols(widths = c(3, 9)
         ,list(
           crosstalk::filter_select("origin_cluster", "origin_cluster"
                                    ,network_agg_od_mp_sd, ~origin_cluster)
           ,crosstalk::filter_select("destination_cluster", "destination_cluster"
                                     ,network_agg_od_mp_sd, ~destination_cluster)
           ,crosstalk::filter_slider("n", "n"
                                     ,network_agg_od_mp_sd, ~n)
         )
         ,leaflet(height = 800) %>% 
           leaflet_default_tiles() %>% 
           addCircleMarkers(data = network_agg_od_mp_sd
                            ,fillColor = ~pal_centroids(network_agg_od_mp$n)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,label = network_agg_od_mp$n
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

make_netowrk_map_od = function(network, cluster_object){
  
  cluster = cluster_object$all %>%  
    mutate(index_cluster = row_number())
  
  leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")
  
  network_agg_od = network$network_agg_od
  
  network_agg = network_agg_od %>% 
    st_drop_geometry() %>%  
    count_percent_zscore(grp_c = c(stableEdgeId, origin_cluster)
                         ,grp_p = c(stableEdgeId)
                         ,col = n, rnd = 2) %>%  
    arrange(origin_cluster) %>% 
    mutate(origin_cluster = str_glue("From Cluster: {origin_cluster}")) %>% 
    pivot_wider(names_from = origin_cluster
                ,values_from = percent)
  
  network_centroids = network_agg_od %>%  
    select(stableEdgeId, geometry) %>%  
    unique() %>% 
    st_true_midpoint()

  network_centroids_agg_comb = network_centroids %>%  
    merge(network_agg)
  
  pal_centroids = colorNumeric(
    "Blues",
    network_links_agg_comb$count)
  
  network_centroids_agg_comb_sd = SharedData$new(network_centroids_agg_comb)
  
  bscols(widths = c(3, 9)
         ,list(
           # crosstalk::filter_select("origin_cluster", "origin_cluster"
           #                          ,network_links_agg_comb, ~origin_cluster)
           # ,crosstalk::filter_select("destination_cluster", "destination_cluster"
           #                           ,network_links_agg_comb, ~destination_cluster)
           crosstalk::filter_slider("n", "n"
                                     ,network_centroids_agg_comb_sd, ~count)
         )
         ,leaflet(height = 800) %>% 
           leaflet_default_tiles() %>% 
           addCircleMarkers(data = network_centroids_agg_comb_sd
                            ,fillColor = ~pal_centroids(network_centroids_agg_comb$count)
                            ,color = "black"
                            ,opacity = .8
                            ,fillOpacity  = .5
                            ,weight = 1
                            ,radius = 5
                            ,group = "Network Links (mid-points)"
                            ,label = network_centroids_agg_comb$count
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




























