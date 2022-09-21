


st_true_midpoint = function(sf_object){
  #gets the true midpoint along a curved line
  temp = sf_object %>%
    mutate(merge_id = row_number())
  
  #new CRS, cast to linestring, selects cols
  sf_object_linestring = temp %>%
    st_transform(2781) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)
  
  #make coords df, pull middle point
  coords_extract = sf_object_linestring %>%
    st_line_sample(n = 5) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    data.frame() %>%
    merge(sf_object_linestring %>%
            st_drop_geometry(),
          by.x = "L1", by.y = "linestring_id") %>%
    group_by(merge_id) %>%
    mutate(n = ceiling(n()/2),
           index = row_number()) %>%
    filter(n == index) %>%
    ungroup() %>%
    select(X, Y, merge_id)
  
  #convert df to spatial
  temp %>%
    st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}




data_spatial_networks = tar_read("data_spatial_networks")

library(crosstalk)

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































