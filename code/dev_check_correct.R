




filtered_data %>%  
  nrow()


#number of trips
filtered_data %>%  select(!network_link_ids) %>%  nrow()

#number of trips after network unnesting: RESULT GOOD MATCHES ABOVE
filtered_data %>% unnest_network_links(.) %>% count(activity_id) %>% nrow()

#number of links after network unnesting
filtered_data %>% unnest_network_links(.) %>% count(activity_id) %>%  summarise(count = sum(n))

#number of links after network unnesting and agg: RESULT GOOD MATCHES ABOVE
processed_data %>% summarise(count = sum(count))

#number of links after network unnesting and agg: RESULT GOOD MATCHES ABOVE
processed_data_ie %>% summarise(count = sum(count))

spatial_data = merge_processed_links(data = processed_data_ie
                                     ,network = network_lnk
                                     ,type = "LINESTRING")

#spatial 
#has the same number of trips of filtered data: NO LEAKAGE GOOD 
processed_data_poly %>%  summarise(count = sum(count))

#same as above: NO LEAKAGE GOOD 
spatial_data_poly %>% st_drop_geometry() %>%  summarise(count = sum(count, na.rm = T))  


spatial_data %>% summarise(count = sum(count))


# mapview(spatial_data_poly_filtered, zcol = "count") + 
(spatial_data_filtered %>%  
    mutate(lwd = rescale_to(count, 25)) %>% 
    mapview(
      zcol = "count"
      ,lwd = "lwd") )+ mapview(spatial_data_poly, zcol = "count")






spatial_data_poly_filtered




processed_data



# file_name = "terminal_links_airport_to_internal.gpkg"
# file_name = "terminal_links_memPortwithFedEx_to_internal.gpkg"
# file_name = "terminal_links_rail_to_internal.gpkg"
# file_name = "terminal_links_poi_to_poi.gpkg"
# file_name = "terminal_links_poi_to_internal.gpkg"
# file_name = "clusters_links_poi_to_poi.gpkg"
# file_name = "clusters_links_poi_to_external.gpkg"
# file_name = "terminal_links_rail_to_internal.gpkg"
# file_name = "terminal_polys_rail_to_internal.gpkg"
# file_name = "terminal_polys_airportwithFedEx_to_internal.gpkg"
file_name = "terminal_links_fedEx_to_internal.gpkg"


# file_name = "clusters_links_poi_to_poi.gpkg"
# file_name = "clusters_links_poi_to_external.gpkg" --good

spatial_data_filtered = here("data/memphis_req/data_for_report", file_name) %>%  
  read_sf()

spatial_data_filtered %>%
  filter(count_adj_max >= .02) %>%
  mapview(lwd = "count")

temp = spatial_data_filtered %>%  
  filter(count_adj_max >= .01)
  
leaflet(height = 700) %>%
  addTiles(group = "OSM (default)") %>%
  leaflet_default_tiles() %>%
  addPolylines(data = spatial_data_filtered
               ,opacity = 1
               ,fillOpacity  = .5
               ,weight = rescale_to(spatial_data_filtered$count, 20)
               ,group = "Network Links (mid-points)"
               ,label = ~count
  ) %>% 
  map_elemet_poi()

location = "data/memphis_req/data_for_report"





rails_links = here(location, "terminal_links_rail_to_internal.gpkg") %>% read_sf()
rails_polys = here(location, "terminal_polys_rail_to_internal.gpkg") %>% read_sf()
mapview(rails_polys, zcol = "count") + mapview(rails_links, lwd = "count")

ap_links = here(location, "terminal_links_memPortwithFedEx_to_internal.gpkg") %>% read_sf()
ap_polys = here(location, "terminal_polys_airportwithFedEx_to_internal.gpkg") %>% read_sf()
mapview(ap_polys, zcol = "count") + mapview(ap_links, lwd = "count")



