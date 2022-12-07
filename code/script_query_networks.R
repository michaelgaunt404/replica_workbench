

network_pnt = tar_read('mem_network_pnt') 

network_lnk = tar_read('mem_network_lnk') 



network_pnt %>%  
  pull(highway)

network_pnt %>%  
  st_drop_geometry() %>%  
  filter(str_detect(highway, '_link')) %>%  nrow()


network_pnt %>%  
  st_drop_geometry() %>%  
  mutate(count = 1) %>% 
  count_percent_zscore(
    grp_c = c('highway')
    ,grp_p = c()
    ,col = count
  )


yolo = mapedit::drawFeatures()

yolo %>%  
  st_bbox()
  
network_pnt_filtered = network_pnt %>%  
  st_filter(yolo)

  network_pnt_filtered %>%  
    st_drop_geometry() %>% 
    count(highway, sort = T)
  
network_pnt_filtered %>%  
  # filter(highway != "residential") %>% 
  filter(!str_detect(highway, '_link')) %>% 
  # filter(str_detect(highway, "secondary")) %>% 
  # sample_n(10000) %>% 
  mapview(zcol = "highway", burst = T)




network_pnt_filtered = network_lnk %>%  
  st_filter(yolo)

network_pnt_filtered %>%  
  filter(highway != "residential") %>%
  filter(!str_detect(highway, '_link')) %>%  
  mapview(zcol = "highway", burst = T)


#section: NETWORK QUERY===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#make connection
con <- dbConnect(
  bigrquery::bigquery(),
  project = "replica-customer",
  dataset = "replica-customer.south_central.south_central_2019_Q4_network_segments"
)

#get sa polys
file_query_poly = tar_read("mem_query_poly_split")

query_poly = file_query_poly %>%  
  read_sf() 

# query_poly_union = query_poly %>%  
#   st_union()

# bounding_box_lrg = query_poly_union %>%  
#   quick_buffer(rad = 100000) %>%
#   st_bbox()

bounding_box_sa = query_poly %>%  
  st_bbox()

#define highway type
highway_type = c("motorway") %>%  
  paste0("'", ., "'", collapse = ", ")

highway_type = c("primary", "secondary", "tertiary") %>%  
  paste0("'", ., "'", collapse = ", ")

highway_type = c("residential") %>%  
  paste0("'", ., "'", collapse = ", ")

#make query
bounding_box = bounding_box_sa
query_string = str_glue("SELECT *
from 
`replica-customer.south_central.south_central_2021_Q4_network_segments`
where 
--highway = 'motorway'
--highway = 'primary'
--highway = 'secondary'
--highway = 'tertiary'
highway = 'trunk'
AND
(startLat > {bounding_box[[2]]} AND startLat < {bounding_box[[4]]}) AND
(endLat > {bounding_box[[2]]} AND endLat < {bounding_box[[4]]}) AND
(startLon > {bounding_box[[1]]} AND startLon < {bounding_box[[3]]}) AND
(endLon > {bounding_box[[1]]} AND endLon < {bounding_box[[3]]})
--limit 100")


network = dbGetQuery(con, query_string)
network %>%  
  count(highway)

# sa_motorway_network = network
# ##sa_pst_network = network
# sa_primary_network = network
# sa_secondary_network = network
# sa_tertiary_network = network
# sa_trunk_network = network


file_location = "data/memphis_req/data_queried_network"
here(file_location, "sa_trunk_network.rds") %>% saveRDS(sa_trunk_network, .)

# sa_motorway_network = here(str_glue("{file_location}/sa_motorway_network.rds")) %>%  readRDS()
# # sa_pst_network = network
# sa_primary_network = here(str_glue("{file_location}/sa_primary_network.rds")) %>%  readRDS()
# sa_secondary_network = here(str_glue("{file_location}/sa_secondary_network.rds")) %>%  readRDS()
# sa_tertiary_network = here(str_glue("{file_location}/sa_tertiary_network.rds")) %>%  readRDS()
# sa_trunk_network = here(str_glue("{file_location}/sa_trunk_network.rds")) %>%  readRDS()



list(sa_motorway_network
     ,sa_primary_network
     ,sa_secondary_network
     ,sa_trunk_network) %>%  
  map(~count(.x, highway))

list(sa_motorway_network
     ,sa_primary_network
     ,sa_secondary_network) %>%  
  map(~{
    .x %>%  
      filter(str_detect(str_to_lower(streetName), "78") |
               str_detect(str_to_lower(streetName), "lamar"))
  }) %>%  
  reduce(bind_rows) %>%  
  st_as_sf(wkt = "geometry", crs = 4326) %>%  
  mapview()



keep = list(sa_motorway_network
            ,sa_primary_network
            ,sa_secondary_network
            ,sa_tertiary_network
            ,sa_trunk_network) %>%  
  map(~{
    .x %>%  
      st_as_sf(wkt = "geometry", crs = 4326) %>%  
      st_filter(yolo) 
    # st_union()
  }) %>%  
  reduce(bind_rows)

keep %>%  
  reduce(rbind)

keep %>% 
  mapview(zcol = "highway")
















here("data/memphis_req/data_queried_network", "sa_motorway_network.rds") %>%  
  saveRDS(sa_motorway_network, .)

here("data/memphis_req/data_queried_network", "sa_pst_network.rds") %>%  
  saveRDS(sa_pst_network, .)


bolo = network %>%  
  head(6)

network_pro = network %>%  
  # sample_n(20000) %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%  
  st_filter(query_poly_union)

network_pro %>%  
  # filter(str_detect(str_to_lower(streetName), "78") |
  #          str_detect(str_to_lower(streetName), "lamar")) %>% 
  # filter(streetName == "MS 4") %>% 
  # st_filter(yolo) %>%
  mapview()


network_pro_pst = sa_motorway_network %>%  
  # sample_n(20000) %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%  
  st_filter(query_poly_union)

network_pro_pst %>%  
  st_filter(yolo) %>%  
  mapview(zcol = "highway")

sa_motorway_network %>%  count(highway)
sa_pst_network %>%  count(highway)








file_name = "terminal_memPort_to_internal.gpkg"

spatial_data_filtered = here("data/memphis_req/data_for_report", file_name) %>%  
  read_sf()


spatial_data_filtered %>%  
  mapview(zcol = "highway")















