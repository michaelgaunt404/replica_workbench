











network_lnk_old = tar_read('mem_network_lnk') 




mean(network_lnk$stableEdgeId_trunc %in% processed_data_ie$network_link_ids_trunc)


network_lnk$stableEdgeId_trunc %>%  sort() %>%  head()

processed_data_ie$network_link_ids_trunc %>%  sort() %>%  head()


network_lnk %>%  
  filter(str_detect(stableEdgeId, "^100005"))





list(network_lnk
     ,network_lnk_old) %>%  
  map(~.x %>%  
        filter(streetName == "Memphis-Arkansas Memorial Bridge") %>%  
        select(!c(stableEdgeId, flags )) %>%  
        st_drop_geometry() %>%  
        pull(osmid))




#section: checking old network==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_old = here("data/memphis_req/data_queried"
     ,"network_memphis_pro_20221101.gpkg") %>%  
  read_sf()

network_old %>%  
  st_drop_geometry() %>% 
  count(highway)

#VERDICT: missing fucking trunk links

#section: checking old network==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_raw = here("data/replica_networks/replica-bulk-network-south-central"
                   ,"network.gpkg") %>%  
  read_sf()

network_old %>%  
  st_drop_geometry() %>% 
  count(highway)

#VERDICT: too large to load


#section: check different newtworks=============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network_raw = here("data/replica_networks/replica-bulk-network-south-central")

table = "replica-customer.south_central.south_central_2019_Q4_network_segments"
table = "replica-customer.south_central.south_central_2021_Q2_network_segments"
table = "replica-customer.south_central.south_central_2021_Q4_network_segments"


#make query
bounding_box = bounding_box_sa
query_string = str_glue(
"SELECT *
from `{table}`
where 
highway = 'motorway'
AND
(startLat > {bounding_box[[2]]} AND startLat < {bounding_box[[4]]}) AND
(endLat > {bounding_box[[2]]} AND endLat < {bounding_box[[4]]}) AND
(startLon > {bounding_box[[1]]} AND startLon < {bounding_box[[3]]}) AND
(endLon > {bounding_box[[1]]} AND endLon < {bounding_box[[3]]})
--limit 100")


network = dbGetQuery(con, query_string)
# network_19q4 = network
# network_21q2 = network
# network_21q4 = network

list(
  network_19q4
  ,network_21q2
  ,network_21q4
     ) %>%  
  map(~{
    temp = .x %>%  
      mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) 
      
    mean(temp$stableEdgeId_trunc %in% processed_data_ie$network_link_ids_trunc)
  })



mean(network_lnk$stableEdgeId_trunc %in% processed_data_ie$network_link_ids_trunc)






