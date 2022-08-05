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
library(tidyverse) #- this one has ALL functions
library(gauntlet)
# library(tigris)
library(sf)
library(mapview)
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
`%notin%` = Negate(`%in%`)

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

network = here("data/replica_20220801/gis/network.gpkg") %>% 
  read_sf() %>% 
  st_transform(4326)

mapview(network)

network_sa = network %>%
  st_filter(zip_codes_sa)

network_sa %>%  
  head()

network_sa$highway %>%  
  unique()

network_sa %>%  
  filter(highway %in% c("motorway")) %>%  
  mapview()

portland_buffer = data.frame(lon = -122.67720, lat = 45.5) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  
  quick_buffer(radius = 10000)

network_pb = network_sa %>%  
  st_filter(portland_buffer)



network_pb %>%  
  filter(highway %notin% c("footway", "residential", "tertiary", "path"
                           ,"service", "track", "pedestrian", "cycleway", "other"
                           ,"living_street", "platform", "unclassified"
                           ,"motorway_link", "primary_link", "trunk_link", "tertiary_link", "secondary_link")) %>% 
  filter(highway != "secondary") %>% 
  mapview(zcol = "highway")

network_cleaned = network %>%  
  filter(highway %notin% c("footway", "residential", "tertiary", "path"
                           ,"service", "track", "pedestrian", "cycleway", "other"
                           ,"living_street", "platform", "unclassified"
                           ,"motorway_link", "primary_link", "trunk_link", "tertiary_link", "secondary_link")) %>% 
  filter(highway != "secondary") 

# 
# network_cleaned %>%  
#   saveRDS(here("data/replica_20220801/gis/network_cleaned.RDS"))
# 
# network_cleaned %>% 
#   sf::write_sf(here("data/replica_20220801/gis/network_cleaned.gpkg"))


# sample_n(network_cleaned, here("data/replica_20220801/gis/network_sa_cleaned.RDS"))


#source data 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

network_sa_cleaned = here("data/replica_20220801/gis/network_sa_cleaned.RDS") %>% 
  readRDS() %>%  
  mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 14, "right", ellipsis = "") %>%  
           as.numeric()) 

# network_sa_cleaned = here("data/replica_20220801/gis/network_sa_cleaned.RDS") %>% 
#   readRDS() %>%  
#   mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 13, "right", ellipsis = "") %>%  
#            as.numeric()) 

network_link_volumes = read_csv_allFiles2(data_location = here("data/replica_20220801")
                   ,specifically = "network-link-volume", latest = F) %>%  
  .[[1]] %>% 
  mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 14, "right", ellipsis = "") %>%  
           as.numeric()) 

network_link_volumes %>%
  ggplot() + 
  geom_histogram(aes(volume)) +
  geom_vline(xintercept = network_link_volumes %>%
               pull(volume) %>% 
               quantile(seq(.1, .9, .1)) %>%  
               unlist())

data = data.frame(volume = rnorm(100000))

percentiles = data %>%
  pull(volume) %>% 
  quantile(seq(0, 1, .1)) %>%  
  unlist()

sd = data %>% pull(volume) %>% sd()
sds = 0 + c(1:3)*sd

yolo = c(.001, .021, .136, .341, .341, .136,.021, .001) %>%  cumsum() 

qnorm(.1,  mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
norm(sds, 0, 1)

data %>% 
  ggplot() + 
  geom_histogram(aes(volume)) +
  geom_vline(xintercept = percentiles) + 
  geom_vline(xintercept = sds, color = 'red', size = 3, alpha = .6) + 
  geom_vline(xintercept = sds, color = 'blue')



#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_link_volumes %>%  
  mutate(volume_pr = percent_rank(volume)) %>%  
  # filter(volume >= quantile(volume, .9)) %>%  
  merge(network_sa_cleaned, .
        ,by = "stableEdgeId_adj") %>%  
  mapview(zcol = "volume")


network_sa_cleaned %>%  
  filter(stableEdgeId %in% network_link_volumes$stableEdgeId)

network_sa_cleaned %>%  
  filter(startLat == 47.26234)

test_1 = network_link_volumes %>%  
  filter(startLat <= 45.63, 
         startLat >= 45.45) %>% 
  filter(str_detect(roadName, "Bridge")) %>%  
  mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 13, "right", ellipsis = "") %>%  
           as.numeric()) %>% 
  arrange(roadName)

test_2 = network_sa_cleaned %>%
  filter(startLat <= 45.63, 
         startLat >= 45.45,
         str_detect(streetName, "Bridge")) %>%  
  mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 13, "right", ellipsis = "") %>%  
           as.numeric())

test_1 %>% 
  merge(test_2, .
        ,by = "stableEdgeId_adj") %>%  
  mapview(zcol = "volume")
  



network_sa_cleaned 
  
##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































