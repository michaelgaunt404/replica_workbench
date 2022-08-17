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
library(here)
library(gauntlet)
library(tigris)
library(sf)
library(mapview)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

# path_to_study_files = 'data/geo_tracts_analysis_20220810'
path_to_study_files = 'data/geo_manual_tracts_20220811'
path_to_study_files = 'data/geo_manual_tracts_20220811/internal_trips'

path_to_study_files_emits = 'data/geo_tracts_analysis_20220810/network_vols_emits'

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
`%notin%` = Negate(`%in%`)

process_replica_network = function(raw_data_list){
  #processes raw data lists 
  #perfroms very surface level cleaning
  list(raw_data_list, names(raw_data_list)) %>%  
    pmap(~.x %>%  
           mutate(source = .y
                  ,stableEdgeId_adj = str_trunc(stableEdgeId, 14, "right", ellipsis = "") %>%  
                    as.numeric()
                  ,volume_rnk = percent_rank(volume)
                  ,volume_rnk_binc = volume_rnk %>%
                    cut(c(0,.7, .8, .9, .95, .98, 1), include.lowest = T)
           )
    ) %>%  
    reduce(bind_rows)
}
 
#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##spatial network================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

# network_sa_cleaned = here("data/replica_20220801/gis/network_sa_cleaned.RDS") %>% 
#   readRDS() %>%  
#   mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 14, "right", ellipsis = "") %>%  
#            as.numeric()) 

network = here("data/gis/network_reduced_link_types_spatial_portland.rds") %>% 
  readRDS() %>%  
  mutate(stableEdgeId_adj = str_trunc(stableEdgeId, 14, "right", ellipsis = "") %>%  
           as.numeric()) 

##tabular network vols===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_link_volumes_raw = read_csv_allFiles2(data_location = path_to_study_files
                   ,specifically = "network_vols", latest = F, clean_string  = ".csv") 

network_link_volumes_pro_int = process_replica_network(network_link_volumes_raw)

#TODO - convert to VMT?
network_link_volumes_pro_int_sf = network_link_volumes_pro_int %>% 
  select(stableEdgeId_adj, direction, starts_with("vol"), source) %>% 
  merge(network, .
        ,by = "stableEdgeId_adj")

#exploration====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_link_volumes_pro_int_sf %>%  
  filter(volume_rnk >= .7) %>% 
    mapview(zcol = "volume_rnk_binc", burst = T, homebutton = F)


##network emits only============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# network_link_volumes_emits_raw = read_csv_allFiles2(data_location = path_to_study_files_emits
#                                               , latest = F, clean_string  = ".csv") 
# 
# network_link_volumes_emits_pro = process_replica_network(network_link_volumes_emits_raw)
# 
# network_link_volumes_emits_pro_sf = network_link_volumes_emits_pro %>% 
#   merge(network_sa_cleaned, .
#         ,by = "stableEdgeId_adj")
# 
# network_link_volumes_emits_pro_sf %>%  
#   filter(volume_rnk >= .7) %>% 
#   mapview(zcol = "volume_rnk_binc", burst = T)
#   


# network_link_volumes %>%
#   ggplot() + 
#   geom_histogram(aes(volume)) +
#   geom_vline(xintercept = network_link_volumes %>%
#                pull(volume) %>% 
#                quantile(seq(.1, .9, .1)) %>%  
#                unlist())
# 
# data = data.frame(volume = rnorm(100000))
# 
# percentiles = data %>%
#   pull(volume) %>% 
#   quantile(seq(0, 1, .1)) %>%  
#   unlist()
# 
# sd = data %>% pull(volume) %>% sd()
# sds = 0 + c(1:3)*sd
# 
# yolo = c(.001, .021, .136, .341, .341, .136,.021, .001) %>%  cumsum() 
# 
# qnorm(.1,  mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# norm(sds, 0, 1)
# 
# data %>% 
#   ggplot() + 
#   geom_histogram(aes(volume)) +
#   geom_vline(xintercept = percentiles) + 
#   geom_vline(xintercept = sds, color = 'red', size = 3, alpha = .6) + 
#   geom_vline(xintercept = sds, color = 'blue')



#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



network_link_volumes %>%  
  count_percent_zscore(
    col = volume
    ,grp_c = c(stableEdgeId, roadName, direction, source)
    ,grp_p = c(stableEdgeId, roadName, direction)
  ) %>%  
  group_by(stableEdgeId, roadName,direction) %>%  
  filter(n() > 1)

network_plot = network_link_volumes %>%  
  group_by(source) %>% 
  mutate(volume_rnk = percent_rank(volume) %>%  
           as.factor()) %>%  
  # filter(volume >= quantile(volume, .9)) %>%  
  merge(network_sa_cleaned, .
        ,by = "stableEdgeId_adj") %>%  
  mapview(zcol = "volume", lwd = 5, layer.name = "Troutdale-Port Network Vols.")

basic_map_specific_ods + network_plot


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










































