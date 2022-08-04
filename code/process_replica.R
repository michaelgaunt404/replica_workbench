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
library(gauntlet)
library(tigris)
library(sf)
library(mapview)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
yolo = function(data, od_col = origin_name, byy = "origin_name", remove_self = F){
  data %>%  
    {if (remove_self) (.) %>% filter(origin_name != destination_name) else .} %>% 
    group_by({{od_col}}) %>%  
    summarise(total_count = sum(total_count)) %>% 
    arrange(desc(total_count)) %>% 
    merge(zip_codes, ., by.x = "ZCTA5CE10", by.y = byy, all.x = T) 
}


#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##replica data----
replica_csvs = read_csv_allFiles2(data_location = 'data/replica_20220801'
                                  ,clean_string = ".csv")
ods_purpose = replica_csvs$ods_purpose %>%  
  janitor::remove_constant() %>%  
  select(!commercial_count)

##zip codes----
zip_codes = c("WA", "OR") %>%  
  map(~tigris::zctas(state = .x, year = 2010) %>%  
  st_transform(crs = 4326)) %>%  
  reduce(rbind)

#study area zip codes 
zip_codes_sa = zip_codes %>% 
  st_filter(counties %>%  
              filter(COUNTYFP %in% index_remove) %>%  
              st_transform(crs = 4326))

zip_codes_sa_index = zip_codes_sa %>%  
  pull(ZCTA5CE10)

zone_neighbors = zip_codes_sa %>%  
  select(ZCTA5CE10 ) %>%  
  st_join(zip_codes_sa %>%  
            select(ZCTA5CE10), st_touches) %>%  
  st_drop_geometry() %>%  
  rownames_to_column() %>%  
  set_names(c('index_nmerge', 'zip', 'zip_n')) %>%  
  data.frame() %>%  
  mutate(flag_neighbor = T)

##cleaning OD data----

#explore
ods_purpose %>% 
  mutate(flag_sa_origin = case_when(origin_name %in% zip_codes_sa_index~T, T~F)
         ,flag_sa_dest = case_when(destination_name %in% zip_codes_sa_index~T, T~F)) %>%  
  mutate(count = 1) %>%  
  count_percent_zscore(grp_c = c(flag_sa_origin, flag_sa_dest), grp_p = c())

ods_purpose %>%  
  mutate(count = total_count) %>%  
  mutate(flag_sa_origin = case_when(origin_name %in% zip_codes_sa_index~T, T~F)
         ,flag_sa_dest = case_when(destination_name %in% zip_codes_sa_index~T, T~F)) %>% 
  filter(!(flag_sa_origin == F & flag_sa_dest == F)) %>%
  count_percent_zscore(grp_c = c(flag_sa_origin, flag_sa_dest), grp_p = c())

#keeping only internal-internal trips
#--->external-external are the most, but removing them i-i trips are 80 in the data
temp_ods_purpose = ods_purpose %>% 
  mutate(flag_sa_origin = case_when(origin_name %in% zip_codes_sa_index~T, T~F)
         ,flag_sa_dest = case_when(destination_name %in% zip_codes_sa_index~T, T~F)) %>%  
  filter((flag_sa_origin == T & flag_sa_dest == T))

#breakdown of same OD travel vs non-same OD travel 
temp_data_plot = temp_ods_purpose %>%  
  mutate(count = total_count
         ,flag_same_od = case_when(origin_name == destination_name~T, T~F))  %>%  
  count_percent_zscore(grp_c = c(origin_name, flag_same_od)
                       ,grp_p = c(origin_name)) %>%  
  arrange(desc(origin_name)) 

temp_data_plot %>%  
  ggplot() + 
  geom_histogram(aes(count)) + 
  facet_grid(rows = vars(flag_same_od), scales = "free") + 
  labs(title = "Histogram of total trips eminating from orignin zone"
       ,subtitle = "veritcal lines are 70th, 80th, and 90th percentils"
       ,x = "Total trips from a origin")

#no correlation between % of same OD trips vs diff OD trips w.r.t total trips
temp_data_plot %>%   
  group_by(origin_name) %>%  
  mutate(count_tot = sum(count)) %>%  
  ungroup() %>%  
  ggplot() + 
  geom_point(aes(percent, count_tot)) + 
  facet_grid(rows = vars(flag_same_od), scales = "free") 

#zones have and average of ~25% of their trips ending in same zone
temp_data_plot %>%  
  filter(flag_same_od == T) %>% 
  ggplot() + 
  geom_histogram(aes(percent)) +
  labs(title = "Histogram of percent of dif OD trips eminating from orignin zone"
       ,x = "Percent of dif OD trips")

#exploring neighbor trips
temp_ods_purpose %>%  
  mutate(count = total_count
         ,flag_same_od = case_when(origin_name == destination_name~T, T~F)) %>% 
  merge(zone_neighbors 
      ,by.x = c("origin_name", "destination_name")
        ,by.y = c("zip", "zip_n"), all.x = T) %>%  
  mutate(flag_neighbor = replace_na(flag_neighbor, F)) %>% 
  mutate(flag_trip_prox = case_when(flag_same_od == T~"Same OD Trip"
                                    ,flag_neighbor == T~"Neighbor Trip"
                                    ,T~"Other Trip")) %>% 
  filter(flag_same_od == F ) %>% 
  count_percent_zscore(grp_c = c(flag_trip_prox)
                       ,grp_p = c())

temp_data = temp_ods_purpose %>%  
  mutate(count = total_count
         ,flag_same_od = case_when(origin_name == destination_name~T, T~F)) %>% 
  merge(zone_neighbors 
        ,by.x = c("origin_name", "destination_name")
        ,by.y = c("zip", "zip_n"), all.x = T) %>%  
  mutate(flag_neighbor = replace_na(flag_neighbor, F)) %>% 
  mutate(flag_trip_prox = case_when(flag_same_od == T~"Same OD Trip"
                                    ,flag_neighbor == T~"Neighbor Trip"
                                    ,T~"Other Trip")) %>%  
  count_percent_zscore(grp_c = c(flag_trip_prox, origin_name)
                       ,grp_p = c(flag_trip_prox))

temp_data %>%  
  filter(flag_trip_prox == "Other Trip") %>%  
  arrange(desc(percent))

c("Same OD Trip", "Neighbor Trip", "Other Trip") %>%  
  map(~temp_data %>%  
        filter(flag_trip_prox == .x) %>% 
        mutate(flag_percentile_limit = case_when(count >= quantile(count, .9)~T, T~F)) %>% 
        merge(zip_codes_sa, ., 
              by.x = "ZCTA5CE10", by.y = "origin_name") %>%  
        mapview(zcol = "flag_percentile_limit"
                , layer.name = .x, alpha.regions = 0.4)) %>%  
  reduce(`+`)





#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zip_codes %>%  
  st_drop_geometry() %>%  
  count(STATEFP10)
zip_codes %>% 
  mutate(flag_color = case_when(ZCTA5CE10  == 83330 |
                                  ZCTA5CE10  == 97801~"color", T~"no color")) %>%  
  mapview(zcol = "flag_color")
  

zip_codes %>% 
  # mutate(flag_color = case_when(ZCTA5CE10  == 83330 |
  #                                 ZCTA5CE10  == 97801~"color", T~"no color")) %>%  
  mapview(zcol = "ZCTA5CE10")

ods_zip_merge_good %>%  
  filter(origin_name == 97217, 
         ZCTA5CE10 != 97217) %>%  
  mutate(total_count = log10(total_count)) %>%
  arrange(desc(total_count)) %>%  
  mapview(zcol = "total_count")

ods_zip_merge_good %>% 
st_filter(counties_reduced)
  



##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================


#first get who emits the most number of trucks 
#---> some percetnile of those?
#order these ODs, get some percentage of those 








































