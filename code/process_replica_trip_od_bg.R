#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script prepares data for OD selection for route identification
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: this script imports trip data from replica 
#-------- sources polys from tigris 
#-------- --need these for study area identification
#-------- --need these to merge data with 
#-------- should not be used for EDA 
#-------- ideally this script should be sourced from RMD
#-------- --RMD could provide folder locations and other inputs, etc
#-------- --currently experiencing bug
#-------- like to turn this into a canned report at somepoint
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

#feeds into read_csv_allFiles2() below
path_to_study_files = 'data/geo_tracts_analysis_20220810'


##folder dictionary----
# 'data/geo_tracts_analysis_20220810' 
#---->first analysis - uses census tracts by replica

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

output_suffix = "mg_20220819"

#cut off threshold for poly aggregation
cut_off = .9

options(tigris_use_cache = TRUE)

##functions----

create_neighbor_df = function(data = ods_filtered_neighbor
                              ,origin = T){
  if (!origin){
    data %>%  
      count_percent_zscore(grp_c = c(destination_id, flag_trip_prox, destination_area)
                           ,grp_p = c(destination_id, destination_area)
                           ,col = total_count) %>%  
      mutate(count_adj_area = count/destination_area)
  } else {
    data %>%  
      count_percent_zscore(grp_c = c(origin_id, flag_trip_prox, origin_area)
                           ,grp_p = c(origin_id, origin_area)
                           ,col = total_count) %>%  
      mutate(count_adj_area = count/origin_area)
  }
}

showme_od_trips =  function(data_n, origin = T, thresh = .9, alpha = .4
                            ,map_color = "flag_percentile_limit_adj"){
  data = data_n
  
  if (origin) {
    byy = "origin_id"
    flag = "internal"
  } else {
    byy = "destination_id"
    flag = "internal"
  }
  
 
  c("Same OD Trip", "Neighbor Trip", "Other Trip"
  ) %>%  
    map(~data %>%  
          filter(flag_trip_prox == .x) %>%
          merge(object_codes_sa %>%  
                  filter(flag_sa == "internal"), .,
                by.x = "GEOID", by.y = byy) %>%  
          mutate(count_adj_area = count/area_km2
                 ,flag_percentile_limit_adj = case_when(count_adj_area >= quantile(count_adj_area, thresh)~T, T~F)
                 ,flag_percentile_limit = case_when(count >= quantile(count, thresh)~T, T~F)) %>% 
          mapview(zcol = map_color
                  ,layer.name = .x, alpha.regions = alpha)) %>%  
    reduce(`+`)
}

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##object codes==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#section is used to prep the underlying spatial polygons for the  replica data 
#if you need to define a study area do it here

###county=======================================================================
#I used this section to define my study/focus area
counties = tigris::counties(state = c("OR", "WA")) %>% 
  st_transform(4326)

print("test")

index_sa = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  pull(COUNTYFP)

counties_sa = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  select() 

###block groups=================================================================
block_groups_raw = c("WA","OR") %>%
  map(~tigris::block_groups(state = .x, year = 2010) %>%
        st_transform(crs = 4326) %>%
        mutate(area_km2 = ALAND10/(1000^2))) %>%
  reduce(rbind) %>% 
  rename(GEOID = GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID))

###tracts=======================================================================
# tracts_raw = c("WA", "OR") %>%  
#   map(~tigris::tracts(state = .x, year = 2010) %>%  
#         st_transform(crs = 4326) %>%  
#         mutate(area_km2 = ALAND10/(1000^2))) %>%  
#   reduce(rbind) %>%  
#   rename(GEOID = GEOID10) %>%
#   mutate(GEOID = as.numeric(GEOID))

###processing===================================================================
#choose which poly you want ot use 
object_codes = block_groups_raw

object_codes_df = object_codes %>%  
  st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1]
         ,lat = st_coordinates(geometry)[,2]) %>%  
  st_drop_geometry()

#study area zip codes 
#uses index from process_geo_for_replica.r script
object_codes_sa = object_codes %>%
  mutate(flag_sa = case_when(COUNTYFP %in% index_sa & 
                               STATEFP == 41~"internal", T~"external"))

#use geoids from now on --> change to geoid if they dont have one yet
object_codes_sa_index = object_codes_sa %>%
  filter(flag_sa == "internal") %>%
  pull(GEOID)

object_codes_sa_df = object_codes_sa %>%  
  select(GEOID, flag_sa, area_km2) %>%  
  st_drop_geometry()

#creates list of neigboring shapefiles for all shapefiles 
#may not need this but was curious 
object_neighbors = object_codes_sa %>%  
  select(GEOID) %>%  
  st_join(object_codes_sa %>%  
            select(GEOID), st_touches) %>%  
  st_drop_geometry() %>%  
  rownames_to_column() %>%  
  set_names(c('index_nmerge', 'zip', 'zip_n')) %>%  
  data.frame() %>%  
  mutate(flag_neighbor = T)

##replica data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###trips==========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trips = read_csv_allFiles2(path_to_study_files
                           ,specifically = "trips_thursday_sep2019")

trips = trips %>%  
  .[[1]]

trips_hc = trips %>%  
  .[vehicle_type == "HEAVY_COMMERCIAL"] 

#choose which level to agg by 
trips_hc_agg_bg = trips_hc %>%  
  .[,.(total_count_hc = .N)
    ,by = .(origin_id = origin_bgrp
            ,destination_id = destination_bgrp)
    ]

###ODs==========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# replica_csvs = read_csv_allFiles2(
#   data_location = path_to_study_files
#   ,clean_string = ".csv", specifically = "ods_purpose")

###processed data===============================================================
od_data = trips_hc_agg_bg %>% 
  filter(!(origin_id == "out_of_region" | 
             destination_id == "out_of_region")) %>% 
  mutate(across(everything(), as.numeric)) %>%  
  merge(object_codes_sa_df, 
        by.x =  "origin_id", by.y = "GEOID", all = T) %>%  
  rename(flag_sa_origin = flag_sa
         ,origin_area = area_km2) %>%  
  merge(object_codes_sa_df, 
        by.x =  "destination_id", by.y = "GEOID", all = T) %>%  
  rename(flag_sa_dest = flag_sa
         ,destination_area = area_km2) %>%  
  mutate(count = 1) %>% 
  mutate(total_count = total_count_hc) 

####QC----
# od_data %>%
#   mutate(across(everything(), is.na)) %>%
#   group_by(destination_id, origin_id, total_count, flag_sa_origin, flag_sa_dest) %>%
#   count()
# 
# od_data %>%  
#   mutate(across(!total_count, is.na), 
#          total_count = replace_na(total_count, 0)) %>%  
#   count_percent_zscore(col = total_count
#                        ,grp_c = c(destination_id, origin_id, flag_sa_origin, flag_sa_dest)
#                        ,grp_p = c())

####final od object----
od_data_pro = od_data %>%  
  filter(!is.na(destination_id) &
           !is.na(origin_id) &
           !is.na(flag_sa_origin)  &
           !is.na(flag_sa_dest)) %>%  
  mutate(flag_same_od = case_when(origin_id == destination_id~"same_od"
                                  ,T~"diff_od")
         ,flag_trip_type = paste0(
           str_trunc(flag_sa_origin, 3, ellipsis = "")
           ,"_"
           ,str_trunc(flag_sa_dest, 3, ellipsis = ""))) %>%
  select(starts_with("origin"), starts_with("dest"), contains("count"), everything())


#explore data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##highlevel summary=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
od_mat_int_ext_count_polys = od_data_pro %>%  
  count_percent_zscore(col = count
                       ,grp_c = c(flag_sa_origin, flag_sa_dest), grp_p = c())

od_mat_int_ext_count_trips = od_data_pro %>%  
  count_percent_zscore(grp_c = c(flag_trip_type), grp_p = c()
                       ,col = total_count) %>%  
  group_by(external_to_external = (flag_trip_type == "ext_ext") == T) %>%  
  mutate(percent_no_ext2ext = case_when(!external_to_external~count/sum(count)
                                        ,T~NA_real_)) %>%  
  ungroup() %>%  
  group_by(int_only = str_detect(flag_trip_type, "int_") == T) %>% 
  mutate(percent_int_origins = case_when(int_only~count/sum(count)
                                      ,T~NA_real_)) %>% 
  ungroup() %>%  
  select(!c(external_to_external, int_only))

##emits and recs================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ods_agg_by_emit_rec = od_data_pro %>%  
  filter(flag_same_od != "same_od") %>%  
  filter(flag_trip_type != "ext_ext") %>% 
  group_by(tract_id = origin_id, tract_area = origin_area) %>%  
  summarise(total_count_emit = sum(total_count), .groups = "drop") %>%   
  merge(., od_data_pro %>%  
          filter(flag_same_od != "same_od") %>%  
          group_by(destination_id) %>%  
          summarise(total_count_rec = sum(total_count), .groups = "drop")
        ,by.x = 'tract_id', by.y = 'destination_id') %>%  
  mutate(total_count_diff = total_count_emit - total_count_rec
         ,total_count_diff_per = ((total_count_emit - total_count_rec)/total_count_rec)
         ,across(starts_with("total_count"), ~.x/tract_area, .names = "{.col}_adj"))

ods_agg_by_emit_rec_sf = ods_agg_by_emit_rec %>% 
  merge(object_codes_sa, ., by.x = "GEOID", by.y = "tract_id")

# c(
#   "total_count_emit_adj"
#   # ,"total_count_rec_adj"
#   # ,"total_count_diff_adj"
#   # ,"total_count_diff_per"
# ) %>%
#   map(~mapview(ods_agg_by_emit_rec_sf
#                ,zcol = .x
#                ,layer.name = .x)) %>%
#   reduce(`+`)

###summary plots================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_emit)))
# 
# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_rec)))
# 
# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_diff)))
# 
# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_emit_adj)))
# 
# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_rec)))
# 
# ods_agg_by_emit_rec %>%  
#   ggplot() + 
#   geom_histogram(aes(log10(total_count_diff_adj)))
# 
# 
# ods_agg_by_emit_rec %>%  
#   filter(total_count_emit >= 50) %>%
#   ggplot() + 
#   geom_histogram(aes(total_count_diff_per ))




##counts by origin==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
od_basic_agg_byOrigin = od_data_pro %>%  
  filter(str_detect(flag_trip_type, "int_")) %>% 
  # filter(flag_same_od != "same_od") %>% 
  group_by(origin_id, origin_area) %>%  
  summarise(total_count = sum(total_count), .groups = "drop") %>%  
  mutate(count_adj_area = total_count/origin_area, 
         count_adj_area_rnk = percent_rank(count_adj_area), 
         count_adj_area_rnk_bin = floor_divide(count_adj_area_rnk, .1),
         count_adj_area_rnk_binc = cut(count_adj_area_rnk, c(0,.7, .8, .9, .95, .98, 1)))

od_basic_agg_byOrigin_sf = od_basic_agg_byOrigin %>%
  merge(object_codes_sa, ., by.x = "GEOID", by.y = "origin_id") 


od_basic_agg_byOrigin_sf %>%  
  saveRDS(here(str_glue("data/geo_tracts_analysis_20220810/org_agg_byOrigin_sf_{output_suffix}.rds")))

##get study polys===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section should be in its own script since its is task specific
od_basic_agg_byOrigin_cut = od_basic_agg_byOrigin %>%  
  filter(count_adj_area_rnk_bin >= cut_off) 

od_basic_agg_byOrigin_sf_cut = od_basic_agg_byOrigin_sf %>% 
  filter(count_adj_area_rnk_bin  >= cut_off) 

index_top_emit_tracts =od_basic_agg_byOrigin_cut %>%  
  pull(origin_id)

dest_agg_by_top_org = od_data_pro %>%  
  filter(flag_trip_type != "ext_ext") %>%  
  filter(origin_id %in% index_top_emit_tracts) %>% 
  group_by(destination_id, destination_area) %>%  
  summarise(total_count = sum(total_count), .groups = "drop") %>%  
  mutate(count_adj_area = total_count/destination_area, 
         count_adj_area_rnk = percent_rank(count_adj_area), 
         count_adj_area_rnk_bin = floor_divide(count_adj_area_rnk, .1),
         count_adj_area_rnk_binc = cut(count_adj_area_rnk, c(0,.7, .8, .9, .95, .98, 1)))

dest_agg_by_top_org_sf = dest_agg_by_top_org %>%
  merge(object_codes_sa, ., by.x = "GEOID", by.y = "destination_id") 

dest_agg_by_top_org_sf_cut = dest_agg_by_top_org_sf %>% 
  filter(count_adj_area_rnk_bin >= cut_off)

dest_agg_by_top_org_sf %>%  
  saveRDS(here("data/geo_tracts_analysis_20220810/dest_agg_by_top_org_sf.rds"))






##counts by destination==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
od_basic_agg_byDest = od_data_pro %>%  
  filter(str_detect(flag_trip_type, "_int")) %>% 
  # filter(flag_same_od != "same_od") %>% 
  group_by(destination_id, destination_area) %>%  
  summarise(total_count = sum(total_count), .groups = "drop") %>%  
  mutate(count_adj_area = total_count/destination_area, 
         count_adj_area_rnk = percent_rank(count_adj_area), 
         count_adj_area_rnk_bin = floor_divide(count_adj_area_rnk, .1),
         count_adj_area_rnk_binc = cut(count_adj_area_rnk, c(0,.7, .8, .9, .95, .98, 1)))

od_basic_agg_byDest_sf = od_basic_agg_byDest %>%
  merge(object_codes_sa, ., by.x = "GEOID", by.y = "destination_id") 

od_basic_agg_byDest_sf %>%  
  saveRDS(here("data/geo_tracts_analysis_20220810/od_basic_agg_byDest_sf.rds"))

##get study polys===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section should be in its own script since its is task specific
cut_off = .9
od_basic_agg_byDest_cut = od_basic_agg_byDest %>%  
  filter(count_adj_area_rnk_bin >= cut_off) 

od_basic_agg_byDest_sf_cut = od_basic_agg_byDest_sf %>% 
  filter(count_adj_area_rnk_bin  >= cut_off) 

index_top_rec_tracts = od_basic_agg_byDest_cut %>%  
  pull(destination_id)

org_agg_by_top_dest = od_data_pro %>%  
  filter(flag_trip_type != "ext_ext") %>%  
  filter(destination_id %in% index_top_rec_tracts) %>% 
  group_by(origin_id, origin_area) %>%  
  summarise(total_count = sum(total_count), .groups = "drop") %>%  
  mutate(count_adj_area = total_count/origin_area, 
         count_adj_area_rnk = percent_rank(count_adj_area), 
         count_adj_area_rnk_bin = floor_divide(count_adj_area_rnk, .1),
         count_adj_area_rnk_binc = cut(count_adj_area_rnk, c(0,.7, .8, .9, .95, .98, 1)))

org_agg_by_top_dest_sf = org_agg_by_top_dest %>%
  merge(object_codes_sa, ., by.x = "GEOID", by.y = "origin_id") 

org_agg_by_top_dest_sf_cut = org_agg_by_top_dest_sf %>% 
  filter(count_adj_area_rnk_bin >= cut_off)

org_agg_by_top_dest_sf %>%  
  saveRDS(here("data/geo_tracts_analysis_20220810/org_agg_by_top_dest_sf.rds"))











# mapview(od_basic_agg_byOrigin_sf_cut
#         ,zcol = "count_adj_area_rnk_binc"#, burst = T
#         ,layer.name = "origin_polys", homebutton = F) +
# mapview(dest_agg_by_top_org_sf_cut
#         ,zcol = "count_adj_area_rnk_binc"#, burst = T
#         ,layer.name = "destination_polys", homebuttoxn = F)


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================


#first get who emits the most number of trucks 
#---> some percetnile of those?
#order these ODs, get some percentage of those 







# 
# 
# od_basic_agg_byOrigin %>%  
#   arrange(count_adj_area ) %>%  
#   mutate(index = row_number()) %>% 
#   ggplot() + 
#   geom_point(aes(index, count_adj_area ))
# 
# od_data_pro %>%  
#   filter(str_detect(flag_trip_type, "int_")) %>% 
#   arrange(origin_id, destination_id) %>% 
#   ggplot() + 
#   geom_point(aes(as.factor(origin_id)
#                  ,as.factor(destination_id)
#                  ,size = total_count_hc))




























