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
counties = tigris::counties(state = c("OR", "WA")) %>% 
  st_transform(4326)

index_sa = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  pull(COUNTYFP)

counties_sa = counties %>%  
  filter(NAME %in% c("Multnomah", "Washington", "Clackamas")) %>%  
  select() 

#raw object
object_codes = c("WA", "OR") %>%  
  map(~tigris::tracts(state = .x, year = 2010) %>%  
        st_transform(crs = 4326) %>%  
        mutate(area_km2 = ALAND10/(1000^2))) %>%  
  reduce(rbind) %>%  
  rename(GEOID = GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID))

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
trips = read_csv_allFiles2("data/geo_tracts_analysis_20220810"
                           ,specifically = "trips")

trips = trips %>%  
  .[[1]]

trips_hc = trips %>%  
  .[vehicle_type == "HEAVY_COMMERCIAL"] 

trips_hc_agg = trips_hc %>%  
  .[,.(total_count_hc = .N)
    ,by = .(origin_fips = origin_us_tract
            ,destination_fips = destination_us_tract)]

###ODs==========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
replica_csvs = read_csv_allFiles2(
  data_location = path_to_study_files
  ,clean_string = ".csv", specifically = "ods_purpose")

ods_purpose_raw = replica_csvs$ods_purpose

od_trip_comb = ods_purpose_raw %>% 
  data.table() %>% 
  merge.data.table(., trips_hc_agg,
                   by = c('origin_fips', 'destination_fips'), all.x = T) %>%  
  .[,`:=`(total_count_hc = replace_na(total_count_hc, 0))]

ods_purpose_merge = od_trip_comb %>%  
  janitor::remove_constant() %>%  
  rename(origin_id = origin_fips
         ,destination_id = destination_fips) %>%  
  select(origin_id, destination_id, total_count, total_count_hc) %>%  
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
  mutate(count = 1)

###QC----
# ods_purpose_merge %>%  
#   mutate(across(everything(), is.na)) %>%  
#   group_by(destination_id, origin_id, total_count, flag_sa_origin, flag_sa_dest) %>%  
#   count()
# 
# ods_purpose_merge %>%  
#   mutate(across(!total_count, is.na), 
#          total_count = replace_na(total_count, 0)) %>%  
#   count_percent_zscore(col = total_count
#                        ,grp_c = c(destination_id, origin_id, flag_sa_origin, flag_sa_dest)
#                        ,grp_p = c())

###final od object----
ods_purpose_merge_good = ods_purpose_merge %>%  
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
  mutate(total_count = total_count_hc) %>% 
  select(starts_with("origin"), starts_with("dest"), contains("count"), everything())


#explore data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
od_mat_int_ext_count_polys = ods_purpose_merge_good %>%  
  count_percent_zscore(col = count
                       ,grp_c = c(flag_sa_origin, flag_sa_dest), grp_p = c())

od_mat_int_ext_count_trips = ods_purpose_merge_good %>%  
  count_percent_zscore(grp_c = c(flag_trip_type), grp_p = c()
                       ,col = total_count) %>%  
  group_by(external_to_external = (flag_trip_type == "ext_ext") == T) %>%  
  mutate(percent_no_ext2ext = case_when(!external_to_external~count/sum(count)
                                        ,T~NA_real_)) %>%  
  ungroup() %>%  
  group_by(int_only = str_detect(flag_trip_type, "int_") == T) %>% 
  mutate(percent_only_int = case_when(int_only~count/sum(count)
                                      ,T~NA_real_)) %>% 
  ungroup() %>%  
  select(!c(external_to_external, int_only))

ods_agg_by_emit_rec = ods_purpose_merge_good %>%  
  filter(flag_same_od != "same_od") %>%  
  filter(flag_trip_type != "ext_ext") %>% 
  group_by(tract_id = origin_id, tract_area = origin_area) %>%  
  summarise(total_count_emit = sum(total_count), .groups = "drop") %>%   
  merge(., ods_purpose_merge_good %>%  
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
#   # "total_count_emit_adj"
#   ,"total_count_rec_adj"
#   # ,"total_count_diff_adj"
#   # ,"total_count_diff_per"
# ) %>%  
#   map(~mapview(ods_agg_by_emit_rec_sf
#                ,zcol = .x
#                ,layer.name = .x)) %>%  
#   reduce(`+`)


 
od_basic_agg_byOrigin = ods_purpose_merge_good %>%  
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

# od_basic_agg_byOrigin_sf %>%
#   mapview(zcol = "count_adj_area_rnk_binc", burst = T, layer.name = "basic_map")


##create base objects===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#keeping only internal-internal trips
#--->external-external are the most, but removing them i-i trips are 80 in the data
ods_filtered = ods_purpose_merge_good %>% 
  filter(flag_sa_origin == "internal" 
         # | flag_sa_dest == "internal"
         )

#breakdown of same OD travel vs non-same OD travel 
ods_filtered_breakdown = ods_filtered %>%  
  mutate(flag_same_od = case_when(origin_id == destination_id~"Same ODs"
                                  ,T~"Different ODs"))  %>%  
  count_percent_zscore(grp_c = c(origin_id, flag_same_od, origin_area )
                       ,grp_p = c(origin_id, origin_area )
                       ,col = total_count, rnd = 3) %>%  
  mutate(count_adj_area = count/origin_area)

##create neighbor flags=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ods_filtered_neighbor = ods_filtered %>%  
  mutate(flag_same_od = case_when(origin_id == destination_id~T, T~F)) %>%
  merge(object_neighbors 
        ,by.x = c("origin_id", "destination_id")
        ,by.y = c("zip", "zip_n"), all.x = T) %>%  
  mutate(flag_neighbor = replace_na(flag_neighbor, F)) %>% 
  mutate(flag_trip_prox = case_when(flag_same_od == T~"Same OD Trip"
                                    ,flag_neighbor == T~"Neighbor Trip"
                                    ,T & flag_sa_dest == "external"~"Other Trip (ext)"
                                    ,T & flag_sa_dest != "external"~"Other Trip (int)")) %>%  
  group_by(flag_trip_prox) %>%  
  mutate(rank = percent_rank(total_count)
         ,rank_fd = floor_divide(rank, .1)) %>%  
  ungroup() %>%  
  arrange(desc(rank))

od_data_origins = create_neighbor_df(data = ods_filtered_neighbor)

###descriptive plot=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

od_data_origins %>%  
  ggplot() + 
  geom_histogram(aes(count)) + 
  facet_grid(rows = vars(flag_trip_prox), scales = "free") + 
  labs(title = "Histogram of total trips eminating from orignin zone"
       ,subtitle = "veritcal lines are 70th, 80th, and 90th percentils"
       ,x = "Total trips from a origin"
       ,y = "count")

od_data_origins %>%  
  ggplot() + 
  geom_histogram(aes(count_adj_area)) + 
  facet_grid(rows = vars(flag_trip_prox ), scales = "free") + 
  labs(title = "Histogram of total trips eminating from orignin zone"
       ,subtitle = "veritcal lines are 70th, 80th, and 90th percentils"
       ,x = "Total trips from a origin"
       ,y = "count")

#no correlation between % of same OD trips vs diff OD trips w.r.t total trips
# od_data_origins %>%   
#   group_by(origin_id) %>%  
#   mutate(count_tot = sum(count)) %>%  
#   ungroup() %>%  
#   ggplot() + 
#   geom_point(aes(percent, count_tot)) + 
#   facet_grid(rows = vars(flag_trip_prox), scales = "free") 

#zones have and average of ~25% of their trips ending in same zone
od_data_origins %>% 
  # filter(flag_trip_prox == "Same OD Trip") %>% 
  ggplot() + 
  geom_histogram(aes(percent)) +
  facet_grid(rows = vars(flag_trip_prox ), scales = "free") + 
  
  labs(title = "Histogram of percent of dif OD trips eminating from orignin zone"
       ,x = "Percent of dif OD trips"
       ,y = "Count")




ods_filtered_neighbor %>%  
  filter(flag_sa_origin == "internal" & flag_sa_dest == "internal") %>% 
         # ,destination_id != origin_id) %>% 
  arrange(origin_id) %>%  
  ggplot() + 
  geom_point(aes(as.factor(origin_id)
                 ,as.factor(destination_id)
                 ,size = rescale_to(total_count/origin_area, 100)
                 ,color = flag_trip_prox), alpha = .1) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


###mapping======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

map = showme_od_trips(data_n = od_data_origins)

od_basic_agg_byOrigin %>%
  merge(object_codes_sa 
        # %>%  
          # filter(flag_sod_basic_agg_byOrigina == "internal")
        , .,
        by.x = "GEOID", by.y = "origin_id") %>%  
  mapview(zcol = "count_adj_area_rnk_binc", burst = T, layer.name = "basic_map")


basic_map_specific_ods = ods_filtered_neighbor %>%  
  filter(origin_id == 41051010303) %>%  
  mutate(total_count_rnk = percent_rank(total_count),
         total_count_rnk_binc = total_count_rnk %>%
           cut(c(0,.7, .8, .9, .95, .98, 1))) %>% 
  merge(object_codes_sa %>%  
          filter(flag_sa == "internal"), .,
        by.x = "GEOID", by.y = "destination_id") %>%  
  mapview(zcol = "total_count_rnk_binc", burst = T, layer.name = "basic_map")


##get study polys===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section should be in its own script since its is task specific
cut_off = .9
od_basic_agg_byOrigin_cut = od_basic_agg_byOrigin %>%  
  filter(count_adj_area_rnk_bin >= cut_off) 

od_basic_agg_byOrigin_sf_cut = od_basic_agg_byOrigin_sf %>% 
  filter(count_adj_area_rnk_bin  >= cut_off) 
  
index_top_emit_tracts =od_basic_agg_byOrigin_cut %>%  
  pull(origin_id)

dest_agg_by_top_org = ods_purpose_merge_good %>%  
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

# dest_agg_by_top_org_sf %>%  
#   mapview(zcol = "count_adj_area_rnk_binc", burst = T, layer.name = "basic_map")



mapview(od_basic_agg_byOrigin_sf_cut
        ,zcol = "count_adj_area_rnk_binc"#, burst = T
        ,layer.name = "origin_polys", homebutton = F) +
mapview(dest_agg_by_top_org_sf_cut
        ,zcol = "count_adj_area_rnk_binc"#, burst = T
        ,layer.name = "destination_polys", homebuttoxn = F)


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








































