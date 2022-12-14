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
library(here)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

mem_bg = read_sf(here("data", "memphis_req/bgrp_area", "bgrp_area.shp")) %>%  
  st_transform(crs = 4326)

mem_taz = read_sf(here("data", "memphis_req/taz_area", "taz_area.shp")) %>%  
  st_transform(crs = 4326)

#for Memphis using selected county geometry
study_area = sel_counties %>%  
  st_union() 

sa_polys = read_rds(here('data/memphis_req/data_for_query', "study_area_polys.RDS"))

study_area = sa_polys$sel_counties %>%  st_union()

# fclust_v1 = here("data", "memphis_req/FreightClusters_v1/FreightClusters_v1.shp") %>% 
#           read_sf() %>%  
#   mutate(name = N
#          ,id = paste0(row_number(), "_", N)) %>%  
#   select(id, name)

# fclust_v1 = here("data", "memphis_req/FreightClusters_v1/FreightClusters_v1.shp") %>% 
#   read_sf() %>%  
#   mutate(name = N
#          ,id = paste0(row_number(), "_", N)) %>%  
#   select(id, name)

##manual copy data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
data_terminal = data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
              type = c("Airport","Airport",
                       "Rail Terminal","Rail Terminal","Rail Terminal","Rail Terminal",
                       "Rail Terminal","Rail Terminal","Rail Terminal",
                       "Rail Terminal","Rail Terminal","Water","Water","Water",
                       "FedEx","FedEx","FedEx","FedEx","FedEx"),
     terminal_name = c("MEM Airport - General",
                       "MEM Airport - FedEX Facility","UP - Marion IMX",
                       "UP - Marion IMX","UP - Marion IMX","NS - Rossville IMX",
                       "NS - Forrest Yard IMX","NS - Forrest Yard IMX",
                       "CSX - Memphis/CN - Intermodal Gateway Memphis IMX",
                       "BNSF - Memphis (Shelby) IMX","BNSF - Harvard AR IMX","Presidents Island",
                       "Pidgeon Industrial Park","Port of West Memphis",
                       "FedEx Ground (555 Compress Dr, Memphis, TN 38106)",
                       "MEM Airport - FedEX Facility",
                       "FedEx Freight (461 Winchester Rd, Memphis, TN 38109)",
                       "FedEx Ground (8505 Nail Rd, Olive Branch, MS 38654)",
                       "FedEx Freight (3301 Mid America Blvd, West Memphis, AR 72301)"),
               taz = c("4715700000355",
                       "4715700000352 and '4715700000354","05035000KHA05","05035000KHA04",
                       "0503500000216","4704700000006","4715700000566",
                       "4715700000073",NA,"4715700000333","0503500000019",NA,NA,
                       NA,"4715700000053","4715700000352 and '4715700000354",
                       "471570223211",
                       "280330707211 cut the polygon as shown in sheet AA","05035000KHA71 need to cut"),
    taz_good = c("Y","Y","Y","Y","Y","Y",
                       "Y","Y",NA,
                       "Need to reduce size of polygon, see sheet CC","Y",NA,NA,NA,"Y","Y","Y",
                       "Need to reduce size of polygon","need to cut polygon as sheet BB"),
         taz_note = c("good fit","good fit","good",
                       "good","good","zone is bigger, but main freight",
                       "zone is bigger, but only residential elsewhere",
                       "zone is bigger, but only residential elsewhere",NA,
                       "Not good, too large","large, but good, rest residetnial and farm",NA,
                       NA,NA,"good fit","good fit",
                       "ok, residential outside, so it should be fine",NA,NA)
)

data_terminal_good = data_terminal %>%  
  filter(taz_good == "Y") %>%  
  mutate(taz = str_remove_all(taz, "[:punct:]")) %>%  
  separate(taz, into = c("taz_1", "taz_2"), sep = "and") %>%  
  mutate(across(c("taz_1", "taz_2"), ~str_trim(.x))) %>%
  select(type, terminal_name, taz_1, taz_2) %>% 
  pivot_longer(cols = c("taz_1", "taz_2")
               ,names_to = "taz_desg", values_to = "id") %>%  
  filter(!is.na(id))
  
#explore data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mem_taz %>%  mapview()

mem_bg_sm = mem_bg %>%  
  st_filter(study_area)  

mem_bg_sm %>%  mapview()

mem_taz_sm = mem_taz %>%  
  st_filter(study_area)  

mem_taz_sm %>%  mapview(label = 'id')

mem_specific = mem_taz_sm %>%  
  merge(data_terminal_good
        ,by = 'id')

mem_specific %>%  
  mapview(zcol = 'terminal_name') 

mem_taz %>%  
  filter(str_detect(id, '^471570223211'))
pull(id) %>%  
  sort() %>%
  mutate(id2 = str_trunc(id, 5, "right", ellipsis = "")) %>% 
  filter(id == '471570')
# filter(id2 == '4715702232')


#analysis: split poly==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zzzzAVE = taz_split_object
taz_split_object = mem_taz_sm %>%  
  filter(str_detect(id, "4715700000333") |
           str_detect(id, "05035000KHA71") |
           str_detect(id, "4715700000380") |
           str_detect(id, "2803300000717") |
           str_detect(id, "4715700000596"))  %>% 
  mapview() %>%  
  editMap()

taz_split_object_pro = zzzzAVE %>%  
  .[['all']] %>% 
  filter(!(X_leaflet_id  %in%  c("3143", "1045", "3618"))) %>%
  st_jitter(.002) %>% 
  mapview()

taz_split_object_pro = taz_split_object_pro %>%  
  rbind(taz_split_object$all) %>%  
  mapview()

split_taz = mem_taz_sm %>%  
  filter(str_detect(id, "4715700000333") |
           str_detect(id, "05035000KHA71") |
           str_detect(id, "4715700000380") |
           str_detect(id, "2803300000717") |
           str_detect(id, "4715700000596")) %>%  
  lwgeom::st_split(taz_split_object_pro) %>%  
  st_collection_extract("POLYGON") %>%
  group_by(name) %>%  
  mutate(id = paste0(id, ".", row_number()))
 
mem_taz_sm %>%  
  filter(id %in% save$id) %>% 
  mapview(col.regions = "red")


taz_comb = mem_taz_sm %>%  
  filter(!(str_detect(id, "4715700000333") |
           str_detect(id, "05035000KHA71") |
           str_detect(id, "4715700000380") |
           str_detect(id, "2803300000717") |
           str_detect(id, "4715700000596")))  %>% 
  rbind(split_taz) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(flag = case_when(str_detect(id, "\\.")~T, T~F)) %>% 
  mapview(zcol = "flag")
  
taz_comb %>%  
  write_sf(here("data/memphis_req/split_taz_polys/split_taz_polys.shp"))

test = read_sf(here("data/memphis_req/split_taz_polys/split_taz_polys.shp"))

##load saved data===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#section used to load current saved data 
#for exploration purposes not to build data 
# split_taz_polys = read_sf(here("data/memphis_req/split_taz_polys/split_taz_polys.shp"))
# 
# split_taz_polys %>% 
#   mutate(flag_f = case_when(str_detect(id, "\\.")~T, T~F)) %>% 
#   mapview(zcol = "flag_f")
# 
# mem_taz_sm = mem_taz %>%  st_filter(split_taz_polys)
# mem_bg_sm = mem_bg %>%  st_filter(split_taz_polys)
# 
# mem_bg %>%  
#   filter(id %in% c("471570223211")) %>% 
#   mapview(col.regions = "blue") + 
#   mapview(mem_taz_sm, col.regions = "red")
# mem_bg_sm = mem_bg %>%  st_filter(split_taz_polys)

##make query data===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
split_taz_polys = read_sf(here("data/memphis_req/split_taz_polys/split_taz_polys.shp"))

###grab UN-split poi polys======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#these polys are the POI polys that did not need to be split
pro_taz_for_query = read_rds(here("data/memphis_req/data_for_query"
                                  ,"processed_taz_for_query.rds")) %>%  
  filter(flag_terminal != "not a terminal") %>%  
  mutate(flag_poi = "poi"
         ,group = terminal_name
         ,id = as.character(id)) %>%  
  select(id, geometry, group, flag_sa, flag_poi)

###grab split poi polys=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#section used to load current saved data 
# split_taz_polys %>% 
#   mutate(flag_f = case_when(str_detect(id, "\\.")~T, T~F)) %>% 
#   mapview(zcol = "flag_f")

split_taz_polys_poi = split_taz_polys %>% 
  filter(str_detect(id, "\\."))  %>%  
  select(id) %>% 
  mutate(group = case_when(
    str_detect(id,"05035000KHA71.2")~"FedEx Freight (West Mem.)"
    ,str_detect(id,"4715700000596.2")~"CSX - Memphis/CN"
    ,str_detect(id,"4715700000333.2")~"BNSF - Memphis (Shelby) IMX"
    ,str_detect(id,"2803300000717.2")~"FedEx Ground (Nail rd)"
    ,str_detect(id,"4715700000380.2")~"FedEx Freight (Mem.)"
    ,T~"no_group_id"
      )) %>%  
  mutate(flag_sa = "internal"
         ,flag_poi = case_when(group == "no_group_id"~"not_poi",T~"poi")) 

###clean sa polys===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#need to add port of memphis polys manually 
split_taz_polys_pro_lg = split_taz_polys %>%  
  select(id) %>% 
  filter(!(id %in% split_taz_polys_poi$id)) %>% 
  filter(!(id %in% pro_taz_for_query$id)) %>% 
  mutate(flag_sa = "internal"
         ,flag_poi = "not_poi") %>%  
  mutate(group = case_when((str_detect(id,"0503500000025") |
                              str_detect(id,"0503500000034") |
                              str_detect(id,"0503500000012") |
                              str_detect(id,"0503500000027") |
                              str_detect(id,"0503500000028") |
                              str_detect(id,"0503500000014"))~"Port of West Memphis"
                           ,T~"no_group_id")) %>% 
  mutate(flag_poi = case_when(group == "Port of West Memphis"~"poi", T~flag_poi))

###combine and save out=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
split_taz_polys_pro_comb = split_taz_polys_pro_lg %>%  
  rbind(split_taz_polys_poi) %>% 
  rbind(pro_taz_for_query)

# split_taz_polys_pro_comb %>%
#   mapview(zcol = "group")

clean_date = function(){
  Sys.Date() %>%  
    gsub("-", "\\1", .)
}

str_glue("split_taz_polys_pro_comb_{clean_date()}.shp") %>% 
  here("data/memphis_req/data_for_query", .) %>% 
  write_sf(split_taz_polys_pro_comb, .)   

  
#analysis: automated query======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#makes objects used for automated query 
#does not currently work great because I cannot query TAZs
#I think okay once I get new TAZ tables
taz_sa = mem_taz_sm %>%  
    st_drop_geometry() %>%  
    select(id) %>%  
    mutate(flag_sa = "internal")
  
taz_term = data_terminal_good %>%  
  filter(id != '471570223211') %>%
  select(terminal_name, id) %>%  unique()

taz_processed = mem_taz %>%  
  merge(taz_sa, all = T) %>%  
  mutate(flag_sa = replace_na(flag_sa, "external")) %>%  
  merge(taz_term, all = T) %>%  
  mutate(flag_terminal = case_when(is.na(terminal_name)~"not a terminal", T~"terminal")) 

taz_processed %>%  
  saveRDS(here("data", "memphis_req/processed_taz_for_query.rds"))
  
taz_processed %>%  
  filter(flag_sa == "study_area") %>%  
  mapview(zcol = "flag_terminal")

  
#analysis: custom poly==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##load saved data===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ec_clusters = here("data"
                   ,"memphis_req/Economic Clusters_Updated_V2/FC_Combined.shp") %>% 
  read_sf() 

mem_taz = read_sf(here("data", "memphis_req/taz_area", "taz_area.shp")) %>%  
  st_transform(crs = 4326) %>%  
  select(id) 

sa_polys = read_rds(here('data/memphis_req/data_for_query', "study_area_polys.RDS"))

study_area = sa_polys$sel_counties %>%  
  st_union() %>%  
  st_as_sf() %>% 
  mutate(flag_sa = "internal") %>% 
  st_transform(4326)

mem_taz_sm = mem_taz %>%
  st_join(study_area) %>%  
  mutate(flag_sa = replace_na(flag_sa, "external")
         ,flag_poi = "not_poi")

##buffering=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
buffed = ec_clusters %>% 
  st_cast("MULTIPOLYGON") %>% #makes all same type
  arrange(N) %>%  
  mutate(id = str_glue("{row_number()}_{N}")) %>%
  mutate(flag_poi = "poi") %>% 
  mutate(group = id) %>% 
  select(id, group, flag_poi) %>% 
  st_difference() 

buff_num = 6
radius = 5
for (i in 1:buff_num){
  buffed = buffed %>% 
    quick_buffer(radius = radius) %>%
    st_difference()
} 

buffed = buffed %>%  
  quick_buffer(radius = -1*buff_num*radius)

##buffering and combined========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
custom_polys_sa = st_difference(
  mem_taz_sm %>%
    filter(flag_sa == "internal") 
  ,st_union(buffed)) %>%  
  bind_rows(buffed) %>% 
  mutate(flag_sa = "internal")

##buffering and combined========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#make geom type attribute
custom_polys_sa_filter = custom_polys_sa %>%  
  # st_filter(buffed) %>%
  mutate(sf_geom_typ = st_geometry_type(geometry) %>%  
           as.character())

# custom_polys_sa_not_buffed = custom_polys_sa %>%  
#   filter(!(id %in% custom_polys_sa_filter$id))

message(paste0(unique(custom_polys_sa_filter$sf_geom_typ), collapse = ", "))

#convert mutli-to-polygon unique ID hashing
custom_polys_sa_filter_explode = custom_polys_sa_filter %>% 
  filter(sf_geom_typ == "MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>%  
  group_by(id) %>%  
  mutate(id = str_glue("{id}_{row_number()}")) %>%  
  ungroup() 

custom_polys_sa_poly_only = bind_rows(
  custom_polys_sa_filter %>%  
    filter(sf_geom_typ != "MULTIPOLYGON") 
  ,custom_polys_sa_filter_explode
  # ,custom_polys_sa_not_buffed
  ) %>% 
  mutate(sf_geom_typ = st_geometry_type(geometry) %>%  
           as.character())

unique(custom_polys_sa_poly_only$sf_geom_typ)

custom_polys_sa_poly_only %>%  
  # filter(str_detect(id, "_")) %>%  
  mapview()
  

# custom_polys_sa_filter_explode_sf =  custom_polys_sa_filter_explode %>%  
#   group_by(id) %>%  
#   nest() %>%  
#   mutate(bb_box = map(data, ~st_bbox(.x) %>%
#                         st_as_sfc() %>%
#                         st_area() %>%  
#                         as.numeric())) %>%  
#   unnest(c("data", "bb_box")) %>%
#   ungroup() %>%
#   mutate(area = as.numeric(st_area(geometry))
#          ,area_1 = area < 50) %>% 
#   mutate(ratio = area/bb_box
#          ,ratio_1 = (bb_box/area)
#          ,ratio_2 = (bb_box/area)>5) %>%
#   mutate(flag_abosrb = )
#   st_as_sf() %>%  
#   st_cast("POLYGON")

bolo = custom_polys_sa_filter %>%  
  filter(sf_geom_typ != "MULTIPOLYGON") %>% 
  bind_rows(custom_polys_sa_filter_explode_sf)

mapview(bolo, zcol = "area_1", burst = T)
  
##check=========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bind_rows(
  custom_polys_sa_not_buffed
  ,custom_polys_sa_poly_only
)

##write and save================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write_sf(custom_polys_sa_poly_only
         ,here("data/memphis_req/data_for_query"
               ,str_glue('study_area_custom_polys_20221121.gpkg')
         )
)

ddd = here("data/memphis_req/data_for_query"
            ,str_glue('study_area_custom_polys_20221121.gpkg')
) %>%  
  read_sf()

ddd %>%  
  filter(str_detect(group, "_"))

mapview(ddd, zcol = "flag_poi") + mapview(sa_polys$sel_states)

##buffering=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##load saved data===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#section used to load current saved data 
custom_taz_polys = read_sf(here("data/memphis_req/custom_taz_polys/custom_taz_polys.shp"))



#~~~~~~~~~~~~~~~script end======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#header 1=======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



































