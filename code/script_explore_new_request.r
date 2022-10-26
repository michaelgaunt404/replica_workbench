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

fclust_v1 = here("data", "memphis_req/FreightClusters_v1/FreightClusters_v1.shp") %>% 
          read_sf() %>%  
  mutate(name = N
         ,id = paste0(row_number(), "_", N)) %>%  
  select(id, name)

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
  # filter(!is.na(terminal_name)) %>%  
  # st_jitter(.005) %>%
  mapview(zcol = 'terminal_name') 

mem_taz %>%  
  filter(str_detect(id, '^2803307'))
  pull(id) %>%  
  sort() %>%
  mutate(id2 = str_trunc(id, 5, "right", ellipsis = "")) %>% 
  filter(id == '471570')
  filter(id2 == '4715702232')

#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

bind_cols(test = seq(1, 3, 1)
           ,test4 = seq(1, 10, 1)) %>%  
  print()
  
#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fclust_v1_buf = fclust_v1 %>% 
  quick_buffer(radius = 10) %>%
  st_difference() %>%
  quick_buffer(radius = 10) %>%
  st_difference() %>%
  quick_buffer(radius = 10) %>%
  st_difference() %>%
  quick_buffer(radius = -30) %>%
  mapview()

mapview(mem_bg_sm) + mapview(fclust_v1)
i = 9
test = mem_bg_sm
for (i in 1:nrow(fclust_v1)){
  print(i)
  test = st_difference(test, fclust_v1[i,])
  
}
 
differenced = st_difference(mem_taz_sm, st_union(fclust_v1_buf)) %>%  
  select(id, name) %>%  
  mutate(flag_target = "f") %>% 
  st_collection_extract("POLYGON")

differenced %>%  st_collection_extract("POLYGON")
  
differenced %>% mapview()

test = rbind(differenced, 
             fclust_v1_buf %>%  
        mutate(flag_target = "t"))

test %>%  
  write_sf(here("data/memphis_req/custom_taz_polys/custom_taz_polys.shp"))

merged = read_sf(here("data/memphis_req/custom_taz_polys/custom_taz_polys.shp"))
  
merged %>%  
  mapview(zcol = "flg_trg")

#save=== data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































