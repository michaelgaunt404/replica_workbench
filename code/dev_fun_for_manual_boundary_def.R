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
library(mapview)
library(tigris)
library(sf)
library(leaflet)
library(mapedit)
library(tidyverse)

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


#run through the scripts=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this is what the workflow should be but it has problems when placed in a function
input_bg_year = 2010

states = states() %>%  
  st_transform(crs = 4326)

sel_states = selectFeatures(states)

message("Please select counties...")

counties = counties(state = sel_states$STATEFP)

sel_counties = selectFeatures(counties) %>% 
  st_transform(crs = 4326) 

message("Extracting blockgroups...")

sel_blockgroup = sel_states$STATEFP %>%  
  map(~block_groups(state = .x, year = input_bg_year) %>%  
        st_transform(crs = 4326) %>%  
        st_filter(sel_counties)) %>%  
  reduce(rbind)

sel_blockgroup

test = block_groups(state = 28, year = input_bg_year) %>%  
  filter()

extracted_blockgroups = sel_counties %>%  
  st_drop_geometry() %>% 
  group_by(STATEFP) %>%  
  group_map(~{
    block_groups(state = unique(.x$STATEFP), year = input_bg_year) %>%
      filter(COUNTYFP %in% unique(.x$COUNTYFP))
  }, .keep = T) %>% 
  reduce(rbind) %>% 
  st_transform(crs = 4326) %>% 
  select(contains("GEOID")) %>% 
  mutate(flag_sa = "internal") %>%
  mutate(flag_poi = "not_poi") %>%  
  rename_with(~str_remove_all(.x, "[:digit:]"))

sel_blockgroups = extracted_blockgroups %>% 
  selectFeatures() %>%
  st_transform(crs = 4326) %>% 
  mutate(flag_poi = "poi")

extracted_blockgroups = extracted_blockgroups %>%  
  filter(!(GEOID %in% sel_blockgroups$GEOID)) %>%  
  rbind(sel_blockgroups)

list(
  sel_states = sel_states
  ,sel_counties = sel_counties
  ,extracted_blockgroups = extracted_blockgroups
) %>%  
  saveRDS(., file = 
            here('data/memphis_req/data_for_query', "study_area_polys.RDS"))


#functions that do not work=====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
select_study_area_polys = function(input_bg_year = 2010){
  # input_bg_year = 2010 
  
  states = states() %>%  
    st_transform(crs = 4326)
  
  sel_states = selectFeatures(states)
  
  message("Please select counties...")
  
  counties = counties(state = sel_states$STATEFP)
  
  sel_counties = selectFeatures(counties) 
  
  message("Extracting blockgroups...")
  
  sel_blockgroup = sel_states$STATEFP %>%  
    map(~block_groups(state = .x, year = input_bg_year) %>%  
          st_transform(crs = 4326) %>%  
          st_filter(sel_counties)) %>%  
    reduce(rbind)
  
  return(
    list(sel_states
         ,sel_counties
         ,sel_blockgroup)
  )
}

select_study_states = function(input_bg_year = 2010){
  # input_bg_year = 2010 
  
  states = states() %>%  
    st_transform(crs = 4326)
  
  message("Please select counties...")
  
  sel_states = selectFeatures(states)
  
  sel_states
}

extract_study_area_blockgroups = function(states, input_bg_year = 2010){
  message("Please select counties...")
  
  counties = counties(state = states$STATEFP)
  
  sel_counties = selectFeatures(counties) 
  
  message("Extracting blockgroups...")
  
  sel_blockgroup = states$STATEFP %>%  
    map(~block_groups(state = .x, year = input_bg_year) %>%  
          st_transform(crs = 4326) %>%  
          st_filter(sel_counties)) %>%  
    reduce(rbind)
  
  return(
    list(sel_counties
         ,sel_blockgroup)
  )
}

states = select_study_states()




edit_blockgroup = function(){
  for(i in 1:99){
    var_edit = readline("Would you likk to make subselections? [y/n]")
    if(var_edit %in%  c("y", "n" )){break}
    message("Incorrect input....")
  }

  if(var_edit != "n" ){
    
    for(i in 1:99){
      print(i)
      if (exists('sa_blockgroup_subset')){rm(sa_blockgroup_subset)}
      
      sa_blockgroup_subset_edit = map_sa_blockgroup@map %>%  
        editMap()
      
      saveRDS(sa_blockgroup_subset_edit$all, here("data/mapedit/temp_sel_bg.rds"))
      map_edit_sa_blockgroup = read_rds(here("data/mapedit/temp_sel_bg.rds"))
      
      fltrd_bg = sa_blockgroup %>%  
        st_join(map_edit_sa_blockgroup) %>%  
        filter(!is.na(X_leaflet_id ))
      
      (mapview(sa_blockgroup
               ,layer.name = "User Defined Study Area"
               ,alpha.regions = .2) + 
          mapview(fltrd_bg
                  ,col = 'fltrd_bg'
                  ,layer.name = "Block Group Subselection"
                  ,col.regions = "red") ) %>% print()
      
      
      # sa_blockgroup_subset = sa_blockgroup %>%  
      #   selectFeatures()
      
      # saveRDS(sa_blockgroup_subset, here("data/mapedit/temp_sel_bg.rds"))
      # map_edit_sa_blockgroup = read_rds(here("data/mapedit/temp_sel_bg.rds"))
      # 
      # message("The following map is the result of your subset....")
      # 
      # (mapview(sa_blockgroup
      #          ,layer.name = "User Defined Study Area"
      #          ,alpha.regions = .2) + 
      #     mapview(map_edit_sa_blockgroup
      #             ,layer.name = "Block Group Subselection"
      #             ,col.regions = "red") ) %>% print()
      

      for(i in 1:99){
        var_edit_again = readline("Would you like to redo the subsetting process? [y/n]")
        if(var_edit_again %in%  c("y", "n" )){break}
        message("Incorrect input....")
      }
      
      if(var_edit_again != "y" ){break}
    }
    
  } else {
    map_edit_sa_blockgroup = map_edit_states
  }
  
  return(list("study_area" = map_edit_states
       ,"subselection" = map_edit_sa_blockgroup))
}


boundaries = edit_blockgroup()








sa_blockgroup_subset = sa_blockgroup %>%  
  selectFeatures()

message("The following map is the result of your subset....")

mapview(sa_blockgroup_subset, layer.name = "User Defined Subset") + 
  mapview(sa_blockgroup, layer.name = "Study Area"
          ,alpha.regions = .2,col.regions = "red")



#==============================


input_bg_year = 2010 

#get and select states
states = states() %>%  
  st_transform(crs = 4326)

sel_states = states %>%  
  selectFeatures()

#get and select counties 
counties = sel_states$STATEFP %>%  
  map(~counties(state = .x)) %>%  
  reduce(rbind)

sel_counties = counties %>%  
  selectFeatures()

sel_counties %>%  
  st_drop_geometry() %>%
  clipr::write_clip()

#get and select blockgroups
blockgroups = list(sel_counties$STATEFP, sel_counties$COUNTYFP) %>% 
  pmap(~block_groups(state = .x, county = .y, year = input_bg_year) %>%  
         st_transform(crs = 4326)) %>%  
  reduce(rbind)

sel_blockgroups = blockgroups %>%  
  selectFeatures()

map_bg = blockgroups %>%  
  mapview()

sel_blockgroups = map_bg@map %>%
  editMap()

# map_temp = sel_blockgroups$all %>%  
#   mapview()
# 
# map_temp %>%  
#   editMap()

sel_blockgroups$all %>% 
  saveRDS("data/temp_map_delete.rds")

temp_object = read_rds("data/temp_map_delete.rds")

 
  mapview(temp_object) + map_bg

sel_blockgroups[['all']] %>%  
  mapview() + map_bg

sel_blockgroups[['finished']] %>%  
  mapview()

map_edit_states_aug = map_edit_states$all %>%  
  st_transform(crs = 4326)

states_fltrd = states %>%  
  st_filter(map_edit_states_aug)

sa_blockgroup = states_fltrd$STATEFP %>%  
  map(~block_groups(state = .x, year = input_bg_year) %>%  
        st_transform(crs = 4326) %>%  
        st_filter(map_edit_states_aug)) %>%  
  reduce(rbind)

map_sa_blockgroup = sa_blockgroup %>%  
  mapview(zcol = "TRACTCE10")

map_sa_blockgroup


















