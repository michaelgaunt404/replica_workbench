
library(mapview)
library(tigris)
library(sf)
library(leaflet)
library(mapedit)
library(tidyverse)

input_bg_year = 2010 

states = states() %>%  
  st_transform(crs = 4326)

map_edit_states = states %>%  
  mapview() %>% 
  editMap() 

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

edit_blockgroup = function(){
  for(i in 1:99){
    var_edit = readline("Would you likk to make subselections? [y/n]")
    if(var_edit %in%  c("y", "n" )){break}
    message("Incorrect input....")
  }

  if(var_edit != "n" ){
    
    for(i in 1:99){
      sa_blockgroup_subset = sa_blockgroup %>%  
        selectFeatures()
      
      sa_blockgroup_subset_1 = sa_blockgroup_subset %>% 
        st_transform(crs = 4326) 
      
      message("The following map is the result of your subset....")
      
      print(sa_blockgroup_subset_1)
      
      leaflet() %>% 
        addPolygons(data = sa_blockgroup
                    ,group = "Study Area"
                    ,color = "black"
                    ,fillColor = "blue"
                    ,fillOpacity = .1
                    ,weight = 1) %>%
        addPolygons(data = sa_blockgroup_subset_1
                    ,group = "User Selection"
                    ,color = "black"
                    ,fillColor = "red"
                    ,fillOpacity = .5
                    ,weight = 1)
        
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
}


edit_blockgroup()








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

#get and select blockgroups
blockgroups = list(sel_counties$STATEFP, sel_counties$COUNTYFP) %>% 
  pmap(~block_groups(state = .x, county = .y, year = input_bg_year) %>%  
         st_transform(crs = 4326) %>%  
         st_filter(map_edit_states_aug)) %>%  
  reduce(rbind)

sel_blockgroups = blockgroups %>%  
  selectFeatures()

sel_blockgroups = blockgroups %>%  
  mapview() %>%
  editMap()

sel_blockgroups[['drawn']] %>%  
  mapview()

sel_blockgroups[['all']] %>%  
  mapview()

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


















