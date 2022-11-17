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
# library(tidyverse)
# library(sf)
# library(tigris)
# library(gauntlet)

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

#functions======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                   col, prefix = NULL, rnd = NULL, cntr_scl = FALSE){
  
  tmp = data %>%
    data.table::data.table() %>%
    .[,.(count = sum(.SD)), .SDcols = col, by = grp_c] %>%
    .[,`:=`(percent = (count/sum(count)) %>%
              { if (!is.null(rnd)) round(., rnd) else .}), by = grp_p] %>%
    { if (cntr_scl) (.) %>%
        .[,`:=`(zscore = as.vector(scale(count))), by = grp_z]
      else .}
  
  if (is.null(prefix)){
    tmp = tmp
  } else {
    newname1 = str_glue("{prefix}_count")
    newname2 = str_glue("{prefix}_percent")
    rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
  
  return(tmp)
  
}

#data prep and query============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
purrr_get_safe_results = function(object, type = "result"){
  object %>%
    map(~.x[[type]])
}

dbGetQuery_safe = purrr::safely(DBI::dbGetQuery)

query_database = function(db_connection, query_string, query_item, dataset){
  temp_data = dbGetQuery(db_connection, query_string)
  
  message(paste0(nrow(temp_data), " rows returned..."))
  
  temp_data %>% 
    mutate(queried_group = query_item
           ,dataset = dataset)
}

query_database_safe = purrr::safely(query_database)

query_database_count = function(db_connection, query_string){
  temp = dbGetQuery(db_connection, query_string)

  temp[[1]]
}

query_poi_to_everything = function(data, db_connection, schema_table, limit = 50){
  
  limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(group != "no_group_id") %>%  
    select(group, id) 
  
  message("Begin query..")
  
  query_index = poly_query_df$group %>%  
    unique() %>%  
    sort() 
  
  queried_data = query_index %>% 
    map(~{
      
      message(paste0("Querying for ", .x))
      
      query_focus_id = poly_query_df %>%  
        filter(group == .x) %>%  
        pull(id) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ") 
      
      query_string = str_glue("SELECT * FROM
  `{schema_table}`
 WHERE start_taz IN ({query_focus_id}) AND mode = 'COMMERCIAL' {limit_query}")
      
      temp_data = query_database_safe(db_connection, query_string, .x,  schema_table)
      
    })
  
  message("++++\n++++\nQuery complete....")
  
  queried_data_results = queried_data %>%  
    purrr_get_safe_results() %>% 
    reduce(bind_rows)
  
  bad_groups = query_index[!(query_index %in% 
                               queried_data_results$queried_group)]
  
  if (length(bad_groups)>0){
    message(str_glue('Unsuccessfully queried groups: \n{paste(bad_groups,collapse = "\n")}'))
  } else {
    message("All groups successfully queried!")
  }
  queried_data_results
}

query_poi_to_everything_k_lmt = function(data, db_connection, schema_table, temp_table, limit = 50){
  
  poly_query_df = data %>%  
    st_drop_geometry() %>% 
    filter(group != "no_group_id") %>%  
    select(group, id) 
  
  message("Begining query...")
  
  query_index = poly_query_df$group %>%  
    unique() %>%  
    sort() 
  
  
  
  queried_data = query_index %>% 
    map(~{
      
      x = query_index[[4]]
      
      query_focus_id = poly_query_df %>%  
        filter(group == .x) %>%  
        pull(id) %>% 
        paste0("'", ., "'") %>% 
        paste(collapse = ", ") 
      
      temp_table_name = temp_table
      
      #i think the second one should be used
      query_string = str_glue("SELECT count(*) as count FROM `{temp_table_name}`
    WHERE start_taz IN ({query_focus_id})")
      
      query_string = str_glue("SELECT max(index) as count FROM `{temp_table_name}`
    WHERE start_taz IN ({query_focus_id})")

      yolo2 = dbGetQuery(db_connection, query_string)
      
      limit = query_database_count(db_connection, query_string)
      limit_k_adj = ceiling(limit/1000)
      print(limit_k_adj)
      
      if (limit == 0){
        message(paste0("''''\nQuerying for ", .x))
        message(str_glue("There are {limit} records for this group, skipping..."))

      } else {

        message(paste0("''''\nQuerying for ", .x))
        message(str_glue("There are {limit} records for this group..."))
        message(str_glue("Will make {limit_k_adj} queries of 1000 records...\n\n''''"))

        temp_data = list(rep(x, limit_k_adj)
                         ,seq(1, limit_k_adj, 1)
        ) %>%
          pmap(~{

            lim_bttm = ((.y-1)*1000)
            lim_uppr = ((.y)*1000)

            query_string = str_glue("SELECT * FROM `{temp_table_name}`
                                WHERE 
                                start_taz IN ({query_focus_id}) AND
                                row >= {lim_bttm} AND
                                row < {lim_uppr};")

            data = dbGetQuery_safe(
              db_connection
              ,query_string)

            if ((!is.null(data$result) & is.null(data$error))==TRUE){
              message(str_glue("''''\nQuery {.y} of {limit_k_adj} successful -- {nrow(data$result)} records received..."))
            } else {
              message("Query {.y} of {limit_k_adj} unsuccessful...")
            }

            data
          })

        temp_data %>%
          purrr_get_safe_results() %>%
          reduce(bind_rows) %>%
          arrange(row)

      }
    })
  
  message("++++\n++++\nQuery complete....")
  
  queried_data_results = queried_data %>%  
    reduce(bind_rows)
  
  bad_groups = query_index[!(query_index %in% 
                               queried_data_results$start_taz)]
  
  if (length(bad_groups)>0){
    message(str_glue('Groups unsuccessfully queried: \n{paste(bad_groups,collapse = "\n")}'))
  } else {
    message("All groups successfully queried!")
  }
  queried_data_results
}

query_replica = function(data, schema_table, temp_table, limit = 50){
  # data = tar_read('mem_query_poly')
  # schema_table ="wsp.south_central_2021_Q4_thursday_trip_taz"
  # data = tar_read("mem_query_poly_custom")
  # schema_table = "wsp.south_central_2021_Q4_thursday_trip_custom_taz"
  # temp_table = "replica-customer._scripteb78f2c5e64e94ee31b960a8052922d00aa7c769.temp_table_k_lmt_dl"
  
  # browser()

  data_pro = read_sf(data)
  
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "replica-customer",
    dataset = schema_table
  )
  
  queried_data =  query_poi_to_everything_k_lmt(data = data_pro
                                          ,db_connection = con
                                          ,schema_table = schema_table
                                          ,temp_table = temp_table
                                          ,limit = limit) 
  
  queried_data
}



#script end=====================================================================



