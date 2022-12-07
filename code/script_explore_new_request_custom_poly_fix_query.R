


dbGetQuery_safe = purrr::safely(dbGetQuery)

data = tar_read("mem_query_poly_custom")
schema_table = "wsp.south_central_2021_Q4_thursday_trip_custom_taz"
limit = 50

data = data_pro
db_connection = con
schema_table = schema_table
limit = limit














# limit_query = ifelse(is.na(limit), ";", str_glue("limit {limit};"))

poly_query_df = data %>%  
  st_drop_geometry() %>% 
  filter(group != "no_group_id") %>%  
  select(group, id) 

message("Begin query...")

query_index = poly_query_df$group %>%  
  unique() %>%  
  sort() %>%  
  .[2]

queried_data = query_index %>% 
  map(~{
    
    query_focus_id = poly_query_df %>%  
      filter(group == .x) %>%  
      pull(id) %>% 
      paste0("'", ., "'") %>% 
      paste(collapse = ", ") 
    
    temp_table_name = "replica-customer._script501e76e2c96f6a4f735ef5bd51cc514d7ee8ffae.temp_test_2"
    
    query_string = str_glue("SELECT count(*) as count FROM `{schema_table}`
    WHERE start_taz IN ({query_focus_id}) AND 
                            mode = 'COMMERCIAL'")
    
    limit = query_database_count(db_connection, query_string)
    limit_k_adj = ceiling(limit/1000)
    
    message(paste0("''''\nQuerying for ", .x))
    message(str_glue("There are {limit} records for this group..."))
    message(str_glue("Will make {limit_k_adj} queries of 1000 records...\n\n''''"))
    
    temp_data = list(rep(x, limit_k_adj) %>%  head(3)
                ,seq(1, limit_k_adj, 1) %>%  head(3)
    ) %>% 
      pmap(~{
        
        lim_bttm = ((.y-1)*1000)
        lim_uppr = ((.y)*1000)
        
        query_string = str_glue("SELECT * FROM `{temp_table_name}`
                                WHERE row >= {lim_bttm} AND 
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
    
    
  })







test = tar_read("mem_data_trip_custom")












































    
    
    
   