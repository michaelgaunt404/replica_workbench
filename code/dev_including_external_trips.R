






query_replica(data = mem_query_poly_custom 
              ,schema_table = "wsp.south_central_2021_Q4_thursday_trip_custom_taz"
              ,temp_table = "replica-customer._script25786380456a801dfb962e82f9f96b037012d3c5.temp_table_k_lmt_dl"
              ,limit = NA)


# query_replica(
data = tar_read("mem_query_poly_custom") 
schema_table ="wsp.south_central_2021_Q4_thursday_trip_custom_taz"
temp_table = "replica-customer._script25786380456a801dfb962e82f9f96b037012d3c5.temp_table_k_lmt_dl"
limit = NA
  

data = tar_read("mem_data_trip_custom")

data %>%  
  pull(index) %>%  max()

data %>%  
  pull(start_taz) %>%  unique()



query_poly %>%  
  mapview()


data_pro %>%  
  select(contains("end")) %>%  
  unique() %>%  
  # filter(flag_sa_end != "internal")
  view()
select(!network_link_ids)





rm_self = T
data_pro = data %>%
  mutate(count = 1) %>%
  mutate(dataset = "wsp.south_central_2021_Q4_thursday_trip_custom_taz") %>%
  select(c('dataset', 'activity_id', 'start_taz'
           ,'end_taz', 'vehicle_type', 'network_link_ids', 'count')) %>%
  merge(., query_poly_df_poi %>%
          select(id, start_taz_group)
        ,by.x = "start_taz", by.y = "id") %>% 
  merge(., query_poly_df_notpoi %>%
          select(id, end_taz_group, flag_poi) %>%
          rename(flag_poi_end = flag_poi)
        ,by.x = "end_taz", by.y = "id", all = T) %>%  
  mutate(flag_sa_end = case_when(end_taz %in% unique(query_poly$id)~"internal"
                                 ,T~"external")
         ,end_taz = case_when(end_taz %in% unique(query_poly$id)~end_taz
                              ,T~"out of study area")) %>% 
  {if (rm_self) (.) %>%  filter(start_taz != end_taz) else .} %>%  
  {if (rm_ext) (.) %>%  filter(flag_sa_end != external) else .}

data_pro_nnet = data_pro %>% select(!network_link_ids)

merged = data_pro_nnet %>%  
  mutate(flag_sa_end = case_when(end_taz %in% unique(query_poly$id)~"internal"
                                 ,T~"external")
         ,end_taz = case_when(end_taz %in% unique(query_poly$id)~end_taz
                                  ,T~"out of study area")) 


data_pro = merged %>%  
  group_by(flag_sa_end, flag_poi_end) %>%  
  sample_n(3000) %>%  
  ungroup()
  










agg_od = data_pro %>%
  count_percent_zscore_dt(
    grp_c = c('dataset', 'start_taz', 'start_taz_group', 'vehicle_type', 'end_taz')
    ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'end_taz')
    # ,grp_p = c('dataset', 'start_taz', 'start_taz_group', 'vehicle_type')
    ,col = 'count'
    ,rnd = 2) %>%
  .[,`:=`(count_pRank_adj = dgt2(percent_rank(count))
          ,count_adj_max = dgt2(count/max(count)))
    ,by = .(dataset, start_taz_group, vehicle_type)] %>% 
  .[,`:=`(ttl_count_orgin = sum(count)), by = .(start_taz_group)] %>% 
  .[,`:=`(ttl_count_orgin_type = sum(count)), by = .(start_taz_group, vehicle_type)] %>% 
  data.frame() %>%  
  rename(percent_origin_trips = percent) %>% 
  mutate(label = str_glue(
    "Origin Group: {start_taz_group} 
    <br> Total Origin Trips: {ttl_count_orgin}
    <br> Destination Poly: {end_taz}
    <hr>
    Metrics  Adj for Vehicle Type 
    <br> Vehicle Type: {vehicle_type}
    <br> Total Origin Trips: {ttl_count_orgin_type} ({100*dgt2(ttl_count_orgin_type/ttl_count_orgin)}% of total)
    <br> Trips to Destination: {count} ({100*dgt2(percent_origin_trips)}% of link total)
    <br> Destination count/max(count): {100*count_adj_max}%
    <br> % of Origin Total: {100*percent_origin_trips}%")) %>% 
  data.table()




agg_o = agg_od %>% 
  count_percent_zscore_dt(grp_c = c("start_taz", "vehicle_type", "flag_sa_end")
                          ,grp_p = c("start_taz")
                          ,col = 'count'
                          ,rnd = 2) %>%  
  .[order(start_taz, vehicle_type)] %>% 
  .[,`:=`(count_ttl_veh = sum(count)), by = .(vehicle_type)] %>% 
  .[,`:=`(count_tll_origin = sum(count)), by = .(start_taz)] %>% 
  .[,`:=`(vehicle_type = ifelse(str_detect(vehicle_type, "HEAVY"), "heavy", "medium"))] %>% 
  data.frame() %>% 
  pivot_wider(names_from = vehicle_type
              ,values_from = c(count, percent, count_ttl_veh)) %>%  
  mutate(label = str_glue(
    "Origin TAZ: {start_taz} 
    <br>Total Origin Trips: {count_tll_origin}
    <br>Heavy Duty Trips: {count_heavy} ({100*percent_heavy}%)
    <br>Medium Duty Trips: {count_medium} ({100*percent_medium}%)"))





data_pro %>% 
  count_percent_zscore_dt(grp_c = c("start_taz", "flag_sa_end")
                          ,grp_p = c("start_taz")
                          ,col = 'count'
                          ,rnd = 2) %>%  
  na.omit() %>% 
  separate(start_taz, c('index', 'start_taz'), "_") %>%  
  arrange(as.numeric(index)) %>%  
  group_by(flag_sa_end) %>%
  summarise(count = sum(count)
            ,percent_mean = dgt2(mean(percent))
            ,percent_sd = dgt2(sd(percent))) %>%
  # ggplot() + 
  # geom_density(aes(percent)) + 
  # facet_grid(rows = vars(flag_sa_end))
  mutate(percent = dgt2(count/sum(count))) %>% 
  select(flag_sa_end, count, percent, contains("percent")) %>% 
  reactable(#bordered = TRUE
            highlight = TRUE
            ,defaultColDef = colDef(
              # header = function(value) gsub(".", " ", value, fixed = TRUE),
              cell = function(value) format(value, nsmall = 1),
              # align = "center",
              minWidth = 70,
              headerStyle = list(background = "#f7f7f8")
            ), columns = list(
              flag_sa_end = colDef(name = "Destination Relation to Study Area")
              ,count = colDef(name = "Total Count")
              ,percent = colDef(name = "Total Percent")
              ,percent_mean = colDef(name = "Average (by origin TAZ)")
              ,percent_sd = colDef(name = "Std. (by origin TAZ)")
            ), columnGroups = list(
              colGroup(name = "Percent Measures", columns = c("percent", "percent_mean", "percent_sd"))
              # ,colGroup(name = "Petal", columns = c("Petal.Length", "Petal.Width"))
            ))


yolo = data_pro %>% 
  count_percent_zscore_dt(grp_c = c("start_taz", "vehicle_type", "flag_sa_end")
                          ,grp_p = c("start_taz", "vehicle_type")
                          ,col = 'count'
                          ,rnd = 2) %>%  
  na.omit() %>% 
  separate(start_taz, c('index', 'start_taz'), "_") %>%  
  arrange(as.numeric(index)) %>%  
  select(!index) %>% 
  mutate(vehicle_type = str_to_lower(gsub("_.*", "\\1", vehicle_type))) 
yolo_sd = SharedData$new(yolo)

bscols(widths = c(12)
       ,filter_select("yolo_sd", "yolo_sd", yolo_sd, ~flag_sa_end)
       ,reactable(yolo_sd
                  ,groupBy = c("start_taz", "flag_sa_end")
                  ,filterable = TRUE
                  ,columns = list(
                    count = colDef(aggregate = "mean", format = colFormat(separators = TRUE))
                  )))
  














