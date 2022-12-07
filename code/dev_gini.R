


library(DescTools)

#define vector of incomes
x <- c(50, 50, 70, 70, 70, 90, 150, 150, 150, 150)
x = c(5, 10)

x = c(.5, .9)

x = c(.5)

#calculate Gini coefficient
Gini(x, unbiased=FALSE, conf.level = .95)

[1] 0.226







spatial_data_filtered %>%  
  filter(count == 98501744571473) %>%  
  mapview()




test = filtered_data %>%  
  unnest_network_links(.) %>%  
  # filter(network_link_ids_trunc == "98501744571473") %>% 
  count_percent_zscore_dt(
    grp_c = c('network_link_ids_trunc', "start_taz")
    ,grp_p = c('network_link_ids_trunc')
    ,col = 'count')



test_agg = test %>%  
  group_by(network_link_ids_trunc) %>%  
  summarise(gni = Gini(percent, unbiased=FALSE) %>%  
              dgt2()
            ,count = sum(count))

yolo = spatial_data = merge_processed_links(data = test_agg
                                            ,network = network_lnk
                                            ,type = "LINESTRING")

yolo %>%  
  filter(count > 10) %>% 
  mapview(zcol = "gni", lwd = "count")


year_change_origin = list(
  list(data_pro_19, data_pro_21)
  ,list(2019, 2021)
) %>%  
  pmap(~{
    .x  %>% 
      # nrow()
      count_percent_zscore(
        grp_c = c('start_taz_group', 'start_taz')
        ,grp_p = c()
        ,col = count) %>%
      mutate(year = .y) %>%
      arrange(desc(count))
  })

year_change_origin_comb = year_change_origin %>%  
  reduce(bind_rows) 

# year_change_origin_comb_pro = year_change_origin_comb %>%  
#   pivot_wider(
#     id_cols = starts_with("start")
#     ,names_from = "year"
#     ,values_from = "count"
#     ,names_glue = "count_{year}"
#   ) %>%  
#   mutate(change_count = count_2021-count_2019
#          ,change_pct = 100*dgt3((change_count)/count_2019))


year_change_origin_comb_pro = 
  year_change_origin_comb %>%  
  arrange(year) %>%  
  group_by(start_taz) %>%  
  mutate(change_count = count-lag(count)
         ,change_pct = change_count/lag(count)
         ,change_pct = replace_na(change_pct, 0)
           ) %>%  
  ungroup()

year_change_origin_comb_pro_pivot = 
  year_change_origin_comb_pro %>%  
  pivot_wider(
    id_cols = "start_taz"
    ,values_from = "count"
    ,names_from = "year"
  ) %>%  
  mutate(start_taz = gsub(".*_", "\\1", start_taz)) %>% clipr::write_clip()

year_change_origin_comb_pro %>%
  filter(year == 2021) %>% 
  summarise(
    across(change_pct
           ,list(mean = mean, sd = sd)
           ,.names = "{.col}_{.fn}"))


year_change_origin_comb_pro %>%
  filter(year == 2021) %>%  
  ggplot() +
  geom_histogram(aes(change_pct)) 

replicate()

year_change_origin_comb_pro %>% 
  ggplot() + 
  geom_point(aes(year, change_pct, color = change_pct, size = count))



index_pct_chnage = year_change_origin_comb_pro %>%
  filter(year == 2021) %>%  
  pull(change_pct)

boot.mm <- replicate(10000
                     ,mean(sample(index_pct_chnage, 60, replace = TRUE))
                     )

hist(boot.mm, breaks = seq(-.5, .5, .02))
mean(boot.mm<0)

quantile(boot.mm, c(.025, .975))






test = seq(5, 100, 5) %>%  
  map(~{
    replicate(10000
              ,mean(sample(index_pct_chnage, .x, replace = TRUE))
    ) %>%  
      quantile(c(.025, .975))
    
  })


test %>%  
  reduce(bind_rows) %>%  
  mutate(index = seq(5, 100, 5)) %>%  
  ggplot() +  
  # geom_point(aes(index, (`97.5%`-`2.5%`)/lag(`97.5%`-`2.5%`)))
  geom_point(aes(index, `97.5%`-`2.5%`))













