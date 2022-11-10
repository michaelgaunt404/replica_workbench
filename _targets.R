# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(here)
# library(htmltools)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "tigris", "shiny"
               ,"here", "DBI", "bigrquery", "htmltools"
               ,"crosstalk", "leaflet", "leafem", "gauntlet", "DT"
               ,"flexdashboard", "tarchetypes"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
) 

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # tar_target(file_manual_cluster, here("data/geo_tracts_analysis_20220810/selected_geoms_5.RDS"), format = "file")
  # ,tar_target(data_manual_cluster, get_manual_cluster(file_manual_cluster))
  # ,tar_target(data_block_groups_sa, get_block_groups())
  # ,tar_target(data_block_groups_selected_od,  get_block_groups_selected_od())
  # ,tar_target(data_bg_clust, data_bg_clust(data_block_groups_selected_od, data_manual_cluster))
  # ,tar_target(data_queried_trips_2019
  #             ,query_trip_info(data_bgclust = data_bg_clust, data_bgsa = data_block_groups_sa
  #                              ,schema_table = "northwest.northwest_2019_Q4_thursday_trip"))
  # ,tar_target(data_queried_trips_2021
  #             ,query_trip_info(data_bgclust = data_bg_clust, data_bgsa = data_block_groups_sa
  #                               ,schema_table = "northwest.northwest_2021_Q2_thursday_trip"))
  # ,tar_target(data_processed_queries,  make_spatial_networks(data_queried_trips_2019, data_queried_trips_2021))
  # ,tar_target(map_objects, make_network_maps(data_processed_queries, data_manual_cluster))
  # ,tar_render(dashboard_cluster_analysis, "index.rmd")
  
  ##Memphis: Split polys-----
  tar_target(mem_network
             ,here("data/memphis_req/data_queried"
                   ,"network_memphis_pro_20221101.gpkg"), format = "file")
  # ,tar_target(mem_query_poly
  #             ,here("data/memphis_req/data_for_query"
  #                   ,"split_taz_polys_pro_comb_20221103.shp"), format = "file")
  # ,tar_target(mem_data_trip
  #             ,query_replica(data = mem_query_poly 
  #                            ,schema_table ="wsp.south_central_2021_Q4_thursday_trip_taz"
  #                            ,limit = NA))
  # ,tar_target(mem_data_network_objects
  #             ,make_spatial_networks(
  #               network = mem_network
  #               ,query_poly = mem_query_poly
  #               ,data = mem_data_trip))
  # ,tar_target(map_interative_anl, make_network_map_anl(mem_data_network_objects, mem_query_poly))
  # ,tar_target(map_interative_anlt, make_network_map_anlt(mem_data_network_objects, mem_query_poly))
  # ,tar_target(map_interative_anlto, make_network_map_anlto(mem_data_network_objects, mem_query_poly))
  # ,tar_render(dashboard_memphis_origin, "analysis/template_analysis_replica_origin.rmd")
  # ,tar_render(dashboard_memphis_network_agg, "analysis/template_analysis_replica_network_agg.rmd")
  # ,tar_render(dashboard_memphis_origin_poly, "analysis/template_analysis_replica_origin_poly.rmd")
  ##Memphis: Custom polys-----
  ,tar_target(mem_query_poly_custom
              ,here("data/memphis_req/data_for_query"
                    ,"custom_taz_polys_pro_comb_20221108.shp"), format = "file")
  ,tar_target(mem_data_trip_custom
              ,query_replica(data = mem_query_poly_custom 
                             ,schema_table ="wsp.south_central_2021_Q4_thursday_trip_custom_taz"
                             ,limit = NA))
  ,tar_target(mem_data_network_objects_custom
              ,make_spatial_networks(
                network = mem_network
                ,query_poly = mem_query_poly_custom
                ,data = mem_data_trip_custom))
  ,tar_render(dashboard_memphis_origin_custom, "analysis/template_analysis_replica_origin.rmd")
  ,tar_render(dashboard_memphis_network_agg_custom, "analysis/template_analysis_replica_network_agg.rmd")
  ,tar_render(dashboard_memphis_origin_poly_custom, "analysis/template_analysis_replica_origin_poly.rmd")
  
  
)


