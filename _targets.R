# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "tigris"
               ,"here", "DBI", "bigrquery"
               ,"crosstalk", "leaflet", "gauntlet"), # packages that your targets need to run
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
  tar_target(file_manual_cluster, here("data/geo_tracts_analysis_20220810/selected_geoms_5.RDS"), format = "file")
  ,tar_target(data_manual_cluster, get_manual_cluster(file_manual_cluster))
  ,tar_target(data_block_groups_sa, get_block_groups())
  ,tar_target(data_block_groups_selected_od,  get_block_groups_selected_od())
  ,tar_target(data_bg_clust, data_bg_clust(data_block_groups_selected_od, data_manual_cluster))
  ,tar_target(data_queried_trips_2019
              ,query_trip_info(data_bgclust = data_bg_clust, data_bgsa = data_block_groups_sa
                               ,schema_table = "northwest.northwest_2019_Q4_thursday_trip"))
  ,tar_target(data_queried_trips_2021
              ,query_trip_info(data_bgclust = data_bg_clust, data_bgsa = data_block_groups_sa
                                ,schema_table = "northwest.northwest_2021_Q2_thursday_trip"))
  ,tar_target(data_processed_queries,  make_spatial_networks(data_queried_trips_2019, data_queried_trips_2021))
)



