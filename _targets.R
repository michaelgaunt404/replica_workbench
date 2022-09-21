# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "tigris", "here", "DBI", "bigrquery"), # packages that your targets need to run
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
  ,tar_target(data_block_group,  get_block_groups_manual())
  ,tar_target(data_bg_clust, data_bg_clust(data_block_group, data_manual_cluster))
  ,tar_target(data_queried_trips , query_trip_info(data_bg_clust))
  ,tar_target(data_spatial_networks,  make_spatial_networks(data_queried_trips))
)