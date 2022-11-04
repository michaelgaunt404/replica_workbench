#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script loads packages used for dev of target objects.
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
library(gauntlet)

pkgs = c("tibble", "tidyverse", "sf", "tigris", "shiny"
         ,"here", "DBI", "bigrquery", "htmltools"
         ,"crosstalk", "leaflet", "leafem", "mapview", "gauntlet", "DT"
         ,"flexdashboard", "tarchetypes")

package_load(pkgs)

#import data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location

#helpful targets functions======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location
tar_visnetwork()

tar_make()

get_dgrive_file("icrs")
fread(tar_read('file_icrs'))
make_icrs_mult("file_icrs")
data_icrs_raw

tar_make('file_icrs')
tar_read("file_icrs")
tar_make('data_icrs_raw')
tar_read("data_icrs_raw")
tar_make('file_icrs')
tar_read("file_icrs")


#script end=====================================================================





