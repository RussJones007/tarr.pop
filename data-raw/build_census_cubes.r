# ------------------------------------------------------------------------------------------------------------------->
# Script:  build_census_cubes.r
# Description:
# Sources two scripts and then calls the build functions for each census data cube.  The census
# build functions are located in census_data_2.r
# 
# 
# Steps:
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  October 9, 2026
# 
# ------------------------------------------------------------------------------------------------------------------->
source("data-raw/control_def.r")
options("tarr.pop.census_build" = FALSE)  # prevent auto build when calling census_data_2.r
source("data-raw/census_data_2.r")

census_cube_root <- tarr.pop::init_cubes()
build_census_decennial(census_cube_root, cache_file = NULL)
build_census_estimates(census_cube_root, input_dir = "G:/Data/Population/Estimates/Census")
build_census_zcta(census_cube_root)
tarr.pop::rebuild_poparray_registry(census_cube_root)

