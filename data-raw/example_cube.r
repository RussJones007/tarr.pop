# -------------------------------------------------------------------------------------->
# Script:  example_cube.r
# Description:
#    Creates a smaller example cube tha can be used for documentation and 
#    vignette examples.  The Texas Demogrphic center population estimates are used as they 
#    present a challenge in how to hanlde the schema changes that were made betweeen
#    2016 through 2018.
#
# Steps:
#    Run this script after setting up the environment in control_def.r
#    Read and select four years of data from four counties.  Save as a
#    data set in the package that can be loaded via sysfile
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: October 12, 2026
# Revised:
# -------------------------------------------------------------------------------------->

# Use the tdc estimates file
tdc_all <- open_poparray(population$texas.demographic.center$estimates)

counties <- c("Collin", "Dallas", "Denton", "Tarrant")
dimnames(tdc_all) |> names()

# Subsetting preserves the source dimension semantics and adapts schema
# applicability to the selected years without realizing the population array.
tdc_example <- tdc_all[
  year = as.character(2015:2018),
  area.name = counties,
  drop = FALSE
]

dim_semantics(tdc_example)

source_info <- list(
  note            = c("This is the package example poparray for use in documentation examples.",
                      "Users can open and use it for practice."),
  source          = c("Originally from the Texas Demographic Center.",
                      "Contains county data for 2015 through 2018  of - Collin, Dallas, Denton and Tarrant.",
                      "Dimesions are year, area.name, sex, age.chr, and race.eth"),
  updated         = glue::glue("Extracted from the TDC estimates file on {today()}"),
  population_type = "Estimate"
)

source_meta(tdc_example) <- source_info
lobstr::obj_size(tdc_example)
source_meta(tdc_example)

# save the example in extdata
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

tarr.pop::save_poparray(tdc_example,
  filepath = "inst/extdata/tdc_example.h5",
  series_id = "tdc_example",
  revision_reason = "Documentation example: four counties, 2015–2018",
  geo = "County as area.name",
  overwrite = TRUE
)

# Verify that dimension labels and semantics survive the HDF5 round trip.
saved_example <- open_poparray("inst/extdata/tdc_example.h5")
stopifnot(
  identical(dimnames(saved_example), dimnames(tdc_example)),
  identical(dim_semantics(saved_example), dim_semantics(tdc_example))
)

rm(source_info, saved_example, tdc_example, tdc_all, counties)





