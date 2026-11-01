# -------------------------------------------------------------------------------------->
# Script: imports.r
# Description:
#   Centralized roxygen import declarations generate package namespace dependencies; this file does not
#   execute population transformations.
#
#   Declares package and function imports within the usethis namespace markers.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: 2026-02-01 (first recorded Git commit)
# -------------------------------------------------------------------------------------->

## usethis namespace: start
#' @import assertthat
#' @import checkmate
#' @importFrom cli cli_abort
#' @importFrom rhdf5 h5createFile h5createDataset h5write
#' @importFrom HDF5Array HDF5Array writeHDF5Array
#' @import data.table
#' @importFrom dplyr filter summarise arrange mutate group_by ungroup transmute select left_join if_else across all_of count where
#' @importFrom abind asub
#' @importFrom ggplot2 autoplot aes geom_bar scale_y_continuous scale_fill_manual coord_flip labs theme_minimal
#' @importFrom ggplot2 facet_grid facet_wrap theme element_blank geom_text geom_line geom_point geom_ribbon
#' @importFrom tibble as_tibble deframe enframe
#' @importFrom forecast auto.arima ets forecast
#' @importFrom stats ts Box.test residuals approx
#' @importFrom scales label_percent label_date label_date_short label_comma label_ordinal label_wrap percent
#' @importFrom tidyr complete expand pivot_wider pivot_longer replace_na nest unnest
#' @importFrom stringr str_detect str_extract str_glue regex str_remove str_replace str_sub str_trim str_flatten str_flatten_comma
#' @importFrom purrr map map_lgl imap walk compose partial keep discard negate reduce attr_getter map_int map2
#' @importFrom rlang set_names syms !!! !! %||%
# @importFrom magrittr set_attr extract extract2 set_attributes set_class
## usethis namespace: end
NULL
