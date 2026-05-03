# ------------------------------------------------------------------------------------------------------------------->
# Script:  construct_cube.r
# Description:
# 
# 
# 
# Steps:
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:
# 
# ------------------------------------------------------------------------------------------------------------------->
library(tidyverse)
#library(tarr)
library(DelayedArray)
library(HDF5Array)

parq_fn <- file.path(tarr::paths$population, "Estimates/Texas Demographic Center/asre") |> 
  list.files(pattern = r"(parquet$)", full.names = TRUE)  

parq <- arrow::read_parquet(file = parq_fn) |> 
  filter(age.char != "All",
         race.eth != "All",
         sex != "All") |> 
  select(-type)
glimpse(parq)

# .libPaths("C:/Users/rwjones/Documents/R/win-library/4.5")
# install.packages("BiocManager")
# BiocManager::install(c("DelayedArray", "S4Arrays", "IRanges", "S4Vectors"))
#remove.packages(c("DelayedArray", "S4Arrays", "BiocGenerics", "IRanges", "S4Vectors", "BiocManager", "BiocVersion"))
#BiocManager::install(c("DelayedArray", "S4Arrays", "BiocGenerics", "S4Vectors", "IRanges"), force=TRUE)

df2dim <- function(df, population_col){
  nms <- setdiff(names(df), population_col)
  ret <- list()
  ret$dimnames <- map(.x = nms, ~ df[[.x]] |> unique() |> as.character()) |> 
    set_names(nms)
  ret$dim      <- map_int(ret$dimnames, ~ length(.x)) |> unname()
  ret
}

create_poparray <- function(df, pop_col){
  dim_list <- df2dim(df, pop_col)
  arr <- array(data = as.integer(df[[pop_col]]), dim = dim_list$dim, dimnames = dim_list$dimnames) 
  da <- DelayedArray(seed = arr)
  # local_file <- tempfile(pattern = "cube", fileext = ",h5")
  # ha <- writeHDF5Array(arr, local_file, name = "temp_pop_cube")
  # ha
}

tmp <- create_poparray(parq, "population")


dt <- 1:100
arr2 <- array(dt, dim = c(2,2,25))
DelayedArray(arr2)

.Internal(inspect(arr))

class(arr)
is.array(arr)
is.matrix(arr)
str(arr)
packageVersion("DelayedArray")  
  
  tmp <- new_poparray(x = parq)
edit_r_profile()
