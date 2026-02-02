# Script to retrive dimnames for population arrays in the data folder

library(tidyverse)

# Get dim_labels for diffferent population files
fls <- list.files("data", pattern = "rda$", full.names = TRUE)
fls

load(fls[1])

get_dims <- function(file){
  e <- new.env()
  load(file, envir = e)
  objs <- ls(e)
  dims <- dimnames(e[[objs[1]]])
  rm(e)
  return(dims)

}

dims_char <- map(fls, get_dims)

names(dims_char) <- basename(fls) |> tools::file_path_sans_ext()

tmp <- compact(dims_char)
dput(tmp)
dput(names(tmp))
fls
rm(census.estimates)
load(fls[4])
county_fips |> head() |> names()
fls[4]
