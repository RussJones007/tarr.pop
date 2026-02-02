# doc.R
# build the package, used to run in the background

usethis::use_version(which = "dev")
devtools::document()
devtools::build(binary = TRUE)
