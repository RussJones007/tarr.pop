# pop_array.r
# This is a test bed script for epxploring storing the population data in an array and retrieval of data as needed
# Code for the matrices was moved t control.def.r in the data-raw folder

?df_2_array
?tarr.population
tmp <- population$seer$single_age()
areas(tmp) <- "Tarrant"
years(tmp) <- "2023"
as.data.frame(tmp) |> View()
?seer
?population
tmp <- population$seer$single_age()

?array_2_df
?retrieve_county_population
?county_population
??population
?pop_list
tp <- retrieve_county_population(.pop_df = population$census, .year = 2010)
tp <- retrieve_county_population(.pop_df = population$census,
                                 .year = c(2010, 2020),
                                 .age  = c(as.character(0:99), "100-104"),
                                 .sex = c("Female", "Male"),
                                 .ethnicity = c("Hispanic", "Non-Hispanic")
)
population$census$age.iv |>obj_size()
tp |> obj_size()

tp_tbl <- county_population(pop_list$census,
                            year = c("2010", "2020"),
                            county = "Tarrant",
                            sex = c("Female", "Male"),
                            age  =  c(as.character(0:99), "100-104"),
                            race = "All",
                            ethnicity = c("Hispanic", "Non-Hispanic"),
                            drop = FALSE)
?aggregate
age_grps <- dimnames(tp_tbl)[["age.char"]] |>
  as.age_group() |>
  tarr::age_cat_iv(age = _, age_group = tarr::age.groups$Yr.5) |>
  as.character() |>
  factor()
age_grps
dimnames(tp_tbl)[["age.char"]] |> summary()
age_grps |> summary()

dimnames(tp_tbl)
dimnames(tp_tbl)[["age.char"]] <- age_grps
dimnames(tp_tbl)[["age.char"]]
(ndx <- which(dimnames(tp_tbl) |> names() == "age.char"))

age_table <- apply(tp_tbl, -ndx, sum)
age_table |> attributes()
age_table |> class()
age_table
age_table_df <- as.data.frame(age_table)

breaks <- seq(0, 100, by = 5)
age_values <- sample(1:100, size = 150, replace = TRUE)
age_labels <- paste(head(breaks, -1), tail(breaks, -1) - 1, sep = "-")
age_labels

# Map age values to bins
age_bins <- cut(age_values, breaks = breaks, labels = age_labels, include.lowest = TRUE)
age_bins |> class()

attributes(tp_tbl)
get_data_col(tp_tbl)
sum(tp_tbl["year" = "2020", , , , , ])

class(tp_tbl)
#undebug(as.data.frame.tarr_pop)
tp_df <- as.data.frame(tp_tbl)
#debug(as_tibble.tarr_pop)
tp_tiblle <- as_tibble(tp_tbl)

(tp_df$year == 2020) |> table()
tp_df[tp_df$year == 2020, "population"] |> sum()

get_data_col(pop_list$census)

pop_list$census |> attributes()

pop_list$census |> attributes()
tarr.population::population |> lobstr::obj_size()
tarr.population::pop_list |> lobstr::obj_size()
dimnames(pop_list$census)

methods(generic.function = "as_tibble")
# check speed
library(microbenchmark)
library(units)

timing <- microbenchmark(times = 100, unit = "ms",
                         tp_mx = pop_list$census[c("2010", "2020"), "Tarrant",
                                                 c("Female", "Male"),
                                                 c(as.character(0:99), "100-104"),
                                                 "All",
                                                 c("Hispanic", "Non-Hispanic"), drop = FALSE] |>
                           magrittr::set_attr(which = "data_col", value = get_data_col(pop_list$census)) |>
                           new_tarr_pop() |>
                           as.data.frame(),

               tp_tbl = county_population(pop = pop_list$census, year = c(2010, 2020),
                                           ethnicity = c("Hispanic", "Non-Hispanic"),
                                           age = c(as.character(0:99), "100-104"),
                                           sex = c("Female", "Male"),
                                           county = "Tarrant"
               ) |>
                 as.data.frame(),

               tp = retrieve_county_population(.pop_df = population$census,
                                               .year = c(2010, 2020),
                                               .age  = c(as.character(0:99), "100-104"),
                                               .sex = c("Female", "Male"),
                                               .ethnicity = c("Hispanic", "Non-Hispanic"))

)

timing
summary(timing, unit = "ms")
boxplot(timing)
ggplot2::autoplot(timing)


library(tarr.population)



