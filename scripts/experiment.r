library(tidyverse)
library(profvis)
library(tictoc)




# Minimal example data
x <- array(
  1:12,
  dim = c(3, 4),
  dimnames = list(
    year = 2020:2022,
    area.name = c("A", "B", "C", "D")
  )
)

pa <- new_poparray(x)
class(pa)
pa

# Subset
#pa2 <- pa[1:2, , drop = FALSE]
pa2 <- pa[1:2, , drop = FALSE]
pa2
pa2 <- pa[1:2, 1:4, drop = FALSE]


# Sanity checks
inherits(pa2, "poparray")                # TRUE
inherits(pa2$handle, "DelayedArray")     # TRUE
!is.array(pa2$handle)                    # TRUE

rm(pa, pa2, x)

#zip <- open_tarr_pop(population$census.bureau$zcta)

cen <- open_tarr_pop(population$census.bureau$census)
cen
years(cen)
ages(cen) |> rage::as.age_group() |> sort()
setdiff(areas(cen), c("Dallas", "Zavala", "Texas"))
prod(dim(cen)  * 4) |> format(big.mark = ",")

#debug(`[.tarr_pop`)
cen_2020 <- cen["2020", c("Tarrant", "Dallas"), c("Female", "Male"), TRUE, TRUE, TRUE]
cen_2020_a <- cen["2020", c("Tarrant", "Dallas"), c("Female", "Male"),,,]
cen_2020_aa <- cen["2020", c("Tarrant", "Dallas"), c("Female", "Male")]
cen_2020_b <- cen[area.name = c("Tarrant", "Dallas"), year = "2020"]
#cen_2020_c <- cen |> filter(year ==2020, area.name == c("Tarrant", "Dallas"))

cen_2020
cen_2020_a
cen_2020_aa
cen_2020_b
cen_2020_c
sum(cen_2020) == sum(cen_2020_a) 
sum(cen_2020) == sum(cen_2020_aa) 
sum(cen_2020) == sum(cen_2020_b)
sum(cen_2020_c) == sum(cen_2020_b)

rm(list = ls(pattern = "cen_2020_"))
rm(cen, cen_2020)

# test projections ---------------------------------------------------------
cen_est <- open_tarr_pop(population$census.bureau$estimates) 
cen_est  
cen_est <- cen_est  |> 
  filter(year  %in% 2010:2024,
         area.name  %in% c("Bell"), 
         race  %in% races(cen_est, remove = regex("combination$"))
         )

years(cen_est)
areas(cen_est)
as.data.frame(cen_est)
as_tibble(cen_est)
cen_est |> class()
lobstr::obj_size(cen_est)

base_years <- cen_est |> 
  filter(year %in% 2011:2021)
base_years
years(base_years)

test_years <- cen_est |> 
  filter(year  %in%  2022:2024)
test_sums <- map_int(2022:2024, ~ test_years |> filter(year == .x) |> sum()) |> set_names((2022:2024))
test_sums

# profile projection functions -----------------------------------------------

#tic.clearlog()
#debug(project_cube)
system.time(
res_arima <-  project(tp = base_years, h = 3, level = 0.95, method = "ARIMA")
)
res_ets <-  project(tp = base_years, h = 3, level = 0.95, method = "ETS")
res_cagr <-  project(tp = base_years, h = 3, level = 0.95, method = "CAGR")
plot(res_arima)

# log <- tic.log(format = FALSE) 
# totals <- tapply(
#   X = vapply(log, \(x) x$toc - x$tic, numeric(1)),
#   INDEX = vapply(log, \(x) x$msg, character(1)),
#   FUN = sum
# )
# 
#totals

proj <- res_arima$projected |>
#  collapse_dim(dim = "sex", groups = list(all = sexes(res_arima$projected)))|> 
  collapse_dim(dim = "age.char", groups = list(all = ages(res_arima$projected))) |> 
  collapse_dim(dim = "race", groups = list(all = races(res_arima$projected))) |> 
  collapse_dim(dim = "ethnicity", groups = list(all = ethnicities(res_arima$projected))) 

as.data.frame(proj) |> View()

class(res_arima$projected)
res_list <- map(list(ARIMA = "ARIMA", CAGR = "CAGR", ETS = "ETS"), 
                ~ project(tp = base_years, h = 3, level = 0.95, method = .x, guard = TRUE))
                
                

# local function to sum a cube by each year it contains
yearly_sums <- function(tp){
  yrs <- years(tp)
  ret <- map_int( yrs, ~ tp |> filter(year == .x) |> sum() |> as.integer()) |> 
    set_names(yrs)
  ret
}

use_git_config(user.name = "Russ Jones", user.email = "RussJones007@gmail.com")
git_vaccinate()


est_list  <- map(res_list, ~ .x[["projected"]] |> yearly_sums())
(diff_list <- map(est_list, ~ .x - test_sums))
pct_list  <- map(est_list, ~ ((.x - test_sums)/test_sums * 100) |> format(digits = 2) |> paste0("%"))
pct_list

est_df <- map(res_list, as.data.frame) |> 
  bind_rows()
test_df <- as.data.frame(test_years) |> 
  rename(census_estimate = population)

all_est <- left_join(est_df, test_df, by = c("year","sex",  "area.name", "age.char", "race", "ethnicity")) |> 
  group_by(model, year, age.char) |> 
  summarise(estimate = sum(projected) |> as.integer(),
            census_est = sum(census_estimate)
  ) |> 
  ungroup() |> 
  mutate(diff = estimate - census_est,
         pct  = (diff/census_est) |> round(digits = 2))

ggplot(all_est, aes(x = age.char)) +
  geom_line(aes(y = census_est, group = model)) +
  geom_line(aes(y = estimate, group = model, color = model)) +
  #scale_
  #geom_bar(stat = "identity")+
  facet_wrap(. ~  year, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90) )
  
ggplot(all_est, aes(x = age.char, y = pct, fill = model)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent) +
  facet_grid(year ~  Model) +
  #ggthemes::theme_tufte() +
  theme_bw()+
  labs(x = "Age Group", y = "Percent Difference", 
       title = "Estimates Percent Difference of Projection vs Census Bureau",
       subtitle = "By Model and Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        strip.placement = "outside",
        strip.text.y.right = element_text(angle = 0),
        panel.grid.major.y = element_line(color = "grey70", linetype = "dotted"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major   = element_blank(),
        axis.ticks.y.left = element_blank())


# Re-write projection engine calls for multiple cores:
for (i in seq_len(nrow(grid))) {
  fixed_k_list <- as.list(grid[i, , drop = FALSE])
  names(fixed_k_list) <- names(grid) # dim positions as character
  
  y <- extract_series(tp, year_k = year_k, fixed_k_list = fixed_k_list)
  
  res <- run_projection_engine(method = method, y = y, years = base_years_chr, h = h, level = level, ...)
  
  w$write_year_slice("projected", fixed_k_list = fixed_k_list, values = res$projected)
  w$write_year_slice("lower",     fixed_k_list = fixed_k_list, values = res$lower)
  w$write_year_slice("upper",     fixed_k_list = fixed_k_list, values = res$upper)
  
  if (is.null(base_years_used)) base_years_used <- as.character(res$base_years)
}

map(seq_len(nrow(grid)), ~ 
      res <- as.list(grid[.x, , drop = FALSE]) |> 
      set_names(names(grid)) |> 
      run_projection_engine(method = method, y = _, years = base_years_chr, h = h, level = level, ...)
    
    w$write_year_slice("projected", fixed_k_list = fixed_k_list, values = res$projected)
    w$write_year_slice("lower",     fixed_k_list = fixed_k_list, values = res$lower)
    w$write_year_slice("upper",     fixed_k_list = fixed_k_list, values = res$upper)
    
      )

project_slice <- function(ndx){
  res <- as.list(grid[ndx, , drop = FALSE]) |> 
    set_names(names(grid)) |> 
    run_projection_engine(method = method, y = _, years = base_years_chr, h = h, level = level, ...)
  
  w$write_year_slice("projected", fixed_k_list = fixed_k_list, values = res$projected)
  w$write_year_slice("lower",     fixed_k_list = fixed_k_list, values = res$lower)
  w$write_year_slice("upper",     fixed_k_list = fixed_k_list, values = res$upper)
  
}




# percent differnce between censsus estimates and modeled projections

res


dfs <- map(list(res, test_years), as.data.frame) 
all_est2dfs <- left_join(dfs[[1]], dfs[[2]], by = c("year","sex",  "area.name", "age.char", "race", "ethnicity")) 
View()


test_sums - sum(cen_2020 |> filter(area.name == "Tarrant"))
cen_2020



last_est <- cen_est |> 
  filter(year == 2024)