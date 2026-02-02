# Testing pop_retriev_2.r


test_that(desc = "Testing the county_population and group_ages functions",
          code = {
            arr <- county_population(pop = census, year = "2020",
                                    age = ages(census,  remove = "All"),
                                    sex = sexes(census, remove = "All"),
                                    )
            expect_true(is.table(arr))
            expect_length(ages(arr), 103)
            expect_length(sexes(arr), 2)
            # test age grouping
            arr <- group_ages(arr, age_groups = rage::age.groups$IMM)
            expect_in(ages(arr), rage::as.age_group(rage::age.groups$IMM) |> as.character())
            expect_equal(sum(arr), 2110640)

            # check that converting to a data frame works
            df <- as.data.frame(arr)
            expect_length(df, 7)  # number of columns
            expect_equal( sum( map_lgl(df, is.factor)), 6)  # six factor variables present
            expect_length( levels(df$age.char), 10)
            expect_equal( sum(df$population), 2110640)   # defaults should result in population for Tarrant

            arr <- county_population(pop = population$census.bureau$estimates(),
                                     year = "2024"
                                    )
            expect_true(is.table(arr))
            expect_length(ages(arr), 1)
            expect_length(sexes(arr), 1)

            df <- as.data.frame(arr)
            expect_length(df, 7)  # number of columns
            expect_equal( sum( map_lgl(df, is.factor)), 6)  # six factor variables present
            expect_equal( sum(df$population), 2230708)

            arr <- county_population(pop  = population$census.bureau$estimates(),
                                    year  = "2024",
                                    age   = ages(census.estimates, remove = "All"),
                                    sex   = sexes(census.estimates, remove = "All"),
                                    race = races(census.estimates,
                                                  remove = regex("(combination|All|Two)")),
                                    ethnicity = ethnicities(census.estimates, "All")
            )

            expect_length(ages(arr), 18)
            arr <- group_ages(arr, age_groups = rage::age.groups$Yr.10)
            expect_length(ages(arr), 8)
            expect_in(sexes(arr), c("Female", "Male"))
            expect_equal(sum(arr), 2165931)
            expect_length(races(arr), 5)
            expect_in( c("Black", "Asian", "White"), races(arr))
            expect_length(ethnicities(arr), 2)
            expect_in("Hispanic", ethnicities(arr))


            df <- as.data.frame(arr)
            expect_length(df, 7)  # number of columns
            expect_equal( sum( map_lgl(df, is.factor)), 6)  # six factor variables present
            expect_equal( sum(df$population), 2165931)
            expect_length(levels(df$age.char), 8)
            expect_in(c("0-9", "10-19", "20-29", "70+"), levels(df$age.char))
          })


