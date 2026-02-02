# test-PopRetrieve.r
# Especially important for the retrive_county_population function

test_that(
  desc = "Testing retrieve_county_population using census data",
  code = {
    df <- retrieve_county_population(.pop_df = census,
                                     .year = 2020,
                                     .age = ages(census, remove = "All"))
    expect_length( levels(df$age.char), 103)  # levels with age group set at 10 years
    expect_equal( df$area.name |> unique() |> as.character(), "Tarrant")  # default for county should be Tarrant
    expect_equal( nrow(df), 103)             # one record for each age group
    expect_equal( sum(df$population), 2110640)  #  population sum is correct

    #undebug(retrieve_county_population)
    df <- retrieve_county_population(census.estimates, .year = 2020, .age = ages(census.estimates, remove = "All"))
    expect_equal( levels(df$age.char) |> length(),
                  ages(census.estimates, remove = "All") |> length())
    df <- retrieve_county_population(.pop_df = population$census.bureau$census(),
                                     .year = 2020,
                                     #.age.groups = age.groups$Yr.10,
                                     .ethnicity = c("Hispanic", "Non-Hispanic"),
                                     .sex = c("Female", "Male")
    )
    expect_equal(with( df, sum(population[sex == "Female" & ethnicity == "Hispanic"])), 310433)
    expect_length(levels(df$sex), 2)
    expect_length(levels(df$age.char), 1)
    expect_length(levels(df$ethnicity), 2)
  })




