context("Data wrangling: fluorstar function")
test_that("Check correct dataframe, attributes, dimensions and attributes class", {

  file <- system.file("extdata", "test_fluorstar_fluorescence_data.txt", package = "mpxtractor")
  df_fluorstar_fluorescence <- mpxtractor::read_fluorstar_data(file)

  #outdata_1 <- system.file("data", "df_fluorstar_fluorescence_outdata.rda", package = "mpxtractor")
  #load(outdata_1)
  #rm(df_fluorstar_fluorescence_outdata)
  #mpxtractor::df_fluorstar_fluorescence_outdata
  # Check both dataframes
  expect_identical(df_fluorstar_fluorescence, df_fluorstar_fluorescence_outdata)
  # check attirbutes
  expect_identical(attributes(df_fluorstar_fluorescence), attributes(df_fluorstar_fluorescence_outdata))
  # check dimension
  expect_identical(dim(df_fluorstar_fluorescence), dim(df_fluorstar_fluorescence_outdata))
  # check class
  expect_identical(sapply(df_fluorstar_fluorescence, class), sapply(df_fluorstar_fluorescence_outdata, class))
})
