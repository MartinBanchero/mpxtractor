context("Data wrangling: spectramax function")
test_that("Check correct dataframe, attributes, dimensions and attributes class", {

  file <- system.file("extdata", "test_spectramax_data_1.txt", package = "mpxtractor")
  df_spectramax <- mpxtractor::read_spectramax_data(file)

  #outdata_1 <- system.file("data", "df_spectramax_outdata_1.rda", package = "mpxtractor")
  #load(outdata_1)
  data(df_spectramax_outdata_1)
  # Check both dataframes
  expect_identical(df_spectramax, df_spectramax_outdata_1)
  # check attirbutes
  expect_identical(attributes(df_spectramax), attributes(df_spectramax_outdata_1))
  # check dimension
  expect_identical(dim(df_spectramax), dim(df_spectramax_outdata_1))
  # check class
  expect_identical(sapply(df_spectramax, class), sapply(df_spectramax_outdata_1, class))
})
