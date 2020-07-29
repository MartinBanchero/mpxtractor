library(mpxtractor)
context("Data wrangling: multiscango function")
test_that("Check correct dataframe, attributes, dimensions and attributes class", {

 file <- system.file("extdata", "test_multiscango_data_1.txt", package = "mpxtractor")

 df_multiscango <- mpxtractor::read_multiscango_data(
  file,
  time_interval = "2 min",
  input_type = "table"
)

  #outdata_1 <- system.file("data", "df_multiscango_outdata_1.rda", package = "mpxtractor")
  #load(outdata_1)
  #mpxtractor::df_multiscango_outdata
  #Check both dataframes
  expect_identical(df_multiscango, df_multiscango_outdata)
  #check attirbutes
  expect_identical(attributes(df_multiscango), attributes(df_multiscango_outdata))
  #check dimension
  expect_identical(dim(df_multiscango), dim(df_multiscango_outdata))
  #check class
  expect_identical(sapply(df_multiscango, class), sapply(df_multiscango_outdata, class))
})
