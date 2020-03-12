library(testthat)
library(mpxtractor)
file = "../spectraMAx/data/20190701_growthrates_mg1363_pt2_smA_1stplate.txt"
test_check("mpxtractor")

test_that("test read_spectraMax_data function", {
      expect_equal(read_spectraMax_data(file = file), )
    }
)
