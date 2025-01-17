source('helper-heading_test_data.R')

## Test Helper functions ----
expect_equal(shift(1:10, 2), c(9:10, 1:8))
expect_equal(shift(1:10, -2), c(3:10, 1:2))