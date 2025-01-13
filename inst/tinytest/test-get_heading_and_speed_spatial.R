source('helper-heading_test_data.R')

## R = 1, forward

expect_equal(
  get_heading_and_speed_spatial(x, y, 1),
  expected_spatial[['R1, forward']]
)

## R = 1, backward
expect_equal(
  get_heading_and_speed_spatial(x, y, 1, forward=FALSE),
  expected_spatial[['R1, backward']]
)

# R = 1.5
expect_equal(
  get_heading_and_speed_spatial(x, y, 1.5),
  expected_spatial[['R1.5']]
)

# t_idxs is set

expect_equal(
  get_heading_and_speed_spatial(x, y, 1, t_idxs),
  expected_spatial[['t_idxs set']]
)