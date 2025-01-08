source('helper-heading_test_data.R')

## R = 1, forward
h_s_R1f <- get_heading_and_speed_spatial(x, y, 1)

expect_equal(
  get_heading_and_speed_spatial(x, y, 1),
  list(
    heads = h_R1f,
    speeds = speed_R1f,
    dts = dt_R1f
  ),
  info = "Checks dt, heading, speed with R=1, forward = TRUE"
)

## R = 1, backward
expect_equal(
  get_heading_and_speed_spatial(x, y, 1, forward=FALSE),
  list(
    heads = h_R1b,
    speeds = speed_R1b,
    dts = dt_R1b
  ),
  info = "Checks dt, heading, speed with R=1, forward = FALSE"
)

# R = 1.5
expect_equal(
  get_heading_and_speed_spatial(x, y, 1.5),
  list(
    heads = h_R1_5f,
    speeds = speed_R1_5f,
    dts = dt_R1_5f
  ),
  info = "Checks dt, heading, speed with R=1.5, forward = TRUE"
)

# t_idxs is set

expect_equal(
  get_heading_and_speed_spatial(x, y, 1, t_idxs),
  list(
    heads = h_t_idxs_set,
    speeds = speed_t_idxs_set,
    dts = dt_t_idxs_set
  ),
  info = "Checks dt, heading, speed with R=1, t_idxs set"
)