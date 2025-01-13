source('helper-group_heading_test_data.R')
source('helper-heading_test_data.R')

# Dealing with missing data, temporal ----

expect_equal(
  # xs + filter mat "infects" x with the NAs form filter mat
  get_group_heading_and_speed(xs+filter_mat, ys, 'temporal', t_window = 2, min_inds_tracked = 6),
  list(
    # ind_count tells how many individuals are *not* NA at each time point in filter mat
    # For outcome to not be NA, both tidx and tidx minus 2 need to have enough data
    heads = ifelse((ind_count < 6)|(shift(ind_count, -2) < 6), NA, h_2),
    speeds = ifelse((ind_count < 6)|(shift(ind_count, -2) < 6), NA, speed_2),
    dts = rep(2,len) # ifelse(ind_count < 6, NA, 2)
  ),
  # High tolerance because missing date can make centroids no
  # longer exactly where we expect them to be, but this test is
  # not focusing on accuracy, just NA handling.
  tolerance = .5
)

# same as above except min_inds_tracked = 8 instead of 6
expect_equal(
  get_group_heading_and_speed(xs+filter_mat, ys, 'temporal', t_window = 2, min_inds_tracked = 8),
  list(
    heads = ifelse((ind_count < 8)|(shift(ind_count, -2) < 8), NA, h_2),
    speeds = ifelse((ind_count < 8)|(shift(ind_count, -2) < 8), NA, speed_2),
    dts = rep(2,len) # ifelse(ind_count < 6, NA, 2)
  ),
  tolerance = .5
)

# Spatial ----

## R = 1, forward ----
expect_equal(
  # eps is needed because of floats introduced in centroid calc
  get_group_heading_and_speed(xs, ys, 'spatial', spatial_R = 1-eps),
  expected_spatial[['R1, forward']]
)

## R = 1, backward -----
expect_equal(
  get_group_heading_and_speed(xs, ys, 'spatial', spatial_R = 1-eps, forward = FALSE),
  expected_spatial[['R1, backward']]
)
