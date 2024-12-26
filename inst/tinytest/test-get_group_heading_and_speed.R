source('helper-group_heading_test_data.R')
source('helper-heading_test_data.R')

all_NA <- rep(NA_real_, len)

# Dealing with NAs
expect_equal(
  list(
    heads = ifelse((ind_count < 6)|(shift(ind_count, -2) < 6), NA, h_2),
    speeds = ifelse((ind_count < 6)|(shift(ind_count, -2) < 6), NA, speed_2),
    dts = rep(2,len) # ifelse(ind_count < 6, NA, 2)
  ),
  get_group_heading_and_speed(xs+filter_mat, ys, 'temporal', t_window = 2, min_inds_tracked = 6),
  tolerance = .5
)
expect_equal(
  list(
    heads = ifelse((ind_count < 8)|(shift(ind_count, -2) < 8), NA, h_2),
    speeds = ifelse((ind_count < 8)|(shift(ind_count, -2) < 8), NA, speed_2),
    dts = rep(2,len) # ifelse(ind_count < 6, NA, 2)
  ),
  get_group_heading_and_speed(xs+filter_mat, ys, 'temporal', t_window = 2, min_inds_tracked = 8),
  tolerance = .5
)

### Spatial

## R = 1, forward, eps is needed because recovering centroid results in a float that's almost x
# no exactly x
g_h_s_R1f <- get_group_heading_and_speed(xs, ys, 'spatial', spatial_R = 1-eps)

# check that i calculates dt correctly
expect_equal(g_h_s_R1f$dts, dt_R1f)

g_h_s_R1f$dts[c(11, 13)]
dt_R1f[c(11, 13)]


# check heads given dt
expect_equal(all_NA[is.na(dt_R1f)], g_h_s_R1f$heads[is.na(dt_R1f)])
expect_equal(h_1[dt_R1f == 1], g_h_s_R1f$heads[dt_R1f == 1])
expect_equal(h_2[dt_R1f == 2], g_h_s_R1f$heads[dt_R1f == 2])

# check speeds given dt
expect_equal(all_NA[is.na(dt_R1f)], g_h_s_R1f$speeds[is.na(dt_R1f)])
expect_equal(speed_1[dt_R1f == 1], g_h_s_R1f$speeds[dt_R1f == 1])
expect_equal(speed_2[dt_R1f == 2], g_h_s_R1f$speeds[dt_R1f == 2])

g_h_s_R1b <- get_group_heading_and_speed(xs, ys, 'spatial', spatial_R = 1-eps, forward = FALSE)

## R = 1, backward
# check that i calculates dt correctly
expect_equal(g_h_s_R1b$dts, dt_R1b)

# check heads given dt
expect_equal(all_NA[is.na(dt_R1b)], g_h_s_R1b$heads[is.na(dt_R1b)])
expect_equal(shift(h_1)[dt_R1b == 1], g_h_s_R1b$heads[dt_R1b == 1])
expect_equal(shift(h_2, 2)[dt_R1b == 2], g_h_s_R1b$heads[dt_R1b == 2])

# check speeds given dt
expect_equal(all_NA[is.na(dt_R1b)], g_h_s_R1b$speeds[is.na(dt_R1b)])
expect_equal(shift(speed_1)[dt_R1b == 1], g_h_s_R1b$speeds[dt_R1b == 1])
expect_equal(shift(speed_2, 2)[dt_R1b == 2], g_h_s_R1b$speeds[dt_R1b == 2])