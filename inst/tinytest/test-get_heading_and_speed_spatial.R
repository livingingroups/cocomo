source('helper-heading_test_data.R')


all_NA <- rep(NA_real_, len)

## R = 1, forward
h_s_R1f <- get_heading_and_speed_spatial(x, y, 1)

# check that i calculates dt correctly
expect_equal(h_s_R1f$dts, dt_R1f)


# check heads given dt
expect_equal(all_NA[is.na(dt_R1f)], h_s_R1f$heads[is.na(dt_R1f)])
expect_equal(h_1[dt_R1f == 1], h_s_R1f$heads[dt_R1f == 1])
expect_equal(h_2[dt_R1f == 2], h_s_R1f$heads[dt_R1f == 2])

# check speeds given dt
expect_equal(all_NA[is.na(dt_R1f)], h_s_R1f$speeds[is.na(dt_R1f)])
expect_equal(speed_1[dt_R1f == 1], h_s_R1f$speeds[dt_R1f == 1])
expect_equal(speed_2[dt_R1f == 2], h_s_R1f$speeds[dt_R1f == 2])

h_s_R1b <- get_heading_and_speed_spatial(x, y, 1, forward=FALSE)

## R = 1, backward
# check that i calculates dt correctly
expect_equal(h_s_R1b$dts, dt_R1b)

# check heads given dt
expect_equal(all_NA[is.na(dt_R1b)], h_s_R1b$heads[is.na(dt_R1b)])
expect_equal(shift(h_1)[dt_R1b == 1], h_s_R1b$heads[dt_R1b == 1])
expect_equal(shift(h_2, 2)[dt_R1b == 2], h_s_R1b$heads[dt_R1b == 2])

# check speeds given dt
expect_equal(all_NA[is.na(dt_R1b)], h_s_R1b$speeds[is.na(dt_R1b)])
expect_equal(shift(speed_1)[dt_R1b == 1], h_s_R1b$speeds[dt_R1b == 1])
expect_equal(shift(speed_2, 2)[dt_R1b == 2], h_s_R1b$speeds[dt_R1b == 2])
