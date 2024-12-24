source('helper-heading_test_data.R')

h_s_R1 <- get_heading_and_speed_spatial(x, y, 1)

expect_equal(h_1, h_s_R1$heads)
expect_equal(h_1, h_s_R1$heads)

expect_equal(speed_1, h_s_R1$speeds)

speed_1 - h_s_R1$speeds

expect_equal(h_1, h_s_R1$heads)