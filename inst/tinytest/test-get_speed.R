source('helper-heading_test_data.R')

expect_equal(get_speed(x, y, 1), speed_1)
expect_equal(get_speed(x, y, 1, 2), speed_2)
expect_equal(get_speed(x, y, 1, 1, FALSE), shift(speed_1))
expect_equal(get_speed(x, y, 1, 2, FALSE), shift(speed_2, 2))
