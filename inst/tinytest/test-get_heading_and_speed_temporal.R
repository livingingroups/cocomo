source('helper-heading_test_data.R')

# Incorrect argument type
expect_error(get_heading_and_speed_temporal(x, y, FALSE))

# default t_window
forward <- get_heading_and_speed_temporal(x, y)
expect_equal(h_1, forward$heads)
expect_equal(speed_1, forward$speeds)

backward <- get_heading_and_speed_temporal(x, y, forward = FALSE)
expect_equal(shift(h_1), backward$heads)
expect_equal(shift(speed_1), backward$speeds)


# t_window = 2 scenario
t_window_2 <- get_heading_and_speed_temporal(x, y, 2)
expect_equal(speed_2, t_window_2$speeds)
expect_equal(h_2, t_window_2$heads)
