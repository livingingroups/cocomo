source('helper-group_heading_test_data.R')

centroids <- get_group_centroid(xs, ys)

# reveals base
expect_equal(centroids, list(x_centr = x_base, y_centr = y_base))

expect_equal(
  get_group_centroid(xs+filter_mat, ys, 6),
  list(
    x_centr = ifelse(ind_count < 6, NA, x_base),
    y_centr = ifelse(ind_count < 6, NA, y_base)
  ),
  # High tolerance because missing date can make centroids no
  # longer exactly where we expect them to be, but this test is
  # not focusing on accuracy, just NA handling.
  tolerance = .5
)
expect_equal(
  get_group_centroid(xs, ys+filter_mat, 8),
  list(
    x_centr = ifelse(ind_count < 8, NA, x_base),
    y_centr = ifelse(ind_count < 8, NA, y_base)
  ),
  tolerance = .5
)

