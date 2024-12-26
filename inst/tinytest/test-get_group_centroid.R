source('helper-group_heading_test_data.R')

centroids <- get_group_centroid(xs, ys)

# reveals base
expect_equal(centroids, list(x_centr = x_base, y_centr = y_base))

expect_equal(
  list(
    x_centr = ifelse(ind_count < 6, NA, x_base),
    y_centr = ifelse(ind_count < 6, NA, y_base)
  ),
  get_group_centroid(xs+filter_mat, ys, 6),
  tolerance = .5
)
expect_equal(
  list(
    x_centr = ifelse(ind_count < 8, NA, x_base),
    y_centr = ifelse(ind_count < 8, NA, y_base)
  ),
  get_group_centroid(xs, ys+filter_mat, 8),
  tolerance = .5
)

