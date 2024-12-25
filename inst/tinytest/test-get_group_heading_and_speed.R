source('helper-group_heading_test_data.R')

centroids <- get_group_centroid(xs, ys)

# approximately reveals base
expect_equal(round(centroids$x_centr), x_base, tolerance = .5)
expect_equal(round(centroids$y_centr), y_base, tolerance = .5)

# exact numbers haven't changed
expect_equal(centroids$x_centr, x_centr)
expect_equal(centroids$y_centr, y_centr)

ind_count <- rep(1:N, length.out=n_times)

filter_mat <- matrix(
  do.call(c,lapply(ind_count, function(x) c(rep(0, x), rep(NA, N - x)))),
  N,
  n_times
)

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

