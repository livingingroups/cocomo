set.seed(200)

# Helper functions

#' Generates individual variaton around desired centroid
#'
#' This is analogous to random effects model where  you can
#' set between individual variation and within individual variation separately.
scatter_group <- function(
  centroid_coords,
  N,
  sd_between_indv = 1,
  sd_within_indv = .2
) {
  n_times <- length(centroid_coords)
  indv_base <- rnorm(N - 1, sd = sd_between_indv)
  within_indv_variation <- rnorm((N - 1) * n_times, sd = sd_within_indv)
  group_coords <- matrix(
    c(
      # rows 1:N-1
      rep(centroid_coords, N - 1) +
        rep(indv_base, each = n_times) +
        within_indv_variation,
      # row N, to be filled in the next line
      rep(NA, n_times)
    ),
    N,
    n_times,
    byrow = TRUE
  )

  # balance out so that the indv_base is exactly the average
  group_coords[N, ] <- centroid_coords * N - colSums(group_coords, na.rm = TRUE)

  group_coords
}

# Configure base params/matrices ----

N <- 12
n_times <- 31

# Group follows same path as indvidiual does in heading test data

# nolint start: indentation_linter
x_base <- c(
   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,
   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,
  10
)
y_base <- c(
   0,   1,   2,   1,   2,   3,   3,   3,   3,   2,
   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,
   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,
  10
)
# nolint end

# Build group movement matrices ----
# Add noise around the centroids
xs <- scatter_group(x_base, N)
ys <- scatter_group(y_base, N)

# Build matrix to simulate missing data -----

# This sets up a matrix that goes from being all 0s to all NAs
# at a consistent rate over time.
ind_count <- rep(1:N, length.out = n_times)
filter_mat <- matrix(
  do.call(c, lapply(ind_count, function(x) c(rep(0, x), rep(NA, N - x)))),
  N,
  n_times
)
