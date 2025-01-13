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
  indv_base <- rnorm(N-1, sd = sd_between_indv)
  within_indv_variation <- rnorm((N-1)*n_times, sd=sd_within_indv)
  group_coords <- matrix(
    c(
      rep(centroid_coords, N-1) +
        rep(indv_base, each = n_times) +
        x_t,
      rep(NA, n_times)
    ),
    N,
    n_times,
    byrow = TRUE
  )

}

# Configure base params/matrices ----

N <- 12
n_times <- 31

# Group follows same path as indvidiual does in heading test data
x_base <- c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y_base <- c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)

# For n-1 individuals, set their deviation 
x_ind_base  <- rnorm(N-1)
y_ind_base  <- rnorm(N-1)

x_t <- rnorm((N-1)*n_times, sd=.2)
y_t <- rnorm((N-1)*n_times, sd=.2)

xs <- scatter_group(x_base, N)
# make the average x_base
xs[N,] <- x_base*(N) - colSums(xs, na.rm=TRUE)


ys <- matrix(
  c(
    rep(y_base, N-1) +
      rep(y_ind_base, each = n_times) +
      y_t,
    rep(NA, n_times)
  ),
  N,
  n_times,
  byrow = TRUE
)
# make the average y_base
ys[N,] <- y_base*(N) - colSums(ys, na.rm=TRUE)

  # For introducing NAs
ind_count <- rep(1:N, length.out=n_times)

filter_mat <- matrix(
  do.call(c,lapply(ind_count, function(x) c(rep(0, x), rep(NA, N - x)))),
  N,
  n_times
)