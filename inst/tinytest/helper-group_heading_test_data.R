set.seed(200)
N <- 12
n_times <- 31
x_base <- c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y_base <- c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)
x_ind_base  <- rnorm(N-1)
y_ind_base  <- rnorm(N-1)
x_t <- rnorm((N-1)*n_times, sd=.2)
y_t <- rnorm((N-1)*n_times, sd=.2)

xs <- matrix(
  c(
    rep(x_base, N-1) +
      rep(x_ind_base, each = n_times) +
      x_t,
    rep(NA, n_times)
  ),
  N,
  n_times,
  byrow = TRUE
)
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