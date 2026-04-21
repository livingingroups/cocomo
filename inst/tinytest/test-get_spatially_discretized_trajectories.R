x <- c(0, .9, 1.1, 2.2, 2.5)
y <- rep(0, length(x))

spat_t <- c(1, 3, 4)
spat_x <- c(0, 1.1, 2.2)
spat_y <- rep(0, length(spat_x))

if(FALSE) expect_equivalent(
  get_spatially_discretized_trajectories(rbind(x), rbind(y), 1, verbose = F),
  list(
    spat_ts = rbind(spat_t),
    spat_xs = rbind(spat_x),
    spat_ys = rbind(spat_y),
    spat_breaks = rbind(1),
    R = 1
  ),
  info = 'basic functionality: one individual'
)

expect_equivalent(
  get_spatially_discretized_trajectories(rbind(x, x), rbind(y,y), 1, verbose = F),
  list(
    spat_ts = rbind(spat_t, spat_t),
    spat_xs = rbind(spat_x, spat_x),
    spat_ys = rbind(spat_y, spat_y),
    spat_breaks = rbind(1,1),
    R = 1
  ),
  info = 'two identical individuals'
)

spat_t <- c(1, 2, 4)
spat_x <- x[spat_t]
spat_y <- y[spat_t]

expect_equivalent(
  get_spatially_discretized_trajectories(rbind(x, x), rbind(y, y), .5, verbose = F),
  list(
    spat_ts = rbind(spat_t, spat_t),
    spat_xs = rbind(spat_x, spat_x),
    spat_ys = rbind(spat_y, spat_y),
    spat_breaks = rbind(1,1),
    R = .5
  ),
  info = 'two identical individuals, R=.5'
)

x <- rbind(
  c(0, 1.1, 2.2, 3.3),
  c(0, .5, 1.1, 1.4)
)
y <- matrix(0, nrow(x), ncol(x))

spat_t <- rbind(
  c(1, 2, 3, 4),
  c(1, 3, NA, NA)
)
spat_x <- rbind(
  c(0, 1.1, 2.2, 3.3),
  c(0, 1.1, NA, NA)
)
spat_y <- matrix(0, nrow(spat_x), ncol(spat_x))
spat_y[is.na(spat_x)] <- NA

expect_equivalent(
  get_spatially_discretized_trajectories( x, y, 1, verbose = F),
  list(
    spat_ts = spat_t,
    spat_xs = spat_x,
    spat_ys = spat_y,
    spat_breaks = rbind(1,1),
    R = 1
  ),
  info = 'two individuals, different final length'
)



x <- rbind(
  c(0, 1.1, 2.2, 3.3, 3.4, 4.5, 5.6),
  c(0, .5, 1.1, 1.4, 0, 1.1, 2.2)
)
y <- matrix(0, nrow(x), ncol(x))

spat_t <- rbind(
  c(1, 2, 3, 4, 5, 6, 7),
  c(1, 3, 5, 6, 7, NA, NA)
)
spat_x <- rbind(
  c(0, 1.1, 2.2, 3.3, 3.4, 4.5, 5.6),
  c(0, 1.1, 0, 1.1, 2.3, NA, NA)
)
spat_y <- matrix(0, nrow(spat_x), ncol(spat_x))
spat_y[is.na(spat_x)] <- NA

if(FALSE) expect_equivalent(
  get_spatially_discretized_trajectories(x, y, 1, breaks=5, verbose = F),
  list(
    spat_ts = spat_t,
    spat_xs = spat_x,
    spat_ys = spat_y,
    spat_breaks = rbind(5,3),
    R = 1
  ),
  info = 'with breaks'
)