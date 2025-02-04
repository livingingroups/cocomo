# skip
exit_file()

# Analyse split and merge
# before time is not tidx - time windwo

padding <- 25
n_times <- 20 + 3 * padding
N <- 2

xs <- matrix(
  c(
    rep(-1, padding),
    c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1),
    rep(0, padding),
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
    rep(1, padding),
    rep(0, n_times)
  ),
  N,n_times,byrow=TRUE
)
ys <- matrix(
  rep(0, N*n_times),
  N,n_times,byrow=TRUE
)
timestamps <- as.POSIXct(1:n_times)

identify_splits_and_merges(xs,ys,timestamps, .4, .6, names = c('Lily', 'Pan'))
fusion_time <- 4+padding
fission_time <- 17+padding

time_window <- 20

expect_equal(
  analyze_split_or_merge_event(
    events = identify_splits_and_merges(xs,ys,timestamps, .4, .6, names = c('Lily', 'Pan'))$events_detected,
    i=1,
    xs,
    ys,
    timestamps,
    thresh_h = .9+eps,
    thresh_l = .1-eps,
    max_time = 20,
    time_window = time_window
  )[c('before_time', 'after_time')],
  list(
    before_time = fusion_time - time_window,
    after_time = fusion_time + time_window,
    #turn_angle_A = 0,
    #turn_angle_B = pi,
    #split_angle = pi
  )
)
