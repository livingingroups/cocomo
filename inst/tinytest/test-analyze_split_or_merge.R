sort_by_names <- function(x) x[sort(names(x))]
eps <- sqrt(.Machine$double.eps)
xs <- matrix(
  c(
    0, 1,
    0, 0
  ),
  2,2,byrow=TRUE
)
ys <- matrix(
  c(
    0, 0,
    0, 0
  ),
  2,2,byrow=TRUE
)
timestamps = as.POSIXct(1:2 )

expect_equal(
  sort_by_names(analyze_split_or_merge_event(
    events = identify_splits_and_merges(xs,ys,timestamps, .5, .5, names = c('Lily', 'Pan'))$events_detected,
    i=1,
    xs,
    ys,
    timestamps,
    time_window=1
  )),
  sort_by_names(list(
    start_time = NA,
    end_time = NA,
    before_time = NA,
    after_time = NA,
    turn_angle_A = NA,
    turn_angle_B = NA,
    split_angle = NA
  ))
)

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

# This one is mostly failing
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
  )[c('start_time', 'end_time')],
  list(
    start_time = fusion_time - (9-6),
    end_time = fusion_time + (6-1) +2 #,
    #before_time = fusion_time - time_window,
    #after_time = fusion_time + time_window,
    #turn_angle_A = 0,
    #turn_angle_B = pi,
    #split_angle = pi
  )
)

# todo
# - check time window smaller than together tinme
# - max time smaller than together time
# - multiple 3-2-1 sequences