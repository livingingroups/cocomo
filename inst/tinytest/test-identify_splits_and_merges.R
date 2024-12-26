# TODO: add shuffle, test boundaries, test more complex entangle/disentangle
# multiple fisions/fusions at single timemstamp

n_times = 9
N = 4

loc_lookup <- list(
  a = c(x = 1, y = 1),
  b = c(x = 2, y = 1),
  c = c(x = 1, y = 2),
  d = c(x = 2, y = 2)
)

loc = c(
  'a', 'a', 'a', 'b', 'b', 'd', 'd', 'd', 'd',
  'a', 'a', 'a', 'b', 'b', 'd', 'd', 'd', 'd',
  'a', 'a', 'a', 'c', 'c', 'd', 'd', 'd', 'd',
  'a', 'a', 'a', 'c', 'c', 'd', 'd', 'd', 'd'
)

xs <- matrix(
  sapply(
    loc,
    function(l) loc_lookup[[l]][['x']]
  ),
  N,
  n_times,
  byrow = TRUE
)

ind_names <- c('Lily', 'Pan', 'Tamu', 'Winnie')

ys <- matrix(
  sapply(
    loc,
    function(l) loc_lookup[[l]][['y']]
  ),
  N,
  n_times,
  byrow = TRUE
)

s_and_m <- identify_splits_and_merges(xs,ys,as.POSIXct(1:n_times),.5,.5, names = ind_names)

expect_equal(
  data.frame(
    event_idx = 1:2,
    tidx = c(3,5),
    event_type = c('fission', 'fusion'),
    n_groups_before = c(1,2),
    n_groups_after = c(2,1),
    big_group_idxs = I(list(1:4, 1:4)),
    big_group = I(list(ind_names, ind_names)),
    group_A_idxs = I(list(1:2, 1:2)),
    group_B_idxs = I(list(3:4, 3:4)),
    group_A = I(list(c('Lily', 'Pan'), c('Lily', 'Pan'))),
    group_B = I(list(c('Tamu', 'Winnie'), c('Tamu', 'Winnie'))),
    n_A = c(2,2),
    n_B = c(2,2),
    n_big_group = c(4,4)
  ),
  s_and_m$events_detected,
  check.attributes = FALSE
)

tinytest::expect_equal(
  list(
    list(
      t = 3,
      groups_before = list(list(1:4)), # list(A = ind_names),
      groups_after = list(list(1:2), list(3:4)), #list( A = c('Lily', 'Pan'), B = c('Tamu', 'Winnie')),
      event_type = 'fission',
      n_groups_before = 1,
      n_groups_after = 2
    ),
    list(
      t = 5,
      groups_before = list(list(1:2), list(3:4)), #list( A = c('Lily', 'Pan'), B = c('Tamu', 'Winnie')),
      groups_after = list(list(1:4)), # list(A = ind_names),
      event_type = 'fusion',
      n_groups_before = 2,
      n_groups_after = 1
    )
  ),
  s_and_m$all_events_info,
  check.attributes=FALSE
)