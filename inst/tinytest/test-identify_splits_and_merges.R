# This script has data and expectations for 3 fission-fusion scenarios

# Names used for all scenarios
ind_names <- c('Lily', 'Pan', 'Tamu', 'Winnie')

# Scenario 1: N=3, only one moves ----

## Setup baseline ----
N <- 3
n_times <- 12

base_x <- matrix(
  # individual 1 stays at x=1
  c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    # individual 2 stays at x=3
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  ),
  # individual 3 will be added later and varies by scenario
  N - 1, n_times,
  byrow = TRUE
)

# no movement on y axis
ys <- matrix(rep(0, N * n_times), N, n_times)

# Each of these represents one timestamp of a together matrix
# They can be combined to create an expected together matrix
together_expectations <- list(
  all_separate = c(
    NA, 0, 0,
    0, NA, 0,
    0, 0, NA
  ),
  `3_with_1` = c(
    NA, 0, 1,
    0, NA, 0,
    1, 0, NA
  ),
  `3_with_2` = c(
    NA, 0, 0,
    0, NA, 1,
    0, 1, NA
  ),
  `3_with_both` = c(
    NA, 0, 1,
    0, NA, 1,
    1, 1, NA
  ),
  all_togetether = c(
    NA, 1, 1,
    1, NA, 1,
    1, 1, NA
  )
)

## 1a Mover starts alone, then moves from 1 to 2 ----
expect_equal(
  identify_splits_and_merges(
    rbind(
      base_x,
      c(-100, -100, .3, .5, .5, .5, .5, .7, .5, .5, .5, .5)
    ),
    ys, as.POSIXct(1:n_times), .4, .6,
    names = ind_names[1:N]
    # we are just testing the together matrix in this scenario
  )$together,
  array(
    c(
      rep(together_expectations[['all_separate']], 2),
      together_expectations[['3_with_1']],
      rep(together_expectations[['3_with_both']], 4),
      rep(together_expectations[['3_with_2']], 5)
    ),
    dim = c(N, N, n_times)
  ) == 1
)

## 1b Mover starts alone, then hovers in between the radii ----
expect_equal(
  identify_splits_and_merges(
    rbind(
      base_x,
      c(-2, -2, .5, .5, .5, .5, .7, .5, .5, .3, .5, .5)
    ),
    ys, as.POSIXct(1:n_times), .6, .9,
    names = ind_names[1:N]
  ),
  list(
    events_detected = data.frame(
      event_idx = 1,
      tidx = c(2),
      event_type = c('fusion'),
      n_groups_before = c(3),
      n_groups_after = c(1),
      big_group_idxs = I(list(1:3)),
      big_group = I(list(ind_names[1:3])),
      group_A_idxs = I(list(1)),
      group_B_idxs = I(list(2)),
      group_C_idxs = I(list(3)),
      group_A = I(list(c('Lily'))),
      group_B = I(list(c('Pan'))),
      group_A = I(list(c('Tamu'))),
      n_A = c(1),
      n_A = c(1),
      n_B = c(1),
      n_big_group = c(3)
    ),
    all_events_info = list(
      list(
        t = 2,
        groups_before = list(list(1), list(2), list(3)),
        groups_after = list(list(1:3)),
        event_type = 'fusion',
        n_groups_before = 3,
        n_groups_after = 1
      )
    ),
    groups_list = list(
      list(1, 2, 3),
      list(1, 2, 3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3),
      list(1:3)
    ),
    groups = matrix(
      c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      ),
      N,
      n_times,
      byrow = TRUE
    ),
    together = array(
      c(
        rep(together_expectations[['all_separate']], 2),
        rep(together_expectations[['3_with_both']], 10)
      ),
      dim = c(N, N, n_times)
    ) == 1,
    R_inner = .6,
    R_outer = .9
  ),
  check.attributes = FALSE
)


# Scenario 2: fission-shuffle-fusion ----

## Define inputs ----

# Using named locations to make it easier to
# read who is together at each timepoint
# without plotting.

# This function converts those named locations to coordiantes
loc_to_coordinates <- function(
  # x or y
  dim,
  # this is a Nxn_times matrix
  # showing the named location that each individual
  # is at at each timepoint
  loc_mat = matrix(c(
    'a', 'a', 'a', 'b', 'b', 'b', 'd', 'd', 'd', 'd',
    'a', 'a', 'a', 'b', 'b', 'c', 'd', 'd', 'd', 'd',
    'a', 'a', 'a', 'c', 'c', 'b', 'd', 'd', 'd', 'd',
    'a', 'a', 'a', 'c', 'c', 'c', 'd', 'd', 'd', 'd'
  ), 4, 10, byrow = TRUE),
  # x,y coodinates of each named location
  loc_lookup = list(
    a = c(x = 1, y = 1),
    b = c(x = 2, y = 1),
    c = c(x = 1, y = 2),
    d = c(x = 2, y = 2)
  )
) {
  matrix(
    sapply(
      loc_mat,
      function(l) loc_lookup[[l]][[checkmate::assert_subset(dim, c('x', 'y'))]]
    ),
    nrow(loc_mat),
    ncol(loc_mat)
  )
}

xs_basic <- loc_to_coordinates('x')
ys_basic <- loc_to_coordinates('y')

n_times <- ncol(xs_basic)
N <- nrow(xs_basic)

all_together <- c(
  NA, 1, 1, 1,
  1, NA, 1, 1,
  1, 1, NA, 1,
  1, 1, 1, NA
)

## Define expected results ----

expected_basic <- list(
  events_detected = data.frame(
    event_idx = 1:3,
    tidx = c(3, 5, 6),
    event_type = c('fission', 'shuffle', 'fusion'),
    n_groups_before = c(1, 2, 2),
    n_groups_after = c(2, 2, 1),
    big_group_idxs = I(list(1:4, NA, 1:4)),
    big_group = I(list(ind_names, NA, ind_names)),
    group_A_idxs = I(list(1:2, NA, c(1, 3))),
    group_B_idxs = I(list(3:4, NA, c(2, 4))),
    group_A = I(list(c('Lily', 'Pan'), NA, c('Lily', 'Tamu'))),
    group_B = I(list(c('Tamu', 'Winnie'), NA, c('Pan', 'Winnie'))),
    n_A = c(2, NA, 2),
    n_B = c(2, NA, 2),
    n_big_group = c(4, NA, 4)
  ),
  all_events_info = list(
    list(
      t = 3,
      groups_before = list(list(1:4)), # If simplified: list(A = ind_names),
      groups_after = list(list(1:2), list(3:4)), # If simplified: list( A = c('Lily', 'Pan'), B = c('Tamu', 'Winnie')),
      event_type = 'fission',
      n_groups_before = 1,
      n_groups_after = 2
    ),
    list(
      t = 5,
      groups_before = list(list(1:2), list(3:4)),
      groups_after = list(list(c(1, 3)), list(c(2, 4))),
      event_type = 'shuffle',
      n_groups_before = 2,
      n_groups_after = 2
    ),
    list(
      t = 6,
      groups_before = list(list(c(1, 3)), list(c(2, 4))),
      groups_after = list(list(1:4)), # If simplified: list(A = ind_names),
      event_type = 'fusion',
      n_groups_before = 2,
      n_groups_after = 1
    )
  ),
  groups_list = list(
    list(1:4),
    list(1:4),
    list(1:4),
    list(1:2, 3:4),
    list(1:2, 3:4),
    list(c(1, 3), c(2, 4)),
    list(1:4),
    list(1:4),
    list(1:4),
    list(1:4)
  ),
  groups = matrix(c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
    1, 1, 1, 2, 2, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 2, 2, 1, 1, 1, 1
  ), N, n_times, byrow = TRUE),
  together = array(
    c(
      rep(all_together, 3),
      rep(c(
        NA, 1, 0, 0,
        1, NA, 0, 0,
        0, 0, NA, 1,
        0, 0, 1, NA
      ), 2),
      c(
        NA, 0, 1, 0,
        0, NA, 0, 1,
        1, 0, NA, 0,
        0, 1, 0, NA
      ),
      rep(all_together, 4)
    ),
    dim = c(N, N, n_times)
  ) == 1,
  R_inner = .5,
  R_outer = .5
)

## Run and test against expectations ----

actual_basic <- identify_splits_and_merges(xs_basic, ys_basic, as.POSIXct(1:n_times), .5, .5, names = ind_names)

expect_equal(
  expected_basic,
  actual_basic,
  check.attributes = FALSE
)

# Scenario 3: Multiple events happening on same timestamp ----

# Here we clone the scenario by cloning each individual
# the clones make the same movements as the original
# but +c(3,3) away from the originals. Clones don't
# fuse or shuffle with originals, nonly with each other.

# Create a new lookup table with same pattern but different locations
xs_with_clone <- rbind(xs_basic, xs_basic + 3)
ys_with_clone <- rbind(ys_basic, ys_basic + 3)

ind_names_with_clone <- c(ind_names, paste0(ind_names, '_clone'))
n_events_basic <- length(expected_basic$all_events_info)

# nolint start: object_length_linter
expected_events_detected_with_clone <- expected_basic$events_detected[rep(1:n_events_basic, each = 2), ]
expected_events_detected_with_clone$event_idx <- 1:(2 * n_events_basic)
new_idx <- seq(1, (2 * n_events_basic), by = 2) + 1
# adjust individual index columns
for (column in c('big_group_idxs', 'group_A_idxs', 'group_B_idxs')) {
  expected_events_detected_with_clone[new_idx, column] <- data.frame(placeholder = I(lapply(
    expected_events_detected_with_clone[new_idx, column],
    function(x) ifelse(is.na(x), NA, x + 4)
  )))
}

# adjust individual name columns
for (column in c('big_group', 'group_A', 'group_B')) {
  expected_events_detected_with_clone[new_idx, column] <- data.frame(placeholder = I(lapply(
    expected_events_detected_with_clone[new_idx, column],
    function(x) ifelse(is.na(x), NA, paste0(x, '_clone'))
  )))
}
# nolint end

all_together_clone <- c(
  NA, 1, 1, 1, 0, 0, 0, 0,
  1, NA, 1, 1, 0, 0, 0, 0,
  1, 1, NA, 1, 0, 0, 0, 0,
  1, 1, 1, NA, 0, 0, 0, 0,
  0, 0, 0, 0, NA, 1, 1, 1,
  0, 0, 0, 0, 1, NA, 1, 1,
  0, 0, 0, 0, 1, 1, NA, 1,
  0, 0, 0, 0, 1, 1, 1, NA
)

expected_with_clone <- list(
  events_detected = expected_events_detected_with_clone,
  all_events_info = do.call(c, lapply(expected_basic$all_events_info, function(event_info) {
    event_info_clone <- event_info
    event_info_clone$groups_before <- lapply(
      event_info$groups_before,
      function(x) list(x[[1]] + 4)
    )
    event_info_clone$groups_after <- lapply(
      event_info$groups_after,
      function(x) list(x[[1]] + 4)
    )
    list(event_info, event_info_clone)
  })),
  groups_list = list(
    list(1:4, 5:8),
    list(1:4, 5:8),
    list(1:4, 5:8),
    list(1:2, 3:4, 5:6, 7:8),
    list(1:2, 3:4, 5:6, 7:8),
    list(c(1, 3), c(2, 4), c(5, 7), c(6, 8)),
    list(1:4, 5:8),
    list(1:4, 5:8),
    list(1:4, 5:8),
    list(1:4, 5:8)
  ),
  groups = matrix(c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
    1, 1, 1, 2, 2, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 3, 3, 3, 2, 2, 2, 2,
    2, 2, 2, 3, 3, 4, 2, 2, 2, 2,
    2, 2, 2, 4, 4, 3, 2, 2, 2, 2,
    2, 2, 2, 4, 4, 4, 2, 2, 2, 2
  ), N * 2, n_times, byrow = TRUE),
  together = array(
    c(
      rep(all_together_clone, 3),
      rep(c(
        NA, 1, 0, 0, 0, 0, 0, 0,
        1, NA, 0, 0, 0, 0, 0, 0,
        0, 0, NA, 1, 0, 0, 0, 0,
        0, 0, 1, NA, 0, 0, 0, 0,
        0, 0, 0, 0, NA, 1, 0, 0,
        0, 0, 0, 0, 1, NA, 0, 0,
        0, 0, 0, 0, 0, 0, NA, 1,
        0, 0, 0, 0, 0, 0, 1, NA
      ), 2),
      c(
        NA, 0, 1, 0, 0, 0, 0, 0,
        0, NA, 0, 1, 0, 0, 0, 0,
        1, 0, NA, 0, 0, 0, 0, 0,
        0, 1, 0, NA, 0, 0, 0, 0,
        0, 0, 0, 0, NA, 0, 1, 0,
        0, 0, 0, 0, 0, NA, 0, 1,
        0, 0, 0, 0, 1, 0, NA, 0,
        0, 0, 0, 0, 0, 1, 0, NA
      ),
      rep(all_together_clone, 4)
    ),
    dim = c(N * 2, N * 2, n_times)
  ) == 1,
  R_inner = .5,
  R_outer = .5
)


expect_equal(
  identify_splits_and_merges(xs_with_clone, ys_with_clone, as.POSIXct(1:n_times), .5, .5, names = ind_names_with_clone),
  expected_with_clone,
  check.attributes = FALSE
)

#The following tests did not pass in the previous version (prior to 2025-02-04). Now all should pass.
# Fails with unhelpful error message if no splits/merges
expect_silent(
  identify_splits_and_merges(
    matrix(
      c(1,1,1,1),
      2,
      2
    ),
    matrix(
      c(1,1,1,1),
      2,
      2
    ),
    as.POSIXct(1:2),
    .5,
    .5
  )$together
)
# This also fails
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
        1,  1,  1,  1,
        0,  1,  1,  0
      ),
      2, 4, byrow=TRUE
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4
    ),
    as.POSIXct(1:4),
    .4,
    .6
  )$together[1,2,],
  c(FALSE, TRUE,TRUE,FALSE)
)

# "Togetherness" not perojected forward correctly
# when indviduals start together.
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
        1,  1,  1,
        .7, .5, .3
      ),
      2, 3, byrow=TRUE
    ),
    matrix(
      c(1,1,1,1,1,1),
      2,
      3
    ),
    as.POSIXct(1:3),
    .4,
    .6
  )$together[1,2,],
  c(TRUE,TRUE,FALSE)
)

# Initial "apart" trampled
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
        1,   1,  1, 1,
        0, 1, 0,  0
      ),
      2, 4, byrow=TRUE
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4
    ),
    as.POSIXct(1:4),
    .5,
    .5
  )$together[1,2,]
  ,
  c(FALSE, TRUE, FALSE, FALSE)
)

# Final "apart" trampled
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
        1,   1,  1,  1,
        0,   0,  1,  0
      ),
      2, 4, byrow=TRUE
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4
    ),
    as.POSIXct(1:4),
    .5,
    .5
  )$together[1,2,]
  ,
  c(FALSE, FALSE, TRUE, FALSE)
)

