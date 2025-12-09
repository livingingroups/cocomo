# Get simplified turn and speed influence

**\[stable\]**

Get the simplified turn and speed influence (movement and positional)
between all pairs of individuals `i` and `j`, or, if `centroid = T` for
all individuals `i` on the group centroid excluding `i`.

## Usage

``` r
get_turn_and_speed_influence_simplified(
  xs,
  ys,
  heading_type,
  centroid = F,
  breaks = NULL,
  spatial_R = NULL,
  t_window = NULL,
  min_percentile = 0.5,
  seconds_per_time_step = 1,
  symmetrize_lr = T,
  max_distance = NULL,
  min_past_speed = NULL,
  min_left_speed = NULL,
  min_right_speed = NULL,
  min_left_pos = NULL,
  min_right_pos = NULL,
  min_faster_speed_diff = NULL,
  min_slower_speed_diff = NULL,
  min_front_pos = NULL,
  min_back_pos = NULL,
  verbose = T,
  output_details = T
)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

- heading_type:

  character string specifying heading type - `'spatial'` or `'temporal'`

- centroid:

  whether to use the group centroid (if `centroid = T`) instead of
  computing influence for each dyad (if `centroid = F`)

- breaks:

  vector of indexes to breaks in the data (e.g. breaks between days)

- spatial_R:

  radius to use for spatial headings (if `heading_type = 'spatial'`)

- t_window:

  temporal window to use for temporal headings (if
  `heading_type = 'temporal'`)

- min_percentile:

  minimum percentile to use for left / right relative speed or distance

- seconds_per_time_step:

  number of seconds corresponding to each time step

- symmetrize_lr:

  whether to symmetrize the thresholds for right and left movement +
  position (default `T`)

- max_distance:

  optional maximum distance between a pair of individuals, or between an
  individual and the centroid, to include a given time point in the
  influence calculations

- min_past_speed:

  optional minimum previous speed of either the group centroid (if
  `centroid = T`) or the 'follower' (if `centroid = F`) to include data
  in the computation - can be used to subset to only times when the
  individual or group are already moving

- min_left_speed:

  minimum speed when i moving to the left to include data (positive
  value)

- min_right_speed:

  minimum speed when i moving to the right to include data (positive
  value)

- min_left_pos:

  minimum distance of i to the left of j to include data (positive
  value)

- min_right_pos:

  minimum distance of i to the right to j include data (positive value)

- min_faster_speed_diff:

  minimum speed difference between i and j when i is going faster to
  include data (positive value)

- min_slower_speed_diff:

  minimum speed difference between i and j when i is going slower to
  include data (positive value)

- min_front_pos:

  minimum position toward the front when i is in front of j to include
  data (positive value)

- min_back_pos:

  minimum position toward the back when i is in back of j to include
  data (positive value)

- verbose:

  whether the print progress (if `T`)

- output_details:

  whether to output computed matrices and arrays used in influence
  computations (if `T`)

## Value

Returns a list containing matrices for the 4 different types of
influence, as well as other computed metrics (if `output_details = T`).
See "Additional information about returned objects" section below for a
list of outputs.

## Details

The simplified movement turn influence of individual `i` on individual
`j` (`turn_influence_movement[i,j]`) is defined as the probability that
`j` turns right in the future given that `i` moved right relative to
`j`'s past heading at a speed above a threshold value `min_right_speed`,
that `j` turns left in the future given that `i` moved left relative to
`j`'s past heading at a speed above a threshold value `min_left_speed`.

The simplified positional turn influence of individual `i` on individual
`j` (`turn_influence_position[i,j]`) is defined as the probability that
`j` turns right in the future given that `i` was located at least
`min_right_dist` meters right of individual `j` relative to `j`'s
current position and heading, or that `j` turns left in the future given
that `i` was located at least `min_left_dist` meters left of individual
`j` relative to `j`'s current position and heading.

The simplified movement speed influence of individual `i` on individual
`j` (`speed_influence_movement[i,j]`) is defined as the probability that
`j` speeds up in the future along the vector defined by `j`'s heading in
the past, given that `i` was moving at least a speed
`min_faster_speed_diff` faster than `j` along `j`'s past heading
direction, or slows down along the vector defined by `j`'s heading in
the past given that `j` was moving at least a speed
`min_slower_speed_diff` slower than `i` along the same vector in the
past.

The simplified positional speed influence of individual `i` on
individual `j` (`speed_influence_position[i,j]`) is defined as the
probability that `j` speeds up in the future along the vector defined by
`j`'s heading in the past given that `i` was in front of `j` by at least
`min_front_pos` meters relative to `j`'s current position and past
heading, or that `j` slows down in the future along the vector defined
by `j`'s heading in the past given that `i` was behind `j` by at least
`min_back_pos` relative to `j`'s current position and past heading.

Future headings and speeds are computed either using a fixed temporal
window `t_window` forward in time (if `heading_type = 'temporal'`) or
based on spatial discretization with a radius `spatial_R` (if
`heading_type = 'spatial'`), i.e. defining the heading based on the when
the focal individual has moved a distance `spatial_R` from its current
location. Similarly, past headings and speeds are computed by looking
backward in time either at a fixed temporal window (`t_window`) or based
on a fixed displacement distance (`spatial_R`).

The parameters specifying minimum speeds and distances left-right and
front-back are either set manually (not recommended) or, if
`min_percentile` is specified, based on percentiles of the relevant
distributions of these variables across all dyads (or across all
individuals relative to the group centroid, if `centroid = T`). The
variable `min_percentile` overrides the manually set minimum speeds /
distances. The minimum speeds (or distances) are computed as follows.
First, split the distribution of relative left-right speeds (for
example) across all dyads into those greater than and those less than 0.
Take the absolute value of all speeds to define a positive speed for
both directions. Next, compute the quantile `min_percentile` for each
distribution (left and right) separately. Finally, set `min_left_speed`
and `min_right_speed` at the computed values. If `symmetrize_lr = T`
(recommended), take instead the mean value of the quantiles for both
distributions and use it for both thresholds (this will result in using
the same relative speed when going right or left as a threshold -
otherwise these could be different if `symmetrize_lr = F`) The same
procedure is done for left-right positions, front-back relative speeds,
and front-back positions, using the same value of `min_percentile`. In
the case of speeds, no symmetrization is done because speed differences
and front-back distances are not expected to be symmetrical.

If `breaks` is set, headings that go across breaks in the data as
specified by this vector are not included in the computations.

If `max_distance` is set, only compute the scores based on times when
the distance between a given pair of individuals (or between) an
individual and the group centroid) is less than `max_distance`. By
default `max_distance = NULL` and this filtering will not be done.

If `min_past_speed` is set, only compute the scores based on times when
the follower individual (or the centroid, if `centroid = T`) was
initially moving at a minimum past speed. By default,
`min_past_speed = NULL` and this filtering will not be done.

## Additional information about returned objects

The output list conatins the following objects:

`params` list of parameters used in the calculation (see input variables
for descriptions)

`turn_influence_movement`: `i x j` matrix representing movement turn
influence of individual `i` on individual `j`

`turn_influence_position`: `i x j` matrix representing positional turn
influence of individual `i` on individual `j`

`speed_influence_movement`: `i x j` matrix representing movement speed
influence of individual `i` on individual `j`

`speed_influence_position`: `i x j` matrix representing positions speed
influence of individual `i` on individual `j`

If `centroid = T`, everything is instead computed at the centroid level
(with the centroid as follower, replacing individual `j`). In this case,
the output matrices will still be the same shape as those for the dyadic
case, but they will have `NA`s for all columns except `j=1`
(representing the influence of each individual `i` on the centroid
excluding that individual). This parallel structure is for compatibility
between dyadic and centroid-based analyses.

If `output_details = T`, some additional parameters and the following
computed matrices / arrays will also be outputted:

`lr_speed`: `i x j x t` array of the past left-right speed of individual
`i` relative to the past heading of individual `j` at time `t`

`lr_pos`: `i x j x t` array of the past left-right position of
individual `i` relative to the current position and past heading of
individual `j` at time `t`

`fb_speed`: `i x j x t` array of the past front-back speed difference
between individual `i` and individual `j` along the vector of the past
heading of individual `j` at time `t`

`fb_pos`: `i x j x t` array of the past front-back position of
individual `i` relative to the current position and past heading of
individual `j` at time `t`

`turn_angle`: `j x t` matrix of the turning angle of individual `j`
between the past and future at time `t`

`speed_up`: `j x t` matrix of the difference in speed of individual `j`
between future and past at time `t`, project along `j`'s own past
heading

The output matrices `turn_angle[j,t]` and `speed_up[j,t]` then represent
the centroid excluding individual `j`, rather than the data for
individual `j` as in the dyadic case. Similarly, the output arrays with
dimensions `i x j x t` will have `NA`s for all values of `j > 1`, and
the `j = 1` values represent the speeds / positions of individuals
relative to the centroid (excluding `i`)

## Author

Ariana Strandburg-Peshkin (primary author)

Jack Winans (code reviewer, 15 August 2024)
