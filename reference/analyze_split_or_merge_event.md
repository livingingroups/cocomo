# Analyze split or merge event

**\[experimental\]**

Analyze (and, if `make_plot=T` make a visualization of) a fission-fusion
event.

## Usage

``` r
analyze_split_or_merge_event(
  events,
  i,
  xs,
  ys,
  timestamps,
  max_time = 600,
  thresh_h = 50,
  thresh_l = 15,
  depart_or_arrive_radius = 15,
  time_window = 300,
  seconds_per_time_step = 1,
  breaks = NULL,
  break_by_day = F,
  make_plot = T
)
```

## Arguments

- events:

  a data frame of fission-fusion events, output from
  `identify_splits_and_merges`

- i:

  index of the row to use in the events table

- xs:

  `n_inds x n_times` matrix of x coordinates

- ys:

  `n_inds x n_times` matrix of y coordinates

- timestamps:

  vector of timestamps of length `n_times` in datetime format

- max_time:

  maximum time steps forward and back to look for the start and end of
  the event (units are timesteps, not seconds)

- thresh_h:

  upper threshold for determining when subgroups are "apart" (default
  50)

- thresh_l:

  lower threshold for determining when subgroups are "together" (default
  15)

- depart_or_arrive_radius:

  threshold for determining what an individual has departed (for
  fissions) or arrived (for fusions) at the group, used in computing
  departure/arrival times and headings

- time_window:

  time steps to move backward or forward in time to identify the before
  and after times (units are timesteps, not seconds)

- seconds_per_time_step:

  seconds per time step (default 1)

- breaks:

  indexes to breaks in the data (default NULL treats data as a
  contiguous sequence). If specified, overrides `break_by_day`

- break_by_day:

  whether to break up data by date (T or F)

- make_plot:

  whether to plot the event (if `T`) or not (if `F`)

## Value

Returns a list (`out`) of information extracted about the event, as well
as a plot (if `make_plot = T`).

The list contains:

`start_time`: start time index

`end_time`: end time index

`before_time`: before time index

`after_time`: after time index

`disps`: matrix of displacements of the different subgroups (rows)
during the different time intervals (columns). Rows and columns are
named for easy access.

`speeds`: same format as `disps` matrix, but with speeds, in m / s

`split_angle`: split angle in degrees, description above

`turn_angle_A`: turning angle for subgroup A in degrees, description
above

`turn_angle_B`: turn angle for subgroup B in degrees, description above

`depart_or_arrive_times`: vector of departure (fission) or arrival
(fusion) times for each individual in big_group_idxs (from original
events table)

`depart_or_arrive_headings`: vector of departure (fission) or arrival
(fusion) headings for each individual in big_group_idxs (from original
events table)

`depart_or_arrive_time_diff`: difference between the mean departure
(fission) or arrival (fusion) time across the two subgroups

`depart_or_arrive_heading_diff`: difference between the mean vector
departure (fission) or arrival (fusion) heading across the two subgroups

The plot shows (top) dyadic distance over time with lines showing the
identified times and (bottom) a visualization of trajectories of the two
subgroups.

## Details

This function takes in information about a fission or fusion event as
well as tracking data to identify relevant time points in the fission or
fusion event and compute relevant metrics about the event. First, the
`start_time` and `end_time` of the event are determined based on a
double threshold method, then a `before_time` and `after_time` are
identified. This identifies 3 phases of the event: before
(`before_time:start_time`), during (`start_time:end_time`) and after
(`end_time:after_time`). Finally the displacements and speeds of the
subgroups during these phases and various relevant angles are
calculated. More details are given below. Note that this function can
only consider events involving 2 subgroups.

## Additional details

*How are the start and end time identified?*

As a start, we look at a window of time around an identified event (can
be manually or automatically identified). The size of the window is
determined by the parameter '`max_time`', and we go from
`(tidx - max_time):(tidx + max_time)` where `tidx` is the identified
event time index.

Within the window, we compute the distance between the centroids of the
two subgroups that are splitting or merging at each time point. We then
use a double threshold method to determine the start and end point of
the fission or fusion event within that time window. To do so, we first
categorize all time points as being above the higher threshold
`thresh_h` (2), between the two thresholds (1), or below the lower
threshold `thresh_l` (0). We then identify contiguous periods of time
where the dyadic distance went from 2-111111(any number of ones)-0 (i.e.
high-mid-low) for a fusion or 0-1111...-2 (i.e. low-mid-high) for a
fission. If there are multiple possible time periods detected within the
window, we choose the one where the start time is closest to `tidx`.
(See also subtlety 1 below).

*How are the before and after times identified?*

The before time is defined as the time point `tidx - time_window`
(default `time_window = 300` time steps), unless the two groups are not
sufficiently together (for a fission) or apart (for a fusion) at that
time. If the latter, the `before_time` is identified as the point just
before the two subgroups cross a threshold `thresh_m` midway between the
upper and lower thresholds used to define the start and end times (i.e.
usually at `(thresh_l + thresh_h) / 2`, though if the upper or lower
thresholds get modified due to *subtlety 1* below, this will also modify
`thresh_m` accordingly).

The logic here is that, for a fusion we are looking for what the full
group (combination of the two eventual subgroups) was doing before they
began to split. However, we do not want this point to fall during a
prior fusion event, so we require the centroids of the two subgroups to
be less than `(thresh_l + thresh_h) / 2` distance apart. The after time
is defined in an analogous way, but using the time period after the
event. It is usually set to `tidx + time_window`, unless the subgroups
come back too close together (for a fission) or go too far apart (for a
fusion). Again, we use the time just prior to crossing the midway point
between the two thresholds as the `after_time`, if this threshold is
crossed before `time_window` seconds has elapsed.

*How are the displacements defined?*

We compute the displacement of the centroid of each subgroup (group A
and group B) as well as the displacement of the centroid of the combined
group (group AB) during each of the time windows: before
(`before_time:start_time`), during (`start_time:end_time`) and after
(`end_time:after_time`).

*How are the angles defined?*

We define 3 relevant angles relevant to a fission event (might define
more later for merge events): `split_angle`: this is the angle at which
the two groups split. It is defined as the angle traced out by the
points `p_A(end_time)`, `p_AB(start_time)`, and `p_B(end_time)` where
`p_A(t)` is the position of subgroup A's centroid at time `t` and
likewise for subgroup B and the combined group. `turn_angle_A`: the
turning angle of subgroup A. This is defined as the angle formed by the
points `p_AB(before_time)`, `p_AB(start_time)`, and `p_A(end_time)`
turn_angle_B: likewise for subgroup B Note that all angles use the point
`p_AB(start_time)` as their central point

*How are departure time difference and departure angular difference
calculated?*

To calculate the departure time difference and angular difference, we
start by computing the centroid of the full group at the event *start
time* - call this the *group start position*. We then determine, for
each individual in each subgroup, a *departure time* that is defined as
the first time after *start time* when that individual crossed a
threshold distance (`depart_or_arrive_radius`) from the *start
position*. We then compute the *departure heading* for each individual
as the vector pointing from the *group start position* to the position
of the individual at its *departure time*.

Once we have computed *departure times* and *departure headings* for
each individual, we compute an aggregated metric of the disagreement in
times and headings for the entire event. For departure time, we
calculate the *time disagreement* as the absolute difference between the
mean `departure_time` of each subgroup. Similarly, for departure
heading, we calculate the *directional disagreement* as the angle
between the (vector) mean heading of each subgroup. The angle between
vectors is defined between 0 and pi radians. The time difference is
defined in seconds.

We can do the equivalent calculations for fusion events. Here, we define
the *group end position* as the centroid of the combined group at the
`end_time`, and use this position as the reference point for all the
other calculations. Looking backward in time, we find the *arrival time*
for each individual, defined as the latest time before the *end time*
where that individual remained outside a threshold distance of
`depart_or_arrive_radius`. The headings and time differences are then
computed as above, and the differences between times and angles as well.

To avoid having different column names for all of the above variables in
the code, we create the columns: `group_start_or_end_position` which is
defined as the *group start position* for a fission and the *group end
position* for a fusion, `depart_or_arrive_times` which is a list of
*departure times* (in the case of fissions) or *arrival times* (in the
case of fusions), `depart_or_arrive_headings` which is a lsit of
*departure headings* (in the case of fissions) or *arrival headings* (in
the case of fusions), `depart_or_arrive_time_diff` which is a single
number representing the *time disagreement* of departure (for fissions)
or arrival (for fusions) times, `depart_or_arrive_heading_diff` which is
a single number representing the *directional disagreement* of departure
(for fissions) or arrival (for fusions) headings.

*SUBTLETIES*:

*Subtlety 1*: Sometimes, due to the multi-scale nature of these events
and the fact that we are approximating subgroup locations with
centroids, the dyadic distance does not go below the lower threshold
`thresh_l` and/or above the upper threshold `thresh_h`. In this case, we
still try to identify the `start_time` and end_time, but modify the
thresholds as follows:

First, let's define the period between `tidx - max_time` and `tidx` as
the *prior period*, the period between `tidx` and `tidx + max_time` as
the *subsequent period*, and the period between `(tidx - max_time/2)`
and `(tidx + max_time/2)` as the *middle period*.

For a fission, if the dyadic distance does not go above `thresh_h` in
the *subsequent period*, then we instead replace `thresh_h` with the
maximum - .001 of the dyadic distance during that time period. Second,
if the dyadic distance during the *middle period* does not drop below
`thresh_l`, then we instead move `thresh_l` to the minimum + .001 of the
dyadic distance during the *middle period*.

For a fusion, everything is reversed. If the dyadic distance does not go
above `thresh_h` during the *prior period*, we replace `thresh_h` with
the maximum - .001 of the dyadic distance during the *subsequent
period*. And if the dyadic distance does not go below `thresh_l` during
the *middle period*, we replace `thresh_l` with the minimum + .001 of
the dyadic distance during the *middle period*.

In very rare cases, due to these rules the upper bound may get changed
to something below the original `thresh_l`, or the lower bound may get
changed to something above the original `thresh_h`. In that case, we
revert to the original thresholds.

*Subtlety 2*: NA handling. NAs are handled in a couple of different
ways:

For finding the `start_time` and `end_time`, NAs are essentially
ignored, and cannot be part of the sequence from `start_time:end_time`
(at least not in terms of the) dyadic distance... individuals can drop
out and they will just be excluded from the centroid calculation.

If no start and end times are found, nothing else is computed (all
output values are filled in with `NA`s or `NULL` for matrices).

For finding the `before_time` and `after_time`, if an `NA` is hit in the
forward or backward direction, the `before_time` (or respectively, the
`after_time`) is marked as `NA`. Metrics involving that time point can
then not be computed and are also set to `NA`.

If the `start_time` and `end_time` are in different data chunks (as
specified by `breaks`), then both are given `NA`.

If the `before_time` and `start_time`, or `after_time` and `end_time`,
are in different data chunks (as specified by `breaks`), then both are
given NA.

If the `before_time` or `after_time` are `NA`, then other metrics
stemming from those times get `NA`

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
