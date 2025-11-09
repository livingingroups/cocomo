# Preprocess GPS data (in matrix format) from level 0 to level 1

This function takes in a level 0 dataset (uncleaned location data in UTM
coordinates) and performs minimal pre-processing to return a level 1
dataset. See
[latlon_to_utm](https://livingingroups.github.io/cocomo/reference/latlon_to_utm.md)
to convert from lat/lon to UTM.

## Usage

``` r
preprocess_gps_level0_to_level1(
  input_file_path = NULL,
  output_file_path = NULL,
  xs = NULL,
  ys = NULL,
  timestamps = NULL,
  ids = NULL,
  breaks = NULL,
  remove_unrealistic_speeds = T,
  remove_isolated_points = T,
  remove_unrealistic_locations = T,
  interpolate_small_gaps = T,
  interpolate_stationary_periods = T,
  max_speed_percentile = 0.9995,
  max_speed = NULL,
  max_sd_away = 10,
  max_dist_percentile = 0.9999,
  max_isolated_point_dist = 1000,
  max_interp_len = 5,
  max_move_dist = 5,
  max_move_time = 5,
  bounding_box = NULL,
  verbose = T
)
```

## Arguments

- input_file_path:

  full path to the input file containing `xs`, `ys`, `timestamps`, and
  `ids` (overrides manual passing in of these parameters), must be an
  RData file

- output_file_path:

  full path to the output file where the level 1 dataset will be stored,
  must end in .RData

- xs:

  `n_inds x n_times` matrix giving x coordinates (in UTM eastings) of
  each individual over time (if an input file is not specified, pass
  this in manually)

- ys:

  `n_inds x n_times` matrix giving y coordinates (in UTM northings) of
  each individual over time (if an input file is not specified, pass
  this in manually)

- timestamps:

  vector of timestamps (if an input file is not specified, pass this in
  manually)

- ids:

  data frame containing information about each individual (if an input
  file is not specified, pass this in manually)

- breaks:

  vector giving indexes to breaks in the data (e.g. gaps between
  recording intervals), if the sequence is not continuous. breaks should
  specify the index associated with the beginning of each interval,
  starting with 1 (the first interval)

- remove_unrealistic_speeds:

  whether to remove unrealistic speeds (`T` or `F`)

- remove_isolated_points:

  whether to remove isolated points (`T` or `F`)

- remove_unrealistic_locations:

  whether to remove unrealistic locations (`T` or `F`)

- interpolate_small_gaps:

  whether to interpolate small gaps (`T` or `F`)

- interpolate_stationary_periods:

  whether to interpolate stationary periods (`T` or `F`) - cannot be run
  unless `interpolate_small_gaps` is also `T`

- max_speed_percentile:

  quantile to use to define the maximum speed

- max_speed:

  maximum speed (overrides max_speed_percentile if specified). This
  value is interpreted as the max allowable meters traveled in one time
  step in `timesteps`.

- max_sd_away:

  standard deviation of xs and ys distributions for each individual
  beyond which points will be removed

- max_dist_percentile:

  quantile to use to define the maximum x and y coordinates of points to
  check for isolation (those outside and isolated will be removed)

- max_isolated_point_dist:

  maximum isolated point distance

- max_interp_len:

  maximum length of an `NA` gap to linearly interpolate (number of time
  points)

- max_move_dist:

  maximum distance moved during a time `max_move_time` to be considered
  stationary during interpolation of stationary periods (interpolated
  using the average position)

- max_move_time:

  maximum time of a gap to interpolate if stationary (number of time
  points)

- bounding_box:

  vector of length 4 giving a bounding box outside of which points will
  be removed - should be in the format
  `c(min_easting, max_easting, min_northing, max_northing)`

- verbose:

  whether to print out progress and information

## Value

Returns a list containing new `xs` and `ys` matrices, and also saves
them plus the `timestamps` and `ids` objects to an output file if
specified

## Details

The function performs the following steps (in order):

1.  If `remove_isolated_points = T`, finds extreme distances
    `> max_dist_percentile` quantile (default 99.99%) or
    `< 1 - max_dist_percentile` of `xs` or `ys` for each individual and,
    if there are no other points from that individual within
    `max_isolated_point_dist` (default 1000 m) of that point, replaces
    them with `NA`s

2.  If `remove_unrealistic_speeds = T`, removes unrealistic speeds
    (greater than `max_speed_percentile`) and replaces them with NAs
    (default .9995 quantile, or alternatively a max speed `max_speed`
    can be set manually).

3.  If `remove_unrealistic_locations = T`, finds extreme xs and ys above
    `mean + sd * max_sd_away` (default 10) for each ind and removes
    those

4.  If `bounding_box != NULL`, removes all points outside of a specified
    `bounding_box = c(min_easting, max_easting, min_northing, max_northing)`

5.  If `interpolate_small_gaps = T`, fills in missing data gaps less
    than length `max_interp_len` with linear interpolation (default 5)

6.  If `interpolate_stationary_periods = T`, finds instances where an
    animal did not move more than `max_move_dist` (default 5 m) during
    an `NA` gap of `< max_move_time` (default 300 timesteps) and
    replaces them with the mean location of the individual between start
    and end of the sequence

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
