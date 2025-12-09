# Get spatially discretized heading and speed over time

**\[stable\]**

Gets spatially discretized heading for a given individual trajectory
This is defined as the unit vector pointing from the individual's
current location at time `t` to its location after it mas moved a
distance of at least `R`.

## Usage

``` r
get_heading_and_speed_spatial(
  x_i,
  y_i,
  R,
  t_idxs = 1:length(x_i),
  forward = T,
  seconds_per_time_step = 1
)
```

## Arguments

- x_i:

  vector of x coordinates for the trajectory

- y_i:

  vector of y coordinates for the trajectory

- R:

  radius used to compute the headings

- t_idxs:

  time indexes at which to compute the headings, speed, dts (defaults to
  entire trajectory)

- forward:

  whether to go forward in time from current position (if T), or
  backward (if F) when computing headings

- seconds_per_time_step:

  number of seconds corresponding to each time step

## Value

Returns a list containing

- `$heads`: a time series of the heading of the individual (a vector of
  the same length as x_i and y_i), in radians, computed based on spatial
  discretization around the time point

- `$speeds`: a time series of the average speed of the individual from
  the first point outside radius R to the current point

- `$dts`: time differences between the current point and the first point
  outside of the radius R (either forward or backward in time)

vector of spatially discretized headings, computed at all times or at
times t_idxs if specified (other times are then filled in with NAs)

## Author

Ariana Strandburg-Peshkin (primary author)

Reviewed by Brock
