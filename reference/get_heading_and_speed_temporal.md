# Get heading and speed over time (temporal)

Gets the headings and speeds of an individual over time given a
trajectory. Uses a temporal window `t_window` (in units of timesteps) to
compute the headings and speeds, and can compute them based on future
movement (`forward = T`) or past movement (`forward = F`). Headings are
defined as `x_head = dx / (dx^2 + dy^2)` and
`y_head = dy / (dx^2 + dy^2)` where,
`dx[t] = x_i[t + t_window] - x_i[t]` and
`dy[t] = y_i[t + t_window] - y_i[t]` for `forward = T` and
`dx[t] = x_i[t] - x_i[t - t_window]` and
`dy[t] = y_i[t] - y_i[t - t_window]` for `forward = F` Speeds are
defined as `speed = sqrt(dx^2 + dy^2) / t_window`.

## Usage

``` r
get_heading_and_speed_temporal(
  x_i,
  y_i,
  t_window = 1,
  forward = T,
  seconds_per_time_step = 1
)
```

## Arguments

- x_i:

  x coordinates of the individual (a vector whose length is the number
  of timesteps) or of a group centroid

- y_i:

  y coordinates of the individual (a vector whose length is the number
  of timesteps) or of the group centroid

- t_window:

  the time window over which to compute speed (number of time steps into
  the future or past)

- forward:

  a boolean value (defaults to T) indicating whether to compute speeds
  forward in time (if T) or backward in time (if F)

- seconds_per_time_step:

  number of seconds corresponding to each time step

## Value

Returns a list containing `$heads`: a time series of the heading of the
individual (a vector of the same length as x_i and y_i), in radians,
`$speeds`: a time series of the speed of the individual at each time
point, and `$dts`: time differences between each point (will be all the
same value, but given as a vector for compatibility with spatial
headings function)

## Author

Ariana Strandburg-Peshkin

Reviewed by Brock
