# Get speed

**\[stable\]**

Gets an individual's speed over time, using a specified time step to
compute the speed

## Usage

``` r
get_speed(x_i, y_i, seconds_per_timestep, t_window = 1, forward = T)
```

## Arguments

- x_i:

  x coordinates of the individual (a vector whose length is the number
  of timesteps) or of a group centroid

- y_i:

  y coordinates of the individual (a vector whose length is the number
  of timesteps) or of the group centroid

- seconds_per_timestep:

  the number of seconds between timestamps (e.g. for data recorded at 1
  Hz, the value would be 1, for data recorded every 30 sec, the value
  would be 30)

- t_window:

  the time window over which to compute speed (number of time steps into
  the future or past)

- forward:

  a boolean value (defaults to T) indicating whether to compute speeds
  forward in time (if T) or backward in time (if F)

## Value

Returns `speeds`: a time series of the speed of individual (a vector of
the same length as x_i and y_i)

## Details

The speed is computed as
`speeds[t] = sqrt ( (x_i[t+t_window] - x_i[t] )^2 + ( y_i[t+t_window] - y_i[t] )^2 ) / (seconds_per_timestep * t_window)`
Note that by default `speeds[t]` gives the instantaneous speed between
time `t` and time `t + t_window`, i.e. the speeds are computed based on
the future not on the past. If `forward` is set to `F`, the speeds will
be computed based on the past, as:
`speeds[t] = sqrt ( (x_i[t] - x_i[t-t_window] )^2 + ( y_i[t] - y_i[t-t_window] )^2 ) / (seconds_per_timestep * t_window)`

## Author

Ariana Strandburg-Peshkin

Reviewed by Brock
