# Get directedness

Gets the directedness of a trajectory over a given time window, as a
function of time.

## Usage

``` r
get_directedness(x_i, y_i, t_window)
```

## Arguments

- x_i:

  vector of x positions for an individual (or the group centroid) of
  length `n_times`

- y_i:

  vector of y positions for an individual (or the group centroid) of
  length `n_times`

- t_window:

  window of time to use for computing directedness (must be an even
  number) - the directedness for time t will be computed using position
  data from `t - t_window / 2` to `t + t_window / 2`

## Value

vector of directedness of the trajectory as a function of time

## Details

The directedness a number which ranges from 0 to 1 where 1 is a straight
path and 0 is a highly tortuous path. It is defined as the net
displacement (distance along a straight-line path from point A to point
B) divided by the path length (total distance traveled along the actual
path from point A to point B).

NOTE: Keep in mind that the time window `t_window` should be given as
the number of time points of data to use when computing directedness, so
the unit is number of timesteps, not seconds.

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
