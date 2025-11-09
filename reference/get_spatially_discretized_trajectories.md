# Get spatially discretized trajectories

Gets spatially discretized trajectories from temporally discretized
trajectories, using a certain "ruler length" R.

## Usage

``` r
get_spatially_discretized_trajectories(xs, ys, R, breaks = NULL, verbose = T)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

- R:

  radius (ruler length) used in spatial discretization

- breaks:

  vector of indexes to breaks in the data (e.g. breaks between days)

- verbose:

  whether to print progress while running

## Value

Returns a list containing `spat_ts` (the time points associated with
each point along the spatially discretized trajectory), `spat_xs` (the x
coordinates of each point along the spatially discretized trajectory),
`spat_ys` (the y coordinates of each point along the spatially
discretized trajectory), `spat_breaks` (indexes to starts of breaks in
the new spatially discretized data), `R` (radius used)

## Details

TODO: Code doesn't deal well with strings of NAs within it - look into
this.

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
