# Get group centroid

Get the group's centroid (mean x and y position) over time. If
specified, only compute if at least `min_inds_tracked` are tracked in a
given time step.

## Usage

``` r
get_group_centroid(xs, ys, min_inds_tracked = NULL)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

- min_inds_tracked:

  if specified, sets a minimum number of individuals that must be
  tracked at any moment in time to compute centroid (otherwise the
  centroid will be NA at that time point

## Value

Returns a list containing coordinates of the group centroid over time,
`x_centr` and `y_centr` (both vectors of length `n_times`)

## Author

Ariana Strandburg-Peshkin (primary author)

Reviewed by Brock
