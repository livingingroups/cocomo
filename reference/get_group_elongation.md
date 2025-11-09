# Get group polarization

Computes the elongation of the group at each time step `t` as well as
the long axis vector. The elongation is a measure of how "line-link" vs
"circle-like" the group is, ranging from 0 (completely circular) to 1
(completely elongated) The elongation is defined by first performing a
PCA on the x/y data of the group. The axis of greatest variation is
defined as the 'long axis' and the axis perpendicular to this is defined
as the 'short axis'. The elonation is then defined as 1 - (length of
short axis / length of long axis).

## Usage

``` r
get_group_elongation(xs, ys, min_inds_tracked = 3)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

- min_inds_tracked:

  if specified, sets a minimum number of individuals that must be
  tracked at any moment in time to compute elongation (otherwise the
  elongation will be NA at that time point). Default is 3 (below this,
  elongation is generally undefined)

## Value

Returns a list containing: `out$elongation`: a vector of length
`n_times` giving the elongation of the group over time
`out$long_axis_angle`: a vector of length `n_times` giving the angle of
the long axis over time

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
