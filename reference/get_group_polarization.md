# Get group polarization

Computes the polarization of the group at each time step `t`. The
polarization is a measure of how aligned the group is, ranging from 0
(completely unaligned) to 1 (completely aligned) The polarization is
defined by adding up (vector addition) all of the heading vectors of all
individuals at a given moment in time, taking the length of the
resultant vector, and dividing this by the number of individuals. Either
spatial or temporal headings can be used.

## Usage

``` r
get_group_polarization(
  xs,
  ys,
  heading_type,
  spatial_R = NULL,
  t_window = NULL,
  forward = T,
  min_inds_tracked = NULL
)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

- heading_type:

  character string specifying heading type - `'spatial'` or `'temporal'`

- spatial_R:

  radius to use for spatial headings (if `heading_type = 'spatial'`)

- t_window:

  temporal window to use for temporal headings (if
  `heading_type = 'temporal'`)

- forward:

  whether to compute headings into the future (`forward = T`) or the
  past (`forward = F`)

- min_inds_tracked:

  if specified, sets a minimum number of individuals that must be
  tracked at any moment in time to compute headings (otherwise the
  polarization will be NA at that time point

## Value

Returns a vector of length `n_times` giving the polarization of the
group over time

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
