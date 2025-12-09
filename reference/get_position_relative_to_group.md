# Get positions relative to group

**\[experimental\]**

Get individual position relative to the group centroid and heading for
all individuals.

## Usage

``` r
get_position_relative_to_group(
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
  tracked at any moment in time to compute heading (otherwise all
  positions will be NA at that time point)

## Value

Returns a list containing rel_xs and rel_ys matrices of each
individual's position relative to the group centroid and heading

## Details

The group centroid is defined as the origin (0,0) and the heading points
along the positive x axis. The heading can be computed either using a
specified time window or using spatial discretization.

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
