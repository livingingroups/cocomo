# Get group heading

**\[stable\]**

Computes the group centroid heading over time using either temporal or
spatial headings, and going either forward (into the future) or backward
(into the past) when computing the heading

## Usage

``` r
get_group_heading_and_speed(
  xs,
  ys,
  heading_type,
  spatial_R = NULL,
  t_window = NULL,
  forward = T,
  min_inds_tracked = NULL,
  seconds_per_time_step = 1
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
  tracked to use that time point in computing heading. headings, speeds,
  and dt that would rely on data with an insufficient number of
  individuals will be reported as NA.

- seconds_per_time_step:

  number of seconds corresponding to each time step

## Value

Returns the group heading over time, a vector of length `n_times`

## Author

Ariana Strandburg-Peshkin (primary author)

Reviewed by Brock
