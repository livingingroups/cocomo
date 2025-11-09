# Get nearest neighbor distance

Gets the distance to an individual `i`'s nearest neighbor over time

## Usage

``` r
get_nearest_neighbor_distance(xs, ys, i)
```

## Arguments

- xs:

  `N x n_times` matrix giving x positions (or UTM eastings) of all `N`
  individuals over `n_times` timesteps

- ys:

  `N x n_times` matrix giving y positions (or UTM northings) of all `N`
  individuals over `n_times` timesteps

- i:

  index of the focal individual (whose nearest neighbor distance will be
  computed)

## Value

Returns a vector of nearest neighbor distances at each time point
(length `= n_times`)

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
