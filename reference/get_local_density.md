# Get local density

**\[experimental\]**

Gets the number of individuals within a radius `R` of a focal individual
`i` at each time point

## Usage

``` r
get_local_density(xs, ys, i, R)
```

## Arguments

- xs:

  `N x n_times` matrix giving x positions of all `N` individuals over
  `n_times` timesteps

- ys:

  `N x n.times` matrix giving y positions of all `N` individuals over
  `n_times` timesteps

- i:

  the focal individual (for whom the local density will be computed)

- R:

  the radius over which to compute the local density (in same units as
  `xs` and `ys`)

## Value

Vector of length `n_times` giving the local density around the focal
individual (number of individuals within a radius R) at each time point

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
