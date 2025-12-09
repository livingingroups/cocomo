# Get group dyadic distances

**\[experimental\]**

Computes the distance between each pair of individuals over time.

## Usage

``` r
get_group_dyadic_distances(xs, ys)
```

## Arguments

- xs:

  `N x n_times` matrix giving x coordinates of each individual over time

- ys:

  `N x n_times` matrix giving y coordinates of each individual over time

## Value

`N x N x n_times` `dyad_dists` array giving dyadic distance between all
pairs of individuals at each time step `dyad_dists[i,j,t]` gives the
distance between individuals `i` and `j` at time `t`

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
