# Convert from matrix format to dataframe

**\[experimental\]**

Creates a dataframe out of GPS data in "matrix format" suitable for
plotting with ggplot

## Usage

``` r
reformat_matrix_to_df(
  xs,
  ys,
  timestamps,
  ids = NULL,
  time_indices_selected,
  id_codes_selected = NULL,
  lats = NULL,
  lons = NULL
)
```

## Arguments

- xs:

  UTM eastings matrix (`n_inds` x `n_times` matrix where xs\[i,t\] gives
  the easting of individual i at time step t)

- ys:

  UTM northings matrix (`n_inds` x `n_times` matrix where ys\[i,t\]
  gives the northing of individual i at time step t)

- timestamps:

  vector of timestamps (POSIXct), must have same dimensions as columns
  of `xs` and `ys` matrices

- ids:

  Dataframe of individuals with individual name stored in a column
  called `id_code`. Only necessary if you want to select a subset of
  individuals using `id_codes_selected`

- time_indices_selected:

  Time indices for data to include in the dataframe, to be matched with
  xs, ys, timestamps

- id_codes_selected:

  Identity codes of individuals to include in dataframe. If none
  provided, all individuals are included

- lats:

  matrix of latitude values (`n_inds` x `n_times`)

- lons:

  matrix of longitude values (`n_inds` x `n_times`)

## Value

Returns a dataframe of id_codes, xs, ys, and timestamps

## Author

Eli Strauss

NOT YET CODE REVIEWED
