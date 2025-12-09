# UTM to latitude / longitude conversion

**\[experimental\]**

Converts a matricx of UTM eastings and northings (eastings first column,
northings second olumn) to a matrix of lons and lats (lons first column,
lats second column)

## Usage

``` r
utm_to_latlon(easts_norths, utm_zone, hemisphere)
```

## Arguments

- easts_norths:

  N x 2 matrix of eastings (col 1) and northings (col2)

- utm_zone:

  numeric or string value of UTM zone

- hemisphere:

  northern or southern hemisphere - specify 'N' or 'S' (not case
  sensitive)

## Value

lons_lats, an N x 2 matrix of longitudes (col 1) and latitudes (col 2)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
