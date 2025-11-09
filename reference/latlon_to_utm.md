# Latitude / longitude to UTM conversion

Converts a matrix of lons and lats (lons first column, lats second
column) to UTM eastings and northings (eastings first column, northings
second column)

## Usage

``` r
latlon_to_utm(lons_lats, utm_zone, hemisphere)
```

## Arguments

- lons_lats:

  N x 2 matrix of longitudes (col 1) and latitudes (col2)

- utm_zone:

  numeric or string value of UTM zone

- hemisphere:

  northern or southern hemisphere - specify 'N' or 'S' (not case
  sensitive)

## Value

easts_norths, an N x 2 matrix of eastings (col 1) and northings (col 2)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
