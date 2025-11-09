# Create a KML that shows animal trajectories (viewable on Google Earth Pro)

This function creates a KML file while can be loaded into Google Earth
Pro to view trajectories of a group over time.

## Usage

``` r
create_trajectories_kml(
  lons,
  lats,
  timestamps,
  id_codes,
  t0,
  tf,
  output_file_path,
  step = 1,
  cols = NULL,
  icons = NULL,
  fixed_locs = NULL,
  fixed_locs_icons = NULL,
  calls = NULL,
  calls_icons = NULL
)
```

## Arguments

- lons:

  `N x n_times` matrix giving longitude coordinates of each individual
  over time

- lats:

  `N x n_times` matrix giving latitutde coordinates of each individual
  over time

- timestamps:

  vector of timestamps (assumed to be in UTC) of length `n_times`

- id_codes:

  vector of character string specifying id codes of each animal to be
  plotted

- t0:

  time index at which to start

- tf:

  time index at which to end

- output_file_path:

  full path to the output file as a character string (must end in .kml)

- step:

  time resolution (in time steps)

- cols:

  vector of length `N` giving colors for each individual, e.g.
  'ffed8031' (first two elements give transparency, last 6 are color
  specified in hex). If NULL, trajectories will be white.

- icons:

  vector of length `N` specifying icons (further information below)

- fixed_locs:

  data frame with lon and lat coordinates of fixed locations to label
  (e.g. dens) - must have columns 'names','lon','lat'

- calls:

  data frame with columns `ind_idx`,`time_idx`,`call_type`, and `time`

- calls_icons:

  vector of length equal to the number of call types specifying which
  icons will be used for displaying calls

## Value

Creates and saves a kml to the specified output_file_path which can be
loaded into Google Earth to view animated trajectories

## Additional details on icon and line color specification

You can specify icons by giving a vector of filenames (character
strings) pointing to images on your computer (e.g. png works). Icons
should be contained in the same folder where the KMLs will be output, so
that Google Earth Pro will be able to read them in when you load the
KMLs. If this argument is set to NULL, the code will instead use
built-in blue markers from Google Earth. However, these are
unfortunately ugly.

You can specify the colors of lines using a hex format where the first
two digits give the transparency (ff = fully opaque) and the last 6
digits give the color in RGB. Unlike in R, a `'#'` should not be used
before the color, and the transparency goes first rather than last. For
example `'ffff0000'` specifies opaque red.

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
