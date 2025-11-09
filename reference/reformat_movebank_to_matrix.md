# Reformat Movebank to matrix format

Takes in regularly sampled GPS data in Movebank format (i.e. data frame
with specified columns - see below) and converts it into the standard
matrix form used by the `cocomo` package.

## Usage

``` r
reformat_movebank_to_matrix(
  movebank_data,
  output_file_path = NULL,
  ids = NULL,
  data_chunks = NULL,
  seconds_per_time_step = 1,
  start_date = NULL,
  end_date = NULL,
  start_time = NULL,
  end_time = NULL,
  utm_zone = NULL,
  hemisphere = NULL,
  output_utm = T,
  output_latlon = T,
  use_UTC = T,
  local_timezone = NULL
)
```

## Arguments

- movebank_data:

  data frame that must include the columns `'timestamp` (timestamp in
  UTC, must be character string of format YYYY-MM-DD HH:MM:SS.SSS),
  `'individual.local.identifier'` (individual id of the tracked
  animals), `'location.long'` (longitude coordinate), and
  `'location.lat` (latitude coordinate). Can optionally also include the
  column `'study.local.timestamp'` (local timestamp for each GPS fix,
  must be character string of format YYYY-MM-DD HH:MM:SS.SSS).

- output_file_path:

  full path to the desired output file (must be a .RData file)

- ids:

  optional ids data frame which if specified will determine the order of
  rows in matrices (`code` column in ids needs to match to
  `individual.local.identifier` column in `movebank_data`)

- data_chunks:

  data frame with columns `start` and `end` specifying starting and
  ending datetimes (in POSIXct format - with time zone!) for each
  contiguous chunk of data. overrides other specified start and end
  times / dates.

- seconds_per_time_step:

  sampling interval of GPS fixes (in seconds)

- start_date:

  a character string specifying the starting date for data sampled in a
  daily basis, only used if `data_chunks = NULL`, format must be
  `'YYYY-MM-DD'`

- end_date:

  a character string specifying the end date for data sampled in a daily
  basis, only used if `data_chunks = NULL`, format must be
  `'YYYY-MM-DD'`

- start_time:

  a character string specifying the start time for data sampled on a
  daily basis, only used if `data_chunks = NULL`, format must be
  `'HH:MM:SS'`

- end_time:

  a character string specifying the end time for data sampled on a daily
  basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`

- utm_zone:

  numeric UTM zone (only needed if `output_utm = T` and if
  `movebank_data` does not have `utm_easting` and `utm_northing`
  columns)

- hemisphere:

  hemisphere (`'north'` or `'south'`) for UTM calculations (only needed
  if `output_utm = T` and if `movebank_data` does not have `utm_easting`
  and `utm_northing` columns)

- output_utm:

  whether to output `xs` and `ys` matrices (`T` or `F`)

- output_latlon:

  whether to output `lats` and `lons` matrices (`T` or `F`)

- use_UTC:

  if T (default) use UTC time rather than local time

- local_timezone:

  specify local timezone string, if not using UTC (use_UTC = F)

## Value

Saves a file to the location `output_file_path` containing the objects:
`timestamps` (vector of timestamps in POSIXct format, of length
`n_times`), `ids` (data frame containing either the original `ids` data
or a single `code` column with the id from `individual.local.identifier`
column in `movebank_data`), `xs` (`n_inds` x `n_times` matrix of UTM
eastings for all individuals at each time point), `ys` (`n_inds` x
`n_times` matrix of UTM northings for all individuals at each time
point), `lons` (`n_inds` x `n_times` matrix of longitudes for all
individuals at each time point), and `lats` (`n_inds` x `n_times` matrix
of longitudes for all individuals at each time point). `xs` and `ys` are
only saved if `output_utm = T`. `lons` and `lats` are only saved if
`output_lonlat = T`

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
