# Import meerkat GPS data

**\[experimental\]**

\#Imports raw meerkat GPS data from Technosmart files (Gipsy 5 or
Axy-Trek) assuming a standardized file folder structure of `input_dir`
and standardized naming schemes as done in the meerkat group collaring
project. Outputs a file containing the xs, ys, and timestamps matrices
as well as another file with the GPS data table in Movebank format

## Usage

``` r
meerkat_import_meerkat_gps_data(
  input_dir,
  output_dir,
  metadata_dir = "~/EAS_shared/meerkat/working/METADATA/",
  tag_type,
  start_date = NULL,
  end_date = NULL,
  start_time = NULL,
  end_time = NULL,
  min_satellites = 5,
  utm_zone = 34,
  hemisphere = "south",
  seconds_per_time_step = 1,
  timezone = "UTC"
)
```

## Arguments

- input_dir:

  full path to input directory where all files from a given deployment
  are stored (e.g.
  `"~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_1/"`)

- output_dir:

  full path to the output directory where processed files will be saved

- metadata_dir:

  full path to the directory holding metadata files
  (GROUPYEAR_INDIVIDUAL_INFO.txt)

- tag_type:

  `'gipsy5'` or `'axytrek'`

- start_date:

  a character string specifying the starting date for data sampled in a
  daily basis, only used if `data_chunks = NULL`, format must be
  `'YYYY-MM-DD'`

- end_date:

  a character string specifying the end date for data sampled on a daily
  basis, only used if `data_chunks = NULL`, format must be
  `'YYYY-MM-DD'`

- start_time:

  a character string specifying the start time for data sampled on a
  daily basis, only used if `data_chunks = NULL`, format must be
  `'HH:MM:SS'`

- end_time:

  a character string specifying the end time for data sampled on a daily
  basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`

- min_satellites:

  minimum number of satellites to include data

- utm_zone:

  numeric UTM zone (only needed if `output_utm = T` and if
  `movebank_data` does not have `utm_easting` and `utm_northing`
  columns)

- hemisphere:

  hemisphere (`'north'` or `'south'`) for UTM calculations (only needed
  if `output_utm = T` and if `movebank_data` does not have `utm_easting`
  and `utm_northing` columns)

- seconds_per_time_step:

  sampling interval of GPS fixes (in seconds)

- timezone:

  timezone to use (UTC)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
