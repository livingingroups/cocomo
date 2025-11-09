# Import Axy-Trek GPS file

Imports GPS data from an Axy Trek file, which may have lines that are
not formatted such that read.delim can parse them. Can currently handle
files from 2021-2023 at least. Output data frame has column names
compatible with the function
[`cocomo::reformat_movebank_to_matrix`](https://livingingroups.github.io/cocomo/reference/reformat_movebank_to_matrix.md).

## Usage

``` r
import_axytrek_gps_file(input_file_path)
```

## Arguments

- input_file_path:

  full path to the input file

## Value

Returns a data frame with the following columns: `timestamp`,
`location.lat`, `location.long`, `V4`, `V5`, `satellite.count`, `V7`

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
