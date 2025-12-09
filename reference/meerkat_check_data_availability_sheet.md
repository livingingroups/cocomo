# Check meerkat data availability sheets for any inconsistencies or errors

**\[experimental\]**

## Usage

``` r
meerkat_check_data_availability_sheet(
  path_to_data_availability_sheet,
  path_to_data_dir
)
```

## Arguments

- path_to_data_availability_sheet:

  path to data availability sheet on the server (.csv)

- path_to_data_dir:

  path to the outer directory of the relevant deployment

## Details

Reads in a data availability sheet and checks for the following:

1.  Flag any missing fields

2.  Check internal consistency of each row

- datatype, tagtype, and datasource are all consistent and into expected
  categories

- normalday column is either 0 or 1

- start and end are correctly formatted times

- filename contains individual id code

- filename has expected file extension

- in audio files, filename contains date matching start date

- normalday and comments columns make sense together

1.  Check that file in filename column exists on server

2.  Finds files on the server that involve an individual from the group
    on a date included in the deployment, but aren't in the data
    availability sheet

Outputs everything into two csvs for manual checking:

- `path_to_data_availability_sheet`\_check.csv: rows to check in data
  availability sheet

- `path_to_data_availability_sheet`\_missing.csv: potential missing
  files (filling gaps in data availability sheet)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
