# Parse Adobe Audition formatted time

Parses a time from the `Start` or `Duration` column of an Audition label
file to seconds

## Usage

``` r
parse_audition_time(audition_time_str)
```

## Arguments

- audition_time_str:

  character string correspoding to an Audition-formatted time

## Value

Returns numeric value of time into the file (in seconds)

## Details

The `Start` and `Duration` columns of Adobe Audition label files have a
non-standard format. Within the first 10 minutes, they are formatted as
M:SS.SSS. After 10 minutes they are formatted as MM:SS.SSS and after the
first hour they are formatted as H:MM:SS.SSS (and presumably after the
first 10 hours as HH:MM:SS.SSS). This function parses these weird labels
and returns a numeric value of the time into the file, in seconds

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
