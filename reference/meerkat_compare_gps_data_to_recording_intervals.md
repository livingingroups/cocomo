# Compare recorded meerkat GPS data to recording intervals noted in metadata

**\[experimental\]**

For a given round of meerkat collaring, verifies whether the GPS data
that is expected to be present (based on `GROUPYEAR_MOV_SUMMARY.txt`
file in METADATA) is present in `xs` and `ys` matrices from
`GROUPYEAR_xy_level0.RData`.

## Usage

``` r
meerkat_compare_gps_data_to_recording_intervals(xy_file, metadata_file)
```

## Arguments

- xy_file:

  path to file with the xy level 0 data

- metadata_file:

  path to corresponding metadata file

## Value

Returns 0 if the metadata and xy data match. Returns 1 otherwise and
prints any discrepancies.

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
