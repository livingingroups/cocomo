# Synch labels within audio file to UTC time

Reads in a label file (in Audition format) and a synch file, gets synch
points, and synchs all labels in the file to UTC. Outputs a table with
an additional column specifying timestamp_UTC. Currently only designed
to work with meerkat data.

## Usage

``` r
meerkat_synch_audio_file_labels_to_UTC(
  path_to_label_file,
  path_to_synch_file = "~/EAS_shared/meerkat/working/METADATA/total_synch_info.csv",
  path_to_rawdata_dir = "~/EAS_shared/meerkat/archive/rawdata/",
  min_offset_outlier = 0.5,
  min_n_synchs = 3,
  min_frac_spanned_by_synchs = 0.2,
  make_plot = T,
  handle_special_cases = T,
  quadratic_fit = F,
  remove_noisy_synchs = T
)
```

## Arguments

- path_to_label_file:

  path to the label file from Audition

- path_to_synch_file:

  path to the synch file

- path_to_rawdata_dir:

  path to directory where raw data is stored (for matching wav file
  names)

- min_offset_outlier:

  minimum offset (in seconds) between fitted times and labeled talking
  clock times to be considered an outlier

- min_n_synchs:

  minimum number of synchs (after excluding outliers) to perform a fit

- min_frac_spanned_by_synchs:

  minimum fraction of the total file length (between first and last
  label time) spanned by synch calls to complete the synching

- make_plot:

  whether to also output a plot showing the synchs in time in recording
  vs talking clock time, with the final fit and outliers indicated

- handle_special_cases:

  whether (`T` or `F`) to handle a few special cases in the synch info
  table, such as when the synch clock stopped or when two synch clocks
  were around due to a group split with rovers - these cases had to be
  hardcoded in. this parameter should always be set to `T` for meerkat
  data

- quadratic_fit:

  whether (`T` or `F`) to perform a quadratic fit to the synchs

- remove_noisy_synchs:

  whether (`T` or `F`) to remove synchs that are labeled as noisy (will
  remove everything that has an x, not case sensitive)

## Value

Returns a list containing:

`filename`: basename of the label file

`synch_completed`: T or F, whether the synch was completed successfully

If `synch_completed == T`, also output:

`labels_synched`: data frame with columns:

`$label_unique_id` (unique id associated with a particular lable,
constructed from `wav_file|label_name|t0_file|duration`)

`$wav_file` (raw wav file name, without file extension)

`$csv_file` (csv label file name without file extension)

`$label_name` (name of the label)

`$date` (date in format YYYYMMDD)

`$id_code` (individual ID code, e.g. 'VCVM001')

`$t0_file` (start time of label marker in file, in seconds into the
file)

`$duration` (label marker duration, in seconds)

`$t0_UTC` (synched time of the label marker start in UTC)

`$tmid_UTC` (synched time of the label marker midpoint in UTC)

`$tf_UTC` (synched time of the label marker end in UTC)

`synchs_used`: data frame containing synch labels used in the fit, and
computed information about them

`outliers`: data frame with points labeled as outliers as well as
filenames, plus some other columns of info

If `synch_completed == F`, also output:

`reason_no_synch`: a string explaining why the file could not be synched

## Details

The label file should be in the format of Audition labels. The Name
column must contain labels of the form 'synch H:MM:SS' or 'synch MM:SS'
to specify the times of synch calls as heard on the talking clock.

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
