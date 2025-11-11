# Auto label synch times Automatic labeling of synch calls that are not labeled with a time, assuming they should be close to multiples of 90 sec apart File must have at least one fully labeled synch - and make sure this is correct! TODO: add more details on how this works here TODO: change max drift to be per time in file instead of absolute

Auto label synch times Automatic labeling of synch calls that are not
labeled with a time, assuming they should be close to multiples of 90
sec apart File must have at least one fully labeled synch - and make
sure this is correct! TODO: add more details on how this works here
TODO: change max drift to be per time in file instead of absolute

## Usage

``` r
meerkat_auto_label_synch_times(
  label_file,
  labeled_synchs,
  outdir,
  machine_labels = T,
  likeli_thresh = 0.6,
  max_drift = 1
)
```

## Arguments

- label_file:

  path to label file

- labeled_synchs:

  data frame of labeled synchs associated with that file

- outdir:

  output directory

- likeli_thresh:

  likelihood threshold (default to 0.6) - only use synchs above this
  threshold (only relevant if machine_labels = T)

- max_drift:

  maximum amount of drift allowed to still consider a synch call to be
  correctly positioned

- machine_labelswhether:

  the labels are machine generated (T) or not (F), defaults to T

## Value

Saves a new label file to the selected directory, with the original
filename with \_autosync.csv appended Also returns the name of the
output file (invisibly)

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
