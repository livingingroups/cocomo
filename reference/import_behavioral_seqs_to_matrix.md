# Import behavioral sequences (output from classifier) to matrix format

**\[experimental\]**

Imports behavioral sequence data from a set of csv files and converts
them to matrix format matching the `xs` and `ys` matrices used in the
`cocomo` library

## Usage

``` r
import_behavioral_seqs_to_matrix(input_dirs, ids, timestamps)
```

## Arguments

- input_dirs:

  list of input directories where behavioral sequence data are stored
  (full paths)

- ids:

  data frame giving individual ids for that deployment

- timestamps:

  vector of timestamps associated with that deployment (in UTC)

## Value

Returns a list containing `out$behavs`, a matrix where `out$behavs[i,t]`
gives the behavior of individual `i` (correspoding to the row in `ids`
at time `t` (corresponding to the index in `timestamps`) In this matrix,
the behavior is represented as an integer value (for quicker
processing). The second item in the list, `out$behavs_key` is a data
frame with columns `behav` (the behavior as a string) and `int_value`
(the correspoding integer value associated with that behavior in the
`out$behavs` matrix)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
