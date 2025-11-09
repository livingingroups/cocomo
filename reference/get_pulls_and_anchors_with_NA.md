# Wrapper for `get_pulls_and_anchors` function, handling datasets with NAs

Create a list of time blocks using NAs in the dataset as seperation
points, then run the `get_pulls_and_anchors` function on each of these
blocks separately.

## Usage

``` r
get_pulls_and_anchors_with_NA(
  xa,
  xb,
  ya,
  yb,
  a,
  b,
  noise_thresh = 5,
  plot_results = F,
  include_initial_fusion = F,
  include_final_fission = F,
  NA_tolerance = 0,
  min_time = NULL,
  verbose = FALSE
)
```

## Arguments

- xa:

  x coordinates for individual a (vector of length `n_times`)

- xb:

  x coordinates for individual b (vector of length `n_times`)

- ya:

  x coordinates for individual a (vector of length `n_times`)

- yb:

  x coordinates for individual b (vector of length `n_times`)

- a:

  index of the first individual

- b:

  index of the second individual

- noise_thresh:

  see documentation of `get_pulls_and_anchors` function

- plot_results:

  see documentation of `get_pulls_and_anchors` function

- include_initial_fusion:

  see documentation of `get_pulls_and_anchors` function

- include_final_fission:

  see documentation of `get_pulls_and_anchors` function

- NA_tolerance:

  How big of a gap in the vector (missing timesteps), created by NAs, is
  tolerated and therefore still combined into a single continuous
  dataset (numeric, positive integer)

- min_time:

  the minimum length (timesteps) of a resulting time block that will
  still be used for analysis (numeric, positive integer)

- verbose:

  If verbose = TRUE, then print out the size of each NA gap

  For following parameters and more information, see documentation of
  `get_pulls_and_anchors` function:

## Value

Returns a data frame containing dyadic interactions between a and b.
Contains columns: `t1`, `t2`, `t3`, `initiator`, `responder`, `type`,
`disparity`, `strength`,`disparity_additive`, and `strength_additive`.

## Details

Time blocks are created by splitting the dataset at the indexes of NAs.
If the length of the NA-chain is below "NA_tolerance", then the script
will ignore these NAs and not split at that position. Instead, the
script will fill the gap with an interpolation of values between the
previous and next existing measured value entry. This results in the
datapoints staying connected/in a single time block for small breaks. If
the NA-chain in the dataset is longer than "NA_tolerance", then the
values before and after that chain are split into separate time blocks.
If "min_time" != NULL, then the script will remove all time blocks
shorter than the specified value (usually time steps), resulting only in
time blocks of relevant lengths. If filtering by "min_time" leaves no
time blocks to analyze, then the script will return "NULL".

Note that if either `include_initial_fusion` or `include_final_fission`
are true, this function will treat each time block separately. In other
words, a fission and/or fusion might be detected for each time block.

## Author

Dario Walser

NOT YET CODE REVIEWED
