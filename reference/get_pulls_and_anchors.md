# Get pulls and anchors

**\[experimental\]**

Get successful ('pull') and failed ('anchor') dyadic interactions
between individuals a and b.

## Usage

``` r
get_pulls_and_anchors(
  xa,
  xb,
  ya,
  yb,
  a,
  b,
  noise_thresh = 5,
  plot_results = F,
  include_initial_fusion = F,
  include_final_fission = F
)
```

## Arguments

- xa:

  x coordinates for individual a (vector of length `n_times`)- must be
  continuous data (no NAs)

- xb:

  x coordinates for individual b (vector of length `n_times`)- must be
  continuous data (no NAs)

- ya:

  x coordinates for individual a (vector of length `n_times`)- must be
  continuous data (no NAs)

- yb:

  x coordinates for individual b (vector of length `n_times`)- must be
  continuous data (no NAs)

- a:

  index of the first individual

- b:

  index of the second individual

- noise_thresh:

  noise threshold (defaults to 5 m)

- plot_results:

  whether to plot results or not

- include_initial_fusion:

  if T, the function will also output an initial fusion event. In most
  use cases, this should be set to false. See below for details.

- include_final_fission:

  if T, the funciton will also output a final fission event. In most use
  cases, this should be set to false. See below for details.

## Value

Returns a data frame containing dyadic interactions between a and b.
Contains columns: `t1`, `t2`, `t3`, `initiator`, `responder`, `type`,
`disparity`, `strength`,`disparity_additive`, and `strength_additive`.
The `disparity` and `strength` are as defined in Strandburg-Peshkin et
al. 2015, whereas `disparity_additive` and `strength_additive` are
alternative formulations of these metrics that add the components
together instead of multiplying them.

## Details on fission and fusion events

If `include_initial_fusion` is set to T, the script will also return the
"fusion" defined by the beginning of the sequence and the first minimum.
If `include_final_fission` is set to T, the script will also return the
"fission" defined by the last minimum and the end of the sequence. If
the first or last min/max values are not minima, half events will not be
returned for them. The purpose of this option is that if this code is
used on a sequence where two individuals come together and then split
apart (in fission-fusion analyses), the first and last "half events"
represent fusion and fission events, so these are extracted. In normal
use cases, this value should be set to F (default). However in cases
where this analysis is being run on the "together periods" of two
individuals in a fission-fusion analysis, these values can be set to T
to return the times and initiators of the initial fusion and eventual
fission event. In the case of fission and fusion events, there are only
two time points return (`t1` and `t2`). `t3` is defined as `NA`. The
`initiator` is defined as the individual with the greater displacement
during the period `t1` to `t2` and the `responder` is the other
individual. The `strength` and `disparity` are defined in parallel to
those for pulls and anchors, but using only the one time period (`t1` to
`t2`). To make the values somewhat comparable to pulls and anchors,
these values are squared.

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
