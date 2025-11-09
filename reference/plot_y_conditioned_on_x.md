# Plot a statistic of y conditioned on the value of x

For paired measurements `(x,y)`, plot a specified statistic `stat` (e.g.
the mean) of `y` for values of `x` within bins specified by the vector
`bins`

## Usage

``` r
plot_y_conditioned_on_x(
  x,
  y,
  bins = NULL,
  stat = "median",
  bin_by_quantile = T,
  n_bins = 10,
  error_bar_range = 0.25,
  xlab = "",
  ylab = "",
  main = "",
  xlim = NULL,
  ylim = NULL
)
```

## Arguments

- x:

  vector of numbers whose value will be conditioned on

- y:

  vector of numbers from which a statistic will be computed for values
  associated with each `x` bin

- bins:

  bins to use (will override `bin_by_quantile` and `n_bins` if
  specified)

- stat:

  which statistic of the distribution of `y` to use (can be
  `'mean' or 'median'`)

- bin_by_quantile:

  specifies whether to bin based on quantiles of the overall
  distribution of `x` (`T` or `F`)

- n_bins:

  number of bins to create if `bin_by_quantile = T` (bins will be evenly
  spaced according to quantiles)

- error_bar_range:

  if not `NULL`, will produce error bars spanning a given quantile range
  of the distribution of `y` for each bin. should be between `0` and
  `0.5` (defaults to `0.25`, or interquantile range)

- xlab:

  label for x axis

- ylab:

  label for y axis

- main:

  main plot label

- xlim:

  if specified, sets limits of x axis

- ylim:

  if specified, sets limits of y axis

## Value

Creates a plot, and also outputs a list with `bins` (bins used), `mids`
(the midpoints of the `x` bins), `y_stats` (the mean or median of `y`
for each bin), `y_uppers` and `y_lowers` (the upper and lower qauntiles
of `y` for each bin as specified by `error_bar_range`) and `ns` (the
number of data points in each bin)

## Details

For example, if `stat = 'mean'`, the function will plot `vals[i]` vs.
`bins[i]` where `vals[i]` is defined as
`mean(y[which(x >= bins[i] & x < bins[i+1])], na.rm=T)`

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
