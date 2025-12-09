# Plot individual behavior and calls during a time period specified by the user.

**\[experimental\]**

Also include a summary of the behavior of the rest of the group.

## Usage

``` r
plot_behav_and_calls(
  behavs,
  calls_array,
  behavs_key,
  focal_ind,
  t0,
  tf,
  nonfocal_calls_to_plot = NULL,
  nonfocal_behavs_to_plot = NULL,
  smooth_window = 31
)
```

## Arguments

- behavs:

  matrix of dimensions `n_inds` x `n_times` where `behavs[i,t]` gives
  the behavior (numeric) of individual `i` at time step `t`

- calls_array:

  array of dimensions `n_inds` x `n_times` x `n_calltypes` where
  `calls_array[i,t,c]` indicates the number of calls of type `c` given
  by the individual `i` at time step `t`

- behavs_key:

  data frame of behavior types with columns `behav` (behavior string)
  and `int_value` (its integer value in the `behavs` matrix)

- focal_ind:

  integer giving the index of the focal individual

- t0:

  integer giving the time step to start at

- tf:

  integer giving the time step to stop at

- nonfocal_calls_to_plot:

  vector of character strings indicating which calls to plot for
  nonfocal individuals (must match call types in `calls_array` 3rd
  dimension names)

- nonfocal_behavs_to_plot:

  vector of character strings indicating which behaviors to plot for
  nonfocal individuals (must match behavior types in `behavs_key`)

- smooth_window:

  smoothing window for indicating presence of nonfocal calls (in time
  steps)

- calls_to_plot:

  vector of character strings indicating call types to plot (other call
  types will get lumped into an "other" category)

- behavs_to_plot:

  vector of character strings indicating behavior types of plot (other
  behavior types will get lumped into an "other" category)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
