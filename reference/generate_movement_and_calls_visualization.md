# Make a visualization of individual positions and calls during a time period specified by the user.

Make a visualization of individual positions and calls during a time
period specified by the user.

## Usage

``` r
generate_movement_and_calls_visualization(
  xs = NULL,
  ys = NULL,
  timestamps = NULL,
  calls = NULL,
  start_time = NULL,
  end_time = NULL,
  time_step = 1,
  output_dir = NULL,
  tail_time = 10,
  call_persist_time = 5,
  colors_inds = NULL,
  colors_calls = NULL,
  pchs_inds = NULL,
  pchs_calls = NULL,
  show_legend_inds = T,
  show_legend_calls = T,
  legend_loc = "topright",
  show_time = T,
  show_scalebar = T,
  scalebar_size = 100,
  scalebar_loc = "bottomleft",
  scalebar_offset = 20,
  ind_names = NULL,
  bg_color = "black",
  ind_point_size = NULL,
  call_point_size = NULL,
  events = NULL,
  highlighted_radius = 1000
)
```

## Arguments

- xs:

  matrix of dimensions `n_inds` x `n_times` where `xs[i,t]` gives the x
  position (numeric) of individual `i` at time step `t`

- ys:

  matrix of dimensions `n_inds` x `n_times` where `xs[i,t]` gives the x
  position (numeric) of individual `i` at time step `t`

- timestamps:

  vector of timestamps (POSIXct), must have same dimensions as columns
  of `xs` and `ys` matrices

- calls:

  data frame where first column (`'ind_idx'`) specifies the index of the
  individual that gave the call, second column (`'time_idx'`) specifies
  the time index at which the call was given, and third column
  (`'call_type'`) specifies the type of call (character string)

- start_time:

  time index at which to start the video

- end_time:

  time index at which to end the video

- time_step:

  time step to use (an image every time_step timesteps will be produced)

- output_dir:

  directory in which to store the folder of outputted images

- tail_time:

  number of previous time steps to plot as a "tail" which trails the
  point showing the current location

- call_persist_time:

  number of previous time steps to still show the calls (they will
  shrink linearly over time in size)

- colors_inds:

  vector of colors to use for each individual (length `n_inds`)

- colors_calls:

  vector of colors to use for each call type (alphabetical order by
  call_type)

- pchs_inds:

  vector of plotting symbols for individuals

- pchs_calls:

  vector of plotting symbols for calls

- show_legend_inds:

  whether to plot a legend showing the names of the individuals (T or F)

- show_legend_calls:

  whether to plot a legend showing the names of the call types (T or F)

- legend_loc:

  location of the legend, either
  `topleft','topright','bottomleft' or 'bottomright'`

- show_time:

  whether to show the timestamp or not (`T` or `F`)

- show_scalebar:

  whether to show a scalebar or not (`T` or `F`)

- scalebar_size:

  number of meters for the scalebar

- scalebar_loc:

  location of the scale bar, either
  `topleft','topright','bottomleft' or 'bottomright'`

- scalebar_offset:

  scalebar offset from the edge (fraction of entire width)

- ind_names:

  vector of names of the individuals

- bg_color:

  background color

- ind_point_size:

  size of the individual points

- call_point_size:

  size of the points for calls

- events:

  data frame with columns `event_id`,
  `start_time_idx`,`end_time_idx`,`initiator`

- highlighted_radius:

  radius of the highlighted location (usually an epicenter from hyena
  whoop analysis)

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
