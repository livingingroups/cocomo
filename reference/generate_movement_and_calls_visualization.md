# Make a visualization of individual positions and calls during a time period specified by the user. The visualization can either show all individuals in the provided data or can zoom in on one individual and show its behavior in detail.

**\[experimental\]**

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
  ind_names = NULL,
  focus_ind = NULL,
  zoom_radius = 400,
  bg_color = "black",
  satellite_map = TRUE,
  utm_epsg = NULL,
  auto_zoom_tiles = TRUE,
  zoom_global = 15,
  zoom_focus = NULL,
  colors_inds = NULL,
  colors_calls = NULL,
  ind_point_size = 2,
  call_point_size = 1,
  pchs_inds = NULL,
  pchs_calls = NULL,
  tail_time = 10,
  call_persist_time = 10,
  show_legend_inds = TRUE,
  sort_legend_inds = NULL,
  show_legend_calls = FALSE,
  legend_loc = "topright",
  scalebar_size = 100,
  events = NULL,
  highlighted_radius = 1000
)
```

## Arguments

- xs:

  matrix of dimensions `n_inds` x `n_times` where `xs[i,t]` gives the x
  position (numeric) of individual `i` at time step `t`, positions in
  UTM format

- ys:

  matrix of dimensions `n_inds` x `n_times` where `ys[i,t]` gives the y
  position (numeric) of individual `i` at time step `t`, positions in
  UTM format

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

- ind_names:

  vector of names of the individuals

- focus_ind:

  name or index of a focal individual. This centers the frame on one
  individual instead of producing a broad overview. When using name,
  ind_names must be provided as well.

- zoom_radius:

  meters around the focal individual (UTM)

- bg_color:

  background color of the plot ("black", "white", etc.)

- satellite_map:

  use a satellite map as the background (T or F)

- utm_epsg:

  WGS 84 EPSG reference number to find the geographic location of the
  satellite map tile, use https://epsg.io to find the reference number

- auto_zoom_tiles:

  choose satellite map zoom automatically when focusing on individual,
  determines satellite map resolution

- zoom_global:

  zoom level for global tiles of the background satellite map,
  determines satellite map resolution

- zoom_focus:

  optional explicit map zoom for focus tiles, determines satellite map
  resolution

- colors_inds:

  vector of colors to use for each individual (length `n_inds`). Color
  indices must match the individual index in calls , xs and ys

- colors_calls:

  vector of colors to use for each call type (alphabetical order by
  call_type)

- ind_point_size:

  size of the individual points

- call_point_size:

  size of the points for calls

- pchs_inds:

  vector of plotting symbols for individuals

- pchs_calls:

  vector of plotting symbols for calls

- tail_time:

  number of previous time steps to plot as a "tail" which trails the
  point showing the current location

- call_persist_time:

  number of previous time steps to still show the calls (they will
  shrink linearly over time in size)

- show_legend_inds:

  whether to plot a legend showing the names of the individuals (T or F)

- sort_legend_inds:

  vector to sort the legend showing the names of the individuals (vector
  containing the sort order by index)

- show_legend_calls:

  whether to plot a legend showing the names of the call types (T or F)

- legend_loc:

  location of the individual legend, either 'topleft' or 'topright'.
  Call legend will be opposite

- scalebar_size:

  number of meters for the scalebar, set to 0 to deactivate

- events:

  data frame with columns `event_id`,
  `start_time_idx`,`end_time_idx`,`initiator`

- highlighted_radius:

  radius of the highlighted location (usually an epicenter from hyena
  whoop analysis)

## Author

Ariana Strandburg-Peshkin (primary author)

Marius Faiß (secondary author)

NOT YET CODE REVIEWED
