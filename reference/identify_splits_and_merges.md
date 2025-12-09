# Identify group splits and merges from multi-individual trajectory data

**\[stable\]**

Detects splits and merges (a.k.a. fissions and fusions) using
"sticky-DBSCAN" method from Libera et al. 2023.

## Usage

``` r
identify_splits_and_merges(
  xs,
  ys,
  timestamps,
  R_inner,
  R_outer,
  breaks = c(1, length(timestamps) + 1),
  names = NULL,
  break_by_day = F
)
```

## Arguments

- xs:

  UTM eastings matrix (`n_inds` x `n_times` matrix where xs\[i,t\] gives
  the easting of individual i at time step t)

- ys:

  UTM northings matrix (`n_inds` x `n_times` matrix where ys\[i,t\]
  gives the northing of individual i at time step t)

- timestamps:

  vector of timestamps (POSIXct), must have same dimensions as columns
  of `xs` and `ys` matrices

- R_inner:

  inner distance threshold to identify periods of connectedness
  (numeric)

- R_outer:

  outer distance threshold to identify periods of connectedness
  (numeric)

- breaks:

  indexes to breaks in the data (default NULL treats data as a
  contiguous sequence). If specified, overrides `break_by_day`

- names:

  optional vector of names (if NULL, will be defined as
  `as.character(1:n_inds)`)

- break_by_day:

  whether to break up data by date (T or F)

## Value

a list containing:

`events_detected`: data frame with info on detected fissions and
fusions, and limited info for shuffles

`all_events_info`: list of information about all fission-fusion (or
shuffle) events.

`groups_list`: list of subgroups in each timestep

`together`: n_inds x n_inds x n_times array of whether dyads are
connected (1) or not (0) or unknown (NA)

`R_inner`: inner radius used in the computations (same as `R_inner`
above)

`R_outer`: outer radius used in the computations (same as `R_outer`
above)

## Details

Start by defining an adjacency matrix (`together` in the code) of which
dyads are "connected" at any moment in time. Dyads are considered to be
connected if they go within a distance `R_inner` of one another, and
they continue to be connected until they leave a distance `R_outer` of
one another on both ends (before and after) of the period where their
distance dropped below `R_inner`. This double threshold makes periods of
connectedness more stable by removing the "flicker" that would result
from having a single threshold.

NA handling: Individuals are considered not together if either of their
positions is not known. If a period of connectedness runs into an NA,
individuals will be considered as connected up until that NA. Once
connectedness of dyads is determined, merge dyads together into groups
by using DBSCAN on 1 - `together` as the distance matrix, with `eps`
equal to something small (.01 in the code). Store these groups in
`groups_list`, a list of lists whose length is equal to `n_times`.
Stepping through the `groups_list`, identify all changes in group
membership, i.e. consecutive time points when the groups do not match.
The algorithm flags any change, including instances where individuals
disappear or reappear in groups due to missing data (but these are later
ignored). Store in `changes` data frame.

In a last step, identify all splits ('fission'), merges ('fusion'), and
things that cannot be classified as either fissions or fusions because
they contain elements of both ('shuffle'). This is done by constructing
a bipartite network at each time step t, where groups at time t are
connected to groups at time t + 1 if they share at least 1 member. Then,
we identify the connected components of this bipartite network.
Components where a single group (node) at time t is connected to
multiple groups (nodes) at time t + 1 get identified and classified as
`event_type = 'fission'`. Components where multiple nodes at time t are
connected to a single node at time t + 1 are classified as
`event_type = 'fusion'`. Components where a single node at time t is
connected to a single node at time t + 1 are skipped (they are not
fissions, fusions, or shuffles). All other events where more complex
things happen are classified as `event_type = 'shuffle'`.

After events are identified, various event features are computed and
saved in a data frame. See list of outputs for more details.

## Additional information about returned objects

`events_detected` data frame:

- `events_detected$event_idx`: unique id number of the event

- `events_detected$tidx`: (initial) time index of the event

- `events_detected$event_type`: "fission" or "fusion" or "shuffle"

- `events_detected$n_groups_before`: number of groups prior to the event

- `events_detected$n_groups_after`: number of groups after the event

- `events_detected$big_group_idxs`: indexes of all the individuals
  involved in the event

- `events_detected$big_group`: names of all the individuals involved in
  the event

- `events_detected$group_A_idxs`, `$group_B_idxs`, `$group_C_idxs`,
  etc.: individual idxs of subgroup members

- `events_detected$group_A`, `$group_B`, `$group_C`, etc.: names of
  subgroup members

- `events_detected$n_A`, `$n_B`, `$n_C` etc.: number of individuals in
  each subgroup

- `events_detected$n_big_group`: number of individuals in the big group
  (original group for fissions, subseq group for fusions)

(NOTE: `big_group_idxs`, `big_group`, `group_A_idxs` etc., `group_A`
etc. `n_A` etc. and `n_big_group` are set to NA for shuffles... you can
get more detailed info for shuffles from `all_events_info` object))

`all_events_info` list:

- `all_events_info[[i]]` contains the following info for event i:

- `all_events_info[[i]]$t`: time index of the event

- `all_events_info[[i]]$groups_before`: (list of lists) list of groups
  before the event (at time t)

- `all_events_info[[i]]$groups_after`: (list of lists) list of groups
  after the event (at time t + 1)

- `all_events_info[[i]]event_type`: 'fission', 'fusion', or 'shuffle'
  (character string)

- `all_events_info[[i]]$n_groups_before`: number of groups before the
  event

- `all_events_info[[i]]$n_groups_after`: number of groups after the
  event

`groups_list` list:

- `groups_list[[t]]` gives a list of the subgroups at time t

- `groups_list[[t]][[1]]` gives the vector of the first subgroup, etc.

## Author

Ariana Strandburg-Peshkin (primary author)

Eli Strauss (code reviewer, May 2024)

Reviewed by Brock (Jan 2025)
