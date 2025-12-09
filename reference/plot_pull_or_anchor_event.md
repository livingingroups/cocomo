# Plot pull or anchor event

**\[experimental\]**

Plots a pull or anchor event, with the initiator colored blue and the
responder colored red. Panels show the positions at `t1`,`t2`, and `t3`
respectively.

## Usage

``` r
plot_pull_or_anchor_event(xs, ys, events, event_idx, ids = NULL)
```

## Arguments

- xs:

  `n_inds x n_times` matrix of x positions

- ys:

  `n_inds x n_times` matrix of y positions

- events:

  events table output from `get_pulls_and_anchors`

- event_idx:

  row in the table to plot

- ids:

  ids table output from `reformat_movebank_to_matrix`

## Value

Returns a plot of the event with the initiator in blue and the responder
in red

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
