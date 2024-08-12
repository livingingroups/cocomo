#' Get simplified turn influence
#'
#' Get the simplified turn influence between all pairs of individuals
#' `i` and `j`, or, if `centroid = T` for all indiividuals `i` on the group centroid.
#' The `simplified movement turn influence` of individual `i` on individual `j`
#' is defined as the probability that `j` turns right in the future given that `i` moved
#' right relative to `j`'s past heading at a speed above a threshold value
#' `min_right_speed` or left relative to `j`'s past heading at a speed above a threshold
#' value `min_left_speed`. This will be computed if `influence_type` is set to `'movement'`.
#' The `simplified positional turn influence` of individual `i` on individual `j`
#' is defined as the probability that `j` turns right in the future given that `i` was located
#' at least `min_right_dist` meters right of individual `j` relative to `j`'s current position
#' and heading or the probability that `j` turns left in the future given that `i` was located
#' at least `min_left_dist` meters left of individual `j` relative to `j`'s current position
#' and heading. This will be computed if `influence_type` is set to `position`.
#' The parameters `min_right_dist` and `min_left_dist`, or `min_right_speed` and
#' `min_left_speed` are either set manually or, if `min_percentile` is specified, based on percentiles
#' of the left-right relative speed and front-back relatives speed distributions across all dyads
#' (or across all individuals relative to the group centroid, if `centroid = T`). The variable
#' `min_percentile` overrides the manually set minimum speeds / distances.
#'
#' The minimum speeds (or distances) are computed as follows. First, split the distribution of relative left-right
#' speeds (or distances) across all dyads into those greater than and those less than 0. Then, compute the quantile
#' `min_percentile` for each distribution separately. Finally, set `min_left_speed` and `min_right_speed` at the computed
#' values. If `symmetrize = T`, take instead the mean absolute value of the quantiles for both distributions and use it
#' for both thresholds (this will result in using the same relative speed when going right or left as a threshold - otherwise these could be different if `symmetrize = F`)
#'
#' The same can be done for the relative left and right distance distributions to set
#' `min_left_dist` and `min_right_dist` respectively, if `influence_type = 'position'`
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' @param idx_breaks vector of indexes to breaks in the data (e.g. breaks between days)
#' @param heading_type character string specifying heading type - `'spatial'` or `'temporal'`
#' @param influence_type character string specifying what type of turn influence to compute - `'position` or `'movement'`
#' @param spatial_R radius to use for spatial headings (if `heading_type = 'spatial'`)
#' @param t_window temporal window to use for temporal headings (if `heading_type = 'temporal'`)
#' @param min_percentile minimum percentile to use for left / right relative speed or distance
#' @param centroid whether to use the group centroid (if `centroid = T`) instead of computing influence for each dyad (if `centroid = F`)
#'
#' @returns Returns an `N x N` matrix of the turn influence of individual `i` (row) on individual `j` (column).
#'
#' @export
#'
get_simplified_turn_influence <- function(xs, ys, idx_breaks, heading_type, influence_type, spatial_R = NULL, t_window = NULL,
                                          min_percentile = NULL, min_left_dist = NULL, min_right_dist = NULL, min_left_speed = NULL, min_right_speed = NULL){


  i <- 1
  j <- 2
  d <- 1

  #get indexes to breaks in the data - add the last timepoint + 1 to the end
  idx_breaks <- c(idx_breaks, ncol(xs)+1)
  for(d in 1:(length(idx_breaks)-1)){

    #get times associated with that chunk
    t_idxs <- idx_breaks[d]:idx_breaks[d+1]

    #get trajectory data of i and j for that chunk of time
    x_i <- xs[i,t_idxs]
    y_i <- ys[i,t_idxs]
    x_j <- xs[j,t_idxs]
    y_j <- ys[j,t_idxs]

    #get past and future headings for i and j
    if(heading_type == 'temporal'){
      heads_i_fut <- get_headings_temporal(x_i, y_i, t_window = t_window, forward = T)
      heads_j_fut <- get_headings_temporal(x_j, y_j, t_window = t_window, forward = T)
      heads_i_past <- get_headings_temporal(x_i, y_i, t_window = t_window, forward = F)
      heads_j_past <- get_headings_temporal(x_j, y_j, t_window = t_window, forward = F)
    }

  }






  #return outputs
  out <- list()
  out$influence_type <- influence_type

  return(out)



}
