#' Get simplified turn and speed influence
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
#' @param heading_type character string specifying heading type - `'spatial'` or `'temporal'`
#' @param idx_breaks vector of indexes to breaks in the data (e.g. breaks between days)
#' @param spatial_R radius to use for spatial headings (if `heading_type = 'spatial'`)
#' @param t_window temporal window to use for temporal headings (if `heading_type = 'temporal'`)
#' @param min_percentile minimum percentile to use for left / right relative speed or distance
#' @param centroid whether to use the group centroid (if `centroid = T`) instead of computing influence for each dyad (if `centroid = F`)
#' @param seconds_per_time_step number of seconds corresponding to each time step
#'
#' @returns Returns an `N x N` matrix of the turn influence of individual `i` (row) on individual `j` (column).
#'
#' @export
#'
get_simplified_turn_and_speed_influence <- function(xs, ys, heading_type, influence_type, idx_breaks = NULL, spatial_R = NULL, t_window = NULL,
                                          min_percentile = NULL, min_left_dist = NULL, min_right_dist = NULL, min_left_speed = NULL, min_right_speed = NULL,
                                          seconds_per_time_step = 1,
                                          verbose = T){

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #check heading_type
  if(!(heading_type %in% c('spatial', 'temporal'))){
    stop('must specify heading_type as spatial or temporal')
  }

  #check influence type
  if(!(influence_type %in% c('movement','position'))){
    stop('must specify influence_type as either movement or position')
  }

  #number of inds and number of times
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #get headings and speeds for all individuals in future and past
  heads_speeds_fut <- list()
  heads_speeds_past <- list()

  if(verbose){print('getting individual heads and speeds')}
  for(i in 1:n_inds){
    if(verbose){print(paste0('ind ',i,'/',n_inds))}
    if(heading_type == 'spatial'){
      heads_speeds_fut[[i]] <- cocomo::get_heading_and_speed_spatial(x_i = xs[i,], y_i = ys[i,], R = spatial_R, forward = T, seconds_per_time_step = seconds_per_time_step)
      heads_speeds_past[[i]] <- cocomo::get_heading_and_speed_spatial(x_i = xs[i,], y_i = ys[i,], R = spatial_R, forward = F, seconds_per_time_step = seconds_per_time_step)
    }
    if(heading_type == 'temporal'){
      heads_speeds_fut[[i]] <- cocomo::get_heading_and_speed_temporal(x_i = xs[i,], y_i = ys[i,], t_window = t_window, forward = T, seconds_per_time_step = seconds_per_time_step)
      heads_speeds_past[[i]] <- cocomo::get_heading_and_speed_temporal(x_i = xs[i,], y_i = ys[i,], t_window = t_window, forward = F, seconds_per_time_step = seconds_per_time_step)
    }
  }

  #for each individual i get...
  # speed_up[i,t] = speed difference of individual i along its past direction of movement between future and past
  # turn_angle[i,t] = turning angle of individual i between future and past
  speed_up <- turn_angle <- matrix(NA, nrow = n_inds, ncol = n_times)
  for(i in 1:n_inds){

    heads_i_fut <- heads_speeds_fut[[i]]$heads
    heads_i_past <- heads_speeds_past[[i]]$heads
    speeds_i_fut <- heads_speeds_fut[[i]]$speeds
    speeds_i_past <- heads_speeds_past[[i]]$speeds

    #get unit vector pointing in direction of past heading of individual i
    dx_i_past_unit <- cos(heads_i_past)
    dy_i_past_unit <- sin(heads_i_past)

    #get dx and dy of individual i in the future (full vectors, not unit vectors) in original reference frame
    dx_i_fut <- speeds_i_fut * cos(heads_i_fut)
    dy_i_fut <- speeds_i_fut * sin(heads_i_fut)

    #project i's future vector on the unit vector of its past heading
    speed_fut_projected <- dx_i_fut * dx_i_past_unit + dy_i_fut * dy_i_past_unit

    #get speed up along past heading and store
    speed_up[i,] <- speed_fut_projected - speeds_i_past

    #get turning angle
    turn_angles_i <- heads_i_fut - heads_i_past
    idxs_over_pi <- which(turn_angles_i >= pi)
    idxs_below_neg_pi <- which(turn_angles_i <= -pi)
    turn_angles_i[idxs_over_pi] <- turn_angles_i[idxs_over_pi] - 2*pi
    turn_angles_i[idxs_below_neg_pi] <- turn_angles_i[idxs_below_neg_pi] + 2*pi
    turn_angle[i,] <- turn_angles_i
  }

  #for each pair (i,j), get...
  # lr_speed[i,j,t] = left-right speed of individual i relative to j's past heading (right = positive)
  # fb_speed_diff[i,j,t] = front-back speed difference of individual i relative to j's past heading and speed (faster = positive)
  # lr_pos[i,j,t] = left-right position of individual i relative to j's current position and past heading (right = positive)
  # fb_pos[i,j,t] = front-back position of individual i relative to j's current position and past heading (front = positive)
  lr_speed <- fb_speed_diff <- array(NA, dim = c(n_inds, n_inds, n_times))
  lr_pos <- fb_pos <- array(NA, dim = c(n_inds, n_inds, n_times))
  for(i in 1:n_inds){
    print(i)
    for(j in 1:n_inds){

      #don't compute individual's influence on themselves
      if(i==j){
        next
      }

      #---Get relevant info for i and j

      #get past speeds and headings for i and j
      heads_i_past <- heads_speeds_past[[i]]$heads
      speeds_i_past <- heads_speeds_past[[i]]$speeds
      heads_j_past <- heads_speeds_past[[j]]$heads
      speeds_j_past <- heads_speeds_past[[j]]$speeds

      #get current locations of individual i and j
      x_i <- xs[i,]
      y_i <- ys[i,]
      x_j <- xs[j,]
      y_j <- ys[j,]

      #----Movement-based turn and speed influence

      #get unit vector of j's past heading in original reference frame
      dx_j_past_unit <- cos(heads_j_past)
      dy_j_past_unit <- sin(heads_j_past)

      #get dx and dy of individual i in the past (full vectors, not unit vectors) in original reference frame
      dx_i_past <- speeds_i_past * cos(heads_i_past)
      dy_i_past <- speeds_i_past * sin(heads_i_past)

      #rotate past speed vector of i into the reference frame defined by the heading of j
      #do this by rotating i's past speed vector by negative theta where theta is the past heading of j
      #using the rotation matrix [cos(theta) -sin(theta);
      #                           sin(theta)  cos(theta)]
      theta <- -heads_j_past
      dx_i_past_rot <- dx_i_past * cos(theta) - dy_i_past * sin(theta)
      dy_i_past_rot <- dx_i_past * sin(theta) + dy_i_past * cos(theta)
      lr_speed[i,j,] <- -dy_i_past_rot #negative because the positive y axis is going left, so now positive values mean going to right
      fb_speed_diff[i,j,] <- dx_i_past_rot - speeds_j_past #speed difference along j's past trajectory heading

      #---Position-based turn and speed influence

      #vector pointing from individual j to individual i in original reference frame
      dx_ji <- x_i - x_j
      dy_ji <- y_i - y_j

      #rotate position into reference frame defined by j's past heading and current position
      dx_ji_rot <- dx_ji * cos(theta) - dy_ji * sin(theta)
      dy_ji_rot <- dx_ji * sin(theta) + dy_ji * cos(theta)
      lr_pos[i,j,] <- -dy_ji_rot #need to take the negative so that right = positive values
      fb_pos[i,j,] <- dx_ji_rot #front-back distance in reference frame defined by j's position and heading

    }

  }

  #get minimum left and right speed thresholds based on min_percentile
  if(!is.null(min_percentile)){
    right_speeds <- lr_speed[which(lr_speed > 0)]
    left_speeds <- lr_speed[which(lr_speed < 0)]
    min_right_speed <- quantile(right_speeds, min_percentile, na.rm=T)
    min_left_speed <- quantile(-left_speeds, min_percentile, na.rm=T)
    if(symmetrize){
      min_lr_speed <- (min_right_speed + min_left_speed) / 2
    }
  }

  #get minimum front-back speed diff thresholds based on min_percentile
  if(!is.null(min_percentile)){
    faster_speed_diffs <- fb_speed_diff[which(fb_speed_diff > 0)]
    slower_speed_diffs <- fb_speed_diff[which(fb_speed_diff < 0)]
    min_faster_speed_diff <- quantile(faster_speed_diffs, min_percentile, na.rm=T)
    min_slower_speed_diff <- quantile(-slower_speed_diffs, min_percentile, na.rm=T)
    #we don't symmetric the front-back speed diffs as this isn't expected to be symmetrical
  }



  #return outputs
  out <- list()
  out$influence_type <- influence_type

  return(out)



}
