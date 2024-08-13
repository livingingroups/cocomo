#' Get simplified turn and speed influence
#'
#' Get the simplified turn and speed influence (movement and positional) between all pairs of individuals
#' `i` and `j`, or, if `centroid = T` for all indiividuals `i` on the group centroid excluding `i`.
#'
#' The simplified movement turn influence of individual `i` on individual `j` (`turn_influence_movement[i,j]`)
#' is defined as the probability that `j` turns right in the future given that `i` moved
#' right relative to `j`'s past heading at a speed above a threshold value
#' `min_right_speed` or left relative to `j`'s past heading at a speed above a threshold
#' value `min_left_speed`.
#'
#' The simplified positional turn influence of individual `i` on individual `j` (`turn_influence_position[i,j]`)
#' is defined as the probability that `j` turns right in the future given that `i` was located
#' at least `min_right_dist` meters right of individual `j` relative to `j`'s current position
#' and heading or the probability that `j` turns left in the future given that `i` was located
#' at least `min_left_dist` meters left of individual `j` relative to `j`'s current position
#' and heading.
#'
#' The simplified movement speed influence of individual `i` on individual `j` (`speed_influence_movement[i,j]`)
#' is defined as the probability that `j` speeds up in the future along the vector defined by `j`'s heading in the past,
#' given that `i` was moving at least a speed `min_faster_speed_diff` faster than `j` along `j`'s past heading direction, or
#' slows down along the vector defined by `j`'s heading in the past given that `j` was moving at least a speed `min_slower_speed_diff`
#' slower than `i` along the same vector in the past.
#'
#' The simplified positional speed influence of individual `i` on individual `j` (`speed_influence_position[i,j]`)
#' is defined as the probability that `j` speeds up in the future along the vector defined by `j`'s heading in the past
#' given that `i` was in front of `j` by at least `min_front_pos` meters relative to `j`'s current position and past heading,
#' or that `j` slows down in the future along the vector defined by `j`'s heading in the past given that `i` was
#' behind `j` by at least `min_back_pos` relative to `j`'s current position and past heading.
#'
#' Future headings and speeds are computed either using a fixed temporal window `t_window` forward in time (if `heading_type = 'temporal'`)
#' or based on spatial discretization with a radius `spatial_R` (if `heading_type = 'spatial'`), i.e. defining the heading based on the
#' when the focal individual has moved a distance `spatial_R` from its current location. Similarly, past headings and speeds
#' are computed by looking backward in time either at a fixed temporal window (`t_window`) or based on a fixed displacement distance (`spatial_R`).
#'
#' The parameters specifying minimum speeds and distances left-right and front-back are either set manually (not recommended) or,
#' if `min_percentile` is specified, based on percentiles of the relevant distributions of these variables across all dyads
#' (or across all individuals relative to the group centroid, if `centroid = T`). The variable
#' `min_percentile` overrides the manually set minimum speeds / distances.
#' The minimum speeds (or distances) are computed as follows. First, split the distribution of relative left-right
#' speeds (for example) across all dyads into those greater than and those less than 0. Take the absolute value of all speeds to
#' define a positive speed for both directions. Next, compute the quantile `min_percentile` for each distribution (left and right) separately.
#' Finally, set `min_left_speed` and `min_right_speed` at the computed
#' values. If `symmetrize_lr = T` (recommended), take instead the mean value of the quantiles for both distributions and use it
#' for both thresholds (this will result in using the same relative speed when going right or left as a threshold - otherwise these could be different if `symmetrize_lr = F`)
#' The same procedure is done for left-right positions, front-back relative speeds, and front-back positions, using the same value of `min_percentile`. In the case of
#' speeds, no symmetrization is done because speed differences and front-back distances are not expected to be symmetrical.
#'
#' If `idx_breaks` is set, headings that go across breaks in the data as specified by this vector are not included in the computations.
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
#' @param symmetrize_lr whether to symmetrize the thresholds for right and left movement + position (default `T`)
#' @param min_left_speed minimum speed when i moving to the left to include data (positive value)
#' @param min_right_speed minimum speed when i moving to the right to include data (positive value)
#' @param min_left_pos minimum distance of i to the left of j to include data (positive value)
#' @param min_right_pos minimum distance of i to the right to j include data (positive value)
#' @param min_faster_speed_diff minimum speed difference between i and j when i is going faster to include data (positive value)
#' @param min_slower_speed_diff minimum speed difference between i and j when i is going slower to include data (positive value)
#' @param min_front_pos minimum position toward the front when i is in front of j to include data (positive value)
#' @param min_back_pos minimum position toward the back when i is in back of j to include data (positive value)
#' @param verbose whether the print progress (if `T`)
#' @param output_details whether to output computed matrices and arrays used in influence computations (if `T`)
#'
#' @returns Returns a list containing matrices for the 4 different types of influence, as well as other computed metrics (if `output_details = T`).
#' See "Additional information about returned objects" section below for a list of outputs.
#'
#' @section Additional information about returned objects
#'
#' The output list conatins the following objects:
#'
#' `params` list of parameters used in the calculation (see input variables for descriptions)
#' `turn_influence_movement`: `i x j` matrix representing movement turn influence of individual `i` on individual `j`
#' `turn_influence_position`: `i x j` matrix representing positional turn influence of individual `i` on individual `j`
#' `speed_influence_movement`: `i x j` matrix representing movement speed influence of individual `i` on individual `j`
#' `speed_influence_position`: `i x j` matrix representing positions speed influence of individual `i` on individual `j`
#'
#' If `centroid = T`, everything is instead computed at the centroid level (with the centroid as follower, replacing individual `j`).
#' In this case, the output matrices will still be the same shape as those for the dyadic case, but they will have `NA`s for all columns
#' except `j=1` (representing the influence of each individual `i` on the centroid excluding that individual).
#' This parallel structure is for compatibility between dyadic and centroid-based analyses.
#'
#' If `output_details = T`, some additional parameters and the following computed matrices / arrays will also be outputted:
#' `lr_speed`: `i x j x t` array of the past left-right speed of individual `i` relative to the past heading of individual `j` at time `t`
#' `lr_pos`: `i x j x t` array of the past left-right position of individual `i` relative to the current position and past heading of individual `j` at time `t`
#' `fb_speed`: `i x j x t` array of the past front-back speed difference between individual `i` and individual `j` along the vector of the past heading of individual `j` at time `t`
#' `fb_pos`: `i x j x t` array of the past front-back position of individual `i` relative to the current position and past heading of individual `j` at time `t`
#' `turn_angle`: `j x t` matrix of the turning angle of individual `j` between the past and future at time `t`
#' `speed_up`: `j x t` matrix of the difference in speed of individual `j` between future and past at time `t`, project along `j`'s own past heading
#'
#' The output matrices `turn_angle[j,t]` and `speed_up[j,t]` then represent the centroid excluding individual `j`, rather than
#' the data for individual `j` as in the dyadic case.
#' Similarly, the output arrays with dimensions `i x j x t` will have `NA`s for all values of `j > 1`, and the `j = 1`
#' values represent the speeds / positions of individuals relative to the centroid (excluding `i`)
#'
#' @export
#'
get_turn_and_speed_influence_simplified <- function(xs, ys, heading_type, centroid = F,
                                                    breaks = NULL, spatial_R = NULL, t_window = NULL,
                                                    min_percentile = 0.5, seconds_per_time_step = 1, symmetrize_lr = T,
                                                    min_left_speed = NULL, min_right_speed = NULL,
                                                    min_left_pos = NULL, min_right_pos = NULL,
                                                    min_faster_speed_diff = NULL, min_slower_speed_diff = NULL,
                                                    min_front_pos = NULL, min_back_pos = NULL,
                                                    verbose = T, output_details = T){

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #check heading_type
  if(!(heading_type %in% c('spatial', 'temporal'))){
    stop('must specify heading_type as spatial or temporal')
  }

  #number of inds and number of times
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #get headings and speeds for all individuals in future and past
  heads_speeds_fut <- list()
  heads_speeds_past <- list()

  if(centroid){
    centr_heads_speeds_fut <- list()
    centr_heads_speeds_past <- list()
  }

  #set idx_breaks, if not yet set
  if(is.null(breaks)){
    breaks <- seq(1, n_times + 1)
  }

  #add end point to breaks if needed
  if(breaks[length(breaks)] < n_times){
    breaks <- c(breaks, n_times + 1)
  }

  if(verbose){print('getting individual headings and speeds')}

  #initialized heads_speeds object to use for each individual
  heads_speeds_init <- list()
  heads_speeds_init$heads <- rep(NA, n_times)
  heads_speeds_init$speeds <- rep(NA, n_times)
  heads_speeds_init$dts <- rep(NA, n_times)

  for(i in 1:n_inds){

    if(verbose){print(paste0('ind ',i,'/',n_inds))}

    heads_speeds_fut[[i]] <- heads_speeds_init
    heads_speeds_past[[i]] <- heads_speeds_init

    #if using the group centroid, have to get headings and speeds for that as well (excluding the focal individual)
    #centr_heads_speeds_fut[[i]] will hold the heading and speed info for the centroid excluding individual i
    if(centroid){
      centr_heads_speeds_fut[[i]] <- heads_speeds_init
      centr_heads_speeds_past[[i]] <- heads_speeds_init
      centr_x_without_i <- colMeans(xs[-i,],na.rm=T)
      centr_y_without_i <- colMeans(ys[-i,],na.rm=T)
    }

    for(b in 1:(length(breaks)-1)){
      time_idxs <- seq(breaks[b], breaks[b+1]-1, 1)

      if(heading_type == 'spatial'){
        out_fut <- cocomo::get_heading_and_speed_spatial(x_i = xs[i,time_idxs], y_i = ys[i,time_idxs], R = spatial_R, forward = T, seconds_per_time_step = seconds_per_time_step)
        out_past <- cocomo::get_heading_and_speed_spatial(x_i = xs[i,time_idxs], y_i = ys[i,time_idxs], R = spatial_R, forward = F, seconds_per_time_step = seconds_per_time_step)

        #get data for the group centroid without i
        if(centroid){
          centr_out_fut <- cocomo::get_heading_and_speed_spatial(x_i = centr_x_without_i[time_idxs], y_i = centr_y_without_i[time_idxs], R = spatial_R, forward = T, seconds_per_time_step = seconds_per_time_step)
          centr_out_past <- cocomo::get_heading_and_speed_spatial(x_i = centr_x_without_i[time_idxs], y_i = centr_y_without_i[time_idxs], R = spatial_R, forward = F, seconds_per_time_step = seconds_per_time_step)
        }

      }
      if(heading_type == 'temporal'){
        out_fut <- cocomo::get_heading_and_speed_temporal(x_i = xs[i,time_idxs], y_i = ys[i,time_idxs], t_window = t_window, forward = T, seconds_per_time_step = seconds_per_time_step)
        out_past <- cocomo::get_heading_and_speed_temporal(x_i = xs[i,time_idxs], y_i = ys[i,time_idxs], t_window = t_window, forward = F, seconds_per_time_step = seconds_per_time_step)

        #get data for the group centroid without i
        if(centroid){
          centr_out_fut <- cocomo::get_heading_and_speed_temporal(x_i = centr_x_without_i[time_idxs], y_i = centr_y_without_i[time_idxs], t_window = t_window, forward = T, seconds_per_time_step = seconds_per_time_step)
          centr_out_past <- cocomo::get_heading_and_speed_temporal(x_i = centr_x_without_i[time_idxs], y_i = centr_y_without_i[time_idxs], t_window = t_window, forward = F, seconds_per_time_step = seconds_per_time_step)
        }

      }

      #store output
      heads_speeds_fut[[i]]$heads[time_idxs] <- out_fut$heads
      heads_speeds_fut[[i]]$speeds[time_idxs] <- out_fut$speeds
      heads_speeds_fut[[i]]$dts[time_idxs] <- out_fut$dts
      heads_speeds_past[[i]]$heads[time_idxs] <- out_past$heads
      heads_speeds_past[[i]]$speeds[time_idxs] <- out_past$speeds
      heads_speeds_past[[i]]$dts[time_idxs] <- out_past$dts

      if(centroid){
        centr_heads_speeds_fut[[i]]$heads[time_idxs] <- centr_out_fut$heads
        centr_heads_speeds_fut[[i]]$speeds[time_idxs] <- centr_out_fut$speeds
        centr_heads_speeds_fut[[i]]$dts[time_idxs] <- centr_out_fut$dts
        centr_heads_speeds_past[[i]]$heads[time_idxs] <- centr_out_past$heads
        centr_heads_speeds_past[[i]]$speeds[time_idxs] <- centr_out_past$speeds
        centr_heads_speeds_past[[i]]$dts[time_idxs] <- centr_out_past$dts
      }

    }
  }

  #for each individual i get...
  # speed_up[i,t] = speed difference of individual i along its past direction of movement between future and past
  # turn_angle[i,t] = turning angle of individual i between future and past
  # if working with the group centroid, instead get speed_up and turn_angle for centroid excluding individual i
  speed_up <- turn_angle <- matrix(NA, nrow = n_inds, ncol = n_times)
  if(verbose){print('getting speed differences and turning angles for all individuals')}
  for(i in 1:n_inds){

    #in the normal case (centroid = F), heads_i_fut represents the headings of individual i
    if(!centroid){
      heads_i_fut <- heads_speeds_fut[[i]]$heads
      heads_i_past <- heads_speeds_past[[i]]$heads
      speeds_i_fut <- heads_speeds_fut[[i]]$speeds
      speeds_i_past <- heads_speeds_past[[i]]$speeds
    }

    #in the centroid case (centroid = T), heads_i_fut represents the headings of the centroid excluding individual i
    if(centroid){
      heads_i_fut <- centr_heads_speeds_fut[[i]]$heads
      heads_i_past <- centr_heads_speeds_past[[i]]$heads
      speeds_i_fut <- centr_heads_speeds_fut[[i]]$speeds
      speeds_i_past <- centr_heads_speeds_past[[i]]$speeds
    }

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

  #i is the leader and j is the follower
  #for each pair (i,j), get...
  # lr_speed[i,j,t] = left-right speed of individual i relative to j's past heading (right = positive)
  # fb_speed_diff[i,j,t] = front-back speed difference of individual i relative to j's past heading and speed (faster = positive)
  # lr_pos[i,j,t] = left-right position of individual i relative to j's current position and past heading (right = positive)
  # fb_pos[i,j,t] = front-back position of individual i relative to j's current position and past heading (front = positive)
  lr_speed <- fb_speed_diff <- array(NA, dim = c(n_inds, n_inds, n_times))
  lr_pos <- fb_pos <- array(NA, dim = c(n_inds, n_inds, n_times))
  if(verbose){print('getting left-right speeds / positions, and front-back speeds / positions for all dyads')}
  for(i in 1:n_inds){
    for(j in 1:n_inds){

      #In the normal (non-centroid) case, get info for i and j
      if(!centroid){
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
      }

      #in the non-centroid case, get info for i and the centroid (call the centroid j in this case, for consistency with the dyadic version)
      if(centroid){

        #only need to do this once per individual i, so skip j > 1
        if(j > 1){
          next
        }

        #---Get relevant info for i and j

        #get past speeds and headings for i and j (j is the centroid in this case)
        heads_i_past <- heads_speeds_past[[i]]$heads
        speeds_i_past <- heads_speeds_past[[i]]$speeds
        heads_j_past <- centr_heads_speeds_past[[i]]$heads
        speeds_j_past <- centr_heads_speeds_past[[i]]$speeds

        #get current locations of individual i and j (j is in the centroid in this case)
        x_i <- xs[i,]
        y_i <- ys[i,]
        x_j <- colMeans(xs[-i,], na.rm=T)
        y_j <- colMeans(ys[-i,], na.rm=T)
      }

      #----Movement-based metrics (left-right speed and front-back speed difference)

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
      fb_speed_diff[i,j,] <- dx_i_past_rot - speeds_j_past #speed difference between i and j along j's past trajectory heading

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


  #----Minimum thresholds

  if(verbose){print('getting thresholds from percentiles (if min_percentile=T)')}

  #get minimum left and right speed thresholds based on min_percentile
  if(!is.null(min_percentile)){
    right_speeds <- lr_speed[which(lr_speed > 0)]
    left_speeds <- lr_speed[which(lr_speed < 0)]
    min_right_speed <- quantile(right_speeds, min_percentile, na.rm=T)
    min_left_speed <- quantile(-left_speeds, min_percentile, na.rm=T)
    if(symmetrize_lr){
      min_lr_speed <- (min_right_speed + min_left_speed) / 2
      min_right_speed <- min_left_speed <- min_lr_speed
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

  #get minimum left and right position thresholds based on min_percentile
  if(!is.null(min_percentile)){
    right_pos <- lr_pos[which(lr_pos > 0)]
    left_pos <- lr_pos[which(lr_pos < 0)]
    min_right_pos <- quantile(right_pos, min_percentile, na.rm=T)
    min_left_pos <- quantile(-left_pos, min_percentile, na.rm=T)
    if(symmetrize_lr){
      min_lr_pos <- (min_right_pos + min_left_pos) / 2
      min_right_pos <- min_left_pos <- min_lr_pos
    }
  }

  #get minimum front and back position thresholds based on min_percentile
  if(!is.null(min_percentile)){
    front_pos <- fb_pos[which(fb_pos > 0)]
    back_pos <- fb_pos[which(fb_pos < 0)]
    min_front_pos <- quantile(front_pos, min_percentile, na.rm=T)
    min_back_pos <- quantile(-back_pos, min_percentile, na.rm=T)
  }

  #----Turn and speed influence for all pairs
  if(verbose){print('getting turn and speed influence for all dyads (or for all inds and the centroid, if centroid = T)')}
  turn_influence_movement <- turn_influence_position <- matrix(NA, nrow = n_inds, ncol = n_inds)
  speed_influence_movement <- speed_influence_position <- matrix(NA, nrow = n_inds, ncol = n_inds)
  for(i in 1:n_inds){
    for(j in 1:n_inds){

      #don't compute for i = j (for dyadic version)
      if(!centroid & i==j){
        next
      }

      #skip everything above j = 1 (for centroid version) because this is empty in that case
      if(centroid & j > 1){
        next
      }

      #turn influence - position
      num <- sum(((lr_pos[i,j,] > min_right_pos) & (turn_angle[j,] < 0)) |
        ((lr_pos[i,j,] < -min_left_pos) & (turn_angle[j,] > 0)), na.rm=T)
      denom <- sum(((lr_pos[i,j,] > min_right_pos) & (turn_angle[j,] != 0)) |
                     ((lr_pos[i,j,] < -min_left_pos) & (turn_angle[j,] != 0)), na.rm=T)
      turn_influence_position[i,j] <- num / denom

      #turn influence - movement
      num <- sum(((lr_speed[i,j,] > min_right_speed) & (turn_angle[j,] < 0)) |
                   ((lr_speed[i,j,] < -min_left_speed) & (turn_angle[j,] > 0)), na.rm=T)
      denom <- sum(((lr_speed[i,j,] > min_right_speed) & (turn_angle[j,] != 0)) |
                     ((lr_speed[i,j,] < -min_left_speed) & (turn_angle[j,] != 0)), na.rm=T)
      turn_influence_movement[i,j] <- num / denom

      #speed influence - position
      num <- sum(((fb_pos[i,j,] > min_front_pos) & (speed_up[j,] > 0)) |
                   ((fb_pos[i,j,] < -min_back_pos) & (speed_up[j,] < 0)), na.rm=T)
      denom <- sum(((fb_pos[i,j,] > min_front_pos) & (speed_up[j,] != 0)) |
                   ((fb_pos[i,j,] < -min_back_pos) & (speed_up[j,] != 0)), na.rm=T)
      speed_influence_position[i,j] <- num / denom

      #speed influence - movement
      num <- sum(((fb_speed_diff[i,j,] > min_faster_speed_diff) & (speed_up[j,] > 0)) |
                   ((fb_speed_diff[i,j,] < -min_slower_speed_diff) & (speed_up[j,] < 0)), na.rm=T)
      denom <- sum(((fb_speed_diff[i,j,] > min_faster_speed_diff) & (speed_up[j,] != 0)) |
                     ((fb_speed_diff[i,j,] < -min_slower_speed_diff) & (speed_up[j,] != 0)), na.rm=T)
      speed_influence_movement[i,j] <- num / denom

    }
  }

  #-----Outputs---
  if(verbose){print('constructing output list')}
  out <- list()

  out$params <- list()
  out$params$heading_type <- heading_type
  out$params$spatial_R <- spatial_R
  out$params$t_window <- t_window
  out$params$centroid <- centroid
  out$params$min_percentile <- min_percentile

  out$turn_influence_movement <- turn_influence_movement
  out$turn_influence_position <- turn_influence_position
  out$speed_influence_movement <- speed_influence_movement
  out$speed_influence_position <- speed_influence_position

  if(output_details){
    out$lr_speed <- lr_speed
    out$lr_pos <- lr_pos
    out$fb_speed_diff <- fb_speed_diff
    out$fb_pos <- fb_pos
    out$turn_angle <- turn_angle
    out$speed_up <- speed_up

    out$params$min_left_speed <- min_left_speed
    out$params$min_right_speed <- min_right_speed
    out$params$min_left_pos <- min_left_pos
    out$params$min_right_pos <- min_right_pos
    out$params$min_faster_speed_diff <- min_faster_speed_diff
    out$params$min_slower_speed_diff <- min_slower_speed_diff
    out$params$min_front_pos <- min_front_pos
    out$params$min_back_pos <- min_back_pos
    out$params$seconds_per_time_step <- seconds_per_time_step
    out$params$symmetrize_lr <- symmetrize_lr

  }

  print('done')

  return(out)

}
