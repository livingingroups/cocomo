#' Get group heading
#'
#' Computes the group centroid heading over time using either temporal or spatial headings, and
#' going either forward (into the future) or backward (into the past) when computing the heading
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' @param heading_type character string specifying heading type - `'spatial'` or `'temporal'`
#' @param spatial_R radius to use for spatial headings (if `heading_type = 'spatial'`)
#' @param t_window temporal window to use for temporal headings (if `heading_type = 'temporal'`)
#' @param forward whether to compute headings into the future (`forward = T`) or the past (`forward = F`)
#' @param min_inds_tracked if specified, sets a minimum number of individuals that must be tracked at any moment in time to compute heading (otherwise the heading will be NA at that time point
#' @param seconds_per_time_step number of seconds corresponding to each time step
#'
#' @returns Returns the group heading over time, a vector of length `n_times`
#' @export
#'
get_group_heading_and_speed <- function(xs, ys, heading_type, spatial_R = NULL, t_window = NULL, forward = T, min_inds_tracked = NULL, seconds_per_time_step = 1){
  checkmate::assert_matrix(xs, 'numeric')
  checkmate::assert_matrix(ys, 'numeric')
  checkmate::assert_subset(heading_type, c('spatial', 'temporal'), empty.ok = FALSE)
  if(heading_type == 'spatial'){
    checkmate::assert_number(spatial_R)
    if(!is.null(t_window)) warning('heading_type is set to spatial so t_window argument is ignored')
  } else {
    if(!is.null(spatial_R)) warning('heading_type is set to temporal so spatial_R argument is ignored')
    checkmate::assert_int(t_window, lower = 1, upper = ncol(x_i))
  }
  checkmate::assert_logic(forward)
  checkmate::assert_int(min_inds_tracked, lower=0, upper=nrow(xs))
  checkmate::assert_number(seconds_per_time_step, lower = 0)


   #TODO: Think about what to do if number of tracked individuals changes - should probably have heading = NA at those times

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #get centroid trajectory
  centr_xy <- cocomo::get_group_centroid(xs = xs, ys = ys, min_inds_tracked = min_inds_tracked)
  x_centr <- centr_xy$x_centr
  y_centr <- centr_xy$y_centr

  #get heading
  if(heading_type == 'temporal'){
    heads_speeds <- cocomo::get_heading_and_speed_temporal(x_i = x_centr, y_i = y_centr, t_window = t_window, forward = forward, seconds_per_time_step = seconds_per_time_step)
  } else {
    heads_speeds <- cocomo::get_heading_and_speed_spatial(x_i = x_centr, y_i = y_centr, R = spatial_R, forward = forward, seconds_per_time_step = seconds_per_time_step )
  }

  return(heads_speeds)

}
