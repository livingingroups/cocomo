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
#'
#' @returns Returns the group heading over time, a vector of length `n_times`
#' @export
#'
get_group_heading <- function(xs, ys, heading_type, spatial_R = NULL, t_window = NULL, forward = T, min_inds_tracked = NULL){

  #TODO: Think about what to do if number of tracked individuals changes - should probably have heading = NA at those times

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #check that the required variables exist for computing headings
  if(heading_type %in% c('spatial','temporal')){
    if(heading_type == 'spatial'){
      if(is.null(spatial_R)){
        stop('Must specify spatial_R for spatial headings')
      }
    }
    if(heading_type == 'temporal'){
      if(is.null(t_window)){
        stop('Must specify t_window for temporal headings')
      }
    }
  } else{
    stop('Must specify heading_type as either spatial or temporal')
  }

  #get centroid trajectory
  centr_xy <- cocomo::get_group_centroid(xs = xs, ys = ys, min_inds_tracked = min_inds_tracked)
  x_centr <- centr_xy$x_centr
  y_centr <- centr_xy$y_centr

  #get heading
  if(heading_type == 'temporal'){
    heads <- cocomo::get_headings_temporal(x_i = x_centr, y_i = y_centr, t_window = t_window, forward = forward)
  } else{
    heads <- cocomo::get_headings_spatial(x_i = x_centr, y_i = y_centr, R = spatial_R, forward = forward )
  }

  return(heads)

}
