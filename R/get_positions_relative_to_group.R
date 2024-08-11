#'Get positions relative to group
#'
#'Get individual positions relative to the group centroid and heading.
#'The group centroid is defined as the origin (0,0) and the heading points along the positive x axis.
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
#' @param min_inds_tracked if specified, sets a minimum number of individuals that must be tracked at any moment in time to compute heading (otherwise all positions will be NA at that time point
#'
#' @returns Returns a list containing rel_xs and rel_ys matrices of each individual's position relative to the group centroid and heading
#' @export
get_positions_relative_to_group <- function(xs, ys, heading_type, spatial_R = NULL, t_window = NULL, forward = T, min_inds_tracked = NULL){

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

  #get centroid headings
  if(heading_type == 'temporal'){
    heads_centr <- cocomo::get_headings_temporal(x_i = x_centr, y_i = y_centr, t_window = t_window, forward = forward)
  } else{
    heads_centr <- cocomo::get_headings_spatial(x_i = x_centr, y_i = y_centr, R = spatial_R, forward = forward )
  }

  #get number of individuals and number of times
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #get x and y positions of all individuals relative to the group centroid, not yet rotated
  x_centr_mat <- matrix(rep(x_centr,each=n_inds),nrow=n_inds,ncol=n_times)
  y_centr_mat <- matrix(rep(y_centr,each=n_inds),nrow=n_inds,ncol=n_times)
  dxs <- xs - x_centr_mat
  dys <- ys - y_centr_mat

  #rotate relative x and y positions by negative centr_heads, where centr_heads is the group heading
  #this uses the rotation matrix to rotate a vector by angle theta:
  #R_rot = [cos(theta), -sin(theta);
  #         sin(theta), cos(theta)]
  heads_centr_mat <- matrix(rep(heads_centr,each=n_inds),nrow=n_inds,ncol=n_times)
  xs_rel <- cos(-heads_centr_mat)*dxs - sin(-heads_centr_mat)*dys
  ys_rel <- sin(-heads_centr_mat)*dxs + cos(-heads_centr_mat)*dys

  #prepare output
  out <- list()
  out$xs_rel <- xs_rel
  out$ys_rel <- ys_rel

  #return output
  return(out)

}
