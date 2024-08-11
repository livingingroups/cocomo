#'Get group polarization
#'
#'Computes the polarization of the group at each time step `t`.
#'The polarization is a measure of how aligned the group is, ranging from 0 (completely unaligned) to 1 (completely aligned)
#'The polarization is defined by adding up (vector addition) all of the heading vectors of all individuals at a given moment in time,
#'taking the length of the resultant vector, and dividing this by the number of individuals.
#'Either spatial or temporal headings can be used.
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
#' @param min_inds_tracked if specified, sets a minimum number of individuals that must be tracked at any moment in time to compute headings (otherwise the polarization will be NA at that time point
#'
#' @returns Returns a vector of length `n_times` giving the polarization of the group over time
#' @export
get_group_polarization <- function(xs, ys, heading_type, spatial_R = NULL, t_window = NULL, forward = T, min_inds_tracked = NULL){

  #throw warning about lack of code review
  warning('This function has not yet been code reviewed - if you would like to review it, contact Ari!')

  #check that the required variable exist
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

  #get number of individuals
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #check matrix dimensions
  if(nrow(ys) != n_inds || ncol(ys) != n_times){
    stop('xs and ys matrices must have same dimensions')
  }

  #get headings for all individuals
  heads <- matrix(NA, nrow = n_inds, ncol = n_times)
  for(i in 1:n_inds){
    if(heading_type == 'temporal'){
      heads[i,] <- cocomo::get_headings_temporal(x_i = xs[i,], y_i = ys[i,], t_window = t_window, forward = forward)
    }
    if(heading_type == 'spatial'){
      heads[i,] <- cocomo::get_headings_spatial(x_i = xs[i,], y_i = ys[i,], R = spatial_R, forward = forward)
    }
  }

  #get x and y components of headings
  heads_x <- cos(heads)
  heads_y <- sin(heads)

  #add up heading vectors for each moment in time (column)
  heads_x_tot <- colSums(heads_x, na.rm=T)
  heads_y_tot <- colSums(heads_y, na.rm=T)

  #get length of the resultant vector
  resultant_vec_len <- sqrt(heads_x_tot^2 + heads_y_tot^2)

  #divide by the number of individuals tracked (n_tracked) to get the polarization at each moment in time
  n_tracked <- colSums(!is.na(xs))
  polarization <- resultant_vec_len / n_tracked

  #if min_inds_tracked is specified, put in NAs where the number of individuals tracked is below this value
  too_few_inds <- which(n_tracked < min_inds_tracked)
  polarization[too_few_inds] <- NA

  #output polarization over time
  return(polarization)

}
