#' Get nearest neighbor distance
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Gets the distance to an individual `i`'s nearest neighbor over time
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x positions (or UTM eastings) of all `N` individuals over `n_times` timesteps
#' @param	ys `N x n_times` matrix giving y positions (or UTM northings) of all `N` individuals over `n_times` timesteps
#' @param	i index of the focal individual (whose nearest neighbor distance will be computed)
#'
#' @return Returns a vector of nearest neighbor distances at each time point (length `= n_times`)
#' @export
get_nearest_neighbor_distance <- function(xs, ys, i){

  #get the position of individual i over time
  xs_i <- xs[i,]
  ys_i <- ys[i,]

  #number of timesteps and number of individuals
  N <- nrow(xs)
  n_times <- ncol(xs)

  #convert these positions to matrices (which we will subtract in the next step to get distances to all individuals)
  xs_i_mat <- matrix(rep(xs_i,each=N),nrow=N,ncol=n_times)
  ys_i_mat <- matrix(rep(ys_i,each=N),nrow=N,ncol=n_times)

  #subtract to get distances in x and y dimensions between focal individual and all other individuals
  dx_mat <- xs - xs_i_mat
  dy_mat <- ys - ys_i_mat

  #get total distance between focal and all other individuals
  dists_mat <- sqrt( dx_mat^2 + dy_mat^2 )

  #distance between a focal individual and itself will be 0, so set this to NA
  dists_mat[i,] <- NA

  #get indexes of columns that are not all NAs
  idxs_non_na_cols <- which(colSums(!is.na(dists_mat))>0)

  #create matrix to hold output
  nn_dists <- rep(NA, ncol(dists_mat))

  #find the minimum distance at each time (column) - this is the nearest neighbor distance
  #if there are no tracked individuals, return NA for that time point
  nn_dists[idxs_non_na_cols] <- apply(dists_mat[,idxs_non_na_cols],MARGIN=2,FUN=min,na.rm=T)

  return(nn_dists)

}
