#' Get local density
#'
#' Gets the number of individuals within a radius `R` of a focal individual `i` at each time point
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param	xs `N x n_times` matrix giving x positions of all `N` individuals over `n_times` timesteps
#' @param	ys `N x n.times` matrix giving y positions of all `N` individuals over `n_times` timesteps
#' @param	i the focal individual (for whom the local density will be computed)
#' @param	R the radius over which to compute the local density (in same units as `xs` and `ys`)
#' @returns Vector of length `n_times` giving the local density around the focal individual (number of individuals within a radius R) at each time point
#' @export
get_local_density <- function(xs, ys, i, R){

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

  #find the number of individuals withiin a distance R at each time point (column)
  local_density <- colSums(dists_mat <= R,na.rm=T)

  return(local_density)

}
