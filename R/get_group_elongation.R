#'Get group polarization
#'
#'Computes the elongation of the group at each time step `t` as well as the long axis vector.
#'The elongation is a measure of how "line-link" vs "circle-like" the group is, ranging from 0 (completely circular) to 1 (completely elongated)
#'The elongation is defined by first performing a PCA on the x/y data of the group. The axis of greatest variation is defined
#'as the 'long axis' and the axis perpendicular to this is defined as the 'short axis'. The elonation is then
#'defined as 1 - (length of short axis / length of long axis).
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' @param min_inds_tracked if specified, sets a minimum number of individuals that must be tracked at any moment in time to compute elongation (otherwise the elongation will be NA at that time point). Default is 3 (below this, elongation is generally undefined)
#'
#' @returns Returns a list containing:
#' `out$elongation`: a vector of length `n_times` giving the elongation of the group over time
#' `out$long_axis_angle`: a vector of length `n_times` giving the angle of the long axis over time
#' @export
get_group_elongation <- function(xs, ys, min_inds_tracked = 3){

  #get number of individuals
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #check matrix dimensions
  if(nrow(ys) != n_inds || ncol(ys) != n_times){
    stop('xs and ys matrices must have same dimensions')
  }

  #vectors to hold output
  elongs <- long_axis_angles <- rep(NA, n_times)

  for(t in 1:n_times){
    x <- xs[,t]
    y <- ys[,t]
    non_na <- which(!is.na(x) & !is.na(y))
    if(length(non_na) > min_inds_tracked){
      pc <- prcomp(cbind(x[non_na],y[non_na]), center = T, scale = F)
      long_axis <- pc$rotation[,1]
      long_axis_angles[t] <- atan2(long_axis[2], long_axis[1])
      elongs[t] <- 1 - (pc$sdev[2]/pc$sdev[1])
    }

  }

  out <- list()
  out$elongation <- elongs
  out$long_axis_angle <- long_axis_angles

  #output polarization over time
  return(out)

}
