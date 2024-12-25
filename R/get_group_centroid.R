#'Get group centroid
#'
#'Get the group's centroid (mean x and y position) over time.
#'If specified, only compute if at least `min_inds_tracked` are tracked in a given
#'time step.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' @param min_inds_tracked if specified, sets a minimum number of individuals that must be tracked at any moment in time to compute centroid (otherwise the centroid will be NA at that time point
#' @returns Returns a list containing coordinates of the group centroid over time, `x_centr` and `y_centr` (both vectors of length `n_times`)
#' @export
#'
get_group_centroid <- function(xs, ys, min_inds_tracked = NULL){
  checkmate::assert_matrix(xs, 'numeric')
  checkmate::assert_matrix(ys, 'numeric')
  checkmate::assert_int(min_inds_tracked, lower=0, upper=nrow(xs), null.ok = TRUE)

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #get x and y position of centroid
  x_centr <- colMeans(xs, na.rm=T)
  y_centr <- colMeans(ys, na.rm=T)

  #replace time points with too few individuals with NAs
  if(!is.null(min_inds_tracked)){
    n_tracked <- colSums(!is.na(xs) & !is.na(ys))
    not_enough_inds <- which(n_tracked < min_inds_tracked)
    x_centr[not_enough_inds] <- NA
    y_centr[not_enough_inds] <- NA
  }

  #prepare output
  out <- list()
  out$x_centr <- x_centr
  out$y_centr <- y_centr

  return(out)

}
