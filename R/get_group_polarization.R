#'Get group polarization
#'
#'Computes the polarization of the group at each time step `t`.
#'The polarization is a measure of how aligned the group is, ranging from 0 (completely unaligned) to 1 (completely aligned)
#'The polarization is defined by adding up (vector addition) all of the heading vectors of all individuals at a given moment in time,
#'taking the length of the resultant vector, and dividing this by the number of individuals.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time

#' @returns Returns a vector of length `n_times` giving the polarization of the group over time
#' (where the heading is undefined, returns NA)

get_group_polarization <- function(xs,ys){

  #get displacements in x and y directions
  dx <- t(apply(xs,MARGIN=1,FUN=diff))
  dy <- t(apply(ys,MARGIN=1,FUN=diff))

  #get total displacement
  total_disp <- sqrt(dx^2 + dy^2)

  #get heading vectors (x and y components) by dividing displacement in each dimension by total displacement
  heads_x <- dx / total_disp
  heads_y <- dy / total_disp

  #add up heading vectors for each moment in time (column)
  heads_x_tot <- colSums(heads_x)
  heads_y_tot <- colSums(heads_y)

  #get length of the resultant vector
  resultant.vec.len <- sqrt(heads.x.tot^2 + heads.y.tot^2)

  #divide by the number of individuals (N) to get the polarization at each moment in time
  N <- nrow(xs)
  polarization <- resultant.vec.len / N

  #append an NA because the polarization is undefined for the last time index
  polarization <- c(polarization,NA)

  #output polarization over time
  return(polarization)

}
