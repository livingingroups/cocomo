#' Get directedness
#'
#' Gets the directedness of a trajectory over a given time window, as a function of time.
#'
#' The directedness a number which ranges from 0 to 1 where 1 is a straight path and 0 is a highly tortuous path.
#' It is defined as the net displacement (distance along a straight-line path from point A to point B) divided by the
#' path length (total distance traveled along the actual path from point A to point B).
#'
#' NOTE: Keep in mind that the time window `t_window` should be given as the number of time points of data to use when computing directedness, so the unit is number of timesteps, not seconds.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param	x_i vector of x positions for an individual (or the group centroid) of length `n_times`
#' @param	y_i vector of y positions for an individual (or the group centroid) of length `n_times`
#' @param	t_window window of time to use for computing directedness (must be an even number) - the directedness for time t will be computed using position data from `t - t_window / 2` to `t + t_window / 2`
#' @returns vector of directedness of the trajectory as a function of time
#' @export
get_directedness <- function(x_i, y_i,t_window){

  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be the same length')
  }

  #check that t_window is even
  if(t_window %% 2 != 0){
    stop('t_window must be even')
  }

  #number of time points in the position vectors
  n_times <- length(x_i)

  #create a vector to hold directedness values
  directedness <- rep(NA,n_times)

  #loop over time and get directedness at each time step
  for(t in seq(t_window/2+1, n_times - t_window/2, 1)){

    #get values of x and y positions for the individual within the current time window
    x_curr <- x_i[seq(t-t_window/2+1, t+t_window/2, 1)]
    y_curr <- y_i[seq(t-t_window/2+1, t+t_window/2, 1)]

    #get change in x and y positions during each time step
    dx_curr <- diff(x_curr)
    dy_curr <- diff(y_curr)

    #get distance traveled in each time step
    dists <- sqrt(dx_curr^2 + dy_curr^2)

    #get the total distance traveled ("path length") across the time window
    path_len <- sum(dists)

    #get the net displacement (distance from the position at beginning of the time window to position at end of time window)
    dx_tot <- x_curr[length(x_curr)] - x_curr[1] #displacement in x dimension
    dy_tot <- y_curr[length(y_curr)] - y_curr[1] #displacement in y dimension
    net_disp <- sqrt( dx_tot^2 + dy_tot^2 )

    #get the directedness at that time point, which is net.disp / path.len
    directedness_curr <- net_disp / path_len

    #store this directedness at position i in the directedness vector (since this is the directedness for the current time)
    directedness[t] <- directedness_curr

  }

  #output directedness vector
  return(directedness)

}
