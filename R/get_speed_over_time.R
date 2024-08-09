#' Get speed over time
#'
#' Gets an individual's instantaneous speed over time, using a specified time window to compute the speed
#'
#'The speed is computed as `speeds[t] = sqrt ( (x_i[t+step] - x_i[t] )^2 + ( y_i[t+step] - y_i[t] )^2 ) / (time_res * step)`
#'Note that by default `speeds[t]` gives the instantaneous speed between time `t` and time `t + step`, i.e. the speeds are computed
#'based on the future not on the past. If `forward` is set to `F`, the speeds will be computed based on the past, as:
#'`speeds[t] = sqrt ( (x_i[t] - x_i[t-step] )^2 + ( y_i[t] - y_i[t-step] )^2 ) / (time_res * step)`
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param	x_i x coordinates of the individual (a vector whose length is the number of timesteps) or of a group centroid
#' @param y_i y coordinates of the individual (a vector whose length is the number of timesteps) or of the group centroid
#' @param time_res the number of seconds between timestamps (e.g. for data recorded at 1 Hz, the value would be 1, for data recorded every 30 sec, the value would be 30)
#' @param step the time step over which to compute speed (number of time steps)
#' @param forward a boolean value (defaults to T) indicating whether to compute speeds forward in time (if T) or backward in time (if F)
#' @return Returns `speeds`: a time series of the speed of individual (a vector of the same length as x_i and y_i)
#'
#' @export
get_speed_over_time <- function(x_i, y_i, time_res, step = 1, forward = T){

  #throw warning about lack of code review
  warning('This function has not yet been code reviewed - if you would like to review it, contact Ari!')

  #get the change in x and y coordinates for each time step
  dx <- diff(x_i)
  dy <- diff(y_i)

  #append an NA to the end or beginning of the distance vectors to keep them the same length as the input data

  if(forward){
    dx <- c(dx,NA)
    dy <- c(dy,NA)
  } else{
    dx <- c(NA, dx)
    dy <- c(NA, dy)
  }

  #get change in time for each step
  dt <- time_res * step

  #get instantaneous speed
  speeds <- sqrt( dx^2 + dy^2 ) / dt

  #output speeds
  return(speeds)

}
