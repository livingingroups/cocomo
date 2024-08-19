#' Get speed over time
#'
#' Gets an individual's instantaneous speed over time, using a specified time window to compute the speed
#'
#'The speed is computed as `sqrt ( (x.i[t+step] - x.i[t] )^2 + ( y.i[t+step] - y.i[t] )^2 ) / (dt * step)`
#'
#' @param	x_i x coordinates of the individual (a vector whose length is the number of timesteps) or of a group centroid
#' @param y_i y coordinates of the individual (a vector whose length is the number of timesteps) or of the group centroid
#' @param time_res the number of seconds between timestamps (e.g. for data recorded at 1 Hz, the value would be 1, for data recorded every 30 sec, the value would be 30)
#' @param step the time step over which to compute speed (number of time steps)
#'
#' @return Returns `speeds`: a time series of the speed of individual
#'
get_speed_over_time <- function(x_i, y_i, time_res, step = 1){

  #get the change in x and y coordinates for each time step
  dx <- diff(x_i)
  dy <- diff(y_i)

  #append an NA to the end of the distance vectors to keep them the same length as the input data
  dx <- c(dx,NA)
  dy <- c(dy,NA)

  #get change in time for each step
  dt <- time_res * step

  #get instantaneous speed
  speeds <- sqrt( dx^2 + dy^2 ) / dt

  #output speeds
  return(speeds)

}
