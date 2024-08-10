#' Get speed over time
#'
#' Gets an individual's instantaneous speed over time, using a specified time step to compute the speed
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

  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be vectors of the same length')
  }

  #get length of x_i and y_i
  len <- length(x_i)

  #get x and y locations at time t and t + step (if going forward)
  #or at time t and t - step (if going backward)
  #pad ends with NAs to make lengths match up
  if(forward){
    x1 <- c(x_i[1:(len - step)], rep(NA, step))
    x2 <- c(x_i[(step + 1):len], rep(NA, step))
    y1 <- c(y_i[1:(len - step)], rep(NA, step))
    y2 <- c(y_i[(step + 1):len], rep(NA, step))

  } else{
    x1 <- c(rep(NA, step), x_i[1:(len - step)])
    x2 <- c(rep(NA, step), x_i[(step + 1):len])
    y1 <- c(rep(NA, step), y_i[1:(len - step)])
    y2 <- c(rep(NA, step), y_i[(step + 1):len])
  }

  #get change in x and y
  dx <- x2 - x1
  dy <- y2 - y1

  #get change in time for each step
  dt <- time_res * step

  #get instantaneous speed
  speeds <- sqrt( dx^2 + dy^2 ) / dt

  #output speeds
  return(speeds)

}
