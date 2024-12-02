#' Get heading and speed over time (temporal)
#'
#' Gets the headings and speeds of an individual over time given a trajectory.
#' Uses a temporal window `t_window` (in units of timesteps) to compute the headings and speeds,
#' and can compute them based on future movement (`forward = T`) or past movement (`forward = F`).
#' Headings are defined as `x_head = dx / (dx^2 + dy^2)` and `y_head = dy / (dx^2 + dy^2)`
#' where, `dx[t] = x_i[t + t_window] - x_i[t]` and `dy[t] = y_i[t + t_window] - y_i[t]` for `forward = T`
#' and `dx[t] = x_i[t] - x_i[t - t_window]` and `dy[t] = y_i[t] - y_i[t - t_window]` for `forward = F`
#' Speeds are defined as `speed = sqrt(dx^2 + dy^2) / t_window`.
#'
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param	x_i x coordinates of the individual (a vector whose length is the number of timesteps) or of a group centroid
#' @param y_i y coordinates of the individual (a vector whose length is the number of timesteps) or of the group centroid
#' @param t_window the time window over which to compute speed (number of time steps into the future or past)
#' @param forward a boolean value (defaults to T) indicating whether to compute speeds forward in time (if T) or backward in time (if F)
#' @param seconds_per_time_step number of seconds corresponding to each time step
#' @returns Returns a list containing `$heads`: a time series of the heading of the individual (a vector of the same length as x_i and y_i), in radians,
#' `$speeds`: a time series of the speed of the individual at each time point, and
#' `$dts`: time differences between each point (will be all the same value, but given as a vector for compatibility with spatial headings function)
#'
#' @export
get_heading_and_speed_temporal <- function(x_i, y_i, t_window = 1, forward = T, seconds_per_time_step = 1){
  checkmate::assert_numeric(x_i)
  checkmate::assert_numeric(y_i)
  checkmate::assert_int(t_window, lower = 1, upper = length(x_i))
  checkmate::assert_logical(forward, len = 1)
  checkmate::assert_numeric(seconds_per_time_step, lower = 0, len = 1)



  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be vectors of the same length')
  }

  #get length of x_i and y_i
  len <- length(x_i)

  #initialize output list
  out <- list()

  #get x and y locations at time t and t + t_window (if going forward)
  #or at time t and t - t_window (if going backward)
  #pad ends with NAs to make lengths match up
  if(forward){
    x1 <- c(x_i[1:(len - t_window)], rep(NA, t_window))
    x2 <- c(x_i[(t_window + 1):len], rep(NA, t_window))
    y1 <- c(y_i[1:(len - t_window)], rep(NA, t_window))
    y2 <- c(y_i[(t_window + 1):len], rep(NA, t_window))

  } else{
    x1 <- c(rep(NA, t_window), x_i[1:(len - t_window)])
    x2 <- c(rep(NA, t_window), x_i[(t_window + 1):len])
    y1 <- c(rep(NA, t_window), y_i[1:(len - t_window)])
    y2 <- c(rep(NA, t_window), y_i[(t_window + 1):len])
  }

  #get change in x and y
  dx <- x2 - x1
  dy <- y2 - y1
  ds <- sqrt(dx^2 + dy^2)

  head_x <- dx / ds
  head_y <- dy / ds

  heads <- atan2(head_y, head_x)

  #output
  out$speeds <- ds / (t_window * seconds_per_time_step)
  out$heads <- heads
  out$dts <- rep(t_window * seconds_per_time_step, len)

  return(out)

}
