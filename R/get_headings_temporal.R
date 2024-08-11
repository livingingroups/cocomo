#' Get headings (temporal)
#' Gets the headings of an individual over time given a trajectory.
#' Uses a temporal window `t_window` (in units of timesteps) to compute the headings, and can compute them
#' based on future movement (`forward = T`) or past movement (`forward = F`).
#' Headings are defined as `x_head = dx / (dx^2 + dy^2)` and `y_head = dy / (dx^2 + dy^2)`
#' where, `dx[t] = x_i[t + t_window] - x_i[t]` and `dy[t] = y_i[t + t_window] - y_i[t]` for `forward = T`
#' and `dx[t] = x_i[t] - x_i[t - t_window]` and `dy[t] = y_i[t] - y_i[t - t_window]` for `forward = F`
#'
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param	x_i x coordinates of the individual (a vector whose length is the number of timesteps) or of a group centroid
#' @param y_i y coordinates of the individual (a vector whose length is the number of timesteps) or of the group centroid
#' @param t_window the time window over which to compute speed (number of time steps into the future or past)
#' @param forward a boolean value (defaults to T) indicating whether to compute speeds forward in time (if T) or backward in time (if F)
#' @return Returns `heads`: a time series of the heading of the individual (a vector of the same length as x_i and y_i), in radians
#'
#' @export
get_headings_temporal <- function(x_i, y_i, t_window = 1, forward = T){

  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be vectors of the same length')
  }

  #get length of x_i and y_i
  len <- length(x_i)

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

  head_x <- dx / sqrt(dx^2 + dy^2)
  head_y <- dy / sqrt(dx^2 + dy^2)

  heads <- atan2(head_y, head_x)

  #output headings
  return(heads)

}
