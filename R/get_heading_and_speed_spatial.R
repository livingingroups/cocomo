#'Get spatially discretized heading and speed over time
#'
#'Gets spatially discretized heading for a given individual trajectory
#'This is defined as the unit vector pointing from the individual's current location at time `t` to
#'its location after it mas moved a distance of at least `R`.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param x_i vector of x coordinates for the trajectory
#' @param y_i vector of y coordinates for the trajectory
#' @param R radius used to compute the headings
#' @param t_idxs: time indexes at which to compute the headings (defaults to entire trajectory)
#' @param forward whether to go forward in time from current position (if T), or backward (if F) when computing headings
#' @param seconds_per_time_step number of seconds corresponding to each time step
#'
#' @returns Returns a list containing
#'   * `$heads`: a time series of the heading of the individual (a vector of the same length as x_i and y_i), in radians, computed based on
#' spatial discretization around the time point
#'   * `$speeds`: a time series of the average speed of the individual from the first point outside radius R to the current point
#'   * `$dts`: time differences between the current point and the first point outside of the radius R (either forward or backward in time)
#'
#' vector of spatially discretized headings, computed at all times or at times t_idxs if specified (other times are then filled in with NAs)
#' @export
get_heading_and_speed_spatial <- function(x_i, y_i, R, t_idxs=1:length(x_i), forward=T, seconds_per_time_step = 1){

  checkmate::assert_numeric(x_i)
  checkmate::assert_numeric(y_i)
  checkmate::assert_numeric(R, len = 1, lower = 0)
  checkmate::assert_integer(
    t_idxs,
    lower = 1,
    upper = length(x_i),
    any.missing = FALSE,
    min.len = 1,
    max.len = length(x_i),
    unique = TRUE,
    typed.missing = FALSE
  )
  checkmate::assert_logical(forward, len = 1)
  checkmate::assert_numeric(seconds_per_time_step, lower = 0, len = 1)


  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be vectors of the same length')
  }

  #initialize
  n_times <- length(x_i)
  spat_heads <- rep(NA, n_times)
  speeds <- rep(NA, n_times)
  dts <- rep(NA, n_times)

  #go backwards for backwards vectors
  if(!forward){
    t_idxs <- rev(t_idxs)
  }

  #loop over all times
  for(t in t_idxs){

    #get current location
    x0 <- x_i[t]
    y0 <- y_i[t]

    #initialize dt
    dt <- 0

    if(is.na(x0)){
      next
    }

    #move forward (or backward) until radius reached
    found <- 0
    na_found <- 0
    time_vec <- min((t+1), n_times):n_times
    if(!forward){
      time_vec <- max((t-1), 1):1
    }
    for(i in time_vec){

      dt <- dt + 1

      if(!forward){
        dx <- x0 - x_i[i]
        dy <- y0 - y_i[i]
      } else{
        dx <- x_i[i] - x0
        dy <- y_i[i] - y0
      }
      dist <- sqrt(dx^2+dy^2)

      if(is.na(dist)){
        spat_heads[t] <- NA
        speeds[t] <- NA
        dts[t] <- NA
        found <- 1
        na_found <- 1
        break
      }
      else{
        if(dist >= R){
          found <- 1
          break
        }
      }
    }

    #if you reach the end of the trajectory, return (and leave rest as NAs)
    if(found){
      if(!na_found){
        spat_heads[t] <- atan2(dy,dx)
        dts[t] <- dt
        speeds[t] <- dist / (dt * seconds_per_time_step)
      }
    } else{
      break
    }
  }

  #save output in a list
  out <- list()
  out$heads <- spat_heads
  out$speeds <- speeds
  out$dts <- dts

  return(out)
}


