#'Get spatially discretized headings
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
#'
#' @returns vector of spatially discretized headings, computed at all times or at times t_idxs if specified (other times are then filled in with NAs)
#' @export
get_headings_spatial <- function(x_i, y_i, R, t_idxs=1:length(x_i), forward=T){

  #throw warning about lack of code review
  warning('This function has not yet been code reviewed - if you would like to review it, contact Ari!')

  #check that x_i and y_i are the same length
  if(length(x_i) != length(y_i)){
    stop('x_i and y_i must be vectors of the same length')
  }

  #initialize
  n_times <- length(x_i)
  spat_heads <- rep(NA,n_times)

  #go backwards for backwards vectors
  if(!forward){
    t_idxs <- rev(t_idxs)
  }

  #loop over all times
  for(t in t_idxs){

    #get current location
    x0 <- x_i[t]
    y0 <- y_i[t]

    if(is.na(x0)){
      next
    }

    #move forward (or backward) until radius reached
    found <- 0
    na_found <- 0
    time_vec <- t:n_times
    if(!forward){
      time_vec <- seq(t,1,-1)
    }
    for(i in time_vec){

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
      }
    } else{
      return(spat_heads)
    }
  }
  return(spat_heads)
}


