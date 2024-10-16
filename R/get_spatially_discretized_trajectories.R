#'Get spatially discretized trajectories
#'
#'Gets spatially discretized trajectories from temporally discretized trajectories,
#' using a certain "ruler length" R.
#'
#' TODO: Code doesn't deal well with strings of NAs within it - look into this.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' @param R radius (ruler length) used in spatial discretization
#' @param breaks vector of indexes to breaks in the data (e.g. breaks between days)
#' @param verbose whether to print progress while running
#'
#' @returns Returns a list containing `spat_ts` (the time points associated with each point along the spatially discretized trajectory),
#' `spat_xs` (the x coordinates of each point along the spatially discretized trajectory),
#' `spat_ys` (the y coordinates of each point along the spatially discretized trajectory),
#' `spat_breaks` (indexes to starts of breaks in the new spatially discretized data),
#' `R` (radius used)
#' @export
get_spatially_discretized_trajectories <- function(xs, ys, R, breaks = NULL, verbose = T){

  #check matrix dimensions
  if(nrow(xs) != nrow(ys) || ncol(xs) != ncol(ys)){
    stop('xs and ys matrices must have same dimensions')
  }

  #get dimensions
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #set breaks, if not yet set
  if(is.null(breaks)){
    breaks <- c(1, n_times + 1)
  }

  #add end point to breaks if needed
  if(breaks[length(breaks)] < n_times){
    breaks <- c(breaks, n_times + 1)
  }

  #number of breaks
  n_breaks <- length(breaks) - 1

  #loop over individuals and compute spatially discretized trajectories
  na_flag <- FALSE
  spat_ts_all <- spat_xs_all <- spat_ys_all <- matrix(NA,nrow=n_inds,ncol=n_times) #matrices to hold all data
  spat_breaks_all <- matrix(NA,nrow=n_inds,ncol=n_breaks)
  for(ind in 1:n_inds){

    if(verbose){
      print('ind:')
      print(ind)
    }

    #get x/y for an individual
    x <- xs[ind,]
    y <- ys[ind,]

    spat_xs <- spat_ys <- spat_ts <- c() #matrices to hold data for individual ind
    spat_breaks <- rep(NA,n_breaks)
    for(i in 1:n_breaks){
      ref_idx <- breaks[i] #set ref index at first time in first chunk of data

      #find first non-NA index
      non_na_found <- 0
      end_reached <- 0
      while(!non_na_found & !end_reached){
        if(is.na(x[ref_idx])){
          ref_idx <- ref_idx + 1
        }
        else{
          non_na_found <- 1
        }
        if(ref_idx == (breaks[i+1]-1)){
          end_reached <- 1
        }
      }

      if(!end_reached){
        spat_xs <- c(spat_xs,x[ref_idx])
        spat_ys <- c(spat_ys,y[ref_idx])
        spat_ts <- c(spat_ts,ref_idx)
        spat_breaks[i] <- length(spat_xs)
        for(t in seq(ref_idx+1,breaks[i+1]-1)){

          #if reference index refers to NA data, set ref index to current index
          if(is.na(x[ref_idx]) | is.na(x[t])){
            ref_idx <- t
            na_flag <- TRUE #flag to indicate that there was an NA break
          }
          else{
            dx <- x[t] - x[ref_idx]
            dy <- y[t] - y[ref_idx]
            ds <- sqrt(dx^2 + dy^2)
            if(ds >= R){
              spat_ts <- c(spat_ts,t)
              if(!na_flag){
                spat_xs <- c(spat_xs,x[t])
                spat_ys <- c(spat_ys,y[t])
              } else{
                spat_xs <- c(spat_xs,NA)
                spat_ys <- c(spat_ys,NA)
              }
              ref_idx <- t
              na_flag <- FALSE #remove Na flag
            }
          }
        }
        spat_ts_all[ind,1:length(spat_ts)] <- spat_ts
        spat_xs_all[ind,1:length(spat_ts)] <- spat_xs
        spat_ys_all[ind,1:length(spat_ts)] <- spat_ys
        spat_breaks_all[ind,1:length(spat_breaks)] <- spat_breaks
      }
    }
  }
  non_nas <- colSums(!is.na(spat_ts_all))
  spat_ts <- spat_ts_all[,which(non_nas > 0)]
  spat_xs <- spat_xs_all[,which(non_nas > 0)]
  spat_ys <- spat_ys_all[,which(non_nas > 0)]
  spat_breaks <- spat_breaks_all
  out <- list(spat_ts = spat_ts, spat_xs = spat_xs, spat_ys = spat_ys, spat_breaks = spat_breaks, R=R)
  return(out)

}
