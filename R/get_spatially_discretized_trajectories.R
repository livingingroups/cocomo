#'Get spatially discretized trajectories
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Gets spatially discretized trajectories from temporally discretized trajectories,
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
#' @param na_threshold number indicating how big of an NA-gap is accepted before the distance calculation is reanchored (reanchor if na_gap>=na_threshold)
#' @param verbose whether to print progress while running
#'
#' @returns Returns a list containing `spat_ts` (the time points associated with each point along the spatially discretized trajectory),
#' `spat_xs` (the x coordinates of each point along the spatially discretized trajectory),
#' `spat_ys` (the y coordinates of each point along the spatially discretized trajectory),
#' `spat_breaks` (indexes to starts of breaks in the new spatially discretized data),
#' `spat_ds` (exact distance travelled at spatially discretized points),
#' `R` (radius used)
#' @export
get_spatially_discretized_trajectories <- function(xs, ys, R, breaks = NULL, na_threshold=1, verbose = T){
  
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
  spat_ts_all <- spat_xs_all <- spat_ys_all <- spat_ds_all <-  matrix(NA,nrow=n_inds,ncol=n_times) #matrices to hold all data
  spat_breaks_all <- matrix(NA,nrow=n_inds,ncol=n_breaks)
  for(ind in 1:n_inds){
    
    if(verbose){
      print('ind:')
      print(ind)
    }
    
    #get x/y for an individual
    x <- xs[ind,]
    y <- ys[ind,]
    
    spat_xs <- spat_ys <- spat_ts <- spat_ds <-  c() #matrices to hold data for individual ind
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
        spat_ds <- c(spat_ds,NA)
        
        na_count <- 0 #set up NA-counter for NA-threshold
        
        for(t in seq(ref_idx+1,breaks[i+1]-1)){
          
          #if reference index refers to NA data and the NA gap is bigger than allowed, set ref index to current index
          if(is.na(x[ref_idx]) | is.na(x[t])){
            na_count <- na_count+1
          }
          else{
            if(na_count>=na_threshold) { #move ref idx to the first non-NA value if NA-gap is too big
              ref_idx <- t
              na_count <- 0 #reset NA counter
              
              spat_ts <- c(spat_ts,t) #flag to indicate that there was an NA break
              spat_xs <- c(spat_xs,NA) 
              spat_ys <- c(spat_ys,NA) 
              spat_ds <- c(spat_ds,NA)
            }
            else{
              dx <- x[t] - x[ref_idx]
              dy <- y[t] - y[ref_idx]
              ds <- sqrt(dx^2 + dy^2)
              if(ds >= R){
                spat_ts <- c(spat_ts,t)
                spat_xs <- c(spat_xs,x[t])
                spat_ys <- c(spat_ys,y[t])
                spat_ds <- c(spat_ds,ds)
                
                ref_idx <- t
                na_count <- 0 #reset NA counter
              }
            }
          }
        }
        spat_ts_all[ind,1:length(spat_ts)] <- spat_ts
        spat_xs_all[ind,1:length(spat_ts)] <- spat_xs
        spat_ys_all[ind,1:length(spat_ts)] <- spat_ys
        spat_breaks_all[ind,1:length(spat_breaks)] <- spat_breaks
        spat_ds_all[ind,1:length(spat_ds)] <- spat_ds
      }
    }
  }
  non_nas <- colSums(!is.na(spat_ts_all))
  spat_ts <- spat_ts_all[,which(non_nas > 0)]
  spat_xs <- spat_xs_all[,which(non_nas > 0)]
  spat_ys <- spat_ys_all[,which(non_nas > 0)]
  spat_breaks <- spat_breaks_all
  spat_ds <- spat_ds_all[,which(non_nas > 0)]
  out <- list(spat_ts = spat_ts, spat_xs = spat_xs, spat_ys = spat_ys, spat_breaks = spat_breaks, spat_ds=spat_ds, R=R)
  return(out)
  
}


