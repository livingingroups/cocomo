#' Convert from matrix format to dataframe
#' 
#' Creates a dataframe out of GPS data in "matrix format" suitable for plotting with ggplot
#' 
#' @author Eli Strauss
#' @author NOT YET CODE REVIEWED
#' 
#'
#' @param xs UTM eastings matrix (`n_inds` x `n_times` matrix where xs[i,t] gives the easting of individual i at time step t)
#' @param ys UTM northings matrix (`n_inds` x `n_times` matrix where ys[i,t] gives the northing of individual i at time step t)
#' @param ids Vector of individuals ordered the same as the rows of xs and ys.
#' @param timestamps vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
#' @param time_indices_selected Time indices for data to include in the dataframe, to be matched with xs, ys, timestamps
#' @param ids_selected Identities of individuals to include in dataframe. If none provided, all individuals are included
#' @param lats matrix of latitude values (`n_inds` x `n_times`)
#' @param lons matrix of longitude values (`n_inds` x `n_times`)
#' 
#' @returns Returns a dataframe of ids, locations, and times 
#' @export




## A function for converting from Ari format to a dataframe for plotting
matrix_format_to_df <- function(xs, ys, timestamps, ids = NULL, 
                                time_indices_selected, ids_selected = NULL, 
                                lats = NULL, lons = NULL){
  
  ## If no ids_selected provided, use all
  if(is.null(ids_selected)){
    ## If there are ids but no ids_selected, use all
    if(!is.null(ids)){
      ids_selected <- ids
    }else{
    ## If no ides, use row numbers  
      ids <- ids_selected <- 1:nrow(xs)
    }
  }else if(is.null(ids)){
    ## If ids_selected provided but no ids, throw error
    stop('Must provide ids if ids_selected is not NULL')
  }
  
  if(!all(nrow(xs) == nrow(ys), ncol(xs) == ncol(ys), ncol(xs) == length(timestamps)))
    stop("dimension mismatch with xs, ys, and timestamps")
  
  ## Check for matching lengths
  if(length(ids) != nrow(xs))
    stop("length of ids must match number of rows in xs, ys")

  
  ## If no IDs at all, use all rows
  
  df <- data.frame(
    id = rep(ids_selected, each = length(time_indices_selected)),
    # Coerce xs and ys to a vector by rows
    x = as.vector(t(xs[match(ids_selected, ids), time_indices_selected])),
    y = as.vector(t(ys[match(ids_selected, ids), time_indices_selected])),
    time = rep(timestamps[time_indices_selected], length(ids_selected))
  )
  
  if(!is.null(lats) & !is.null(lons)){
    df$lons <- as.vector(t(lons[match(ids_selected, ids), time_indices_selected]))
    df$lats <- as.vector(t(lats[match(ids_selected, ids), time_indices_selected]))
  }
  return(df)
}
