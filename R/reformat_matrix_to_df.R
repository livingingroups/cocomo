#' Convert from matrix format to dataframe
#' 
#' Creates a dataframe out of GPS data in "matrix format" suitable for plotting with ggplot
#' 
#' @author Eli Strauss
#' @author NOT YET CODE REVIEWED
#' 
#'
#' @param xs UTM eastings matrix (`n_inds` x `n_times` matrix where xs\[i,t\] gives the easting of individual i at time step t)
#' @param ys UTM northings matrix (`n_inds` x `n_times` matrix where ys\[i,t\] gives the northing of individual i at time step t)
#' @param ids Dataframe of individuals with individual name stored in a column called `id_code`. Only necessary if you want to select a subset of individuals using `id_codes_selected`
#' @param timestamps vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
#' @param time_indices_selected Time indices for data to include in the dataframe, to be matched with xs, ys, timestamps
#' @param id_codes_selected Identity codes of individuals to include in dataframe. If none provided, all individuals are included
#' @param lats matrix of latitude values (`n_inds` x `n_times`)
#' @param lons matrix of longitude values (`n_inds` x `n_times`)
#' 
#' @returns Returns a dataframe of id_codes, xs, ys, and timestamps 
#' @export




## A function for converting from Ari format to a dataframe for plotting
reformat_matrix_to_df <- function(xs, ys, timestamps, ids = NULL, 
                                time_indices_selected, id_codes_selected = NULL, 
                                lats = NULL, lons = NULL){
  
  ## If no id_codes_selected provided, use all
  if(is.null(id_codes_selected)){
    ## If there are ids but no id_codes_selected, use all
    if(!is.null(ids)){
      ## Check for matching lengths
      if(nrow(ids) != nrow(xs))
        stop("length of ids must match number of rows in xs, ys")
      id_codes_selected <- ids$id_code
    }else{
    ## If no ids, use row numbers  
      id_codes_selected <- 1:nrow(xs)
    }
  }else if(is.null(ids)){
    ## If id_codes_selected provided but no ids, throw error
    stop('Must provide ids if id_codes_selected is not NULL')
  }else{
    ## Check for matching lengths
    if(nrow(ids) != nrow(xs))
      stop("number of rows in ids must match number of rows in xs, ys")
    ## Check that all codes selected are contained in ids
    if(!all(id_codes_selected %in% ids$id_code))
      stop('some id_codes in id_codes_selected not found in ids')
  }
  
  if(!all(nrow(xs) == nrow(ys), ncol(xs) == ncol(ys), ncol(xs) == length(timestamps)))
    stop("dimension mismatch with xs, ys, and timestamps")
  
  df <- data.frame(
    id_code = rep(id_codes_selected, each = length(time_indices_selected)),
    # Coerce xs and ys to a vector by rows
    x = as.vector(t(xs[match(id_codes_selected, ids), time_indices_selected])),
    y = as.vector(t(ys[match(id_codes_selected, ids), time_indices_selected])),
    timestamp = rep(timestamps[time_indices_selected], length(id_codes_selected))
  )
  
  if(!is.null(lats) & !is.null(lons)){
    df$lons <- as.vector(t(lons[match(id_codes_selected, ids), time_indices_selected]))
    df$lats <- as.vector(t(lats[match(id_codes_selected, ids), time_indices_selected]))
  }
  return(df)
}
