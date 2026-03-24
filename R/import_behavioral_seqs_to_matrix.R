#' Import behavioral sequences (output from classifier) to matrix format
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Imports behavioral sequence data from a set of csv files and converts them to
#' matrix format matching the `xs` and `ys` matrices used in the `cocomo` library
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param input_dirs list of input directories where behavioral sequence data are stored (full paths)
#' @param ids data frame giving individual ids for that deployment
#' @param timestamps vector of timestamps associated with that deployment (in UTC)
#'
#' @returns Returns a list containing `out$behavs`, a matrix where `out$behavs[i,t]` gives the behavior of
#' individual `i` (correspoding to the row in `ids` at time `t` (corresponding to the index in `timestamps`)
#' In this matrix, the behavior is represented as an integer value (for quicker processing). The second item
#' in the list, `out$behavs_key` is a data frame with columns `behav` (the behavior as a string) and `int_value`
#' (the correspoding integer value associated with that behavior in the `out$behavs` matrix)
#'
#'
#' @importFrom lubridate date
#'
#' @export
import_behavioral_seqs_to_matrix <- function(input_dirs, ids, timestamps){

  files <- c()
  for(dir in 1:length(input_dirs)){
    files <- c(files, list.files(input_dirs[[dir]], full.names = T))
  }

  #create data frame to hold data in original format
  behavs_df <- data.frame()

  #loop over files and collect data into a data frame
  for(i in 1:length(files)){
    print(i)
    file <- files[i]
    id_code <- tools::file_path_sans_ext(basename(file))
    currdat <- read.csv(file)
    currdat$datetime <- as.POSIXct(currdat$datetime, tz = 'UTC')
    currdat$id_code <- id_code
    ind_idx <- which(ids$id_code == id_code)
    currdat$ind_idx <- ind_idx
    behavs_df <- rbind(behavs_df, currdat)
  }

  #create behaviors key
  all_behavs <- unique(behavs_df$state)
  behavs_key <- data.frame(behav = all_behavs, int_value = 1:length(all_behavs))

  #get indices corresponding to the timestamps vector for each UTC time
  behavs_df$time_idx <- match(behavs_df$datetime, timestamps)

  #add in the behaviors integer value to the behavs_df dataframe
  behavs_df$int_value <- behavs_key$int_value[match(behavs_df$state, behavs_key$behav)]

  #create behavs matrix to hold output
  n_inds <- nrow(ids)
  n_times <- length(timestamps)
  behavs <- matrix(NA, nrow = n_inds, ncol = n_times)

  #store behavioral data in matrix (in integer form)
  non_nas <- which(!is.na(behavs_df$time_idx) & !is.na(behavs_df$ind_idx))
  behavs[cbind(behavs_df$ind_idx[non_nas], behavs_df$time_idx[non_nas])] <- behavs_df$int_value[non_nas]

  out <- list()
  out$behavs <- behavs
  out$behavs_key <- behavs_key

  return(out)


}
