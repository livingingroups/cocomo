#' Reformat Movebank to matrix format
#'
#' Takes in regularly sampled GPS data in Movebank format (data frame with specified columns)
#' and converts it into the standard matrix form used by the cocomo package.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param movebank_data data frame that must include the columns
#' `'individual_local_identifier'` (individual id of the tracked animals,
#' `'study_local_timestamp'` (local timestamp for each GPS fix, must be POSIXct objects including timezone),
#' `'location_long'` (longitude coordinate), and
#' `'location_lat` (latitude coordinate)
#' @param output_file_path full path to the desired output file
#' @param data_chunks data frame specifying starting and ending datetimes (in POSIXct format - with time zone!) for each contiguous chunk of data. overrides other specified start and end times / dates
#' @param seconds_per_time_step sampling interval of GPS fixes (in seconds)
#' @param start_date a character string specifying the starting date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param end_date a character string specifying the end date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param start_time a character string specifying the start time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param start_time a character string specifying the end time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @export
reformat_movebank_to_matrix <- function(movebank_data, output_file_path = NULL,
                                        data_chunks = NULL,
                                        seconds_per_time_step = 1,
                                        start_date = NULL,
                                        end_date = NULL,
                                        start_time = NULL,
                                        end_time = NULL,
                                        utm_zone = NULL,
                                        hemisphere = NULL,
                                        output_utm = T,
                                        output_latlon = T){


  #check the movebank_data dataframe has the correct columns
  columns_included <- colnames(movebank_data)
  columns_needed <- c('individual_local_identifier','study_local_timestamp','location_long','location_lat')
  if(sum(!(columns_needed %in% columns_included))>0){
    stop('movebank_data input is missing one or more needed columns - see documentation')
  }

  #if utm coordinates are not already included, get them
  if(sum(c('utm_easting','utm_northing') %in% columns_included) < 2){
    if(is.null(utm_zone) | is.null(hemisphere)){
      stop('utm data not included but also utm zone and/or hemisphere are not specified - need to specify utm zone and hemisphere')
    }
    #get utm coordinates
    lons_lats <- cbind(movebank_data$location_long, movebank_data$location_lat)
    easts_norths <- cocomo::latlon_to_utm(lons_lats = lons_lats, utm_zone = utm_zone, hemisphere = hemisphere)

    #add to data frame
    movebank_data$utm_easting <- easts_norths[,1]
    movebank_data$utm_northing <- easts_norths[,2]
  }

  #get timezone
  timezone <- lubridate::tz(movebank_data$study_local_timestamp[1])

  #if chunks have not been directly specified, create them from start and end dates, and start and end times
  if(is.null(data_chunks)){

    #first make sure start and end dates and start and end times are available
    if(is.null(start_date) | is.null(end_date) | is.null(start_time) | is.null_end_times){
      stop('if not specifying data chunks directly, must specify start and end date as well as start and end time')
    }

    #get sequence of dates
    dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = 1)

    #create data frame to hold start and stop for each chunk
    data_chunks <- data.frame(start = paste(dates, rep(start_time, length(dates))),
                              end = paste(dates, rep(end_time, length(dates))))
    data_chunks$start <- as.POSIXct(data_chunks$start, tz = 'UTC')
    data_chunks$end <- as.POSIXct(data_chunks$end, tz = 'UTC')

    #if chunks cross date boundaries, fix so that end is on subsequent day
    if(data_chunks$end[1] < data_chunks$start[1]){
      data_chunks$end <- paste(dates + 1, rep(end_time, length(dates)))
      data_chunks$end <- as.POSIXct(data_chunks$end, tz = 'UTC')
    }
  }

  #create timestamps vector
  timestamps <- c()
  for(i in 1:nrow(data_chunks)){
    timestamps_chunk <- seq.POSIXt(from = data_chunks$start[i],
                                   to = data_chunks$end[i],
                                   by = seconds_per_time_step)
    timestamps <- c(timestamps, timestamps_chunk)
  }

  #convert back to POSIXct (for some reason it comes out numeric after using seq.POSIXt)
  #also add timezone specified by the user
  timestamps <- as.POSIXct(timestamps, tz = timezone)

  #get list of individuals
  inds <- unique(movebank_data$individual_local_identifier)

  #get number of individuals and number of timestamps
  n_inds <- length(inds)
  n_times <- length(timestamps)

  #create matrices
  lats <- lons <- xs <- ys <- matrix(NA, nrow = n_inds, ncol = n_times)

  #fill in matrices
  for(i in 1:n_inds){

    #get data for individual i
    data_ind <- movebank_data[which(movebank_data$individual_local_identifier == inds[i]),]

    #match timestamps in input data to those in timestamps vector
    time_idxs <- match(data_ind$study_local_timestamp, timestamps)

    #fill in matrices
    lats[i,time_idxs] <- data_ind$location_lat
    lons[i,time_idxs] <- data_ind$location_long
    xs[i,time_idxs] <- data_ind$utm_easting
    ys[i,time_idxs] <- data_ind$utm_northing

  }

  #create ids data frame
  ids <- data.frame(id = inds)

  #create list of which variables to output
  to_output <- c('ids','timestamps')
  if(output_latlon){
    to_output <- c(to_output,c('lons','lats'))
  }
  if(output_utm){
    to_output <- c(to_output, c('xs','ys'))
  }

  #save output
  save(list = to_output, file = output_file_path)

}


