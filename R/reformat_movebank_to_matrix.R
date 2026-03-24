#' Reformat Movebank to matrix format
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Takes in regularly sampled GPS data in Movebank format (i.e. data frame with specified columns - see below)
#' and converts it into the standard matrix form used by the `cocomo` package.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param movebank_data data frame that must include the columns
#' `'timestamp` (timestamp in UTC, must be character string of format YYYY-MM-DD HH:MM:SS.SSS),
#' `'individual.local.identifier'` (individual id of the tracked animals),
#' `'location.long'` (longitude coordinate), and
#' `'location.lat` (latitude coordinate).
#'  Can optionally also include the column `'study.local.timestamp'` (local timestamp for each GPS fix, must be character string of format YYYY-MM-DD HH:MM:SS.SSS).
#' @param output_file_path full path to the desired output file (must be a .RData file)
#' @param ids optional ids data frame which if specified will determine the order of rows in matrices (`code` column in ids needs to match to `individual.local.identifier` column in `movebank_data`)
#' @param data_chunks data frame with columns `start` and `end` specifying starting and ending datetimes (in POSIXct format - with time zone!) for each contiguous chunk of data. overrides other specified start and end times / dates.
#' @param seconds_per_time_step sampling interval of GPS fixes (in seconds)
#' @param start_date a character string specifying the starting date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param end_date a character string specifying the end date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param start_time a character string specifying the start time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param end_time a character string specifying the end time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param utm_zone numeric UTM zone (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#' @param hemisphere hemisphere (`'north'` or `'south'`) for UTM calculations (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#' @param output_utm whether to output `xs` and `ys` matrices (`T` or `F`)
#' @param output_latlon whether to output `lats` and `lons` matrices (`T` or `F`)
#' @param use_UTC if T (default) use UTC time rather than local time
#' @param local_timezone specify local timezone string, if not using UTC (use_UTC = F)
#'
#' @returns Saves a file to the location `output_file_path` containing the objects: `timestamps` (vector of timestamps in POSIXct format, of length `n_times`),
#' `ids` (data frame containing either the original `ids` data or a single `code` column with the id from `individual.local.identifier` column in `movebank_data`), `xs` (`n_inds` x `n_times` matrix of UTM eastings for all individuals at each time point),
#' `ys` (`n_inds` x `n_times` matrix of UTM northings for all individuals at each time point), `lons` (`n_inds` x `n_times` matrix of longitudes for all individuals at each time point), and
#' `lats` (`n_inds` x `n_times` matrix of longitudes for all individuals at each time point). `xs` and `ys` are only saved if `output_utm = T`. `lons` and `lats` are only saved if `output_lonlat = T`
#'
#' @export
reformat_movebank_to_matrix <- function(movebank_data, output_file_path = NULL,
                                        ids = NULL,
                                        data_chunks = NULL,
                                        seconds_per_time_step = 1,
                                        start_date = NULL,
                                        end_date = NULL,
                                        start_time = NULL,
                                        end_time = NULL,
                                        utm_zone = NULL,
                                        hemisphere = NULL,
                                        output_utm = T,
                                        output_latlon = T,
                                        use_UTC = T,
                                        local_timezone = NULL){


  #check the movebank_data dataframe has the correct columns
  columns_included <- colnames(movebank_data)
  columns_needed <- c('timestamp','individual.local.identifier','location.long','location.lat')
  if(sum(!(columns_needed %in% columns_included))>0){
    stop('movebank_data input is missing one or more needed columns - see documentation')
  }

  if(!use_UTC & !('study.local.timestamp' %in% columns_included)){
    stop('if use_UTC = F, study.local.timestamp column must be included')
  }

  if(!use_UTC & is.null(local_timezone)){
    stop('must specify local_timezone if use_UTC = F')
  }

  #if utm coordinates are not already included, get them
  if(sum(c('utm_easting','utm_northing') %in% columns_included) < 2 & output_utm){
    if(is.null(utm_zone) | is.null(hemisphere)){
      stop('utm data not included but also utm zone and/or hemisphere are not specified - need to specify utm zone and hemisphere')
    }
    #get utm coordinates
    lons_lats <- cbind(movebank_data$location.long, movebank_data$location.lat)
    easts_norths <- cocomo::latlon_to_utm(lons_lats = lons_lats, utm_zone = utm_zone, hemisphere = hemisphere)

    #add to data frame
    movebank_data$utm_easting <- easts_norths[,1]
    movebank_data$utm_northing <- easts_norths[,2]
  }

  #specify which timezone to use based on use_UTC
  if(use_UTC){
    timezone <- 'UTC'
  } else{
    timezone <- local_timezone
  }

  #if chunks have not been directly specified, create them from start and end dates, and start and end times
  if(is.null(data_chunks)){

    #first make sure start and end dates and start and end times are available
    if(is.null(start_date) | is.null(end_date) | is.null(start_time) | is.null(end_time)){
      stop('if not specifying data chunks directly, must specify start and end date as well as start and end time')
    }

    #get sequence of dates
    dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = 1)

    #create data frame to hold start and stop for each chunk
    data_chunks <- data.frame(start = paste(dates, rep(start_time, length(dates))),
                              end = paste(dates, rep(end_time, length(dates))))
    data_chunks$start <- as.POSIXct(data_chunks$start, tz = timezone)
    data_chunks$end <- as.POSIXct(data_chunks$end, tz = timezone)

    #if chunks cross date boundaries, fix so that end is on subsequent day
    if(data_chunks$end[1] < data_chunks$start[1]){
      data_chunks$end <- paste(dates + 1, rep(end_time, length(dates)))
      data_chunks$end <- as.POSIXct(data_chunks$end, tz = timezone)
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

  #convert timestamp or study.local.timestamp column to POSIXct
  if(use_UTC){
    movebank_data$timestamp <- as.POSIXct(movebank_data$timestamp, tz = timezone)
  } else{
    movebank_data$study.local.timestamp <- as.POSIXct(movebank_data$study.local.timestamp, tz = timezone)
  }

  #get list of individuals and number of individuals
  if(is.null(ids)){
    inds <- unique(movebank_data$individual.local.identifier)
  } else{
    inds <- ids$code
  }
  n_inds <- length(inds)

  #get number of timestamps
  n_times <- length(timestamps)

  #create matrices
  if(output_latlon){
    lats <- lons <- matrix(NA, nrow = n_inds, ncol = n_times)
  }
  if(output_utm){
    xs <- ys <- matrix(NA, nrow = n_inds, ncol = n_times)
  }

  #fill in matrices
  for(i in 1:n_inds){

    #get data for individual i
    data_ind <- movebank_data[which(movebank_data$individual.local.identifier == inds[i]),]

    #match timestamps in input data to those in timestamps vector
    if(use_UTC){
      time_idxs <- match(data_ind$timestamp, timestamps)
    } else{
      time_idxs <- match(data_ind$study.local.timestamp, timestamps)
    }

    #fill in matrices
    if(sum(!is.na(time_idxs))>0){

      non_nas <- which(!is.na(time_idxs))

      if(output_latlon){
        lats[i,time_idxs[non_nas]] <- data_ind$location.lat[non_nas]
        lons[i,time_idxs[non_nas]] <- data_ind$location.long[non_nas]
      }

      if(output_utm){
        xs[i,time_idxs[non_nas]] <- data_ind$utm_easting[non_nas]
        ys[i,time_idxs[non_nas]] <- data_ind$utm_northing[non_nas]
      }
    }

  }

  #create ids data frame if needed
  if(is.null(ids)){
    ids <- data.frame(code = inds)
  }

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


