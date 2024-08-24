#' Import meerkat data to matrix format
#'
#' @param input_dir full path to input directory where all files from a given deployment are stored (e.g. `"~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_1/"`)
#' @param tag_type `'gipsy5'` or `'axytrek'`
#' @param data_chunks data frame with columns `start` and `end` specifying starting and ending datetimes (in POSIXct format - with time zone!) for each contiguous chunk of data. overrides other specified start and end times / dates.
#' @param seconds_per_time_step sampling interval of GPS fixes (in seconds)
#' @param start_date a character string specifying the starting date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param end_date a character string specifying the end date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param start_time a character string specifying the start time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param end_time a character string specifying the end time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param utm_zone numeric UTM zone (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#' @param hemisphere hemisphere (`'north'` or `'south'`) for UTM calculations (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#'
#'
import_meerkat_gps_data <- function(input_dir, tag_type = 'gipsy5', timezone = 'UTC'){

  #get all files
  all_files <- list.files(input_dir, pattern = ".*csv$", recursive = T)

  #get collar files and focal files
  collar_files <- all_files[grep('COLLAR/GPS', all_files)]
  focal_files <- all_files[grep('FOCAL', all_files)]

  #check for the right number of subdirectories (there should be 4)
  n_subdirs <- sapply(collar_files, FUN = function(x){return(length(strsplit(x,'/')[[1]]))})
  collar_files <- collar_files[which(n_subdirs == 4)]

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

  #read in all GPS data
  gps_data_df <- data.frame()
  if(length(collar_files)>0){
    for(i in 1:length(collar_files)){

      #get file info
      file <- paste0(input_dir,collar_files[i])
      file_basename <- basename(file)

      #split up name into parts and extract info
      basename_split <- strsplit(file_basename, '_')
      group_id <- basename_split[[1]][1]
      ind_id <- basename_split[[1]][2]
      file_dates <- gsub('.csv','',basename_split[[1]][length(basename_split[[1]])])
      file_start_date <- strsplit(file_dates,'-')[[1]][1]
      file_end_date <- strsplit(file_dates,'-')[[1]][2]

      #reformat dates
      file_start_date <- as.Date(file_start_date, format = '%Y%m%d')
      file_end_date <- as.Date(file_end_date, format = '%Y%m%d')

      #read in data from file
      curr_dat <- read.csv(file, sep = '\t', header=T)

      #reformat timestamp
      curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y %H:%M:%S')

      #filter out data outside of date range (here the collar was recording but not on a meerkat)
      idxs_keep <- which(as.Date(curr_dat$timestamp) >= file_start_date & as.Date(curr_dat$timestamp) <= file_end_date)
      curr_dat <- curr_dat[idxs_keep, ]

      #number of rows
      n_rows <- length(idxs_keep)

      #format data
      if(n_rows > 0){
        data_to_add <- data.frame(filename = rep(file_basename, n_rows),
                                  ind_id = rep(ind_id, n_rows),
                                  group_id = rep(group_id, n_rows),
                                  timestamp = curr_dat$timestamp,
                                  lon = curr_dat$location.long,
                                  lat = curr_dat$location.lat,
                                  satellites = curr_dat$satellite.count,
                                  hdop = curr_dat$hdop,
                                  type = rep('collar', n_rows)
                                  )

        #add to data frame
        gps_data_df <- rbind(gps_data_df, data_to_add)
      }
    }
  }

  for(i in 1:length(focal_files)){

    #get file info
    file <- paste0(input_dir,focal_files[i])
    file_basename <- basename(file)

    #split up name into parts and extract info
    basename_split <- strsplit(file_basename, '_')
    group_id <- basename_split[[1]][1]
    ind_id <- basename_split[[1]][2]
    file_date <- gsub('.csv','',basename_split[[1]][length(basename_split[[1]])])

    #reformat date
    file_date <- as.Date(file_date, format = '%Y%m%d')

    #read in data from file
    curr_dat <- read.csv(file, sep = '\t', header=T)

    #reformat timestamp
    curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y %H:%M:%S')

    #filter out data outside of date range
    idxs_keep <- which(as.Date(curr_dat$timestamp) == file_date)
    curr_dat <- curr_dat[idxs_keep,]

    #number of rows
    n_rows <- length(idxs_keep)

    if(n_rows > 0){
      data_to_add <- data.frame(filename = rep(file_basename, n_rows),
                                ind_id = rep(ind_id, n_rows),
                                group_id = rep(group_id, n_rows),
                                timestamp = curr_dat$timestamp,
                                lon = curr_dat$location.long,
                                lat = curr_dat$location.lat,
                                satellites = curr_dat$satellite.count,
                                hdop = curr_dat$hdop,
                                type = rep('focal', n_rows)
      )

      #add to data frame
      gps_data_df <- rbind(gps_data_df, data_to_add)

    }
  }

  return(gps_data_df)

}
