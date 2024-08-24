#' Import meerkat GPS data
#'
#' #Imports raw meerkat GPS data from Technosmart files (Gipsy 5 or Axy-Trek) assuming a standardized
#' file folder structure of `input_dir` and standardized naming schemes as done in the meerkat group collaring project.
#' Outputs a file containing the xs, ys, and timestamps matrices as well as another file with the GPS data table in Movebank format
#'
#' @param input_dir full path to input directory where all files from a given deployment are stored (e.g. `"~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_1/"`)
#' @param output_dir full path to the output directory where processed files will be saved
#' @param tag_type `'gipsy5'` or `'axytrek'`
#' @param start_date a character string specifying the starting date for data sampled in a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param end_date a character string specifying the end date for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'YYYY-MM-DD'`
#' @param start_time a character string specifying the start time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param end_time a character string specifying the end time for data sampled on a daily basis, only used if `data_chunks = NULL`, format must be `'HH:MM:SS'`
#' @param min_satellites minimum number of satellites to include data
#' @param utm_zone numeric UTM zone (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#' @param hemisphere hemisphere (`'north'` or `'south'`) for UTM calculations (only needed if `output_utm = T` and if `movebank_data` does not have `utm_easting` and `utm_northing` columns)
#' @param seconds_per_time_step sampling interval of GPS fixes (in seconds)
#' @param timezone timezone to use (UTC)
#'
#' @importFrom lubridate hour parse_date_time
#' @export
import_meerkat_gps_data <- function(input_dir, output_dir,
                                    tag_type,
                                    start_date = NULL, end_date = NULL,
                                    start_time = NULL, end_time = NULL,
                                    min_satellites = 5,
                                    utm_zone = 36, hemisphere = 'south',
                                    seconds_per_time_step = 1,
                                    timezone = 'UTC'){

  #make sure tag type is specified
  if(!tag_type %in% c('gipsy5','axytrek')){
    stop('must specify tag_type as either gipsy5 or axytrek')
  }

  #get relevant files
  if(tag_type == 'gipsy5'){
    #get all files
    all_files <- list.files(input_dir, pattern = ".*csv$", recursive = T)

    #get collar files and focal files
    collar_files <- all_files[grep('COLLAR/GPS', all_files)]
    focal_files <- all_files[grep('FOCAL', all_files)]
  }

  if(tag_type == 'axytrek'){
    all_files <- list.files(input_dir, pattern = ".*txt$", recursive = T)
    collar_files <- all_files[grep('COLLAR/GPS', all_files)]
    focal_files <- all_files[grep('FOCAL', all_files)]
  }

  #check collar files for the right number of subdirectories (there should be 4)
  #ignore any files with the wrong number of subdirectories
  n_subdirs_collar <- sapply(collar_files, FUN = function(x){return(length(strsplit(x,'/')[[1]]))})
  collar_files <- collar_files[which(n_subdirs_collar == 4)]

  #check focal files for the right number of subdirectories
  n_subdirs_focal <- sapply(focal_files, FUN = function(x){return(length(strsplit(x,'/')[[1]]))})
  focal_files <- focal_files[which(n_subdirs_focal == 4)]

  #--------Read in all GPS data and create data frame------
  gps_data_df <- data.frame()
  if(length(collar_files)>0){
    for(i in 1:length(collar_files)){

      #get file info
      file <- paste0(input_dir,collar_files[i])

      #if file is empty, skip
      if(file.size(file) == 0){
        next
      }

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
      if(tag_type=='gipsy5'){
        curr_dat <- read.csv(file, sep = '\t', header=T)

        #reformat timestamp
        curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y %H:%M:%S')

      }
      if(tag_type=='axytrek'){
        curr_dat <- read.delim(file, sep = '\t', header=F)
        colnames(curr_dat) <- c('timestamp','location.lat','location.long','V4','V5','satellite.count','V7','V8')

        #reformat timestamp
        curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y,%H:%M:%S')

      }


      #filter out data outside of date range (here the collar was recording but not on a meerkat)
      idxs_keep <- which(as.Date(curr_dat$timestamp) >= file_start_date &
                           as.Date(curr_dat$timestamp) <= file_end_date &
                           curr_dat$satellite.count >= min_satellites)
      curr_dat <- curr_dat[idxs_keep, ]

      #number of rows
      n_rows <- length(idxs_keep)

      #format data
      if(n_rows > 0){
        data_to_add <- data.frame(filename = rep(file_basename, n_rows),
                                  individual.local.identifier = rep(ind_id, n_rows),
                                  animal.group.id = rep(group_id, n_rows),
                                  timestamp = curr_dat$timestamp,
                                  location.long = curr_dat$location.long,
                                  location.lat = curr_dat$location.lat,
                                  satellites = curr_dat$satellite.count,
                                  record_type = rep('collar', n_rows),
                                  tag_type = rep(tag_type, n_rows)
                                  )

        #add to data frame
        gps_data_df <- rbind(gps_data_df, data_to_add)
      }
    }
  }

  for(i in 1:length(focal_files)){

    #get file info
    file <- paste0(input_dir,focal_files[i])

    #if file is empty, skip
    if(file.size(file) == 0){
      next
    }

    file_basename <- basename(file)

    #split up name into parts and extract info
    basename_split <- strsplit(file_basename, '_')
    group_id <- basename_split[[1]][1]
    ind_id <- basename_split[[1]][2]
    file_date <- gsub('.csv','',basename_split[[1]][length(basename_split[[1]])])

    #reformat date
    file_date <- as.Date(file_date, format = '%Y%m%d')

    #read in data from file
    if(tag_type == 'gipsy5'){
      curr_dat <- read.csv(file, sep = '\t', header=T)

      #reformat timestamp
      curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y %H:%M:%S')
    }
    if(tag_type=='axytrek'){
      curr_dat <- read.delim(file, sep = '\t', header=F)
      colnames(curr_dat) <- c('timestamp','location.lat','location.long','V4','V5','satellite.count','V7','V8')

      #reformat timestamp - axy treks can have multiple formats so need parse_date_time
      curr_dat$timestamp <- lubridate::parse_date_time(curr_dat$timestamp, tz = 'UTC', orders = c('%d/%m/%Y,%H:%M:%S', '%Y-%m-%d,%H:%M:%S'))
    }

    #filter out data outside of date range and with too few satellites
    idxs_keep <- which(as.Date(curr_dat$timestamp) == file_date & curr_dat$satellite.count >= min_satellites)
    curr_dat <- curr_dat[idxs_keep,]

    #number of rows
    n_rows <- length(idxs_keep)

    if(n_rows > 0){
      data_to_add <- data.frame(filename = rep(file_basename, n_rows),
                                individual.local.identifier = rep(ind_id, n_rows),
                                animal.group.id = rep(group_id, n_rows),
                                timestamp = curr_dat$timestamp,
                                location.long = curr_dat$location.long,
                                location.lat = curr_dat$location.lat,
                                satellites = curr_dat$satellite.count,
                                record_type = rep('focal', n_rows),
                                tag_type = rep(tag_type, n_rows)
      )

      #add to data frame
      gps_data_df <- rbind(gps_data_df, data_to_add)

    }
  }

  #name output files
  split_dir <- strsplit(input_dir,'/')
  output_name <- split_dir[[1]][length(split_dir[[1]])]

  #save gps data in data frame format
  save(gps_data_df, file = paste0(output_dir, output_name, '_raw_gps_data_df.RData'))

  #-----create xs and ys matrices------

  #if start date, end date, start time, or end time are not specified, need to infer them
  if(is.null(start_time) | is.null(end_time) | is.null(start_date) | is.null(end_date)){

    #get timestamps when at least half the tracked individuals are present
    timestamps_table <- table(gps_data_df$timestamp)
    n_inds <- length(unique(gps_data_df$individual.local.identifier))
    timestamps_many_tracked <- names(timestamps_table)[which(timestamps_table >= (n_inds / 2))]
    timestamps_many_tracked <- as.POSIXct(timestamps_many_tracked, tz = timezone)

    #if not specified, get start and end times and dates
    if(is.null(start_time) | is.null(end_time)){

      #get the hours associated with timestamps where at least half individuals were tracked
      hours_many_tracked <- table(lubridate::hour(timestamps_many_tracked))
      print(hours_many_tracked)

      #infer the start and end hour
      hours_many_tracked <- hours_many_tracked / max(hours_many_tracked)
      hours_many_tracked <- names(hours_many_tracked)[which(hours_many_tracked > 0.1)]
      start_hour <- min(as.numeric(hours_many_tracked))
      end_hour <- max(as.numeric(hours_many_tracked)) + 1

      #create start_time and end_time strings
      if(start_hour >= 10){
        start_time <- paste0(start_hour,':00:00')
      } else{
        start_time <- paste0('0',start_hour, ':00:00')
      }
      if(end_hour >= 10){
        end_time <- paste0(end_hour,':00:00')
      } else{
        end_time <- paste0('0',end_hour, ':00:00')
      }

      if(end_hour - start_hour != 3){
        warning(paste('for deployment', output_name, 'recording length found not to be 3 hours:', start_hour,'-',end_hour))
      }
    }

    #if not specified, get start and end dates
    if(is.null(start_date) | is.null(end_date)){

      #get the dates associated with timestamps where at least half individuals were tracked
      dates_many_tracked <- table(lubridate::date(timestamps_many_tracked))

      #infer the start and end hour
      dates_many_tracked <- dates_many_tracked / max(dates_many_tracked)
      dates_many_tracked <- names(dates_many_tracked)[which(dates_many_tracked > 0.1)]
      start_date <- as.character(min(dates_many_tracked))
      end_date <- as.character(max(dates_many_tracked))
    }
  }

  #convert to xs and ys matrices
  xy_file_path <- paste0(output_dir, output_name, '_xy_level0.RData')
  cocomo::reformat_movebank_to_matrix(movebank_data = gps_data_df,
                                      output_file_path = xy_file_path,
                                      seconds_per_time_step = seconds_per_time_step,
                                      start_date = start_date, end_date = end_date,
                                      start_time = start_time, end_time = end_time,
                                      utm_zone = utm_zone, hemisphere = hemisphere,
                                      output_utm = T, output_latlon = F, use_UTC = T)

  #lat lon matrices in a separate file
  xy_file_path <- paste0(output_dir, output_name, '_latlon_level0.RData')
  cocomo::reformat_movebank_to_matrix(movebank_data = gps_data_df,
                                      output_file_path = xy_file_path,
                                      seconds_per_time_step = seconds_per_time_step,
                                      start_date = start_date, end_date = end_date,
                                      start_time = start_time, end_time = end_time,
                                      utm_zone = utm_zone, hemisphere = hemisphere,
                                      output_utm = F, output_latlon = T, use_UTC = T)


}
