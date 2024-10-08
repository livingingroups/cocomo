#' Import meerkat GPS data
#'
#' #Imports raw meerkat GPS data from Technosmart files (Gipsy 5 or Axy-Trek) assuming a standardized
#' file folder structure of `input_dir` and standardized naming schemes as done in the meerkat group collaring project.
#' Outputs a file containing the xs, ys, and timestamps matrices as well as another file with the GPS data table in Movebank format
#'
#' @param input_dir full path to input directory where all files from a given deployment are stored (e.g. `"~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2017/HM_2017_1/"`)
#' @param output_dir full path to the output directory where processed files will be saved
#' @param metadata_dir full path to the directory holding metadata files (GROUPYEAR_INDIVIDUAL_INFO.txt)
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
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @importFrom lubridate hour
#' @export
meerkat_import_meerkat_gps_data <- function(input_dir, output_dir,
                                    metadata_dir = '~/EAS_shared/meerkat/working/METADATA/',
                                    tag_type,
                                    start_date = NULL, end_date = NULL,
                                    start_time = NULL, end_time = NULL,
                                    min_satellites = 5,
                                    utm_zone = 34, hemisphere = 'south',
                                    seconds_per_time_step = 1,
                                    timezone = 'UTC'){

  #make sure input directory ends in '/'
  if(substr(input_dir,nchar(input_dir),nchar(input_dir)) != '/'){
    input_dir <- paste0(input_dir,'/')
  }

  #make sure tag type is specified
  if(!tag_type %in% c('gipsy5','axytrek')){
    stop('must specify tag_type as either gipsy5 or axytrek')
  }

  #get relevant files
  if(tag_type == 'gipsy5'){
    #get all files
    all_files <- list.files(input_dir, pattern = ".*csv$", recursive = T)

    #add in any garmin files, since they won't be csvs
    garmin_files <- list.files(input_dir, pattern='GARMIN', recursive = T)
    all_files <- unique(c(all_files, garmin_files))

    #get collar files and focal files
    collar_files <- all_files[grep('COLLAR[S]{0,1}/GPS', all_files)]
    focal_files <- all_files[grep('FOCAL', all_files)]
  }

  if(tag_type == 'axytrek'){
    all_files <- list.files(input_dir, pattern = ".*txt$", recursive = T)
    collar_files <- all_files[grep('COLLAR[S]{0,1}/GPS', all_files)]
    focal_files <- all_files[grep('FOCAL', all_files)]
  }

  #check collar files for the right number of subdirectories (there should be 4)
  #ignore any files with the wrong number of subdirectories
  n_subdirs_collar <- sapply(collar_files, FUN = function(x){return(length(strsplit(x,'/')[[1]]))})
  collar_files <- collar_files[which(n_subdirs_collar == 4)]

  #check focal files for the right number of subdirectories
  n_subdirs_focal <- sapply(focal_files, FUN = function(x){return(length(strsplit(x,'/')[[1]]))})
  focal_files <- focal_files[which(n_subdirs_focal == 4)]

  #remove any error log files
  errorlogs_collar <- grep('errorLog', collar_files)
  errorlogs_focal <- grep('errorLog', focal_files)
  if(length(errorlogs_collar)>0){
    collar_files <- collar_files[-errorlogs_collar]
  }
  if(length(errorlogs_focal)>0){
    focal_files <- focal_files[-errorlogs_focal]
  }

  #--------Read in metadata and store in data frame----------

  #determine which metadata file is needed
  ind_info_files <- list.files(metadata_dir, recursive = T, pattern = 'INDIVIDUAL_INFO.txt')
  ind_info_groupyears <- sapply(ind_info_files, FUN =function(x){return(strsplit(x,'_')[[1]][1])})
  ind_info_groups <- gsub('[0-9]{4}', '', ind_info_groupyears)
  ind_info_years <- gsub('[A-Z]{1,2}', '', ind_info_groupyears)
  ind_info_groupyears <- paste0(ind_info_groups, '_',ind_info_years)
  for(i in 1:length(ind_info_groupyears)){
    if(grepl(ind_info_groupyears[i], input_dir)){
      metadata_file <- ind_info_files[i]
    }
  }

  #read in metadata
  ids <- read.delim(paste0(metadata_dir,metadata_file), sep = '\t', header=T)

  #--------Read in all GPS data and create data frame------
  gps_data_df <- data.frame()

  n_focal_files <- length(focal_files)
  n_collar_files <- length(collar_files)
  files_to_read <- c(focal_files, collar_files)
  recording_types <- c(rep('focal',n_focal_files),rep('collar',n_collar_files))

  if(length(files_to_read)>0){
    for(i in 1:length(files_to_read)){

      #get file info
      file <- paste0(input_dir,files_to_read[i])

      #if file is empty, skip
      if(file.size(file) == 0){
        next
      }

      file_basename <- basename(file)

      #manually fix issue where 2023 was incorrectly typed as 2923 in BS_2023 data
      if(basename(input_dir)=='BS_2023'){
        file_basename <- gsub('2923','2023',file_basename)
      }

      #split up name into parts and extract ind id
      basename_split <- strsplit(file_basename, '_')
      group_id <- basename_split[[1]][1]
      ind_id <- basename_split[[1]][2]

      #get dates
      if(recording_types[i] == 'collar'){

        #get file dates
        file_dates <- regmatches(file_basename, regexpr('20[0-9]{6}[-_]20[0-9]{6}', file_basename))
        file_start_date <- strsplit(file_dates,'[-_]')[[1]][1]
        file_end_date <- strsplit(file_dates,'[-_]')[[1]][2]

        #reformat dates
        file_start_date <- as.Date(file_start_date, format = '%Y%m%d')
        file_end_date <- as.Date(file_end_date, format = '%Y%m%d')
      }
      if(recording_types[i] == 'focal'){

        #get file date
        file_date <- regmatches(file_basename, regexpr('20[0-9]{6}', file_basename))

        #reformat date
        file_date <- as.Date(file_date, format = '%Y%m%d')
        file_start_date <- file_end_date <- file_date
      }

      #if file is a garmin file, read it in differently
      tag_type_curr <- tag_type
      if(grepl('GARMIN',file)){
        tag_type_curr <- 'garmin'

        #if can't read in file, skip. otherwise, read in the file
        curr_dat <- tryCatch(read.delim(file), error = function(e){return(NULL)})
        if(is.null(curr_dat)){
          next
        }

        #there are two types of garmin formats. deal with first format (5 cols)
        if(ncol(curr_dat) == 5){

          #in the first format, Longitude and Latitude columns are actually UTM Eastings and Northings respectively - go figure
          #so replace them with lats and lons
          easts_norths <- cbind(curr_dat$Longitude, curr_dat$Latitude)
          lons_lats <- cocomo::utm_to_latlon(easts_norths, utm_zone = utm_zone, hemisphere = hemisphere)
          curr_dat$Longitude <- lons_lats[,1]
          curr_dat$Latitude <- lons_lats[,2]

          colnames(curr_dat) <- c('location.long','location.lat','altitude','timestamp','satellite.count')

          #add a column for satellites and fill it with min_satellites to be compatible with later code
          curr_dat$satellite.count <- min_satellites

          #reformat timestamp
          curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = timezone, format = '%Y-%m-%d %H:%M:%S')

        }

        if(ncol(curr_dat) == 27){
          curr_dat <- curr_dat[,c('time','lon','lat')]
          curr_dat$timestamp <- as.POSIXct(curr_dat$time, format = '%Y-%m-%dT%H:%M:%SZ', tz = timezone)
          colnames(curr_dat) <- c('time_old','location.long','location.lat','timestamp')

          #add a column for satellites and fill it with min_satellites to be compatible with later code
          curr_dat$satellite.count <- min_satellites
        }
      }

      #read in data from file
      if(tag_type_curr=='gipsy5'){
        curr_dat <- read.csv(file, sep = '\t', header=T)

        #reformat timestamp
        curr_dat$timestamp <- as.POSIXct(curr_dat$timestamp, tz = 'UTC', format = '%d/%m/%Y %H:%M:%S')

      }
      if(tag_type_curr=='axytrek'){
        curr_dat <- cocomo::import_axytrek_gps_file(input_file_path = file)
      }

      #if file was empty, skip
      if(is.null(curr_dat)){
        next
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
                                  record_type = rep(recording_types[i], n_rows),
                                  tag_type = rep(tag_type_curr, n_rows)
                                  )

        #add to data frame
        gps_data_df <- rbind(gps_data_df, data_to_add)
      }
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

      #infer the start and end hour
      hours_many_tracked <- hours_many_tracked / max(hours_many_tracked)
      hours_many_tracked <- names(hours_many_tracked)[which(hours_many_tracked > 0.25)]
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
                                      ids = ids,
                                      seconds_per_time_step = seconds_per_time_step,
                                      start_date = start_date, end_date = end_date,
                                      start_time = start_time, end_time = end_time,
                                      utm_zone = utm_zone, hemisphere = hemisphere,
                                      output_utm = T, output_latlon = F, use_UTC = T)

  #lat lon matrices in a separate file
  xy_file_path <- paste0(output_dir, output_name, '_latlon_level0.RData')
  cocomo::reformat_movebank_to_matrix(movebank_data = gps_data_df,
                                      output_file_path = xy_file_path,
                                      ids = ids,
                                      seconds_per_time_step = seconds_per_time_step,
                                      start_date = start_date, end_date = end_date,
                                      start_time = start_time, end_time = end_time,
                                      utm_zone = utm_zone, hemisphere = hemisphere,
                                      output_utm = F, output_latlon = T, use_UTC = T)


}
