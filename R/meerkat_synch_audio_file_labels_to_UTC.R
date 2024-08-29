#' Synch labels within audio file to UTC time
#'
#' Reads in a label file (in Audition format) and a synch file, gets synch points,
#' and synchs all labels in the file to UTC. Outputs a table with an additional
#' column specifying timestamp_UTC. Currently only designed to work with meerkat data.
#'
#' The label file should be in the format of Audition labels. The Name column must contain
#' labels of the form 'synch H:MM:SS' or 'synch MM:SS' to specify the times of synch calls
#' as heard on the talking clock.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param path_to_label_file path to the label file from Audition
#' @param path_to_synch_file path to the synch file
#' @param min_offset_outlier minimum offset (in seconds) between fitted times and labeled talking clock times to be considered an outlier
#' @param min_n_synchs minimum number of synchs (after excluding outliers) to perform a fit
#' @param min_frac_spanned_by_synchs minimum fraction of the total file length (between first and last label time) spanned by synch calls to complete the synching
#' @param make_plot whether to also output a plot showing the synchs in time in recording vs talking clock time, with the final fit and outliers indicated
#'
#' @returns Returns a list containing `filename` (base name of the label file),
#' `synch_completed` (T or F whether the synch was completed successfully - if F, no other objects are returned in the list)
#' `labels_synched` (data frame with columns `Name`,`duration`,`start_UTC`,`start_time_in_file`,`filename`),
#' `synchs_used` (data frame containing synch labels used in the fit, and computed information about them)
#' and `outliers` (data frame with points labeled as outliers as well as filenames, plus some other columns)
#'
#' @importFrom lubridate parse_date_time
#'
#' @export
meerkat_synch_audio_file_labels_to_UTC <- function(path_to_label_file,
                                           path_to_synch_file = '~/EAS_shared/meerkat/working/METADATA/total_synch_info.csv',
                                           min_offset_outlier = 0.5,
                                           min_n_synchs = 3,
                                           min_frac_spanned_by_synchs = 0.2,
                                           make_plot = T){

  #commented out label file path is messed up somehow - check this file
  #path_to_label_file <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2019/2_labels_verified/20190625/HM_VHMF015_RTTB_R25_20190618-20190629_file_8_(2019_06_25-07_44_59)_255944_LL_BA.csv'
  #path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/2019_synch_info_all.csv'

  #read in labels and get times in file
  labels <- read.csv(path_to_label_file, sep = '\t', header =T, quote = "")
  if(ncol(labels)==1){
    stop(paste('label file cannot be imported as normal:', path_to_label_file))
  }

  #rename colnames in case they get messed up
  colnames(labels) <- c('Name','Start','Duration','Time.Format','Type','Description')

  #get start time and duration in sec into file
  labels$start_time_in_file <- sapply(labels$Start, FUN = function(x){return(cocomo::parse_audition_time(x))})
  labels$duration <- sapply(labels$Duration, FUN = function(x){return(cocomo::parse_audition_time(x))})

  #get date of label file
  if(grepl('SOUNDFOC', basename(path_to_label_file))){
    label_file_date <- stringr::str_match(basename(path_to_label_file), '20[0-9]{6}')
  } else{
    label_file_date <- stringr::str_match(basename(path_to_label_file), '\\(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]')
    label_file_date <- gsub('\\(', '', label_file_date)
    label_file_date <- gsub('\\_', '', label_file_date)
  }

  #read in synch file
  synch_info <- read.csv(path_to_synch_file, sep = ',')

  #get synch info for the given date
  synch_info_curr <- synch_info[which(synch_info$Date == label_file_date[1,1]),]
  #if there is no synch info for that date, throw error
  if(nrow(synch_info_curr)==0){
    stop(paste('synch info not found for specified date:', label_file_date))
  }
  #this is to deal with cases where there are comments indicating some lines relate to rover follows instead of group follows
  if(nrow(synch_info_curr)>1){
    synch_info_curr <- synch_info_curr[which(synch_info_curr$comments == '')[1],]
  }

  #TODO: FROM BAPTISTE'S SCRIPT
  #first we need to find in the synchInfo table the GPS time at which the talking clock was started
  #for certain days there are several entries in the synchInfo table for various reasons, so we need to select the appropriate one:
  # if( (date == "20190712" & indCode %in% c("VCVM001","VHMM007","VHMM008")) |  # day when there was a group split and the two sub-groups were followed with different talking clocks
  #     (date=="20170809" & (grepl("clockGap",file) | nbrFile>3)) |  # day when the synch calls were restarted mid-session
  #     (date == "20170825" & grepl("clockGap",file))){ # day when the talking clock was accidentally paused
  #   #in such cases we take the second line
  #   synchStart <- as.POSIXct(synchInfo$GPS.Time.UTC[match(date,synchInfo$Date)+1],tz="UTC") - as.difftime (substr(synchInfo$Speaker.Time[match(date,synchInfo$Date)+1],12,19))
  # }else{
  #   #otherwise it is the first line
  #   synchStart <- as.POSIXct(synchInfo$GPS.Time.UTC[match(date,synchInfo$Date)],tz="UTC") - as.difftime (substr(synchInfo$Speaker.Time[match(date,synchInfo$Date)],12,19))
  # }

  #TODO: throw out non multiples of 90 s

  #TODO: what to do if only one synch call?

  #get UTC offset from talking clock
  talking_clock_reference_time <- strsplit(synch_info_curr$Speaker.Time, ' ')[[1]][2]
  talking_clock_reference_time <- cocomo::parse_audition_time(talking_clock_reference_time)
  UTC_offset <- as.POSIXct(synch_info_curr$GPS.Time.UTC, tz = 'UTC') - as.numeric(talking_clock_reference_time)

  #find synchs
  synchs <- labels[grep('sync', labels$Name, ignore.case = T),]

  #get times of synchs, according to the talking clock (in sec)
  #talking clock labels are in auditon format, so can use audition parsing function
  if(nrow(synchs)>0){
    time_labels <- gsub('[a-z"]*','', synchs$Name, ignore.case=T) #remove a-z characters, slashes, and quotes
    synchs$talking_clock_time <- sapply(time_labels, FUN = function(x){return(cocomo::parse_audition_time(x))})

    #remove NA synchs
    if(sum(is.na(synchs$talking_clock_time))>0){
      synchs <- synchs[!is.na(synchs$talking_clock_time),]
    }
  }

  #count up how many synchs there are, and what fraction of the file duration they span
  n_synchs <- sum(!is.na(synchs$talking_clock_time))

  #if there are two few synchs, cannot synch file
  #need at least min_n_synchs
  if(n_synchs < min_n_synchs){
    warning(paste('cannot synch file:', path_to_label_file, '- too few synchs marked'))
    out <- list()
    out$filename <- basename(path_to_label_file)
    out$synch_completed <- F
    return(out)

  }

  #get time span of synchs
  if(sum(!is.na(synchs$start_time_in_file))>0){
    span_synchs <- max(synchs$start_time_in_file, na.rm=T) - min(synchs$start_time_in_file, na.rm=T)
  } else{
    span_synchs <- 0
  }

  #get fraction of file covered by the span of synchs
  span_file <- max(labels$start_time_in_file, na.rm=T) - min(labels$start_time_in_file, na.rm=T)
  frac_span_synchs <- span_synchs / span_file

  #if not enough of the file is covered by synchs, warning and return
  if(frac_span_synchs < min_frac_spanned_by_synchs){
    warning(paste('not enough of file is spanned by synchs - file:', basename(path_to_label_file)))
    out <- list()
    out$filename <- basename(path_to_label_file)
    out$synch_completed <- F
    return(out)
  }

  #fit a linear function to the relationship between time in file and talking clock time for synch points
  #synchs$start_time_in_file_sq <- synchs$start_time_in_file^2

  #remove outliers sequentially until there are no more
  redo_fit <- T
  n_non_nas <- sum(!is.na(synchs$talking_clock_time))
  outliers <- data.frame()
  n_outliers <- 0
  while(redo_fit & (n_non_nas - n_outliers) >= min_n_synchs){

    #perform the fit
    synch_fit <- lm('talking_clock_time ~ start_time_in_file', data = synchs)

    #get coefficients
    intercept <- synch_fit$coefficients[1]
    slope <- synch_fit$coefficients[2]
    #slope_sq <- synch_fit$coefficients[3]

    #predict times of synchs based on fit and calculate offsets
    synchs$predicted_start_time_talking_clock <- synchs$start_time_in_file*slope + intercept
    synchs$offset <- synchs$predicted_start_time_talking_clock - synchs$talking_clock_time

    #Find most extreme offset
    max_offset <- max(abs(synchs$offset), na.rm=T)
    max_offset_idx <- which(abs(synchs$offset) == max_offset)[1]

    #if the offset is greater than a threshold (indicating outliers), remove it and try again, and store it in the outliers table
    redo_fit <- F
    if(max_offset > min_offset_outlier){
      outliers <- rbind(outliers, synchs[max_offset_idx,])
      synchs <- synchs[-max_offset_idx,]
      redo_fit <- T #flag to indicate the fit needs to be redone
    }

    #number of outliers
    n_outliers <- nrow(outliers)

  }

  #Convert all times in file to talking clock time
  labels$start_time_talking_clock <- labels$start_time_in_file*slope + intercept

  #Convert all times from talking clock time to UTC using UTC offset
  labels$start_UTC <- labels$start_time_talking_clock + UTC_offset

  #predicted talking clock time for synchs and outliers
  if(nrow(outliers)>0){
    outliers$predicted_talking_clock_time <- outliers$start_time_in_file * slope + intercept
  }
  if(nrow(synchs)>0){
    synchs$predicted_talking_clock_time <- synchs$start_time_in_file * slope + intercept
  }

  #keep only relevant columns
  labels <- labels[,c('Name','duration','start_UTC','start_time_in_file')]

  #add a column for the file name
  labels$filename <- rep(basename(path_to_label_file), nrow(labels))

  #if specified, make plot of synch points and fit
  if(make_plot){
    synchs_and_outliers <- rbind(synchs, outliers)
    plot(synchs_and_outliers$start_time_in_file, synchs_and_outliers$talking_clock_time - synchs_and_outliers$predicted_talking_clock_time, xlab = 'File time (sec)', ylab = 'Offset (s)', main = basename(path_to_label_file))
    if(nrow(outliers)>0){
      points(outliers$start_time_in_file, outliers$talking_clock_time - outliers$predicted_talking_clock_time, col = 'red', pch = 19)
    }

    abline(h=0)
  }

  #if there are not enough remaining synchs, return and throw a warning
  if(nrow(synchs) < min_n_synchs){
    warn_string <- paste('not enough non-outlier synchs for file:', basename(path_to_label_file))
    warning(warn_string)
    out <- list()
    out$filename <- basename(path_to_label_file)
    out$synch_completed <- F
    return(out)
  }

  #if we had to remove outliers, throw a warning
  if(n_outliers > 0){
    warning(paste(n_outliers, 'outliers found in file:', basename(path_to_label_file)))
  }

  if(nrow(outliers)>0){
    outliers$filename <- basename(path_to_label_file)
  }
  synchs$filename <- basename(path_to_label_file)

  #format outliers and synchs to a simpler format
  if(nrow(outliers)>0){
    outliers <- outliers[,c('filename','Name','Start','Duration','start_time_in_file','talking_clock_time','predicted_talking_clock_time')]
  }
  if(nrow(synchs)>0){
    synchs <- synchs[,c('filename','Name','Start','Duration','start_time_in_file','talking_clock_time','predicted_talking_clock_time')]
  }

  #output
  out <- list()
  out$filename <- basename(path_to_label_file)
  out$synch_completed <- T
  out$labels_synched <- labels
  out$synchs_used <- synchs
  out$outliers <- outliers

  return(out)

}
