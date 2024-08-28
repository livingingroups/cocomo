#' Synch labels within audio file to UTC time
#'
#' Reads in a label file (in Audition format) and a synch file, gets synch points,
#' and synchs all labels in the file to UTC. Outputs a table with an additional
#' column specifying timestamp_UTC. Currently only designed to work with meerkat data.
#'
#' TODO: Do something with beeps?
#'
#' The label file should be in the format of Audition labels. The Name column must contain
#' labels of the form 'synch H:MM:SS' or 'synch MM:SS' to specify the times of synch calls
#' as heard on the talking clock.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param path_to_label_file
#' @param path_to_synch_file
#' @param min_offset_outlier minimum offset (in seconds) between fitted times and labeled talking clock times to be considered an outlier
#' @param min_n_synchs minimum number of synchs (after excluding outliers) to perform a fit
#' @param min_frac_spanned_by_synchs minimum fraction of the total file length (between first and last label time) spanned by synch calls to complete the synching
#' @param make_plot whether to also output a plot showing the synchs in time in recording vs talking clock time, with the final fit and outliers indicated
#'
#' @importFrom lubridate parse_date_time
#'
#' @export
synch_audio_file_labels_to_UTC <- function(path_to_label_file,
                                           path_to_synch_file,
                                           min_offset_outlier = 2,
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

  #get UTC offset from talking clock
  talking_clock_reference_time <- strsplit(synch_info_curr$Speaker.Time, ' ')[[1]][2]
  talking_clock_reference_time <- cocomo::parse_audition_time(talking_clock_reference_time)
  UTC_offset <- as.POSIXct(synch_info_curr$GPS.Time.UTC, tz = 'UTC') - as.numeric(talking_clock_reference_time)

  #find synchs
  synchs <- labels[grep('sync', labels$Name, ignore.case = T),]

  #get times of synchs, according to the talking clock (in sec)
  #talking clock labels are in auditon format, so can use audition parsing function
  if(nrow(synchs)>0){
    time_labels <- gsub('[A-Z]*','', synchs$Name, ignore.case=T)
    synchs$talking_clock_time <- sapply(time_labels, FUN = function(x){return(cocomo::parse_audition_time(x))})
  }

  #count up how many synchs there are, and what fraction of the file duration they span
  n_synchs <- sum(!is.na(synchs$talking_clock_time))

  #if there are two few synchs, cannot synch file
  #need at least 2
  if(n_synchs < 2){
    warning(paste('cannot synch file:', path_to_label_file, '- too few synchs marked'))
    return(NULL)
  }

  #fit a quadratic function to the relationship between time in file and talking clock time for synch points
  synchs$start_time_in_file_sq <- synchs$start_time_in_file^2

  #perform the fit
  synch_fit <- lm('talking_clock_time ~ start_time_in_file + start_time_in_file_sq', data = synchs)
  intercept <- synch_fit$coefficients[1]
  slope <- synch_fit$coefficients[2]
  slope_sq <- synch_fit$coefficients[3]

  #Use same conversion on the synchs - calculate offsets
  synchs$predicted_start_time_talking_clock <- synchs$start_time_in_file*slope + synchs$start_time_in_file^2*slope_sq + intercept
  synchs$offset <- synchs$predicted_start_time_talking_clock - synchs$talking_clock_time

  #Check whether the fit is good enough - how many outliers are there
  outliers <- which(abs(synchs$offset) > min_offset_outlier)
  n_outliers <- length(outliers)

  #get time span of remaining synchs
  if(sum(!is.na(synchs$start_time_in_file))>0){
    span_synchs <- max(synchs$start_time_in_file, na.rm=T) - min(synchs$start_time_in_file, na.rm=T)
  } else{
    span_synchs <- 0
  }

  #get fraction of file covered by the span of synchs
  span_file <- max(labels$start_time_in_file, na.rm=T) - min(labels$start_time_in_file, na.rm=T)
  frac_span_synchs <- span_synchs / span_file

  #Convert all times in file to talking clock time, if there are enough synchs and if they span a large enough fraction of the labeled file
  if(nrow(synchs) > min_n_synchs & frac_span_synchs > min_frac_spanned_by_synchs){
    labels$start_time_talking_clock <- labels$start_time_in_file*slope + labels$start_time_in_file^2*slope_sq + intercept
  } else{
    stop('not enough synchs or not enough fraction of file covered by synchs - cannot synch file')
  }

  #Convert all times from talking clock time to UTC using UTC offset
  labels$start_UTC <- labels$start_time_talking_clock + UTC_offset

  #keep only relevant columns
  labels <- labels[,c('Name','duration','start_UTC','start_time_in_file')]

  #add a column for the file name
  labels$filename <- rep(basename(path_to_label_file), nrow(labels))

  #if specified, make plot of synch points (including outliers) and fit
  if(make_plot){
    plot(synchs$start_time_in_file, synchs$talking_clock_time, xlab = 'File time (sec)', ylab = 'Talking clock time', main = basename(path_to_label_file))
    if(length(outliers)>0){
      points(synchs$start_time_in_file[outliers], synchs$talking_clock_time[outliers], col = 'red', pch = 19)
    }
    x_fit <- seq(min(labels$start_time_in_file, na.rm=T), max(labels$start_time_in_file, na.rm=T), length.out=1000)
    y_fit <- intercept + x_fit*slope + x_fit^2*slope_sq
    lines(x_fit, y_fit)
  }

  if(n_outliers > 0){
    stop(paste0('outliers found - check file:', basename(path_to_label_file)))
  }

  return(labels)

}
