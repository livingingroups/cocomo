#' Synch labels within audio file to UTC time
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads in a label file (in Audition format) and a synch file, gets synch points,
#' and synchs all labels in the file to UTC. Outputs a table with an additional
#' column specifying timestamp_UTC. Currently only designed to work with meerkat data.
#'
#' @details
#' The label file should be in the format of Audition labels. The Name column must contain
#' labels of the form 'synch H:MM:SS' or 'synch MM:SS' to specify the times of synch calls
#' as heard on the talking clock.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param path_to_label_file path to the label file from Audition
#' @param path_to_synch_file path to the synch file
#' @param path_to_rawdata_dir path to directory where raw data is stored (for matching wav file names)
#' @param min_offset_outlier minimum offset (in seconds) between fitted times and labeled talking clock times to be considered an outlier
#' @param min_n_synchs minimum number of synchs (after excluding outliers) to perform a fit
#' @param min_frac_spanned_by_synchs minimum fraction of the total file length (between first and last label time) spanned by synch calls to complete the synching
#' @param make_plot whether to also output a plot showing the synchs in time in recording vs talking clock time, with the final fit and outliers indicated
#' @param handle_special_cases whether (`T` or `F`) to handle a few special cases in the synch info table, such as when the synch clock stopped or when two synch clocks were around due to a group split with rovers - these cases had to be hardcoded in. this parameter should always be set to `T` for meerkat data
#' @param quadratic_fit whether (`T` or `F`) to perform a quadratic fit to the synchs
#' @param remove_noisy_synchs whether (`T` or `F`) to remove synchs that are labeled as noisy (will remove everything that has an x, not case sensitive)
#'
#' @returns Returns a list containing:
#'
#' `filename`: basename of the label file
#'
#' `synch_completed`: T or F, whether the synch was completed successfully
#'
#' If `synch_completed == T`, also output:
#'
#' `labels_synched`: data frame with columns:
#'
#' `$label_unique_id` (unique id associated with a particular lable, constructed from `wav_file|label_name|t0_file|duration`)
#'
#' `$wav_file` (raw wav file name, without file extension)
#'
#' `$csv_file` (csv label file name without file extension)
#'
#' `$label_name` (name of the label)
#'
#' `$date` (date in format YYYYMMDD)
#'
#' `$id_code` (individual ID code, e.g. 'VCVM001')
#'
#' `$t0_file` (start time of label marker in file, in seconds into the file)
#'
#' `$duration` (label marker duration, in seconds)
#'
#' `$t0_UTC` (synched time of the label marker start in UTC)
#'
#' `$tmid_UTC` (synched time of the label marker midpoint in UTC)
#'
#' `$tf_UTC` (synched time of the label marker end in UTC)
#'
#' `synchs_used`: data frame containing synch labels used in the fit, and computed information about them
#'
#' `outliers`: data frame with points labeled as outliers as well as filenames, plus some other columns of info
#'
#' If `synch_completed == F`, also output:
#'
#' `reason_no_synch`: a string explaining why the file could not be synched
#'
#' @importFrom lubridate parse_date_time
#'
#' @export
meerkat_synch_audio_file_labels_to_UTC <- function(path_to_label_file,
                                           path_to_synch_file = '~/EAS_shared/meerkat/working/METADATA/total_synch_info.csv',
                                           path_to_rawdata_dir = '~/EAS_shared/meerkat/archive/rawdata/',
                                           min_offset_outlier = 0.5,
                                           min_n_synchs = 3,
                                           min_frac_spanned_by_synchs = 0.2,
                                           make_plot = T,
                                           handle_special_cases = T,
                                           quadratic_fit = F,
                                           remove_noisy_synchs = T){

  #----READ IN LABELS----

  #read in labels and get times in file
  labels <- read.csv(path_to_label_file, sep = '\t', header =T, quote = "")
  if(ncol(labels)==1){
    stop(paste('label file cannot be imported as normal:', path_to_label_file))
  }

  #rename colnames in case they get messed up on import
  colnames(labels) <- c('Name','Start','Duration','Time.Format','Type','Description')

  #remove any extraneous quotes and white space in label names
  labels$Name <- gsub('"', '', labels$Name, fixed = T)
  labels$Name <- trimws(labels$Name)

  #----GET LABEL FILE INFO----

  #get base name (without path) of label file
  label_file_name <- basename(path_to_label_file)

  #get date of label file
  if(grepl('SOUNDFOC', label_file_name)){
    label_file_date <- stringr::str_match(label_file_name, '20[0-9]{6}')
    label_file_date <- label_file_date[1,1]
  } else{
    label_file_date <- stringr::str_match(label_file_name, '\\(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]')
    label_file_date <- gsub('\\(', '', label_file_date)
    label_file_date <- gsub('\\_', '', label_file_date)[1,1]
  }

  #get info from file name
  label_file_name_no_ext <- gsub('.csv', '', label_file_name, fixed = T)
  label_file_split <- strsplit(label_file_name_no_ext, '_')[[1]]

  group_code <- label_file_split[1]
  id_code <- label_file_split[2]

  #get list of all wav files in the current year's raw data
  rawdata_subdir <- paste0('meerkat_movecomm_', substr(label_file_date,1,4))
  wav_files <- list.files(paste0(path_to_rawdata_dir, '/', rawdata_subdir), recursive = T, pattern = 'wav$', ignore.case = T)
  wav_files <- basename(wav_files)
  wav_files <- gsub('.wav', '', wav_files, fixed = T)
  wav_files <- gsub('.WAV', '', wav_files, fixed = T)

  #find matching wav file that corresponds to this label file
  #if can't find, wav_file_name will be NA
  wav_file_name <- NA
  if(length(wav_files)>0){
    for(i in 1:length(wav_files)){
      if(grepl(wav_files[i], label_file_name_no_ext, fixed = T)){
        wav_file_name <- wav_files[i]
        break
      }
    }
  } else{
    warning(paste('cannot find wav files in the specified directory'))
  }

  #if can't find matching wave file, throw a warning
  if(is.na(wav_file_name)){
    warning(paste('cannot find corresponding wav file for label file:', label_file_name))
  }

  #get start time and duration in sec into file
  labels$start_time_in_file <- sapply(labels$Start, FUN = function(x){return(cocomo::parse_audition_time(x))})
  labels$duration <- sapply(labels$Duration, FUN = function(x){return(cocomo::parse_audition_time(x))})

  #----GET SYNCH INFO----

  #read in synch file
  synch_info <- read.csv(path_to_synch_file, sep = ',')

  #get synch info for the given date
  synch_info_curr <- synch_info[which(synch_info$Date == label_file_date),]

  #if there is no synch info for that date, throw error
  if(nrow(synch_info_curr)==0){
    stop(paste('synch info not found for specified date:', label_file_date))
  }

  #handle some special cases from 2017 and 2019 when synch clock stopped or two synch clocks were present during a group split
  if(handle_special_cases){
    # day when there was a group split and the two sub-groups were followed with different talking clocks - here there are two lines and we need to choose the correct one
    if(label_file_date == '20190712'){
      if(id_code %in% c('VCVM001','VHMM007','VHMM008')){
        synch_info_curr <- synch_info_curr[2,]
      } else{
        synch_info_curr <- synch_info_curr[1,]
      }
    }

    # one of the days, the synch clock was reset midway through - I decided to not synch any labels in this date for now as it is too complicated
    if(label_file_date == '20170809'){
      warning(paste('cannot synch file:', path_to_label_file, '- files from 20170809 had an issue with synch calls'))
      out <- list()
      out$filename <- label_file_name
      out$synch_completed <- F
      out$reason_no_synch <- paste('synch clock restarted midway through - cannot synch')
      return(out)
    }

    #one of the days, synch calls were erroneous other than in a middle window - we will not consider synch calls recorded outside this window (see also below)
    if(label_file_date == '20170825'){
      min_talking_clock_time <- 40*60 #minimum talking clock time is 40 minutes for this day only!
      max_talking_clock_time <- 2*60*60 + 50*60 #maximum talking clock time is 2 hr 40 min for this day only
      synch_info_curr <- synch_info_curr[1,] #the first synch in the table is the relevant one
    }
  } else{
    #otherwise, if there are multiple rows, just take the first one (but this should not happen outside of special cases)
    if(nrow(synch_info_curr)>1){
      synch_info_curr <- synch_info_curr[1,]
    }
  }

  #get UTC offset from talking clock
  talking_clock_reference_time <- strsplit(synch_info_curr$Speaker.Time, ' ')[[1]][2]
  talking_clock_reference_time <- cocomo::parse_audition_time(talking_clock_reference_time)
  UTC_offset <- as.POSIXct(synch_info_curr$GPS.Time.UTC, tz = 'UTC') - as.numeric(talking_clock_reference_time)

  #find synchs
  synchs <- labels[grep('sync', labels$Name, ignore.case = T),]

  #remove noisy synchs
  if(remove_noisy_synchs){
    if(length(grep('x', synchs$Name, ignore.case = T)) > 0){
      synchs <- synchs[-grep('x', synchs$Name, ignore.case = T),]
    }
  }

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

  #if handling special cases, deal with the issue on 20170825
  #need to remove synchs from outside of the time window specified by min_talking_clock_time and max_talking_clock_time
  if(handle_special_cases){
    if(label_file_date == '20170825'){
      synchs <- synchs[which(synchs$talking_clock_time >= min_talking_clock_time & synchs$talking_clock_time <= max_talking_clock_time),]
    }
  }

  #count up how many synchs there are, and what fraction of the file duration they span
  n_synchs <- sum(!is.na(synchs$talking_clock_time))

  #if there are two few synchs, cannot synch file
  #need at least min_n_synchs
  if(n_synchs < min_n_synchs){
    warning(paste('cannot synch file:', path_to_label_file, '- too few synchs marked'))
    out <- list()
    out$filename <- label_file_name
    out$synch_completed <- F
    out$reason_no_synch <- paste('too few synchs:', n_synchs)
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
    warning(paste('not enough of file is spanned by synchs - file:', label_file_name))
    out <- list()
    out$filename <- label_file_name
    out$synch_completed <- F
    out$reason_no_synch <- paste('not enough of file is spanned by synchs:', frac_span_synchs)
    return(out)
  }

  #----FITTING SYNCH----

  #fit a linear or quadratic function to the relationship between time in file and talking clock time for synch points
  #remove outliers sequentially until there are no more

  if(quadratic_fit){
    synchs$start_time_in_file_sq <- synchs$start_time_in_file^2
    labels$start_time_in_file_sq <- labels$start_time_in_file^2
  }

  redo_fit <- T
  outliers <- data.frame()
  n_outliers <- 0
  while(redo_fit & nrow(synchs) >= min_n_synchs){

    #perform the fit
    if(quadratic_fit){
      synch_fit <- lm('talking_clock_time ~ start_time_in_file + start_time_in_file_sq', data = synchs)
    } else{
      synch_fit <- lm('talking_clock_time ~ start_time_in_file', data = synchs)
    }

    #get coefficients
    intercept <- synch_fit$coefficients[1]
    slope <- synch_fit$coefficients[2]

    if(quadratic_fit){
      slope_sq <- synch_fit$coefficients[3]
    }

    #predict times of synchs based on fit and calculate offsets
    if(quadratic_fit){
      synchs$predicted_start_time_talking_clock <- synchs$start_time_in_file_sq*slope_sq + synchs$start_time_in_file*slope + intercept
    } else{
      synchs$predicted_start_time_talking_clock <- synchs$start_time_in_file*slope + intercept
    }
    synchs$offset <- synchs$predicted_start_time_talking_clock - synchs$talking_clock_time

    #Find most extreme offset
    max_offset <- max(abs(synchs$offset), na.rm=T)
    max_offset_idx <- which(abs(synchs$offset) == max_offset)[1]

    #if the offset for a synch is greater than a threshold (indicating outliers), remove it and try again, and store it in the outliers table
    redo_fit <- F
    if(max_offset > min_offset_outlier){
      outliers <- rbind(outliers, synchs[max_offset_idx,])
      synchs <- synchs[-max_offset_idx,]
      redo_fit <- T #flag to indicate the fit needs to be redone
    }

    #number of outliers
    n_outliers <- nrow(outliers)

  }

  #if there were still outliers after remvoing the maximum number of synchs possible, file cannot be synched
  if(redo_fit){
    warning(paste('not enough synchs after removal of outliers - file:', label_file_name))
    out <- list()
    out$filename <- label_file_name
    out$synch_completed <- F
    out$reason_no_synch <- paste('not enough synchs (n =', nrow(synchs), ') after removal of outliers (n = ', (n_outliers),')')
    return(out)
  }

  #----SYNCH LABEL FILE----

  #Convert all times in file to talking clock time
  if(quadratic_fit){
    labels$start_time_talking_clock <- labels$start_time_in_file*slope + labels$start_time_in_file_sq*slope_sq + intercept
  } else{
    labels$start_time_talking_clock <- labels$start_time_in_file*slope + intercept
  }

  #Convert all times from talking clock time to UTC using UTC offset
  labels$t0_UTC <- labels$start_time_talking_clock + UTC_offset

  #predicted talking clock time for synchs and outliers
  if(nrow(outliers)>0){
    if(quadratic_fit){
      outliers$predicted_talking_clock_time <- outliers$start_time_in_file * slope + outliers$start_time_in_file_sq * slope_sq + intercept
    } else{
      outliers$predicted_talking_clock_time <- outliers$start_time_in_file * slope + intercept
    }
    outliers$offset <- outliers$talking_clock_time - outliers$predicted_talking_clock_time
  }
  if(nrow(synchs)>0){
    if(quadratic_fit){
      synchs$predicted_talking_clock_time <- synchs$start_time_in_file * slope + synchs$start_time_in_file_sq * slope_sq + intercept
    } else{
      synchs$predicted_talking_clock_time <- synchs$start_time_in_file * slope + intercept
    }
    synchs$offset <- synchs$talking_clock_time - synchs$predicted_talking_clock_time
  }

  #create relevant columns and name them
  labels$csv_file <- gsub('.csv$','',label_file_name, fixed = T)
  labels$wav_file <- wav_file_name
  labels$label_name <- labels$Name
  labels$date <- label_file_date
  labels$id_code <- id_code
  labels$group_code <- group_code
  labels$label_unique_id <- paste(labels$wav_file, labels$label_name, labels$start_time_in_file, labels$duration, sep = '|')
  labels$tf_UTC <- labels$t0_UTC + labels$duration
  labels$tmid_UTC <- labels$t0_UTC + (labels$duration / 2)
  labels$t0_file <- labels$start_time_in_file

  #keep only relevant columns
  labels <- labels[,c('label_unique_id','wav_file','csv_file','label_name','date','id_code','t0_file','duration','t0_UTC','tmid_UTC','tf_UTC')]

  #----PLOTTING----

  #if specified, make plot of synch points and fit
  if(make_plot){
    synchs_and_outliers <- rbind(synchs, outliers)
    yrange <- max(c(abs(synchs_and_outliers$offset), min_offset_outlier), na.rm=T)
    plot(synchs_and_outliers$start_time_in_file, synchs_and_outliers$talking_clock_time - synchs_and_outliers$predicted_talking_clock_time, xlab = 'File time (sec)', ylab = 'Offset (s)', main = label_file_name, ylim = c(-yrange, yrange), cex.main = 0.5)
    if(nrow(outliers)>0){
      points(outliers$start_time_in_file, outliers$talking_clock_time - outliers$predicted_talking_clock_time, col = 'red', pch = 19)
    }

    abline(h=0)
    abline(h=min_offset_outlier, col = 'red')
    abline(h=-min_offset_outlier, col = 'red')
  }

  #----OUTPUT----

  #if there are not enough remaining synchs, return and throw a warning
  if(nrow(synchs) < min_n_synchs){
    warn_string <- paste('not enough non-outlier synchs for file:', label_file_name)
    warning(warn_string)
    out <- list()
    out$filename <- label_file_name
    out$synch_completed <- F
    out$reason_no_synch <- paste('not enough non_outlier synchs:', nrow(synchs))
    return(out)
  }

  if(nrow(outliers)>0){
    outliers$filename <- label_file_name
  }
  synchs$filename <- label_file_name

  #format outliers and synchs to a simpler format
  if(nrow(outliers)>0){
    outliers <- outliers[,c('filename','Name','Start','Duration','start_time_in_file','talking_clock_time','predicted_talking_clock_time','offset')]
  }
  if(nrow(synchs)>0){
    synchs <- synchs[,c('filename','Name','Start','Duration','start_time_in_file','talking_clock_time','predicted_talking_clock_time','offset')]
  }

  #output
  out <- list()
  out$filename <- label_file_name
  out$synch_completed <- T
  out$reason_no_synch <- NA
  out$labels_synched <- labels
  out$synchs_used <- synchs
  out$outliers <- outliers

  return(out)

}
