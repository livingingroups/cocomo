#' Synch labels within audio file to UTC time
#'
#' Reads in a label file (in Audition format) and a synch file, get synch points,
#' and synchs all labels in the file to UTC. Outputs a table with an additional
#' column specifying timestamp_UTC
#'
#' @param path_to_label_file
#' @param path_to_synch_file
#'
#'
#' @importFrom lubridate parse_date_time
#'
synch_audio_file_labels_to_UTC <- function(path_to_label_file, path_to_synch_file){

  #commented out label file path is messed up somehow - check this file
  #path_to_label_file <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2019/2_labels_verified/20190625/HM_VHMF015_RTTB_R25_20190618-20190629_file_8_(2019_06_25-07_44_59)_255944_LL_BA.csv'
  path_to_label_file <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2019/2_labels_verified/20190627/HM_VHMF001_HTB_R16_20190618-20190629_file_10_(2019_06_27-07_44_59)_275944_SE_BA.csv'
  path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/2019_synch_info_all.csv'

  #read in labels and get times in file
  labels <- read.csv(path_to_label_file, sep = '\t')
  labels$start_time_in_file <- sapply(labels$Start, FUN = function(x){return(cocomo::parse_audition_time(x))})
  labels$duration <- sapply(labels$Duration, FUN = function(x){return(cocomo::parse_audition_time(x))})

  #find synchs and beeps
  synchs <- labels[grep('sync', labels$Name, ignore.case = T),]
  beeps <- labels[grep('beep', labels$Name, ignore.case = T),]

  #get times of synchs and beeps, according to the talking clock
  if(nrow(synchs)>0){
    time_labels <- gsub('[A-Z]*','', synchs$Name, ignore.case=T)
    synchs$talking_clock_time <- sapply(time_labels, FUN = function(x){return(cocomo::parse_audition_time(x))})
  }
  if(nrow(beeps)>0){
    time_labels <- gsub('[A-Z]*','', beeps$Name, ignore.case=T)
    beeps$talking_clock_time <- sapply(time_labels, FUN = function(x){return(cocomo::parse_audition_time(x))})
  }

}
