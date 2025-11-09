#' Auto label synch times
#' Automatic labeling of synch calls that are not labeled with a time, assuming they should be close to multiples of 90 sec apart
#' File must have at least one fully labeled synch - and make sure this is correct!
#' TODO: add more details on how this works here
#' TODO: change max drift to be per time in file instead of absolute
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param label_file path to label file
#' @param labeled_synchs data frame of labeled synchs associated with that file
#' @param outdir output directory
#' @param machine_labelswhether the labels are machine generated (T) or not (F), defaults to T
#' @param likeli_thresh likelihood threshold (default to 0.6) - only use synchs above this threshold (only relevant if machine_labels = T)
#' @param max_drift maximum amount of drift allowed to still consider a synch call to be correctly positioned
#'
#' @returns Saves a new label file to the selected directory, with the original filename with _autosync.csv appended
#' Also returns the name of the output file (invisibly)
#' @export
meerkat_auto_label_synch_times <- function(label_file, labeled_synchs, outdir, machine_labels = T, likeli_thresh = 0.6, max_drift = 1){

  #read labels
  labels <- read.csv2(file = label_file, header=T, sep = '\t', stringsAsFactors=F)

  #get only synchs
  synchs <- labels[grep('sync',labels$Name),]

  #if machine labels, filter to only high likelihood synchs
  if(machine_labels){
    #get likelihoods
    synchs$likeli <- as.numeric(substring(synchs$Description,1,5))
    #filter only to high likelihood synchs
    synchs <- synchs[which(synchs$likeli > likeli_thresh),]
  }

  #time into file in seconds
  synchs$start_sec <- sapply(X = synchs$Start, FUN = cocomo::parse_audition_time)

  #get time differences between consecutive synchs
  dts <- diff(synchs$start_sec)
  synchs$dt_next <- c(dts, NA)
  synchs$dt_prev <- c(NA, dts)

  #get rid of any synchs that are not close enough to the expected timeline
  synchs <- synchs[which(synchs$dt_next > 30 & synchs$dt_next > 30),] #remove close together synchs
  synchs$offtimeline_next <- pmin(synchs$dt_next %% 90, 90 - synchs$dt_next %% 90)
  synchs$offtimeline_prev <- pmin(synchs$dt_prev %% 90, 90 - synchs$dt_prev %% 90)
  synchs <- synchs[which(synchs$offtimeline_next <= max_drift & synchs$offtimeline_prev <= max_drift),]

  #get correct time difference between synchs, assuming 90 sec differences between them and missing some in middle
  synchs$file_time_since_first_synch <- (synchs$start_sec - synchs$start_sec[1])
  synchs$real_time_since_first_synch <- round(synchs$file_time_since_first_synch / 90) * 90

  #get offset
  synchs$offset <- synchs$file_time_since_first_synch - synchs$real_time_since_first_synch

  #find any labeled synchs (synchs that have numbers in them)
  #labeled_synchs <- synchs[grep('[0-9]', synchs$Name),]
  #if(nrow(labeled_synchs)==0){
  #  stop('no labeled synchs found - need at least one labeled synch')
  #}
  #synchtime <- gsub("[A-Za-z[:space:]]", "", labeled_synchs$Name)
  #labeled_synchs$talking_clock_time <- sapply(synchtime, cocomo::parse_audition_time)

  #use first labeled synch as reference point - get talking clock time of first synch
  ref_talking_clock_time <- labeled_synchs$synch_time_sec[1]
  ref_file_time <- labeled_synchs$time_into_file[1]
  ref_time_offset <- ref_talking_clock_time - ref_file_time
  first_talking_clock_time <- round((synchs$start_sec[1] + ref_time_offset)/90)*90

  #label all the synchs in the file relative to the reference synch
  synchs$inferred_talking_clock_time <- synchs$real_time_since_first_synch + first_talking_clock_time

  synchs$time_lab <- sapply(synchs$inferred_talking_clock_time, function(seconds){return(sprintf("%02d:%02d:%02d",
                            seconds %/% 3600,
                            (seconds %% 3600) %/% 60,
                            seconds %% 60))})
  synchs$new_name <- paste0('synch ', synchs$time_lab)

  #add labeled synchs back into original table and save file
  labels_match_str <- paste0(labels$Start,'_',labels$Duration)
  synchs_match_str <- paste0(synchs$Start,'_',synchs$Duration)
  labels$Name[match(synchs_match_str,labels_match_str)] <- synchs$new_name

  #save output file
  base_filename <- tools::file_path_sans_ext(basename(label_file))
  outfile <- paste0(outdir, base_filename,'_autosync.csv')

  write.table(x = labels, file = outfile, sep = '\t', quote = F)

  invisible(outfile)

}

