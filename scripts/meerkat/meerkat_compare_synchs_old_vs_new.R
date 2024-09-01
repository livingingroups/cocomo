#Compare synched calls from Baptiste's script with new synching script

#run this immediately after running the meerkat_audio_synch_script for a given group / year

new_synched_labels <- labels_synched
old_synched_labels <- data.frame()

#change this to directory holding old synch labels
synched_files_dir <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2017/3_labels_synched/'
old_synched_files <- list.files(synched_files_dir, recursive = T)

#get old labels
setwd(synched_files_dir)
for(i in 1:length(old_synched_files)){

  old_file <- old_synched_files[i]

  labels_old <- read.csv(old_file, header= T, sep = '\t')
  labels_old <- labels_old[,c('wavFileName','csvFileName','entryName','date','t0File','t0GPS_UTC', 'duration')]

  old_synched_labels <- rbind(old_synched_labels, labels_old)

}

#get time in file, duration, and wave file name without extension
old_synched_labels$t0_file <- sapply(old_synched_labels$t0File, cocomo::parse_audition_time)
old_synched_labels$duration <- sapply(old_synched_labels$duration, cocomo::parse_audition_time)
old_synched_labels$wav_file <- gsub('.wav','',old_synched_labels$wavFileName, fixed=T)
old_synched_labels$wav_file <- gsub('.WAV','',old_synched_labels$wav_file, fixed=T)

#get label unique ids to match - convert to lower case
old_synched_labels$label_unique_id <- paste(tolower(old_synched_labels$wav_file), round(old_synched_labels$t0_file,3), round(old_synched_labels$duration,3),sep='|')
new_synched_labels$label_unique_id <- paste(tolower(new_synched_labels$wav_file), round(new_synched_labels$t0_file,3), round(new_synched_labels$duration,3),sep='|')

#match to find calls that are the same, and compute the new synched UTC time to compare to the old from Baptiste's script output
old_synched_labels$new_t0_UTC <- new_synched_labels$t0_UTC[match(old_synched_labels$label_unique_id, new_synched_labels$label_unique_id)]

#difference between synched UTC time in old vs new labels
old_synched_labels$old_new_diff <- old_synched_labels$new_t0_UTC - as.POSIXct(old_synched_labels$t0GPS_UTC, tz = 'UTC')

#histogram of differences
hist(as.numeric(old_synched_labels$old_new_diff), breaks=seq(-1000,1000,.01), xlim = c(-.5,.5))
abline(v=-0.2, col = 'red')
abline(v=0.2, col = 'red')

quantile(abs(as.numeric(old_synched_labels$old_new_diff[which(old_synched_labels$date != '20170825' & old_synched_labels$wav_file != 'HM_VCVM001_HMB_R11_20170821-20170825_file_5_(2017_08_24-06_44_59)_ASWMUX221163')])), c(0.75,0.95), na.rm=T)

#75% and 95% quantile of absolute difference between old and new
quantile(abs(as.numeric(old_synched_labels$old_new_diff)), c(0.75, 0.95), na.rm=T)

#NOTES:

#Overall, the new synched labels and hte old ones are very similar. Most are within 200 ms of one another.

#A few differences occurred in 2017.
#The file HM_VCVM001_HMB_R11_20170821-20170825_file_5_(2017_08_24-06_44_59)_ASWMUX221163_LABEL_RY.csv' is
#synched different between old (Baptiste script) and new (Ari script). Confirmed manually that my script's results
#look more reasonable.
#All files on 20170825 were synched wrongly in Baptiste's script - second synch in table was used rather than first one,
#and confirmed my new way should be correct

#Overall quantiles of differences between old and new synched labels:
# HM2019 -  75%: 0.03648138, 95% 0.11307191 sec
# L2019 - 75% 0.0159, 95% 0.046995 sec
# HM2017 (excluding the messed up date/file - see above) - 75% 0.047, 95% 0.461

