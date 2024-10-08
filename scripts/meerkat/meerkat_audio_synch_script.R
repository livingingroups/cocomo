#This script can be used to synchronize all audio files in a folder
#It currently does not save the synched files anywhere - it is just for testing
#It creates a data frame of outliers to look into and a list of unsynched files and reasons they couldn't be synched


library(devtools)
install_github('livingingroups/cocomo', force = T)
library(cocomo)

#path to folder containing files to synch
basedir <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2017/2_labels_verified/'

#path to synch metadata file
path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/total_synch_info.csv'

#path to rawdata directory
path_to_rawdata_dir <- '~/EAS_shared/meerkat/archive/rawdata/'

#parameters
min_n_synchs <- 3
min_offset_outlier <- 0.2
min_frac_spanned_by_synchs = 0.2
make_plot <- T
handle_special_cases <- T

#get all files in directory
all_files <- list.files(basedir, recursive = T)

#initialize data frame to hold outliers from all files
outliers <- data.frame()

#initialize data frame to hold unsynched files
unsynched_files <- data.frame()

#all labels
labels_synched <- data.frame()

#loop over files and try to synch them - currently not saving output anywhere
for(i in 1:length(all_files)){
  path_to_label_file <- paste0(basedir, all_files[i])
  out <- cocomo::meerkat_synch_audio_file_labels_to_UTC(path_to_label_file = path_to_label_file,
                                                      path_to_synch_file = path_to_synch_file,
                                                      path_to_rawdata_dir = path_to_rawdata_dir,
                                                      min_offset_outlier = min_offset_outlier,
                                                      min_n_synchs = min_n_synchs,
                                                      min_frac_spanned_by_synchs = min_frac_spanned_by_synchs,
                                                      make_plot = make_plot,
                                                      handle_special_cases = handle_special_cases)

  if(out$synch_completed){
    if(nrow(out$outliers)>0){
      outliers <- rbind(outliers, out$outliers)
    }
    labels_synched <- rbind(labels_synched, out$labels_synched)
  } else{
    row <- data.frame(filename = out$filename, reason = out$reason_no_synch)
    unsynched_files <- rbind(unsynched_files, row)
  }

}

