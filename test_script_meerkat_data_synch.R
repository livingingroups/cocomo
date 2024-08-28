#Synch files

basedir <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2017/2_labels_verified/'
path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/2017_synch_info_all.csv'

all_files <- list.files(basedir, recursive = T)
path_to_synch_file

for(i in 1:length(all_files)){
  path_to_label_file <- paste0(basedir, all_files[i])
  test <- synch_audio_file_labels_to_UTC(path_to_label_file = path_to_label_file, path_to_synch_file = path_to_synch_file)

}
