#Synch files
library(devtools)
install_github('livingingroups/cocomo', force = T)
library(cocomo)

basedir <- '~/EAS_shared/meerkat/working/processed/acoustic/HM20'
path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/2017_synch_info_all.csv'

all_files <- list.files(basedir, recursive = T)
path_to_synch_file

test <- list()

for(i in 1:length(all_files)){
  path_to_label_file <- paste0(basedir, all_files[i])
  test[[i]] <- cocomo::synch_audio_file_labels_to_UTC(path_to_label_file = path_to_label_file, path_to_synch_file = path_to_synch_file)

}
