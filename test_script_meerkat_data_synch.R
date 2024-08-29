#Synch files
library(devtools)
install_github('livingingroups/cocomo', force = T)
library(cocomo)

basedir <- '~/EAS_shared/meerkat/working/processed/acoustic/HM2019/2_labels_verified/'
path_to_synch_file <- '~/EAS_shared/meerkat/working/METADATA/total_synch_info.csv'
min_n_synchs <- 3
min_offset_outlier <- 0.5
min_frac_spanned_by_synchs = 0.2
make_plot <- T

all_files <- list.files(basedir, recursive = T)
path_to_synch_file

test <- list()

for(i in 1:length(all_files)){
  path_to_label_file <- paste0(basedir, all_files[i])
  test[[i]] <- meerkat_synch_audio_file_labels_to_UTC(path_to_label_file = path_to_label_file,
                                                      path_to_synch_file = path_to_synch_file,
                                                      min_offset_outlier = min_offset_outlier,
                                                      min_n_synchs = min_n_synchs,
                                                      min_frac_spanned_by_synchs = min_frac_spanned_by_synchs,
                                                      make_plot = make_plot)

}
