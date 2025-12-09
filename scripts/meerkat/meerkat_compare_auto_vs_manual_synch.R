#Compare auto synch vs manual synch for a given file

synch_info_file <- '/mnt/EAS_shared/meerkat/working/METADATA/2022_synch_info_all.csv'
load('/mnt/EAS_shared/meerkat/working/processed/acoustic/synched_animal2vec_predictions/labeled_animal2vec_synchs_2022_old.RData')

files <- files_table$pred_file[which(files_table$status=='done')]
#files <- files[!grepl('SOUNDFOC',files)]

#path to directories containing verified, manually labeled files
#manual_dirs <- c('/mnt/EAS_ind/astrandburg/meerkat_synch_test/Synch checked/HM2017/corrected_csv/',
#                 '/mnt/EAS_ind/astrandburg/meerkat_synch_test/Synch checked/HM2019/corrected_csv/',
#                 '/mnt/EAS_ind/astrandburg/meerkat_synch_test/Synch checked/L2019/corrected_csv/')
manual_dirs <- c('/mnt/EAS_ind/astrandburg/meerkat_synch_test/files_for_SOROKA_fine_tuning/')
manual_synch_files <- list.files(manual_dirs, recursive=T, full.names = T)

#get the relevant auto synch file
diffs <- list()
frac_outliers_auto <- outliers_manual <- outliers_auto <- synched_auto <- synched_manual <- rep(NA, length(files))
for(i in 1:length(files)){
  print(i)
  print(length(files))
  auto_file <- files[i]
  file_base <- basename(auto_file)
  file_base_no_ext <- tools::file_path_sans_ext(file_base)

  #find the matching manual synch file for comparison
  f <- grep(file_base_no_ext, manual_synch_files, fixed = T)
  if(length(f)==0){
    diffs[[i]] <- NULL
    next
  }
  manual_synch_file <- manual_synch_files[f]
  print(manual_synch_file)

  #find the relevant synch info from the synchs_all table, and auto label the rest of the synchs
  synchs_file <- synchs_all[which(basename(synchs_all$pred_file) == basename(auto_file)),]
  outfile <- cocomo::meerkat_auto_label_synch_times(auto_file, labeled_synchs = synchs_file, outdir = '/mnt/EAS_ind/astrandburg/meerkat_synch_test/autosync_files/')

  #create a comparable file that has all the (non-synch) labels from the autosynched file but draws the synchs from the manually labeled files
  manual_labs <- read.csv(file = manual_synch_file, sep = '\t', header = T, quote = "")
  if(ncol(manual_labs) != 6){
    diffs[[i]] <- NULL
    next
  }
  manual_synchs <- manual_labs[which(grepl('synch',manual_labs$Name) & !grepl('synch x',manual_labs$Name)),]
  auto_labs <- read.csv(auto_file, sep = '\t')
  combined_labs <- rbind(auto_labs, manual_synchs)
  manual_filename <- paste0(gsub('_autosync.csv', '',outfile, fixed = T), '_manualsync.csv')
  write.table(combined_labs, manual_filename, sep = '\t', quote = F)

  labels_autosync <- tryCatch(cocomo::meerkat_synch_audio_file_labels_to_UTC(path_to_label_file = outfile, quadratic_fit = T, path_to_synch_file = synch_info_file),
                              error = function(e) {
                                message("Skipped due to error: ", e$message)
                                return(NULL)
                              }
  )

  labels_manualsync <- tryCatch(cocomo::meerkat_synch_audio_file_labels_to_UTC(path_to_label_file = manual_filename, quadratic_fit = T, path_to_synch_file = synch_info_file),
                                error = function(e) {
                                  message("Skipped due to error: ", e$message)
                                  return(NULL)
                                }
  )

  #store whether it was synched and number of outliers
  if(!is.null(labels_manualsync)){
    synched_manual[i] <- labels_manualsync$synch_completed
    if(synched_manual[i] == T){
      outliers_manual[i] <- nrow(labels_manualsync$outliers)
    }
  }

  if(!is.null(labels_autosync)){
    synched_auto[i] <- labels_autosync$synch_completed
    if(synched_auto[i] == T){
      outliers_auto[i] <- nrow(labels_autosync$outliers)
      frac_outliers_auto[i] <- nrow(labels_autosync$outliers) / (nrow(labels_autosync$synchs_used) + nrow(labels_autosync$outliers))
    }
  }


  if(!is.null(labels_manualsync) & !is.null(labels_autosync)){

    manual_labs <- labels_manualsync$labels_synched
    auto_labs <- labels_autosync$labels_synched

    #compare timeline offsets for the two files - only the non-synch calls
    manual_labs <- manual_labs[!grepl('synch',manual_labs$label_name),]
    auto_labs <- auto_labs[!grepl('synch',auto_labs$label_name),]

    diffs[[i]] <- as.numeric(difftime(manual_labs$t0_UTC, auto_labs$t0_UTC, tz = 'UTC', units = 'sec'))
  } else{
    diffs[[i]] <- NULL
  }

}

#collect up summary data
maxes <- mins <- medians <- uppers <- lowers <- median_abs <- rep(NA, length(files))
for(i in 1:length(files)){
  if(!is.null(diffs[[i]])){
    maxes[i] <- max(diffs[[i]], na.rm=T)
    mins[i] <- min(diffs[[i]], na.rm=T)
    medians[i] <- median(diffs[[i]], na.rm=T)
    uppers[i] <- quantile(diffs[[i]], 0.975, na.rm=T)
    lowers[i] <- quantile(diffs[[i]], 0.025, na.rm=T)
    median_abs[i] <- median(abs(diffs[[i]]),na.rm=T)
  }
}

#Make plots
par(mar=c(4,12,1,1))
plot(median_abs, 1:length(median_abs), xlab = 'Median time difference (s)', ylab = '', yaxt='n', pch=19, col = 'red', cex = 0.5, xlim=c(0,2))
axis(side = 2, at = 1:length(medians), labels = tools::file_path_sans_ext(basename(files)), las = 2, cex.axis = 0.3)
abline(v=0,lwd=2)
abline(v=c(-.2,.2),lty=2)
abline(h=1:length(median_abs),lwd=0.2,col='gray')

synched_manual_but_not_auto <- which(synched_manual & !synched_auto)
synched_auto_but_not_manual <- which(synched_auto & !synched_manual)
if(length(synched_manual_but_not_auto) > 0){
  abline(h=synched_manual_but_not_auto, col = 'blue')
}
if(length(synched_auto_but_not_manual) > 0){
  abline(h=synched_auto_but_not_manual, col = 'darkgreen')
}

par(mar=c(5,4,1,1))
plot(outliers_auto, median_abs, pch = 19, cex = 0.3, xlab = 'Number of outliers (auto synch)', ylab = 'Median absolute time difference (s)')
abline(h=0.2, lty = 2)

plot(frac_outliers_auto, median_abs, pch = 19, cex = 0.3, xlab = 'Fraction of outliers (auto synch)', ylab = 'Median absolute time difference (s)')
abline(h=0.2, lty = 2)

which(median_abs > 1)

#for 2022, mismatching files (> 1 sec median absolute difference between the manual and automatic synchs) are:
#"NQ_VNQM029_SHLS_S016_20220612-20120616_FL3_(2022_06_13-07_30_00).csv" - off by 3600
# this one has an offset of an hour - the 3 times labeled using the script for manually labeling times were off by 1 hr (3 vs 2)
#"RW_VRWF002_LRRT_S009_20220712-20120619_FL1_(2022_07_12-07_30_00).csv" - off by 843
# in this one the manual synch looks really wonky
#"RW_VRWF010_MBRT_S021_20220712-20120619_FL2_(2022_07_13-07_30_00).csv" - off by 5
# here the auto synch has fewer points. the offset is not that huge though
#"RW_VRWF010_MBRT_S021_20220712-20120619_FL6_(2022_07_17-07_30_00).csv" - off by 1.1
# here the auto synch has not that many synch points
