#Script for labeling automatically-produced synchs from animal2vec
#Runs on RStudio server and accesses files from the data server

#Instructions:
#Listen to the audio clip and label the synch time in the format H:MM:SS
#You can replay the clip by clicking on the play button in the Viewer
#If you cannot understand the synch call, simply hit enter to skip to the next one
#If you enter anything other than the correct format (or one of the specified commands, see below), the current synch call wall automatically repeat
#You can go back to the previous clip by typing "back"
#If you can tell the collar is not on a meerkat, type "notonmeerkat"
#If you'd like to flag the file as problematic, type "flag" (you will then skip it, and it will be specially flagged)
#If you'd like to skip a file for some other reason, type "skipfile" (you will skip the file, and it will be labeled as skipped)
#
#If after 20 attempts to label synchs, you have not labeled at least 3, the file will automatically be skipped and labeled as "couldnotsynch"
#
#After you finish labeling a file, the system will check whether the synchs you have labeled make sense together (are approx the right time difference apart).
#If they are not, you will get a warning and you will have to repeat that file.
#In this case, try the file again, listening carefully to the synchs. Potentially try labeling other synchs than those you tried before.
#If after several tries you still receive a warning, you should flag the file by typing "flag"
#
#You may quit at any time by pressing escape. Your progress will be saved.

library(tuneR)
library(base64enc)
library(htmltools)

#User specifies what year
year <- readline('What year would you like to label? ')

predictions_dir <- paste0('/mnt/EAS_shared/meerkat/working/processed/acoustic/animal2vec_predictions/large_model_v2/', year, '/csv/')
rawdata_dir <- '/mnt/EAS_shared/meerkat/archive/rawdata/'
outdir <- '/mnt/EAS_shared/meerkat/working/processed/acoustic/synched_animal2vec_predictions/'
outfile <- paste0(outdir, 'labeled_animal2vec_synchs_',year,'.RData') #output file to save file names and associated synchs that have been labeled
outfile_backup <- paste0(outdir, 'labeled_animal2vec_synchs_',year,'_backup_',Sys.Date(), '.RData')
max_to_try <- 20 #max number of synchs to try before giving up on that file
n_synchs_to_label <- 3 # number of synchs to label per file
pad_start <- 0.5
pad_end <- 2
max_drift_per_hr <- 5 #maximum drift per hour to allow (otherwise need to relabel synchs)

#FUNCS
#Function to play audio segment in browser (RStudio Server media viewer)
#Note: this will save some tmp wav files in the tmp folder
play_audio_segment <- function(file, from = 0, to = 5, units = "seconds", autoplay = TRUE) {
  stopifnot(file.exists(file))

  # --- Step 1: Read only the portion you need ---
  seg <- readWave(file, from = from, to = to, units = units)

  # --- Step 2: Write segment to a temp WAV file ---
  tmpfile <- tempfile(fileext = ".wav")
  writeWave(seg, tmpfile)

  # --- Step 3: Embed audio in HTML ---
  src <- paste0(
    "data:audio/wav;base64,",
    base64encode(tmpfile)
  )

  # Explicit autoplay attempt
  audio_tag <- tags$html(
    tags$body(
      tags$audio(
        src = src,
        controls = NA,
        autoplay = if (autoplay) "autoplay" else NULL
      )
    )
  )

  html_file <- tempfile(fileext = ".html")
  save_html(audio_tag, html_file)

  # --- Step 4: Display in RStudio Viewer or browser ---
  viewer <- getOption("viewer")
  if (!is.null(viewer)) viewer(html_file) else utils::browseURL(html_file)

  invisible(html_file)
}

#MAIN

#make sure the specified year is available in predictions
if(!dir.exists(predictions_dir)){
  stop('predictions for the specified year are not available - try again')
}

#print instructions
cat('Synch Labeling Instructions: \n')
cat('Listen to the audio clip and label the synch time in the format H:MM:SS\n')
cat('You can replay the clip by clicking on the play button in the Viewer\n')
cat('If you cannot understand the synch call, simply hit enter to skip to the next one\n')
cat('If you enter anything other than the correct format (or one of the specified commands, see below), the current synch call wall automatically repeat\n')
cat('You can go back to the previous clip by typing "back"\n')
cat('If you can tell the collar is not on a meerkat, type "notonmeerkat" (the file will be skipped and marked accordingly)\n')
cat('If you would like to flag the file as problematic, type "flag" (you will then skip it, and it will be specially flagged)\n')
cat('If you would like to skip a file for some other reason, type "skipfile" (you will skip the file, and it will be labeled as skipped)\n')
cat('\n')
cat('If after 20 attempts to label synchs, you have not labeled at least 3, the file will automatically be skipped and labeled as "couldnotsynch"\n')
cat('\n')
cat('After you finish labeling a file, the system will check whether the synchs you have labeled make sense together (are approx the right time difference apart).\n')
cat('If they are not, you will get a warning and you will have to repeat that file.\n')
cat('In this case, try the file again, listening carefully to the synchs. Potentially try labeling other synchs than those you tried before.\n')
cat('#If after several tries you still receive a warning, you should flag the file by typing "flag"\n')
cat('\n')
cat('You may quit at any time by pressing escape. Your progress will be saved\n')

if(!file.exists(outfile)){

  #SETUP (if not already done)
  #get wave files
  wav_files <- list.files(rawdata_dir, pattern = '(?i)\\.wav$', ignore.case = T, recursive = T, full.names=T)

  #get prediction files
  pred_files <- list.files(predictions_dir, recursive = T, full.names=T)

  #remove secondary predictions
  pred_files <- pred_files[which(!grepl('secondary_predictions',pred_files))]

  #make a table of prediction files and associated paths to wave files
  files_table <- data.frame(pred_file = pred_files)
  pred_basenames <- tools::file_path_sans_ext(basename(pred_files))
  wav_basenames <- tools::file_path_sans_ext(basename(wav_files))
  files_table$wav_file <- wav_files[match(pred_basenames, wav_basenames)]
  files_table$status <- 'todo'

  #initialize synchs table
  synchs_all <- data.frame()

  #initialize user time to 0
  user_time <- 0

} else{
  load(outfile)
}

cat('Number of files COMPLETED:',sum(files_table$status=='done', na.rm=T))
cat('\n')
cat('Number of files skipped:',sum(files_table$status%in%c('skipfile','couldnotsynch','notonmeerkat'), na.rm=T))
cat('\n')
cat('Numer of files flagged (and skipped):', sum(files_table$status=='flag', na.rm=T))
cat('\n')
cat('Number of files REMAINING:',sum(files_table$status=='todo', na.rm=T))
cat('\n')
cat('Total time spent on this year so far:', user_time, 'minutes')
cat('\n')

readline('Hit enter to begin! ')

#log user start time
user_start_time <- Sys.time()

#loop over all files and label 3 synchs per file
idxs <- which(files_table$status == 'todo')
i <- 1
while(i <= length(idxs)){
  pred_file <- files_table$pred_file[idxs[i]]
  wav_file <- files_table$wav_file[idxs[i]]

  #read in labels
  labels <- read.csv(pred_file, header = T, sep = '\t')
  synchs <- labels[which(labels$Name == 'synch'),]

  if(nrow(synchs)==0){
    next
  }

  #sort synchs by likelihood
  synchs$likeli <- as.numeric(substring(synchs$Description, 1, 5))
  synchs <- synchs[order(synchs$likeli, decreasing = T),]

  #make a column to indicate whether the synch has been labeled or not
  #1 = labeled, 0 = unlabeled (because couldn't be heard), NA = not tried yet
  synchs$done <- NA

  #make a column to contain the label (string)
  synchs$label <- ''

  #for this file, loop over synchs and label them
  file_complete <- F
  j <- 1

  print(pred_file)
  while((j <= nrow(synchs)) & (j <= max_to_try)){

    if(sum(synchs$done,na.rm=T)>= n_synchs_to_label){
      synchs_curr <- synchs[which(synchs$done == T),]
      synchs_curr$pred_file <- pred_file
      synchs_curr$wav_file <- wav_file
      file_complete <- T
      break
    }

    start_time <- cocomo::parse_audition_time(synchs$Start[j])
    duration <- cocomo::parse_audition_time(synchs$Duration[j])

    play_start <- start_time - pad_start
    if(play_start < 0){
      play_start <- 0
    }

    play_end <- start_time + duration + pad_end

    play_audio_segment(wav_file, from = play_start, to = play_end )
    user_label <- readline(prompt = 'Synch Label (H:MM:SS): ')

    #if the user presses enter (enters nothing), skip to the next label
    if(user_label == ''){
      synchs$done[j] <- F
      j <- j + 1
      next
    }

    #if the user enters something in the right format (H:MM:SS) then add it as a label
    if(grepl("^[0-9]{1,2}:[0-5][0-9]:[0-5][0-9]$", user_label)){
      synchs$label[j] <- user_label
      synchs$done[j] <- T
      j <- j + 1
      next
    }

    #if the user types back, go back 1
    if(user_label == 'back'){
      j <- j - 1

      #make sure not to go back beyond the start
      if(j < 1){
        j <- 1
      }
      next
    }

    #if the user realizes the file is from a time not on a meerkat, they can type notonmeerkat to skip the entire file
    if(user_label == 'notonmeerkat'){
      break
    }

    #if the user wants to skip the file for another reason ,they can do so by typing skipfile
    if(user_label == 'skipfile'){
      break
    }

    #flag the file if something seems wrong with it
    if(user_label == 'flag'){
      break
    }

    #otherwise just repeat the same synch

  }

  #if the file was skipped, note this in the table and move on to next file
  if(!file_complete){
    if(user_label == ''){
      files_table$status[idxs[i]] <- 'couldnotsynch'
    } else{
      files_table$status[idxs[i]] <- user_label
    }
    i <- i + 1
  }

  #if file is complete, check that synchs line up reasonably well
  #if so, save, if not, alert the user and try again
  if(file_complete){
    passed_checks <- F

    #check if the synchs line up

    #get time into file
    synchs_curr$time_into_file <- sapply(synchs_curr$Start, cocomo::parse_audition_time)
    synchs_curr <- synchs_curr[order(synchs_curr$time_into_file),]

    #get labeled times of synchs
    synchs_curr$synch_time_sec <- sapply(synchs_curr$label, cocomo::parse_audition_time)

    #get time elapsed according to labels and according to file time
    synchs_curr$dt_labeled <- synchs_curr$synch_time_sec - synchs_curr$synch_time_sec[1]
    synchs_curr$dt_filetime <- synchs_curr$time_into_file - synchs_curr$time_into_file[1]

    #get drift
    synchs_curr$drift <- abs((synchs_curr$dt_filetime - synchs_curr$dt_labeled) / synchs_curr$dt_filetime) * 60 * 60 #drift per hr
    max_drift <- max(synchs_curr$drift, na.rm=T)

    #if max drift is below the cutoff, the checks are passed
    if(max_drift < max_drift_per_hr){
      passed_checks <- T
    } else{
      passed_checks <- F
    }

    #if checks were passed, append to data frame and save, then move to next file
    if(passed_checks){
      synchs_all <- rbind(synchs_all, synchs_curr)
      files_table$status[idxs[i]] <- 'done'

      time_elapsed <- as.numeric(difftime(Sys.time(),user_start_time, units = 'mins'))
      user_time <- user_time + time_elapsed

      save(list = c('synchs_all', 'files_table','user_time'), file = outfile)
      save(list = c('synchs_all', 'files_table','user_time'), file = outfile_backup)
      i <- i + 1 #move on to next file
    } else{
      print(paste0('Warning: clock drift checks were not passed. Max clock drift per hr = ',max_drift, '--> redoing'))
      files_table$status[idxs[i]] <- 'todo'
    }
  }
}


