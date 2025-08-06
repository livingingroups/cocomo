#' Check meerkat data availability sheets for any inconsistencies or errors
#'
#' Reads in a data availability sheet and checks for the following:
#' 1. Flag any missing fields
#' 2. Check internal consistency of each row
#'  - datatype, tagtype, and datasource are all consistent and into expected categories
#'  - normalday column is either 0 or 1
#'  - start and end are correctly formatted times
#'  - filename contains individual id code
#'  - filename has expected file extension
#'  - in audio files, filename contains date matching start date
#'  - normalday and comments columns make sense together
#' 3. Check that file in filename column exists on server
#' 4. Finds files on the server that involve an individual from the group on a
#'    date included in the deployment, but aren't in the data availability sheet
#'
#' Outputs everything into a csv for manual checking, saves as path_to_data_availability_sheet_check.csv
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param path_to_data_availability_sheet path to data availability sheet on the server (.csv)
#' @param path_to_data_dir path to the outer directory of the relevant deployment
#'
#' @returns 0 if successful
#'
#'
meerkat_check_data_availability_sheet <- function(path_to_data_availability_sheet, path_to_data_dir){

  #-----Setup-----
  #Required fields (must be filled in data availability sheet)
  required_fields <- c('individual','start','end','datatype','datasource','tagtype','tag','normalday','filename','source','experiment')

  #Reasons for a day to not be normal
  abnormalday_reasons <- c('babysit','bs','rov','igi','rain','burrow','split','capture')

  #Valid combinations of datatype, datasource, tagtype, file extension
  valid_combos <- matrix(nrow = 0, ncol = 4)
  colnames(valid_combos) <- c('datatype','datasource','tagtype','extension')
  valid_combos <- rbind(valid_combos, c('gps','focal','gipsy','txt'))
  valid_combos <- rbind(valid_combos, c('gps','focal','axy','txt'))
  valid_combos <- rbind(valid_combos, c('gps','focal','garmin','txt'))
  valid_combos <- rbind(valid_combos, c('gps','collar','gipsy','txt'))
  valid_combos <- rbind(valid_combos, c('gps','collar','axy','txt'))
  valid_combos <- rbind(valid_combos, c('audio','focal','mic',''))
  valid_combos <- rbind(valid_combos, c('audio','collar','edic','wav'))
  valid_combos <- rbind(valid_combos, c('audio','collar','soroka','wav'))
  valid_combos <- rbind(valid_combos, c('acc','collar','axy','csv'))
  valid_combos <- as.data.frame(valid_combos)
  valid_combos_str <- paste(valid_combos$datatype, valid_combos$datasource, valid_combos$tagtype, sep ='_')
  valid_combos_str_ext <- paste(valid_combos$datatype, valid_combos$datasource, valid_combos$tagtype, valid_combos$extension, sep ='_')

  #-----Read in and preprocess-----
  #Read in data availability sheet
  avail <- read.csv(path_to_data_availability_sheet, stringsAsFactors = F, header = T, na.strings = c('','NA',' ','  '))

  #Add a column with row number
  avail$rownum <- 1:nrow(avail)

  #Replace backslashes with forward slashes in filenames
  avail$filename <-gsub('[\\]','/',avail$filename)

  #Set any fields that are only empty space to NA

  #Create a data frame to hold any output
  #extra column 'reason' specifies why the data was flagged
  tocheck <- data.frame(matrix(ncol = ncol(avail)+1, nrow = 0))
  colnames(tocheck) <- c(colnames(avail),'reason')

  #---Flag any rows with missing fields----
  rows <- which(!complete.cases(avail[,required_fields]))
  if(length(rows)>0){
    missing <- avail[rows,]
    missing$reason <- 'missing_field'
    tocheck <- rbind(tocheck, missing)
  }

  #----Check internal consistency of each row----
  #datatype, datasource, tagtype - flag invalid combinations
  combos <- paste(avail$datatype, avail$datasource, avail$tagtype, sep='_')
  rows <- which(!(combos %in% valid_combos_str))
  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'invalid datatype/dataousrce/tagtype combo'
    tocheck <- rbind(tocheck, invalid)
  }

  #normalday is 0 or 1
  rows <- which(! avail$normalday %in% c(0,1))
  if(length(rows) >0){
    invalid <- avail[rows,]
    invalid$reason <- 'normalday col not 0/1'
    tocheck <- rbind(tocheck, invalid)
  }

  #start and end are correctly formatted times
  starts <- as.POSIXct(avail$start, tz = 'UTC', format = '%Y-%m-%d %H:%M:%S')
  ends <- as.POSIXct(avail$end, tz = 'UTC', format = '%Y-%m-%d %H:%M:%S')
  rows <- which(is.na(starts) | is.na(ends))
  if(length(rows) >0){
    invalid <- avail[rows,]
    invalid$reason <- 'invalid time format'
    tocheck <- rbind(tocheck, invalid)
  }

  #start is earlier than end
  rows <- which(starts > ends)
  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'start later than end'
    tocheck <- rbind(tocheck, invalid)
  }

  #filename (basename and parent directory) contains individual id code
  id_codes <- avail$individual
  filenames <- avail$filename
  basenames <- basename(filenames)
  parent_dirs <- basename(gsub("/[^_]+$", "", filenames))
  rows <- c()
  for(i in 1:nrow(avail)){
    if(!grepl(id_codes[i], basenames[i], fixed = T)){
      rows <- c(rows, i)
      next
    }
    if(!grepl(id_codes[i], parent_dirs[i], fixed = T)){
      rows <- c(rows, i)
      next
    }
  }

  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'id code mismatch (individual vs filename)'
    tocheck <- rbind(tocheck, invalid)
  }

  #filename has expected file extension
  exts <- tolower(tools::file_ext(basenames))
  combos <- paste(avail$datatype, avail$datasource, avail$tagtype, exts, sep = '_')
  rows <- which(!(combos %in% valid_combos_str_ext))
  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'wrong file extension'
    tocheck <- rbind(tocheck, invalid)
  }

  #date matching start date (only for audio files)
  start_dates <- as.Date(starts)
  rows <- c()
  for(i in 1:nrow(avail)){
    curr_date <- start_dates[i]
    curr_date_str2 <- gsub('-','',curr_date)
    if(avail$datatype[i] == 'audio'){
      if(!grepl(curr_date, basenames[i]) & !grepl(curr_date_str2, basenames[i])){
        rows <- c(rows, i)
      }
    }
  }

  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'date not found in audio filename'
    tocheck <- rbind(tocheck, invalid)
  }

  #normalday and comments align
  rows <- c()
  for(i in 1:nrow(avail)){
    comment <- avail$comments[i]
    if(!is.na(comment)){
      found <- F
      for(j in 1:length(abnormalday_reasons)){
        if(grepl(abnormalday_reasons[j], comment, ignore.case = T)){
          found <- T
        }
      }
      if(found){
        if(avail$normalday[i] != 0){
          rows <- c(rows, i)
        }
      }
    }
  }

  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'comment / normalday mismatch'
    tocheck <- rbind(tocheck, invalid)
  }

  #get all files on the server
  files_on_server <- list.files(path = path_to_data_dir, recursive = T, full.names = T)
  files_on_server <- gsub(".*EAS_shared","EAS_shared",files_on_server)
  files_on_server <-gsub('[\\]','/',files_on_server)
  files_on_server <-gsub('//','/', files_on_server)

  #check that each filename exists on the server
  rows <- c()
  for(i in 1:nrow(avail)){
    filename <- avail$filename[i]
    if(is.na(filename)){
      next
    }
    n_found <- sum(grepl(filename, files_on_server, fixed = T))
    if(n_found == 0){
      rows <- c(rows, i)
    }
  }

  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'filename not found on server'
    tocheck <- rbind(tocheck, invalid)
  }

  #check for SOUNDFOC
  rows <- c()
  for(i in 1:nrow(avail)){
    if(avail$datatype[i]=='audio' & avail$datasource[i]=='focal'){
      if(!grepl('SOUNDFOC',filenames[i],fixed=T)){
        rows <- c(rows, i)
      }
    }
  }

  if(length(rows)>0){
    invalid <- avail[rows,]
    invalid$reason <- 'audio focal does not include SOUNDFOC string'
    tocheck <- rbind(tocheck, invalid)
  }

  #check for files that may have been missed in the data availability sheet

  #unique ids and dates
  ids <- unique(avail$individual)
  dates <- unique(date(starts))
  dates_all <- date(avail$start)

  #get all combos of ids and dates and data types - check for any missed files
  expected_datatypes <- unique(avail$datatype)
  expected_combos <- expand.grid(id = ids, date = dates, datatype = expected_datatypes, stringsAsFactors = F)
  expected_combos$date[which(expected_combos$datatype %in% c('gps','acc'))] <- NA
  expected_combos <- expected_combos[!duplicated(expected_combos),]


  #check for combos of id, tag type, and (in case of audio) date that are missing from the data availability sheet
  missing_combos <- data.frame()
  for(i in 1:nrow(expected_combos)){
    if(!is.na(expected_combos$date[i])){
      found <- sum(avail$individual == expected_combos$id[i] & avail$datatype == expected_combos$datatype[i] & dates_all == expected_combos$date[i], na.rm=T)
    } else{
      found <- sum(avail$individual == expected_combos$id[i] & avail$datatype == expected_combos$datatype[i], na.rm=T)
    }
    if(found == 0){
      missing_combos <- rbind(missing_combos, expected_combos[i,])
    }
  }

  #see if there are any files on the server that fit the description of these missing combos
  file_exts <- tolower(tools::file_ext(files_on_server))
  potential_missed_files <- data.frame()
  if(nrow(missing_combos)>0){
    for(i in 1:nrow(missing_combos)){
      datatype <- missing_combos$datatype[i]
      id <- missing_combos$id[i]
      date <- missing_combos$date[i]
      date2 <- gsub('-','',date)

      #audio files have wav extension and match exact id and date
      if(datatype == 'audio'){
        date_match <- grepl(date, basename(files_on_server), fixed = T) | grepl(date2, basename(files_on_server), fixed =T)
        potential_files <-files_on_server[which(file_exts == 'wav' & grepl(id, basename(files_on_server), fixed=T) & date_match)]
      }

      #gps files have txt extension, gps in the file path, and match id (but not necessarily date)
      if(datatype == 'gps'){
        potential_files <-files_on_server[which(file_exts == 'txt' & grepl(id, basename(files_on_server), fixed=T) & grepl('gps',files_on_server, ignore.case =T))]
      }

      #acc files have csv extension, gps in the file path, and match id (but not necessarily date)
      if(datatype == 'acc'){
        potential_files <-files_on_server[which(file_exts == 'csv' & grepl(id, basename(files_on_server), fixed=T) & grepl('gps',files_on_server, ignore.case =T))]
      }

      #add to data frame of potential missed files
      if(length(potential_files)>0){
        curr <- data.frame(file = potential_files, id = rep(id, length(potential_files)),
                           date = rep(date, length(potential_files)),
                           datatype = rep(datatype, length(potential_files)))
        potential_missed_files <- rbind(potential_missed_files, curr)
      }

    }
  }

  #save outputs
  base <- tools::file_path_sans_ext(path_to_data_availability_sheet)
  savename_check <- paste0(base,'_check.csv')
  savename_missing <- paste0(base,'_missing.csv')
  write.csv(x = tocheck, file = savename_check)
  write.csv(x = potential_missed_files, file = savename_missing)


}
