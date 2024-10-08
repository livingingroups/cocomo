#' Import Axy-Trek GPS file
#'
#' Imports GPS data from an Axy Trek file, which may have lines that are not
#' formatted such that read.delim can parse them. Can currently handle files
#' from 2021-2023 at least. Output data frame has column names compatible with
#' the function `cocomo::reformat_movebank_to_matrix`.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param input_file_path full path to the input file
#'
#' @returns Returns a data frame with the following columns: `timestamp`, `location.lat`,
#' `location.long`, `V4`, `V5`, `satellite.count`, `V7`
#'
#' @importFrom stringr str_count
#' @importFrom lubridate parse_date_time
#'
#' @export
import_axytrek_gps_file <- function(input_file_path){

  file_lines <- readLines(input_file_path)
  n_tabs <- stringr::str_count(file_lines, pattern = '\t')
  lines_to_use <- which(n_tabs %in% c(7,8,13))
  file_lines_use <- file_lines[lines_to_use]
  file_lines_split <- sapply(file_lines_use, FUN = function(x){return(strsplit(x, '\t'))})
  df <- do.call(rbind.data.frame, file_lines_split)

  #if now rows found, return NULL
  if(nrow(df)==0){
    return(NULL)
  }

  #combine first 2 columns into full timestamp if needed
  if(ncol(df)>=9){
    df[,1] <- paste(df[,1],df[,2],sep=',')
    df <- df[,-2]
  }

  #rename columns
  if(ncol(df) == 8){
    colnames(df) <- c('timestamp','location.lat','location.long','V4','V5','satellite.count','V7','V8')
  }
  if(ncol(df) == 13){
    colnames(df) <- c('timestamp','location.long','location.lat','V4','V5','V6','V7','satellite.count','V9','V10','V11','V12','V13')
  }

  #parse timestamp
  df$timestamp <- lubridate::parse_date_time(df$timestamp, tz = 'UTC', orders = c('%d/%m/%Y,%H:%M:%S',
                                                                                  '%Y-%m-%d,%H:%M:%S'))

  #return only relevant columns
  df <- df[,c('timestamp','location.lat','location.long','satellite.count')]

  #return data frame
  return(df)

}
