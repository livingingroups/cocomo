#' Parse Adobe Audition formatted time
#'
#' Parses a time from the `Start` or `Duration` column of an Audition label file to seconds
#'
#' The `Start` and `Duration` columns of Adobe Audition label files have a non-standard
#' format. Within the first 10 minutes, they are formatted as M:SS.SSS. After 10 minutes
#' they are formatted as MM:SS.SSS and after the first hour they are formatted as
#' H:MM:SS.SSS (and presumably after the first 10 hours as HH:MM:SS.SSS). This function
#' parses these weird labels and returns a numeric value of the time into the file, in seconds
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @returns Returns numeric value of time into the file (in seconds)
#'
#' @param audition_time_str character string correspoding to an Audition-formatted time
#'
#' @export
parse_audition_time <- function(audition_time_str){

  #if string is not in the right format, return NA
  if(!grepl('[0-9]{1,2}:[0-9]{2}', audition_time_str)){
    return(NA)
  }
  if(grepl('[a-z]', audition_time_str)){
    return(NA)
  }
  if(grepl('[A-Z]', audition_time_str)){
    return(NA)
  }

  #split up time by colons
  split_time <- strsplit(audition_time_str, ':')[[1]]

  #convert to seconds
  #if only one colon, assume it's minutes and seconds
  if(length(split_time) == 2){
    t_sec <- as.numeric(split_time[1])*60 + as.numeric(split_time[2])
  }

  #if two colons, assume it's hours, minutes, an seconds
  if(length(split_time) == 3){
    t_sec <- as.numeric(split_time[1])*60*60 + as.numeric(split_time[2])*60 + as.numeric(split_time[3])
  }

  return(t_sec)

}

