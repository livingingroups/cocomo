#'Create a KML that shows animal trajectories (viewable on Google Earth Pro)
#'
#' This function creates a KML file while can be loaded into Google Earth Pro to view trajectories of a group over time.
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param lons `N x n_times` matrix giving longitude coordinates of each individual over time
#' @param lats `N x n_times` matrix giving latitutde coordinates of each individual over time
#' @param timestamps vector of timestamps (assumed to be in UTC) of length `n_times`
#' @param id_codes vector of character string specifying id codes of each animal to be plotted
#' @param t0 time index at which to start
#' @param tf time index at which to end
#' @param output_file_path full path to the output file as a character string (must end in .kml)
#' @param step time resolution (in time steps)
#' @param cols vector of length `N` giving colors for each individual, e.g. 'ffed8031' (first two elements give transparency, last 6 are color specified in hex). If NULL, trajectories will be white.
#' @param icons vector of length `N` specifying icons (further information below)
#' @param fixed_locs data frame with lon and lat coordinates of fixed locations to label (e.g. dens) - must have columns 'names','lon','lat'
#' @param calls data frame with columns `ind_idx`,`time_idx`,`call_type`, and `time`
#' @param calls_icons vector of length equal to the number of call types specifying which icons will be used for displaying calls
#'
#' @section Additional details on icon and line color specification:
#' You can specify icons by giving a vector of filenames (character strings) pointing to images on your computer (e.g. png works).
#' Icons should be contained in the same folder where the KMLs will be output, so that Google Earth Pro will be able to read them in when you load the KMLs.
#' If this argument is set to NULL, the code will instead use built-in blue markers from Google Earth. However, these are unfortunately ugly.
#'
#' You can specify the colors of lines using a hex format where the first two digits give the transparency (ff = fully opaque) and the last 6 digits give the color in RGB.
#' Unlike in R, a `'#'` should not be used before the color, and the transparency goes first rather than last.
#' For example `'ffff0000'` specifies opaque red.
#'
#' @returns Creates and saves a kml to the specified output_file_path which can be loaded into Google Earth to view animated trajectories
#' @export
#'
create_trajectories_kml <- function(lons, lats, timestamps, id_codes, t0, tf, output_file_path, step = 1, cols = NULL, icons = NULL, fixed_locs = NULL, fixed_locs_icons = NULL, calls = NULL, calls_icons = NULL){

  #get number of individuals
  n_inds <- nrow(lons)

  #if no colors specified, use white
  if(is.null(cols)){
    cols <- rep('ffffffff', n_inds)
  }

  #if no icons specified, use ugly google earth built-in ones
  if(is.null(icons)){
    icons <- rep('http://maps.google.com/mapfiles/kml/paddle/blu-blank-lv.png',n_inds)
  }

  #if calls are specified, get number of call types
  #if no calls icons specified, use diamonds for all call types
  if(!is.null(calls)){
    call_types <- unique(calls$call_type)
    if(is.null(calls_icons)){
      calls_icons <- rep('https://maps.google.com/mapfiles/kml/shapes/open-diamond.png', length(call_types))
    }
  }
  #if calls are specified, create column for icons
  if(!is.null(calls)){
    calls$icon <- ''
    calls$icon <- call_types[match(calls$call_type, call_types)]
  }

  #get time indexes
  tidxs <- seq(t0,tf,step)

  # START WRITING
  sink(output_file_path)

  # start output
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  cat("<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\">\n")
  cat("<Document>\n")

  #plot fixed locations
  if(!is.null(fixed_locs)){
    if(is.null(fixed_locs_icons)){
      fixed_locs_icons <- rep('https://maps.google.com/mapfiles/kml/shapes/target.png', nrow(fixed_locs))
    }
    for(i in 1:nrow(fixed_locs)){
       cat("<Placemark>\n")
       cat(paste("<name>",fixed_locs$name[i],"</name>\n", sep = ' '))
       cat("<Point>\n")
       cat(paste("<coordinates>",fixed_locs$lon[i],fixed_locs$lat[i],'</coordinates>',sep=' '))
       cat("</Point>\n")
       cat(paste("<Icon><href>",fixed_locs_icons[i],"</href></Icon>"))
       cat("</Placemark>\n")
    }
  }

  #tracks of ids
  for(i in 1:n_inds){
    cat(paste("<Style id=\"track-",id_codes[i],"\"><IconStyle><scale>0.5</scale><Icon><href>",icons[i],"</href></Icon></IconStyle><LineStyle><color>",as.character(cols[i]),"</color><colorMode>normal</colorMode></LineStyle></Style>\n",sep=""))
  }

  #tracks of calls
  if(!is.null(calls)){
    for(i in 1:nrow(calls)){
      cat(paste("<Style id=\"track-call",i,"\"><IconStyle><scale>0.5</scale><Icon><href>",calls$icon[i],"</href></Icon></IconStyle><LineStyle><color>",'ffffffff',"</color><colorMode>normal</colorMode></LineStyle></Style>\n",sep=""))
    }
  }

  #time strings
  timestamps <- as.character(format(timestamps, '%y-%m-%d %H:%M:%S'))
  timestamps <- gsub(' ','T',timestamps)
  timestamps <- paste0(timestamps,'.000Z')

  #locations of individuals
  for(i in 1:n_inds){
    cat("<Folder>\n")
    cat(paste("<name>",id_codes[i],"</name>\n",sep=""))
    cat("<Placemark>")
    cat(paste("<styleUrl>#track-",id_codes[i],"</styleUrl>\n",sep=""))
    cat("<visibility>0</visibility>\n")
    cat(paste("<name>",id_codes[i],"</name>\n",sep=''))
    cat("<gx:Track>\n")
    cat("<gx:altitudeMode>relativeToGround</gx:altitudeMode>\n")

    # FOR EACH TIME - location of individuals
    for(tt in tidxs){
      if(!is.na(lons[i,tt])){
        cat(sprintf("<when>%s</when>\n",timestamps[tt]),sep="")
      }
    }
    # FOR EACH TIME
    for(tt in tidxs){
      if(!is.na(lons[i,tt])){
        cat(sprintf("<gx:coord>%s</gx:coord>\n",paste(lons[i,tt],lats[i,tt])),sep="")
      }
    }

    cat("</gx:Track>\n")
    cat("</Placemark>\n")
    cat("</Folder>\n")
  }

  #locations and times of calls
  if(!is.null(calls)){
    for(i in 1:nrow(calls)){
      cat("<Folder>\n")
      cat(paste("<name>",i,"</name>\n",sep=""))
      cat("<Placemark>")
      cat(paste("<styleUrl>#track-call",i,"</styleUrl>\n",sep=""))
      cat("<visibility>0</visibility>\n")
      cat(paste("<name>",i,"</name>\n",sep=''))
      cat("<gx:Track>\n")
      cat("<gx:altitudeMode>relativeToGround</gx:altitudeMode>\n")

      # when and where
      cat(sprintf("<when>%s</when>\n",timestamps[calls$time_idx[i]]),sep="")
      cat(sprintf("<gx:coord>%s</gx:coord>\n",paste(lons[calls$ind_idx[i],calls$time_idx[i]],lats[calls$ind_idx[i],calls$time_idx[i]])),sep="")

      cat("</gx:Track>\n")
      cat("</Placemark>\n")
      cat("</Folder>\n")
    }
  }

  cat("</Document>\n")
  cat("</kml>\n")
  sink()



}
