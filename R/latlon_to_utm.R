#'Latitude / longitude to UTM conversion
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'Converts a matrix of lons and lats (lons first column, lats second column) to UTM
#'eastings and northings (eastings first column, northings second column)
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param lons_lats N x 2 matrix of longitudes (col 1) and latitudes (col2)
#' @param utm_zone numeric or string value of UTM zone
#' @param hemisphere northern or southern hemisphere - specify 'N' or 'S' (not case sensitive)

#'@return easts_norths, an N x 2 matrix of eastings (col 1) and northings (col 2)

#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom magrittr %>%
#' @export

latlon_to_utm <- function(lons_lats, utm_zone, hemisphere){

  #error checking - hemisphere
  if(!(hemisphere %in% c('S','s','south','South','N','n','north','North'))){
    stop('Need to specify hemisphere (s or n)')
  }

  #error checking - lons_lats matrix
  input_dims <- dim(lons_lats)
  if(length(input_dims) != 2){
    stop('lons_lats does not have dimension 2')
  }
  if(input_dims[2] != 2){
    stop('lons_lats does not have 2 columns')
  }

  #error checking - utm zone
  if(is.na(as.numeric(utm_zone))){
    stop('utm_zone must be numeric or a character string containing only a number')
  }

  #main
  lonlat <- data.frame(lon = as.numeric(lons_lats[,1]), lat = as.numeric(lons_lats[,2]))
  non_na_idxs <- which(!is.na(lonlat$lon) & !is.na(lonlat$lat))
  lonlat_sf <- lonlat[non_na_idxs,] %>% sf::st_as_sf(coords = c('lon','lat'), crs = 4326)

  #create CRS string
  if(hemisphere %in% c('S','s','south','South')){
    crs_string <- paste0("+proj=utm +datum=WGS84 +units=m +no_defs +south +zone=", utm_zone)
  }
  if(hemisphere %in% c('N','n','north','North')){
    crs_string <- paste0("+proj=utm +datum=WGS84 +units=m +no_defs +north +zone=", utm_zone)
  }

  xy_sf <- lonlat_sf %>% sf::st_transform(crs = crs_string)

  easts_norths <- matrix(NA, nrow = nrow(lons_lats), ncol = 2)
  easts_norths[non_na_idxs,] <- sf::st_coordinates(xy_sf)

  return(easts_norths)

}
