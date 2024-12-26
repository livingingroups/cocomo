#'Get angle between vectors in radians or degrees
#'
#'Get the angle between two vectors specified by their end points using law of cosines.
#'Vector 1 is defined as the vector pointing from the point `(x1_i, y1_i)` to `(x1_f, y1_f)` and
#'vector 2 is defined as the vector point from the point `(x2_i, y2_i)` to `(x2_f, y2_f)`.
#'The angle is defined as the angle produced if the two vectors are joined at the initial
#'endpoints (rather than head-to-tail). The resulting angle is always positive.
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param x1_i x coordinate of initial point of vector 1
#' @param y1_i y coordinate of initial point of vector 1
#' @param x1_f x coordinate of final point of vector 1
#' @param y1_f y coordinate of final point of vector 1
#' @param x2_i x coordinate of initial point of vector 2
#' @param y2_i y coordinate of initial point of vector 2
#' @param x2_f x coordinate of final point of vector 2
#' @param y2_f y coordinate of final point of vector 2
#' @param degrees if `T`, angle will be returned in degrees (otherwise it will be returned in radians)
#'
#' @returns Returns the angle between the two specified vectors
#' @export
get_angle_between_vectors <- function(x1_i, y1_i, x1_f, y1_f, x2_i, y2_i, x2_f, y2_f, degrees = F){

  dx1 <- x1_f - x1_i
  dx2 <- x2_f - x2_i
  dy1 <- y1_f - y1_i
  dy2 <- y2_f - y2_i

  #get dot product
  dot <- dx1*dx2 + dy1*dy2

  #magnitude of vectors (length)
  mag1 <- sqrt(dx1^2 + dy1^2)
  mag2 <- sqrt(dx2^2 + dy2^2)

  #cos of angle
  cosang <- dot / (mag1* mag2)

  #get the angle
  angle <- acos(cosang)

  #convert to degrees if needed
  if(degrees){
    angle <- angle*180/pi
  }

  return(angle)

}
