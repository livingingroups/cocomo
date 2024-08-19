#' Plot network matrix
#'
#' Plot a matrix with color representing the edge weights of a network
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param mat the matrix to plot (matrix of dimensions `n_inds x n_inds`)
#' @param mat_col_palette color palette to use for the matrix plot
#' @param ind_names the names for the x and y labels (vector of length `n_inds`)
#' @param ind_cols colors to use for the labels (vector of length `n_inds`)
#' @param zlim a vector of length 2 giving the numerical limits of z (the color scale)
#' @param xlab x axis label (rows of matrix)
#' @param ylab y axis label (columns of matrix)
#'
#'
#' @importFrom fields image.plot
#' @importFrom dichromat colorRampPalette
#'
#' @export
plot_network_matrix <- function(mat, mat_col_palette = NULL, ind_names = NULL, ind_cols = NULL, zlim = NULL, xlab = '', ylab = '', main = ''){

  #set color palette if needed
  if(is.null(mat_col_palette)){
    mat_col_palette <- colorRampPalette(c('black','white','red'))
  }

  #set zlims if needed
  if(is.null(zlim)){
    zlim <- c(min(mat, na.rm=T), max(mat, na.rm=T))
  }

  #make plot

  fields::image.plot(mat, col = mat_col_palette(256), zlim = zlim, xaxt = 'n', yaxt = 'n', xlab = xlab, ylab = ylab ,cex.axis = 0.5, main = main)

  #add individual id labels to axes
  axis(1, at = seq(0,1,length.out=length(ind_names)), labels = ind_names, las=2, cex.axis = 0.7)
  axis(2, at = seq(0,1,length.out=length(ind_names)), labels = ind_names, las = 2, cex.axis = 0.7)

  #add color points to axes
  par(xpd=T)
  points(seq(0,1,length.out=length(ind_names)), rep(-1/25,length(ind_names)), pch = 21, bg = ind_cols)
  points(rep(-1/25,length(ind_names)), seq(0,1,length.out=length(ind_names)), pch = 21, bg = ind_cols)

}
