#' Plot a statistic of y conditioned on the value of x
#'
#' For paired measurements `(x,y)`, plot a specified statistic `stat` (e.g. the mean) of
#' `y` for values of `x` within bins specified by the vector `bins`
#'
#' For example, if `stat = 'mean'`, the function will plot `vals[i]` vs. `bins[i]` where
#' `vals[i]` is defined as `mean(y[which(x >= bins[i] & x < bins[i+1])], na.rm=T)`
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param x vector of numbers whose value will be conditioned on
#' @param y vector of numbers from which a statistic will be computed for values associated with each `x` bin
#' @param bins bins to use (will override `bin_by_quantile` and `n_bins` if specified)
#' @param stat which statistic of the distribution of `y` to use (can be `'mean' or 'median'`)
#' @param bin_by_quantile specifies whether to bin based on quantiles of the overall distribution of `x` (`T` or `F`)
#' @param n_bins number of bins to create if `bin_by_quantile = T` (bins will be evenly spaced according to quantiles)
#' @param error_bar_range if not `NULL`, will produce error bars spanning a given quantile range of the distribution of `y` for each bin. should be between `0` and `0.5` (defaults to `0.25`, or interquantile range)
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param main main plot label
#' @param xlim if specified, sets limits of x axis
#' @param ylim if specified, sets limits of y axis
#'
#' @returns Creates a plot, and also outputs a list with `bins` (bins used), `mids` (the midpoints
#' of the `x` bins), `y_stats` (the mean or median of `y` for each bin), `y_uppers`
#' and `y_lowers` (the upper and lower qauntiles of `y` for each bin as specified
#' by `error_bar_range`) and `ns` (the number of data points in each bin)
#'
#' @export

plot_y_conditioned_on_x <- function(x, y,
                                    bins = NULL,
                                    stat = 'median',
                                    bin_by_quantile = T,
                                    n_bins = 10,
                                    error_bar_range = .25,
                                    xlab = '',
                                    ylab = '',
                                    main = '',
                                    xlim = NULL,
                                    ylim = NULL
                                    ){


  #check if x and y have the same dimensions
  if(length(x) != length(y)){
    stop('x and y must have the same length')
  }

  #check that a supported statistic has been specified
  if(!(stat %in% c('mean','median'))){
    stop('must specify stat as either mean or median')
  }

  #if bins were not manually specified, create them from the quantiles of x
  if(is.null(bins)){
    bins <- quantile(x[which(!is.infinite(x))], seq(0,1,length.out = n_bins), na.rm=T)
  }

  #create a vector mids for plotting
  mids <- (bins[1:(length(bins)-1)] + bins[2:length(bins)]) / 2

  #create variables to store the values we will compute
  y_stats <- uppers <- lowers <- rep(NA, length(mids))
  ns <- rep(0, length(mids))

  #loop over bins and compute the test statistic of y for each bin range based on x
  for(i in 1:length(mids)){

    #get indexes where x falls within the bin
    idxs <- which(x >= bins[i] & x < bins[i+1])

    #compute the values of y_stat and uppers and lowers for each bin
    if(length(idxs) > 0){

      if(stat == 'mean'){
        y_stats[i] <- mean(y[idxs], na.rm=T)
      }
      if(stat == 'median'){
        y_stats[i] <- median(y[idxs], na.rm=T)
      }

      #get number of data points in each bin
      ns[i] <- sum(!is.na(y[idxs]))

      #get upper and lower error bars
      if(!is.null(error_bar_range)){
        uppers[i] <- quantile(y[idxs], 1 - error_bar_range, na.rm=T)
        lowers[i] <- quantile(y[idxs], error_bar_range, na.rm=T)
      }
    }
  }

  #get xlim and ylim if they aren't specified
  if(is.null(xlim)){
    xlim <- range(mids, na.rm=T)
  }
  if(is.null(ylim)){
    ylim <- range(c(uppers, lowers), na.rm=T)
  }

  #make the plot
  print(xlim)
  plot(mids, y_stats, xlim = xlim, ylim = ylim, main = main, xlab = xlab, ylab = ylab, pch = 19, cex = 2, cex.axis = 1.5, cex.lab = 2)

  if(!is.null(error_bar_range)){
    arrows(mids, lowers, mids, uppers, length = 0.1, angle = 90, lwd = 2, code = 3)
  }

  out <- list()
  out$bins <- bins
  out$mids <- mids
  out$stat <- stat
  out$error_bar_range <- error_bar_range
  out$y_stats <- y_stats
  out$ns <- ns
  out$y_uppers <- uppers
  out$y_lowers <- lowers

  return(out)


}




