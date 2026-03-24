#' Plot pull or anchor event
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Plots a pull or anchor event, with the initiator colored blue and the responder colored red.
#' Panels show the positions at `t1`,`t2`, and `t3` respectively.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs `n_inds x n_times` matrix of x positions
#' @param ys `n_inds x n_times` matrix of y positions
#' @param events events table output from `get_pulls_and_anchors`
#' @param event_idx row in the table to plot
#' @param ids ids table output from `reformat_movebank_to_matrix`
#'
#' @returns Returns a plot of the event with the initiator in blue and the responder in red
#'
#' @export
plot_pull_or_anchor_event <- function(xs, ys, events, event_idx, ids=NULL){

  #get info from the events table
  t1 <- events$t1[event_idx]
  t2 <- events$t2[event_idx]
  t3 <- events$t3[event_idx]
  event_type <- events$type[event_idx]
  initiator <- events$initiator[event_idx]
  responder <- events$responder[event_idx]
  if(is.character(initiator) | is.character(responder)){
    if(is.null(ids)){
      stop("Define 'ids' so the initiator and responder character strings can be matched with an index value.")
    }
    else{
      initiator <- which(ids==initiator)
      responder <- which(ids==responder)
    }
  }
  disparity <- events$disparity[event_idx]
  strength <- events$strength[event_idx]

  #get x and y data for plot
  x_l <- xs[initiator, t1:t3]
  y_l <- ys[initiator, t1:t3]
  x_f <- xs[responder, t1:t3]
  y_f <- ys[responder, t1:t3]
  cols_l <- topo.colors(length(x_l))
  cols_f <- heat.colors(length(x_f))

  #get x and y boundaries
  xmin <- min(c(x_l, x_f), na.rm=T)
  xmax <- max(c(x_l, x_f), na.rm=T)
  ymin <- min(c(y_l, y_f), na.rm=T)
  ymax <- max(c(y_l, y_f), na.rm=T)

  par(mfrow=c(1,3))
  #t1
  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = event_type,xlab='x',ylab='y')
  lines(x_l, y_l, col = '#00006622')
  lines(x_f, y_f, col = '#66000022')
  points(xs[initiator,t1], ys[initiator,t1], col = 'darkblue', pch = 19)
  points(xs[responder,t1], ys[responder,t1], col = 'darkred', pch = 19)

  #t2
  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('disp =', round(disparity, digits=2)),xlab='x',ylab='y')
  lines(x_l, y_l, col = '#00006622')
  lines(x_f, y_f, col = '#66000022')
  points(xs[initiator,t2], ys[initiator,t2], col = 'darkblue', pch = 19)
  points(xs[responder,t2], ys[responder,t2], col = 'darkred', pch = 19)

  #t3
  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('strength =',round(strength, digits=2)),xlab='x',ylab='y')
  lines(x_l, y_l, col = '#00006622')
  lines(x_f, y_f, col = '#66000022')
  points(xs[initiator,t3], ys[initiator,t3], col = 'darkblue', pch = 19)
  points(xs[responder,t3], ys[responder,t3], col = 'darkred', pch = 19)

}

