#' Plot pull or anchor event
#'
#' Plots a pull or anchor event, with the initiator colored blue and the (potential) follower colored red.
#' Panels show the positions at `t1`,`t2`, and `t3` respectively.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs: `n_inds x n_times` matrix of x positions
#' @param ys: `n_inds x n_times` matrix of y positions
#' @param events: events table output from `get_pulls_and_anchors`
#' @param event_idx: row in the table to plot
#'
#' @returns Returns a plot of the event with the leader in blue and the follower in red
#'
#' @export
plot_pull_or_anchor_event <- function(xs, ys, events, event_idx){

  #get info from the events table
  t1 <- events$t1[event_idx]
  t2 <- events$t2[event_idx]
  t3 <- events$t3[event_idx]
  event_type <- events$type[event_idx]
  leader <- events$leader[event_idx]
  follower <- events$follower[event_idx]
  disparity <- events$disparity[event_idx]
  strength <- events$strength[event_idx]

  #get x and y data for plot
  x_l <- xs[leader, t1:t3]
  y_l <- ys[leader, t1:t3]
  x_f <- xs[follower, t1:t3]
  y_f <- ys[follower, t1:t3]
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
  points(xs[leader,t1], ys[leader,t1], col = 'darkblue', pch = 19)
  points(xs[follower,t1], ys[follower,t1], col = 'darkred', pch = 19)

  #t2
  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('disp =', round(disparity, digits=2)),xlab='x',ylab='y')
  lines(x_l, y_l, col = '#00006622')
  lines(x_f, y_f, col = '#66000022')
  points(xs[leader,t2], ys[leader,t2], col = 'darkblue', pch = 19)
  points(xs[follower,t2], ys[follower,t2], col = 'darkred', pch = 19)

  #t3
  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('strength =',round(strength, digits=2)),xlab='x',ylab='y')
  lines(x_l, y_l, col = '#00006622')
  lines(x_f, y_f, col = '#66000022')
  points(xs[leader,t3], ys[leader,t3], col = 'darkblue', pch = 19)
  points(xs[follower,t3], ys[follower,t3], col = 'darkred', pch = 19)

}

