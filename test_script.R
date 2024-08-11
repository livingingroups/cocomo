#Test script for various functions - informal for now

library(cocomo)

load('~/Dropbox/meerkats/processed_data/HM2019_COORDINATES_all_sessions_with_scans.RData')

xs <- HM2019_allX
ys <- HM2019_allY
timestamps <- HM2019_timeLine
ids <- HM2019_indInfo

#Relative positions
heading_type <- 'temporal'
t_window <- 600
min_inds_tracked <- 5
forward <- T
rel_pos <- cocomo::get_positions_relative_to_group(xs = xs, ys = ys, heading_type = heading_type, t_window = t_window, min_inds_tracked = min_inds_tracked, forward = forward)
xy_centr <- cocomo::get_group_centroid(xs = xs, ys = ys, min_inds_tracked = min_inds_tracked)
x_centr <- xy_centr$x_centr
y_centr <- xy_centr$y_centr
head_centr <- cocomo::get_group_heading(xs = xs, ys = ys, heading_type = heading_type, t_window = t_window, forward = forward, min_inds_tracked = min_inds_tracked)
t0 <- 5000
tf <- t0 + t_window * 5

#make plot
quartz()

#initialize plot
plot(xs[,t0:tf], ys[,t0:tf], asp = 1, cex = 0.001, pch = 19, xlab = 'Easting', ylab = 'Northing')

#plot ind trajectories
for(i in 1:nrow(xs)){
  lines(xs[i,t0:tf],ys[i,t0:tf],col='gray',lwd =0.5)
}

#plot group centroid trajectory
lines(x_centr[t0:tf], y_centr[t0:tf], lwd = 1, col = 'red')

#plot group headings at 2 different points
x_vec <- 5
y_vec <- 5
x_rot <- cos(head_centr)*x_vec - sin(head_centr)*y_vec
y_rot <- sin(head_centr)*x_vec + sin(head_centr)*y_vec
x_group_axis <- x_centr + x_rot
y_group_axis <- y_centr + y_rot

arrows(x_centr[t0], y_centr[t0], x_group_axis[t0], y_group_axis[t0])
arrows(x_centr[t0+t_window], y_centr[t0+t_window], x_group_axis[t0+t_window], y_group_axis[t0+t_window])

points(x_centr[t0],y_centr[t0], col = 'red',cex = 2, lwd=3)
points(xs[,t0],ys[,t0],col='black',cex = .5, pch = 19)
points(x_centr[t0+t_window],y_centr[t0+t_window], col = 'red',cex = 2, lwd=3)
points(xs[,t0+t_window],ys[,t0+t_window],col='blue',cex = .5, pch = 19)
points(x_centr[t0+t_window*2],y_centr[t0+t_window*2], col = 'red',cex = 2, lwd=3)

