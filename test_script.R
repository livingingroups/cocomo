#Test script for various functions - informal for now

library(cocomo)
library(plotrix)
library(lubridate)

load('~/Dropbox/meerkats/processed_data/HM2019_COORDINATES_all_sessions_with_scans.RData')

xs <- HM2019_allX
ys <- HM2019_allY
timestamps <- HM2019_timeLine
ids <- HM2019_indInfo
breaks <- HM2019_dayIdx

#Relative positions
heading_type <- 'spatial'
t_window <- 600
spatial_R <- 10
min_inds_tracked <- 5
forward <- F
rel_pos <- cocomo::get_positions_relative_to_group(xs = xs, ys = ys, heading_type = heading_type, t_window = t_window, spatial_R = spatial_R, min_inds_tracked = min_inds_tracked, forward = forward)
xy_centr <- cocomo::get_group_centroid(xs = xs, ys = ys, min_inds_tracked = min_inds_tracked)
x_centr <- xy_centr$x_centr
y_centr <- xy_centr$y_centr
head_centr <- cocomo::get_group_heading(xs = xs, ys = ys, heading_type = heading_type, t_window = t_window, spatial_R = spatial_R, forward = forward, min_inds_tracked = min_inds_tracked)
t0 <- 55000
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
lines(x_centr[t0:tf], y_centr[t0:tf], lwd = 2, col = 'black')

#plot group headings at 2 different points
x_group_axis <- x_centr + 20*cos(head_centr)
y_group_axis <- y_centr + 20*sin(head_centr)

arrows(x_centr[t0], y_centr[t0], x_group_axis[t0], y_group_axis[t0], col = 'black')
arrows(x_centr[t0+t_window], y_centr[t0+t_window], x_group_axis[t0+t_window], y_group_axis[t0+t_window],col = 'blue')
arrows(x_centr[t0+2*t_window], y_centr[t0+2*t_window], x_group_axis[t0+2*t_window], y_group_axis[t0+2*t_window],col = 'red')

plotrix::draw.circle(x_centr[t0], y_centr[t0], radius = spatial_R, border = 'black')
points(xs[,t0],ys[,t0],col='black',cex = .5, pch = 19)
plotrix::draw.circle(x_centr[t0+t_window], y_centr[t0+t_window], radius = spatial_R, border = 'blue')
points(xs[,t0+t_window],ys[,t0+t_window],col='blue',cex = .5, pch = 19)
plotrix::draw.circle(x_centr[t0+t_window*2], y_centr[t0+t_window*2], radius = spatial_R, border = 'red')
points(xs[,t0+2*t_window], ys[,t0+2*t_window], col = 'red', cex = .5, pch = 19)


#turn influence

hm2019_dyad <- cocomo::get_turn_and_speed_influence_simplified(xs, ys, heading_type = 'spatial', breaks = breaks, spatial_R = 10, min_percentile = 0.01)
hm2019_centr <- cocomo::get_turn_and_speed_influence_simplified(xs, ys, heading_type = 'spatial', breaks = breaks, spatial_R = 10, min_percentile = 0.01, centroid = T)


#TODO: test turn and speed influence code with visualizations

i <- 2
j <- 6
x <- out_nq2021$lr_speed[i,j,]
y <- -out_nq2021$turn_angle[j,]
bins <- seq(-quantile(x,0.99,na.rm=T),quantile(x,0.99,na.rm=T),length.out=9)
mids <- (bins[1:(length(bins)-1)] + bins[2:length(bins)]) / 2
p <- rep(NA, length(bins)-1)
for(i in 1:(length(bins)-1)){
  idxs <- which(x >= bins[i] & x < bins[i+1])
  p[i] <- mean(y[idxs] > 0, na.rm=T)
}
plot(mids,p,ylim=c(0,1))
abline(h=0.5)
abline(v=0)

#coati data
load('~/Downloads/presidente_cohesive_GPS_data_2024_08_12.RData')
breaks <- c(1,which(diff(as.numeric(cohesive_ts)) > 1)+1)

coati_dyad <- cocomo::get_turn_and_speed_influence_simplified(xs = cohesive_xs, ys = cohesive_ys, heading_type = 'spatial', centroid = F, breaks = breaks, spatial_R = 10, min_percentile = 0.5)
coati_centr <- cocomo::get_turn_and_speed_influence_simplified(xs = cohesive_xs, ys = cohesive_ys, heading_type = 'spatial', centroid = T, breaks = breaks, spatial_R = 10, min_percentile = 0.5)

