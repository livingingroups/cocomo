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


#level 0 to level 1 example
level_0_file <- '~/Dropbox/hyenas/hyena_data/RData/hyena_gps_level0.RData'
level_1_file <- '~/Desktop/test_hyena_level1.RData'
level1out <- cocomo::preprocess_gps_level0_to_level1(input_file_path = level_0_file,
                                             output_file_path = level_1_file)

load('~/Dropbox/meerkats/processed_data/RW2021_COORDINATES_all_sessions.RData')
xs <- RW2021_allX
ys <- RW2021_allY
ids <- RW2021_indInfo
timestamps <- RW2021_timeLine
breaks <- RW2021_dayIdx
breaks <- breaks[1:(length(breaks)-1)]
level_1_file <- '~/Desktop/test_meerkat_level1.RData'
level1out <- cocomo::preprocess_gps_level0_to_level1(xs = xs, ys = ys, timestamps = timestamps,
                                                     ids = ids, breaks = breaks,
                                                     output_file_path = level_1_file)

#visualization test data
start_time <- 5000
end_time <- 6000
time_step <- 5
calls <- data.frame(ind_idx = c(rep(1,20), rep(2,20)),
                    time_idx = round(seq(start_time, end_time, length.out = 40)),
                    call_type = c(rep('cc',10), rep('sn',10),rep('al',10),rep('cc',10)))
cocomo::generate_movement_and_calls_visualization(xs = xs, ys = ys, timestamps = timestamps,
                                                  calls = calls, start_time = start_time,
                                                  end_time = end_time, time_step = 1, output_dir = '~/Desktop',
                                                  tail_time = 60, call_persist_time = 60,
                                                  scalebar_size = 10, scalebar_offset = 40)

#plot y conditioned on x
pol <- cocomo::get_group_polarization(xs, ys, heading_type = 'temporal', t_window = 10, min_inds_tracked = 3)
head_speed <- cocomo::get_group_heading_and_speed(xs, ys, heading_type = 'temporal',t_window = 10 ,min_inds_tracked = 3)
speed <- head_speed$speeds
test <- cocomo::plot_y_conditioned_on_x(pol, speed)

#hyena influence matrices
load('~/EAS_shared/hyena/working/processed/cc23/hyena_south2023_xy_level0_30s.RData')
load('~/EAS_shared/hyena/working/processed/cc23/hyena_ids.RData')

hyena_ids$color <- '#FF0000'
hyena_ids$color[which(hyena_ids$sex == 'm' & hyena_ids$status == 'i')] <- '#0000FF'
hyena_ids$color[which(hyena_ids$sex == 'm' & hyena_ids$status == 'r')] <- '#AAAAFF'
hyena_influence <- cocomo::get_turn_and_speed_influence_simplified(xs, ys, heading_type = 'spatial', centroid = F, spatial_R = 10, seconds_per_time_step = 30, max_distance = 300, min_past_speed = 1)
pdf(file = '~/EAS_shared/hyena/working/analysis/hyena_influence_test.pdf', width = 10, height = 10)
par(mfrow=c(2,2))
zlim <- c(0.15,0.85)
cocomo::plot_network_matrix(mat = hyena_influence$turn_influence_movement[-c(8, 12, 19), -c(8, 12, 19)], ind_names = hyena_ids$id[-c(8, 12, 19)], ind_cols = hyena_ids$color[-c(8, 12, 19)], main = 'Movement turn influence', zlim = zlim)
cocomo::plot_network_matrix(mat = hyena_influence$turn_influence_position[-c(8, 12, 19), -c(8, 12, 19)], ind_names = hyena_ids$id[-c(8, 12, 19)], ind_cols = hyena_ids$color[-c(8, 12, 19)], main = 'Position turn influence', zlim = zlim)
cocomo::plot_network_matrix(mat = hyena_influence$speed_influence_movement[-c(8, 12, 19), -c(8, 12, 19)], ind_names = hyena_ids$id[-c(8, 12, 19)], ind_cols = hyena_ids$color[-c(8, 12, 19)], main = 'Movement speed influence', zlim = zlim)
cocomo::plot_network_matrix(mat = hyena_influence$speed_influence_position[-c(8, 12, 19), -c(8, 12, 19)], ind_names = hyena_ids$id[-c(8, 12, 19)], ind_cols = hyena_ids$color[-c(8, 12, 19)], main = 'Position speed influence', zlim = zlim)
dev.off()

#analyze split/merge
load('~/Dropbox/coati/processed/galaxy/galaxy_xy_highres_level1.RData')
load('~/Dropbox/coati/processed/galaxy/galaxy_coati_ids.RData')
timestamps <- ts
rm(ts)
breaks <- c(1, which(diff(timestamps) > 1) + 1)
out <- cocomo::identify_splits_and_merges(xs = xs, ys = ys, timestamps = ts, R_inner = 15, R_outer = 50, breaks = breaks, names = coati_ids$name)
events <- out$events_detected
out_analyze <- cocomo::analyze_split_or_merge_event(i = 7, events = events, xs = xs, ys = ys, timestamps = timestamps, max_time = 600, thresh_h = 50, thresh_l = 15, breaks = breaks)
