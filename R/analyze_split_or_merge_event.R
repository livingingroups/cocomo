#' Analyze split or merge event
#'
#' Analyze (and, if `make_plot=T` make a visualization of) a fission-fusion event.
#'
#' This function takes in information about a fission or fusion event as well as
#' tracking data to identify relevant time points in the fission or fusion event and compute relevant metrics about the event.
#' First, the `start_time` and `end_time` of the event are determined based on
#' a double threshold method, then a `before_time` and `after_time` are identified.
#' This identifies 3 phases of the event: before (`before_time:start_time`), during
#' (`start_time:end_time`) and after (`end_time:after_time`).
#' Finally the displacements and speeds of the subgroups during these phases and various
#' relevant angles are calculated. More details are given below. Note that this function can only
#' consider events involving 2 subgroups.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @section Additional details:
#'
#' *How are the start and end time identified?*
#'
#' As a start, we look at a window of time around an identified event (can be
#' manually or automatically identified). The size of the window is determined by
#' the parameter '`max_time`', and we go from `(tidx - max_time):(tidx + max_time)`
#' where `tidx` is the identified event time index.
#'
#' Within the window, we compute the distance between the centroids of the two
#' subgroups that are splitting or merging at each time point.
#' We then use a double threshold method to determine the start and end point of
#' the fission or fusion event within that time window. To do so, we first
#' categorize all time points as being above the higher threshold `thresh_h` (2),
#' between the two thresholds (1), or below the lower threshold `thresh_l` (0). We
#' then identify contiguous periods of time where the dyadic distance went from
#' 2-111111(any number of ones)-0 (i.e. high-mid-low) for a fusion or 0-1111...-2
#' (i.e. low-mid-high) for a fission. If there are multiple possible time periods
#' detected within the window, we choose the one where the start time is closest to `tidx`. (See also
#' subtlety 1 below).
#'
#' *How are the before and after times identified?*
#'
#' The before time is defined as the time point `tidx - time_window` (default
#' `time_window = 300` time steps), unless the two groups are not sufficiently together (for
#' a fission) or apart (for a fusion) at that time. If the latter, the `before_time`
#' is identified as the point just before the two subgroups cross a threshold `thresh_m` midway
#' between the upper and lower thresholds used to define the start and end times
#' (i.e. usually at `(thresh_l + thresh_h) / 2`, though if the upper or lower
#' thresholds get modified due to *subtlety 1* below, this will also modify `thresh_m`
#' accordingly).
#'
#' The logic here is that, for a fusion we are looking for what the full group (combination of the
#' two eventual subgroups) was doing before they began to split. However, we do not
#' want this point to fall during a prior fusion event, so we require the centroids
#' of the two subgroups to be less than `(thresh_l + thresh_h) / 2` distance apart.
#' The after time is defined in an analogous way, but using the time period after the
#' event. It is usually set to `tidx + time_window`, unless the subgroups come back too
#' close together (for a fission) or go too far apart (for a fusion). Again, we use
#' the time just prior to crossing the midway point between the two thresholds as
#' the `after_time`, if this threshold is crossed before `time_window` seconds has elapsed.
#'
#' *How are the displacements defined?*
#'
#' We compute the displacement of the centroid of each subgroup (group A and group B)
#' as well as the displacement of the centroid of the combined group (group AB) during
#' each of the time windows: before (`before_time:start_time`), during (`start_time:end_time`)
#' and after (`end_time:after_time`).
#'
#' *How are the angles defined?*
#'
#' We define 3 relevant angles relevant to a fission event (might define more later
#' for merge events):
#' `split_angle`: this is the angle at which the two groups split. It is defined as
#' the angle traced out by the points `p_A(end_time)`, `p_AB(start_time)`, and `p_B(end_time)`
#' where `p_A(t)` is the position of subgroup A's centroid at time `t` and likewise
#' for subgroup B and the combined group.
#' `turn_angle_A`: the turning angle of subgroup A. This is defined as the angle
#' formed by the points `p_AB(before_time)`, `p_AB(start_time)`, and `p_A(end_time)`
#' turn_angle_B: likewise for subgroup B
#' Note that all angles use the point `p_AB(start_time)` as their central point
#'
#' *How are departure time difference and departure angular difference calculated?*
#'
#' To calculate the departure time difference and angular difference, we start by computing
#' the centroid of the full group at the event _start time_ - call this the _group start position_.
#' We then determine, for each individual in each subgroup, a _departure time_ that is defined
#' as the first time after _start time_ when that individual crossed a threshold distance (`depart_or_arrive_radius`)
#' from the _start position_. We then compute the _departure heading_ for each individual as the
#' vector pointing from the _group start position_ to the position of the individual at its _departure time_.
#'
#' Once we have computed _departure times_ and _departure headings_ for each individual, we compute
#' an aggregated metric of the disagreement in times and headings for the entire event. For departure time, we take the difference between the `departure_time` of each pair of individuals
#' that are in different subgroups, and then take the mean of these. This is defined as the _departure time difference_.
#' Similarly, for departure heading, we take the difference in headings (angle between vectors) of each pair
#' of individuals in different subgroups, then take the mean of these to get the _departure heading difference_. The angle between vectors is defined between
#' 0 and 180 degrees for each pair of individuals. The time difference is defined in seconds.
#'
#' We can do the equivalent calculations for fusion events. Here, we define the _group end position_
#' as the centroid of the combined group at the `end_time`, and use this position as the reference point
#' for all the other calculations. Looking backward in time, we find the _arrival time_ for each individual,
#' defined as the latest time before the _end time_ where that individual remained outside a threshold
#' distance of `depart_or_arrive_radius`. The headings and time differences are then computed as above, and the
#' differences between times and angles as well.
#'
#' To avoid having different column names for all of the above variables in the code, we create the columns:
#' `group_start_or_end_position` which is defined as the _group start position_ for a fission and the _group end position_ for a fusion,
#' `depart_or_arrive_times` which is a list of _departure times_ (in the case of fissions) or _arrival times_ (in the case of fusions),
#' `depart_or_arrive_headings` which is a lsit of _departure headings_ (in the case of fissions) or _arrival headings_ (in the case of fusions),
#' `depart_or_arrive_time_diff` which is a single number representing the _departure time difference_ (for fissions) or _arrival time difference_ (for fusions), and
#' `depart_or_arrive_heading_diff` which is a single number representing the _departure heading difference_ (for fissions) or _arrival heading difference_ (for fusions).
#'
#' *SUBTLETIES*:
#'
#' *Subtlety 1*: Sometimes, due to the multi-scale nature of these events and the
#' fact that we are approximating subgroup locations with centroids, the dyadic
#' distance does not go below the lower threshold `thresh_l` and/or above the upper
#' threshold `thresh_h`. In this case, we still try to identify the `start_time` and
#' end_time, but modify the thresholds as follows:
#'
#' First, let's define the period between `tidx - max_time` and `tidx` as the *prior period*,
#' the period between `tidx`  and `tidx + max_time` as the *subsequent period*, and the
#' period between `(tidx - max_time/2)` and `(tidx + max_time/2)` as the *middle period*.
#'
#' For a fission, if the dyadic distance does not go above `thresh_h` in the *subsequent period*,
#' then we instead replace `thresh_h` with the maximum - .001 of the dyadic
#' distance during that time period. Second, if the dyadic distance during the *middle period*
#' does not drop below `thresh_l`, then we instead move `thresh_l` to the minimum + .001
#' of the dyadic distance during the *middle period*.
#'
#' For a fusion, everything is reversed. If the dyadic distance does not go
#' above `thresh_h` during the *prior period*, we replace `thresh_h` with the maximum - .001
#' of the dyadic distance during the *subsequent period*. And if the dyadic distance does
#' not go below `thresh_l` during the *middle period*, we replace `thresh_l` with the minimum + .001
#' of the dyadic distance during the *middle period*.
#'
#' In very rare cases, due to these rules the upper bound may get changed to
#' something below the original `thresh_l`, or the lower bound may get changed to
#' something above the original `thresh_h`. In that case, we revert to the original thresholds.
#'
#' *Subtlety 2*: NA handling. NAs are handled in a couple of different ways:
#'
#' For finding the `start_time` and `end_time`, NAs are essentially ignored, and cannot
#' be part of the sequence from `start_time:end_time` (at least not in terms of the)
#' dyadic distance... individuals can drop out and they will just be excluded from
#' the centroid calculation.
#'
#' If no start and end times are found, nothing else is computed (all output values are
#' filled in with `NA`s or `NULL` for matrices).
#'
#' For finding the `before_time` and `after_time`, if an `NA` is hit in the forward or
#' backward direction, the `before_time` (or respectively, the `after_time`) is marked
#' as `NA`. Metrics involving that time point can then not be computed and are also set to
#' `NA`.
#'
#' If the `start_time` and `end_time` are in different data chunks (as specified by `breaks`), then both are given `NA`.
#'
#' If the `before_time` and `start_time`, or `after_time` and `end_time`, are in different data chunks (as specified by `breaks`), then
#' both are given NA.
#'
#' If the `before_time` or `after_time` are `NA`, then other metrics stemming from those times get `NA`
#'
#' @param events a data frame of fission-fusion events, output from `identify_splits_and_merges`
#' @param i index of the row to use in the events table
#' @param xs `n_inds x n_times` matrix of x coordinates
#' @param ys `n_inds x n_times` matrix of y coordinates
#' @param timestamps vector of timestamps of length `n_times` in datetime format
#' @param max_time maximum time steps forward and back to look for the start and end of the event (units are timesteps, not seconds)
#' @param thresh_h upper threshold for determining when subgroups are "apart" (default 50)
#' @param thresh_l lower threshold for determining when subgroups are "together" (default 15)
#' @param depart_or_arrive_radius threshold for determining what an individual has departed (for fissions) or arrived (for fusions) at the group, used in computing departure/arrival times and headings
#' @param time_window time steps to move backward or forward in time to identify the before and after times (units are timesteps, not seconds)
#' @param seconds_per_time_step seconds per time step (default 1)
#' @param breaks indexes to breaks in the data (default NULL treats data as a contiguous sequence). If specified, overrides `break_by_day`
#' @param break_by_day whether to break up data by date (T or F)
#' @param make_plot whether to plot the event (if `T`) or not (if `F`)
#'
#' @returns Returns a list (`out`) of information extracted about the event, as well as a plot (if `make_plot = T`).
#'
#' The list contains:
#'
#' `start_time`: start time index
#'
#' `end_time`: end time index
#'
#' `before_time`: before time index
#'
#' `after_time`: after time index
#'
#' `disps`: matrix of displacements of the different subgroups (rows) during the different
#'   time intervals (columns). Rows and columns are named for easy access.
#'
#' `speeds`: same format as `disps` matrix, but with speeds, in m / s
#'
#' `split_angle`: split angle in degrees, description above
#'
#' `turn_angle_A`: turning angle for subgroup A in degrees, description above
#'
#' `turn_angle_B`: turn angle for subgroup B in degrees, description above
#'
#' `depart_or_arrive_times`: vector of departure (fission) or arrival (fusion) times for each individual in big_group_idxs (from original events table)
#'
#' `depart_or_arrive_headings`: vector of departure (fission) or arrival (fusion) headings for each individual in big_group_idxs (from original events table)
#'
#' `depart_or_arrive_time_diff`: mean departure (fission) or arrival (fusion) time difference across all individuals not in the same subgroup
#'
#' `depart_or_arrive_heading_diff`: mean departure (fission) or arrival (fusion) heading difference across all individuals not in the same subgroup
#'
#' The plot shows (top) dyadic distance over time with lines showing the identified times
#' and (bottom) a visualization of trajectories of the two subgroups.
#'
#' @importFrom stringr str_locate_all
#' @importFrom lubridate date
#'@export
analyze_split_or_merge_event <- function(events, i,
                                         xs, ys, timestamps,
                                         max_time = 600,
                                         thresh_h = 50, thresh_l = 15,
                                         depart_or_arrive_radius = 15,
                                         time_window = 300,
                                         seconds_per_time_step = 1,
                                         breaks = NULL, break_by_day = F,
                                         make_plot = T){

  #get info about the event from the events data frame
  t_event <- events$tidx[i] #time of the event
  group_A <- events$group_A_idxs[i][[1]] #group A individual idxs
  group_B <- events$group_B_idxs[i][[1]] #group B individual idxs
  group_A_names <- events$group_A[i] #group A names
  group_B_names <- events$group_B[i] #group B names
  event_type <- events$event_type[i] #event type - fission, fusion, or shuffle

  if(!(event_type %in% c('fission','fusion'))){
    stop('event must be a fission or fusion')
  }

  #calculate a few metrics from event info
  ti <- t_event - max_time #initial time to plot
  tf <- t_event + max_time #final time to plot
  nA <- length(group_A) #number of individuals in subgroup A
  nB <- length(group_B) #number of individuals in subgroup B
  nT <- ncol(xs) #number of time points overall

  if(break_by_day){
    days <- lubridate::date(timestamps)
    day_start_idxs <- c(1, which(diff(days)==1)+1)
    day_start_idxs <- c(day_start_idxs, length(timestamps)+1)
    if(!exists('breaks')){
      breaks <- day_start_idxs
    }
  }

  #if braeks is null, create a breaks variable specifying one data chunk
  if(is.null(breaks)){
    breaks <- c(1, length(timestamps) + 1)
  }

  #if needed, add an end point to breaks
  if(exists('breaks')){
    if(breaks[length(breaks)] < (length(timestamps) + 1)){
      breaks <- c(breaks, length(timestamps) + 1)
    }
  }

  #if time of event is too close too the beginning or end of the tracking period, return empty list
  if(ti <1 | tf > nT){
    out <- list()
    out$start_time <- out$end_time <- out$before_time <- out$after_time <- NA
    out$disps <- out$speeds <- NULL
    out$turn_angle_A <- out$turn_angle_B <- out$split_angle <- NA
    return(out)
  }

  #get x and y coordinates of the relevant individuals in each subgroup
  xA <- matrix(xs[group_A,],nrow=nA,ncol=nT)
  xB <- matrix(xs[group_B,],nrow=nB,ncol=nT)
  yA <- matrix(ys[group_A,],nrow=nA,ncol=nT)
  yB <- matrix(ys[group_B,],nrow=nB,ncol=nT)

  #get centroids of the two subgroups
  xcA <- colMeans(xA, na.rm=T)
  ycA <- colMeans(yA, na.rm=T)
  xcB <- colMeans(xB, na.rm=T)
  ycB <- colMeans(yB, na.rm=T)

  #get distance between centroids
  dyad_dist <- sqrt((xcA - xcB)^2 + (ycA - ycB)^2)

  #classify the dyadic distance into categories:
  #0 = below lower thershold
  #1 = between thresholds
  #2 = above higher threshold
  dyad_dist_event <- dyad_dist[ti:tf]

  #first consider modifying thresholds according to subtlety 1 above
  upper <- thresh_h
  lower <- thresh_l
  after_idxs <- (max_time+1):(2*max_time+1) #indexes after the marked event
  middle_idxs <- (max_time / 2):(max_time*3/2)
  before_idxs <- 1:max_time #indexes before the marked event
  if(event_type == 'fission'){
    if(sum(!is.na(dyad_dist_event[after_idxs]))>1){
      if(max(dyad_dist_event[after_idxs],na.rm=T) < thresh_h){
        upper <- max(dyad_dist_event[after_idxs],na.rm=T) - .001
      } else{
        upper <- thresh_h
      }
    }
    if(sum(!is.na(dyad_dist_event[middle_idxs]))>1){
      if(min(dyad_dist_event[middle_idxs],na.rm=T) > thresh_l){
        lower <- min(dyad_dist_event[middle_idxs],na.rm=T) + .001
      } else{
        lower <- thresh_l
      }
    }
  }
  if(event_type == 'fusion'){
    if(sum(!is.na(dyad_dist_event[before_idxs]))>1){
      if(max(dyad_dist_event[before_idxs],na.rm=T) < thresh_h){
        upper <- max(dyad_dist_event[before_idxs],na.rm=T) - .001
      } else{
        upper <- thresh_h
      }
    }
    if(sum(!is.na(dyad_dist_event[middle_idxs]))>1){
      if(min(dyad_dist_event[middle_idxs],na.rm=T) > thresh_l){
        lower <- min(dyad_dist_event[middle_idxs],na.rm=T) + .001
      } else{
        lower <- thresh_l
      }
    }
  }

  #if the upper bound was changed to something < thresh_l, move threshold back to thresh_h
  if(upper <= thresh_l){
    upper <- thresh_h
  }
  #likewise for lower bound
  if(lower >= thresh_h){
    lower <- thresh_l
  }

  #middle threshold is average of upper and lower (modified from earlier, used to be average of original thresh_l and thresh_h)
  thresh_m <- (upper + lower)/2

  #get category of each moment in time
  #0 = below lower, 1 = middle, 2 = above upper
  category <- rep(NA, length(dyad_dist_event))
  category[which(dyad_dist_event < lower)] <- 0
  category[which(dyad_dist_event >= lower & dyad_dist_event < upper)] <- 1
  category[which(dyad_dist_event >= upper)] <- 2
  category[which(is.na(dyad_dist_event))] <- 3 #NAs are denoted with 3

  #run length encoding to get sequences of each category
  seqs <- rle(category)

  #find sequences of high-middle-low (2,1,0) or low-mid-high (0,1,2)
  seqs_str <- paste0(as.character(seqs$values), collapse = '') #convert to string

  #find sequences that go 0-1-2 (low-middle-high) for a fission)
  if(event_type=='fission'){
    event_loc <- as.data.frame(stringr::str_locate_all(seqs_str,'012')[[1]])
  }
  #find sequences that go 2-1-0 (high-middle-lower) for a fusion)
  if(event_type == 'fusion'){
    event_loc <- as.data.frame(stringr::str_locate_all(seqs_str,'210')[[1]])
  }

  #for seuqneces of h-m-l or l-m-h (for fission and fusion respectively), get the time index when they start and end
  if(nrow(event_loc)>0){
    for(r in 1:nrow(event_loc)){
      event_loc$start_time[r] <- ti + sum(seqs$lengths[1:event_loc$start[r]]) - 1
      event_loc$end_time[r] <- ti + sum(seqs$lengths[1:event_loc$end[r]-1])
    }
  }

  #create an object to output the start and end times
  start_time <- event_loc$start_time
  end_time <- event_loc$end_time

  #if there is more than one start time, go with the closest to the originally identified fission or fusion point
  if(length(start_time)>1){
    ff_time <- events$tidx[i]
    time_diff <- abs(start_time-ff_time)
    start_time <- start_time[which(time_diff==min(time_diff))]
  }

  #if they were equidistant, just choose the first one
  if(length(start_time)>1){
    start_time <- start_time[1]
  }

  #for end time, if there were two of them, choose the first one after start time
  if(length(end_time)>1){
    end_time <- end_time[which(end_time >= start_time)][1]
  }

  #if start time not found (NULL) change to NA
  if(is.null(start_time)){start_time <- NA}
  if(is.null(end_time)){end_time <- NA}

  #if the start time is in a different chunk of data from the end time, make both NA
  if(!is.na(start_time) & !is.na(end_time)){
    start_chunk <- max(which(breaks <= start_time))
    end_chunk <- max(which(breaks <= end_time))
    if(start_chunk != end_chunk){
      start_time <- NA
      end_time <- NA
    }
  }

  #save start and end time to output list
  out <- list(start_time = start_time, end_time = end_time)

  if(!is.na(start_time) & !is.na(end_time)){

    #GET BEFORE AND AFTER TIMES
    #find the before_time and after_time (times before the start time and after
    #the end time of the event)

    #by default, the before_time is time_window steps before the start_time
    min_before_time <- start_time - time_window

    #by default, the after_time is time_window steps after the end_time
    max_after_time <- end_time + time_window

    #go backward in time until the two groups cross thresh_m or until the time window has elapsed
    for(t in seq(start_time, min_before_time, -1)){
      xc_A <- mean(xs[events$group_A_idxs[i][[1]], t], na.rm=T)
      yc_A <- mean(ys[events$group_A_idxs[i][[1]], t], na.rm=T)
      xc_B <- mean(xs[events$group_B_idxs[i][[1]], t], na.rm=T)
      yc_B <- mean(ys[events$group_B_idxs[i][[1]], t], na.rm=T)
      dist_apart <- sqrt((xc_A - xc_B)^2 + (yc_A - yc_B)^2)

      #if you hit an NA, then the before time is undefined. t <- NA and break
      if(is.na(dist_apart)){
        t <- NA
        break
      }

      #if in a fision, the distance apart exceeds thresh_m in the time before the event, then
      #stop and define the before_time as the point where the threshold was crossed
      #this is to ensure that the group is together before a fission during the before period
      if(events$event_type[i] == 'fission' & dist_apart > thresh_m){
        break
      }

      #similarly, if in a fusion, the distance apart falls below thresh_m in the time before the event,
      #then stop and define before_time as the point where the threshold was crossed
      #this is to ensure that the group is actually apart before a fusion event during the before period
      if(events$event_type[i] == 'fusion' & dist_apart < thresh_m){
        break
      }
    }

    #store the time, which will either be determined by the time window or by the
    #subgroups crossing the threshold thresh_m
    before_time <- t

    #if the before time is in a different chunk, make it NA
    if(!is.na(before_time)){
      start_chunk <- max(which(breaks <= start_time))
      before_chunk <- max(which(breaks <= before_time))
      if(start_chunk != before_chunk){
          before_time <- NA
      }
    }

    #same thing as above, but for the after time
    for(t in seq(end_time, max_after_time, 1)){
      xc_A <- mean(xs[events$group_A_idxs[i][[1]], t], na.rm=T)
      yc_A <- mean(ys[events$group_A_idxs[i][[1]], t], na.rm=T)
      xc_B <- mean(xs[events$group_B_idxs[i][[1]], t], na.rm=T)
      yc_B <- mean(ys[events$group_B_idxs[i][[1]], t], na.rm=T)
      dist_apart <- sqrt((xc_A - xc_B)^2 + (yc_A - yc_B)^2)

      #if you hit an NA, then the after time is undefined. t <- NA and break
      if(is.na(dist_apart)){
        t <- NA
        break
      }
      if(events$event_type[i] == 'fission' & dist_apart < thresh_m){
        break
      }
      if(events$event_type[i] == 'fusion' & dist_apart > thresh_m){
        break
      }
    }

    #store the after time, which will either be determined by the time_window
    #or by the subgroups crossing the threshold thresh_m
    after_time <- t

    #if the after time is in a different data chunk, make it NA
    if(!is.na(after_time)){
      end_chunk <- max(which(breaks <= end_time))
      after_chunk <- max(which(breaks <= after_time))
      if(end_chunk != after_chunk){
        after_time <- NA
      }
    }

    #store the before and after times for later returning
    out$before_time <- before_time
    out$after_time <- after_time

    #GET SPEEDS BEFORE AND AFTER

    #Get centroid locations for time_before, time_start, time_end, time_after, for the
    #groups A, B, and AB. Store in xs_AB and ys_AB matrices with labeled row (group) and column (time) names
    times <- c(before_time, start_time, end_time, after_time)
    xs_AB <- ys_AB <- matrix(NA, nrow = 3, ncol = length(times))
    row.names(xs_AB) <- row.names(ys_AB) <- c('A','B','AB')
    colnames(xs_AB) <- colnames(ys_AB) <- c('before_time','start_time','end_time','after_time')
    for(t in 1:length(times)){
      if(!is.na(times[t])){
        xs_AB[1,t] <- mean(xs[events$group_A_idxs[i][[1]], times[t]],na.rm=T)
        xs_AB[2,t] <- mean(xs[events$group_B_idxs[i][[1]], times[t]],na.rm=T)
        xs_AB[3,t] <- mean(xs[c(events$group_A_idxs[i][[1]],events$group_B_idxs[i][[1]]), times[t]],na.rm=T)
        ys_AB[1,t] <- mean(ys[events$group_A_idxs[i][[1]], times[t]],na.rm=T)
        ys_AB[2,t] <- mean(ys[events$group_B_idxs[i][[1]], times[t]],na.rm=T)
        ys_AB[3,t] <- mean(ys[c(events$group_A_idxs[i][[1]],events$group_B_idxs[i][[1]]), times[t]],na.rm=T)
      }
    }

    #get displacements before during and after and store in disps matrix
    xdiffs <- t(diff(t(xs_AB)))
    ydiffs <- t(diff(t(ys_AB)))
    colnames(xdiffs) <- colnames(ydiffs) <- c('before','during','after')
    disps <- sqrt(xdiffs^2 + ydiffs^2)

    #store disps matrix in output list
    out$disps <- disps

    #speeds in m / min
    dts <- diff(times)
    speeds <- disps
    for(j in 1:length(dts)){
      speeds[,j] <- disps[,j] / (dts[j] * seconds_per_time_step)
    }

    out$speeds <- speeds

    #GET ANGLES
    #split angle, turn angle of A, turn angle of B relative to initial group heading
    split_angle <- get_angle_between_vectors(x1_i = xs_AB['AB','start_time'],
                                         y1_i = ys_AB['AB','start_time'],
                                         x1_f = xs_AB['A','end_time'],
                                         y1_f = ys_AB['A', 'end_time'],
                                         x2_i = xs_AB['AB', 'start_time'],
                                         y2_i = ys_AB['AB', 'start_time'],
                                         x2_f = xs_AB['B', 'end_time'],
                                         y2_f = ys_AB['B', 'end_time'])
    turn_angle_A <- get_angle_between_vectors(x1_i = xs_AB['AB','before_time'],
                                          y1_i = ys_AB['AB','before_time'],
                                          x1_f = xs_AB['AB','start_time'],
                                          y1_f = ys_AB['AB', 'start_time'],
                                          x2_i = xs_AB['AB', 'start_time'],
                                          y2_i = ys_AB['AB', 'start_time'],
                                          x2_f = xs_AB['A', 'end_time'],
                                          y2_f = ys_AB['A', 'end_time'])
    turn_angle_B <- get_angle_between_vectors(x1_i = xs_AB['AB','before_time'],
                                          y1_i = ys_AB['AB','before_time'],
                                          x1_f = xs_AB['AB','start_time'],
                                          y1_f = ys_AB['AB', 'start_time'],
                                          x2_i = xs_AB['AB', 'start_time'],
                                          y2_i = ys_AB['AB', 'start_time'],
                                          x2_f = xs_AB['B', 'end_time'],
                                          y2_f = ys_AB['B', 'end_time'])

    out$turn_angle_A <- turn_angle_A
    out$turn_angle_B <- turn_angle_B
    out$split_angle <- split_angle
  } else{

    #if the start or end time are NA, return NAs and NULLs for the different metrics
    out$start_time <- out$end_time <- out$before_time <- out$after_time <- NA
    out$turn_angle_A <- out$turn_angle_B <- out$split_angle <- NA
    out$disps <- out$speeds <- NULL
  }

  #GET DEPARTURE OR ARRIVAL TIMES AND HEADINGS FOR ALL INDIVIDUALS, AND TIME / HEADING DIFFERENCES FOR FULL EVENT
  big_group_idxs <- events$big_group_idxs[i][[1]]
  if(!is.na(start_time) & !is.na(end_time)){
    if(event_type == 'fission'){
      group_start_or_end_position_x <- mean(xs[big_group_idxs, start_time], na.rm=T)
      group_start_or_end_position_y <- mean(ys[big_group_idxs, start_time], na.rm=T)
    }
    if(event_type == 'fusion'){
      group_start_or_end_position_x <- mean(xs[big_group_idxs, end_time], na.rm=T)
      group_start_or_end_position_y <- mean(ys[big_group_idxs, end_time], na.rm=T)
    }

    #get distances of all individuals in the big group from the group_start_or_end_position
    dists_from_group_start_or_end_position <- sqrt((xs[big_group_idxs,] - group_start_or_end_position_x)^2 + (ys[big_group_idxs,] - group_start_or_end_position_y)^2)

    #get departure or arrival times for all individuals in the big group
    depart_or_arrive_times <- rep(NA, length(big_group_idxs))
    for(ind in 1:length(big_group_idxs)){
      times_outside <- which(dists_from_group_start_or_end_position[ind,] > depart_or_arrive_radius)
      if(event_type == 'fission'){
        depart_or_arrive_times[ind] <- min(times_outside[which(times_outside > start_time)])
      }
      if(event_type == 'fusion'){
        depart_or_arrive_times[ind] <- max(times_outside[which(times_outside < end_time)])
      }
    }

    #get departure or arrival headings for each individual
    if(event_type == 'fission'){
      dxs <- xs[cbind(big_group_idxs, depart_or_arrive_times)] - group_start_or_end_position_x
      dys <- ys[cbind(big_group_idxs, depart_or_arrive_times)] - group_start_or_end_position_y
    }
    if(event_type == 'fusion'){
      dxs <- -xs[cbind(big_group_idxs, depart_or_arrive_times)] + group_start_or_end_position_x
      dys <- -ys[cbind(big_group_idxs, depart_or_arrive_times)] + group_start_or_end_position_y
    }
    depart_or_arrive_headings <- atan2(dys, dxs)

    #make a matrix determining whether each pair of individuals is in the same subgroup or not, for computing time and heading diffs
    time_diff_tot <- ang_diff_tot <- n_comparisons <- 0
    for(i in 1:(length(big_group_idxs)-1)){
      for(j in i:length(big_group_idxs)){

        #if the individuals are in the same subgroup, don't include them
        if((big_group_idxs[i] %in% group_A & big_group_idxs[j] %in% group_A) |
           (big_group_idxs[i] %in% group_B & big_group_idxs[j] %in% group_B)){
          next
        }

        #otherwise, add their absolute time difference and angle to the calculation
        time_diff_tot <- time_diff_tot + abs(depart_or_arrive_times[i] - depart_or_arrive_times[j])*seconds_per_time_step
        ang_diff_tot <- ang_diff_tot + acos(cos(depart_or_arrive_headings[i])*cos(depart_or_arrive_headings[j]) +
                                      sin(depart_or_arrive_headings[i])*sin(depart_or_arrive_headings[j]))
        n_comparisons <- n_comparisons + 1

      }
    }
    time_diff <- time_diff_tot / n_comparisons
    ang_diff <- ang_diff_tot / n_comparisons

    out$depart_or_arrive_times <- depart_or_arrive_times
    out$depart_or_arrive_headings <- depart_or_arrive_headings
    out$depart_or_arrive_time_diff <- time_diff
    out$depart_or_arrive_heading_diff <- ang_diff


  }

  #make a plot if desired
  if(make_plot == T){
    par(mfrow=c(2,1))
    plot(ti:tf, dyad_dist[ti:tf],type='l', main = paste(event_type, timestamps[t_event]),xlab='Time (min)',ylab = 'Distance apart (m)')
    abline(v=t_event,col='black', lty = 2)
    abline(h = thresh_h, col = 'darkorange1')
    abline(h = thresh_l, col = 'magenta')
    abline(h = thresh_m, col = '#AAAAAA')
    abline(v=out$start_time, col = 'green')
    abline(v=out$end_time, col = 'red')
    abline(v=out$before_time, col = '#004400')
    abline(v=out$after_time, col = '#440000')

    xmin <- min(min(xA[,ti:tf],na.rm=T),min(xB[,ti:tf],na.rm=T))
    xmax <- max(max(xA[,ti:tf],na.rm=T),max(xB[,ti:tf],na.rm=T))
    ymin <- min(min(yA[,ti:tf],na.rm=T),min(yB[,ti:tf],na.rm=T))
    ymax <- max(max(yA[,ti:tf],na.rm=T),max(yB[,ti:tf],na.rm=T))
    plot(NULL, xlim=c(xmin,xmax),ylim=c(ymin,ymax),asp=1, xlab='Easting', ylab = 'Northing', main = paste('(Red =', group_A_names, '), (Blue =', group_B_names,')'))
    for(j in 1:nrow(xA)){
      lines(xA[j,ti:tf],yA[j,ti:tf],type='l',col='#FFAA0033')
    }
    for(j in 1:nrow(xB)){
      lines(xB[j,ti:tf],yB[j,ti:tf],type='l',col='#0000FF33')
    }
    lines(xcA[ti:tf],ycA[ti:tf], col = '#FFAA00', lwd = 2)
    lines(xcB[ti:tf],ycB[ti:tf], col = '#0000FF', lwd = 2)

    points(xcA[t_event], ycA[t_event], pch = 8, col = 'black')
    points(xcB[t_event], ycB[t_event], pch = 8, col = 'black')
    points(xcA[ti], ycA[ti], pch = 1, col = 'black')
    points(xcB[ti], ycB[ti], pch = 1, col = 'black')
    points(xcA[tf], ycA[tf], pch = 4, col = 'black')
    points(xcB[tf], ycB[tf], pch = 4, col = 'black')

    #algorithm-identified start and end times
    points(xcA[out$start_time],ycA[out$start_time],pch = 1, col = 'green')
    points(xcB[out$start_time],ycB[out$start_time],pch = 1, col = 'green')
    points(xcA[out$end_time],ycA[out$end_time],pch = 4, col = 'red')
    points(xcB[out$end_time],ycB[out$end_time],pch = 4, col = 'red')

  }

  #return the output object
  invisible(out)
}

