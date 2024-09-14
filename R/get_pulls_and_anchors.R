#' Get pulls and anchors
#'
#' Get successful ('pull') and failed ('anchor') dyadic interactions between individuals a and b.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xa: x coordinates for individual a (vector of length `n_times`)- must be continuous data (no NAs)
#' @param	xb: x coordinates for individual b (vector of length `n_times`)- must be continuous data (no NAs)
#' @param ya: x coordinates for individual a (vector of length `n_times`)- must be continuous data (no NAs)
#' @param	yb: x coordinates for individual b (vector of length `n_times`)- must be continuous data (no NAs)
#' @param a: index of the first individual
#' @param	b: index of the second individual
#' @param	noise_thresh: noise threshold (defaults to 5 m)
#' @param plot_results: whether to plot results or not
#' @param include_half_events: if T, the function will also output "half events" at the beginning and end of the sequence. This is used in conjunction with fission-fusion analyses. In most use cases, this should be set to false. See below for details.
#'
#'
#' @section Details on half events
#'
#' If `include_half_events` is set to T, the script will also return the
#' "half events" defined by the beginning of the sequence and the first minimum
#' (if the first max/min value is a minimum), and the end of the sequence and
#' the last minimum (if the last max/min value is a minimum). If the first or
#' last min/max values are not minima, half events will not be returned for them.
#' The purpose of this option is that if this code is used on a sequence where two
#' individuals come together and then split apart (in fission-fusion analyses), the
#' first and last "half events" represent a fusion and fission event, so these
#' are extracted. In normal use cases, this value should be set to F (default).
#' However in cases where this analysis is being run on the "together periods"
#' of two individuals in a fission-fusion analysis, it can be set to T to return
#' the times and initiators of the initial fusion and eventual fission event.
#' In the case of half events, there are only two time points return (`t1` and `t2`).
#' `t3` is defined as `NA`. The `leader` is defined as the individual with the greater
#' displacement during the period `t1` to `t2` and the `follower` is the other
#' individual. The `strength` and `disparity` are defined in parallel to those for
#' pulls and anchors, but using only the one time period (`t1` to `t2`). To make the
#' values somewhat comparable to pulls and anchors, these values are squared.
#'
#'
#' @returns
#' Returns a data frame  containing dyadic interactions between a and b.
#' Contains columns: `t1`, `t2`, `t3`, `leader`, `follower`, `type`, `disparity`, `strength`,`disparity_additive`, and `strength_additive`.
#' The `disparity` and `strength` are as defined in Strandburg-Peshkin et al. 2015, whereas
#' `disparity_additive` and `strength_additive` are alternative formulations of these metrics that add the components together instead of multiplying them.
#'
#' @export
get_pulls_and_anchors <- function(xa, xb, ya, yb, a, b, noise_thresh = 5, plot_results = F, include_half_events = F){

  #get the number of times
  n_times <- length(xa)

  #check to make sure xa, xb, ya, and yb all contain the same number of elements
  if(length(xb) != n_times | length(ya) != n_times | length(yb) != n_times){stop('x and y data vectors are not all the same length')}

  #check to make sure data streams are continuous
  if(any(is.na(c(xa,xb,ya,yb)))){stop('x and/or y data contain NAs for the specified individuals')}

  #get dyadic distances over time
  dyad_dist <- sqrt((xa-xb)^2 + (ya-yb)^2)

  #---find first minimum---
  #get first instance of a change in dyadic distance above noise.thresh, and determine whether it is going up or down
  i <- 1
  curr_min <- 1
  found = F
  while(found == F & i < length(dyad_dist)){
    dist_change <- dyad_dist[i] - dyad_dist[1]
    if(dyad_dist[i]<dyad_dist[curr_min]){
      curr_min <- i
    }
    if(abs(dist_change)>noise_thresh){
      found = T
      if(dist_change > 0){
        up_first <- T
      }
      else{
        up_first <- F
      }
    }
    else{
      i <- i + 1
    }
  }

  #if no first minimum found, return NULL
  if(!found){
    return(NULL)
  }

  #find starting point (if it went up first, this is curr.min, otherwise, this is the first minimum)
  if(up_first){
    first <- curr_min
  } else{
    curr_min <- 1
    found <- F
    while(i < n_times & !found){
      dist_diff <- dyad_dist[i] - dyad_dist[curr_min]
      if(dist_diff < 0){
        curr_min <- i
      }
      else{
        if(dist_diff > noise_thresh){
          found <- T
          first <- curr_min
        }
      }
      i<-i+1
    }
    if(!found){ #if no starting point found, return NULL
      return(NULL)
    }
  }

  #----pull out maxes and mins in dyadic distance----
  min_max_min <- data.frame(t1 = NA, t2 = NA, t3 = NA)
  min_max_min$t1[1] <- first
  ref_idx <- first
  ref_val <- dyad_dist[first]
  data_idx <- 1
  going_up <- T
  for(i in first:n_times){
    curr_idx <- i
    curr_val <- dyad_dist[i]
    ref_val <- dyad_dist[ref_idx]
    if(going_up){ #if going up
      if(curr_val > ref_val){ #if the current value is greater than the reference value
        ref_idx <- curr_idx #replace the reference value with the current value
        ref_val <- dyad_dist[ref_idx]
      }
      else{ #if not
        if(abs(curr_val - ref_val) > noise_thresh){ #if the difference between current and reference exceeds the noise threshold
          min_max_min$t2[data_idx] <- ref_idx #record the local max
          going_up <- F #switch to going down
          ref_idx <- i #ref index becomes the current index
        }
      }
    }
    else{ #if going down
      if(curr_val < ref_val){ #if the current value is less than the reference value
        ref_idx <- curr_idx #replace the reference value with the current value
        ref_val <- dyad_dist[ref_idx]
      }
      else{ #if not
        if(abs(curr_val - ref_val) > noise_thresh){ #if the difference between current and reference exceeds a noise threshold
          min_max_min$t3[data_idx] <- ref_idx #record the local min as the end of the current sequence
          data_idx <- data_idx + 1 #increment the data index
          min_max_min <- rbind(min_max_min,c(ref_idx,NA,NA)) #record the local min as the start of the next sequence #USED TO BE REF.IDX
          going_up <- T #switch to going up
          ref_idx <- i #ref_idx becomes the current index
        }
      }
    }
  }

  if(plot_results){
    plot(dyad_dist)
    for(i in 1:nrow(min_max_min)){
      abline(v=min_max_min$t1[i],col='red')
      abline(v=min_max_min$t2[i],col='green')
      abline(v=min_max_min$t3[i],col='red')
    }
  }

  #extract half events (if specified)
  #the first half event runs from 1 to the first minimum found, and is a fusion
  if(include_half_events){
    fusion <- data.frame(t1 = 1, t2 = min_max_min$t1[1], type = 'fusion')
    fission <- data.frame(t1 = min_max_min$t1[nrow(min_max_min)], t2 = n_times, type = 'fission')
    half_events <- rbind(fusion, fission)
  }

  #exclude any half events (missing t3) from the main events table
  min_max_min <- min_max_min[which(!is.na(min_max_min$t3)),]

  #if no min_max_min sequences were found, and not including half events, return NULL
  if(nrow(min_max_min)==0 & !include_half_events){
    return(NULL)
  }

  if(nrow(min_max_min) > 0){
    #---determine whether they are successful pulls ('pull') or failed pulls ('anchor') and who is leading (attempting to pull)

    #get displacements for each individual during t1 to t2 (1) and t2 to t3 (2)
    min_max_min$disp_a_1 <- sqrt((xa[min_max_min$t2] - xa[min_max_min$t1])^2 + (ya[min_max_min$t2] - ya[min_max_min$t1])^2)
    min_max_min$disp_a_2 <- sqrt((xa[min_max_min$t3] - xa[min_max_min$t2])^2 + (ya[min_max_min$t3] - ya[min_max_min$t2])^2)
    min_max_min$disp_b_1 <- sqrt((xb[min_max_min$t2] - xb[min_max_min$t1])^2 + (yb[min_max_min$t2] - yb[min_max_min$t1])^2)
    min_max_min$disp_b_2 <- sqrt((xb[min_max_min$t3] - xb[min_max_min$t2])^2 + (yb[min_max_min$t3] - yb[min_max_min$t2])^2)

    #the leader is the one who moves more during the first time interval, the follower the one that moves less
    min_max_min$leader <- a
    min_max_min$follower <- b
    min_max_min$leader[which(min_max_min$disp_b_1 > min_max_min$disp_a_1)] <- b
    min_max_min$follower[which(min_max_min$disp_b_1 > min_max_min$disp_a_1)] <- a

    #it's a pull if the leader moves less during the second time interval
    min_max_min$type <- 'pull'
    min_max_min$type[which((min_max_min$leader == a) & (min_max_min$disp_a_2 > min_max_min$disp_b_2))] <- 'anchor'
    min_max_min$type[which((min_max_min$leader == b) & (min_max_min$disp_b_2 > min_max_min$disp_a_2))] <- 'anchor'

    #get the disparity of each event
    min_max_min$disparity <- (abs(min_max_min$disp_a_1 - min_max_min$disp_b_1) * abs(min_max_min$disp_a_2 - min_max_min$disp_b_2))/((min_max_min$disp_a_1 + min_max_min$disp_b_1)*(min_max_min$disp_a_2 + min_max_min$disp_b_2))
    min_max_min$disparity_additive <- ((abs(min_max_min$disp_a_1 - min_max_min$disp_b_1) + abs(min_max_min$disp_a_2 - min_max_min$disp_b_2)))/
      (((min_max_min$disp_a_1 + min_max_min$disp_b_1) + (min_max_min$disp_a_2 + min_max_min$disp_b_2)))

    #get the strength of each event
    min_max_min$strength <- (abs(dyad_dist[min_max_min$t2] - dyad_dist[min_max_min$t1])*abs(dyad_dist[min_max_min$t3] - dyad_dist[min_max_min$t2]))/((dyad_dist[min_max_min$t2] + dyad_dist[min_max_min$t1])*(dyad_dist[min_max_min$t2] + dyad_dist[min_max_min$t3]))
    min_max_min$strength_additive <- ((abs(dyad_dist[min_max_min$t2] - dyad_dist[min_max_min$t1]) + abs(dyad_dist[min_max_min$t3] - dyad_dist[min_max_min$t2])))/
      (((dyad_dist[min_max_min$t2] + dyad_dist[min_max_min$t1]) + (dyad_dist[min_max_min$t2] + dyad_dist[min_max_min$t3])))

    #remove un-needed columns
    events <- min_max_min[,c('t1','t2','t3','leader','follower','type','disparity','strength','disparity_additive','strength_additive'),]

  } else{
    events <- NULL
  }

  #if including half events, get the equivalent info for them
  if(include_half_events){
    half_events$disp_a_1 <- sqrt((xa[half_events$t2] - xa[half_events$t1])^2 + (ya[half_events$t2] - ya[half_events$t1])^2)
    half_events$disp_b_1 <- sqrt((xb[half_events$t2] - xb[half_events$t1])^2 + (yb[half_events$t2] - yb[half_events$t1])^2)

    #the leader is the one who moves more during the first time interval, the follower the one that moves less
    half_events$leader <- a
    half_events$follower <- b
    half_events$leader[which(half_events$disp_b_1 > half_events$disp_a_1)] <- b
    half_events$follower[which(half_events$disp_b_1 > half_events$disp_a_1)] <- a

    #get the disparity of each event
    half_events$disparity <- (half_events$disp_a_1 - half_events$disp_b_1)^2/(half_events$disp_a_1 + half_events$disp_b_1)^2
    half_events$disparity_additive <- abs(half_events$disp_a_1 - half_events$disp_b_1) / (half_events$disp_a_1 + half_events$disp_b_1)

    #get the strength of each event
    half_events$strength <- abs(dyad_dist[half_events$t2] - dyad_dist[half_events$t1])/(dyad_dist[half_events$t2] + dyad_dist[half_events$t1])
    half_events$strength_additive <- abs(dyad_dist[half_events$t2] - dyad_dist[half_events$t1])/ (dyad_dist[half_events$t2] + dyad_dist[half_events$t1])

    #add columns with NAs to make equivalent to min_max_min table
    half_events$t3 <- NA

    #remove un-needed columns
    half_events <- half_events[,c('t1','t2','t3','leader','follower','type','disparity','strength','disparity_additive','strength_additive'),]

    #combine the two events tables (if the first one was not empty) otherwise just use the half events table
    if(!is.null(events)){
      events <- rbind(events, half_events)
    } else{
      events <- half_events
    }

  }

  #return all events
  return(events)

}
