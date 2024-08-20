#'Identify group splits and merges from multi-individual trajectory data
#'
#'Detects splits and merges (a.k.a. fissions and fusions) using "sticky-DBSCAN"
#'method from Libera et al. 2023.
#'
#'Start by defining an adjacency matrix (`together` in the code) of which dyads
#'are "connected" at any moment in time. Dyads are considered to be connected if
#'they go within a distance `R_inner` of one another, and they continue to be
#'connected until they leave a distance `R_outer` of one another on both ends
#'(before and after) of the period where their distance dropped below `R_inner`.
#'This double threshold makes periods of connectedness more stable by removing
#'the "flicker" that would result from having a single threshold.
#'
#'NAs are handled by ending a period of connectedness if an individual has an NA
#'at the point immediately before / after (if the were connected after / before
#'that NA). Individuals with NAs are not included in the together matrix and will
#'not be included in the groups.
#'Once connectedness of dyads is determined, merge dyads together into groups by
#'using DBSCAN on 1 - `together` as the distance matrix, with `eps` equal to
#'something small (.01 in the code).
#'Store these groups in `groups_list`, a list of lists whose length is equal to
#'`n_times`.

#'Stepping through the `groups_list`, identify all changes in group membership,
#'i.e. consecutive time points when the groups do not match. The algorithm flags
#' any change, including instances where individuals disappear or reappear in
#' groups due to missing data (but these are later ignored). Store in `changes`
#' data frame.
#'
#'In a last step, identify all splits ('fission'), merges ('fusion'), and things that
#'cannot be classified as either fissions or fusions because they contain elements
#'of both ('shuffle'). This is done by constructing a bipartite network at each time
#'step t, where groups at time t are connected to groups at time t + 1 if they share
#'at least 1 member. Then, we identify the connected components of this bipartite
#'network. Components where a single group (node) at time t is connected to multiple
#'groups (nodes) at time t + 1 get identified and classified as `event_type = 'fission'`.
#'Components where multiple nodes at time t are connected to a single node at time t + 1
#'are classified as `event_type = 'fusion'`. Components where a single node at time t
#'is connected to a single node at time t + 1 are skipped (they are not fissions, fusions,
#'or shuffles). All other events where more complex things happen are classified as
#'`event_type = 'shuffle'`.
#'
#'After events are identified, various event features are computed and saved in a data frame.
#'See list of outputs for more details.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author Eli Strauss (code reviewer, May 2024)
#'
#' @param xs UTM eastings matrix (`n_inds` x `n_times` matrix where xs[i,t] gives the easting of individual i at time step t)
#' @param ys UTM northings matrix (`n_inds` x `n_times` matrix where ys[i,t] gives the northing of individual i at time step t)
#' @param timestamps vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
#' @param R_inner inner distance threshold to identify periods of connectedness (numeric)
#' @param R_outer outer distance threshold to identify periods of connectedness (numeric)
#' @param breaks indexes to breaks in the data (default NULL treats data as a contiguous sequence). If specified, overrides `break_by_day`
#' @param break_by_day whether to break up data by date (T or F)

#' @return a list containing:
#'
#'`events_detected`: data frame with info on detected fissions and fusions, and limited info for shuffles
#'
#'`all_events_info`: list of information about all fission-fusion (or shuffle)
#'events.
#'
#'`groups_list`: list of subgroups in each timestep
#'
#'`together`: n_inds x n_inds x n_times array of whether dyads are
#'connected (1) or not (0) or unknown (NA)
#'
#'`R_inner`: inner radius used in the computations (same as `R_inner` above)
#'
#'`R_outer`: outer radius used in the computations (same as `R_outer` above)
#'
#'@section Additional information about returned objects:
#'
#'`events_detected` data frame:
#'
#'`events_detected$event_idx`: unique id number of the event
#'
#'`events_detected$tidx`: (initial) time index of the event
#'
#'`events_detected$event_type`: "fission" or "fusion" or "shuffle
#'
#'`events_detected$n_groups_before`: number of groups prior to the event
#'
#'`events_detected$n_groups_after`: number of groups after the event
#'
#'`events_detected$big_group_idxs`: indexes of all the individuals involved in the event
#'
#'`events_detected$big_group`: names of all the individuals involved in the event
#'
#'`events_detected$group_A_idxs`, `$group_B_idxs`, `$group_C_idxs`, etc.: individual idxs of subgroup members
#'
#'`events_detected$group_A`, `$group_B`, `$group_C`, etc.: names of subgroup members
#'
#'`events_detected$n_A`, `$n_B`, `$n_C` etc.: number of individuals in each subgroup
#'
#'`events_detected$n_big_group`: number of individuals in the big group (original group for fissions, subseq group for fusions)
#'
#'(NOTE: `big_group_idxs`, `big_group`, `group_A_idxs` etc.,
#'`group_A` etc. `n_A` etc. and `n_big_group` are set to NA for shuffles...
#'you can get more detailed info for shuffles from `all_events_info` object))
#'
#'`all_events_info` list:
#'
#'`all_events_info[[i]]` contains the following info for event i:
#'
#'`all_events_info[[i]]$t`: time index of the event
#'
#'`all_events_info[[i]]$groups_before`: (list of lists) list of groups before the event (at time t)
#'
#'`all_events_info[[i]]$groups_after`: (list of lists) list of groups after the event (at time t + 1)
#'
#'`all_events_info[[i]]event_type`: 'fission', 'fusion', or 'shuffle' (character string)
#'
#'`all_events_info[[i]]$n_groups_before`: number of groups before the event
#'
#'`all_events_info[[i]]$n_groups_after`: number of groups after the event
#'
#'`groups_list` list:
#'
#'`groups_list[[t]]` gives a list of the subgroups
#'
#'`groups_list[[t]][[1]]` gives the vector of the first subgroup, etc.
#'
#' @importFrom lubridate date
#' @importFrom dbscan dbscan
#' @export
identify_splits_and_merges <- function(xs, ys, timestamps, R_inner, R_outer, breaks = c(1, length(timestamps)+1), names = NULL, break_by_day = F, verbose = T){

  #----Identify subgroups at each point
  if(verbose){print('Identifying subgroups at each point using sticky DBSCAN')}
  #number of inds and times
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #day start indexes
  if(break_by_day){
    days <- lubridate::date(timestamps)
    day_start_idxs <- c(1, which(diff(days)==1)+1)
    day_start_idxs <- c(day_start_idxs, length(timestamps)+1)
    if(!exists('breaks')){
      breaks <- day_start_idxs
    }
  }

  #if needed, add an end point to breaks
  if(exists('breaks')){
    if(breaks[length(breaks)] < (length(timestamps) + 1)){
      breaks <- c(breaks, length(timestamps) + 1)
    }
  }

  #Get dyadic distances for each pair, then use double threshold method to determine if they are together at any moment
  dyad_dists <- together <- array(NA, dim = c(n_inds, n_inds, n_times))
  for(i in 1:(n_inds-1)){
    for(j in (i+1):n_inds){

      #dyadic distance
      dx <- xs[i,] - xs[j,]
      dy <- ys[i,] - ys[j,]
      dyad_dists[i,j,] <- sqrt(dx^2 + dy^2)

      #together or not
      #loop over days (if breaking by day) or treat whole dataset as one "day". for each day...
      for(d in 1:(length(breaks)-1)){

        #get times for that day
        t_day <- breaks[d]:(breaks[d+1]-1)

        #dyadic distances on that day
        dyad_dists_ij <- dyad_dists[i,j,t_day]

        if(sum(!is.na(dyad_dists_ij))==0){
          next
        }

        #times when together within inner radius
        together_inner <- dyad_dists_ij <= R_inner

        #times when together within outer radius
        together_outer <- dyad_dists_ij <= R_outer

        together_ij <- together_inner

        #if they are never together, store that and skip to next individual
        if(sum(together_inner,na.rm=T)==0){
          together[i,j,t_day] <- together[j,i,t_day] <- together_ij
          next
        }

        #go backwards from crossing points into inner radius to find the 'starts' when crossed the outer radius
        inner_starts <- which(diff(together_inner)==1)+1  ## Add 1 to make indices of differences line up with indices of together_inner
        if(length(inner_starts)==0){
          together[i,j,t_day] <- together[j,i,t_day] <- together_ij
          next
        }
        for(k in 1:length(inner_starts)){
          crossing <- inner_starts[k]
          curr_time <- crossing
          for(curr_time in seq(crossing,1,-1)){
            ## If NA, treat as though they are outside of together_outer
            if(is.na(together_outer[curr_time])){
              start <- curr_time + 1
              break
            }
            ## If time index 1 is reached, they started together so start of event = start of study
            if(curr_time == 1){
              start <- curr_time
              break
            }
            ## If together_outer switches to F, mark the last T as the start of the event
            if(together_outer[curr_time]==F){
              start <- curr_time + 1
              break
            }

          }
          together_ij[start:crossing] <- T
        }

        #go forwards from crossing points out of outer radius to find the 'ends' when crossed the outer radius
        inner_ends <- which(diff(together_inner)==-1)
        if(length(inner_ends)==0){
          together[i,j,t_day] <- together[j,i,t_day] <- together_ij
          next
        }
        for(k in 1:length(inner_ends)){
          crossing <- inner_ends[k]
          curr_time <- crossing
          for(curr_time in seq(crossing,length(together_ij),1)){
            ## If NA, treat as though they are outside of together_outer
            if(is.na(together_outer[curr_time])){
              end <- curr_time - 1
              break
            }
            ## If the last time index is reached, end of event = end of day
            if(curr_time == length(together_outer)){
              end <- curr_time
              break
            }
            ## If together_outer switches to F, mark the last T as the end of the event
            if(together_outer[curr_time]==F){
              end <- curr_time - 1
              break
            }

          }
          together_ij[crossing:end] <- T
        }

        together[i,j,t_day] <- together[j,i,t_day] <- together_ij

      }
    }
  }

  #Identify groups from together matrices and store in group matrix
  groups <- matrix(NA, nrow = n_inds, ncol = n_times)
  for(t in 1:n_times){
    ## Work only with individuals who have a location for this timepoint
    non.nas <- which(colSums(!is.na(together[,,t]))>0)
    if(length(non.nas)<=1){
      next
    }
    non.nas.together <- together[non.nas,non.nas,t]
    diag(non.nas.together) <- 1
    ## Use DBSCAN to pull out groups (i.e. connected subcomponents of together matrix)
    grps.non.nas <- dbscan(x = as.dist(1 - non.nas.together), eps = .1,minPts=1)$cluster
    groups[non.nas, t] <- grps.non.nas
  }

  #also store groups as lists of lists
  groups_list <- list()
  for(t in 1:n_times){

    #create a list to hold the groups at that timestep
    groups_list[[t]] <- list()
    if(sum(!is.na(groups[,t]))==0){
      next
    }

    #
    max_group_id <- max(groups[,t],na.rm=T)
    for(i in seq(1,max_group_id)){
      groups_list[[t]][[i]] <- which(groups[,t]==i)
    }
  }

  #Identifying changes in group membership in consecutive time steps
  if(verbose){print('Identifying changes in group membership')}
  event_times <- c()
  for(d in 1:(length(breaks)-1)){
    t_day <- breaks[d]:(breaks[d+1]-1)
    for(t in t_day[1:(length(t_day)-1)]){

      if(length(groups_list[[t]])==0 | length(groups_list[[t+1]])==0){
        next
      }
      ## If any groups arent present in next step, store this time as a time in which a change event occurred
      if(!all(groups_list[[t]] %in% groups_list[[t+1]])){
        event_times <- c(event_times, t)
      }
    }
  }

  #for each time when the subgrouping patterns changed...
  all_events_info <- list()
  event_idx <- 1
  for(tidx in 1:length(event_times)){
    #print(tidx)
    t <- event_times[tidx]

    #get groups before and after
    groups_curr <- groups_list[[t]]
    groups_next <- groups_list[[t+1]]

    #remove any individuals who are not present in one or the other timestep from both timesteps
    inds_present_curr <- unlist(groups_curr)
    inds_present_next <- unlist(groups_next)
    inds_to_remove <- unique(c(setdiff(inds_present_curr, inds_present_next), setdiff(inds_present_next, inds_present_curr)))
    if(length(inds_to_remove)>0){
      for(g in 1:length(groups_curr)){
        for(i in seq(length(groups_curr[[g]]), 1, -1)){
          if(groups_curr[[g]][i] %in% inds_to_remove){
            groups_curr[[g]] <- groups_curr[[g]][-i]
          }
        }
      }
      for(g in 1:length(groups_next)){
        for(i in seq(length(groups_next[[g]]), 1, -1)){
          if(groups_next[[g]][i] %in% inds_to_remove){
            groups_next[[g]] <- groups_next[[g]][-i]
          }
        }
      }
    }

    #remove any now-empty groups from groups_curr and groups_next
    groups_curr[!unlist(lapply(groups_curr, length))] <- NULL
    groups_next[!unlist(lapply(groups_next, length))] <- NULL

    #if there are no more groups after removing individuals, go to the next time index
    n_groups_curr <- length(groups_curr)
    n_groups_next <- length(groups_next)
    if(n_groups_curr == 0){next}
    if(n_groups_next == 0){next}

    #find sets of connected groups across the current and future timestep
    #construct a directed network connection_net[i,j] where the rows represent groups in the
    #current timestep and the cols represent groups in the next timestep. Define
    #connection_net[i,j] = 1 if group i from the current timestep and group j from the next
    #timestep share a member, and 0 otherwise.

    connection_net <- matrix(F, nrow = n_groups_curr, ncol = n_groups_next)
    for(i in 1:n_groups_curr){
      for(j in 1:n_groups_next){
        if(length(intersect(groups_curr[[i]], groups_next[[j]])) > 0){
          connection_net[i,j] <- T
        }
      }
    }

    #get connected components of the bipartite network by iteratively selecting
    #a "seed" group at time t, pulling it's connections at time t+1,
    #saving that as a component, then removing it from remaining groups

    ### initialize full set of remaining groups at each time point
    ## we will go through and move groups from here to component_groups_A/B
    remaining_groups_A <- 1:nrow(connection_net)
    remaining_groups_B <- 1:ncol(connection_net)
    #component id
    idx <- 1
    component_groups_A <- component_groups_B <- list()
    ## iterate as long as there are remaining groups at the first time point to process
    while(length(remaining_groups_A) > 0){

      #start with seed group from first time point (A)
      seed_A <- remaining_groups_A[1]

      #initiatlize the object tracking which groups are connected
      grps_A <- c(seed_A)
      grps_B <- c()

      #make a duplicate to track whether it changes after the upcoming while loop
      grps_A_orig <- c()
      grps_B_orig <- c()

      ##while loop terminates once the operations has an effect on grps_A and grps_B
      while(!setequal(grps_A_orig, grps_A) | !setequal(grps_B_orig,grps_B)){

        #set to same so that while loop stops if while loop has no effect
        grps_A_orig <- grps_A
        grps_B_orig <- grps_B

        #for each identified group in A, add unique groups in B that are connected
        #to that group in A
        for(i in 1:length(grps_A)){
          grps_B <- union(grps_B, which(connection_net[grps_A[i],]))
        }

        #then do the same but in reverse (B->A) to catch rare cases where two groups in A
        #might both be connected to the same group in B
        for(i in 1:length(grps_B)){
          grps_A <- union(grps_A, which(connection_net[,grps_B[i]]))
        }
      }

      #Save A and B components, linked by shared component id
      component_groups_A[[idx]] <- c(grps_A)
      component_groups_B[[idx]] <- c(grps_B)

      #iterate component id
      idx <- idx + 1

      #remove processed groups from remaining groups and continue with remaining groups
      remaining_groups_A <- setdiff(remaining_groups_A, grps_A)
      remaining_groups_B <- setdiff(remaining_groups_B, grps_B)

    }

    #remove instances where group did not change (one to one connections between groups)
    i <- 1
    while(i <= length(component_groups_A)){
      if(length(component_groups_A[[i]])==1 & length(component_groups_B[[i]])==1){
        component_groups_A[[i]] <- component_groups_B[[i]] <- NULL
        i <- i - 1
      }
      i <- i + 1
    }

    #for each of the component events
    if(length(component_groups_A)>0){
      for(i in 1:length(component_groups_A)){
        #classify into event types and store subgroup memberships in a data frame
        component_groups_before <- component_groups_A[[i]]
        component_groups_after <- component_groups_B[[i]]
        n_groups_before <- length(component_groups_before)
        n_groups_after <- length(component_groups_after)
        #fission = 1 group becomes multiple
        if(n_groups_before == 1){
          event_type <- 'fission'
        } else{
          #fusion = multiple groups become 1
          if(n_groups_after == 1){
            event_type <- 'fusion'
          } else{
            #shuffle = multiple groups become multiple groups
            event_type <- 'shuffle'
          }
        }

        ## save information on the subgroups that change
        groups_before <- list()
        groups_after <- list()
        for(g in 1:n_groups_before){
          groups_before[[g]] <- list(groups_curr[[component_groups_before[[g]]]])
        }
        for(g in 1:n_groups_after){
          groups_after[[g]] <- list(groups_next[[component_groups_after[[g]]]])
        }

        #store data
        n_groups_before <- length(groups_before)
        n_groups_after <- length(groups_after)
        event_info <- list(t = t, groups_before = groups_before, groups_after = groups_after, event_type = event_type, n_groups_before = n_groups_before, n_groups_after = n_groups_after)
        all_events_info[[event_idx]] <- event_info
        event_idx <- event_idx + 1
      }
    }
  }

  #get maximum number of subgroups during fissions or fusions, used subsequently for storing data
  n_subgroups <- rep(NA, length(all_events_info))
  for(i in 1:length(all_events_info)){
    n_subgroups[i] <- max(all_events_info[[i]]$n_groups_before, all_events_info[[i]]$n_groups_after)
  }
  max_n_subgroups <- max(n_subgroups, na.rm=T)


  ##store data in dataframe
  events_detected <- data.frame()
  for(i in 1:length(all_events_info)){
    event_type <- all_events_info[[i]]$event_type
    n_groups_before <- all_events_info[[i]]$n_groups_before
    n_groups_after <- all_events_info[[i]]$n_groups_after
    row <- data.frame(event_idx = i,
                      tidx = all_events_info[[i]]$t,
                      event_type = all_events_info[[i]]$event_type,
                      n_groups_before = n_groups_before,
                      n_groups_after = n_groups_after)

    #initialize columns for storing data
    row$big_group_idxs <- list(c(NA))
    row$big_group <- list(c(NA))
    ## create columns based on maximum number subgroups during events in data
    for(j in 1:max_n_subgroups){
      row[paste('group',LETTERS[j],'idxs',sep='_')] <- list(c(NA))
    }
    for(j in 1:max_n_subgroups){
      row[paste('group',LETTERS[j],sep='_')] <- list(c(NA))
    }
    for(j in 1:max_n_subgroups){
      row[paste('n',LETTERS[j], sep = '_')] <- NA
    }
    row$n_big_group <- NA

    #if event is a fission, the large group is from the first time step
    if(event_type == 'fission'){
      row$big_group_idxs <- all_events_info[[i]]$groups_before[[1]]
      row$big_group <- list(names[unlist(all_events_info[[i]]$groups_before[[1]])])
      for(j in 1:n_groups_after){
        row[,paste('group',LETTERS[j],'idxs',sep='_')] <- all_events_info[[i]]$groups_after[j]
        row[,paste('group',LETTERS[j],sep='_')] <- list(list(c(names[unlist(all_events_info[[i]]$groups_after[j])])))
        row[,paste('n',LETTERS[j],sep='_')] <- length(all_events_info[[i]]$groups_after[j][[1]][[1]])
      }
      row$n_big_group <- length(row$big_group_idxs[[1]])
    }

    #if event is a fusion, the large group is from the second time step
    if(event_type == 'fusion'){
      row$big_group_idxs <- all_events_info[[i]]$groups_after[[1]]
      row$big_group <- list(names[unlist(all_events_info[[i]]$groups_after[[1]])])
      for(j in 1:n_groups_before){
        row[,paste('group',LETTERS[j],'idxs',sep='_')] <- all_events_info[[i]]$groups_before[j]
        row[,paste('group',LETTERS[j],sep='_')] <- list(list(c(names[unlist(all_events_info[[i]]$groups_before[j])])))
        row[,paste('n',LETTERS[j],sep='_')] <- length(all_events_info[[i]]$groups_before[j][[1]][[1]])
      }
      row$n_big_group <- length(row$big_group_idxs[[1]])
    }



    events_detected <- rbind(events_detected, row)
  }

  #return things
  out <- list(events_detected = events_detected, all_events_info = all_events_info, groups_list = groups_list, groups = groups, together = together, R_inner = R_inner, R_outer = R_outer)
  return(out)

}
