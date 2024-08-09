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
#' @param R_inner inner distance threshold to identify periods of connectedness (numeric)
#' @param R_outer outer distance threshold to identify periods of connectedness (numeric)
#' @param xs UTM eastings matrix (`n_ninds` x `n_times` matrix where xs[i,t] gives the easting of individual i at time step t)
#' @param ys UTM northing matrix (`n_ninds` x `n_times` matrix where ys[i,t] gives the northing of individual i at time step t)
#' @param ts vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
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
#'`events_detected$group_A_idxs`, $group_B_idxs, $group_C_idxs, etc.: individual idxs of subgroup members
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
identify_splits_and_merges <- function(R_inner, R_outer, xs, ys, ts, breaks = c(1, length(ts)+1), names = NULL, break_by_day = F, verbose = T){

  #----Identify subgroups at each point
  if(verbose){print('Identifying subgroups at each point using sticky DBSCAN')}
  #number of inds and times
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #day start indexes
  if(break_by_day){
    days <- lubridate::date(ts)
    day_start_idxs <- c(1, which(diff(days)==1)+1)
    day_start_idxs <- c(day_start_idxs, length(ts)+1)
    if(!exists('breaks')){
      breaks <- day_start_idxs
    }
  }

  #Get dyadic distances for each pair, then use double threhsold method to determine if htey are otgether at any moment
  dyad_dists <- together <- array(NA, dim = c(n_inds, n_inds, n_times))
  for(i in 1:(n_inds-1)){
    for(j in (i+1):n_inds){

      #dyadic distance
      dx <- xs[i,] - xs[j,]
      dy <- ys[i,] - ys[j,]
      dyad_dists[i,j,] <- sqrt(dx^2 + dy^2)

      #together or not
      #loop over days. for each day...
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

        if(sum(together_inner,na.rm=T)==0){
          together[i,j,t_day] <- together[j,i,t_day] <- together_ij
          next
        }

        #go backwards from crossing points into inner radius to find the 'starts' when crossed the outer radius
        inner_starts <- which(diff(together_inner)==1)+1
        if(length(inner_starts)==0){
          together[i,j,t_day] <- together[j,i,t_day] <- together_ij
          next
        }
        for(k in 1:length(inner_starts)){
          crossing <- inner_starts[k]
          curr_time <- crossing
          for(curr_time in seq(crossing,1,-1)){
            if(is.na(together_outer[curr_time])){
              start <- curr_time + 1
              break
            }
            if(curr_time == 1){
              start <- curr_time
              break
            }
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
            if(is.na(together_outer[curr_time])){
              end <- curr_time - 1
              break
            }
            if(curr_time == length(together_outer)){
              end <- curr_time
              break
            }
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

  #Identify groups from together matrices
  groups <- matrix(NA, nrow = n_inds, ncol = n_times)
  for(t in 1:n_times){
    non.nas <- which(colSums(!is.na(together[,,t]))>0)
    if(length(non.nas)<=1){
      next
    }
    non.nas.together <- together[non.nas,non.nas,t]
    diag(non.nas.together) <- 1
    grps.non.nas <- dbscan::dbscan(x = as.dist(1 - non.nas.together), eps = .1,minPts=1)$cluster
    groups[non.nas, t] <- grps.non.nas
  }

  #store groups as lists of lists
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
      groups_curr <- groups_list[[t]]
      groups_next <- groups_list[[t+1]]
      if(sum(!is.na(groups_curr))==0 | sum(!is.na(groups_next))==0){
        next
      }
      n_groups_curr <- length(groups_curr)
      n_groups_next <- length(groups_next)
      matches <- rep(F, n_groups_curr)
      for(i in 1:n_groups_curr){
        group_curr <- groups_curr[[i]]
        matched <- F
        for(j in 1:n_groups_next){
          group_next <- groups_next[[j]]
          if(setequal(group_curr,group_next)){
            matched <- T
          }
        }
        matches[i] <- matched
      }
      if(sum(matches)<n_groups_curr){
        event_times <- c(event_times, t)
      }
    }
  }

  #for each time when the subgrouping patterns changed...
  all_events_info <- list()
  event_idx <- 1
  for(tidx in 1:length(event_times)){
    print(tidx)
    t <- event_times[tidx]

    #get groups before and after
    groups_curr <- groups_list[[t]]
    groups_next <- groups_list[[t+1]]

    #remove any individuals who are not present in one or the other timestep from both timesteps
    inds_present_curr <- unlist(groups_curr)
    inds_present_next <- unlist(groups_next)
    inds_to_remove <- setdiff(inds_present_curr, inds_present_next)
    if(length(inds_to_remove)>0){
      for(g in 1:length(groups_curr)){
        for(i in 1:length(groups_curr[[g]])){
          if(groups_curr[[g]][i] %in% inds_to_remove){
            groups_curr[[g]] <- groups_curr[[g]][-i]
          }
        }
      }
      for(g in 1:length(groups_next)){
        for(i in 1:length(groups_next[[g]])){
          if(groups_next[[g]][i] %in% inds_to_remove){
            groups_next[[g]] <- groups_next[[g]][-i]
          }
        }
      }
    }

    #remove any now-empty groups from groups_curr and groups_next
    g <- 1
    gmax <- length(groups_curr)
    while(g <= gmax){
      if(length(groups_curr[[g]])==0){
        groups_curr[[g]] <- NULL
        g <- g - 1
        gmax <- gmax - 1
      }
      g <- g + 1
    }
    g <- 1
    gmax <- length(groups_next)
    while(g <= gmax){
      if(length(groups_next[[g]])==0){
        groups_next[[g]] <- NULL
        g <- g - 1
        gmax <- gmax - 1
      }
      g <- g + 1
    }

    #find sets of connected groups across the current and future timestep
    #construct a directed network connection_net[i,j] where the rows represent groups in the
    #current timestep and the cols represent groups in the next timestep. Define
    #connection_net[i,j] = 1 if group i from the current timestep and group j from the next
    #timestep share a member, and 0 otherwise.
    n_groups_curr <- length(groups_curr)
    n_groups_next <- length(groups_next)
    connection_net <- matrix(F, nrow = n_groups_curr, ncol = n_groups_next)
    for(i in 1:n_groups_curr){
      for(j in 1:n_groups_next){
        if(length(intersect(groups_curr[[i]], groups_next[[j]])) > 0){
          connection_net[i,j] <- T
        }
      }
    }

    #get connected components of the bipartite network
    remaining_groups_A <- 1:nrow(connection_net)
    remaining_groups_B <- 1:ncol(connection_net)
    idx <- 1
    component_groups_A <- component_groups_B <- list()
    while(length(remaining_groups_A) > 0){
      seed_A <- remaining_groups_A[1]

      grps_A <- c(seed_A)
      grps_B <- c()
      grps_A_orig <- grps_A
      grps_B_orig <- grps_B

      while(setequal(grps_A_orig, grps_A) & setequal(grps_B_orig,grps_B)){
        grps_A_orig <- grps_A
        grps_B_orig <- grps_B
        for(i in 1:length(grps_A)){
          grps_B <- union(grps_B, which(connection_net[grps_A[i],]))
        }
        for(i in 1:length(grps_B)){
          grps_A <- union(grps_A, which(connection_net[,grps_B[i]]))
        }
      }

      component_groups_A[[idx]] <- c(grps_A)
      component_groups_B[[idx]] <- c(grps_B)
      idx <- idx + 1
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
        if(n_groups_before == 1){
          event_type <- 'fission'
        } else{
          if(n_groups_after == 1){
            event_type <- 'fusion'
            } else{
              event_type <- 'shuffle'
          }
        }

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

  #get maximum number of subgroups during fissions or fusions
  n_subgroups <- rep(NA, length(all_events_info))
  for(i in 1:length(all_events_info)){
    n_subgroups[i] <- max(all_events_info[[i]]$n_groups_before, all_events_info[[i]]$n_groups_after)
  }
  max_n_subgroups <- max(n_subgroups, na.rm=T)

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
    row$big_group_idxs <- list(c(NA))
    row$big_group <- list(c(NA))
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
  out <- list(events_detected = events_detected, all_events_info = all_events_info, groups_list = groups_list, together = together, R_inner = R_inner, R_outer = R_outer)
  return(out)

}
