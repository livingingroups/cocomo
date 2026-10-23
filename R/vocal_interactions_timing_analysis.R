#' Vocal interactions timing analysis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Carries out the analysis described in Demartsev et al. 2024 (doi: 10.1098/rstb.2023.0188)
#' It works as follows:
#' Make a table of all calls within a given call category (set of calls), identify the one who gave the call as the 'caller'
#' For each call, identify all other individuals ('responders') who were present (calls labeled) on that day
#' Also identify the distance between the caller and responder at the time when the call was given by the caller (if desired)
#' Save this table as callresp
#' Note: This table DOES include self-responses (which can be useful for looking at self-response patterns, or can be filtered out later)

#' Then for each of row in this table, look at the call sequence of the 'responder' over time, lined up so that t = 0 = time of the 'caller' call
#' Run a kernel over this time series to smooth it with a bandwidth bw = .1 sec (or as specified by parameters bw)
#' Save the output to a matrix as callresp_seqs, such that the indices in the table match the indices in the matrix

#' In subsequent code, these sequences can be further explored and plotted.
#' For instance, we can get the mean across all (or a subset) of rows of the matrix for different conditions:
#' Different distance bins (aggregate only data from caller-responder pairs within a range of distances from one another)
#' Different age/sex/dominance classes (aggregate only data from caller-responder pairs where the caller was a certain status, and the responder was a certain status)
#' Investigate self-interactions
#' Etc.
#' callresp_table: [data frame] of call-response pairs w/ relevant columns
#'  caller: [str] id of the calling individual whose call triggers the sequence (lined up at t = 0)
#'  responder: [str] id of the responding individual whose subsequent calls are being monitored
#'  t0: [numeric] time of the caller call in POSIXct form
#'  tf: [numeric] time of the caller call in POSIXct form
#'  distance: [numeric] distance between caller and responder at the time of the original call
#'  a few other columns used in processing
#' callresp.seqs: [matrix] of call-response sequences,
#'   where each row represents the sequence associated with each row in the callresp data frame
#'   and each column represents a point in time
#' tseq: [vector] giving the time point associated with the columns of callresp_seqs (in seconds)
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param calls data frame of calls, with columns call_type (call type, character string), t0 (start time in POSIXct UTC), tf (end time in POSIXct UTC), ind_idx (individual index, matching to rows in xs and ys matrices if present), and optionally time_idx (index of closest time point that matches to timestamps vector)
#' @param caller_calltypes string giving a regular expression for what initiator (focal) call types to include
#' @param responder_calltypes string giving a regular expression for what repsonder call types to include
#' @param bw bandwidth of the kernel to use in the analysis
#' @param step time resolution (in sec)
#' @param max_lag max time lag to consider (in sec)
#' @param trigger_on string specifying if you should trigger on the beginning of calls ('begin') or end of calls ('end')
#' @param xs `N x n_times` matrix of eastings for all individuals
#' @param ys `N x n_times` matrix of northings for all individuals
#' @param timestamps vector of POSIXct (UTC) timestamps corresponding to columns in xs and ys
#' @param audioOn `N x n_times` matrix of whether each individual has audio data at each second (T or F)
#'
#' @returns Returns a list containing:
#' `callresp`: the table of callers, responders, call times, distances, etc.
#' `callresp_seqs`: vocal sequence of the responder relative to the time of the caller's call
#' `out$tseq`: time sequence used (maps to the columns of callresp_seqs)
#' `out$callresp_means_all`: mean across the columns of callresp_seqs - the average response to a call (across all data, including self responses)
#' `out$callresp_means_other`: mean across the columns of callresp_seqs - the average response to a call including only non-self responses, i.e. responses to others individuals)
#' `out$callresp_means_self`: mean across the columnns of callresp_seqs - the average response to a call including only self-responses), i.e. the typical time between calls in a single individual's vocal sequence
#' @export
get_vocal_interactions_timing <- function(calls, caller_calltype, responder_calltype, bw = 0.1, step = 0.02, max_lag = 3, trigger_on = 'begin', xs = NULL, ys = NULL, timestamps = NULL, audioOn = NULL){

  #-------LIBRARY-------
  library(lubridate)
  warning('This function is new and has not yet been properly tested - use with caution')

  timestamp()
  print('preprocessing')

  #make sure trigger.on is specified as either triggering on beginning or end of call, otherwise error
  if(!(trigger_on %in% c('begin','end'))){
    stop('trigger.on parameter should be either begin or end')
  }

  #convert times to POSIXlt
  if(!(is.POSIXct(calls$t0) & is.POSIXct(calls$tf))){
    stop('calls$t0 and calls$tf must be in POSIXct format')
  }

  #remove double-marked calls (same start and end time and same individual calling)
  dups <- which(duplicated(cbind(calls$ind,as.numeric(calls$t0),as.numeric(calls$tf))))
  if(length(dups)>0){
    calls <- calls[-dups,]
  }

  #indicate which calls are in the caller call type list, and which are in repsonder call type list
  calls$isCallerCallType <- F
  calls$isResponderCallType <- F
  calls$isCallerCallType[grepl(caller_calltype, calls$call_type)] <- T
  calls$isResponderCallType[grepl(responder_calltype, calls$call_type)] <- T
  print('For the caller, using all calls fitting the regular expression:')
  print(caller_calltype)
  print('For the responder, using all calls fitting the regular expression:')
  print(responder_calltype)

  if(exists('audioOn')){
    n_times <- ncol(audioOn)
  }

  #get number of individuals, assume that all individuals are numbered from 1 to n_inds
  n_inds <- max(calls$ind_idx)

  #---------------------------MAIN-------------------------------
  #time sequence bins - a series of time lags relative to the caller's call (at t=0), ranging from - max.lag to max.lag in steps of step
  tseq <- seq(-max_lag, max_lag, step)

  #create an expanded data frame (callresp) that contains rows for each other 'responder' individual (not the caller)
  #so for each call by a given individual, there will be a row for every other individual in the group
  timestamp()
  print('generating call-response table')
  callresp <- data.frame()
  inds_list <- seq(1, n_inds)

  #for each row in the call response table in the given groupyear
  for(i in 1:nrow(calls)){

    row <- calls[i,]

    t_idx <- row$time_idx

    #if time window falls outside of overall recording period bounds, skip
    if(exists('audioOn') & !is.null(t_idx)){
      if(is.na(t_idx) || (t_idx - max_lag) < 1 || (t_idx + max_lag) > n_times){
        next
      }
    }

    #if the caller is of the type we are using
    if(row$isCallerCallType == T){

      #find which individuals were possible responders (they were not missing audio data around that call)
      if(!is.null(t_idx) & exists('audioOn')){
        missingAudio <- rowSums(!audioOn[,(t_idx - max_lag):(t_idx + max_lag)])
        inds <- inds_list[which(missingAudio == 0)]
      } else{
        inds <- inds_list
      }

      #create rows for each possible (non-missing) responder at that time
      new_rows <- row[rep(1,length(inds)),]
      new_rows$responder <- inds
      new_rows$caller <- row$ind_idx

      #add to the bottom of the callresp data frame we are building up
      callresp <- rbind(callresp, new_rows)
    }
  }

  #now we have a callresp data frame with callers and possible repsonders for each call of the correct type given by the correct status class
  #now we need to add some other info to this data frame

  #get distance between caller and responder at time of the call (t0)
  #loop over group years
  timestamp()
  print('computing distances between individuals across all recording periods')

  #if including distance, get distance between pairs
  if(!is.null(xs) & !is.null(ys)){

    callresp$distance <- NA
    #compute distance between caller and responder at the time of the call
    xc <- xs[cbind(callresp$caller, callresp$time_idx)]
    yc <- ys[cbind(callresp$caller, callresp$time_idx)]
    xr <- xs[cbind(callresp$responder, callresp$time_idx)]
    yr <- ys[cbind(callresp$responder, callresp$time_idx)]
    dists <- sqrt((xc - xr)^2 + (yc - yr)^2)

    #store distances in callresp table
    callresp$distance <- dists

  }

  #now that we have the matrix of all callers and responders, call times, and distances between them, we need to generate vectors of the vocal sequences of the responders (callresp.seqs)

  #create a matrix to hold call/response sequences
  #each row will represent a sequence of a particular responder associated with the call of a caller
  #the rows match up with the callresp data frame
  callresp_seqs <- matrix(data=NA,nrow=nrow(callresp),ncol = length(tseq))

  #loop through and get individual call patterns starting at tf
  timestamp()
  print('gathering vocal sequences across all recording periods')
  for(i in 1:nrow(callresp_seqs)){

    #print progress
    if(i %% 10000 == 0){
      print(paste(i, '/', nrow(callresp_seqs)))
    }

    #get the time point that we will consider to be 0 (the time of caller's call, either beginning or end of it)
    if(trigger_on == 'begin'){
      zero_time <- callresp$t0[i]
    } else{
      zero_time <- callresp$tf[i]
    }

    #get caller and responder id
    responder <- callresp$responder[i]
    caller <- callresp$caller[i]

    #get call times of the responder (only of the correct call type, and removing the current call)
    current_call_idx <- which(calls$ind_idx == caller & calls$t0 == callresp$t0[i] & calls$tf == callresp$tf[i])

    #some error checking - if can't find the call throw an error
    if(length(current_call_idx)==0){
      stop('could not find current call in the table')
    }
    #this should also not happen because we removed duplicates before, but just as a check
    if(length(current_call_idx) > 1){
      warning('found two calls with same label, start, and stop time')
    }

    #remove the current call from the table of calls we are using
    calls_use_removeocurrent <- calls[-current_call_idx,]

    #get call times of the current responder (foc), if they are of the correct type
    #this line might be slightly confusing, but basically we are now looking for the rows where the calls were given by the specified responder (caller == responder)
    #in some cases, the caller and responder might be the same individual - we keep these in the table to be able to look at self-replies later if we want to
    call_times_foc <- calls_use_removeocurrent$t0[which(calls_use_removeocurrent$isResponderCallType & calls_use_removeocurrent$ind_idx == responder)]

    #get time differences between the responder call times and the zero time
    dt_foc <- call_times_foc - zero_time
    dt_foc <- dt_foc[which(abs(dt_foc) < max_lag)] #get rid of way early or way late calls to reduce compute time

    #generate a vector to hold the call sequence of the responder
    #it will be 0 where there are no calls, with little "spikes" where calls are (we use gaussian kernels with width bw)
    call_seq <- rep(0, length(tseq))

    #add "spikes" for each responder call
    if(length(dt_foc)>0){
      for(j in 1:length(dt_foc)){
        call_seq <- call_seq + dnorm(tseq, mean = dt_foc[j], sd = bw)
      }
    }

    #add the current sequence to the big matrix of all sequences
    callresp_seqs[i,] <- call_seq
  }

  timestamp()

  #get means
  callresp_means <- colMeans(callresp_seqs)
  idxs_other <- which(callresp$caller != callresp$responder)
  idxs_self <- which(callresp$caller == callresp$responder)
  callresp_means_other <- colMeans(callresp_seqs[idxs_other,])
  callresp_means_self <- colMeans(callresp_seqs[idxs_self,])

  print('done')

  out <- list()
  out$callresp_table <- callresp
  out$callresp_seqs <- callresp_seqs
  out$tseq <- tseq
  out$callresp_means_all <- callresp_means
  out$callresp_means_other <- callresp_means_other
  out$callresp_means_self <- callresp_means_self

  return(out)

}
