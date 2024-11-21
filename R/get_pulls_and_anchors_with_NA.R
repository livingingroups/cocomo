#' Wrapper for `get_pulls_and_anchors` function, handling datasets with NAs
#'
#' Create a list of time blocks using NAs in the dataset as seperation points, then run the `get_pulls_and_anchors` function on each of these blocks separately.
#'
#' Time blocks are created by splitting the dataset at the indexes of NAs. If the length of the NA-chain is below "NA_tolerance", then the script will ignore these NAs and not split at that position.
#' Instead, these NAs will be filled in with the value of the next (non-NA) position in the vector. TODO: check this - this seems to be what it's doing, but is this correct and is this the desired behavior?
#' This results in the datapoints staying connected/in a single time block for small breaks. If the NA-chain in the dataset is longer than "NA_tolerance",
#' then the values before and after that chain are split into separate time blocks.
#' If "min_time" != NULL, then the script will remove all time blocks shorter than the specified value (usually time steps), resulting only in time blocks of relevant lengths.
#'
#' Note that if either `include_initial_fusion` or `include_final_fission` are true, this function will treat each time block separately. In other words,
#' a fission and/or fusion might be detected for each time block. TODO: I added this. Please check for correctness.
#'
#' If "Error in xa_blocks[[i]] : subscript out of bounds" appears, then the min_time set excludes all time blocks, leaving no events to analyse.
#' TODO: We might want to check this within the function and have it return NULL instead of throwing an error. This is what the main function does if there are no events.
#'
#'
#' @author Dario Walser
#' @author NOT YET CODE REVIEWED
#'
#' @param xa: x coordinates for individual a (vector of length `n_times`)
#' @param	xb: x coordinates for individual b (vector of length `n_times`)
#' @param ya: x coordinates for individual a (vector of length `n_times`)
#' @param	yb: x coordinates for individual b (vector of length `n_times`)
#' @param a: index of the first individual
#' @param	b: index of the second individual
#' @param NA_tolerance: How big of a gap in the vector (missing timesteps), created by NAs, is tolerated and therefore still combined into a single continuous dataset (numeric, positive integer)
#' @param min_time: the minimum length (timesteps) of a resulting time block that will still be used for analysis (numeric, positive integer)
#' @param verbose: If verbose = TRUE, then print out the size of each NA gap
#'
#'
#' For following parameters and more information, see documentation of `get_pulls_and_anchors` function:
#'
#' @param noise_thresh: see documentation of `get_pulls_and_anchors` function
#' @param plot_results: see documentation of `get_pulls_and_anchors` function
#' @param include_initial_fusion: see documentation of `get_pulls_and_anchors` function
#' @param include_final_fission: see documentation of `get_pulls_and_anchors` function
#'
#' @returns
#' Returns a data frame  containing dyadic interactions between a and b.
#' Contains columns: `t1`, `t2`, `t3`, `initiator`, `responder`, `type`, `disparity`, `strength`,`disparity_additive`, and `strength_additive`.
#' @export
#'
get_pulls_and_anchors_with_NA <- function(xa, xb, ya, yb, a, b, noise_thresh = 5, plot_results = F, include_initial_fusion = F, include_final_fission = F, NA_tolerance = 0, min_time = NULL, verbose = FALSE){

  if (anyNA(xa) | anyNA(xb)) { #check if there are any NAs in the vectors xa or xb. Assuming that the vector pairs xa and ya or xb and yb within possess NA in same positions.
    #If TRUE, start process of removing NAs and splitting into time blocks

    while(sum(!cumprod(is.na(xa)) & rev(!cumprod(is.na(rev(xa)))))!=length(xa) | sum(!cumprod(is.na(xb)) & rev(!cumprod(is.na(rev(xb)))))!=length(xb)) { #control if xa | xb still has leading or trailing NAs

      leading_NA <- sum(cumprod(is.na(xa))) #save number of leading NAs in dataset, which will be removed
      core_values <- !cumprod(is.na(xa)) & rev(!cumprod(is.na(rev(xa)))) #gain indexes of entries between leading and trailing NAs (core), looking at xa
      xa <- xa[core_values] #only keep core entries for each parameter/vector
      xb <- xb[core_values] #removing same indexes from all vectors to maintain matching entries (time steps)
      ya <- ya[core_values]
      yb <- yb[core_values]

      leading_NA <- leading_NA+sum(cumprod(is.na(xb))) #save number of leftover leading NAs, which will be removed
      core_values <- !cumprod(is.na(xb)) & rev(!cumprod(is.na(rev(xb)))) #repeat, looking at xb
      xa <- xa[core_values]
      xb <- xb[core_values]
      ya <- ya[core_values]
      yb <- yb[core_values]
    }

    miss_xab <- sort(unique(c(which(is.na(xa)), which(is.na(xb))))) #gain all indexes of NAs in xa and xb

    breakpts <- c(0, miss_xab, length(xa)+1) #create index points as markers for where to split the vector into time blocks (start and stop points)

    xa_blocks <- list() #empty lists to fill with coordinates, sorted in time blocks
    xb_blocks <- list()
    ya_blocks <- list()
    yb_blocks <- list()

    indexes <- NULL #set up vector to save starting indexes of each time block
    skip_count <- 1 #by default there is 1 skip, as the breakpoints are also NAs
    NA_gaps <- NULL #set up vector to keep track of how many NAs were skipped
    for (i in 1:(length(breakpts)-1)) { #last break-point will only act as STOP point and won't be necessary as START

      if(breakpts[i+1]-breakpts[i]==1) {
        skip_count <- skip_count+1 #if two NAs follow each other, then skip loop iteration and don't create list entry, count skips
      }
      else {
        if(i!=1 & skip_count<=NA_tolerance){ #check if NAs have already been tolerated/skipped the set max amount of times
          xa_blocks[[length(xa_blocks)]] <- append(xa_blocks[[length(xa_blocks)]], xa[(breakpts[i]+1):(breakpts[i+1]-1)]) #attach data to last entry in list to connect data
          xb_blocks[[length(xb_blocks)]] <- append(xb_blocks[[length(xb_blocks)]], xb[(breakpts[i]+1):(breakpts[i+1]-1)])
          ya_blocks[[length(ya_blocks)]] <- append(ya_blocks[[length(ya_blocks)]], ya[(breakpts[i]+1):(breakpts[i+1]-1)])
          yb_blocks[[length(yb_blocks)]] <- append(yb_blocks[[length(yb_blocks)]], yb[(breakpts[i]+1):(breakpts[i+1]-1)])
        }
        else {
          xa_blocks <- append(xa_blocks, list(xa[(breakpts[i]+1):(breakpts[i+1]-1)])) #start AFTER break-point 1 and stop BEFORE break-point 2
          xb_blocks <- append(xb_blocks, list(xb[(breakpts[i]+1):(breakpts[i+1]-1)]))
          ya_blocks <- append(ya_blocks, list(ya[(breakpts[i]+1):(breakpts[i+1]-1)]))
          yb_blocks <- append(yb_blocks, list(yb[(breakpts[i]+1):(breakpts[i+1]-1)]))

          tol_count <- 0 #TODO Check this, I don't think this code is doing anything
          indexes <- c(indexes, breakpts[i]+1) #save the starting index of time block
        }
        NA_gaps <- c(NA_gaps, skip_count) #append number of skipped NAs
        skip_count <- 1 #reset skip count after entry into list was made
      }
    }

    indexes <- indexes+leading_NA #correct the index for previously removed leading NAs and adjust to original original sequence again

    if(!is.null(min_time) & sum(lapply(xa_blocks, length)<min_time)!=0) {

      indexes <- indexes[-which(lapply(xa_blocks,length)<min_time)] #remove entries of indexes corresponding to time blocks that are too short

      xb_blocks <- xb_blocks[-which(lapply(xa_blocks,length)<min_time)] #remove time blocks that are shorter than specified
      ya_blocks <- ya_blocks[-which(lapply(xa_blocks,length)<min_time)]
      yb_blocks <- yb_blocks[-which(lapply(xa_blocks,length)<min_time)]
      xa_blocks <- xa_blocks[-which(lapply(xa_blocks,length)<min_time)]
    }
    events <- list()
    for (i in 1:length(xa_blocks)) {
      event <- get_pulls_and_anchors(xa=xa_blocks[[i]], xb=xb_blocks[[i]], ya=ya_blocks[[i]], yb=yb_blocks[[i]], a=a, b=b,
                                     noise_thresh = noise_thresh, plot_results = plot_results,
                                     include_initial_fusion = include_initial_fusion, include_final_fission = include_final_fission)
      event[,1:3] <- event[,1:3]+indexes[i]
      events <- append(events, list(event))
    }
    events <- do.call(rbind.data.frame, events)
    if(verbose==TRUE){
      cat("NA gaps:", NA_gaps)
    }
    return(events)


  }
  else{ #if there are no NAs, run the function normally

    events <- get_pulls_and_anchors(xa=xa, xb=xb, ya=ya, yb=yb, a=a, b=b,
                             noise_thresh = noise_thresh, plot_results = plot_results,
                             include_initial_fusion = include_initial_fusion, include_final_fission = include_final_fission)
    return(events)
  }
}


