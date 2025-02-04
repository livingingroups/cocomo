#'Get whether two individuals are together, using a "sticky" definition
#'
#'

get_together_sticky <- function(together_inner, together_outer){
  #build vector to hold whether you are together by a sticky definition
  together_ij <- rep(NA, length(together_inner))

  #if you are together within the inner threshold, you are together no matter what
  inner_idxs <- which(together_inner)
  together_ij[inner_idxs] <- TRUE

  #if together_outer is F, you are not together no matter what
  apart_idxs <- which(!together_outer)
  together_ij[apart_idxs] <- FALSE

  #make a vector to hold the information on whether your status is ambiguous or not]
  #if you are between the thresholds, your status is ambiguous
  status_ambig <- rep(T, length(together_inner))
  status_ambig[inner_idxs] <- F
  status_ambig[apart_idxs] <- F

  #get indexes to the ambiguous times
  ambig_idxs <- which(status_ambig)

  #for an ambiguous time, look forward in time and find your latest
  #non-ambiguous status before and your earliest non-ambiguous status after
  if(length(ambig_idxs)>0){
    for(i in 1:length(ambig_idxs)){
      ambig_idx <- ambig_idxs[i]
      forward <- 0
      found_fwd <- F
      while(!found_fwd){

        #if you reach the end of the vector, break and you did not find an unambiguous time
        if(ambig_idx + forward > length(together_ij)){
          break
        }

        #if you reach a point where the status is not ambiguous, set found to T
        if(!status_ambig[ambig_idx + forward]){
          found_fwd <- T

        #otherwise keep moving forward
        } else{
          forward <- forward + 1
        }
      }

      #same thing in reverse
      backward <- 0
      found_bwd <- F
      while(!found_bwd){

        #if you reach the beginning of the vector, break and you did not find an unambiguous time
        if(ambig_idx - backward < 1){
          break
        }

        #if you reach a point where the status is not ambiguous, set found to T
        if(!status_ambig[ambig_idx - backward]){
          found_bwd <- T

          #otherwise keep moving forward
        } else{
          backward <- backward + 1
        }
      }

      #if unambiguous times were found both before and after
      if(found_fwd & found_bwd){
        #if both are outside the outer threshold, then set together_ij at that ambiguous time to FALSE
        #otherwise, set it to TRUE because they are together due to the "sticky" rule
        if(!together_outer[ambig_idx - backward] & !together_outer[ambig_idx + forward]){
          together_ij[ambig_idx] <- FALSE
        } else{
          together_ij[ambig_idx] <- TRUE
        }
      }

      #if an unambiguous index was found only on one end, and they were together within the inner htreshold at this point, this also makes them "together" due to the sticky rule
      if(found_fwd){
        if(together_inner[ambig_idx + forward]){
          together_ij[ambig_idx] <- TRUE
        }
      }
      if(found_bwd){
        if(together_inner[ambig_idx - backward]){
          together_ij[ambig_idx] <- TRUE
        }
      }

      #otherwise, together_ij remains ambiguous (NA)
    }
  }

  #any NAs are not considered together
  together_ij[which(is.na(together_ij))] <- FALSE

  return(together_ij)
}


