#' Plot individual behavior and calls during a time period specified by the user.
#' Also include a summary of the behavior of the rest of the group.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param behavs matrix of dimensions `n_inds` x `n_times` where `behavs[i,t]` gives the behavior (numeric) of individual `i` at time step `t`
#' @param calls_array array of dimensions `n_inds` x `n_times` x `n_calltypes` where `calls_array[i,t,c]` indicates the number of calls of type `c` given by the individual `i` at time step `t`
#' @param behavs_key data frame of behavior types with columns `behav` (behavior string) and `int_value` (its integer value in the `behavs` matrix)
#' @param calls_to_plot vector of character strings indicating call types to plot (other call types will get lumped into an "other" category)
#' @param behavs_to_plot vector of character strings indicating behavior types of plot (other behavior types will get lumped into an "other" category)
#' @param focal_ind integer giving the index of the focal individual
#' @param t0 integer giving the time step to start at
#' @param tf integer giving the time step to stop at
#' @param nonfocal_calls_to_plot vector of character strings indicating which calls to plot for nonfocal individuals (must match call types in `calls_array` 3rd dimension names)
#' @param nonfocal_behavs_to_plot vector of character strings indicating which behaviors to plot for nonfocal individuals (must match behavior types in `behavs_key`)
#' @param smooth_window smoothing window for indicating presence of nonfocal calls (in time steps)
#' @export
#' @importFrom zoo rollsum
plot_behav_and_calls <-function(behavs,
                                calls_array,
                                behavs_key,
                                focal_ind,
                                t0,
                                tf,
                                nonfocal_calls_to_plot = NULL,
                                nonfocal_behavs_to_plot = NULL,
                                smooth_window = 31){
  #Get basic info for the plot
  n_calls <- dim(calls_array)[3]
  n_behavs <- nrow(behavs_key)
  n_inds <- nrow(behavs)
  n_times <- (tf - t0) + 1

  #get behavioral sequence for the focal and other individuals
  behav_seqs <- behavs[,t0:tf]
  behav_seq_focal <- behav_seqs[focal_ind,]

  #get call sequence for the focal and other individuals
  call_seqs <- calls_array[,t0:tf,]
  call_seq_focal <- call_seqs[focal_ind,,]

  #get colors
  cols_behavs <- c(terrain.colors(n_behavs))
  cols_calls <- c(rainbow(n_calls))

  #get number of plots
  n_plots <- 2
  if(!is.null(nonfocal_calls_to_plot)){
    n_plots <- n_plots + 1
  }
  if(!is.null(nonfocal_behavs_to_plot)){
    n_plots <- n_plots + 1
  }

  #PLOTS

  par(mfrow=c(n_plots,1),mar=c(0,0,0,0))

  #plot of the focal's behavior
  plot(NULL, xlim = c(t0,tf), ylim = c(1, n_behavs+1),yaxt='n',xaxt='n')
  for(b in 1:n_behavs){
    idxs <- which(behav_seq_focal == b)
    if(length(idxs)>0){
      arrows(x0 = idxs+t0-1, y0 = rep(b,length(idxs)), x1 = idxs+t0-1, y1 = rep(b+1,length(idxs)), col = cols_behavs[b], lwd = 0.5, length = 0)
    }
  }
  for(b in 1:n_behavs){
    text(x=t0+(tf-t0)/2, y = b+1/2, labels=behavs_key$behav[b])
  }
  text(x = t0, y = n_behavs+1, paste0('Duration = ',tf-t0+1,' time steps'), adj = c(0,1))

  #plot of the focal's calls, as well as whether the focal has data (gray if it doesn't)
  plot(NULL, xlim = c(t0,tf), ylim = c(1,n_calls+1),yaxt='n',xaxt='n')
  nodata_idxs <- which(is.na(call_seq_focal))
  if(length(nodata_idxs)>0){
    arrows(x0 = nodata_idxs+t0-1, y0 = rep(1,length(nodata_idxs)), x1 = nodata_idxs+t0-1, y1 = rep(n_calls+1, length(nodata_idxs)),col='gray',length=0)
  }

  calltypes <- dimnames(calls_array)[3][[1]]
  for(c in 1:n_calls){
    idxs <- which(call_seq_focal[,c]>0)
    if(length(idxs)>0){
      arrows(x0=idxs+t0-1,x1=idxs+t0-1,y0=rep(c,length(idxs)),y1=rep(c+1,length(idxs)),col=cols_calls[c],length=0)
    }
  }
  for(c in 1:n_calls){
    text(x=t0+(tf-t0)/2, y = c+1/2, labels=calltypes[c],col='black')
  }

  #Group behaviors
  if(!is.null(nonfocal_behavs_to_plot)){
    nonfoc_behavs <- behavs[-focal_ind,t0:tf]
    plot(NULL, xlim = c(t0,tf),ylim=c(0,1),xaxt='n',yaxt='n')
    for(b in 1:length(nonfocal_behavs_to_plot)){
      behav <- nonfocal_behavs_to_plot[b]
      behav_int <- behavs_key$int_value[which(behavs_key$behav == behav)]
      behav_freq <- colMeans(nonfoc_behavs==behav_int, na.rm=T)
      lines(t0:tf, behav_freq,col=cols_behavs[behav_int])
    }
    text(t0+(tf-t0)/2, 0.5, 'Group behavior %s')
  }

  #Group call rates
  if(!is.null(nonfocal_calls_to_plot)){
    plot(NULL, xlim = c(t0,tf),ylim=c(0,1),xaxt='n',yaxt='n')
    for(c in 1:length(nonfocal_calls_to_plot)){
      nonfoc_calls <- calls_array[-focal_ind,seq(t0-(smooth_window-1)/2,tf+(smooth_window-1)/2),c]
      call <- nonfocal_calls_to_plot[c]
      call_int <- which(calltypes==call)
      nonfoc_calls_smoothed <- matrix(NA, nrow=nrow(nonfoc_calls), ncol = ncol(nonfoc_calls)-(smooth_window-1))
      for(i in 1:nrow(nonfoc_calls)){
        nonfoc_calls_smoothed[i,] <- zoo::rollsum(x=nonfoc_calls[i,],k=smooth_window,align='center')
      }
      call_freq <- colMeans(nonfoc_calls_smoothed[,]>0, na.rm=T)
      lines(t0:tf, call_freq,col=cols_calls[call_int])
    }
    text(t0+(tf-t0)/2, 0.5, 'Group call %s')
  }

}
