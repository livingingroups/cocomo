#' Generate images for movement and calls visualization
#'
#' Generate a bunch of images (PNGs) showing trajectory data over a given time period, optionally on top of a Google Earth map,
#' and optionally with calls plotted on top.
#' These are saved as 1.png, 2.png, 3.png etc. to a folder, and you can then use the following ffmpeg command (or similar)
#' to merge them into a video:
#' ffmpeg -framerate 2 -i %d.png -r 8 -c:v libx264 -pix_fmt yuvj420p output_file_name.mp4
#' Data must be entered either as `xs` and `ys` matrices (UTM) if `plot_on_map = F` or as `lats` and `lons` matrices (latitude / longitude) if `plot_on_map = T`
#'
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs UTM eastings matrix (`n_inds` x `n_times` matrix where xs[i,t] gives the easting of individual i at time step t)
#' @param ys UTM northings matrix (`n_inds` x `n_times` matrix where ys[i,t] gives the northing of individual i at time step t)
#' @param timestamps vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
#' @param calls data frame where first column (`'ind_idx'`) specifies the index of the individual that gave the call, second column (`'time_idx'`) specifies the time index at which the call was given, and third column (`'call_type'`) specifies the type of call (character string)
#' @param start_time time index at which to start the video
#' @param end_time time index at which to end the video
#' @param time_step time step to use (an image every time_step timesteps will be produced)
#' @param output_dir directory in which to store the folder of outputted images
#' @param tail_time number of previous time steps to plot as a "tail" which trails the point showing the current location
#' @param call_persist_time number of previous time steps to still show the calls (they will shrink linearly over time in size)
#' @param colors_inds vector of colors to use for each individual (length `n_inds`)
#' @param colors_calls vector of colors to use for each call type (alphabetical order by call_type)
#' @param pchs_inds vector of plotting symbols for individuals
#' @param pchs_calls vector of plotting symbols for calls
#' @param show_legend_inds whether to plot a legend showing the names of the individuals (T or F)
#' @param show_legend_calls whether to plot a legend showing the names of the call types (T or F)
#' @param legend_loc location of the legend, either `topleft','topright','bottomleft' or 'bottomright'`
#' @param show_time whether to show the timestamp or not (`T` or `F`)
#' @param show_scalebar whether to show a scalebar or not (`T` or `F`)
#' @param scalebar_loc location of the scale bar, either `topleft','topright','bottomleft' or 'bottomright'`
#' @param scalebar_size number of meters for the scalebar
#' @param scalebar_offset scalebar offset from the edge (fraction of entire width)
#' @param ind_names vector of names of the individuals
#' @param bg_color background color
#'
#' @export
generate_movement_and_calls_visualization <-function(xs = NULL, ys = NULL,
                                                     timestamps = NULL, calls = NULL,
                                                     start_time = NULL, end_time = NULL, time_step = 1,
                                                     output_dir = NULL,
                                                     tail_time = 10, call_persist_time = 5,
                                                     colors_inds = NULL, colors_calls = NULL,
                                                     pchs_inds = NULL, pchs_calls = NULL,
                                                     show_legend_inds = T, show_legend_calls = T, legend_loc = 'topright',
                                                     show_time = T,
                                                     show_scalebar = T, scalebar_size = 100, scalebar_loc = 'bottomleft', scalebar_offset = 20,
                                                     ind_names = NULL,
                                                     bg_color = 'black'
                                                     ){

  #---CHECKS---

  #check xs and ys exist
  if(is.null('xs') | is.null('ys')){
    stop('for map plotting, need to specify xs and ys')
  }

  if(is.null(timestamps)){
    stop('must specify timestamps')
  }

  if(is.null(start_time) | is.null(end_time)){
    stop('must specify start_time and end_time')
  }

  if(is.null(output_dir)){
    stop('must specify output directory')
  }

  #check dimensions
  if(nrow(xs) != nrow(ys) | ncol(xs) != ncol(ys)){
    stop('xs and ys must have the same dimensions')
  }
  if(length(timestamps) != ncol(xs)){
    stop('timestamps must have the same dimensions as the columns of xs and ys or lons and lats')
  }

  #---SETTING UP---

  #get number of individuals and time steps
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #get colors for individuals if not specified
  if(is.null(colors_inds)){
    colors_inds <- rainbow(n_inds)
  }

  #get text color
  if(bg_color == 'black'){
    text_color <- 'white'
  } else{
    text_color <- 'black'
  }

  #get symbols for individuals if not specified
  if(is.null(pchs_inds)){
    pchs_inds <- rep(19, n_inds)
  }

  #get call types and specify colors for calls
  if(!is.null(calls)){

    #get unique call types and sort into alphabetical order
    call_types <- sort(unique(calls$call_type))
    n_call_types <- length(call_types)

    #specify colors for calls if not specified
    if(is.null(colors_calls)){
      colors_calls <- cm.colors(n_call_types)
    }

    #check that colors_calls is the right length
    if(length(colors_calls) != n_call_types){
      stop('colors_calls vector needs to be the length of the number of unique call types')
    }

    #get symbols for call types if not specified
    if(is.null(pchs_calls)){
      pchs_calls <- rep(8, n_call_types)
    }
  }



  #get minimum time
  if((start_time - tail_time) < 1){
    min_time <- 1
  } else{
    min_time <- start_time - tail_time
  }

  #get plot boundaries
  if((start_time - tail_time) > 1){
    curr_xs = xs[,(start_time - tail_time):end_time]
    curr_ys = ys[,(start_time - tail_time):end_time]
  } else{
    curr_xs = xs[,1:end_time]
    curr_ys = ys[,1:end_time]
  }
  xmin = min(curr_xs, na.rm=T)
  xmax = max(curr_xs, na.rm=T)
  ymin = min(curr_ys, na.rm=T)
  ymax = max(curr_ys, na.rm=T)
  xrange <- xmax - xmin
  yrange <- ymax - ymin

  #get coordinates of scale bar and scale bar text
  if(show_scalebar){

    if(scalebar_loc == 'topright'){
      scalebar_xmax <- xmax - xrange / scalebar_offset
      scalebar_xmin <- xmax - scalebar_size
      scalebar_y <- ymax - yrange / scalebar_offset
    }
    if(scalebar_loc == 'topleft'){
      scalebar_xmin <- xmin + xrange / scalebar_offset
      scalebar_xmax <- xmin + scalebar_size
      scalebar_y <- ymax - yrange / scalebar_offset
    }
    if(scalebar_loc == 'bottomright'){
      scalebar_xmax <- xmax - xrange / scalebar_offset
      scalebar_xmin <- xmax - scalebar_size
      scalebar_y <- ymin + yrange / scalebar_offset
    }
    if(scalebar_loc == 'bottomleft'){
      scalebar_xmin <- xmin + xrange / scalebar_offset
      scalebar_xmax <- xmin + scalebar_size
      scalebar_y <- ymin + yrange / scalebar_offset
    }
    scalebar_text_x <- mean(c(scalebar_xmin, scalebar_xmax))
    scalebar_text_y <- scalebar_y + yrange / scalebar_offset / 2

  }

  #vector of time steps
  time_steps <- seq(start_time, end_time, time_step)

  #round call times to the nearest time step that will be plotted
  calls <- calls[which(calls$time_idx >= start_time & calls$time_idx <= end_time),]
  if(nrow(calls) > 0){
    for(i in 1:nrow(calls)){
      calls$time_idx[i] <- time_steps[which.min(abs(time_steps - calls$time_idx[i]))]
    }
  }

  #create directory in which to store images
  dir_name <- paste(output_dir,'/seq_',start_time,'-',end_time,sep='')
  if(!dir.exists(dir_name)){
    dir.create(dir_name)
  }
  setwd(dir_name)

  #create images
  img_idx <- 1
  for(t in time_steps){

    print(t)

    #get xs and ys for current positions, x_t and y_t vectors
    x_t <- xs[,t]
    y_t <- ys[,t]

    #get xs and ys for tail, x_past and y_past matrices
    if(tail_time > 0){
      if((t - tail_time) < 1){
        past_idxs <- 1:t
      } else{
        past_idxs <- (t - tail_time):t
      }
      x_past <- as.matrix(xs[,past_idxs])
      y_past <- as.matrix(ys[,past_idxs])
    }

    #get calls now and in past
    calls_now <- calls[which(calls$time_idx == t),]
    calls_past <- calls[which(calls$time_idx < t & calls$time_idx >= (t - call_persist_time)),]

    #make figure
    filename = paste0(img_idx,'.png')
    png(file=filename,width=10,height=4,units='in',res=300)
    par(mar=c(0,0,0,0))
    par(bg = bg_color)

    #initialize plot
    plot(NULL,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xaxt='n',yaxt='n',xlab='',ylab='',bg=bg_color,asp=1)

    #plot "tails" (past locations)
    if(tail_time > 0){
      for(i in 1:n_inds){
          lines(x_past[i,],y_past[i,],col=colors_inds[i],lwd=2)
      }
    }

    #plot a legend if specified
    if(show_legend_inds | show_legend_calls){

      #get appropriate symbols and colors and names
      if(show_legend_inds & show_legend_calls){
        pchs_legend <- c(pchs_inds, pchs_calls)
        cols_legend <- c(colors_inds, colors_calls)
        names_legend <- c(ind_names, call_types)
      }
      if(show_legend_inds & !show_legend_calls){
        pchs_legend <- pchs_inds
        cols_legend <- colors_inds
        names_legend <- ind_names
      }
      if(!show_legend_inds & show_legend_calls){
        pchs_legend <- pchs_calls
        cols_legend <- colors_calls
        names_legend <- call_types
      }

      #plot legend
      legend(legend_loc, legend = names_legend, pch = pchs_legend, col = cols_legend, text.col = text_color)
    }

    #make a scale bar
    if(show_scalebar){
      lines(c(scalebar_xmin, scalebar_xmax),c(scalebar_y, scalebar_y), col = text_color, lwd = 3)
      text(labels = paste(scalebar_size, 'm'), x = scalebar_text_x, y = scalebar_text_y, col = text_color)
    }

    #plot current locations
    points(x_t, y_t, pch = pchs_inds, cex=2, col=colors_inds, bg=colors_inds)

    #plot calls in the past
    if(!is.null(calls) & call_persist_time > 0){
      if(nrow(calls_past)>0){
        points(xs[cbind(calls_past$ind_idx, calls_past$time_idx)],
               ys[cbind(calls_past$ind_idx, calls_past$time_idx)],
               col = colors_calls[match(calls_past$call_type, call_types)],
               pch = pchs_calls[match(calls_past$call_type, call_types)],
               cex = 1)
      }
    }

    #plot current calls
    if(!is.null(calls)){
      if(nrow(calls_now)>0){
        points(xs[cbind(calls_now$ind_idx, calls_now$time_idx)],
             ys[cbind(calls_now$ind_idx, calls_now$time_idx)],
             col = colors_calls[match(calls_now$call_type, call_types)],
             pch = pchs_calls[match(calls_now$call_type, call_types)],
             cex = 2)
      }
    }


    dev.off()

    img_idx<-img_idx+1

  }

}
