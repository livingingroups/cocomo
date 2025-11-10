library(glue)
library(stringr)
library(sf)
library(rosm)
library(maptiles)
library(terra)
library(raster)

#' Make a visualization of individual positions and calls during a time period specified by the user.
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author Marius Faiß (secondary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param xs matrix of dimensions `n_inds` x `n_times` where `xs[i,t]` gives the x position (numeric) of individual `i` at time step `t`, positions in UTM format
#' @param ys matrix of dimensions `n_inds` x `n_times` where `ys[i,t]` gives the y position (numeric) of individual `i` at time step `t`, positions in UTM format
#' @param timestamps vector of timestamps (POSIXct), must have same dimensions as columns of `xs` and `ys` matrices
#' @param calls data frame where first column (`'ind_idx'`) specifies the index of the individual that gave the call, second column (`'time_idx'`) specifies the time index at which the call was given, and third column (`'call_type'`) specifies the type of call (character string)
#' @param start_time time index at which to start the video
#' @param end_time time index at which to end the video
#' @param time_step time step to use (an image every time_step timesteps will be produced)
#' @param output_dir directory in which to store the folder of outputted images
#' @param tail_time number of previous time steps to plot as a "tail" which trails the point showing the current location
#' @param call_persist_time number of previous time steps to still show the calls (they will shrink linearly over time in size)
#' @param colors_inds vector of colors to use for each individual (length `n_inds`). Color indices must match the individual index in calls , xs and ys
#' @param colors_calls vector of colors to use for each call type (alphabetical order by call_type)
#' @param pchs_inds vector of plotting symbols for individuals
#' @param pchs_calls vector of plotting symbols for calls
#' @param show_legend_inds whether to plot a legend showing the names of the individuals (T or F)
#' @param sort_legend_inds vector to sort the legend showing the names of the individuals (vector containing the sort order by index)
#' @param show_legend_calls whether to plot a legend showing the names of the call types (T or F)
#' @param legend_loc location of the individual legend, either `topleft','topright','bottomleft' or 'bottomright'`. Call legend will be opposite
#' @param scalebar_size number of meters for the scalebar
#' @param ind_names vector of names of the individuals
#' @param bg_color background color of the plot ("black", "white", etc.)
#' @param satellite_map use a satellite map as the background (T or F)
#' @param utm_epsg WGS 84 EPSG reference number to find the geographic location of the satellite map tile, use https://epsg.io to find the reference number
#' @param ind_point_size size of the individual points
#' @param call_point_size size of the points for calls
#' @param events data frame with columns `event_id`, `start_time_idx`,`end_time_idx`,`initiator`
#' @param highlighted_radius radius of the highlighted location (usually an epicenter from hyena whoop analysis)
#'
#'
#' @export
generate_movement_and_calls_visualization <- function(xs = NULL, ys = NULL, timestamps = NULL, calls = NULL, start_time = NULL, 
                                                      end_time = NULL, time_step = 1, output_dir = NULL, tail_time = 10, 
                                                      call_persist_time = 5, colors_inds = NULL, colors_calls = NULL, 
                                                      pchs_inds = NULL, pchs_calls = NULL, show_legend_inds = T, 
                                                      sort_legend_inds = NULL, show_legend_calls = F, legend_loc = 'topright',
                                                      scalebar_size = 100, ind_names = NULL, bg_color = 'black', satellite_map = T, utm_epsg = NULL,
                                                      ind_point_size = 2, call_point_size = 2, events = NULL, 
                                                      highlighted_radius = 1000
                                                      ){

  #---CHECKS---
  #check xs and ys exist
  if(is.null(xs) | is.null(ys)){
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

  #get symbols for individuals if not specified
  if(is.null(pchs_inds)){
    pchs_inds <- rep(19, n_inds)
  }

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

    #check that pchs_calls is the right length
    if(length(pchs_calls) != n_call_types){
      stop('pchs_calls vector needs to be the length of the number of unique call types')
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

  # compute raw bounds ignoring extreme outliers
  xq <- stats::quantile(curr_xs, probs = c(0.10, 0.90), na.rm = TRUE)
  yq <- stats::quantile(curr_ys, probs = c(0.10, 0.90), na.rm = TRUE)
  xmin <- xq[1]; xmax <- xq[2]
  ymin <- yq[1]; ymax <- yq[2]

  xmin = min(curr_xs, na.rm=T)
  xmax = max(curr_xs, na.rm=T)
  ymin = min(curr_ys, na.rm=T)
  ymax = max(curr_ys, na.rm=T)
  xrange <- xmax - xmin
  yrange <- ymax - ymin

  #pad the min and max a little bit
  xmin <- xmin - xrange / 3
  xmax <- xmax + xrange / 3
  ymin <- ymin - yrange / 7
  ymax <- ymax + yrange / 7
  xrange <- xmax - xmin
  yrange <- ymax - ymin

  # Clamp aspect ratio by expanding the shorter axis
  ar_min = 0.5
  ar_max = 2
  ar_data <- yrange / xrange
  ar_target <- max(ar_min, min(ar_data, ar_max))  # clamp
  cx <- (xmin + xmax) / 2
  cy <- (ymin + ymax) / 2

  if (ar_data < ar_min) {
    # too flat: increase y-range to meet ar_min
    target_yrange <- ar_min * xrange
    extra <- (target_yrange - yrange) / 2
    ymin <- cy - (yrange / 2 + extra)
    ymax <- cy + (yrange / 2 + extra)
  } else if (ar_data > ar_max) {
    # too tall: increase x-range to meet ar_max
    target_xrange <- yrange / ar_max
    extra <- (target_xrange - xrange) / 2
    xmin <- cx - (xrange / 2 + extra)
    xmax <- cx + (xrange / 2 + extra)
  }

  # recompute final ranges, ar
  xrange <- xmax - xmin
  yrange <- ymax - ymin
  ar <- yrange / xrange  # this equals ar_target now

  # device size: enforce min pixels so legends/text don't get cramped
  res_dpi <- 200
  min_width_px <- 800
  min_height_px <- 600
  fig_width_in <- 6

  # start from requested width in inches, but ensure minimum pixels
  width_px  <- max(min_width_px,  round(fig_width_in * res_dpi))
  height_px <- max(min_height_px, round(width_px * ar))
  width_in  <- width_px  / res_dpi
  height_in <- height_px / res_dpi

  # UI scaling parameters
  ui_ref_px <- 900
  legend_cex <- max(0.7, min(1.2, height_px / ui_ref_px))
  title_cex  <- 1.1 * legend_cex
  attrib_cex <- 0.7 * legend_cex
  scalebar_lwd <- max(2, 2.5 * legend_cex)
  title_y <- ymax - 0.035 * (ymax - ymin)

  # build the satellite map
  if (satellite_map) {
    if(is.null(utm_epsg)){
      stop('must specify EPSG reference number to retrieve map tiles')
    }
    
    # cache tiles for repeated use
    tile_cache <- file.path(output_dir, "tile_cache")
    dir.create(tile_cache, showWarnings = FALSE, recursive = TRUE)
    
    # map data attribution
    attrib <- "Sources: Esri, Tom Tom, FAO, NOAA, USGS | Powered by Esri"
    attrib_x <- xmax - 0.02 * (xmax - xmin)
    attrib_y <- ymin + 0.025 * (ymax - ymin)

    crs_utm <- sf::st_crs(utm_epsg)

    # Build the extent polygon explicitly (avoids the bbox-to-polygon NA trap)
    coords <- matrix(
      c(xmin, ymin,
        xmin, ymax,
        xmax, ymax,
        xmax, ymin,
        xmin, ymin),
      ncol = 2, byrow = TRUE
    )
    ext_utm <- sf::st_sfc(sf::st_polygon(list(coords)), crs = crs_utm)

    bm <- maptiles::get_tiles(ext_utm,
                              provider = "Esri.WorldImagery",
                              zoom = 15,     # 14–16
                              crop = TRUE,
                              cachedir = tile_cache,
                              verbose = TRUE)
    terra::crs(bm)

    # speed up map generation
    # downsample the basemap to at most the device resolution
    nx <- terra::ncol(bm)
    ny <- terra::nrow(bm)
    fx <- ceiling(nx / width_px)
    fy <- ceiling(ny / height_px)
    if (fx > 1 || fy > 1) {
      bm <- terra::aggregate(bm, fact = c(fx, fy), fun = "mean")
    }

    # Use first three bands (ignore alpha if present)
    if (terra::nlyr(bm) >= 4) bm <- bm[[1:3]]
    for (i in 1:3) {
      v <- terra::values(bm[[i]], mat = FALSE)  # numeric vector
      q <- stats::quantile(v, probs = c(0.02, 0.98), na.rm = TRUE, names = FALSE)
      qmin <- q[1]; qmax <- q[2]
      if (!is.finite(qmin) || !is.finite(qmax) || qmax <= qmin) next
      b <- bm[[i]]
      b <- (b - qmin) / (qmax - qmin)
      b <- terra::clamp(b, 0, 1)
      bm[[i]] <- b * 255
    }
    bm <- terra::round(bm)
  }

  # plotting the scalebar
  scalebar_xmin <- xmin + xrange * 0.05
  scalebar_xmax <- scalebar_xmin + scalebar_size
  scalebar_y    <- ymin + yrange * 0.05

  # text for the scalebar
  scalebar_text_x <- (scalebar_xmin + scalebar_xmax) / 2
  scalebar_text_y <- scalebar_y + yrange * (0.05 / 2)

  #vector of time steps
  time_steps <- seq(start_time, end_time, time_step)

  #round call times to the nearest time step that will be plotted
  calls <- calls[which(calls$time_idx >= start_time & calls$time_idx <= end_time),]
  if(nrow(calls) > 0){
    for(i in 1:nrow(calls)){ # TODO isnt this unnecessary?
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
  total_idx <- length(time_steps)

  for(t in time_steps){
    print(glue("{img_idx}/{total_idx}"))

    #get xs and ys for current positions, x_t and y_t vectors
    x_t <- xs[,t]
    y_t <- ys[,t]

    # find individuals without GPS data and set their legend text color to grey
    nogps_inds <- union(which(is.na(x_t)), which(is.na(y_t)))
    ind_text_color <- rep(text_color, n_inds)
    ind_text_color[nogps_inds] = "#737373"

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

    #if plotting events, get info about events
    in_event <- F
    if(!is.null(events)){
      curr_event_idx <- which(events$start_time_idx <= t & events$end_time_idx >= t)
      if(length(curr_event_idx)>0){
        in_event <- T
        initiator <- events$initiator[curr_event_idx]
        start_time_idx <- events$start_time_idx[curr_event_idx]
        end_time_idx <- events$end_time_idx[curr_event_idx]
      }
    }
    title_col <- if (in_event) 'red' else text_color

    #make figure
    filename = paste0(img_idx,'.png')
    png(file=filename, width=width_in, height=height_in, units='in', res=res_dpi)
    par(mar=c(0,0,0,0), xaxs = 'i', yaxs = 'i', xpd = NA)
    par(bg=bg_color)
    par(ps=13)

    if(satellite_map && !is.null(bm)){
      plot(NA, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xaxt='n', yaxt='n', xlab='', ylab='', asp=1, type='n')
      terra::plotRGB(bm, add=TRUE, axes=FALSE, stretch = "lin", r = 1, g = 2, b = 3)

      # darken map for contrast
      rect(xmin, ymin, xmax, ymax, col = rgb(0,0,0,0.5), border = NA)

      # add attribution
      text(x = attrib_x, y = attrib_y, labels = attrib, adj = c(1, 0), cex = attrib_cex, col = rgb(1, 1, 1, 0.85))
    } 
    else{
      # black blank background if not using a satellite map
      plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bg = bg_color, asp = 1)
    }

    # plot the title
    text(x = (xmin+xmax)/2, y = title_y, labels = timestamps[t], col = title_col, cex = title_cex, font = 2)

    #plot event highlighted location (epicenter)
    if(in_event){
      ang_bins <- seq(0,2*pi,length.out=100)
      for(e in seq_along(curr_event_idx)){
        highlighted_loc_x <- xs[initiator[e], start_time_idx[e]]
        highlighted_loc_y <- ys[initiator[e], start_time_idx[e]]
        x_circ <- highlighted_radius*cos(ang_bins) + highlighted_loc_x
        y_circ <- highlighted_radius*sin(ang_bins) + highlighted_loc_y
        lines(x_circ,y_circ, lwd = 1, col = 'red')
      }
    }

    if (show_legend_inds) {
      # sort individuals
      sort_order <- if (is.null(sort_legend_inds)) 1:n_inds else sort_legend_inds
      lg_ind_names <- ind_names[sort_order]
      lg_pchs_inds <- pchs_inds[sort_order]
      lg_colors_inds <- colors_inds[sort_order]
      lg_ind_text_color <- ind_text_color[sort_order]

      # base cex (use your existing legend_cex if you compute one; otherwise 1)
      base_cex <- if (exists("legend_cex")) legend_cex else 1

      # allow legend to occupy at most 25–30% of map height
      max_h_user <- 0.95 * (ymax - ymin)

      draw_cex <- base_cex
      # Try shrinking cex until it fits, but not below 0.4×base
      for (s in seq(1, 0.4, by = -0.05)) {
        test_cex <- base_cex * s
        sz <- legend(legend_loc, legend = lg_ind_names, pch = lg_pchs_inds,
                    col = lg_colors_inds, text.col = lg_ind_text_color,
                    cex = test_cex, ncol = 1, y.intersp = 0.85,
                    bty = "n", plot = FALSE)
        if (sz$rect$h <= max_h_user) {
          draw_cex <- test_cex
          break
        }
      }

      # draw the legend with the chosen cex
      legend(legend_loc, legend = lg_ind_names, pch = lg_pchs_inds,
            col = lg_colors_inds, text.col = lg_ind_text_color,
            cex = draw_cex, ncol = 1, y.intersp = 0.85,
            inset = 0.01, bty = adjustcolor("black", alpha.f = 0.5))
    }

    # plot call legend
    if(show_legend_calls){
      # move call legend opposite of the individual legend
      if(legend_loc == "topright"){
        call_legend_loc <- "topleft"
      }
      if(legend_loc == "topleft"){
        call_legend_loc <- "topright"
      }
      if(legend_loc == "bottomright"){
        call_legend_loc <- "bottomleft"
      }
      if(legend_loc == "bottomleft"){
        call_legend_loc <- "bottomright"
      }
      base_cex <- if (exists("legend_cex")) legend_cex else 1
      max_h_user <- 0.95 * (ymax - ymin)
      draw_cex <- base_cex
      for (s in seq(1, 0.5, by = -0.05)) {
        test_cex <- base_cex * s
        sz <- legend(call_legend_loc, legend = call_types, pch = pchs_calls,
                    pt.bg = colors_calls, col = "white", text.col = text_color,
                    cex = test_cex, ncol = 1, y.intersp = 0.5,
                    bty = "n", plot = FALSE)
        if (sz$rect$h <= max_h_user) {
          draw_cex <- test_cex
          break
        }
      }
      legend(call_legend_loc, legend = call_types, pch = pchs_calls,
            pt.bg = colors_calls, col = "white", text.col = text_color,
            cex = draw_cex, ncol = 1, y.intersp = 0.85,
            inset = 0.01, bty = adjustcolor("black", alpha.f = 0.85))

    }

    #plot "tails" (past locations)
    if(tail_time > 0){
      for(i in 1:n_inds){
          lines(x_past[i,],y_past[i,],col=colors_inds[i],lwd=2)
      }
    }

    #plot current locations
    points(x_t, y_t, pch = pchs_inds, cex=ind_point_size, col=colors_inds, bg=colors_inds)

    #plot calls in the past
    if(!is.null(calls) & call_persist_time > 0){
      if(nrow(calls_past)>0){
        points(xs[cbind(calls_past$ind_idx, calls_past$time_idx)],
               ys[cbind(calls_past$ind_idx, calls_past$time_idx)],
               bg = colors_calls[match(calls_past$call_type, call_types)],
               pch = pchs_calls[match(calls_past$call_type, call_types)],
               cex = call_point_size, col = "white")
      } 
    }

    #plot current calls
    if(!is.null(calls)){
      if(nrow(calls_now)>0){
        points(xs[cbind(calls_now$ind_idx, calls_now$time_idx)],
             ys[cbind(calls_now$ind_idx, calls_now$time_idx)],
             bg = colors_calls[match(calls_now$call_type, call_types)],
             pch = pchs_calls[match(calls_now$call_type, call_types)],
             cex = call_point_size, col = "white")
      }
    }

    # make a scale bar
    lines(c(scalebar_xmin, scalebar_xmax), c(scalebar_y, scalebar_y), col = text_color, lwd = scalebar_lwd)
    text(labels = paste(scalebar_size, 'm'), x = scalebar_text_x, y = scalebar_text_y, col = text_color, cex = legend_cex)

    dev.off()
    img_idx <- img_idx + 1
  }
  }
