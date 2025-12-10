#' Make a visualization of individual positions and calls during a time period specified by the user. 
#' The visualization can either show all individuals in the provided data or can zoom in on one individual
#' and show its behavior in detail.
#' 
#' `r lifecycle::badge("experimental")`
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
#' @param ind_names vector of names of the individuals
#' @param focus_ind name or index of a focal individual. This centers the frame on one individual instead of producing a broad overview. When using name, ind_names must be provided as well.
#' @param zoom_radius meters around the focal individual (UTM)
#' @param bg_color background color of the plot ("black", "white", etc.)
#' @param satellite_map use a satellite map as the background (T or F)
#' @param utm_epsg WGS 84 EPSG reference number to find the geographic location of the satellite map tile, use https://epsg.io to find the reference number
#' @param auto_zoom_tiles choose satellite map zoom automatically when focusing on individual, determines satellite map resolution
#' @param zoom_global zoom level for global tiles of the background satellite map, determines satellite map resolution
#' @param zoom_focus optional explicit map zoom for focus tiles, determines satellite map resolution
#' @param colors_inds vector of colors to use for each individual (length `n_inds`). Color indices must match the individual index in calls , xs and ys
#' @param colors_calls vector of colors to use for each call type (alphabetical order by call_type)
#' @param ind_point_size size of the individual points
#' @param call_point_size size of the points for calls
#' @param pchs_inds vector of plotting symbols for individuals
#' @param pchs_calls vector of plotting symbols for calls
#' @param tail_time number of previous time steps to plot as a "tail" which trails the point showing the current location
#' @param call_persist_time number of previous time steps to still show the calls (they will shrink linearly over time in size)
#' @param show_legend_inds whether to plot a legend showing the names of the individuals (T or F)
#' @param sort_legend_inds vector to sort the legend showing the names of the individuals (vector containing the sort order by index)
#' @param show_legend_calls whether to plot a legend showing the names of the call types (T or F)
#' @param legend_loc location of the individual legend, either 'topleft' or 'topright'. Call legend will be opposite
#' @param scalebar_size number of meters for the scalebar, set to 0 to deactivate
#' @param events data frame with columns `event_id`, `start_time_idx`,`end_time_idx`,`initiator`
#' @param highlighted_radius radius of the highlighted location (usually an epicenter from hyena whoop analysis)
#' 
#' @export
generate_movement_and_calls_visualization <- function(xs = NULL, ys = NULL, timestamps = NULL, calls = NULL, start_time = NULL, 
                                                      end_time = NULL, time_step = 1, output_dir = NULL, ind_names = NULL, 
                                                      focus_ind = NULL, zoom_radius = 400, bg_color = 'black', 
                                                      satellite_map = TRUE, utm_epsg = NULL, auto_zoom_tiles = TRUE, 
                                                      zoom_global = 15, zoom_focus = NULL, colors_inds = NULL, colors_calls = NULL, 
                                                      ind_point_size = 2, call_point_size = 1, pchs_inds = NULL, 
                                                      pchs_calls = NULL, tail_time = 10, call_persist_time = 10, 
                                                      show_legend_inds = TRUE, sort_legend_inds = NULL, 
                                                      show_legend_calls = FALSE, legend_loc = 'topright', scalebar_size = 100, 
                                                      events = NULL, highlighted_radius = 1000){

  # ---CHECKS---
  # check xs and ys exist
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

  # check dimensions
  if(nrow(xs) != nrow(ys) | ncol(xs) != ncol(ys)){
    stop('xs and ys must have the same dimensions')
  }
  if(length(timestamps) != ncol(xs)){
    stop('timestamps must have the same dimensions as the columns of xs and ys or lons and lats')
  }

  if(legend_loc == "bottomright" | legend_loc == "bottomleft"){
    stop('please choose topleft or topright to avoid conflict with scale bar or map attributions')
  }

  if (!is.null(focus_ind) && is.null(zoom_radius)) {
    stop('zoom_radius (meters) must be provided when focus_ind is set')
  }

  #---SETTING UP---

  # get number of individuals and time steps
  n_inds <- nrow(xs)

  # get symbols for individuals if not specified
  if(is.null(pchs_inds)){
    pchs_inds <- rep(19, n_inds)
  }

  # get colors for individuals if not specified
  if(is.null(colors_inds)){
    colors_inds <- rainbow(n_inds)
  }

  text_color <- if (bg_color == 'black') 'white' else 'black'

  # call types and plotting params
  if (!is.null(calls)) {
    call_types <- sort(unique(calls$call_type))
    n_call_types <- length(call_types)
    if (is.null(colors_calls)) {
      colors_calls <- cm.colors(n_call_types)
    }
    # check that colors_calls is the right length
    if(length(colors_calls) != n_call_types){
      stop('colors_calls vector needs to be the length of the number of unique call types')
    }
    if (is.null(pchs_calls)) {
      pchs_calls <- rep(8, n_call_types)
    }
    # check that pchs_calls is the right length
    if(length(pchs_calls) != n_call_types){
      stop('pchs_calls vector needs to be the length of the number of unique call types')
    }
  }

  # resolve focus individual to index
  focus_idx <- NULL
  if (!is.null(focus_ind)) {
    if (is.numeric(focus_ind)) {
      if (length(focus_ind) != 1 || focus_ind < 1 || focus_ind > n_inds) {
        stop('focus_ind (numeric) must be a single index in 1..n_inds')
      }
      focus_idx <- as.integer(focus_ind)
    } else if (is.character(focus_ind)) {
      if (is.null(ind_names)) stop('provide ind_names to use focus_ind by name')
      match_idx <- match(focus_ind, ind_names)
      if (is.na(match_idx)) stop('focus_ind name not found in ind_names')
      focus_idx <- match_idx
    } else {
      stop('focus_ind must be numeric (index) or character (name)')
    }
  }

  # global bounds (used for legends/scalebar sizing and fallback)
  if ((start_time - tail_time) > 1) {
    curr_xs <- xs[, (start_time - tail_time):end_time]
    curr_ys <- ys[, (start_time - tail_time):end_time]
  } else {
    curr_xs <- xs[, 1:end_time]
    curr_ys <- ys[, 1:end_time]
  }


  # check if focus individual has any GPS data
  if (!is.null(focus_idx)) {
    if (is.na(curr_xs[focus_idx]) || is.na(curr_ys[focus_idx])) {
      message(glue("{focus_ind} has no GPS data in this timeframe"))
      return(NA)
    }
  }

  # global boundaries
  gxmin <- suppressWarnings(min(curr_xs, na.rm = TRUE))
  gxmax <- suppressWarnings(max(curr_xs, na.rm = TRUE))
  gymin <- suppressWarnings(min(curr_ys, na.rm = TRUE))
  gymax <- suppressWarnings(max(curr_ys, na.rm = TRUE))
  gxrange <- gxmax - gxmin
  gyrange <- gymax - gymin

  # padding
  if (show_legend_calls && show_legend_inds) { gpad <- 3 } else { gpad <- 8 }
  gxmin <- gxmin - gxrange / gpad
  gxmax <- gxmax + gxrange / gpad
  gymin <- gymin - gyrange / 8
  gymax <- gymax + gyrange / 8
  gxrange <- gxmax - gxmin
  gyrange <- gymax - gymin

  # controlling aspect ratio
  gar_min <- 0.5
  gar_max <- 2
  gar_data <- gyrange / gxrange
  gcx <- (gxmin + gxmax) / 2
  gcy <- (gymin + gymax) / 2

  if (gar_data < gar_min) {
    target_yrange <- gar_min * gxrange
    extra <- (target_yrange - gyrange) / 2
    gymin <- gcy - (gyrange / 2 + extra)
    gymax <- gcy + (gyrange / 2 + extra)
  } else if (gar_data > gar_max) {
    target_xrange <- gyrange / gar_max
    extra <- (target_xrange - gxrange) / 2
    gxmin <- gcx - (gxrange / 2 + extra)
    gxmax <- gcx + (gxrange / 2 + extra)
  }

  gxrange <- gxmax - gxmin
  gyrange <- gymax - gymin

  # satellite map config (tiles fetched per-frame)
  tile_cache <- NULL
  crs_utm <- NULL
  attrib <- "Sources: Esri, Tom Tom, FAO, NOAA, USGS | Powered by Esri"

  if (satellite_map) {
    if (is.null(utm_epsg)) {stop('must specify EPSG reference number to retrieve map tiles')}
    tile_cache <- file.path(output_dir, "map_tile_cache")
    dir.create(tile_cache, showWarnings = FALSE, recursive = TRUE)
    crs_utm <- sf::st_crs(utm_epsg)
  }

  # time steps
  time_steps <- seq(start_time, end_time, time_step)

  # subset and round call times to plotted steps
  if (!is.null(calls)) {
    calls <- calls[which(calls$time_idx >= start_time & calls$time_idx <= end_time), ]
    if (nrow(calls) > 0) {
      for (i in 1:nrow(calls)) {
        calls$time_idx[i] <- time_steps[which.min(abs(time_steps - calls$time_idx[i]))]
      }
    }
  }

  # create output directory
  dir_name <- paste(output_dir, '/seq_', start_time, '-', end_time, sep = '')
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  setwd(dir_name)

  # create images
  img_idx <- 1
  total_idx <- length(time_steps)

  # last known center for focus fallback and de-jittering
  last_center_x <- NA_real_
  last_center_y <- NA_real_

  # keep last successfully fetched tiles to reuse on transient failures
  bm_last <- NULL

  # --- ITERATE OVER TIME STEPS---
  for (t in time_steps) {
    cat(sprintf("\r%d/%d", img_idx, total_idx))
    flush.console()

    # get xs and ys for current positions, x_t and y_t vectors
    x_t <- xs[, t]
    y_t <- ys[, t]

    # find individuals without GPS data and set their legend text color to grey
    nogps_inds <- union(which(is.na(x_t)), which(is.na(y_t)))
    ind_text_color <- rep(text_color, n_inds)
    ind_text_color[nogps_inds] <- "#737373"

    # get xs and ys for tail, x_past and y_past matrices
    if(tail_time > 0) {
      if((t - tail_time) < 1){
        past_idxs <- 1:t
      } else {
        past_idxs <- (t - tail_time):t
      }
      x_past <- as.matrix(xs[, past_idxs])
      y_past <- as.matrix(ys[, past_idxs])
    }

    # get calls now and in past
    if (!is.null(calls)) {
      calls_now <- calls[which(calls$time_idx == t), ]
      calls_past <- calls[which(calls$time_idx < t & calls$time_idx >= (t - call_persist_time)), ]
    } else {
      calls_now <- NULL
      calls_past <- NULL
    }

    # if plotting events, get info about events
    in_event <- FALSE
    if (!is.null(events)) {
      curr_event_idx <- which(events$start_time_idx <= t & events$end_time_idx >= t)
      if (length(curr_event_idx) > 0) {
        in_event <- TRUE
        initiator <- events$initiator[curr_event_idx]
        start_time_idx <- events$start_time_idx[curr_event_idx]
        end_time_idx <- events$end_time_idx[curr_event_idx]
      }
    }
    title_col <- if (in_event) 'red' else text_color

    # when focus is activated, calculate dynamic per-frame bounds with de-jittering
    use_focus <- !is.null(focus_idx)
    if (use_focus) {
      cx_t <- x_t[focus_idx]
      cy_t <- y_t[focus_idx]

      # fallbacks if focus individuals location is NA
      if (is.na(cx_t) || is.na(cy_t)) {
        if (!is.na(last_center_x) && !is.na(last_center_y)) {
          cx_t <- last_center_x
          cy_t <- last_center_y
        } else {
          past <- if ((t - tail_time) < 1) 1:t else (t - tail_time):t
          cx_hist <- xs[focus_idx, past]
          cy_hist <- ys[focus_idx, past]
          valid <- which(!is.na(cx_hist) & !is.na(cy_hist))
          if (length(valid) > 0) {
            recent_ix <- max(valid)
            cx_t <- cx_hist[recent_ix]
            cy_t <- cy_hist[recent_ix]
          } else {
            cx_t <- (gxmin + gxmax) / 2
            cy_t <- (gymin + gymax) / 2
          }
        }
      }

      # filter out GPS drift to avoid jittering frames
      if (!is.na(last_center_x) && !is.na(last_center_y)) {
        d_move <- sqrt((cx_t - last_center_x)^2 + (cy_t - last_center_y)^2)
        # 1% jitter is being filtered
        if (d_move <= 0.01 * zoom_radius) {
          cx_t <- last_center_x
          cy_t <- last_center_y
        }
      }

      # calculate focus individual boundaries
      xmin <- cx_t - zoom_radius
      xmax <- cx_t + zoom_radius
      ymin <- cy_t - zoom_radius
      ymax <- cy_t + zoom_radius

      xrange <- xmax - xmin
      yrange <- ymax - ymin
      ar <- yrange / xrange
      last_center_x <- cx_t
      last_center_y <- cy_t
    } 
    # use global boundaries
    else {
      xmin <- gxmin; xmax <- gxmax; ymin <- gymin; ymax <- gymax
      xrange <- xmax - xmin; yrange <- ymax - ymin
      ar <- yrange / xrange
    }

    # device sizing per frame
    res_dpi <- 150
    min_width_px <- 800
    min_height_px <- 600
    fig_width_in <- 6
    width_px  <- max(min_width_px,  round(fig_width_in * res_dpi))
    height_px <- max(min_height_px, round(width_px * ar))
    width_in  <- width_px  / res_dpi
    height_in <- height_px / res_dpi

    ui_ref_px <- 700
    legend_cex <- max(0.7, min(1.5, height_px / ui_ref_px))
    title_cex  <- 1.1 * legend_cex
    attrib_cex <- 0.6 * legend_cex
    scalebar_lwd <- max(2, 2.5 * legend_cex)
    title_y <- ymax - 0.035 * (ymax - ymin)

    # scalebar geometry per frame
    if (scalebar_size > 0) {
      scalebar_xmin <- xmin + xrange * 0.05
      scalebar_xmax <- scalebar_xmin + scalebar_size
      scalebar_y    <- ymin + yrange * 0.05
      scalebar_text_x <- (scalebar_xmin + scalebar_xmax) / 2
      scalebar_text_y <- scalebar_y + yrange * (0.05 / 2)
    }

    # per-frame attribution position
    attrib_x <- xmax - 0.02 * (xmax - xmin)
    attrib_y <- ymin + 0.025 * (ymax - ymin)

    # choose zoom level per frame
    selected_zoom <- zoom_global
    if (use_focus) {
      if (!is.null(zoom_focus)) {
        selected_zoom <- zoom_focus
      } else if (auto_zoom_tiles) {
        selected_zoom <- if (zoom_radius < 500) 18 else if (zoom_radius < 1000) 17 else zoom_global
      } else {
        selected_zoom <- zoom_global
      }
    }

    # make figure
    filename <- paste0(img_idx, '.png')
    png(file = filename, width = width_in, height = height_in, units = 'in', res = res_dpi)
    par(mar = c(0, 0, 0, 0), xaxs = 'i', yaxs = 'i', xpd = NA)
    par(bg = bg_color)
    par(ps = 13)

    if (satellite_map) {
      # fetch tiles for the current frame extent (cached)
      coords_frame <- matrix(
        c(xmin, ymin,
          xmin, ymax,
          xmax, ymax,
          xmax, ymin,
          xmin, ymin),
        ncol = 2, byrow = TRUE
      )
      ext_utm_frame <- sf::st_sfc(sf::st_polygon(list(coords_frame)), crs = crs_utm)

      plot(NA, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', asp = 1, type = 'n')

      bm_frame <- try(maptiles::get_tiles(ext_utm_frame, provider = "Esri.WorldImagery",zoom = selected_zoom,
                                          crop = TRUE, cachedir = tile_cache, verbose = FALSE), silent = TRUE)

      if (!inherits(bm_frame, "try-error")) {
        bm_last <- bm_frame
        terra::plotRGB(bm_frame, add = TRUE, axes = FALSE, stretch = "lin", r = 1, g = 2, b = 3)
      } else if (!is.null(bm_last)) {
        # fallback to last successful tiles if current fetch fails
        terra::plotRGB(bm_last, add = TRUE, axes = FALSE, stretch = "lin", r = 1, g = 2, b = 3)
      }
      # darken map for contrast
      rect(xmin, ymin, xmax, ymax, col = rgb(0.09, 0.07, 0.06, 0.5), border = NA)

      # map attribution
      text(x = attrib_x, y = attrib_y, labels = attrib, adj = c(1, 0), cex = attrib_cex, col = rgb(1, 1, 1, 0.85))
    } else {
      plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bg = bg_color, asp = 1)
    }

    # plot the title
    if (is.null(focus_ind)) {
        title_text <- timestamps[t]
    } else {
      title_text <- glue("{focus_ind}: {timestamps[t]}")
    }
    text(x = (xmin + xmax) / 2, y = title_y, labels = title_text, col = title_col, cex = title_cex, font = 2)

    # plot event highlighted location (epicenter)
    if (in_event) {
      ang_bins <- seq(0, 2 * pi, length.out = 100)
      for (e in seq_along(curr_event_idx)) {
        highlighted_loc_x <- xs[initiator[e], start_time_idx[e]]
        highlighted_loc_y <- ys[initiator[e], start_time_idx[e]]
        x_circ <- highlighted_radius * cos(ang_bins) + highlighted_loc_x
        y_circ <- highlighted_radius * sin(ang_bins) + highlighted_loc_y
        lines(x_circ, y_circ, lwd = 1, col = 'red')
      }
    }

    # individual legend
    if (show_legend_inds) {
      # sort individual names in the legend
      sort_order <- if (is.null(sort_legend_inds)) 1:n_inds else sort_legend_inds
      lg_ind_names <- ind_names[sort_order]
      lg_pchs_inds <- pchs_inds[sort_order]
      lg_colors_inds <- colors_inds[sort_order]
      lg_ind_text_color <- ind_text_color[sort_order]

      # max height of the legend
      max_h_user <- 0.95 * (ymax - ymin)
      draw_cex <- legend_cex

      # try shrinking cex until it fits, but not below 0.4 × base
      for (s in seq(1, 0.4, by = -0.05)) {
        test_cex <- legend_cex * s
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
             inset = 0.01, bty = "black")
    }

    # plot call legend
    if(show_legend_calls){

      # move call legend opposite of the individual legend
      if(legend_loc == "topright") {
        call_legend_loc <- "topleft"
      }
      if (legend_loc == "topleft") {
        call_legend_loc <- "topright"
      }

      max_h_user <- 0.95 * (ymax - ymin)
      draw_cex <- legend_cex

      for (s in seq(1, 0.5, by = -0.05)) {
        test_cex <- legend_cex * s
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
             inset = 0.01, bty = "black")
    }

    # plot "tails" (past locations)
    if (tail_time > 0) {
      for (i in 1:n_inds) {
        lines(x_past[i, ], y_past[i, ], col = colors_inds[i], lwd = 2)
      }
    }

    # plot current locations
    points(x_t, y_t, pch = pchs_inds, cex = ind_point_size, col = colors_inds, bg = colors_inds)

    # emphasize focused individual
    if (use_focus && !is.na(x_t[focus_idx]) && !is.na(y_t[focus_idx])) {
      points(x_t[focus_idx], y_t[focus_idx], pch = 21, cex = ind_point_size,
             col = "white", bg = colors_inds[focus_idx], lwd = 2)
    }

    # plot calls in the past
    if (!is.null(calls) && call_persist_time > 0) {
      if (nrow(calls_past) > 0) {
        points(xs[cbind(calls_past$ind_idx, calls_past$time_idx)],
               ys[cbind(calls_past$ind_idx, calls_past$time_idx)],
               bg = colors_calls[match(calls_past$call_type, call_types)],
               pch = pchs_calls[match(calls_past$call_type, call_types)],
               cex = call_point_size, col = "white")
      }
    }

    # plot current calls
    if (!is.null(calls)) {
      if (nrow(calls_now) > 0) {
        points(xs[cbind(calls_now$ind_idx, calls_now$time_idx)],
               ys[cbind(calls_now$ind_idx, calls_now$time_idx)],
               bg = colors_calls[match(calls_now$call_type, call_types)],
               pch = pchs_calls[match(calls_now$call_type, call_types)],
               cex = call_point_size, col = "white")
      }
    }

    # plot scale bar
    if (scalebar_size > 0) {
      lines(c(scalebar_xmin, scalebar_xmax), c(scalebar_y, scalebar_y), col = text_color, lwd = scalebar_lwd)
      text(labels = paste(scalebar_size, 'm'), x = scalebar_text_x, y = scalebar_text_y, col = text_color, 
           cex = legend_cex)
    }

    dev.off()
    img_idx <- img_idx + 1
  }
}
