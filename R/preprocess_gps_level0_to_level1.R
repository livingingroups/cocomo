#' Preprocess GPS data (in matrix format) from level 0 to level 1
#'
#' This function takes in a level 0 dataset (raw GPS data) and performs minimal
#' pre-processing to return a level 1 dataset.
#' 
#' 
#' The function performs the following steps (in order):
#' 1. If `remove_unrealistic_speeds = T`, removes unrealistic speeds (greater than `max_speed_percentile`) and replaces them with NAs (default .9995 quantile, or alternatively a max speed `max_speed` can be set manually.
#'
#' 2. If `remove_isolated_points = T`, finds extreme distances `> max_dist_percentile` quantile (default 99.99%) or `< 1 - max_dist_percentile` of `xs` or `ys` for each individual and, if there are no other points from that individual within `max_isolated_point_dist` (default 1000 m) of that point, replaces them with `NA`s
#'
#' 3. If `remove_unrealistic_locations = T`, finds extreme xs and ys above `mean + sd * max_sd_away` (default 10) for each ind and removes those
#'
#' 4. If `bounding_box != NULL`, removes all points outside of a specified `bounding_box = c(min_easting, max_easting, min_northing, max_northing)`
#'
#' 5. If `interpolate_small_gaps = T`, fills in missing data gaps less than length `max_interp_len` with linear interpolation (default 5)
#'
#' 6.	If `interpolate_stationary_periods = T`, finds instances where an animal did not move more than `max_move_dist` (default 5 m) during an `NA` gap of `< max_move_time` (default 300 timesteps) and replaces
#'		them with the mean location of the individual between start and end of the sequence
#'
#' @author Ariana Strandburg-Peshkin (primary author)
#' @author NOT YET CODE REVIEWED
#'
#' @param input_file_path full path to the input file containing `xs`, `ys`, `timestamps`, and `ids` (overrides manual passing in of these parameters), must be an RData file
#' @param output_file_path full path to the output file where the level 1 dataset will be stored, must end in .RData
#' @param xs `n_inds x n_times` matrix giving x coordinates of each individual over time (if an input file is not specified, pass this in manually)
#' @param ys `n_inds x n_times` matrix giving y coordinates of each individual over time (if an input file is not specified, pass this in manually)
#' @param timestamps vector of timestamps (if an input file is not specified, pass this in manually)
#' @param ids data frame containing information about each individual (if an input file is not specified, pass this in manually)
#' @param breaks vector giving indexes to breaks in the data (e.g. gaps between recording intervals), if the sequence is not continuous. breaks should specify the index associated with the beginning of each interval, starting with 1 (the first interval)
#' @param remove_unrealistic_speeds whether to remove unrealistic speeds (`T` or `F`)
#' @param remove_unrealistic_locations whether to remove unrealistic locations (`T` or `F`)
#' @param remove_isolated_points whether to remove isolated points (`T` or `F`)
#' @param interpolate_small_gaps whether to interpolate small gaps (`T` or `F`)
#' @param interpolate_stationary_periods whether to interpolate stationary periods (`T` or `F`) - cannot be run unless `interpolate_small_gaps` is also `T`
#' @param max_speed_percentile quantile to use to define the maximum speed
#' @param max_speed maximum speed (overrides max_speed_percentile if specified)
#' @param max_sd_away standard deviation of xs and ys distributions for each individual beyond which points will be removed
#' @param max_dist_percentile quantile to use to define the maximum x and y coordinates (all those outside will be removed)
#' @param max_isolated_point_dist maximum isolated point distance
#' @param max_interp_len maximum length of an `NA` gap to linearly interpolate (number of time points)
#' @param max_move_dist maximum distance moved during a time `max_move_time` to interpolate using the average position
#' @param max_move_time maximum time of a gap to interpolate if stationary (in timesteps)
#' @param bounding_box vector of length 4 giving a bounding box outside of which points will be removed - should be in the format `c(min_easting, max_easting, min_northing, max_northing)`
#' @param verbose whether to print out progress and information
#'
#' @returns Returns a list containing new `xs` and `ys` matrices, and also saves them plus the `timestamps` and `ids` objects to an output file if specified
#'
#' @export
#'
preprocess_gps_level0_to_level1 <- function(input_file_path = NULL,
                                            output_file_path = NULL,
                                            xs = NULL,
                                            ys = NULL,
                                            timestamps = NULL,
                                            ids = NULL,
                                            breaks = NULL,
                                            remove_unrealistic_speeds = T,
                                            remove_isolated_points = T,
                                            remove_unrealistic_locations = T,
                                            interpolate_small_gaps = T,
                                            interpolate_stationary_periods = T,
                                            max_speed_percentile = .9995,
                                            max_speed = NULL,
                                            max_sd_away = 10,
                                            max_dist_percentile = .9999,
                                            max_isolated_point_dist = 1000,
                                            max_interp_len = 5,
                                            max_move_dist = 5,
                                            max_move_time = 5,
                                            bounding_box = NULL,
                                            verbose = T){
  #Load level 0 gps data
  if(!is.null(input_file_path)){
    if(verbose){
      print('loading level 0 data')
    }
    load(input_file_path)
  }

  #Check for the existence of xs, ys, timestamps, and ids
  if(is.null(xs) | is.null(ys) | is.null(timestamps)){
    stop('xs, ys, and timestamps need to be contained in the input file or passed in manually as variables')
  }
  if(is.null(ids)){
    warning('ids variable is not included - it is not strictly needed but will therefore not be saved in the output file')
  }

  #number of indidivudals
  n_inds <- nrow(xs)
  n_times <- ncol(xs)

  #check dimensions
  if(nrow(ys) != n_inds | ncol(ys) != n_times){
    stop('xs and ys must have the same dimensions')
  }
  if(length(timestamps) != n_times){
    stop('timestamps must be the same length as the number of columns in xs and ys matrices')
  }

  if(verbose){
    print('initial NA frac:')
    print(sum(is.na(xs))/length(xs))
  }

  #if breaks is not present assume the data is all in one continuous chunk
  if(is.null(breaks)){
    breaks <- c(1)
    include_breaks <- F
  } else{
    include_breaks <- T
  }

  #add an end point to breaks for later usage in the for loops below
  breaks_with_end <- c(breaks, n_times + 1)

  #get number of breaks, for later usage in the for loop below
  n_breaks <- length(breaks)

  #1.----Remove unrealistic speeds----

  if(remove_unrealistic_speeds){

    if(verbose){
      print('removing unrealistic speeds')
    }

    #Find extreme speeds and replace with NAs
    speeds <- matrix(NA,nrow=n_inds,ncol=n_times)
    for(b in 1:n_breaks){

      #get time indexes associated with each chunk
      tidxs <- seq(breaks_with_end[b], breaks_with_end[b+1]-1, 1)

      #for each individual get speed
      for(i in 1:n_inds){
        speeds[i,tidxs[1:(length(tidxs)-1)]] <- sqrt(diff(xs[i,tidxs])^2 + diff(ys[i,tidxs])^2)
      }
    }


    #get maximum speed based on percentile unless it was specified
    if(is.null(max_speed)){
      max_speed <- quantile(speeds, max_speed_percentile, na.rm=T)
    }

    #replace unrealistic speeds with NAs
    for(i in 1:n_inds){
      unrealistic_speed_idxs <- which(speeds[i,] > max_speed)
      xs[i,unrealistic_speed_idxs] <- NA
      ys[i,unrealistic_speed_idxs] <- NA
      xs[i,unrealistic_speed_idxs + 1] <- NA
      ys[i,unrealistic_speed_idxs + 1] <- NA
    }

    if(verbose){
      print('after removing unrealistic speeds, fraction of NAs:')
      print(sum(is.na(xs))/length(xs))
    }
  }

  #2.-----Remove unrealistic locations if they aren't near other points-----
  if(remove_isolated_points){

    if(verbose){
      print('removing unrealistic isolated points')
    }

    for(i in 1:n_inds){

      xi <- xi_new <- xs[i,]
      yi <- yi_new <- ys[i,]
      non_nas <- which(!is.na(xi))

      max_x_i <- quantile(xi, max_dist_percentile, na.rm=T)
      max_y_i <- quantile(yi, max_dist_percentile, na.rm=T)
      min_x_i <- quantile(xi, 1-max_dist_percentile, na.rm=T)
      min_y_i <- quantile(yi, 1-max_dist_percentile, na.rm=T)

      #get very large or very small values of x and y (outside their normal range)
      bigs <- unique(c(which(xi > max_x_i), which(yi > max_y_i)))
      smalls <- unique(c(which(xi < min_x_i), which(yi < min_y_i)))
      extremes <- unique(c(bigs,smalls))

      if(verbose){
        print(paste('number of extreme points found for individual ',i, '= ', length(extremes)))
      }

      #for each extreme value, find the previous and next non-NA data point
      #if this previous point is more than max_isolated_point_dist away, just replace the unrealistic location with NA
      #otherwise, leave it
      for(j in 1:length(extremes)){
        t_idx <- extremes[j]

        #if prev_t or next_t are not available (usually because you've hit the end of the contiguous chunk, skip and don't make any changes)
        if(length(which(non_nas < t_idx))==0){
          next
        }
        if(length(which(non_nas > t_idx))==0){
          next
        }

        prev_t <- max(non_nas[which(non_nas < t_idx)])
        next_t <- min(non_nas[which(non_nas > t_idx)])

        dist_prev <- sqrt( (xs[i, prev_t] - xs[i, t_idx])^2 + (ys[i, prev_t] - ys[i, t_idx])^2 )
        dist_next <- sqrt( (xs[i, next_t] - xs[i, t_idx])^2 + (ys[i, next_t] - ys[i, t_idx])^2 )

        #if the distance is not defined, skip and don't change anything
        if(is.na(dist_prev) | is.na(dist_next)){
          next
        }

        if(dist_prev > max_isolated_point_dist & dist_next > max_isolated_point_dist){
          if(verbose)
            print(paste('found unrealistic distance at time:',t_idx,'dist_prev = ',dist_prev,'dist_next = ',dist_next,'dt1=',(t_idx-prev_t),'dt2=',(next_t-prev_t)))

          xi_new[t_idx] <- NA
          yi_new[t_idx] <- NA
        }

      }

      xs[i,] <- xi_new
      ys[i,] <- yi_new

    }
  }

  #3. Remove very unrealistic locations (beyond a maximal standard deviation from the distribution) no matter what
  if(remove_unrealistic_locations){

    if(verbose){
      print('removing very unrealistic locations beyond a max standard deviation')
    }

    #Find and remove super unrealistic locations (>mean + max_sd_away*sd for each ind in x or y)
    for(i in 1:n_inds){

      xi <- xs[i,]
      yi <- ys[i,]
      extremes_high_x <- which(xi > mean(xi,na.rm=T) + max_sd_away*sd(xi,na.rm=T))
      extremes_low_x <- which(xi < mean(xi,na.rm=T) - max_sd_away*sd(xi,na.rm=T))
      extremes_high_y <- which(yi > mean(yi,na.rm=T) + max_sd_away*sd(yi,na.rm=T))
      extremes_low_y <- which(yi < mean(yi,na.rm=T)- max_sd_away*sd(yi,na.rm=T))
      extremes <- unique(c(extremes_high_x, extremes_high_y, extremes_low_x, extremes_low_y))

      if(length(extremes)>0){
        xs[i, extremes] <- NA
        ys[i, extremes] <- NA

        if(verbose){
          print(paste0('for individual ',i,' found ', length(extremes), ' extreme locations and replaced with NAs'))
        }
      }
    }
  }

  #4.--------Remove points outside of a specified bounding box, if a bounding box is entered---
  if(!is.null(bounding_box)){

    if(verbose){
      print('removing points outside of bounding box')
    }

    if(length(bounding_box) != 4){
      stop('bounding_box vector must be of length 4')
    }

    if(bounding_box[2] < bounding_box[1] | bounding_box[3] < bounding_box[4]){
      stop('bounding box order seems to be wrong - should be in order: min_easting, max_easting, min_northing, max_northing')
    }

    #find points outside of the bounding box
    idxs_outside_box <- which((xs < bounding_box[1]) | (xs > bounding_box[2]) | (ys < bounding_box[3]) | (ys > bounding_box[4]))

    #remove the points outside of the bounding box (replace with NAs)
    if(length(idxs_outside_box) > 0){
      xs[idxs_outside_box] <- NA
      ys[idxs_outside_box] <- NA
    }

  }


  #5./6.-----Interpolate small gaps and stationary periods-----

  if(interpolate_small_gaps){

    if(verbose){
      print('interpolating small gaps (and slightly longer stationary periods, if specified)')
    }

    #Interpolate through seqs of NAs of length < max_interp_len
    for(i in 1:n_inds){
      for(b in 1:n_breaks){

        #get time indexes associated with each continuous chunk
        tidxs <- seq(breaks_with_end[b]:(breaks_with_end[b+1]-1))

        #data for a single individual
        x <- xs[i,tidxs]
        y <- ys[i,tidxs]

        #vectors to hold interpolated data
        x_interp <- x
        y_interp <- y

        #get runs of NAs
        runs <- rle(is.na(x)) #get runs of NAs
        vals <- runs$values
        lens <- runs$lengths
        idxs <- c(0,cumsum(runs$lengths))

        #for each run of NAs, fill in with linearly interp values if length is less than max_interp_len
        for(j in 1:length(vals)){
          if(vals[j]==TRUE){
            first_idx <- idxs[j] + 1
            last_idx <- idxs[j+1]

            #If not too near the beginning or the end of the sequence...
            if((first_idx > 1) & (last_idx < length(x))){

              #Get values before and after NA sequence
              prev_val_x <- x[first_idx-1]
              next_val_x <- x[last_idx + 1]
              prev_val_y <- y[first_idx-1]
              next_val_y <- y[last_idx +1]

              #Fill in with linear interpolation if the NA sequence is short (< max_interp_len)
              if(lens[j] <= max_interp_len){
                interp_vals_x <- seq(prev_val_x, next_val_x, length.out = lens[j] + 2)
                interp_vals_y <- seq(prev_val_y, next_val_y, length.out = lens[j] + 2)
                x_interp[seq(first_idx - 1, last_idx + 1)] <- interp_vals_x
                y_interp[seq(first_idx - 1,last_idx + 1)] <- interp_vals_y
              }

              if(interpolate_stationary_periods){

                #Otherwise...if less than 5 minutes gap...
                if((lens[j] > max_interp_len) & (lens[j] < max_move_time)){

                  #Fill in with mean value at start and end if they are close enough ( <= max_move_dist)
                  dist_moved <- sqrt((next_val_x - prev_val_x)^2 + (next_val_y - prev_val_y)^2)
                  time_elapsed <- last_idx - first_idx
                  if(dist_moved < max_move_dist){
                    mean_x <- mean(c(next_val_x, prev_val_x))
                    mean_y <- mean(c(next_val_y, prev_val_y))
                    x_interp[seq(first_idx, last_idx)] <- mean_x
                    y_interp[seq(first_idx, last_idx)] <- mean_y
                  }
                }
              }
            }
          }
        }

        xs[i,tidxs] <- x_interp
        ys[i,tidxs] <- y_interp

      }
    }

    if(verbose){
      print('after interpolating:')
      print(sum(is.na(xs))/length(xs))
    }
  }

  #save output
  out_list <- c('xs','ys','timestamps')
  if(!is.null(ids)){
    out_list <- c(out_list, 'ids')
  }
  if(include_breaks){
    out_list <- c(out_list, 'breaks')
  }
  save(file = output_file_path, list = out_list)

  #return output
  out <- list()
  out$xs <- xs
  out$ys <- ys
  out$timestamps <- timestamps
  if(!is.null(ids)){
    out$ids <- ids
  }

  if(include_breaks){
    out$breaks <- breaks
  }

  return(out)

}
