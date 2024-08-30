#' Compare recorded meerkat GPS data to recording intervals noted in metadata
#'
#' For a given round of meerkat collaring, verifies whether the GPS data that is
#' expected to be present (based on `GROUPYEAR_MOV_SUMMARY.txt` file in METADATA) is present in
#' `xs` and `ys` matrices from `GROUPYEAR_xy_level0.RData`.
#'
#' @author Ariana Strandburg-Peshkin
#' @author NOT YET CODE REVIEWED
#'
#' @param xy_file path to file with the xy level 0 data
#' @param metadata_file path to corresponding metadata file
#'
#' @returns Returns 0 if the metadata and xy data match. Returns 1 otherwise and prints any discrepancies.
#' @export
meerkat_compare_gps_data_to_recording_intervals <- function(xy_file, metadata_file){

  #get xy data
  load(xy_file)

  #get individuals and dates from xy data
  inds_xy <- ids$code
  timestamps_dates <- as.Date(timestamps)
  dates_xy <- unique(timestamps_dates)

  #get metadata
  metadata <- read.delim(metadata_file, header=T, sep = '\t')

  #get individuals and dates from metadata
  inds_metadata <- metadata$code
  dates_metadata <- colnames(metadata)
  dates_metadata <- dates_metadata[-1]
  dates_metadata <- gsub('X','', dates_metadata)
  dates_metadata <- as.character(as.Date(dates_metadata, format = '%Y%m%d'))

  #rename rows and columns of metadata matrix with ind id codes and formatted dates
  metadata <- metadata[,-1]
  colnames(metadata) <- dates_metadata
  row.names(metadata) <- inds_metadata

  #check whether inds and dates in xy are found in metadata - otherwise throw error
  if(!all(inds_xy %in% inds_metadata)){
    warning('some individuals in xy data are not found in metadata - checking matching inds only')
  }
  if(!all(dates_xy %in% dates_metadata)){
    warning('some dates in xy data are not found in metadata - checking matching dates only')
  }

  #get list of individuals and dates to check
  inds <- intersect(inds_xy, inds_metadata)
  dates <- intersect(as.character(dates_xy), as.character(dates_metadata))

  #subset metadata to contain only relevant individuals and dates
  metadata <- metadata[which(row.names(metadata) %in% inds),]
  metadata <- metadata[,which(colnames(metadata) %in% c('code',dates))]

  #convert metadata to a matrix
  metadata <- as.matrix(metadata)

  #for xy data, compute % of GPS fixes for each individual on each date
  xy_data <- matrix(NA, nrow = length(inds), ncol = length(dates))
  row.names(xy_data) <- inds
  colnames(xy_data) <- dates
  for(i in 1:length(dates)){
    date_curr <- dates[i]
    time_idxs <- which(timestamps_dates == date_curr)
    frac_tracked <- rowMeans(!is.na(xs[,time_idxs]))
    xy_data[,i] <- frac_tracked
  }

  #convert metadata matrix to T / F (or NA) matrix
  metadata_tf <- matrix(NA, nrow = nrow(metadata), ncol = ncol(metadata))
  row.names(metadata_tf) <- row.names(metadata)
  colnames(metadata_tf) <- colnames(metadata)
  metadata_tf[grep('Record', metadata)] <- T
  metadata_tf[grep('No', metadata)] <- F
  metadata_tf[grep('Scan', metadata)] <- F
  metadata_tf[grep('Absent', metadata)] <- NA

  #convert xy matrix to T / F
  xy_tf <- as.matrix(xy_data > 0)

  #compare the two matrices and find any mismatches
  metadata_xy_match <- xy_tf == metadata_tf

  #if they match, all good - return 0
  if(sum(!metadata_xy_match, na.rm=T) == 0){
    print('metadata and xy data match')
    return(0)
  }

  #if there are mismatches, attribute them
  if(sum(!metadata_xy_match, na.rm=T) != 0){
    for(i in 1:length(inds)){
      for(j in 1:length(dates)){
        if(is.na(metadata_tf[i,j])){
          next
        }

        if(xy_tf[i,j] & !metadata_tf[i,j]){
          print(paste('ind',inds[i],'on date',dates[j],'has', round(xy_data[i,j]*100, 3) ,'% data but metadata lists it as not recorded'))
        }
        if(!xy_tf[i,j] & metadata_tf[i,j]){
          print(paste('ind',inds[i],'on date',dates[j],'is missing data that should be present according to metadata'))
        }

      }
    }


  }

  return(1)




}
