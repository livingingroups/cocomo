library(methods)
# Constructors - internal, no validation

setOldClass(c('array'))

setClass(
  "Cocomotion",
  contains = 'array'
)

setClass(
  "Cocomotions",
  contains = 'array',
  # slots = ...
)

# Helpers - To actually use in creation

Cocomotion <- function(x, y, t_start, t_by = 1) {
  x <- checkmate::assert_numeric(x)
  n_times <- length(x)
  y <- checkmate::assert_numeric(x, len = n_times)

  t_start <- checkmate::assert_posixct(t_start)
  t_by <- checkmate::assert_number(t_by, lower = 0, finite = TRUE)
  ts <- seq(from = t_start, by = t_by, length.out = n_times)

  new(
    'Cocomotion',
    array(c(x,y), dim = c(1,n_times,2), dimnames = list(
      '',
      ts,
      c('x', 'y')
    ))
  )
}

Cocomotions <- function(xs, ys, t_start, t_by = 1, individual_names = NULL) {
  xs <- checkmate::assert_matrix(xs, mode = 'numeric')
  n_times <- ncol(xs)
  ys <- checkmate::assert_matrix(ys, mode = 'numeric', nrows = nrow(xs), ncols = ncol(xs))

  t_start <- checkmate::assert_posixct(t_start)
  t_by <- checkmate::assert_number(t_by, lower = 0, finite = TRUE)
  ts <- seq(from = t_start, by = t_by, length.out = n_times)

  if (!is.null(individual_names)) {
    individual_names <- as.character(seq(1, length(x)))
  } else if (!is.null(dimnames(xs)[[1]])) {
    individual_names <- dimnames(xs)[[1]]
  } else if (!is.null(dimnames(ys)[[1]])) {
    individual_names <- dimnames(ys)[[1]]
  } else {
    individual_names <- as.character(seq(to=nrow(xs)))
  }

  N <- dim(xs)[1]
  n_times <- dim(xs)[2]
  new(
    "Cocomotions",
    array(c(xs,ys), dim = c(N, n_times, 2), dimnames = list(
      individual_names,
      ts,
      c('x', 'y')
    ))
  )
}

# Methods

# n_individuals method
setGeneric("n_individuals", function(x) standardGeneric("n_individuals"))

setMethod('n_individuals', 'array', function(x) dim(x)[[1]])
setMethod('n_individuals', 'numeric', function(x) 1)

# ^ similar idea for x, y, heading...

# plot method
plot.Cocomotion <- function(x, ...){
  NextMethod(x[,,'x'], x[,,'y'], ...)
}
plot.Cocomotions <- function(x, ...){
  NextMethod(x[,,'x'], x[,,'y'], type = 'l', ...)
}

# Example script

source('oop_experiment/helper-group_heading_test_data.R')

one_individual <- Cocomotion(x_base, y_base, as.POSIXct(1))

group <- Cocomotions(xs, ys, as.POSIXct(1))

plot(one_individual)

plot(group)

n_individuals(one_individual)

n_individuals(group)
