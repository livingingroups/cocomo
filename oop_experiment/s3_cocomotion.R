# Constructors - internal, no validation 
new_cocomotion <- function(x, y) {
  n_times <- length(x)
  structure(
    array(c(x,y), dim = c(1,n_times,2), dimnames = list(
      '', 
      names(x),
      c('x', 'y')
    )),
    class = c('cocomotion', 'array')
  )
}

new_cocomotions <- function(xs, ys) {
  N <- dim(xs)[1]
  n_times <- dim(xs)[2]
  structure(
    array(c(xs,ys), dim = c(N, n_times, 2), dimnames = list(
      dimnames(xs)[[1]],
      dimnames(xs)[[2]],
      c('x', 'y')
    )),
    class = c('cocomotions', 'array')
  )
}

# Helpers - To actually use in creation

cocomotion <- function(x, y, t_start, t_by = 1) {
  x <- checkmate::assert_numeric(x)
  n_times <- length(x)
  y <- checkmate::assert_numeric(x, len = n_times)

  t_start <- checkmate::assert_posixct(t_start)
  t_by <- checkmate::assert_number(t_by, lower = 0, finite = TRUE)
  ts <- seq(from = t_start, by = t_by, length.out = n_times)

  names(x) <- ts

  new_cocomotion(x, y)
}

cocomotions <- function(xs, ys, t_start, t_by = 1, individual_names = NULL) {
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

  dimnames(xs) <- list(
    individual_names,
    ts
  )
  new_cocomotions(xs, ys)
}

# Methods

# n_individuals method
n_individuals <- function(x) {
  UseMethod('n_individuals')
}
n_individuals.array <- function(x) {
  dim(x)[[1]]
}
# numeric that's *not* a matrix or array represents a single individual
n_individuals.numeric <- function(x) 1
# ^ similar idea for heading...

# plot method
plot.cocomotion <- function(x, ...){
  NextMethod(x[,,'x'], x[,,'y'], ...)
}
plot.cocomotions <- function(x, ...){
  NextMethod(x[,,'x'], x[,,'y'], type = 'l', ...)
}

# Example script

source('oop_experiment/helper-group_heading_test_data.R')

one_individual <- cocomotion(x_base, y_base, as.POSIXct(1))

group <- cocomotions(xs, ys, as.POSIXct(1))

#plot(one_individual)

#plot(group)

n_individuals(one_individual)

n_individuals(group)
