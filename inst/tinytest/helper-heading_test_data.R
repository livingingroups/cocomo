# Helper Functions ----

## Define helper functions ----

#' Shifts vector in either direction
#' @param v vector to shift
#' @param n number of indices to shift, can be negative
shift <- function(v, by = 1) {
  return(if (by > 0) {
    c(
      v[(length(v) - by + 1):length(v)], v[1:(length(v) - by)]
    )
  } else {
    c(
      v[(1 - by):length(v)], v[1:-by]
    )
  })
}


# Simple Test Scenario ----

#  Below is ascci version of the path used in test scenario

# 10     | ---
#  9     |
#  8   .--.
#  7   \ | \
#  6    \|  \
#  5         |
#  4         |
#  3    .--. |
#  2 /\/    \|
#  1/
#  012345678910

## Test input data ----

### Constants  ----
eps <- sqrt(.Machine$double.eps)
c1 <- atan2(1, 2) / pi # Plouffe's gamma-constant
c2 <- 1 / 2 + c1
c3 <- 1 - c1

### Inputs (x, y) ----
# nolint start: line_length_lintr
x       <-      c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y       <-      c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)

len <- length(x)

### Expected output (speed, headings, dt) ----

#### Temporal ----

# Temporal, t_window = 1
speed_1 <- sqrt(c(2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 0, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, NA))
h_1 <- pi * c(1 / 4, 1 / 4, -1 / 4, 1 / 4, 1 / 4, 0, 0, 0, -1 / 4, -1 / 4, 1 / 2, 1 / 2, 1 / 2, 3 / 4, 3 / 4, 3 / 4, 3 / 4, 1, 1, NaN, -1 / 4, -1 / 4, 1 / 2, 1 / 2, 1 / 2, 1 / 2, 0, 0, 0, 0, NA)

# Temporal, t_window = 2
speed_2 <- sqrt(c(8, 4, 4, 8, 5, 4, 4, 5, 8, 1, 4, 4, 5, 8, 8, 8, 5, 4, 1, 2, 8, 1, 4, 4, 4, 2, 4, 4, 4, NA, NA)) / 2
h_2 <- pi * c(1 / 4, 0, 0, 1 / 4, c1, 0, 0, -c1, -1 / 4, 0, 1 / 2, 1 / 2, c2, 3 / 4, 3 / 4, 3 / 4, c3, 1, 1, -1 / 4, -1 / 4, 0, 1 / 2, 1 / 2, 1 / 2, 1 / 4, 0, 0, 0, NA, NA)

#### Spatial ----

expected_spatial <- list()

# constants
all_NA <- rep(NA_real_, len)

# Spatial, R = 1, forward = TRUE
# dt varies
dt <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA)
# nolint end


# look up speed and headings  from temporal case based on dt
expected_spatial[['R1, forward']] <- list(
  heads = ifelse(
    dt == 1,
    h_1,
    ifelse(
      dt == 2,
      h_2,
      NA_integer_
    )
  ),
  speeds = ifelse(
    dt == 1,
    speed_1,
    ifelse(
      dt == 2,
      speed_2,
      NA_integer_
    )
  ),
  dts = dt
)

# Spatial, R = 1, forward = FALSE
dt <- shift(dt)

expected_spatial[['R1, backward']] <- list(
  heads = ifelse(
    dt == 1,
    shift(h_1),
    ifelse(
      dt == 2,
      shift(h_2, 2),
      NA_integer_
    )
  ),
  speeds = ifelse(
    dt == 1,
    shift(speed_1),
    ifelse(
      dt == 2,
      shift(speed_2, 2),
      NA_integer_
    )
  ),
  dts = dt
)

# Spatial, R = 1.5, forward = TRUE

dt <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 2, 4, 3, 2, 4, 2, 2, 2, 3, 2, 2, 2, NA, NA)

# for R = 1.5, most of dt are 2, so we can just pull those from  speed_2, heading_2
speed <- integer(len)
speed[-c(10, 19, 20, 22, 26)] <- speed_2[-c(10, 19, 20, 22, 26)]

heading <- integer(len)
heading[-c(10, 19, 20, 22, 26)] <- h_2[-c(10, 19, 20, 22, 26)]


# for at those indices where  dt is not 2, set speed and heading manually

speed[10] <- sqrt(5) / 4
speed[19] <- sqrt(5) / 4
speed[20] <- 2 * sqrt(2) / 3
speed[22] <- sqrt(5) / 4
speed[26] <- sqrt(5) / 3

heading[10] <- atan2(2, 1)
heading[19] <- -atan2(2, 1)
heading[20] <- -pi / 4
heading[22] <- atan2(2, 1)
heading[26] <- atan2(1, 2)

expected_spatial[['R1.5']] <- list(
  heads = heading,
  speeds = speed,
  dts = dt
)

# Spatial, R = 1, t_indxs = c(5, 20)

t_idxs <- c(5, 20)
is_selected <- seq_len(len) %in% t_idxs

expected_spatial[['t_idxs set']] <- list(
  heads = ifelse(
    is_selected,
    # hardcoded values for indexes 5, 20
    pi * c(1 / 4, -1 / 4),
    NA_integer_
  ),
  speeds = ifelse(
    is_selected,
    # hardcoded values for indexes 5, 20
    sqrt(c(2, .5)),
    NA_integer_
  ),
  dts = ifelse(
    is_selected,
    # hardcoded values for indexes 5, 20
    c(1, 2),
    NA_integer_
  )
)