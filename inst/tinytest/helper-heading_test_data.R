
# Helper Functions ----

## Define helper functions ----

#' Shifts vector in either direction
#' @param v vector to shift
#' @param n number of indices to shift, can be negative
shift <- function(v, n=1) return(if (n>0) c(
    v[(length(v)-n+1):length(v)], v[1:(length(v)-n)]
  ) else c(
    v[(1-n):length(v)], v[1:-n]
  )
)

## Test Helper functions ----
expect_equal(shift(1:10, 2), c(9:10,1:8))
expect_equal(shift(1:10, -2), c(3:10,1:2))

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
c1 <- atan2(1,2)/pi # Plouffe's gamma-constant
c2 <- 1/2 + c1
c3 <- 1 - c1

### Inputs (x, y) ----
x       <-      c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y       <-      c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)

len <- length(x)

### Expected output (speed, headings, dt) ----

#### Temporal ----

# Temporal, t_window = 1
speed_1 <- sqrt(c(  2,   2,   2,   2,   2,   1,   1,   1,   2,   2,   1,   1,   1,   2,   2,   2,   2,   1,   1,   0,   2,   2,   1,   1,   1,   1,   1,   1,   1,   1,  NA))
h_1     <-   pi*c(1/4, 1/4,-1/4, 1/4, 1/4,   0,   0,   0,-1/4,-1/4, 1/2, 1/2, 1/2, 3/4, 3/4, 3/4, 3/4,   1,   1, NaN,-1/4,-1/4, 1/2, 1/2, 1/2, 1/2,   0,   0,   0,   0,  NA)

# Temporal, t_window = 2
speed_2 <- sqrt(c(  8,   4,   4,   8,   5,   4,   4,   5,   8,   1,   4,   4,   5,   8,   8,   8,   5,   4,   1,   2,   8,   1,   4,   4,   4,   2,   4,   4,   4,  NA,  NA))/2
h_2     <-   pi*c(1/4,   0,   0, 1/4,  c1,   0,   0,-c1, -1/4,   0, 1/2, 1/2,  c2, 3/4, 3/4, 3/4,  c3,   1,   1,-1/4,-1/4,   0, 1/2, 1/2, 1/2, 1/4,   0,   0,   0,   NA, NA)

#### Spatial ----

# constants
all_NA <- rep(NA_real_, len)

# Spatial, R = 1, forward = TRUE
# dt varies
dt_R1f  <-      c(  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   2,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,  NA)
# look up speed and headings  from temporal case based on dt
speed_R1f <- ifelse(
  dt_R1f == 1,
  speed_1,
  ifelse(
    dt_R1f == 2,
    speed_2,
    NA_integer_
  )
)

h_R1f <- ifelse(
  dt_R1f == 1,
  h_1,
  ifelse(
    dt_R1f == 2,
    h_2,
    NA_integer_
  )
)

# Spatial, R = 1, forward = FALSE
dt_R1b <- shift(dt_R1f)

speed_R1b <- ifelse(
  dt_R1b == 1,
  shift(speed_1),
  ifelse(
    dt_R1b == 2,
    shift(speed_2, 2),
    NA_integer_
  )
)

h_R1b <- ifelse(
  dt_R1b == 1,
  shift(h_1),
  ifelse(
    dt_R1b == 2,
    shift(h_2, 2),
      NA_integer_
  )
)

# Spatial, R = 1.5, forward = TRUE
dt_R1_5f<-      c(  2,   2,   2,   2,   2,   2,   2,   2,   2,   4,   2,   2,   2,   2,   2,   2,   2,   2,   4,   3,   2,   4,   2,   2,   2,   3,   2,   2,   2,  NA,  NA)

# for R = 1.5, most of dt are 2, so we can just pull those from  speed_2, heading_2
speed_R1_5f <- integer(len)
speed_R1_5f[-c(10,19,20,22,26)] <- speed_2[-c(10,19,20,22,26)]

h_R1_5f <- integer(len)
h_R1_5f[-c(10,19,20,22,26)] <- h_2[-c(10,19,20,22,26)]

# for at those indices where  dt is not 2, set speed and heading manually

speed_R1_5f[10] <- sqrt(5)/4
speed_R1_5f[19] <- sqrt(5)/4
speed_R1_5f[20] <- 2*sqrt(2)/3
speed_R1_5f[22] <- sqrt(5)/4
speed_R1_5f[26] <- sqrt(5)/3

h_R1_5f[10] <- atan2(2,1)
h_R1_5f[19] <- -atan2(2,1)
h_R1_5f[20] <- -pi/4
h_R1_5f[22] <- atan2(2,1)
h_R1_5f[26] <- atan2(1,2)

# Spatial, R = 1, t_indxs = c(5, 20)

t_idxs <- c(5, 20)
is_selected <- seq_len(len) %in% t_idxs

dt_t_idxs_set <- ifelse(
  is_selected,
  # hardcoded values for indexes 5, 20
  c(1, 2),
  NA_integer_
)

speed_t_idxs_set <- ifelse(
  is_selected,
  # hardcoded values for indexes 5, 20
  sqrt(c(2,.5)),
  NA_integer_
)

h_t_idxs_set <- ifelse(
  is_selected,
  # hardcoded values for indexes 5, 20
  pi*c(1/4, -1/4),
  NA_integer_
)