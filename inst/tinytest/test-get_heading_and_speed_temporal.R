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

# constants
c1 <- atan2(1,2)/pi # Plouffe's gamma-constant
c2 <- 1/2 + c1
c3 <- 1 - c1

x       <-      c(  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,   4,   5,   6,   6,   6,   6,   6,   7,   8,   9,  10)
y       <-      c(  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,   1,   2,   3,   4,   5,   6,   7,   8,   8,   8,   8,   7,   6,   7,   8,   9,  10,  10,  10,  10,  10)
speed_1 <- sqrt(c(  2,   2,   2,   2,   2,   1,   1,   1,   2,   2,   1,   1,   1,   2,   2,   2,   2,   1,   1,   0,   2,   2,   1,   1,   1,   1,   1,   1,   1,   1,  NA))
speed_2 <- sqrt(c(  8,   4,   4,   8,   5,   4,   4,   5,   8,   1,   4,   4,   5,   8,   8,   8,   5,   4,   1,   2,   8,   1,   4,   4,   4,   2,   4,   4,   4,  NA,  NA))/2
h_1     <-   pi*c(1/4, 1/4,-1/4, 1/4, 1/4,   0,   0,   0,-1/4,-1/4, 1/2, 1/2, 1/2, 3/4, 3/4, 3/4, 3/4,   1,   1, NaN,-1/4,-1/4, 1/2, 1/2, 1/2, 1/2,   0,   0,   0,   0,  NA)
h_2     <-   pi*c(1/4,   0,   0, 1/4,  c1,   0,   0,-c1, -1/4,   0, 1/2, 1/2,  c2, 3/4, 3/4, 3/4,  c3,   1,   1,-1/4,-1/4,   0, 1/2, 1/2, 1/2, 1/4,   0,   0,   0,   NA, NA)


len <- length(x)

# Incorrect argument type
expect_error(get_heading_and_speed_temporal(x, y, F))

# default t_window
forward <- get_heading_and_speed_temporal(x, y)
expect_equal(h_1, forward$heads)
expect_equal(speed_1, forward$speeds)

backward <- get_heading_and_speed_temporal(x, y,  forward = F)
expect_equal(c(NA, h_1[1:(len-1)]), backward$heads)
expect_equal(c(NA, speed_1[1:(len-1)]), backward$speeds)


# t_window = 2
t_window_2 <- get_heading_and_speed_temporal(x, y, 2)
expect_equal(speed_2, t_window_2$speeds)
expect_equal(h_2, t_window_2$heads)