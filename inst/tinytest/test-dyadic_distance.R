xs <- matrix(c(
  0, 0, 0, 0,
  1, 2, 3, 4,
  0, 0, 0, 0
), 3, 4, byrow = TRUE)

ys <- matrix(c(
  0, 0, 0, 0,
  0, 0, 0, 0,
  2, 4, 6, 8
), 3, 4, byrow = TRUE)

expect_equal(
   actual <- get_group_dyadic_distances(
    xs,
    ys
  ),
  expected <- array(c(
    NA, 1, 2,
    1, NA, sqrt(5),
    2, sqrt(5), NA,

    NA, 2, 4,
    2, NA, sqrt(20),
    4, sqrt(20), NA,

    NA, 3, 6,
    3, NA, sqrt(45),
    6, sqrt(45), NA,

    NA, 4, 8,
    4, NA, sqrt(80),
    8, sqrt(80), NA
  ), c(3, 3, 4))
)
