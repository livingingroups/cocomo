# skip
exit_file()

# Fails with unhelpful error message if no splits/merges
expect_silent(
  identify_splits_and_merges(
    matrix(
      c(1,1,1,1),
      2,
      2 
    ),
    matrix(
      c(1,1,1,1),
      2,
      2 
    ),
    as.POSIXct(1:2),
    .5,
    .5
  )$together
) 
# This also fails
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
         1,  1,  1,  1,
         0,  1,  1,  0
      ),
      2, 4, byrow=TRUE 
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4 
    ),
    as.POSIXct(1:4),
    .4,
    .6
  )$together[1,2,],
  c(FALSE, TRUE,TRUE,FALSE)
) 

# "Togetherness" not perojected forward correctly  
# when indviduals start together.
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
         1,  1,  1,
        .7, .5, .3
      ),
      2, 3, byrow=TRUE 
    ),
    matrix(
      c(1,1,1,1,1,1),
      2,
      3 
    ),
    as.POSIXct(1:3),
    .4,
    .6
  )$together[1,2,],
  c(TRUE,TRUE,FALSE)
) 

# Initial "apart" trampled
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
         1,   1,  1, 1,
        0, 1, 0,  0
      ),
      2, 4, byrow=TRUE 
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4 
    ),
    as.POSIXct(1:4),
    .5,
    .5
  )$together[1,2,]
  ,
  c(FALSE, TRUE, FALSE, FALSE)
) 

# Final "apart" trampled
expect_equal(
  identify_splits_and_merges(
    matrix(
      c(
         1,   1,  1,  1,
         0,   0,  1,  0
      ),
      2, 4, byrow=TRUE 
    ),
    matrix(
      c(1,1,1,1,1,1,1,1),
      2,
      4 
    ),
    as.POSIXct(1:4),
    .5,
    .5
  )$together[1,2,]
  ,
  c(FALSE, FALSE, TRUE, FALSE)
) 
