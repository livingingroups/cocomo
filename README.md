# cocomo library

This library contains functions used by the Communication and Collective Movement (CoCoMo) group and others in the Ecology of Animal Societies Department at the Max Planck Institute of Animal Behavior to analyze simultaneous tracking data from animal groups. The package is maintained by Ariana Strandburg-Peshkin. Functions have been code reviewed by at least one other person unless otherwise specified in the documentation under "Authors" (at the time of writing, most functions have not yet been code reviewed). 

**If you would like to contribute to this package by code revieweing any of the functions, or by adding your own functions, please contact Ariana Strandburg-Peshkin.**

The library is built for datasets that have a standardized "matrix format", containing the following objects:

**xs**: matrix of x coordinates (UTM eastings) of all individuals in a group or population (rows) at every time point (columns)
xs[i,t] gives the x / easting position of individual i at time point t

**ys**: matrix of y coordinates (UTM northings) of all individuals in a group or population (rows) at every time point (columns)
ys[i,t] gives the y / northing position of individual i at time point t

**timestamps**: vector of timestamps corresponding to the columns of xs and ys matrices. Timestamps must be uniformly sampled, though it is possible to have gaps (e.g. between different days of recording)

**ids**: data frame giving information about the tracked individuals, with rows correpsonding to the rows of the xs and ys matrices. The columns contained are flexible.

**This package is currently in early development, so accuracy is definitely not guaranteed - use with caution!**
