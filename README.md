# cocomo library

## Description

This library contains functions used by the Communication and Collective Movement (CoCoMo) group and others in the Department for the Ecology of Animal Societies at the Max Planck Institute of Animal Behavior to analyze simultaneous tracking data from animal groups. The package is maintained by Ari Strandburg-Peshkin. 

**This package is currently in early development, so accuracy is definitely not guaranteed - use with caution!**

## Installation

You can install the package by first installing the `devtools` library, then running:

`library(devtools)`

`install_github('livingingroups/cocomo')`

## Dataset structure

The library is built for movement (typically GPS) datasets that have a standardized "matrix format", containing the following objects:

`xs`: matrix of x coordinates (UTM eastings) of all individuals in a group or population (rows) at every time point (columns)
xs[i,t] gives the x / easting position of individual i at time point t

`ys`: matrix of y coordinates (UTM northings) of all individuals in a group or population (rows) at every time point (columns)
ys[i,t] gives the y / northing position of individual i at time point t

`timestamps`: vector of timestamps corresponding to the columns of xs and ys matrices. Timestamps must be uniformly sampled, though it is possible to have gaps (e.g. between different days of recording)

`ids`: data frame giving information about the tracked individuals, with rows correpsonding to the rows of the xs and ys matrices. There must be one column called `id_code` which contains a unique individual identifier for each animal (e.g. for meerkats: `'VCVM001'`, for hyenas: `'WRTH'`, for coatis: `'Luna'`) The other columns contained are flexible, and can include information on age, sex, dominance, etc.

Some functions also take in time series of calls. These should be formatted as a data frame that includes, at a minimum, the following columns:

`ind_idx`: index of the individual making the call (numeric)

`time_idx`: time index closest to the time at which the call was produced

`call_type`: string specifying the call type label

`time`: exact time at which the call was produced (synched to UTC time, including fractions of a second)

Note that times should generally be in UTC, however most functions would also work with local time. The user should verify that the same time zone is used for both GPS and audio data.

## Specific datasets

While most functions in this package are intended to be general, it also contains some functions used for processing data from specific species datasets maintained by the CoCoMo group / EAS department. These functions may also interface with the file structure on the EAS data server. All functions that are species-specific begin with the species name, e.g. `meerkat_import_GPS_data`. In addition, there are some species-specific scripts in the scripts folder.

## Code review

Our eventual goal is to have all functions in the package code reviewed by at least one other person than the person who wrote the code. Code reviewers are credited in the function documentation under "Author(s)" (they are indicated as code reviewers). Functions that have not yet been code reviewed specify "NOT YET CODE REVIEWED" under "Author(s)" in the documentation. At the time of writing, most functions have no yet been code reviewed. **If you would like to contribute to this package by code revieweing any of the functions, please contact Ari.**

## Contributing

If you would like to contribute code to the library, please submit a pull request, or contact Ari. 

## Licensing

This package is distributed under a GNU GPLv3 license. Basically, you can do almost anything you want with it, except distribute closed-source versions. See COPYING.txt for the full license.

## Attribution

Please cite this package if you use it:

`cite('cocomo')`

## Style guidelines

To facilitate code review and consistency within the package, we have a few minimal code style guidelines. Some are required, and some are only recommended.

### Required

- Code must be indented according to standard practice (e.g. code within a `for` loop should be indented)
- Code must be commented such that a reader can tell what it is doing
- Functions must contain documentation in Roxygen format (see existing functions for examples)
- Each function must indicate a primary author, as well as a code reviewer (or, if no code reviewer, should indicate NOT YET CODE REVIEWED)
- Variable and function names should use underscores_between_words rather than periods.between.words or CamelCase.
- When using functions from other packages, use `@importFrom` at the top to import specific functions rather than the whole package
- When using functions from other packages, call them in the code using the package name, e.g. `cocomo::get_group_heading` rather than just `get_group_heading`

### Recommended

- Function names should start with a verb (e.g. `get_group_heading`) to distinguish them from variables
- Where possible, avoid using lots of external packages
- Avoid tidyverse if possible (sorry, no offense intended!). Edit: I have just been informed that stringr and lubridate are part of tidyverse, so apparently I am not following this recommendation too closely.
- We generally use the US rather than the British spellings of words
