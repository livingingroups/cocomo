.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    'cocomo package contains both experimental and stable functions.',
    'Experimental functions have not been code reviewed and are likely to change in the future.'
  ))
}
