################################################################################
# Some routines to ease the use of R. Probably redundant with other libraries...
#
# BLB 2016, 2017
#
################################################################################

#-------------------------------------------------------------------------------
# User input
#-------------------------------------------------------------------------------
readInteger <- function(call)
{
  n <- readline(prompt = call)
  n <- as.integer(n)
  if (is.na(n)) {
    n <- readinteger()
  }
  return(n)
}

readString <- function(call)
{
  n <- readline(prompt = call)
  return(n)
}


#-------------------------------------------------------------------------------
# To stop quietly a code... Used instead of stop() that throw and error.
#-------------------------------------------------------------------------------
stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
} # stopQuietly()