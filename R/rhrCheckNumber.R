##' Validate numeric input
##'
##' Takes a numeric input and validates it with regard to length, range and missing values
##' @title rhrCheckNumber
##' @param val numeric vector
##' @param arg character, name of argument
##' @param from lower boundary
##' @param to upper boundary
##' @param length max length
##' @param canHaveNA logical, can the vector contain NA's?
##' @param fun name of the function
##' @export
rhrCheckNumber <- function(val, arg, length=1, from=NA, to=NA, canHaveNA=FALSE, fun=as.character(as.list(match.call())[[1]])) {

  ## Did we receive a number?
  if (mode(val) != "numeric") {
    stop(paste0(fun, ": expected ", arg, " to be numeric not ", mode(val)))
  }

  ## Check for NA's
  if (!canHaveNA) {
    if (any(is.na(val))) {
      stop(paste0(fun, ": ", arg, " can not contain NA's"))
    }
  }

  ## check if range is ok
  ## check for lower
  if (!is.na(from)) {
    if (any(val < from)) {
      stop(paste0(fun, ": ", arg, " is outside the acceptable range"))
    }
  }

  ## check for upper
  if (!is.na(to)) {
    if (any(val > to)) {
      stop(paste0(fun, ": ", arg, " is outside the acceptable range"))
    }
  }

  ## check length
  if (length(val) != length) {
    ## if longer, trim
    if (length(val) > length) {
      val <- val[1:length]
      warning(paste0(fun, ": ", arg, " should be of length ", length, ", discarded superfluous elements"))
    } else if (length(val) < length) {
      val <- rep(val, length)[1:length]
      warning(paste0(fun, ": ", arg, " should be of length ", length, ", recycled entries"))
    }
  }

  val
  
}
