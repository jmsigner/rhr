rhrCheckLevels <- function(levels) {
  ## Levels
  levels <- as.numeric(levels)

  if (mode(levels) != "numeric") {
    stop(paste0("rhrCheckLevels: levels should be of class numeric. The provided level is of class ", class(levels)))
  }

  ## Levels
  if (any(levels > 100) | any(levels < 1)) {
    stop(paste0("rhr: rhrCheckLevels: levels should be between 1 and 100. The current range is ", paste0(range(levels), collapse=" - ")))
  }

  ## NAs
  if (any(is.na(levels))) {
    stop(paste0("rhr: rhrCheckLevels: can not contain NAs"))
  }

  levels[order(levels)]
}
