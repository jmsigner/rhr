##' Schoener's ratio
##'
##' Function to calculate Schoeners V.
##'
##' This implementation uses the normal distribution as a sampling distribution. Relocations are ordered by timestamp and then relocations are sampled based on \code{interval}. If relocations are not equally spaced the nearest relocation forward is used.
##'
##' @param dat \code{data.frame} with 3 columns. The first column contains x coordinates, the second column contains y coordinates and the third column contains a timestamp as \code{POSIXct}. 
##' @param interval Numeric scalar, the interval in seconds.
##' @param alpha Numeric scalar, alpha value used to calculate the critical value.
##' @param minM Numeric scalar, the minimum number of pairs required, if m is smaller than this argument it will return \code{NA}.
##' @param consec Locagical scalar, indicates whether or not the observations are consecutive or not.
##' @return \code{vector} vector of length six. 
##' \itemize{
##'  \item{"V"}{Schoeners V}
##'  \item{"m"}{Number of pairs used}
##'  \item{"r2"}{Mean squared distance from the center of activity}
##'  \item{"t2"}{Mean squared distance between relocations}
##'  \item{"cv"}{Critical value}
##'  \item{"m"}{The time interval in seconds}
##' }
##' @useDynLib rhr
##' @export
##' @references Swihart, R. and Slade N. 1985, Testing for indpendence of observations in animal movement, _Ecology_, 66(4), 1176 - 1184
##' @example inst/examples/exrhrSchoener.R

rhrSchoener <- function(dat, interval, alpha=0.25, minM=10, consec=TRUE) {

  if (ncol(dat) < 3) {
    stop("rhrSchoener: dat: three columns are required")
  }

  ## User may not provide fixes in the right order, reorder them
  dat <- dat[order(dat[, 3]), ]

  alpha <- rhrCheckNumber(alpha, "alpha", from=0, to=1)

  if (any(!complete.cases(dat))) {
    dat <- dat[complete.cases(dat), ]
    warning("In rhrSchoener: removed NA")
  }

  if (any(duplicated(dat[,3]))) {
    dat <- dat[!duplicated(dat[,3]), ]
    warning("In rhrSchoener: removed duplicates")
  }

  which <- rhrBaseIntervalSubset(as.numeric(dat[,3]), interval)

  dat <- dat[as.logical(which),]
  m <- nrow(dat) - 1

  if (m < minM) {
    warning(paste0("m smaller than ", minM))
    return(c(V=NA, m=m, r2=NA, t2=NA, cv=NA, interval=NA))
  }

  t2 <- 1/m * (sum((dat[1:(nrow(dat) - 1), 1] - dat[2:nrow(dat), 1])^2) +
               sum((dat[1:(nrow(dat) - 1), 2] - dat[2:nrow(dat), 2])^2))
  r2 <- rhrBaseMSD(dat[,1], dat[,2])
  V <- t2/r2
  
  ## Eccentricity (as defined in Swihart and Slade 1985, p. 1177)
  ecc <- sqrt(Reduce("/", eigen(cov(dat[, 1:2]))$values))
  
  ## Obtain critical value
  if (consec) {
    s <- exp(-0.0502 + 0.164 * ecc - 0.0156 * ecc^2 - 0.437 * log(m))
  } else {
    s <- exp(-0.0679 + 0.179 * ecc - 0.0169 * ecc^2 - 0.471 * log(m))
  }
  cv <- 2 - (qnorm(1 - alpha) * s)

  return(c(V=V, m=m, r2=r2, t2=t2, cv=cv, interval=interval))
}

