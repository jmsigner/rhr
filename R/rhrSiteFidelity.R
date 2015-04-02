##' rhrSiteFidelity
##'
##' @param dat a data.frame with at least 2 columns. The first column contains the x-coordinates, the second column contains the y-coordinates 
##' @param n the number of simulated trajectories.
##' @param alpha Numeric vector of length one, giving the alpha value.
##' @useDynLib rhr
##' @export
##' @return object of class \code{RhrFidelity}, which is a list of length 4. \code{msd.dat} and \code{li.dat} is the mean square distance and linearity for the real date. \code{msd.sim} and \code{li.sim} are the mean square distances and linearities for the simulated trajectories. 
##' @examples
##' ## simulated data
##' set.seed(123)
##' dat <- data.frame(x=runif(1000, 0, 100), y=runif(1000, 0, 100))
##' rhrSiteFidelity(dat, n=500)
##'
##' ## Example Data
##' data(datSH)
##' res <- rhrSiteFidelity(datSH[, 2:3])

rhrSiteFidelity <- function(dat, n=100, alpha=0.05) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## --------------------------------------------------------------------------- #
  ## Some argument checking
  dat <- rhrCheckData(dat, returnSP=FALSE)

  n <- rhrCheckNumber(n, "n", from=1)
  alpha <- rhrCheckNumber(alpha, "alpha", from=0, to=1)

  x <- dat[, 1]
  y <- dat[, 2]
  
  ## simulate n random walks
  a <- replicate(n, rhrBase::rhrBasePRW(x, y), simplify=FALSE)

  ## msd 
  msdDat <- rhrBaseMSD(x, y)
  msdSim <- sapply(a, function(x) rhrBaseMSD(x[, 1], x[, 2]))

  ## li
  liDat <- li(x, y)
  liSim <- sapply(a, function(x) li(x[,1], x[,2]))

  ## CI
  msdCI <- quantile(msdSim, probs=c(alpha / 2, 1 - alpha / 2))
  liCI <- quantile(liSim, probs=c(alpha / 2, 1 - alpha / 2))
  

  res <- list(msdDat=msdDat, liDat=liDat, msdSim=msdSim, liSim=liSim,
              msdCI=msdCI, liCI=liCI)

  
  res <- structure(res, class=c("RhrSiteFidelity", "list"))
  attr(res, "alpha") <- alpha

  invisible(res)
}



########### Round digits
##' @export
## FIXME: round values
print.RhrSiteFidelity <- function(x, ...) {
  cat("* rhrSiteFidelity \n")
  cat("================= \n")
  cat(sprintf("MSD (data)       : %s\n", x$msdDat))
  cat(sprintf("LI (data)        : %s\n", x$liDat))
  cat(sprintf("CI simulated MSD : (%s - %s)\n", x$msdCI[1], x$msdCI[2]))
  cat(sprintf("CI simulated LI  : (%s - %s)\n", x$liCI[1], x$liCI[2]))

}

##' @export
##' @method plot RhrSiteFidelity
## FIXME: remove plotit
plot.RhrSiteFidelity <- function(x, plotit=TRUE, ...) {
  p1 <- ggplot(data.frame(x=x$msdSim), aes(x=x)) + geom_histogram() +
    expand_limits(x=range(c(x$msdSim, x$msdDat))) +
      geom_vline(data=data.frame(x=x$msdCI), aes(xintercept=x), linetype=2, colour="red") +
        geom_vline(data=data.frame(x=x$msdDat), aes(xintercept=x), colour="red", size=1.5) +
          theme_bw() + labs(x="Mean Squared Distance from Center of Activity", y="count")

  p2 <- ggplot(data.frame(x=x$liSim), aes(x=x)) + geom_histogram() +
    expand_limits(x=range(c(x$liSim, x$liDat))) +
      geom_vline(data=data.frame(x=x$liCI), aes(xintercept=x), linetype=2, colour="red") +
        geom_vline(data=data.frame(x=x$liDat), aes(xintercept=x), colour="red", size=1.5) +
          theme_bw() + labs(x="Linearity Index", y="count")

  if (plotit) {
   gridExtra::grid.arrange(p1, p2, ncol=1)
 } else {
   gridExtra::arrangeGrob(p1, p2, ncol=1)
 }


}


 

## ------------------------------------------------------------------------------ ##  
## Function to calc linearity

li <- function(x, y) {
  d               <- cumdist(x,y)
  line.distance   <- sqrt((x[1] - x[length(x)])^2 + (y[1] - y[length(y)])^2)
  walked.distance <- sum(d)
  return(line.distance / walked.distance)
}
