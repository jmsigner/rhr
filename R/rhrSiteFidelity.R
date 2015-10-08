#' Test for site fidelity of animal movement.
#'
#' @template trackS
#' @param n Numeric scalar. The number of simulated trajectories.
#' @param alpha Numeric scalar. The alpha value used for the bootstrapping.
#' @useDynLib rhr
#' @importFrom Rcpp sourceCpp
#' @export
#' @return object of class \code{RhrFidelity}, which is a list of length 4. \code{msd.dat} and \code{li.dat} is the mean square distance and linearity for the real date. \code{msd.sim} and \code{li.sim} are the mean square distances and linearities for the simulated trajectories. 
#' 
#' @references Spencer, S. R., Cameron, G. N., & Swihart, R. K. (1990). Operationally defining home range: temporal dependence exhibited by hispid cotton rats. Ecology, 1817-1822.
#' @example inst/examples/ex-rhrSiteFidelity.R

rhrSiteFidelity <- function(track, n=100, alpha=0.05) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()
  
  ## function for later
  li <- function(x, y) {
    line.distance   <- sqrt((x[1] - x[length(x)])^2 + (y[1] - y[length(y)])^2)
    walked.distance <- sum(sqrt((x[-1] - x[-length(x)])^2 + (y[-1] - y[-length(y)])^2))
    return(line.distance / walked.distance)
  }

  ## --------------------------------------------------------------------------- #
  ## Some argument checking
  dat <- rhrCheckData(track, returnSP=FALSE)

  n <- rhrCheckNumber(n, "n", from=1)
  alpha <- rhrCheckNumber(alpha, "alpha", from=0, to=1)

  x <- dat[, 1]
  y <- dat[, 2]
  
  ## simulate n random walks
  a <- replicate(n, rhrBasePRW(x, y), simplify=FALSE)

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

#' @export
plot.RhrSiteFidelity <- function(x, plotit=TRUE, ...) {
  p1 <- ggplot2::ggplot(data.frame(x=x$msdSim), ggplot2::aes(x=x)) + ggplot2::geom_histogram() +
    ggplot2::expand_limits(x=range(c(x$msdSim, x$msdDat))) +
    ggplot2::geom_vline(data=data.frame(x=x$msdCI), ggplot2::aes(xintercept=x), linetype=2, colour="red") +
    ggplot2::geom_vline(data=data.frame(x=x$msdDat), ggplot2::aes(xintercept=x), colour="red", size=1.5) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x="Mean Squared Distance from Center of Activity", y="count")

  p2 <- ggplot2::ggplot(data.frame(x=x$liSim), ggplot2::aes(x=x)) + 
    ggplot2::geom_histogram() +
    ggplot2::expand_limits(x=range(c(x$liSim, x$liDat))) +
    ggplot2::geom_vline(data=data.frame(x=x$liCI), ggplot2::aes(xintercept=x), linetype=2, colour="red") +
    ggplot2::geom_vline(data=data.frame(x=x$liDat), ggplot2::aes(xintercept=x), colour="red", size=1.5) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x="Linearity Index", y="count")

  if (plotit) {
   gridExtra::grid.arrange(p1, p2, ncol=1)
 } else {
   gridExtra::arrangeGrob(p1, p2, ncol=1)
 }


}


 

