##' Calculate a Bandwidth for Kernel Density Estimation.
##'
##' Wraps \code{KernSmooth::dpik} to estimate bandwidth for kernel density estimation in the two dimensional case.
##'
##' @param xy data.frame with two columns: x and y coordinates
##' @param rescale character, indicating if and if how data are rescaled. \code{unitvar} rescales x and y coordinates to unit variance, \code{xvar} rescales x and y coordinate to variance of x and \code{none} uses the raw data.
##' @param correct logical value that indicates whether or not the estimate should be correct for the two dimensional case, see details.
##' @param ... additional arguments passed to \code{KernSmooth::dpik}.
#
##' @details This function calcualtes bandwidth values for kernel density estimation by wrapping \code{KernSmooth::dpik}. If \code{correct} is TRUE, the bandwidth is trasformed with power 5/6 to correct for using an univariate implementation for bivariate data (Gitzen et. al 2006).

##' @return \code{vector} of length two
##' @seealso \code{KernSmooth::dpik}
##' @export
##' @references Gitzen, R. A., Millspaugh, J. J., & Kernohan, B. J. (2006). Bandwidth selection for fixed-kernel analysis of animal utilization distributions. _Journal of Wildlife Management_, 70(5), 1334-1344.
##' 
##' @author Johannes Signer 
##' @examples
##' data(datSH)
##' rhrHpi(datSH[, 2:3])

rhrHpi <- function(xy, rescale="none", correct=TRUE, ...) {

  ## Some input validation
  xy <- rhrCheckData(xy, returnSP=FALSE)

  
  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHpi: scale: not one of unitvar, xvar or none")
  }

  xs <- xy[, 1]
  ys <- xy[, 2]

  if (rescale == "unitvar") {
    ## standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    ## standardize x and y by 
    ys <- (ys / sd(ys)) * sd(xs)
  } 

  hx <- KernSmooth::dpik(xs, ...)
  hy <- KernSmooth::dpik(ys, ...)

  h <- c(hx, hy)

  if (rescale == "unitvar") {
    h <- KernSmooth::dpik(xs, ...)
    h <- h * c(sd(xy[, 1]), sd(xy[, 2]))
  }   

  ## Gitzen et al. 2006 suggested that if bandwidth is estimated for each coordinate seperately, to correct it x^(5/6)
  if (correct) {
    h <- h^(5/6)
  } 

  res <- list(h=h, rescale=rescale, correct=correct)
  res
}
