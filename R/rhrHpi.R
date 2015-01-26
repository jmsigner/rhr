##' Select a bandwidth for Kernel Density Estimation
##'
##' Wraps \code{KernSmooth::dpik} to select bandwidth for kernel density estimation the plug-in-the-equation method in two dimensios.
##'
##' This function calcualtes bandwidths for kernel density estimation by wrapping \code{KernSmooth::dpik}. If \code{correct} is \code{TRUE}, the bandwidth is trasformed with power 5/6 to correct for using an univariate implementation for bivariate data (Gitzen et. al 2006).
##' @template xy 
##' @template rescale 
##' @param correct Logical scalar that indicates whether or not the estimate should be correct for the two dimensional case.
##' @param ... additional arguments passed to \code{KernSmooth::dpik}.
##' @return \code{list} with the calculated bandwidth, the standardization method and \code{correction}.
##' @seealso \code{KernSmooth::dpik}
##' @export
##' @references Gitzen, R. A., Millspaugh, J. J., & Kernohan, B. J. (2006). Bandwidth selection for fixed-kernel analysis of animal utilization distributions. _Journal of Wildlife Management_, 70(5), 1334-1344.
##' @example inst/examples/rhrHpi.R

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
