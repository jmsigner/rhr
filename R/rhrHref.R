##' Calcualte reference bandwidth
##'
##' Function to estimate 2 dimensional reference bandwidth for kernel density estimation
##'
##' @param xy data.frame with two columns: x and y coordinates
##' @param rescale character, indicating if and if how data are rescaled. \code{unitvar} rescales x and y coordinates to unit variance, \code{xvar} rescales x and y coordinate to variance of x and \code{none} uses the raw data.

##' @details Function to calcualte reference bandwidth. This implementation is based on Worton (1989). If variances differ greatly, it is advaisable to rescale the data using \code{rescale="unitvar"}. If the data is suspected to multimodal other bandwidth estimation methods may be more suitable.

##' Seaman and Powell (1996) modified the method slighty by calculating the reference bandwidth for each dimension independently. The same result is obtained if \code{rescale=TRUE}.

##' If rescaling is performed with unit variance both methods will lead to the same result.

##' @return \code{list} of length two with the calculated bandwidth and the standardization method. 
##' @export
##' @references Seaman, D. E., & Powell, R. A. (1996). An evaluation of the accuracy of kernel density estimators for home range analysis. _Ecology, 77(7)_, 2075-2085.
##' @references Worton, B. J. (1989). Kernel methods for estimating the utilization distribution in home-range studies. _Ecology, 70(1)_, 164-168.
##' 
##' @author Johannes Signer 
##' @examples
##' data(datSH)
##' rhrHref(datSH[, 2:3])
rhrHref <- function(xy, rescale="none") {

  ## Some input validation
  xy <- rhrCheckData(xy, returnSP=FALSE)

  
  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHref: scale: not one of unitvar, xvar or none")
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


  n <- nrow(xy)

  h <- sqrt(0.5 * (var(xs) +  var(ys))) * n^(-1/6)
  h <- c(h, h)

  if (rescale == "unitvar") {
    h <- h * c(sd(xy[, 1]), sd(xy[, 2]))
  }   
  res <- list(h=h, rescale=rescale)
  res
}
