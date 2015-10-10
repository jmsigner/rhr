##' Select a bandwidth for Kernel Density Estimation
##'
##' Use two dimensional reference bandwidth to select a bandwidth for kernel density estimation. 
##'
##' This implementation is based on Worton (1989). If variances differ greatly, it is advaisable to rescale the data using \code{rescale="unitvar"}. If the data is suspected to multimodal other bandwidth estimation methods may be more suitable.
##'
##' @template xy 
##' @template rescale 
##' @return \code{list} with the calculated bandwidth and the standardization method. 
##' @export
##' @references Worton, B. J. (1989). Kernel methods for estimating the utilization distribution in home-range studies. _Ecology, 70(1)_, 164-168.
##' 

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
