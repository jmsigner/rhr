##' Select a bandwidth for Kernel Density Estimation
##'
##' Use two dimensional reference bandwidth to select a bandwidth for kernel density estimation. 
##' Find the smallest value for bandwidth (h) that results in n polygons
##' (usually n=1) contiguous polygons at a given level.
##'
##' This implementation uses a bisection algorithm to the find the smallest value
##' value for the kernel bandwidth within \code{range} that produces an home-range
##' isopleth at \code{level} consisting of \code{n} polygons. Note, no difference is
##' is made between the two dimensions. 
##'
##' @template xy 
##' @template trast 
##' @param range Numeric vector, indicating the lower and upper bound of the search range. If \code{range} is to large with regard to \code{trast}, the algorithm will fail.
##' @param numOfParts Numeric numeric scalar, indicating the number of contiguous  polygons desired. This will usually be one.
##' @template levels
##' @param tol Numeric scalar, indicating which difference of to stop.
##' @param maxIt Numeric scalar, indicating the maximum number of acceptable iterations. 
##' @return \code{list} with the calculated bandwidth, exit status and the number of iteration.
##' @export
##' @references Kie, John G. "A rule-based ad hoc method for selecting a bandwidth in kernel home-range analyses." Animal Biotelemetry 1.1 (2013): 1-12. 
##' 

rhrHrefScaled <- function(xy,
                          range=rhrHref(xy)$h * c(0.01, 2), 
                          trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL),
                          numOfParts=1, levels=95,
                          tol=0.1,
                          maxIt=500) {

  ## Input checks
  if (!is.numeric(range)) {
    stop("rhrHrefScaled: numeric should be numeric")
  }

  xy <- rhrCheckData(xy, returnSP=FALSE)

  if (!is(trast, "RasterLayer")) {
    stop("rhrHrefScaled: trast should be a RasterLayer")
  }

  levels <- rhrCheckLevels(levels)

  if (length(levels) > 1) {
    levels <- levels[1]
    warning("rhrHrefScaled: only first element of levels was used")
  }
    


  hmin <-min(range)
  hmax <- max(range)
  hcur <- mean(c(hmin, hmax))
  success <- FALSE

  for (i in 1:maxIt) {
    if (i > 1) {
      hcur <- hnew
    }
    tmpEst <- rhrIsopleths(rhrKDE(xy, h=hcur, trast=trast, levels=levels))
    allPolys <- slot(slot(tmpEst, "polygons")[[1]], "Polygons")
    nHoles <- sum(!sapply(allPolys, slot, "hole"))

    if (nHoles <= numOfParts) {
      ## decrease h
      hmax <- hcur
    } else {
      ## increase h
      hmin <- hcur
    }
    hnew <- mean(c(hmax, hmin))
    if (abs(hcur - hnew) <= tol && nHoles <= numOfParts) {
      success=TRUE
      break
    }
  }

  return(list(h=hcur, success=success, niter = i))

}
