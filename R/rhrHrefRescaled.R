##' Select a Bandwidth for Kernel Density Estimation
##'
##' Find bandwidth that results in n polygons (usually n=1) for a Kernel Density Estimate.
##'
##' @param xy A numeric data.frame with columns: x and y coordinates.
##' @param range A numeric vector of length two, indicating the range of the search area. Be careful, if the search range is to large and trast to small, the algorithm will fail.
##' @param trast A raster, to act as a template for kernel density estimates. 
##' @param numOfParts A numeric value indicating the number of polygons desired. This will usually be one.
##' @param level A numeric value that specifies at which home range level the analysis should be performed.
##' @param tol A numeric value indicating when the algorithm stops.
##' @param maxIt A numeric value giving the maximum number of iterations acceptable.
##' @return list with the found bandwidth and whether or not the methods was successful.
##' @references Kie, John G. "A rule-based ad hoc method for selecting a bandwidth in kernel home-range analyses." Animal Biotelemetry 1.1 (2013): 1-12. 
##' @author Johannes Signer
##' @export
rhrHrefScaled <- function(xy,
                          range=rhrHref(xy)$h * c(0.01, 100), 
                          trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.4), nrow=100, res=NULL),
                          numOfParts=1, level=95,
                          tol=0.1,
                          maxIt=500) {

  ## Debug only
  if (FALSE) {
    data(datSH)
    xy <- datSH[, 2:3]
    range=range=do.call(seq, as.list(c(rhrHref(xy)$h * c(0.1, 2), length.out=100)))
    trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.4), nrow=100, res=NULL)
    numOfParts=1
    level=95
  }
  

  ## Input checks
  if (!is.numeric(range)) {
    stop("rhrHrefScaled: numeric should be numeric")
  }

  xy <- rhrCheckData(xy, returnSP=FALSE)

  if (!is(trast, "RasterLayer")) {
    stop("rhrHrefScaled: trast should be a RasterLayer")
  }


  ## We could use Newton-Raphson type of optimisation here

  hmin <-min(range)
  hmax <- max(range)
  hcur <- mean(c(hmin, hmax))
  success <- FALSE

  for (i in 1:maxIt) {
    if (i > 1) {
      hcur <- hnew
    }
    tmpEst <- rhrIsopleths(rhrKDE(xy, h=hcur, trast=trast), levels=level)
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
    if (abs(hcur - hnew) <= tol) {
      success=TRUE
      break
    }
  }

  return(list(h=hcur, success=success))

}
