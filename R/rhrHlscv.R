##' Calculate a Bandwidth for Kernel Density Estimation
##'
##' Use two dimensional least square cross validation to select the bandwidth of a kernel density estimate.
##'
##' @param xy A two column numeric data.frame with two columns: the x and y coordinates. 
##' @param range numeric vector with different candidate h values. 
##' @param whichMin A character indicating if the \code{global} or \code{local} minimum should be searched for.
##' @param rescale A character, indicating if and if how data are rescaled. \code{unitvar} rescales x and y coordinates to unit variance, \code{xvar} rescales x and y coordinate to variance of x and \code{none} uses the raw data.
##' @param binned whether or not a binned version is used or not. 
##' @param trast A RasterLayer with the desired extent and resolution.


##' @details Function to calcualte least square cross validation bandwidth. This implementation is based on Seaman and Powell (1996). Additionally a binned version of the algorithm is available. This should be faster for large datasets containing serval thousands of relocations. If \code{whichMin} is \code{"global"} the global minimum is returned, else the local minimum with the largest candidate bandwidth is returned.

##' @return \code{vector} of length two
##' @export
##' @references Seaman, D. E., & Powell, R. A. (1996). An evaluation of the accuracy of kernel density estimators for home range analysis. _Ecology, 77(7)_, 2075-2085.
##' @references Carr and Rodges
##' 
##' @author Johannes Signer 
##' @useDynLib rhr

##' @examples 
##' \dontrun{
##' ## Unbinned version
##' data(datSH)
##' hlscv <- rhrHlscv(datSH[1:1000, 2:3])
##'
##' ## Plotting the different values for h
##' x <- datSH[, 2]
##' y <- datSH[, 3]
##' 
##' ## Create a binned grid
##' trast <- raster(xmn=round(min(x)) - 100, xmx=round(max(x)) + 100,
##'                 ymn=round(min(y)) - 100, ymx=round(max(y)) + 100,
##'                 nrow=250, ncol=250)
##' 
##' freq <- rasterize(cbind(xs, ys), trast, fun="count")
##' freq <- rasterToPoints(freq)
##' hlscvb <- rhrHlscv(datSH[1:1000, 2:3], binned=TRUE)
##'
##' }

rhrHlscv <- function(xy, range=do.call(seq, as.list(c(rhrHref(xy)$h * c(0.1, 2), length.out=100))), 
                     whichMin="global", rescale="none", binned=FALSE,
                     trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {

  ## Some input validation
  xy <- rhrCheckData(xy, returnSP=FALSE)

  if (is.null(trast)) {
    stop("rhrHlscv: trast is missing")
  }

  if (ncol(xy) > 2) {
    warning("rhrHlscv: xy: using only the first two columns")
  }

  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHlscv: scale: not one of unit, sd or none")
  }

  if (binned) {
    ## count
    wei <- raster::rasterize(xy, trast, fun="count")
    wei <- raster::rasterToPoints(wei)
    xy <- data.frame(wei[, 1:2])
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
 

  ## reference bandwidth

  converged <- TRUE

  if (binned) {
    res <- binnedCV(xs, ys, wei[, 3], range)
  } else {
    res <- unbinnedCV(xs, ys, range)
  }


  if (whichMin == "global") {
    h <- range[which.min(res)]
  } else {
    h <- range[max(localMinima(res))]
  }

  ## Did h converge?
  if (range[1] == h | range[length(range)] == h) {
    converged <- FALSE
    warning("rhrHlscv: lscv did not converge.")
  }

  ## prepare return
  if (rescale == "unitvar") {
    h <- h * c(sd(xy[, 1]), sd(xy[, 2]))
  } else {
    h <- c(h, h)
  }
  list(h=h, converged=converged, res=res, whichMin=whichMin, rescale=rescale, binned=binned, range=range)
}

## Helper function from: http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMinima <- function(x) {
  ## Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
} 
