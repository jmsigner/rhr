##' Kernel Density Estimation (KDE)
##'
##' A function to estimate home ranges with kernel density estimation. 
##'
##' Kernels densities are estimated with \code{KernSmooth::bkde2d}. This is a binned approximation of 2D kernel density estimates (see \code{?KernSmooth::bkde2d} for more details. 
##'
##' @param xy \code{data.frame} with two columns: x and y coordinates.
##' @param h character ("href" or "hlscv") specifying the method to estimate the bandwidth or numeric value specifying the bandwidth.
##' @param trast a \code{RasterLayer} used as an template for the output grid.
##' @param levels A numeric vector, indicating isopleths of KDE. 
##'
##' @seealso \code{KernSmooth::bkde2d}, \code{rhr::rhrHref}, \code{rhr::rhrHlscv}, \code{rhr::rhrHpi}
##' @return object of class \code{RhrHREstimator}
##' @import Rcpp ggplot2 grid maptools methods rgdal
##' @export
##' 
##' @examples
##' data(datSH)
##' \dontrun{ 
##' # Kernel with href bandwidth estimation
##' k1 <- rhrKDE(datSH[, 2:3], h="href", res=100)
##' plot(k1)
##' 
##' # what is the actually estimated bandwidth?
##' k1$parameters$h
##' 
##' # Kernel with href bandwidth estimation
##' k2 <- rhrKDE(datSH[, 2:3], h="lscv", res=100)
##' plot(k2)
##' 
##' # what is the actually estimated bandwidth?
##' k2$parameters$h
##' }

rhrKDE <- function(xy,
                   h=rhrHref(xy)$h, levels = 95, 
                   trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input 
  projString <- if (inherits(xy, "SpatialPoints")) {
    sp::proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    sp::proj4string(xy$dat)
  } else {
    sp::CRS(NA_character_)
  }
  xy <- rhrCheckData(xy, returnSP=FALSE)
  levels <- rhrCheckLevels(levels)

  ## ---------------------------------------------------------------------------- #
  ## Check bandwidth
  if (!is.numeric(h)) {
    stop("rhrKDE: bandwidth should be numeric")
  } else if (length(h) > 2) {
    warning("rhrKDE: h only first 2 elements used")
    h <- h[1:2]
  } else if(length(h) < 2) {
    warning("rhrKDE: same bandwidth is used in x and y direction")
    h <- rep(h, 2)
  }

  ## ---------------------------------------------------------------------------- #
  ## Estimate kernels
  res <- tryCatch(
    expr=list(msg=NULL, exitStatus=0, res=.rhrKDE(xy, h, trast)),
    error=function(e) list(msg=e, exitStatus=1))

  if (res$exitStatus == 0) {
    sp::proj4string(res$res) <- projString
  }

  res <- structure(
    list(
    exitStatus=res$exitStatu,
    msg=res$msg,
    call=call,
    args=args,
    res=list(hr=res$res)),
    class=c("RhrKDE", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}


##' @export
.rhrKDE <- function(xy, h, trast) {
  ## prep kde
  xrange <- c(raster::xmin(trast), raster::xmax(trast))
  yrange <- c(raster::ymin(trast), raster::ymax(trast))
  rncol <- raster::ncol(trast)
  rnrow <- raster::nrow(trast)


  ## Create Raster
  kde <- KernSmooth::bkde2D(xy, bandwidth=h, range.x=list(xrange, yrange), gridsize=c(rncol, rnrow))

  ## Finish output
  r1 <- raster::raster(t(kde$fhat)[nrow(trast):1,], xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2])
  r1
}

##' @export
print.RhrKDE <- function(x, ...) {
  cat("* rhrHREstimatorKDE \n")
  cat("* ----------------- \n")
  cat(sprintf("* Observations (n) : %s\n", nrow(x$arguments$xy)))
  cat(sprintf("* Bandwidth (h)    : %s\n", x$arguments$h))
}

##' Calculate cumulative UD for RhrHrEstimatorKDE
##' 
##' @param x an object of class rhrHREstimatorKDE
##' @param ... further arguments, none implemented
##' @export
##' @method rhrCUD RhrKDE

rhrCUD.RhrKDE <- function(x, ...) {

  r1 <- x$res$hr
  v <- raster::getValues(r1)
  v <- v / sum(v, na.rm=TRUE)
  udFromDat <- raster::setValues(r1, v)

  v <- cumsum(v[order(-v)])[order(order(-v))]
  r2 <- raster::setValues(r1, v) 

  return(r2)
}

##' @export
rhrIsopleths.RhrKDE <- function(x, levels=NULL, ...) {

  if (is.null(levels)) {
    levels <- x$args$levels
  } 

  levels <- rhrCheckLevels(levels)
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels) 
}

##' @export
rhrUD.RhrKDE <- function(x, ...) {
  x$res$hr
}

##' @export
rhrArea.RhrKDE <- function(x, levels=95, ...) {
  tmp <- rhrIsopleths(x, levels)
  data.frame(tmp)
}

##' @export
rhrData.RhrKDE <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

##' @export
##' @rdname rhrHasUD
rhrHasUD.RhrKDE <- function(x, ...) {
  TRUE
}

##' @method plot RhrKDE
##' @export
plot.RhrKDE <- function(x, levels = NULL, addIsopleths=TRUE, ...) {

  if (is.null(levels)) {
    levels <- x$args$levels
  } 
  
  plot(rhrUD(x))
  if (addIsopleths) {
    plot(rhrIsopleths(x, levels), add=TRUE)
  }
}
