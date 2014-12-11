## ============================================================================== ##  
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ============================================================================== ##

##' Kernel Density Estimation (KDE)
##'
##' A function to estimate home ranges with kernel density estimation. 
##'
##' @param xy \code{data.frame} with two columns: x and y coordinates.
##' @param h character ("href" or "hlscv") specifying the method to estimate the bandwidth or numeric value specifying the bandwidth.
##' @param trast a \code{RasterLayer} used as an template for the output grid.

##' @details The size and resolution of the resulting utilization distribution (UD) grid is influenced by \code{traster, xrange, yrange, increaseExtent, buffer, res, gridsize}. The size of the grid can be set either through a template raster (\code{traster}), \code{xrange} and \code{yrange} or \code{increaseExtent}. \code{traster} takes precedence over \code{xrange} and \code{yrange}, \code{buffer} and \code{grid}. If none of the previous arguments are provided, \code{xrange} and \code{yrange} are taken from the data.
##'
##' The resolution of the resulting UD grid can be set through either \code{res} or \code{gridsize}. \code{res} takes precedence over \code{gridsize}. If none of the previous arguments is provided the grid is set by default to a 100 by 100 grid.

##' The bandwidth can be provided by the user or estimated through the reference bandwidth (this method is often refered to as the ad hoc method), plug in the euqtion method or the least square cross validation method. Reference bandwidth estimation is implemented as suggested by Silverman 1986. Plugin the equation method is wrapped from \code{KernSmooth::dpki} and a simple binned version Silverman's suggestion for least square cross validation is implemented.
#
##' Kernels densities are estimated with \code{KernSmooth::bkde2d}. This is a binned approximation of 2D kernel density estimates (see \code{?KernSmooth::bkde2d} for more details. 
##'

##' @seealso \code{KernSmooth::bkde2d}, \code{KernSmooth::dpik}, \code{rhr::rhrHref}, \code{rhr::rhrHlscv}, \code{rhr::rhrHpi}


##' @return object of class \code{RhrHREstimator}
##' @export
##' 
##' @author Johannes Signer 
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
                   h=rhrHref(xy)$h, 
                   trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input 
  projString <- if (inherits(xy, "SpatialPoints")) {
    proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    proj4string(xy$dat)
  } else {
    CRS(NA_character_)
  }
  xy <- rhrCheckData(xy, returnSP=FALSE)

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
    proj4string(res$res) <- projString
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
  xrange <- c(xmin(trast), xmax(trast))
  yrange <- c(ymin(trast), ymax(trast))
  rncol <- ncol(trast)
  rnrow <- nrow(trast)


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
rhrIsopleths.RhrKDE <- function(x, levels=95, ...) {

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
plot.RhrKDE <- function(x, addIsopleths=TRUE, ...) {
  if (addIsopleths) {
    tempol <- rhrIsopleths(x, ...)
  }
  plot(rhrUD(x))
  if (addIsopleths) {
    plot(rhrIsopleths(x), add=TRUE)
  }
}
