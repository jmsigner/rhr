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

##' Brownian Bridge Movement Model (BBMM)
##'
##' A function to estimate home ranges with kernel density estimation. 
##'
##' @param xy \code{data.frame} with two columns: x and y coordinates.
##' @param time a vector with time stamps for each relocation.
##' @param rangesigma1 parameter 1
##' @param sigma2 parameter 2
##' @param trast a \code{RasterLayer} used as an template for the output grid.

##' @seealso \code{adehabitatHR::kernelbb}


##' @return object of class \code{RhrBBMM}
##' @export
##' 
##' @author Johannes Signer 

rhrBBMM <- function(xy,
                    time, 
                    rangesigma1=c(0, 10000), 
                    sigma2=123,
                    trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {


  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  projString <- if (inherits(xy, "SpatialPoints")) {
    sp::proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    sp::proj4string(xy$dat)
  } else {
    sp::CRS(NA_character_)
  }
  xy <- rhrCheckData(xy, returnSP=FALSE)

  if (nrow(xy) != length(time)) {
    stop("rhrTTSI: not every observation has a timestamp")
  }

  xyt <- data.frame(x = xy[, 1],
                    y = xy[, 2],
                    timestamp = time)

  
  xyt <- adehabitatLT::as.ltraj(xyt[, 1:2], xyt[, 3], id=1)
  sigma1 <- adehabitatHR::liker(xyt, rangesig1=c(0, 10000), sig2=sigma2, plotit=FALSE)[[1]]$sig1

  raster::res(trast) <- rep(min(raster::res(trast)), 2)
  
  ## ---------------------------------------------------------------------------- #
  ## Estimate BBMM
  res <- tryCatch(
    expr=list(
      exitStatus=0,
      res=raster::raster(as(adehabitatHR::kernelbb(xyt, sig1=sigma1, sig2=sigma2, grid=as(trast, "SpatialPixels")), "SpatialGridDataFrame"))),
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
      res=res$res,
      sigma1=sigma1), 
    class=c("RhrBBMM", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}


##' @export
print.RhrBBMM <- function(x, ...) {
  cat("* rhrHREstimatorBBMM \n")
  cat("* ----------------- \n")
  cat(sprintf("* Observations (n) : %s\n", nrow(x$arguments$xy)))
}

##' @export
rhrUD.RhrBBMM <- function(x, ...) {
  x$res
}

##' @export
rhrCUD.RhrBBMM <- function(x, ...) {
  rhrUD2CUD(rhrUD(x))
}

##' @export
rhrIsopleths.RhrBBMM <- function(x, levels=95, ...) {
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

##' @export
rhrArea.RhrBBMM <- function(x, levels=95, ...) {
  tmp <- rhrIsopleths(x, levels)
  data.frame(tmp)
}

##' @export
rhrData.RhrBBMM <- function(x, ...) {
  x$args$xy
}

##' @export
rhrHasUD.RhrBBMM <- function(x, ...) {
  TRUE
}

##' @export
##' @method plot RhrBBMM
plot.RhrBBMM <- function(x, addIsopleths=TRUE, levels=95, ...) {
  if (addIsopleths) {
    tempol <- rhrIsopleths(x, levels, ...)
  }

  plot(rhrUD(x))

  if (addIsopleths) {
    plot(rhrIsopleths(x), add=TRUE)
  }
}
