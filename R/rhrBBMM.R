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
##' @param sigma1
##' @param sigma2
##' @param trast a \code{RasterLayer} used as an template for the output grid.

##' @seealso \code{KernSmooth::bkde2d}, \code{KernSmooth::dpik}, \code{rhr::rhrHref}, \code{rhr::rhrHlscv}, \code{rhr::rhrHpi}


##' @return object of class \code{RhrBBMM}
##' @export
##' 
##' @author Johannes Signer 

rhrBBMM <- function(xyt,
                    rangesigma1=c(0, 10000), 
                    sigma2=123,
                    trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL),
                    proj4string=NA) {

  ## ------------------------------------------------------------------------------ ##  
  ## Debug only
  if (FALSE) {
    datSH <- datSH[!duplicated(ymd(datSH$day) + hms(datSH$time)), ]
    xyt <- as.ltraj(datSH[, 2:3], ymd(datSH$day) + hms(datSH$time), id=1)
    trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)
    proj4string <- NA
    sig2 <- 10
  }

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  
  ## check input 
  xyt <- as.ltraj(xyt[, 1:2], xyt[, 3], id=1)
  projString <- rhrProjString(xyt, projString=proj4string)

  sigma1 <- adehabitatHR::liker(xyt, rangesig1=c(0, 10000), sig2=sigma2, plotit=FALSE)[[1]]$sig1

  res(trast) <- rep(min(res(trast)), 2)
  
  ## ---------------------------------------------------------------------------- #
  ## Estimate BBMM
  res <- tryCatch(
    expr=list(
      exitStatus=0,
      res=raster(as(adehabitatHR::kernelbb(xyt, sig1=sigma1, sig2=sigma2, grid=as(trast, "SpatialPixels")), "SpatialGridDataFrame"))),
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
      res=res$res,
      sigma1=sigma1), 
    class=c("RhrBBMM", "RhrEst", "list"))
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
plot.RhrBBMM <- function(x, addIsopleths=TRUE, levels=95, ...) {

  if (addIsopleths) {
    tempol <- rhrIsopleths(x, levels, ...)
  }

  plot(rhrUD(x))

  if (addIsopleths) {
    plot(rhrIsopleths(x), add=TRUE)
  }
}
