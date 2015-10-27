#' Brownian Bridge Movement Model (BBMM)
#'
#' A wrapper around \code{adehabitatHR::kernelbb}. 
#'
#' @template trackST
#' @param rangesigma1 parameter 1
#' @param sigma2 parameter 2
#' @param trast a \code{RasterLayer} used as an template for the output grid.
#' @template levels

#' @seealso \code{adehabitatHR::kernelbb}


#' @return object of class \code{RhrBBMM}
#' @export
#' 

rhrBBMM <- function(track,
                    rangesigma1=c(0, 10000), 
                    sigma2=100,
                    levels = 95, 
                    trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()
  
  xy <- track
  
  if (!inherits(xy, "RhrTrackST")) {
    stop("xy not recognized")
  }

  projString <- getEPSG(xy)
  time <- rhrTimes(xy)
  xy <- rhrCheckData(xy, returnSP=FALSE)


  xyt <- data.frame(x = xy[, 1],
                    y = xy[, 2],
                    timestamp = time)

  if (any(duplicated(xyt$timestamp))) {
    xyt <- xyt[!duplicated(xyt$timestamp), ]
  }
  
  xyt <- adehabitatLT::as.ltraj(xyt[, 1:2], xyt[, 3], id=1)
  sigma1 <- adehabitatHR::liker(xyt, rangesig1=rangesigma1, sig2=sigma2, plotit=FALSE)[[1]]$sig1

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


#' @export
#' @method print RhrBBMM
print.RhrBBMM <- function(x, ...) {
  cat("* rhrHREstimatorBBMM \n")
  cat("* ----------------- \n")
  cat(sprintf("* Observations (n) : %s\n", nrow(x$arguments$xy)))
}

#' @export
rhrUD.RhrBBMM <- function(x, ...) {
  x$res
}

#' @export
rhrCUD.RhrBBMM <- function(x, ...) {
  rhrUD2CUD(rhrUD(x))
}

#' @export
rhrIsopleths.RhrBBMM <- function(x, levels, ...) {
  
  if (missing(levels)) {
    levels <- rhrArgs(x)$levels
  }
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

#' @export
rhrArea.RhrBBMM <- function(x, ...) {
  tmp <- rhrIsopleths(x, ...)
  data.frame(tmp)
}

#' @export
rhrData.RhrBBMM <- function(x, ...) {
  x$args$xy
}

#' @export
rhrHasUD.RhrBBMM <- function(x, ...) {
  TRUE
}

#' @export
#' @method plot RhrBBMM
plot.RhrBBMM <- function(x, addIsopleths=TRUE, levels=95, ...) {
  if (addIsopleths) {
    tempol <- rhrIsopleths(x, levels, ...)
  }
  plotRaster(rhrUD(x))
  if (addIsopleths) {
    sp::plot(rhrIsopleths(x), add=TRUE)
  }
}

##' @export
rhrArgs.RhrBBMM <- function(x, ...) {
  x$args
}

##' @export
rhrData.RhrBBMM <- function(x, spatial = FALSE, as.data.frame=FALSE, ...) {
  if (as.data.frame) {
    xx <- rhrCheckData(x$args$track, returnSP=spatial)
  } else {
    x$args$track
  }
}
