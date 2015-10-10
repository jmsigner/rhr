##' Create \code{RasterLayer} from the extent of points.
##' 
##' Function to create an empty \code{RasterLayer} from bounding box of points defined by x and y coordinates.
##' 
##' Takes x and y coordinates of points as input and creates a \code{RasterLayer} with the input coordiantes. The range in either direction can be provided, otherwise it will be obtained from the data.
##' @param xy A two columned \code{data.frame} or \code{matrix} containing the x and y coordinates of the points.
##' @param xrange,yrange Optional numeric vector of length providing the range.
##' @param res resolution
##' @export

rasterFromXYVect <- function(xy, xrange=NA, yrange=NA, res=100) {

  if (any(is.na(xrange)) | length(xrange) != 2) {
    xrange <- c(min(xy[,1]), max(xy[,1]))
    warning("retrieved x-range from data")
  } 
  if (any(is.na(yrange)) | length(yrange) != 2) {
    yrange <- c(min(xy[,2]), max(xy[,2]))
    warning("retrieved y-range from data")
  }

  ## determine gridsize
  ncolumns <- ceiling(diff(xrange) / res)
  nrows <- ceiling(diff(yrange) / res)

  return(raster::raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2],
                        nrows=nrows, ncols=ncolumns))
}

##' Takes coordinates from points and returns a (buffered, extended) bounding box
##'
##' The buffer width is the same in all directions. If both buffer and extendRange are provided, precedence is given to buffer. 
##' @title rhrExtFromPoints
##' @param xy data.frame, matrix or SpatialPoints; containing the coordinates
##' @param buffer numeric value; containing the buffer width
##' @param extendRange numeric value; containing the factor to extend the range
##' @seealso \code{\link{extendrange}} for more information on extending the range. 
##' @export
##' @return matrix
##' @author Johannes Signer

rhrExtFromPoints <- function(xy, buffer=NULL, extendRange=NULL) {
  ## check input
  if(missing(xy)) {
    stop("rhrExtFromPoints: xy not provided, but required")
  }

  xy <- rhrCheckData(xy, returnSP=FALSE)


  if (ncol(xy) < 2) {
    stop("rhrExtFromPoints: xy has less than 2 columns")
  }

  if (ncol(xy) > 2) {
    warning("rhrExtFromPoints: xy has more than 2 columns, only first two are used")
    xy <- xy[, 1:2]
  }

  if (all(sapply(list(buffer, extendRange), is.null))) {
    warning("rhrExtFromPoints: buffer and extendRange are NULL, bbox will be returned")

    ## get bounding box off the data
    rg <- t(apply(xy, 2, range))
  }

  if (all(!sapply(list(buffer, extendRange), is.null))) {
    warning("rhrExtFromPoints: buffer and extendRange are provided. Precedence is given to buffer")
    extendRange <- NULL
  }

  if (!is.null(buffer)) {

    if (!is.numeric(buffer)) {
      stop("rhrExtFromPoints: buffer is not numeric")
    }

    if (length(buffer) > 2) {
      buffer <- buffer[1:2]
      warning("rhrExtFromPoints: only first 2 elements for buffer are used")
    }

    if (length(buffer) == 2) {
      message("rhrExtFromPoints: first element used as xbuffer and second as ybuffer")
    } else {
      buffer <- rep(buffer, 2)
    }

    xrange <- buffer[1] * c(-1, 1) + range(xy[, 1])
    yrange <- buffer[2] * c(-1, 1) + range(xy[, 2])
    rg <- rbind(xrange, yrange)

  }

  if (!is.null(extendRange)) {

    if (!is.numeric(extendRange)) {
      stop("rhrExtFromPoints: extendRange can not coerced to numeric")
    }

    if (length(extendRange) > 2) {
      extendRange <- extendRange[1:2]
      warning("rhrExtFromPoints: only first 2 elements for extendRange are used")
    }

    if (length(extendRange) == 2) {
      warning("rhrExtFromPoints: first element used as xextendRange and second as yextendRange")
    } else {
      extendRange <- rep(extendRange, 2)
    }

    xrange <- extendrange(range(xy[, 1]), f=extendRange[1]) 
    yrange <- extendrange(range(xy[, 2]), f=extendRange[2]) 
    rg <- rbind(xrange, yrange)
  }

  dimnames(rg) <- list(c("x", "y"), c("min", "max"))
  class(rg) <- c("RhrRasterExt")
  rg
}

##' Create a raster layer from extent
##'
##' This function wraps \code{raster::rast} and adds functionality to a take control over the resolution. Resolution is given precedence over \code{nrow} and \code{ncol}.
##' @title Create a raster 
##' @param ext RhrRasterExt, giving the extent of the raster
##' @param nrow numeric; number of rows
##' @param ncol numeric; number of columns
##' @param res numeric; the resolution
##' @param sameRes logical; indicating if the same resolution be used in x and y direction
##' @export
##' @return raster
##' @author Johannes Signer

rhrRasterFromExt <- function(ext, nrow=NULL, ncol=NULL, res=1, sameRes=TRUE) {

  if (missing(ext)) {
    stop("rhrRasterFromExt: extent not provided")
  }

  if (!inherits(ext, "RhrRasterExt")) {
    stop("rhrRasterFromExt: extent wrong class")
  }

  if (all(sapply(list(res, ncol, nrow), is.null))) {
    stop("rhrRasterFromExt: res, ncol and nrow are NULL")
  }

  if (!is.null(res)) {
    if (length(res) > 1) {
      res <- res[1]
      warning("rhrRasterFromExt: res, first element used")
    }
  }

  if (is.null(res) & (!is.null(nrow) + !is.null(ncol) == 1)) {
    message("rhrRasterFromExt: using equal nrow and ncol")

    if (is.null(nrow)) {
      nrow <- ncol
    } else {
      ncol <- nrow
    }

  } else if (!is.null(nrow) & !is.null(ncol) & !is.null(res)) {
    message("rhrRasterFromExt: nrow, ncol and res provided, precedence is given to res")

    ## nrow and ncol 
    ncol <- ceiling(diff(ext[1, ]) / res)
    nrow <- ceiling(diff(ext[2, ]) / res)

  } else if ((!is.null(nrow) | !is.null(ncol)) & !is.null(res)) {
    message("rhrRasterFromExt: nrow or ncol provided and res provided, precedence is given to res")

    ## nrow and ncol 
    ncol <- ceiling(diff(ext[1, ]) / res)
    nrow <- ceiling(diff(ext[2, ]) / res)

  } else if (is.null(nrow) & is.null(ncol)) {
    ## nrow and ncol 
    ncol <- ceiling(diff(ext[1, ]) / res)
    nrow <- ceiling(diff(ext[2, ]) / res)
  }

  if (is.null(res)) {
    resx <- diff(ext[1, ]) / ncol
    resy <- diff(ext[2, ]) / nrow

    if (sameRes) {
      if (resx > resy) {
        resx <- resy
        ## increase number of columns to still match extend
        ncol <- ceiling(diff(ext[1, ]) / resx)
        
      } else {
        resy <- resx
        nrow <- ceiling(diff(ext[2, ]) / resy)
      }
    }

    xmin <- ext[1,1]
    xmax <- xmin + resx * ncol
    ymin <- ext[2,1]
    ymax <- ymin + resy * nrow
  } else {
    xmin <- ext[1,1]
    xmax <- xmin + res * ncol
    ymin <- ext[2,1]
    ymax <- ymin + res * nrow
  }

  raster::raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, nrows=nrow, ncols=ncol)
  
}
  

