##' rasterFromXYVect
##' 
##' Creates empty raster
##' @param xy a data.frame or matrix. The first column are x coordinates and the second column are y coordinates
##' @param xrange range of x
##' @param yrange range of y
##' @param res resolution

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
##' @examples
##' dat <- data.frame(x=rnorm(100), y=rnorm(100))
##' ## Get the bounding box
##' rhrExtFromPoints(dat)
##' ## Get the bounding box buffered by 10 units
##' rhrExtFromPoints(dat, buffer=10)
##' ## Get the bounding box with an extended range
##' rhrExtFromPoints(dat, extendRange=0.5)
##' ## Note, buffer takes precedence over extendRange
##' rhrExtFromPoints(dat, buffer=10, extendRange=0.5)

rhrExtFromPoints <- function(xy, buffer=NULL, extendRange=NULL) {
  ## check input
  if(missing(xy)) {
    stop("rhrExtFromPoints: xy not provided, but required")
  }

  ## Check that xy is either data.frame, matrix or SpatialPoints*
  if (inherits(xy, "SpatialPoints")) {
    xy <- coordinates(xy)
  }

  if (inherits(xy, "RhrMappedData")) {
    xy <- data.frame(xy)
  }

  if (!class(xy) %in% c("matrix", "data.frame")) {
    stop("rhrExtFromPoints: xy should be a matrix or data.frame")
  }

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
      warning("rhrExtFromPoints: first element used as xbuffer and second as ybuffer")
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
##' @export
##' @return raster
##' @author Johannes Signer
##' @examples
##' dat <- data.frame(x=rnorm(100), y=rnorm(100))
##' ## Get the bounding box
##' ext <- rhrExtFromPoints(dat)
##' r <- rhrRasterFromExt(ff, nrow=10, ncol=100) 
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
  
