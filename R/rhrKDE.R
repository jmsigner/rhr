##' Kernel Density Estimation (KDE)
##'
##' Function to estimate home ranges with kernel density estimation. 
##'
##' Kernels densities are estimated with \code{KernSmooth::bkde2d}. This is a binned approximation of 2D kernel density estimates (see \code{?KernSmooth::bkde2d} for more details. 
##'
##' @template xy
##' @template levels
##' @param h Numeric \code{vector} with the bandwidth of the kernel. A scalar value will be applied to both dimensions.
##' @template trast
##' @seealso \code{KernSmooth::bkde2d}, \code{rhr::rhrHref}, \code{rhr::rhrHlscv}, \code{rhr::rhrHpi}
##' @return Object of class \code{RhrKDE}
##' @export
##' 

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
    args$h <- h <- h[1:2]
  } else if(length(h) < 2) {
    warning("rhrKDE: same bandwidth is used in x and y direction")
    args$h <- h <- rep(h, 2)
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
print.RhrKDE <- function(x, as_md = FALSE, ...) {
  if (!as_md) {
    cat("* RhrKDE \n")
    cat("* ------ \n")
    cat(sprintf("* Observations (n) : %s\n", nrow(rhrData(x))))
    cat(sprintf("* Bandwidth (h)    : %s\n", paste0(rhrTuningParameter(x)$value, collapse = ", ")))
    cat(sprintf("* Template raster  : %s\n", as.character(rhrArgs(x)$trast)))
    cat(sprintf("* Levels           : %s\n", paste0(rhrLevels(x), collapse = ", ")))
  } else {
    
    knitr::kable(data.frame(
      What = c("Observations (n)",  "Bandwidth (h)",  "Template raster", "Levels"), 
      Value = c(nrow(rhrData(x)),
                paste0(rhrTuningParameter(x)$value, collapse = ", "), 
                as.character(rhrArgs(x)$trast), paste0(rhrLevels(x), collapse = ", ")) 
    ), row.name = FALSE)
  }
}

#' @export

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
    levels <- rhrLevels(x)
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
rhrArgs.RhrKDE <- function(x, ...) {
  x$args
}

##' @export
##' @rdname rhrHasUD
rhrHasUD.RhrKDE <- function(x, ...) {
  TRUE
}

#' @export
plot.RhrKDE <- function(x, levels = NULL, addIsopleths=TRUE, ...) {
  
  if (is.null(levels)) {
    levels <- x$args$levels
  } 
  plotRaster(rhrUD(x), ...)
  if (addIsopleths) {
    sp::plot(rhrIsopleths(x, levels), add=TRUE)
  }
}

##' @export
rhrTuningParameter.RhrKDE <- function (x, msg = FALSE, digits = 3, ...) {
  if (msg) {
    paste0("Value of tuning parameter h: ", paste0(round(x$args$h, digits), collapse = ", "))
  } else {
    list(name = "h",
         value = round(x$args$h, digits))
  }
}
