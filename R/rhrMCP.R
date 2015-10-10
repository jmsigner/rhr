##' Minimum Convex Polygon (MCP)
##'
##' Function to estimate the minimum convex polygon (MCP) home range. 
##'
##' Computes the minimum convex polygon of a subset of points. First the centroid of the home range is found with \code{rgeos::gCentroid} and then the `%-levels` points are used to calculate a minimum convex polygon. 
##'
##' @template xy
##' @template levels
##' @return Object of class \code{RhrMCP}.
##' @export
##' @seealso \code{adehabitatHR::mcp}, \code{rgeos::gConvexHull}

rhrMCP <- function(xy, levels=95) {

  ## ============================================================================== ##  
  ## Start

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

  xy <- rhrCheckData(xy, returnSP=TRUE)
  levels <- rhrCheckLevels(levels)

  ## ============================================================================== ##  
  ## Calc MCP
  bb <- tryCatch(
    expr=list(msg=NULL, exitStatus=0, res=.rhrMCP(xy, levels)),
    error=function(e) list(msg=e, exitStatus=1))

  long <- lat <- group <- level <- lon <- NULL
  if (bb$exitStatus == 0) {
    sp::proj4string(bb$res) <- projString
  }

  res <- structure(
    list(
    exitStatus=bb$exitStatu,
    msg=bb$msg,
    call=call,
    args=args,
    res=list(hr=bb$res)),
    class=c("RhrMCP", "RhrGeoEst", "RhrEst", "list"))
  return(invisible(res))
}

##' @export
.rhrMCP <- function(xy, levels) {
  dists <- data.frame(id=1:length(xy), dist=as.vector(
                                         rgeos::gDistance(
                                           rgeos::gCentroid(xy), xy, byid=TRUE)))
  
  ## calculate mcps
  mcps <- lapply(levels, function(l) rgeos::gConvexHull(xy[dists[dists$dist <= quantile(dists$dist, l/100), "id"], ], id=l))

  ## Project
  bb <- do.call(rbind, mcps)
  bb <- sp::SpatialPolygonsDataFrame(bb, data.frame(level=names(bb), area=rgeos::gArea(bb, byid=TRUE)))
}


#' @export
print.RhrKDE <- function(x, as_md = FALSE, ...) {
  cat("* RhrMCP \n")
  cat("* ------ \n")
  cat(sprintf("* Observations (n) : %s\n", nrow(rhrData(x))))
  cat(sprintf("* Levels           : %s\n", paste0(rhrLevels(x), collapse = ", ")))
}


##' @export
rhrIsopleths.RhrMCP <- function(x, ...) {
  x$res$hr
}

##' @export
rhrArea.RhrMCP <- function(x, ...) {
  data.frame(x$res$hr)
}

##' @export
rhrData.RhrMCP <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

##' @export
rhrLevels.RhrMCP <- function(x, ...) {
  x$args$levels
}

##' @export
rhrArgs.RhrMCP <- function(x, ...) {
  x$args
}

#' @export
#' @importFrom graphics plot
plot.RhrMCP <- function(x, ..., base) {

  long <- lat <- group <- level <- lon <- NULL

  ## fortify poly
  tempol <- rhrIsopleths(x)
  tempol@data$id <- rownames(tempol@data)
  tempolPoints <- try(ggplot2::fortify(tempol, region="id"))
  tempolDF <- base::merge(tempolPoints, tempol@data, by="id")

  points <- rhrData(x, spatial=FALSE)

  names(points)[1:2] <- c("lon", "lat")

  ggplot2::ggplot(tempolDF, ggplot2::aes(x=long, y=lat, group=group, color=factor(level))) + 
    ggplot2::geom_point(data=points, ggplot2::aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) +
    ggplot2::geom_path(size=3, alpha=0.4) + 
    ggplot2::labs(colour=("MCP levels"), x="lon", y="lat") +
    ggplot2::geom_path(size=0.2, colour="black") +
    ggplot2::scale_color_manual(values=terrain.colors(10)) + 
    ggplot2::theme_bw() 
}

