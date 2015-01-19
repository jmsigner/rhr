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
##' @example inst/examples/rhrMCP.R

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


##' @export
print.RhrMCP <- function(x, ...) {
  if (x$exitStatus == 0) {
    cat("* Results: Minimum ConvexPolygon \n")
    cat("* ------------------------------ \n")
    cat(paste0("* Observations (n) : ", nrow(x$args$xy), "\n"))
    cat(paste0("* Levels           : ", x$args$levels), "\n")
    cat(paste0("* To obtain the areas use: rhrArea(x)\n"))
  } else {
    cat("* Results: Minimum ConvexPolygon \n")
    cat("* ------------------------------ \n")
    cat(paste0("* A error occured during the calculations\n"))
    cat(paste0("* Error message: ", x$msg), "\n")
  }
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

##' @method plot RhrMCP
##' @export
plot.RhrMCP <- function(x, title=NULL, ...) {

  long <- lat <- group <- level <- lon <- NULL

  ## fortify poly
  tempol <- rhrIsopleths(x)
  tempol@data$id <- rownames(tempol@data)
  tempolPoints <- try(ggplot2::fortify(tempol, region="id"))
  tempolDF <- merge(tempolPoints, tempol@data, by="id")

  points <- rhrData(x, spatial=FALSE)

  names(points)[1:2] <- c("lon", "lat")

  ggplot2::ggplot(tempolDF, aes(x=long, y=lat, group=group, color=factor(level))) + 
    ggplot2::geom_point(data=points, aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) +
      ggplot2::geom_path(size=3, alpha=0.4) + 
        ggplot2::labs(colour=("MCP levels"), x="lon", y="lat", title=title) +
          ggplot2::geom_path(size=0.2, colour="black") +
            ggplot2::scale_color_manual(values=terrain.colors(10)) + theme_bw() 
}

