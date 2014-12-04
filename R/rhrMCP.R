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

##' Minimum Convex Polygon (MCP)
##'
##' Computes the minimum convex polygon of a subset of points. First the centroid of the home range is found with \code{rgeos::gCentroid} and then the `100 - levels` points are used to calculate a minimum convex polygon. 
##' @title rhrMCP
##' @param xy data.frame with two columns. The first column contains x coordinates and the second column contains y coordinates
##' @param levels vector with the percentage of closest points to the centroid that are used to calculated MCP
##' @param proj4string character, with the projection (it is parsed with \code{sp::CRS}).  
##' @return object of class \code{RhrMCP}
##' @export
##' @author Johannes Signer inspired from \code{adehabitatHR::mcp}
##' @seealso \code{adehabitatHR::mcp}, \code{rgeos::gConvexHull}
##' @examples
##' data(datSH)
##' ## Calculate mcp at one level
##' mcp1 <- rhrMCP(datSH[, 2:3], levels=95)
##' ## Calculate mcp at several levels
##' mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))
##'
##' ## Area at each isopleth level
##' rhrArea(mcp2)
##'
##' ## SptialPolygonsDataFrame of isopleth
##' rhrIsopleths(mcp2)
rhrMCP <- function(xy, levels=95) {

  ## ============================================================================== ##  
  ## Start

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

  xy <- rhrCheckData(xy, returnSP=TRUE)
  levels <- rhrCheckLevels(levels)

  ## ============================================================================== ##  
  ## Calc MCP
  bb <- tryCatch(
    expr=list(msg=NULL, exitStatus=0, res=.rhrMCP(xy, levels)),
    error=function(e) list(msg=e, exitStatus=1))

  if (bb$exitStatus == 0) {
    proj4string(bb$res) <- projString
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

