##' Local Convex Hull (LoCoH)
##'
##' Function to estimate home ranges with local convex hulls.
##'
##' Three different methods to determine the neighboring points are available:
##' \itemize{
##'  \item{"k"}{uses k-nearest neighbors}
##'  \item{"r"}{uses all neighbors within a radius r}
##'  \item{"a"}{uses all neighbors that can be reached within a distance a. The distance to all points is calculated and then cummulatively summed starting from the smallest until \code{a} is reached.}}.
##' \code{autoN} attempts to estimate values for the tuning parameters k,r,a.
##' If \code{type} is \code{"k"} then \code{"n"} is the square root of all locations.
##' If \code{type} is \code{"a"} then \code{"n"} is the maximum distance between any two relocations.
##' If \code{type} is \code{"r"} then \code{"n"} is 5 % of the maximum distance between any two relocations.
##'
##' @template xy 
##' @param type Scalar character, one of "k", "r", "a". Method to determine the tuning parameter \code{n}. 
##' @param n Numeric scalar, value of the tuning parameter.
##' @param minPts Numeric scalar, the minimum number of neighbors required.
##' @param autoN Boolean scalar, whether or not \code{n} should be determined automatically.
##' @template levels 
##' @return Object of class \code{RhrLoCoH}
##' @references Getz, W. M., & Wilmers, C. C. (2004). A local nearest-neighbor convex-hull construction of home ranges and utilization distributions. _Ecography_, 27(4), 489-505.
##' @references Getz, W. M., Fortmann-Roe, S., Cross, P. C., Lyons, A. J., Ryan, S. J., & Wilmers, C. C. (2007). LoCoH: nonparameteric kernel methods for constructing home ranges and utilization distributions. _PloS one_, 2(2), e207.
##' @export

rhrLoCoH <- function(xy, type="k", n=10, levels=95, minPts=3, autoN=FALSE) {

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input coordinates
  projString <- getEPSG(xy)

  xy <- rhrCheckData(xy, returnSP=FALSE)
  
  # Remove quotes from type
  type <- gsub("['\"]", "", type)

  ## type
  if (!type %in% c("a", "k", "r")) {
    stop(paste0("rhrLocoh: incorrect type"))
  }

  n <- as.numeric(n)
  if (is.na(n)) {
    stop(paste("rhrLocoh: n should be numeric, not ", n))
  }
  
  ## Are levels between 1 and 100, remove duplicated, order and add 0
  levels <- rhrCheckLevels(levels)

  ## determine n automatically and overwrite
  if (autoN) {
    if (type == "k") {
      args$n <- n <- round(sqrt(nrow(xy)))
    }
    if (type == "a") {
      args$n <- n <- round(max(dist(xy)))
    }
    if (type == "r") {
      args$n <- n <- round(max(dist(xy)) * 0.05)
    }
  }
  
  ## calculation
  bb <- tryCatch(
    expr=list(msg=NULL, exitStatus=0, res=.rhrLoCoH(xy=xy, type=type, n=n, minPts=minPts, level=levels)),
    error=function(e) list(msg=e, exitStatus=1))

  if (bb$exitStatus == 0) {
    sp::proj4string(bb$res) <- projString
  }

  res <- structure(
    list(
    exitStatus=bb$exitStatu,
    msg=bb$msg,
    call=call,
    args=args,
    res=list(hr=bb$res, n=n)),
    class=c("RhrLoCoH", "RhrGeoEst", "RhrEst", "list"))
  return(invisible(res))

}

.rhrLoCoH <- function(xy, type, n, minPts=minPts, level=levels, allLevels=FALSE) {
  ## All levels is set to FALSE, it can only be changed by actually calling rhr:::.rhrLoCoH
  no <- 1:nrow(xy)
  if (type == "k") {
    if (n > nrow(xy)) {
      n <- nrow(xy)
      warning(paste0("Locoh, type k, n > number of points, set n to number of points (", n, ")"))
    }
    ## 1. calc dist
    ## 2. order by dist
    ## 3. take n nearest
    aa <- lapply(no, function(i)
      no[order(sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2))][1:n])
  } else if (type == "r") {
    ## 1. calc dist
    ## 2. take all pts with dist <= n
    aa <- lapply(no, function(i)
      no[sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2) <= n])
  } else if (type == "a") {
    # 1. calc dist
    # 2. order by dist
    # 3. take cum dist
    # 4. take points where cumist <= n
    aa <- lapply(no, function(i) {
                   di <- sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2)
                   no[order(di)][cumsum(di[order(di)]) <= n]
            })
  }


  ## remove the ones with less than minPts pts
  aa <- aa[sapply(aa, length) >= minPts]

  xysp <- sp::SpatialPointsDataFrame(xy[, 1:2], data=data.frame(id=1:nrow(xy)))

  zz <- lapply(aa, function(x) xysp[x, ])
  mcps <- lapply(zz, rgeos::gConvexHull)

  mcpAreas <- sapply(mcps, rgeos::gArea)

  mcpAreasOrder <- order(mcpAreas)

  ff <- zz[mcpAreasOrder]
  mm <- mcps[mcpAreasOrder]

  pp <- rep(NA_integer_, length(ff))
  seen <- c()
  ## this is still slow
  for (i in 1:length(ff)) {
    seen <- union(seen, ff[[i]]$id)
    pp[i] <- length(unique(seen))
  }

  qq <- list()
  qq[[1]] <- mm[[1]]
  pp <- pp/nrow(xy) * 100
  
  if (!allLevels) {
    wlevel <- sapply(level, function(l) which.min(abs(pp - l)))
    for (i in seq_along(wlevel)) {
      ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
      p1 <- lapply(1:wlevel[i], function(i) sp::Polygon(mm[[i]]@polygons[[1]]@Polygons[[1]]@coords))
      ff <- sp::SpatialPolygons(list(sp::Polygons(p1, ID=1)))

      qq[[i]] <- rgeos::gBuffer(rgeos::gUnaryUnion(ff), width=0, id=i)
    }

    rr <- do.call(rbind, qq)
    areas <- sapply(qq, rgeos::gArea)

    qq2 <- sp::SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp[wlevel], 2),
                                        area=areas), match.ID=FALSE)
  } else {

    qq[[1]] <- mm[[1]]
    for (i in 2:length(mm)) {
      ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
      qq[[i]] <- rgeos::gBuffer(rgeos::gUnaryUnion(
        rgeos::gUnion(qq[[i-1]], mm[[i]])), width=0, id=i)
    }
    
    rr <- do.call(rbind, qq)
    areas <- sapply(qq, rgeos::gArea)

    qq2 <- sp::SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp, 2),
                                          area=areas), match.ID=FALSE)
    qq2 <- qq2[!duplicated(cbind(qq2$level, qq2$area)), ]
  }
  
  qq2
}

##' @export
rhrArgs.RhrLoCoH <- function(x, ...) {
  x$args
}


##' @export
print.RhrLoCoH <- function(x, ...) {
  cat("* RhrLoCoH \n")
  cat("* -------- \n")
  cat(sprintf("* Observations     : %s\n", nrow(rhrData(x))))
  cat(sprintf("* Type             : %s\n", rhrTuningParameter(x)$name))
  cat(sprintf("* Type n           : %s\n", rhrTuningParameter(x)$value))
  cat(sprintf("* Auto n           : %s\n", rhrArgs(x)$autoN))
  cat(sprintf("* Levels           : %s\n", paste0(rhrLevels(x), collapse = ", ")))
}
  
##' @export
rhrIsopleths.RhrLoCoH <- function(x, ...) {
  x$res$hr
}

##' @export
rhrArea.RhrLoCoH <- function(x, ...) {
  data.frame(rhrIsopleths(x, ...))
}

##' @export
rhrLevels.RhrLoCoH <- function(x, ...) {
  x$args$levels
}

##' @export
rhrTuningParameter.RhrLoCoH <- function (x, msg = FALSE, digits = 3, ...) {
  if (msg) {
    paste0("Value of tuning parameter ", x$args$type, ": ", round(x$args$n, digits))
  } else {
    list(name = x$args$type,
         value = x$args$n)
  }
}

##' @export
rhrData.RhrLoCoH <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

#' @export
plot.RhrLoCoH <- function(x, title=NULL, ...) {

  long <- lat <- group <- level <- lon <- NULL
  
  ## fortify poly
  tempol <- rhrIsopleths(x, ...)
  tempol@data$id <- rownames(tempol@data)
  tempolPoints <- try(ggplot2::fortify(tempol, region="id"))
  tempolDF <- merge(tempolPoints, tempol@data, by="id")

  points <- rhrData(x, spatial=FALSE)
  names(points)[1:2] <- c("lon", "lat")

  ggplot2::ggplot(tempolDF, ggplot2::aes(x=long, y=lat, group=group, color=factor(level))) + 
    ggplot2::geom_point(data=points, ggplot2::aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) +
    ggplot2::geom_path(size=3, alpha=0.4) + 
    ggplot2::labs(colour=("LoCoH levels"), x="lon", y="lat", title=title) +
    ggplot2::geom_path(size=0.2, colour="black") +
    ggplot2::scale_color_manual(values=terrain.colors(10)) + ggplot2::theme_bw() 
}
