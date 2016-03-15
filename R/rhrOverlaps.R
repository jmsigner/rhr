#' Different Methods to calculate home-range overlaps (in alpha)
#'
#' @param x RhrEst
#' @param y RhrEst
#' @template dots
#' @return \code{data.frame} with the isopleth level and area in units of the coordinate system. 
#' @name overlaps
NULL

#' @rdname overlaps
#' @export
rhrOverlap <- function (x, ...) {
  UseMethod("rhrOverlap", x )
}

#' @export
rhrOverlap.RhrProbEst <- function(x, y, levels = 95) {
  if (length(levels) > 1) {
    levels <- levels[1]
    warning("Only first elements is used")
  }
  levels = 95
  x <- rhrIsopleths(x, levels = levels)
  y <- rhrIsopleths(y, levels = levels)
  rhrOverlapBase(x, y)
}

#' @export
rhrOverlap.RhrGeoEst <- function(x, y, levels = 95) {
  if (length(levels) > 1) {
    levels <- levels[1]
    warning("Only first elements is used")
  }
  x <- rhrIsopleths(x, levels = levels)
  y <- rhrIsopleths(y, levels = levels)
  if (levels %in% x$level & levels %in% y$level) {
    rhrOverlapBase(x[x$level == levels, ], y[y$level == levels, ])
  } else {
    stop("level not estimated, rerun est")
  }
}


#' @export
rhrOverlap.list <- function(x) {
  
  res <- diag(length(x)) 
  
  for (i in 1:length(x)) {
    for (j in 1:length(x)) {
      if (j > i) {
        res[i, j] <- res[j, i] <- rhrOverlap(x[[i]], x[[j]])
      }
    }
  }
  colnames(res) <- rownames(res) <- names(x)
  res
}

rhrOverlapBase <- function(x, y) {
  if (rgeos::gIntersects(x, y)) {
    ol <- rgeos::gIntersection(x, y)
    return(rgeos::gArea(ol) / rgeos::gArea(x))
  } else {
    return(0) 
  }
}

#' @rdname overlaps
#' @export
rhrBA <- function (x, ...) {
  UseMethod ("rhrBA", x )
}

#' @export
rhrBA.RhrProbEst <- function(x, y) {
  x <- rhrUD(x)
  y <- rhrUD(y)
  
  if (!identical(raster::extent(x), raster::extent(y))) {
    stop("x and y do not have an identical extent")
  }
  r1 <- x[]
  r2 <- y[]
  r1 <- r1 / sum(r1)
  r2 <- r2 / sum(r2)
  ## bhattacharyya's afinity
  sum(sqrt(r1 * r2))
}

#' @export
rhrBA.list <- function(x) {
  
  if (!all(sapply(x, inherits, "RhrProbEst"))) {
    stop("list can only include obj of RhrProbEst")
  }
  
  res <- diag(length(x)) 
  
  for (i in 1:length(x)) {
    for (j in 1:length(x)) {
      if (j > i) {
        res[i, j] <- res[j, i] <- rhrBA(x[[i]], x[[j]])
      }
    }
  }
  colnames(res) <- rownames(res) <- names(x)
  res
}

#' @export
rhrBA.RhrProbEst <- function(x, y) {
  x <- rhrUD(x)
  y <- rhrUD(y)
  
  if (!identical(raster::extent(x), raster::extent(y))) {
    stop("x and y do not have an identical extent")
  }
  r1 <- x[]
  r2 <- y[]
  r1 <- r1 / sum(r1)
  r2 <- r2 / sum(r2)
  ## bhattacharyya's afinity
  sum(sqrt(r1 * r2))
}



