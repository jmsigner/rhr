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

rhrOverlapBase <- function(x, y) {
  if (rgeos::gIntersects(x, y)) {
    ol <- rgeos::gIntersection(x, y)
    return(rgeos::gArea(ol) / rgeos::gArea(x))
  } else {
    return(0) 
  }
}

#' @rdname overlaps
rhrBA <- function (x, ...) {
  UseMethod ("rhrBA", x )
}



#' @export
rhrBA.default <- function (x, ...) {
  paste0 ("rhrBA is not defined for object of class ", class(x))
}

#' @export
rhrBA.RhrProbEst <- function(x, y) {
  x <- rhrUD(x)
  y <- rhrUD(y)
  r1 <- x[]
  r2 <- y[]
  r1 <- r1 / sum(r1)
  r2 <- r2 / sum(r2)
  ## bhattacharyya's afinity
  sum(sqrt(r1 * r2))
}
