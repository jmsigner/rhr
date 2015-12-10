#' Different Methods to calculate home-range overlaps (in alpha)
#'
#' @param x RhrEst
#' @param y RhrEst
#' @template dots
#' @return \code{data.frame} with the isopleth level and area in units of the coordinate system. 
#' @export
#' @examples 
#' @name overlaps
#' @export
NULL


#' @rdname overlaps
#' export
rhrOverlap <- function(x, y, levels = 95) {
  if (length(levels) > 1) {
    levels <- levels[1]
    warning("Only first elements is used")
  }
  x <- rhrIsopleths(x, levels = levels)
  y <- rhrIsopleths(y, levels = levels)
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
