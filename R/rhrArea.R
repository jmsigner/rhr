#' Area of a home-range estimate
#'
#' \code{rhrArea} returns the area of a home-range estimate. 
#'
#' For geometric estimators the isopleth levels at which the area is calculated is
#' already determined when the estimate is performed. Probabilistic estimators take
#' an additional argument, \code{levels}, that determines at which isopleth level
#' of the UD the area is evaluated. 
#'
#' @template RhrEst
#' @template dots
#' @return \code{data.frame} with the isopleth level and area in units of the coordinate system. 
#' @export
#' @examples 
#' 
#' data(datSH)
#' 
#' # Geometric estimator
#' mcp <- rhrMCP(datSH[, 2:3])
#' rhrArea(mcp)
#' 
#' mcp <- rhrMCP(datSH[, 2:3], levels = seq(50, 95, 5))
#' rhrArea(mcp)
#' 
#' # Note the levels argument does not change anything for geometric 
#' # estimators such mcp
#' rhrArea(mcp, levels = 44)
#'
#' 
#' ## Probabilistic estimator
#' kde <- rhrKDE(datSH[, 2:3])
#' rhrArea(kde)
#' 
#' ## at a different isopleth level
#' rhrArea(kde, levels = 50)
#' 
#' ## or at sequence of isopleth levels
#' rhrArea(kde, level = seq(50, 95, 5))

rhrArea <- function (x , ...) {
  UseMethod ("rhrArea", x)
}



##' @export
rhrArea.default <- function (x , ...) {
  paste0 ("rhrArea is not defined for object of class ", class(x))
}
