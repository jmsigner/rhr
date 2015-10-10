##' Estimate Core Area
##'
##' This function estimates core areas for animal home ranges. A core area, is an area of animals home range that is used more intensively.
##' 
##' @param x a RhrHREstimator* object
##' @param method method used to calculate core area. At the moment only powell90 is implemented
##' @param ... none implemented
##' @export
##' @return object of class RhrHRCoreArea
##' @note Core areas are areas within an animals home range that receive greater intensity of use than other areas. Core areas are often used to answer questions of territoriality or habitat selection. Oftentimes core areas are estimated by calculating home ranges at a level of 20 or 50%, i.e. determine a cut off value of the utilization distribution (Laver and Kelly 2008). This approach neglects differences by individuals. 
##' @note Seaman and Powell (1990) presented an area independent method for core area estimation, that does not rely on a predefined isopleth cut off values. This methods estimates the core area by estimating two values for each grid cell of the study area. First for each grid cell the number of relocations that fall within the cell are counted. In the next step the following two values are calculated for each cell:
##' \enumerate{
##' \item Percentage of maximum relative frequency. The grid cell with the highest relative frequency of relocations is assigned a value of 100 %
##' \item Percent of home range, that is the percentage of pixels with a higher relative frequency of relocations. The percentage of maximum relative frequency is then plotted against the percent of home range. The point with the greatest distance to the line with intercept 1 and slope -1 is used as threshold.
##' }

##' @references Peter N. Laver and Marcella J. Kelly. A critical review of home range studies. The Journal of Wildlife Management, 72(1):290-298, 2008
##' @references Erran D. Seaman and Roger A. Powell. Identifying patterns and intensity of home range use. Bears: their biology and management, 243-249, 1990


##' @rdname rhrCoreArea

rhrCoreArea <- function(x, method="seaman90", ...) {
  UseMethod("rhrCoreArea", x)
}

##' @export
rhrCoreArea.default <- function (x , ...) {
  paste0("rhrCoreArea is not defined for object of class ", class(x))
}


##' @export
##' @rdname rhrCoreArea

rhrCoreArea.RhrEst <- function(x, method="powell90", ...) {

  if (!is(x, "RhrEst")) {
    stop("rhrCoreArea: x: not of class RhrHREstimator")
  }

  if (!rhrHasUD(x)) {
    stop("UD is required to calculate core area")
  }

  if (method == "powell90") {
    ## x <- rhrKDE(datSH[, 2:3])
    prob <- rhrUD(x)[] / sum(rhrUD(x)[], na.rm=TRUE)
    pctprob <- prob / max(prob)
    pctrnge <- sapply(prob, function(x) sum(x <= prob)/length(prob))
    dd <- sapply(1:length(pctprob), function(i)
      distancePointLine(pctrnge[i], pctprob[i], 0, 1, 1, 0))

    iso <- 100 - pctrnge[which.max(dd)] * 100
    res <- list(iso=iso,
                method=method)

  }
  return(res)
}

distancePointLine <- function(x, y, x1, y1, x2, y2) {
  ## code from: http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  normalLength <- ((x2 - x1)^2 + (y2 - y1)^2)
  abs((x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)) / normalLength
}
