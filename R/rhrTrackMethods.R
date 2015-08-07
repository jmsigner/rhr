## head
#' @export
head.RhrTrack <- function(x, n = 6) {
  head(x$track@data, n)
}

## tail 
#' @export
tail.RhrTrack <- function(x, n = 6) {
  tail(x$track@data, n)
}


## length
#' @export
length.RhrTrack <- function(x) {
  nrow(x$track@data)
}

## nrow
#' @export
nrow.RhrTrack <- function(x) {
  nrow(x$track@data)
}

## ncol
#' @export
ncol.RhrTrack <- function(x) {
  ncol(x$track@data)
}

## dim
#' @export
dim.RhrTrack <- function(x) {
  dim(x$track@data)
}

#' Extract relocations from a track.
#' 
#' Extract the relocation points from a track and return them as a `SpatialPointsPointsDataframe` object.
#' @param x Object of class `RhrTrack*`.
#' @example inst/examples/ex-rhrPoints.R
#' 
#' @export
rhrPoints <- function(x, ...) {
  UseMethod("rhrPoints", x)
}

#' @export
rhrPoints.RhrTrackS <- function(x, ...) {
  x$track
}

#' @export
rhrPoints.RhrTrackST <- function(x, ...) {
  as(x$track, "SpatialPointsDataFrame")
}

#' Extract time stamps from a track.
#' 
#' Extracts the time stamps from a track and return them as a vector.
#' @param x Object of class `RhrTrack*`.
#' @example inst/examples/ex-rhrTimes.R
#' 
#' @export
rhrTimes <- function(x, ...) {
  UseMethod("rhrTimes", x)
}

#' @export
rhrTimes.RhrTrackST <- function(x, ...) {
  time(x$track)
}


#' Number of relocations in a track.
#'
#' Returns the number of relocations in a track.
#'
#' @param x Object of class \code{RhrTrack*}
#' @export
#' @example inst/examples/ex-rhrN.R
rhrN <- function(x, ...) {
  UseMethod("rhrN", x)
}

##' @export
rhrN.RhrTrack <- function(x) {
  nrow(x)
}

#' Extract segments from a track.
#' 
#' Extract the segments, linear interpolations between two points, from the track.
#' @param x Object of class `RhrTrack*`.
#' @param spatial Logical value, should the result be spatial (SpatialLinesDataFrame).
#' @example inst/examples/ex-rhrSegments.R
#' @export
rhrSegments <- function(x, ...) {
  UseMethod("rhrSegments", x)
}

#' @export
rhrSegments.RhrTrackS <- function(x, spatial = FALSE, ...) {
  
  a <- sp::coordinates(rhrPoints(x))
  cc <- x$trackConnections
  
  n <- nrow(a)
  a <- data.frame(x0 = a[-n, 1], 
                  x1 = a[-1, 1], 
                  y0 = a[-n, 2], 
                  y1 = a[-1, 2], 
                  dist = cc$distance, 
                  direction = cc$direction)
  if (spatial) {
    ## todo: carry forward epsg
    l <- SpatialLines(lapply(1:nrow(a), function(i) with(a[i, ], Lines(list(Line(cbind(c(x0, x1), c(y0, y1)))), as.character(i)))))
    SpatialLinesDataFrame(l, a[, c("dist", "direction")])
  } else {
    a
  }
}

#' @export
rhrSegments.RhrTrackST <- function(x, spatial = FALSE, ...) {
  
  cc <- sp::coordinates(rhrPoints(x))
  
  a <- data.frame(x0 = head(cc[, 1], -1), 
                  x1 = tail(cc[, 1], -1), 
                  y0 = head(cc[, 2], -1), 
                  y1 = tail(cc[, 2], -1), 
                  x$trackConnections)
  if (spatial) {
    ## todo: carry forward epsg
    l <- SpatialLines(lapply(1:nrow(a), function(i) with(a[i, ], Lines(list(Line(cbind(c(x0, x1), c(y0, y1)))), as.character(i)))))
    SpatialLinesDataFrame(l, a[, c("dist", "direction")])
  } else {
    a
  }
}



#' @export
plot.RhrTrack <- function(x, ...) {
  x <- coordinates(rhrPoints(x))
  plot(x[, 1], x[, 2], xlab  = "", ylab = "", asp = 1, type = "l", las = 1, ...)
  points(x[1, 1], x[1, 2], pch = 19, col = "red", cex = 1.5)
  points(x[nrow(x), 1], x[nrow(x), 2], pch = 15, col = "red", cex = 1.5)
  legend("topleft", pch = c(19, 15), col = "red", legend = c("start", "end"))
}

## print
prepLine <- function(field, val) {
  field <- if (stringr::str_length(field) > 30) {
    paste0(string::str_sub(field, 1, 27), "...")
  } else {
    field
  }
  field <- stringr::str_pad(field, width = 30, side = "right")
  val <- stringr::str_pad(val, width = 20, side = "right")
  paste0("## ", field, ":     ", val, "\n")
}

sep <- function(n = 57, char = "=") {
  paste0("## ", paste0(rep(char, n - 3), collapse = ""), "\n")
  
}

oneLine <- function(x) {
  paste0("## ", x, "\n")
}

#' Checks if the track is regularly spaced in time.
#'
#' Returns `TRUE` if time differences between consecutive relocations are equal.
#'
#' @param x Object of class \code{RhrTrackS*}
#' @export
#' @example inst/examples/ex-rhrIsRegular.R
is.regular <- function(x) {
  UseMethod("is.regular", x)
}

#' @export
is.regular.RhrTrack <- function(x) {
  if (is(x, "RhrTrackSTR")) {
    TRUE
  } else {
    FALSE
  }
}

#' Checks if the track has time stamps
#'
#' Retunrs `TRUE` if the track has time stamps.
#'
#' @param x Object of class \code{RhrTrack*}
#' @export
#' @example inst/examples/ex-rhrHasTS.R

rhrHasTS <- function(x) {
  UseMethod("rhrHasTS")
}

#' @export
rhrHasTS.RhrTrack <- function(x) {
  inherits(x, "RhrTrackST")
}


#' Working with timestamps
#'
#' Functions to deal with time stamps of a track. 
#' 
#' `rhrTrackStart` returns the first the time stamp of the beginning of a track. `rhrTrackEnd` returns the last time stamp and `rhrTrackSpan` the time span of a track.
#'
#' @param x Object of class \code{RhrTrack*}
#' @example inst/examples/ex-rhrTrackTime.R
#' @name rhrTrackTime
 
#' @export
#' @rdname rhrTrackTime
rhrTrackStart <- function(x) {
  UseMethod("rhrTrackStart")
}

#' @export
rhrTrackStart.RhrTrackST <- function(x) {
  min(rhrTimes(x))
}

#' @rdname rhrTrackTime
#' @export
rhrTrackEnd <- function(x) {
  UseMethod("rhrTrackEnd")
}

#' @export
rhrTrackEnd.RhrTrackST <- function(x) {
  max(rhrTimes(x))
}

#' @rdname rhrTrackTime
#' @export
rhrTrackSpan <- function(x) {
  UseMethod("rhrTrackSpan")
}

#' @export
rhrTrackSpan.RhrTrackST <- function(x) {
  lubridate::new_interval(rhrTrackStart(x), rhrTrackEnd(x))
}

#' @rdname rhrTrackTime
#' @export
rhrTrackBy <- function(x) {
  UseMethod("rhrTrackBy")
}

#' @export
rhrTrackBy.RhrTrackSTR <- function(x) {
  xx <- diff(rhrTimes(x))
  units(xx) <-"secs"
  as.numeric(xx[1])
}


#' @export
rhrTrackData <- function(x) {
  UseMethod("rhrTrackData")
}

#' @export
rhrTrackData.RhrTrack <- function(x) {
  as.data.frame(x$track)
}



#' @export
print.RhrTrack <- function(x) {
  
  attr <- rhrTrackData(x)
  segs <- rhrSegments(x, spatial = FALSE)
  
  cat("\n", 
      sep(), 
      oneLine(paste0("Object of class ",  class(x)[1])), 
      sep(), 
      "\n", 
      oneLine("Summary"), 
      sep(char = "-"), 
      prepLine("Number of relocation", rhrN(x)), 
      if (is.regular(x)) prepLine("Missing relocations", rhrN(x)), 
      prepLine("Traj with time", rhrHasTS(x)), 
      prepLine("Regular track", is.regular(x)), 
      if (rhrHasTS(x)) prepLine("Start", as.character(rhrTrackStart(x))), 
      if (rhrHasTS(x)) prepLine("End", as.character(rhrTrackEnd(x))), 
      if (is.regular(x)) prepLine("By", 
                                  as.character(lubridate::seconds_to_period(rhrTrackBy(x)))), 
      "\n", 
      oneLine("Track data"), 
      sep(char = "-"), 
      apply(
        cbind(names(attr),
              sapply(sapply(attr, class), paste0, collapse = ",")), 1, 
        function(xx) prepLine(xx[1], xx[2])) ,
      sep(), 
      "\n", 
      oneLine("Segments data"), 
      sep(char = "-"), 
      apply(
        cbind(names(segs),
              sapply(sapply(segs, class), paste0, collapse = ",")), 1, 
        function(xx) prepLine(xx[1], xx[2])) ,
      sep(), 
      "\n"
  )
}

#' Split a track
#'
#' Returns a list of subtracks.
#'
#' @param x Object of class \code{RhrTrack*}
#' @param f Vector indicating the grouping
#' @param minN Integer, the minimum number of relocations required in a group in order to create a new track. 
#' @export
#' @example inst/examples/ex-rhrSplit.R
rhrSplit <- function(x, ...) {
  UseMethod("rhrSplit")
}

#' @export
rhrSplit.RhrTrackS <- function(x, f, minN = 3) {
  
  if (nrow(x) != length(f)) {
    stop("x and f are not of the same length")
  }
  
  pts <- split(x$track, f)
  lapply(pts, function(y) if (nrow(y) >= minN) rhrTrack(y) else NULL)
  
}

#' @export
rhrSplit.RhrTrackST <- function(x, f, minN = 3) {
  
  if (nrow(x) != length(f)) {
    stop("x and f are not of the same length")
  }
  
  pts <- split(x$track, f)
  lapply(pts, function(y) if (nrow(y) >= minN) rhrTrack(as(y, "SpatialPointsDataFrame"), time(y)) else NULL)
  
}
