#' @export
head.RhrTrack <- function(x, n = 6, ...) {
  base::head(x$track@data, n)
}

#' @export
tail.RhrTrack <- function(x, n = 6, ...) {
  tail(x$track@data, n)
}

#' @export
length.RhrTrack <- function(x) {
  nrow(x$track@data)
}

#' @export
nrow.RhrTrack <- function(x) {
  nrow(x$track@data)
}

#' @export
ncol.RhrTrack <- function(x) {
  ncol(x$track@data)
}

#' @export
dim.RhrTrack <- function(x) {
  dim(x$track@data)
}


#' Extract relocations from a track.
#' 
#' Extract the relocation points from a track and return them as a `SpatialPointsPointsDataframe` object.
#' @param x Object of class `RhrTrack*`.
#' @param ... none implemented.
#' @export
rhrPoints <- function(x, ...) {
  UseMethod("rhrPoints", x)
}

#' @export
rhrPoints.RhrTrackS <- function(x, ...) {
  x$track
}

#' @export
rhrPoints.RhrTracks <- function(x, ...) {
  pts <- do.call(rbind, lapply(x, rhrPoints))
  pts$id <- rep(names(x), rhrN(x))
  pts
}

#' @export
rhrPoints.RhrTrackST <- function(x, ...) {
  as(x$track, "SpatialPointsDataFrame")
}


#' Extract meta data from a track
#' @param x list.
#' @param ... none implemented.
#' 
#' @export
rhrMeta <- function(x, ...) {
  UseMethod("rhrMeta", x)
}

#' @export
rhrMeta.RhrTrack <- function(x, ...) {
  x$meta
}

#' @export
rhrMeta.RhrTracks <- function(x, ...) {
  lapply(x, rhrMeta)
}

#' Extract time stamps from a track.
#' 
#' Extracts the time stamps from a track and return them as a vector.
#' @param x Object of class `RhrTrack*`.
#' @param ... none implemented.
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
#' @param ... none implemented.
#' @export

rhrN <- function(x, ...) {
  UseMethod("rhrN")
}

#' @export
rhrN.default <- function(x, ...) {
  paste0 ("rhrN is not defined for object of class", class(x))
}

##' @export
rhrN.RhrTrack <- function(x, ...) {
  nrow(x$track)
}

##' @export
rhrN.RhrTracks <- function(x, ...) {
  sapply(x, rhrN)
}

#' Extract segments from a track.
#' 
#' Extract the segments, linear interpolations between two points, from the track.
#' @param x Object of class `RhrTrack*`.
#' @param spatial Logical value, should the result be spatial (SpatialLinesDataFrame).
#' @param ... none implemented.
#' @export
rhrSegments <- function(x, spatial, ...) {
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
                  distance = cc$distance, 
                  direction = cc$direction)
  if (spatial) {
    ## todo: carry forward epsg
    l <- sp::SpatialLines(lapply(1:nrow(a), function(i) with(a[i, ], sp::Lines(list(sp::Line(cbind(c(x0, x1), c(y0, y1)))), as.character(i)))))
    sp::SpatialLinesDataFrame(l, a[, c("distance", "direction")])
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
    l <- sp::SpatialLines(lapply(1:nrow(a), function(i) with(a[i, ], Lines(list(Line(cbind(c(x0, x1), c(y0, y1)))), as.character(i)))))
    sp::SpatialLinesDataFrame(l, a[, c("distance", "direction", "duration", "speed")])
  } else {
    a
  }
}


#' Prints RhrTrack object.
#' 
#' @param x RhrTrack object.
#' @template dots
#' @export
plot.RhrTrack <- function(x, ...) {
  x <- sp::coordinates(rhrPoints(x))
  plot(x[, 1], x[, 2], xlab  = "", ylab = "", asp = 1, type = "l", las = 1, ...)
  points(x[1, 1], x[1, 2], pch = 19, col = "red", cex = 1.5)
  points(x[nrow(x), 1], x[nrow(x), 2], pch = 15, col = "red", cex = 1.5)
  legend("topleft", pch = c(19, 15), col = "red", legend = c("start", "end"))
}

#' Prints RhrTracks object.
#' 
#' @param x RhrTracks object.
#' @template dots
#' @export
plot.RhrTracks <- function(x, ...) {
  x <- rhrPoints(x)
  ids <- x$id
  n <- length(unique(ids))
  cols <- rainbow(n)  
  
  x <- data.frame(sp::coordinates(x))
  plot(x[, 1], x[, 2], xlab  = "x", ylab = "y", asp = 1, type = "n", las = 1, ...)
  xs <- split(x, ids)
  for(i in seq_len(n)) {
    xx <- xs[[i]]
    lines(xx[, 1], xx[, 2], col = cols[i])
    points(xx[1, 1], xx[1, 2], pch = 19, col = cols[i], cex = 1.5)
    points(xx[nrow(xx), 1], xx[nrow(xx), 2], pch = 15, col = cols[i], cex = 1.5)
  }
  legend("topleft", pch = c(19, 15), legend = c("start", "end"))
}

## print
prepLine <- function(field, val) {
  field <- if (stringr::str_length(field) > 30) {
    paste0(stringr::str_sub(field, 1, 27), "...")
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
#' @param ... none implemented.
#' @export
is.regular <- function(x, ...) {
  UseMethod("is.regular", x)
}

#' @export
is.regular.RhrTrack <- function(x, ...) {
  if (is(x, "RhrTrackSTR")) {
    TRUE
  } else {
    FALSE
  }
}

#' Checks if the track has time stamps
#'
#' Retunrs \code{TRUE} if the track has time stamps.
#'
#' @param x Object of class \code{RhrTrack*}
#' @param ... none implemented.
#' @export

rhrHasTS <- function(x, ...) {
  UseMethod("rhrHasTS")
}

#' @export
rhrHasTS.RhrTrack <- function(x, ...) {
  inherits(x, "RhrTrackST")
}


#' Working with timestamps
#'
#' Functions to deal with time stamps of a track. 
#' 
#' `rhrTrackStart` returns the first the time stamp of the beginning of a track. `rhrTrackEnd` returns the last time stamp and `rhrTrackSpan` the time span of a track.
#'
#' @param x Object of class \code{RhrTrack*}
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


#' @export
#' @rdname rhrTrackTime
rhrTracksStart <- function(x) {
  UseMethod("rhrTracksStart")
}

#' @export
rhrTracksStart.RhrTracksST <- function(x) {
  min(do.call(base::c, lapply(x, rhrTrackStart)))
}

#' @rdname rhrTrackTime
#' @export
rhrTracksEnd <- function(x) {
  UseMethod("rhrTracksEnd")
}

#' @export
rhrTracksEnd.RhrTracksST <- function(x) {
  max(do.call(base::c, lapply(x, rhrTrackEnd)))
}

#' @rdname rhrTrackTime
#' @export
rhrTracksSpan <- function(x) {
  UseMethod("rhrTracksSpan")
}

#' @export
rhrTracksSpan.RhrTracksST <- function(x) {
  lubridate::new_interval(rhrTracksStart(x), rhrTracksEnd(x))
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

#' Returns data of a RhrTrack* object.
#' 
#' @param x RhrTrack* object.
#' @param ... ignored
#' @export
rhrTrackData <- function(x, ...) {
  UseMethod("rhrTrackData")
}

#' @export
rhrTrackData.RhrTrack <- function(x, ...) {
  as.data.frame(x$track)
}



#' @export
print.RhrTrack <- function(x, ...) {
  
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
#' @param ... none implemented.
#' @export
rhrSplit <- function(x, f, minN, ...) {
  UseMethod("rhrSplit")
}

#' @export
rhrSplit.RhrTrackS <- function(x, f, minN = 3, ...) {
  
  if (nrow(x) != length(f)) {
    stop("x and f are not of the same length")
  }
  
  pts <- split(x$track, f)
  lapply(pts, function(y) if (nrow(y) >= minN) rhrTrack(y) else NULL)
  
}

#' @export
rhrSplit.RhrTrackST <- function(x, f, minN = 3, ...) {
  
  if (nrow(x) != length(f)) {
    stop("x and f are not of the same length")
  }
  
  pts <- split(x$track, f)
  lapply(pts, function(y) if (nrow(y) >= minN) rhrTrack(as(y, "SpatialPointsDataFrame"), time(y)) else NULL)
  
}

#' Returns the bounding box of a \code{RhrTrack*}
#'
#' @param x Object of class \code{RhrTrack*}.
#' @param f Numeric value, fraction by which the bounding box is extended.
#' @return A matrix with the bounding box.
#' @param ... none implemented.
#' @export
rhrBBX <- function(x, f, ...) {
  UseMethod("rhrBBX")
}

#' @export
rhrBBX.RhrTrack <- function(x, f = 0, ...) {
  rhrExtBBX(sp::bbox(rhrPoints(x)), f)
}

#' @export
rhrBBX.RhrTracks <- function(x, f = 0, ...) {
  if (length(x) == 1) {
    rhrBBX(x[[1]])
  } else {
    abbx <- simplify2array(lapply(x, function(y) sp::bbox(rhrPoints(y))))
    rhrExtBBX(cbind(apply(abbx[, 1, ], 1, min), apply(abbx[, 2, ], 1, max)), f)
  }
}

rhrExtBBX <- function(x, f) {
  x[1, ] <- grDevices::extendrange(r = x[1, ], f = f)
  x[2, ] <- grDevices::extendrange(r = x[2, ], f = f)
  x
}



# Extract methods ---------------------------------------------------------

#' @export
"[.RhrTrackS" <- function(x, i, j) {
  if(!missing(j)) {
    warning("j is ignored")
  }
  rhrTrack(rhrPoints(x)[i, ])
}

#' @export
"[.RhrTrackST" <- function(x, i, j) {
  if(!missing(j)) {
    warning("j is ignored")
  }
  rhrTrack(rhrPoints(x)[i, ], rhrTimes(x)[i])
}

# rhrWithin ---------------------------------------------------------------

#' Spatial subset of a track
#'
#' Performs a subset of a track based on the spatial position of relocations. Only relocations that are within a polygon (\code{SpatialPolygons*}) are selected a new track is created. 
#' @param x Object of class \code{RhrTrack*}.
#' @param y Object of class \code{SpatialPolygons*}
#' @param ... none implemented.
#' @return Object of class \code{rhrTrack*}. 
#' @export

rhrWithin <- function(x, y, ...) {
  UseMethod("rhrWithin")
}

#' @export
rhrWithin.RhrTrack <- function(x, y, ...) {
  wp <- which(rgeos::gWithin(rhrPoints(x), y, byid = TRUE))
  if (length(wp) > 1) {
    x[wp, ]
  }
}

#' @export
rhrWithin.RhrTracks <- function(x, y, ...) {
  x <- lapply(x, rhrWithin, y)
  x <- x[!sapply(x, is.null)]
  
  class(x) <- c(
    if (all(sapply(x, is, "RhrTrackS"))) "RhrTracksS",
    if (all(sapply(x, is, "RhrTrackST"))) "RhrTracksST", 
    if (all(sapply(x, is, "RhrTrackSTR"))) "RhrTracksSTR", 
    "RhrTracks", "list")
  x
}


bbx2sp <- function(x) {
  rgeos::gEnvelope(sp::SpatialPoints(cbind(x[1, ], x[2, ])))
}


#' Temporal subset of a track
#'
#' Performs a subset of a track based on a provided time interval. Only relocations that are within the time interval are selected and (a) new track(s) is created. 
#' @param x Object of class \code{RhrTrackST*} or code \code{RhrTracksST}.
#' @param y Object of class \code{lubridate::Interval}
#' @template dots
#' @return Object of class \code{rhrTrack*}. 
#' @export

rhrWithinTime <- function(x, y, ...) {
  UseMethod("rhrWithinTime")
}

#' @export
rhrWithinTime.RhrTrackST <- function(x, y, ...) {
  wp <- which(lubridate::`%within%`(rhrTimes(x), y)) 
  if (length(wp) > 1) {
    x[wp, ]
  }
}

#' @export
rhrWithinTime.RhrTracksST <- function(x, y, ...) {
  x <- lapply(x, rhrWithinTime, y)
  x <- x[!sapply(x, is.null)]
  
  class(x) <- c(
    if (all(sapply(x, is, "RhrTrackS"))) "RhrTracksS",
    if (all(sapply(x, is, "RhrTrackST"))) "RhrTracksST", 
    if (all(sapply(x, is, "RhrTrackSTR"))) "RhrTracksSTR", 
    "RhrTracks", "list")
  x
}

summary.RhrTracksS <- function(x, ...) {
  m <- data.frame(
    id = names(x), 
    n = sapply(x, rhrN)
  )
  row.names(m) <- NULL
  m
}

summary.RhrTracksST <- function(x, ...) {
  m <-  data.frame(
    id = names(x), 
    n = sapply(x, rhrN), 
    start = sapply(x, function(x) as.character(rhrTrackStart(x))), 
    end = sapply(x, function(x) as.character(rhrTrackEnd(x)))
  )
  row.names(m) <- NULL
  m
}


# could be nicer methods --------------------------------------------------

rhrAnimalById <- function(x, ids) {
  sel <- which(names(x) %in% ids)
  y <- base::`[`(x, sel)
  class(y) <- class(x)
  y
}

