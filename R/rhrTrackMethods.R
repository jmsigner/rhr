#' @export
#' @method head RhrTrack
head.RhrTrack <- function(x, n = 6, ...) {
  utils::head(x$track@data, n)
}

#' @export
#' @method tail RhrTrack
tail.RhrTrack <- function(x, n = 6, ...) {
  tail(x$track@data, n)
}

#' @export
#' @method length RhrTrack
length.RhrTrack <- function(x) {
  nrow(x$track@data)
}

#' @export
#' @method nrow RhrTrack
nrow.RhrTrack <- function(x) {
  nrow(x$track@data)
}

#' @export
#' @method ncol RhrTrack
ncol.RhrTrack <- function(x) {
  ncol(x$track@data)
}

#' @export
#' @method dim RhrTrack
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



# rhrCRS ------------------------------------------------------------------

#' Coordinate Reference System (CRS) of a track
#'
#' Returns the proj4string.
#'
#' @param x Object of class \code{RhrTrack*}
#' @param ... none implemented.
#' @export

rhrCRS <- function(x, ...) {
  UseMethod("rhrCRS")
}

#' @export
rhrCRS.default <- function(x, ...) {
  paste0 ("rhrCRS is not defined for object of class", class(x))
}

##' @export
rhrCRS.RhrTrack <- function(x, ...) {
  sp::proj4string(x$track)
}


# rhrN --------------------------------------------------------------------

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


# Segments ----------------------------------------------------------------

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
  cons <- x$trackConnections
  
  a <- data.frame(x0 = head(cc[, 1], -1), 
                  x1 = tail(cc[, 1], -1), 
                  y0 = head(cc[, 2], -1), 
                  y1 = tail(cc[, 2], -1), 
                  start = rhrTimes(x)[-1], 
                  end = rhrTimes(x)[-rhrN(x)],  
                  distance = cons$distance,
                  direction = cons$direction)
  if (spatial) {
    ## todo: carry forward epsg
    l <- sp::SpatialLines(lapply(1:nrow(a), function(i) with(a[i, ], Lines(list(Line(cbind(c(x0, x1), c(y0, y1)))), as.character(i)))))
    sp::SpatialLinesDataFrame(l, a[, c("distance", "direction", "start", "end")])
  } else {
    a
  }
}


#' @method plot RhrTrack 
#' @export
plot.RhrTrack <- function(x, ...) {
  x <- sp::coordinates(rhrPoints(x))
  plot(x[, 1], x[, 2], xlab  = "x", ylab = "y", asp = 1, type = "l", las = 1, ...)
  points(x[1, 1], x[1, 2], pch = 19, col = "red", cex = 1.5)
  points(x[nrow(x), 1], x[nrow(x), 2], pch = 15, col = "red", cex = 1.5)
  legend("topleft", pch = c(19, 15), col = "red", legend = c("start", "end"))
}

#' @export
#' @method plot RhrTracks
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




#' Plot time
#' 
#' Plot a histogram of times when relocations where recorded
#' @param x Object of class `RhrTrack*`.
#' @param ... none implemented.
#' @export
plotTime <- function(x, ...) {
  UseMethod("plotTime", x)
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

#' Checks if the track has a time stamp.
#'
#' Retunrs \code{TRUE} if the track has time stamps and \code{FALSE} otherwise.
#'
#' @template trackx
#' @template dots
#' 
#' @export
#' @examples 
#' 
#' data(datSH)
#' 
#' ## Create a SpatialPoints objects with the relocation
#' sp <- sp::SpatialPoints(datSH[, 2:3])
#' 
#' ## Parse time
#' time <- lubridate::ymd_hms(paste(datSH$day, datSH$time))
#' 
#' ## Create an object of RhrTrackS (only space)
#' trackS <- rhrTrack(sp)
#' 
#' ## Create an object of RhrTrackST (only space)
#' trackST <- rhrTrack(sp, time)
#' 
#' rhrHasTS(trackS)
#' rhrHasTS(trackST)

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
  lubridate::interval(rhrTrackStart(x), rhrTrackEnd(x))
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
  lubridate::interval(rhrTracksStart(x), rhrTracksEnd(x))
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
#' @method print RhrTrack
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


# rhrBBX ------------------------------------------------------------------



#' Returns the bounding box of a \code{RhrTrack*}
#'
#' @param x Object of class \code{RhrTrack*}.
#' @param spatial Logical value, if \code{TRUE} a \code{SpatialPolygons} object is returned.
#' @param f Numeric value, fraction by which the bounding box is extended.
#' @return A matrix with the bounding box.
#' @template dots
#' @export
#' @examples 
#' 
#' data(trackS)
#' rhrBBX(trackS)
#' 
#' # Extends the range by 5% to each side
#' rhrBBX(trackS, 0.05)
#' 
#' 
#' # Check that is actually works
#' bbx <- rhrBBX(trackS)
#' ext <- apply(bbx, 1, diff)
#' 
#' ext * 1.1
#' apply(rhrBBX(trackS, 0.05), 1, diff)  

rhrBBX <- function(x, f, spatial, ...) {
  UseMethod("rhrBBX")
}

#' @export
rhrBBX.RhrTrack <- function(x, f = 0, spatial = FALSE, ...) {
  b <- rhrExtBBX(sp::bbox(rhrPoints(x)), f)
  if (spatial) {
    b <- rgeos::gEnvelope(sp::SpatialPoints(base::t(b)))
    sp::proj4string(b) <- rhrCRS(x) 
  }
  b
}

#' @export
rhrBBX.RhrTracks <- function(x, spatial = FALSE, f = 0, ...) {
  if (spatial) {
    stop("spatial = TRUE, is not yet implemented for rhrTracks")
  }
  if (length(x) == 1) {
    rhrBBX(x[[1]], f)
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
#' @param y Object of class \code{lubridate::Interval}, also a vector of intervals is possible
#' @template dots
#' @return Object of class \code{rhrTrack*}. 
#' @export

rhrWithinTime <- function(x, y, ...) {
  UseMethod("rhrWithinTime")
}

#' @export
rhrWithinTime.RhrTrackST <- function(x, y, ...) {
  xt <- rhrTimes(x)
  wp <- which(apply(sapply(y, function(i) lubridate::`%within%`(xt, i)), 1, any))
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

#' @export
#' @method summary RhrTracksS
summary.RhrTracksS <- function(x, ...) {
  m <- data.frame(
    id = names(x), 
    n = sapply(x, rhrN)
  )
  row.names(m) <- NULL
  m
}

#' @export
#' @method summary RhrTracksST
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


# Burstify ----------------------------------------------------------------
#' Burstify a track
#'
#' Burstifying assumes, that the track has been regularized before. The smallest time interval is choosen as a reference interval.
#'
#' @param x Object of class \code{RhrTrack*}
#' @param minN Integer, the minimum number of relocations required for a birst. 
#' @template dots
#' @export
rhrBurstify <- function(x, minN = 3, ...) {
 UseMethod("rhrBurstify")
}

#' @export
rhrBurstify.RhrTrackST <- function(x, minN = 3, ...) {
  
  difft <- diff(rhrTimes(x))
  mint <- as.numeric(min(difft))
  
  w <- data.frame(
    x = 1:(nrow(x) - 1), 
    y = 2:nrow(x), 
    tdiff = difft == mint
  )
  
  
  rr <- rle(w$tdiff)
  rr$values <- 1:length(rr$values)
  w$group <- inverse.rle(rr)
  w <- w[w$tdiff, ]
  w <- split(w, w$group)
  
  ## check sufficient points
  w <- w[sapply(w, nrow) >= minN]
  w <- lapply(w, function(xx) unique(unlist(xx[, 1:2])))
  
  w <- lapply(w, function(i) x[i, ])
  names(w) <- 1:length(w)
  class(w) <- c("RhrTracks", "list")
  
  return(w)
}

#' Interpolate missing points
#'
#' Interpolating assumes, that the track has been regularized before. It picks the smallest time interval, an interpolates for all gaps that are larger than the minimum time interval. It is assumed, that all gaps are a multiplie of the smallest time gap. 
#' 
#' Currently, rhrInterpolages loses all attributes and creates a new track.
#'
#' @param x Object of class \code{RhrTrack*}
#' @template dots
#' @export
rhrInterpolate <- function(x, ...) {
  UseMethod("rhrInterpolate")
}

#' @export
rhrInterpolate.RhrTrackST <- function(x, ...) {
  
  times <- rhrTimes(x)
  n <- length(times)
  difft <- as.numeric(diff(times))
  mint <- min(difft)
  
  w <- data.frame(
    x = 1:(nrow(x) - 1), 
    y = 2:nrow(x), 
    diff = difft,
    interpolate = difft != mint, 
    howMany = difft / mint,
    start = times[-n], 
    end = times[-1]
  )
  
  if (all(!w$interpolate)) {
    stop("Nohting to interpolate, all time diffs are equal")
  }
  
  w <- w[w$interpolate, ]
  w <- split(w, 1:nrow(w))
  
  pts <- coordinates(rhrPoints(x))
  
  w <- lapply(w, function(ww) {
    cc <- pts[c(ww$x, ww$y), ]
    cc <- approx(cc[, 1], cc[, 2], n = ww$howMany + 1)
    ww <- data.frame(x = cc$x, y = cc$y, time = seq(ww$start, ww$end, length.out = ww$howMany + 1))
    ww[-c(1, nrow(ww)), ]
  })
  
  ## check sufficient points
  w <- do.call(rbind, w)
  
  xx <- data.frame(
    sp::coordinates(rhrPoints(x)), 
    time = rhrTimes(x)
  )
  
  names(xx)[1:2] <- c("x", "y")
  xx <- rbind(xx, w)
  sp::coordinates(xx) <- ~x+y
  xx <- xx[order(xx$time), ]
  rhr::rhrTrack(xx, time = xx$time)
  
}

# could be nicer methods -------------------------^-----------------------

rhrAnimalById <- function(x, ids) {
  sel <- which(names(x) %in% ids)
  y <- base::`[`(x, sel)
  class(y) <- class(x)
  y
}

