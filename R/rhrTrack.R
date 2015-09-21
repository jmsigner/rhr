#' Create objects of class `RhrTrack` to represent animal relocation data.
#' 
#' Creates object of class `RhrTrack*` from `SpatialPoints*` and, if provided, time stamps. 
#' 
#' @param sp SpatialPoints* with the relocation of the animal or \code{RhrMappedData}.
#' @param time Vector with objects of class `POSIXct`, giving the time stamp of relocations.
#' @param duplicates String, indicating how to handle duplicted time stamps. Currently only  "remove" is supported. 
#' @param meta Named list, providing meta information about the animal.
#' @export
#' @example inst/examples/ex-rhrTrack.R

rhrTrack <- function(sp, time, duplicates = "remove", meta) {
  
  # check input -------------------------------------------------------------
  
  if (missing(sp)) {
    stop("sp not provided")
  }
  
  if (missing(time)) {
    time <- NULL
  }
  
  if (missing(meta)) {
    meta <- NULL
  }
  
  if (is(sp, "RhrMappedData")) {
    sp <- sp$dat
    if(!all(is.na(sp$timestamp))) {
      time <- sp$timestamp
    }
  }
  
  if (!inherits(sp, "SpatialPoints")) {
    stop("sp not SpatialPoints")
  }
  
  if (!inherits(time, "POSIXct") & !is.null(time)) {
    stop("time should be of class POSIXct")
  }
  
  if (is(sp, "SpatialPoints")) {
    sp <- SpatialPointsDataFrame(sp, data.frame(ones = rep(1L, length(sp))))
  }
  
  
  # determine track time ----------------------------------------------------
  
  trackType <- if (is.null(time)) {
    c("RhrTrackS", "RhrTrack")
  } else {
    if (length(sp) != length(time)) {
      stop("space and time are of unequal length")
    }
    dt <- as.vector(diff(time))
    if (isTRUE(all.equal(max(dt), min(dt)))) {
      c("RhrTrackSTR", "RhrTrackST", "RhrTrackS", "RhrTrack")
    } else {
      c("RhrTrackST", "RhrTrackS", "RhrTrack")
    }
  }
  
  matDup <- function(x, ...) {
    which(rowSums(apply(x, 2, duplicated)) != ncol(x))
  }
  
  if (trackType[1] == "RhrTrackS") {
    
    track <- sp[matDup(coordinates(sp)), ]
    
    ## adapted from: https://github.com/edzer/trajectories/blob/master/R/Class-Tracks.R#L48
    cc <- coordinates(track)
    ll <- identical(is.projected(track), FALSE)
    distance <- LineLength(cc, ll, FALSE)
    
    if (ll) { # distance is in km, transform to m:
      distance = distance * 1000.0
    }
    
    direction <- trajectories:::directions_ll(cc, ll)
    trackConnections <- data.frame(distance = distance, direction = direction)
    
  } else if (any(trackType == "RhrTrackST")) {
    whichToUse <- matDup(cbind(coordinates(sp), time))
    sp <- sp[whichToUse, ]
    time <- time[whichToUse]
    track <- trajectories::Track(spacetime::STIDF(as(sp, "SpatialPoints"), time, sp@data))
    trackConnections <- track@connections
  } else {
    stop("something went wrong")
  }
    
  
  track <- list(
    track = track, 
    trackConnections = trackConnections, 
    meta = meta
    )
  class(track) <- trackType
  track
}


#' @export
#' @rdname rhrTrack
#' @param id Vector containing the id for each point in \code{sp}.

rhrTracks <- function(sp, ts, id, meta) {
  
  
  if (missing(sp)) {
    stop("sp not provided")
  }
  
  if (missing(meta)) {
    meta <- NULL
  }
  
  if (missing(ts)) {
    ts <- NULL
  }
  
  if (is(sp, "RhrMappedData")) {
    sp <- sp$dat
    if(!all(is.na(sp$timestamp))) {
      ts <- sp$timestamp
    }
  }
  
  if (!inherits(sp, "SpatialPoints")) {
    stop("sp not SpatialPoints")
  }
  
  
  if (missing(id)) {
    rep("Animal_1", n)
  }
  
  n <- length(sp)
  
  if (n != length(id)) {
    stop("rhrTracks: length of points and id do not match")
  }
    
  
  dd <- lapply(split(seq_len(n), id), function(x) sp[x, ])
  
  ## we need at least 2 pts
  
  if(any(sapply(dd, length) < 2)) {
    message("rhrTracks: removed ids with less than 2 relocations.")
  }
  
  if (!is.null(ts)) ts <- split(ts, id)
    
  keep <- which(sapply(dd, length) > 2)
  dd <- dd[keep]
  meta <- meta[keep]
  if (!is.null(ts)) ts <- ts[keep]
  
  tracks <- if (all(is.null(ts))) {
    lapply(1:length(dd), function(i) rhrTrack(dd[[i]], meta = meta[[i]]))
  } else {
    lapply(1:length(dd), function(i) rhrTrack(dd[[i]], ts[[i]], meta = meta[[i]]))
  }
  
  names(tracks) <- names(dd)
  
  cl1 <- sapply(lapply(tracks, class ), "[[", 1)
  cl1 <- if (all(cl1 == cl1[1]) && cl1[1] == "RhrTrackS") {
    "RhrTracksS"
  } else if (all(cl1 == cl1[1]) && cl1[1] == "RhrTrackST") {
    c("RhrTracksS", "RhrTracksST")
  } else if (all(cl1 == cl1[1]) && cl1[1] == "RhrTrackSTR") {
    c("RhrTracksS", "RhrTracksST", "RhrTracksSTR")
  } else {
    NULL
  }
    
  class(tracks) <- c(rev(cl1), "RhrTracks", "list")
  tracks
}
