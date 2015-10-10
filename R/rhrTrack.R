#' Create objects of class `RhrTrack` to represent animal relocation data.
#' 
#' Creates object of class `RhrTrack*` from `SpatialPoints*` and, if provided, time stamps. 
#' 
#' @param sp SpatialPoints* with the relocation of the animal or \code{RhrMappedData}.
#' @param time Vector with objects of class `POSIXct`, giving the time stamp of relocations.
#' @param duplicates String, indicating how to handle duplicted time stamps. Currently only  "remove" is supported. 
#' @param meta Named list, providing meta information about the animal.
#' @export

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
    sp <- sp::SpatialPointsDataFrame(sp, data.frame(ones = rep(1L, length(sp))))
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
    
    track <- sp[matDup(sp::coordinates(sp)), ]
    
    ## adapted from: https://github.com/edzer/trajectories/blob/master/R/Class-Tracks.R#L48
    cc <- sp::coordinates(track)
    ll <- identical(sp::is.projected(track), FALSE)
    distance <- sp::LineLength(cc, ll, FALSE)
    
    if (ll) { # distance is in km, transform to m:
      distance = distance * 1000.0
    }
    
    direction <- directions_ll(cc, ll)
    trackConnections <- data.frame(distance = distance, direction = direction)
    
  } else if (any(trackType == "RhrTrackST")) {
    whichToUse <- matDup(cbind(sp::coordinates(sp), time))
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
#' @param ts Vector containing the timestamp for each point in \code{sp}.
#' @param metas List of list with meta data, length of \code{metas} should be the same as the number of levels in \code{id}.

rhrTracks <- function(sp, ts, id, metas) {
  
  
  if (missing(sp)) {
    stop("sp not provided")
  }
  
  if (missing(metas)) {
    metas <- NULL
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
  metas <- metas[keep]
  if (!is.null(ts)) ts <- ts[keep]
  
  tracks <- if (all(is.null(ts))) {
    lapply(1:length(dd), function(i) rhrTrack(dd[[i]], meta = metas[[i]]))
  } else {
    lapply(1:length(dd), function(i) rhrTrack(dd[[i]], ts[[i]], meta = metas[[i]]))
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

# this is take from trjaectories:::directions_ll 
directions_ll <- function (cc, ll) 
{
  if (!ll) {
    dcc = matrix(apply(cc, 2, diff), ncol = ncol(cc))
    ((atan2(dcc[, 1], dcc[, 2])/pi * 180) + 360)%%360
  }
  else {
    cc = cc * pi/180
    lat1 = head(cc[, 2], -1)
    lat2 = tail(cc[, 2], -1)
    lon1 = head(cc[, 1], -1)
    lon2 = tail(cc[, 1], -1)
    dlon = lon2 - lon1
    az = atan2(sin(dlon) * cos(lat2), cos(lat1) * sin(lat2) - 
                 sin(lat1) * cos(lat2) * cos(dlon))
    ((az/pi * 180) + 360)%%360
  }
}
