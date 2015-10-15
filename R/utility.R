getEPSG <- function(xy) {
  if (inherits(xy, "SpatialPoints")) {
    sp::proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    sp::proj4string(xy$dat) 
  } else if (is(xy, "RhrTrack")) {
    sp::proj4string(rhrPoints(xy))
  } else {
    sp::CRS(NA_character_)
  }
}
