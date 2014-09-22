rhrProjString <- function(xy, projString=NULL) {

  if (FALSE) {
    projString <- NULL
  }

  isProj <- tryCatch(sp::is.projected(xy), error=function(e) NA)
  if (!is.na(isProj)) {
    return(sp::CRS(sp::proj4string(xy)))
  } else if (!is.null(projString)) {
    if (!is.na(projString)) {
      projString <- tryCatch(sp::CRS(projString),  # stores the projection information
                             error=function(e) {
                               warning("Failed to create proj4string, result will not be projected")
                               sp::CRS(as.character(NA_character_))
                             }
                             )
    } else {
      projString <- sp::CRS(NA_character_)
    }
  } else {
    projString <- sp::CRS(NA_character_)
  }
  return(projString)
}
