rhrCheckData <- function(dat, returnSP=FALSE) {
  
  if (!returnSP) {
    ## remapped already, no need to worry about it anymore
    if (inherits(dat, "RhrMappedData")) {
      return(data.frame(dat))

      ## data.frame
    } else if(inherits(dat, "data.frame")) {
      if (ncol(dat) > 2) {
        dat <- dat[, 1:2]
        warning("rhrCheckData: xy: more than 2 columns, only the first 2 are used")
      }
      return(dat)

      ## complex numbers
    } else if (inherits(dat, "complex")) {
      return(data.frame(x=Re(dat), y=Im(dat)))

      ## SpatialPoints
    } else if (inherits(dat, "SpatialPoints")) {
      return(data.frame(sp::coordinates(dat)))
    } else {
      stop(paste0("rhrCheckData: dat should be of class data.frame, complex or SpatialPoints. The provided dat is of class ", class(dat)))
    }

  } else {

    ## remapped already, no need to worry about it anymore
    if (inherits(dat, "RhrMappedData")) {
      return(sp::SpatialPoints(data.frame(dat)[, c("lon", "lat")]))

      ## data.frame
    } else if(inherits(dat, "data.frame")) {
      if (ncol(dat) > 2) {
        dat <- dat[, 1:2]
        warning("rhrCheckData: xy: more than 2 columns, only the first 2 are used")
      }
      return(sp::SpatialPoints(dat))

      ## complex numbers
    } else if (inherits(dat, "complex")) {
      return(sp::SpatialPoints(data.frame(x=Re(dat), y=Im(dat))))

      ## SpatialPoints
    } else if (inherits(dat, "SpatialPoints")) {
      return(dat)
    } else {
      stop(paste0("rhrCheckData: dat should be of class data.frame, complex or SpatialPoints. The provided dat is of class ", class(dat)))
    }
  }
}



