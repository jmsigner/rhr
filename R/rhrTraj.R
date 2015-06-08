##' Constructur for a Trajectory
##'
##' Creates an object of `RhrTraj*` from `SpatialPoints`. 
##'
##' @param sp Object of class SpatialPoints, the relocation. If `sp` is an object of `SpatialPointsDataFrame`, point attributes are carried forward. 
##' @param time Object of class `POSIXct`, the time when the relocations were recorded. If `time` is NULL without time stamps is created.
##' @return Object of class \code{RhrTrajS*}
##' @export
##' @example inst/examples/rhrTraj.R
##' 
rhrTraj <- function(sp, time = NULL) {
  
  ## check if we have a Spatial*DF
  if (!inherits(sp, "SpatialPoints")) {
    stop("Spatial Points required")
  }
  
  ## Check for time
  hasTS <- FALSE
  if (inherits(time, "POSIXct")) {
    hasTS <- TRUE
  } 
  
  ## Attribute data
  n <- length(sp)
  
  ## ID
  attrDat <-  if (is(sp, "SpatialPointsDataFrame")) {
    data.frame(slot(sp, "data"))
  } else {
    data.frame(id = 1)
  }
  
  traj <- if (hasTS) {
    spacetime::STIDF(as(sp, "SpatialPoints"), time, attrDat)
  } else {
    sp::SpatialPointsDataFrame(as(sp, "SpatialPoints"), attrDat)
  }
  
  traj <- rhrTrajConstructor(traj) 
  return(traj)
}


rhrTrajConstructor <- function(traj) {
  ## traj
  if (nrow(traj) < 2) {
    stop("A trajectory needs at least two relocations")
  }
  
  traj <- structure(
    list(traj = traj, 
         properties = NULL, 
         time = time
    ), 
    class = c(if (inherits(traj, "STI")) "RhrTrajST" else  "RhrTrajS", 
              "RhrTraj", "list"))
  
  traj[["segments"]] <- rhrCalcTrackStats(traj)
  return(traj)
}

rhrTrajConstructorSTR <- function(traj1, traj2) {
  
  ## traj1 should be a data.frame with x and y coords in the first two columns, 
  ## the timestamp in the third column, number of the new point in the frouth
  ## column and the difference to the old point in the fifth column. 
  
  ## traj2 has the same n of rows, and contains additional attribute data
  
  ## traj
  ## traj1 and traj2 are both ordered by time
  if (nrow(traj1) < 2) {
    stop("A trajectory needs at least two relocations")
  }
  
  traj1 <- traj1[order(traj1$rhrTrajTime), ]
  traj2 <- traj2[order(traj2$rhrTrajTime), ]
  
  
  whichComplete <- which(complete.cases(traj1))
  
  xx <- spacetime::STIDF(SpatialPoints(traj1[whichComplete, 1:2]), 
                         traj1[whichComplete, 3], 
                         traj2[whichComplete, "rhrTrajTime", drop = FALSE])
  
  
  traj <- structure(
    list(traj = xx, 
         trajFull = traj1, 
         properties = list(from = min(traj1[, 3]), 
                           to = max(traj1[, 3]), 
                           by = as.integer(traj1[2, 3]) - as.integer(traj1[1, 3])), 
         attribute = traj2
    ), 
    class = c("RhrTrajSTR", "RhrTrajST", "RhrTraj", "list"))
  traj[["segments"]] <- rhrCalcTrackStats(traj)
  
  return(traj)
}


## head
##' @export
head.RhrTraj <- function(x, n = 6) {
  head(rhrTrajAttribute(x), n)
}


## tail
##' @export
tail.RhrTraj <- function(x, n = 6) {
  tail(rhrTrajAttribute(x), n)
}

# Extract methods ---------------------------------------------------------
##' @export
"[.RhrTraj" <- function(x, i, j) {
  x <- rhrTrajLocations(x)
  if (missing(i)) {
    i <- 1:nrow(x)
  }
  if (missing(j)) {
    j <- 1:ncol(x)
  }
  x <- `[`(x, i, j, drop = FALSE)
  rhrTrajConstructor(x)
}


##' @export
"[.RhrTrajSTR" <- function(x, i, j) {
  stop("No `[` method implemented for RhrTrajSTR")
}


##' @export
"[<-.RhrTraj" <- function(x, i, j, value) {
  
  xx <- rhrTrajAttribute(x)
  x <- rhrTrajLocations(x)
  
  if (missing(i)) {
    i <- 1:nrow(xx)
  }
  if (missing(j)) {
    j <- ncol(xx) + 1
  }
  
  xx <- `[<-`(xx, i, j, value)
  
  slot(x, "data", check = FALSE) <- xx
  x <- rhrTrajConstructor(x)
  return(x)
}

##' @export
"[<-.RhrTrajSTR" <- function(x, i, j, value) {
  stop("No `[` method implemented for RhrTrajSTR")
}

## nrow, ncol, dim

##' @export
nrow.RhrTraj <- function(x) {
  nrow(rhrTrajAttribute(x))
}

##' @export
ncol.RhrTraj <- function(x) {
  ncol(rhrTrajAttribute(x))
}

##' @export
dim.RhrTraj <- function(x) {
  dim(rhrTrajAttribute(x))
}

## `$` and `$<-` 
##' @export
"$.RhrTraj" <- function(x, name) {
  x <- rhrTrajAttribute(x)
  x[[name]]
}

##' @export
"$.RhrTrajSTR" <- function(x, name) {
  x <- blowupTraj(x)
  x$traj2[[name]]
}

##' @export
"$<-.RhrTraj" <- function(x, name, value) {
  
  xx <- rhrTrajAttribute(x)
  x <- rhrTrajLocations(x)
  
  i <- 1:nrow(xx)
  j <- name
  
  xx <- `[<-`(xx, i, j, value)
  
  slot(x, "data", check = FALSE) <- xx
  x <- rhrTrajConstructor(x)
  return(x)
}


##' @export
"$<-.RhrTraj" <- function(x, name, value) {
  
  x <- blowupTraj(x)
  xx <-x$traj2
  x <- x$traj1
  
  i <- 1:nrow(xx)
  j <- name
  
  xx <- `[<-`(xx, i, j, value)
  
  x <- rhrTrajConstructorSTR(x, xx)
  return(x)
}


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
  
#' @export
plot.RhrTraj <- function(x, ...) {
  x <- coordinates(rhrTrajRelocations(x))
  plot(x, type = "l", xlab = "x", ylab = "y", las = 1, asp = 1, ...)
}


#' @export
print.RhrTraj <- function(x) {
  
  attr <- rhrTrajAttribute(x)
  
  cat("\n", 
      sep(), 
      oneLine(paste0("Object of class ",  class(x)[1])), 
      sep(), 
      "\n", 
      oneLine("Summary"), 
      sep(char = "-"), 
      prepLine("Number of relocation", rhrN(x)), 
      if (is.regular(x)) prepLine("Missing relocations", rhrN(x, TRUE)), 
      prepLine("Traj with time", rhrHasTS(x)), 
      prepLine("Regular traj", is.regular(x)), 
      if (rhrHasTS(x)) prepLine("Start", as.character(rhrTrajStart(x))), 
      if (rhrHasTS(x)) prepLine("End", as.character(rhrTrajEnd(x))), 
      if (is.regular(x)) prepLine("By", 
                                  as.character(lubridate::seconds_to_period(rhrTrajBy(x)))), 
      "\n", 
      oneLine("Attribute data"), 
      sep(char = "-"), 
      apply(
        cbind(names(attr),
              sapply(sapply(attr, class), paste0, collapse = ",")), 1, 
        function(xx) prepLine(xx[1], xx[2])) ,
      sep(), 
      "\n"
      )
}


#' Method to check if a trajectory is regularely spaced in time.
#' 
#' @param x Object of \code{RhrTrajS*}.
#' @param ... additional arguments, none implemented.
#'
#' @export
is.regular <- function(x, ...) {
  UseMethod("is.regular")
}

#' @export
is.regular.RhrTraj <- function(x) {
  if (is(x, "RhrTrajSTR")) {
    diffs <- diff(rhrTrajTimes(x)) 
    
    if (all(diffs[1] == diffs)) {
      return(TRUE)
    }
  } 
  FALSE
}

rhrTrajStart <- function(x, ...) {
  UseMethod("rhrTrajStart", x)
}

rhrTrajStart.RhrTrajST <- function(x, ...) {
  min(time(rhrTrajLocations(x)))
}

rhrTrajStart.RhrTrajSTR <- function(x, ...) {
  x[["properties"]]$from
}


rhrTrajBy <- function(x, ...) {
  UseMethod("rhrTrajBy", x)
}

rhrTrajBy.RhrTrajSTR <- function(x, ...) {
  x[["properties"]]$by
}

rhrTrajEnd <- function(x, ...) {
  UseMethod("rhrTrajEnd", x)
}

rhrTrajEnd.RhrTrajST <- function(x, ...) {
  max(time(rhrTrajLocations(x)))
}

rhrTrajEnd.RhrTrajSTR <- function(x, ...) {
  x[["properties"]]$to
}

## Blowup traj
blowupTraj <- function(x, ...) {
  UseMethod("blowupTraj", x)
}

blowupTraj.RhrTrajSTR <- function(x, fulltraj) {
  
  or <- data.frame(rhrTrajTime = rhrTrajTimes(x))  # regular times, all time stamps
  ## or <- seq(fulltraj$from, fulltraj$to, by = paste(fulltraj$by, "sec"))
  tt <- time(rhrTrajLocations(x))  # time stamps of relocs
  
  sp <- rhrTrajRelocations(x)
  coord <- data.frame(coordinates(sp), tt)
  names(coord) <- c("x", "y", "rhrTrajTime")
  
  dat <- x[["attribute"]]
  
  coord <- merge(coord, or, all.y = TRUE)
  coord <- coord[, c("x", "y", "rhrTrajTime")]
  
  dat <- merge(dat, or, all.y = TRUE)
  dat$rhrTrajSpace <- ifelse(is.na(dat$rhrTrajSpace), FALSE, TRUE)
  
  list(traj1 = coord, traj2 = dat)
}

