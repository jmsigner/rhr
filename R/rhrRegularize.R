#' Regularizes a trajectory
#'
#' Constructs a regular trajectory (i.e., with identical time intervals between relocations). 
#' 
#' If more than one relocation falls within the time window, the relocation closest in time is chosen.
#'
#' @param x Object of class \code{RhrTrajS*}
#' @param newTraj Vector with objects of class \code{POSIXct}, providing temporal spacing of new trajectory. 
#' @param side Character scalar, indicating whether to consider relocations to "left" (i.e., before), to the "right" (i.e., after) or on "both" sides of a new relocation. 
#' @param windw Numeric scalar, the time window in seconds that should be scanned for new relocations. 
#' @param ... None implemented.
#' @return Object of class \code{RhrTrajS*}
#' @export
rhrRegularize <- function(x, newTraj, side, windw, ...) {
  UseMethod("rhrRegularize", x)
}

#' @export
rhrRegularize.RhrTracksST <- function(x, newTraj, 
                                    side = c("both"), 
                                    windw = lubridate::period_to_seconds(lubridate::minutes(10)), ...) {
  x <- lapply(x, function(y) rhrRegularize(y, newTraj, side, windw, ...))
  class(x) <- tracksClass(x)
  x
}
  
##' @export
rhrRegularize.RhrTrackST <- function(x, newTraj, 
                                    side = c("both"), 
                                    windw = lubridate::period_to_seconds(lubridate::minutes(10)), ...) {
  
  if (missing(newTraj)) {
    stop("new trajectory is required")
  }
  
  if (!side %in% c("both", "left", "right")) {
    stop("side should be one of: both, left, right")
  }
  
  if (side == "both") {
    windw <- windw / 2
  }
  
  obs <- as.integer(rhrTimes(x))
  new <- as.integer(newTraj)
  
  if (!all(diff(new))) {
    stop("newTraj is not regularely spaced")
  }
  
  ## left, right both
  wcl <- if (side == "both") {
    lapply(new, function(z) {
      w <- which(obs >= (z - windw) & obs <= (z + windw))
      w[which.min(abs(obs[w] - z))]
    })
  } else if (side == "left") {
    lapply(new, function(z) {
      w <- which(obs >= (z - windw) & obs <= z)
      w[which.min(abs(obs[w] - z))]
    })
  } else if (side == "right") {
    lapply(new, function(z) {
      w <- which(obs <= (z + windw) & obs >= z)
      w[which.min(abs(obs[w] - z))]
    })
  }
  
  wcl1 <- unlist(lapply(wcl, function(x) if (length(x) == 1) x else NA))
  w_new <- which(!is.na(wcl1))
  wcl1 <- wcl1[!is.na(wcl1)]
  
  if (all(is.na(wcl1))) {
    stop("No relocation in new trajectory, larger window size?")
  }
  
  
  nt1 <- data.frame(sp::coordinates(rhrPoints(x))[wcl1, ],
                    rhrTime = newTraj[w_new], 
                    reloc = wcl1, 
                    ptsn = 1:length(newTraj[wcl1]), 
                    diff = sapply(seq_along(wcl1), function(i) {
                      if (!is.na(wcl1[i])) {
                        new[i] - obs[wcl1[i]]
                      } else {
                        NA
                      }
                    })) 
  
  rownames(nt1) <- 1:nrow(nt1)
  colnames(nt1)[1:2] <- c("x", "y")
  
  sp::coordinates(nt1) <- ~x+y
  rhr::rhrTrack(nt1, time = nt1$rhrTime)
}
