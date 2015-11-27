#' Asymptote of a home-range estimate
#' 
#' \code{rhrAsymptote} returns the sample size at which a home-range is reached, if it present. 
#'
#' Bootstrapped home-range sizes are calculated for different sample sizes. Starting
#' from \code{ns[1]} relocations until a maximum sample size \code{ns[length(ns)]}
#' points. Home-range sizes are plotted against the sample sizes.
#' Laver (2005, 2005) suggested to use the following cutoff value: the number
#' of locations at which estimates of the 95 \% confidence interval of the
#' bootstrapped home-range area is within a specified percentage of the
#' total home range area (that is the area of the home range with all
#' relocations) for at least \code{nTimes} times. Harris 1990 suggested to use random
#' sampling for discontinuous radio tracking data and sequential sampling for
#' continuous radio tracking data. 
#'
#' @param x Object of class RhrEst, the home-range estimate for which the asymptote is calculated.
#' @param ns Numeric vector, the sample sizes for which the bootstrap is performed.
#' @param nrep Numeric value, number of bootstrap replicates for each sample size.
#' @param tolTotArea Numeric value (greater than 0 and smaller than 1), tolerance to the total area (that is the area using all relocations).
#' @param nTimes Numeric value, number of times the confidence interval is required to be within tolerated total area
#' @param sampling Character value, either 'random' or 'sequential'. See below for details.

#' @return An object of class \code{RhrHRAsymptote}

#' @references Harris, S., et al. "Home-range analysis using radio-tracking data-a review of problems and techniques particularly as applied to the study of mammals." Mammal review 20.2-3 (1990): 97-123.
#' @references Peter N Laver. Cheetah of the serengeti plains: a home range analysis. Master's thesis, Virginia Polytechnic Institute and State University, 2005
#' @references Peter N. Laver and Marcella J. Kelly. A critical review of home range studies. The Journal of Wildlife Management, 72(1):290-298, 2008

#' @export
#' @examples
#' 
#' data(datSH)
#' 
#' ## With MCP
#' \dontrun{
#' mcp <- rhrMCP(datSH[, 2:3])
#' mcpA <- rhrAsymptote(mcp)
#' plot(mcpA)
#' 
#' # maybe increase ns
#' # how many samples do we have?
#' nrow(datSH)
#' ns <- seq(20, nrow(datSH), 10)
#' mcpA <- rhrAsymptote(mcp, ns = ns)
#' plot(mcpA)
#' mcpA
#' 
#' # An asymptote is reached, but it seems that there is more structure in the
#' # data
#' # Lets have a look at the dates when the fixes were recorded
#' library(lubridate)
#' datSH$day <- ymd(datSH$day)
#' table(year(datSH$day))
#' # Lets only look at fixes from the first year
#' d1 <- datSH[year(datSH$day) == 2008, ]
#' nrow(d1)
#' ns <- seq(20, nrow(d1), 10)
#' mcp <- rhrMCP(d1[, 2:3])
#' mcpA <- rhrAsymptote(mcp, ns = ns)
#' plot(mcpA)
#' mcpA
#' }



rhrAsymptote <- function(x, ns=seq(nrow(rhrData(x)) / 10, nrow(rhrData(x)), length.out = 10), nrep=10, tolTotArea=0.05, nTimes=5, sampling="sequential") {
  
  args <- as.list(environment())
  call <- match.call()

  ## Input checks
  if (!is(x, "RhrEst")) {
    stop("x is not of class RhrEst")
  }

  if (!is(ns, "numeric")) {
    stop("ns is not numeric")
  }

  if (!is(nrep, "numeric")) {
    stop("nrep is not numeric")
  }

  if (nrep <= 0) {
    stop("nrep should be >= 0")
  }

  if (!is(tolTotArea, "numeric")) {
    stop("tolTotArea is not numeric")
  }
  
  if (!is(nTimes, "numeric")) {
    stop("nTimes is not numeric")
  }

  if (!(tolTotArea > 0 & tolTotArea <= 1)) {
    stop("tolTatArea should be between 0 and 1")
  }

  if (nTimes <= 0) {
    stop("nTimes should be > 0")
  }

  if (!sampling %in% c("random", "sequential")) {
    stop("sampling should be either random or sequential")
  }

  if (max(ns) > nrow(rhrData(x))) {
    ns <- ns[ns <= nrow(rhrData(x))]
  }

  if (length(ns) <= 1) {
    stop("Requested sample size larger than the number of observation")
  }

  
  # Which estimator was used
  est <- sub("^R(*)", "r\\1", class(x)[1])

  providedArgs <- rhrArgs(x)
  for (i in seq_along(providedArgs)) {
    if (is.character(providedArgs[[i]])) {
      providedArgs[[i]] <- shQuote(providedArgs[[i]])
    }
  }

  xyp <- rhrData(x)
  n <- nrow(xyp)
  smpls <- rep(ns, each = nrep)
  bb <- do.call(rbind,
                lapply(smpls, function(nss) 
                  cbind(ns=nss, rhrArea(
                    do.call(est, ## call a hr 
                            c(if (sampling == "sequential") list((z=xyp[1:nss, ])[sample(nrow(z), replace=TRUE),])
                              else list(xyp[sample(n, nss),]),
                              providedArgs[-1]))
                    ))))
  
  totalA <- rhrArea(x)
  totalA$lower <- totalA$area * (1 - tolTotArea)
  totalA$upper <- totalA$area * (1 + tolTotArea)

      
  ## calculate confidence intervals
  confints <- do.call(rbind, lapply(split(bb, bb[, c("level", "ns")]), function(x) {
    tt <- quantile(x$area, probs=c(0.05, 0.95))
    data.frame(level=x$level[1], ns=x$ns[1], lower=tt[1], upper=tt[2])
  }))



  confints <- split(confints, confints$level)

  ## figure out when the Asymptote was reached at each level j
  ## The asymptote is reached when the both CI are inside the area tolerance for x times
  asymReached <- sapply(seq_along(confints), function(j) {
    ## check if is inside
    isInside <- rle((confints[[j]]$lower >= totalA[j, "lower"])  &
                    (confints[[j]]$upper <= totalA[j, "upper"]))

    whereInside <- with(isInside, which(lengths >= nTimes & values))[1]

    ## NA if all outside
    if (length(whereInside) > 0 & !is.na(whereInside)) {
      unique(ns)[sum(isInside$lengths[1:(whereInside-1)]) + nTimes]
    } else {
      NA
    }
  })

  asymReached <- data.frame(level=as.numeric(names(confints)),
                            ns=asymReached)

  out <- list(args = args, 
              call = call, 
    asymptote=asymReached, confints=do.call("rbind", confints), hrAreas=bb, call=match.call(),
              params=list(ns=ns, tolTotArea=tolTotArea), totalA=totalA, hrEstimator=x)
  class(out) <- c("RhrHRAsymptote", "list")
  out

}


#' @export
#' @method  print RhrHRAsymptote
print.RhrHRAsymptote <- function(x, ...) {

  cat(paste0("class                    : ", class(x)),
      paste0("asymptote calculated for : ", paste0(x$asymptote$level, collapse=",")),
      paste0("asypotote reached at     : ", paste0(x$asymptote$ns, collapse=",")),
      sep="\n")

}

#' @export
#' @method  plot RhrHRAsymptote
plot.RhrHRAsymptote <- function(x, ...) {

  ## Input checks
  ## to be completed
  cc <- reshape2::melt(x$confints, id=c("level", "ns"))

  ## Define vars
  area <- xx <- ymin <- ymax <- value <- variable <- NULL

  ## totalA
  totalA <- x$totalA
  tolTotArea <- x$params$tolTotArea
  ns <- x$params$ns
  
  dd <- data.frame(xx=rep(unique(x$params$ns), length(unique(x$hrAreas$level))),
                   ymin=rep(totalA$lower, each=length(unique(ns))),
                   ymax=rep(totalA$upper, each=length(unique(ns))),
                   level=rep(totalA$level, each=length(unique(ns))))
  
  ## When were the asymtotes reaches?
  asymR <- x$asymptote
  asymR <- asymR[complete.cases(asymR), ]

  
  
  p <- ggplot2::ggplot(x$hrAreas, ggplot2::aes(x=ns, y=area, group=ns)) +
    ggplot2::geom_point(alpha=0.5) +  
    ggplot2::geom_ribbon(data=dd, ggplot2::aes(x=xx, ymin=ymin, ymax=ymax, group=NULL, y=NULL, alpha=0.4)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=area), linetype="dashed", data=totalA)  +
    ggplot2::geom_line(ggplot2::aes(x=ns, y=value, group=variable), alpha=0.5, data=reshape2::melt(x$confints, id.vars=c("level", "ns"))) 

  if (nrow(asymR) >= 1) {
    p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept=ns), linetype="solid", colour="red", data=asymR) 
  }

  p <- p + ggplot2::facet_wrap(~level, ncol=2, scales="free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(x="Number of points", y="Area")

  return(p)
}


#' @export
rhrArgs.RhrHRAsymptote <- function(x, ...) {
  x$args
}

#' @export
rhrPrettyArgs.RhrHRAsymptote <- function (x, as.rmd = FALSE, includeName = FALSE, ...) {
  
  x <- rhrArgs(x)
  
  ns <- if (length(x$ns) > 5) {
    paste(paste(x$ns[1:2], collapse = ", "), "...", paste(tail(x$ns, 2), collapse = ", "), sep = " ")
    
  } else {
    paste(x$ns, collapse = ", ")
  }
  
  names(x)
  res <- data.frame(
    Parameter = c(if (includeName) "Estimator", 
                  "Number of samples", "Number of repicates", 
                  "Tolerance to total area", "Number of times within total area", 
                  "sampling regime"), 
    Value = c(if (includeName) class(x$x)[1], 
              ns, x$nrep, 
              x$tolTotArea, x$nTimes, x$sampling), 
    stringsAsFactors = FALSE
  )
  
  if (as.rmd) {
    knitr::kable(res)
  } else {
   res 
  }
}
