##' to come
##'
##' to come
##' @title to come
##' @param dat 
##' @param fields 
##' @param proj4string 
##' @param dateFormat 
##' @param timeFormat 
##' @param defaultId
##' @return RhrMappedData
##' @export 
##' @author Johannes Signer
rhrMapFields <- function(dat, fields=list(lon=NA, lat=NA, id=NA, date=NA, time=NA), proj4string=NULL, dateFormat="ymd", timeFormat="hms",
                         defaultId="Animal_1", duplicates) {
  ## Debug
  if (FALSE) {
    dat <- datSH
    fields <- list(lon="x_epsg31467",
                   lat="y_epsg31467",
                   id=NA, 
                   date="day",
                   time="time")
    dateFormat <- "ymd"
    timeFormat <- "hms"
    defaultId <- "Animal_1"
  }

  if (is(dat, "SpatialPointsDataFrame")) {
    if (is.null(proj4string)) {
      proj4string <- proj4string(dat)
    }

    coords <- coordinates(dat)
    dat <- data.frame(dat)
    dat$lon <- coords[, 1]
    dat$lat <- coords[, 2]

    fields$lon <- "lon"
    fields$lat <- "lat"
  }

  if (!is(dat, "data.frame")) {
    stop("rhrMapFields: dat should be a data.frame")
  }

  ## Make sure we have all fields
  if (!"lat" %in% names(fields)) {
    stop("rhrMapFields: latitude is required")
  }

  if (!"lon" %in% names(fields)) {
    stop("rhrMapFields: longitude is required")
  }

  if (!"id" %in% names(fields) || is.na(fields$id)) {
    fields$id <- NULL
  } else if (is.na(fields$id)) {
    fields$id <- NULL
  }
  

  if (!"date" %in% names(fields)) {
    fields$date <- NULL
  } else if (is.na(fields$date)) {
    fields$date <- NULL
  }


  if (!"time" %in% names(fields)) {
    fields$time <- NULL
  } else if (is.na(fields$time)) {
    fields$time <- NULL
  }

  outdat <- data.frame(id=rep(NA, nrow(dat)), 
                       lon=rep(NA, nrow(dat)),
                       lat=rep(NA, nrow(dat)), 
                       timestamp=rep(NA, nrow(dat)))

  ## Now fill in the values
  if (is.null(fields$lon)) {
    stop("rhrMapFields: lon required")
  } else {
    outdat$lon <- dat[, fields$lon]
  }

  if (is.null(fields$lat)) {
    stop("rhrMapFields: lat required")
  } else {
    outdat$lat <- dat[, fields$lat]
  }

  if (is.null(fields$id)) {
    outdat$id <- defaultId
  } else {
    outdat$id <- dat[, fields$id]
  }

  ## 4 possibilities
  ## date provided but not time
  ## time provided but not date -> error
  ## date and time provided
  ## date as ymd_hm(s)

  if (!is.null(fields$date) && !is.null(fields$time)) {
    if (dateFormat %in% c("dmy", "ymd", "mdy") && timeFormat %in% c("hms", "hm")) {
      dateParsed <- eval(parse(text=paste0(dateFormat, "(dat[, fields$date])")))
      timeParsed <- eval(parse(text=paste0(timeFormat, "(dat[, fields$time])")))  
      outdat$timestamp <- dateParsed + timeParsed
    } else {
      warning("rhrMapFields: date and/or time format couldn't be parsed")
      outdat$timestamp <- NA
    }
  } else if (is.null(fields$date) && !is.null(fields$time)) {
    warning("rhrMapFields: no date provided")
    outdat$timestamp <- NA
  } else if (!is.null(fields$date) && is.null(fields$time)) {
    if (dateFormat %in% c("dmy", "ymd", "mdy")) {
      outdat$timestamp <- eval(parse(text=paste0(dateFormat, "(dat[, fields$date])")))  
    } else if (dateFormat %in% c("ymd_h", "ymd_hm", "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h", "mdy_hm", "mdy_hms")) {
      outdat$timestamp <- eval(parse(text=paste0(dateFormat, "(dat[, fields$date])")))  
    } else {
      warning("rhrMapFields: date format couldn't be parsed")
      outdat$timestamp <- NA
    }
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Deal with duplicates
  hasTS <- ifelse(all(is.na(outdat$timestamp)), FALSE, TRUE)

  ## split data by animal
  dat <- split(outdat, outdat$id)

  nobs <- lapply(dat, nrow)

  ## check if there are missing
  nIncompleteCases <- lapply(dat, function(x) sum(!complete.cases(x[, if (!hasTS) c("id", "lon", "lat") else c("id", "lon", "lat", "timestamp")])))

  ## remove missing
  dat <- lapply(dat, function(x) x[complete.cases(x[, if (!hasTS) c("id", "lon", "lat") else c("id", "lon", "lat", "timestamp")]), ])

  ## check for duplicated with/without time
  nDuplicated <- lapply(dat, function(x) sum(duplicated(x[, if (!hasTS) c("id", "lon", "lat") else c("id", "lon", "lat", "timestamp")])))
  dat <- lapply(dat, function(x) x[!duplicated(x[, if (!hasTS) c("id", "lon", "lat") else c("id", "lon", "lat", "timestamp")]), ])

  ## final number of obs
  nobsFinal <- sapply(dat, nrow)
  dat <- do.call(rbind, dat)

  ## ------------------------------------------------------------------------------ ##  
  ## Deal with missing



  invisible(structure(list(res=list(nobs=nobs,
                             nIncompleteCases=nIncompleteCases,
                             nDuplicated=nDuplicated,
                             nobsFinal=nobsFinal),
                           dat=dat),
                      class=c("RhrMappedData", "list")))
}
