##' Convenience function to prepare data for further analysis.
##'
##' For date and time the lubridate conventions are used. E.g. day-month-year is abbreviated as \code{dmy}.
##' @title Map information to columns
##' @param dat A data.frame or SptialPointsDataFrame. 
##' @param fields A list with named elements for lon, lat and id. Optionally also for date and time.
##' @param projString A object of class CRS.
##' @param dateFormat A character indicating date format, see details.
##' @param timeFormat A character indicating time format, see details.
##' @param defaultId A character indicating default ID in case id is missing from fields.
##' @return A list of class \code{RhrMappedData}. The list contains the following elements: \code{dat} (SpatialPointsDataFrame, with the data), \code{hasTS} (logical vector indicating if there is a timestamp) and a list that gives information about the number of missing and duplicated points. 
##' @export 
##' @example examples/exrhrMapFields.R
##' @author Johannes Signer
rhrMapFields <- function(dat, fields=list(lon=NA, lat=NA, id=NA, date=NA, time=NA),
                         projString=NULL, dateFormat="ymd", timeFormat="hms",
                         defaultId="Animal_1") {
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
    rhrMapFields(dat, fields)
  }

  ## check if we have a Spatial*DF
  if (is(dat, "SpatialPointsDataFrame")) {
    if (is.null(projString)) {
      projString <- proj4string(dat)
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

      dateParsed <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
      timeParsed <- lubridate::parse_date_time(dat[, fields$time], timeFormat)
      outdat$timestamp <- dateParsed + (lubridate::hour(timeParsed) * 3600 +
                                          lubridate::minute(timeParsed) * 60 +
                                          lubridate::second(timeParsed))
    } else {
      warning("rhrMapFields: date and/or time format couldn't be parsed")
      outdat$timestamp <- NA
    }
  } else if (is.null(fields$date) && !is.null(fields$time)) {
    warning("rhrMapFields: no date provided")
    outdat$timestamp <- NA
  } else if (!is.null(fields$date) && is.null(fields$time)) {
    if (dateFormat %in% c("dmy", "ymd", "mdy")) {
      outdat$timestamp <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
    } else if (dateFormat %in% c("ymd_h", "ymd_hm", "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h", "mdy_hm", "mdy_hms")) {
      outdat$timestamp <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
    } else {
      warning("rhrMapFields: date format couldn't be parsed")
      outdat$timestamp <- NA
    }
  }

  

  ## ------------------------------------------------------------------------------ ##  
  ## Deal with duplicates
  hasTS <- if (all(is.na(outdat$timestamp))) FALSE else TRUE

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

  if (is(projString, "CRS")) {
    dat <- SpatialPointsDataFrame(dat[, c("lon", "lat")], data=dat, proj4string=projString)
  } else {
    dat <- SpatialPointsDataFrame(dat[, c("lon", "lat")], data=dat, proj4string=CRS(as.character(NA)))
    warning("rhrMapFields: proj4string not of class CRS, using NA")
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Deal with missing

  invisible(structure(list(res=list(nobs=nobs,
                             nIncompleteCases=nIncompleteCases,
                             nDuplicated=nDuplicated,
                             nobsFinal=nobsFinal),
                           hasTS=hasTS,
                           dat=dat),
                      class=c("RhrMappedData", "list")))
}
