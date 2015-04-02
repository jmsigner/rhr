
## ## more attributes
## library(lubridate)
## 
## n <- 10
## xy <- data.frame(x = cumsum(rnorm(10)), y = cumsum(rnorm(10)), ts = sample(now() + 1:10))
## 
## plot(xy$x, xy$y, type = "l", asp = 1)
                ##  
## mp <- rhrMapFields(xy, list(lon = "x", lat = "y", date = "ts"), dateFormat = "ymd_hms")
## 
## 
## rhrMapFields <- function(dat, fields=list(lon=NA, lat=NA, id=NA, date=NA, time=NA),
                         ## projString=NULL, dateFormat="ymd", timeFormat="hms",
                         ## defaultId="Animal_1") {
## 
  ## ## check if we have a Spatial*DF
  ## if (is(dat, "SpatialPointsDataFrame")) {
    ## if (is.null(projString)) {
      ## projString <- sp::proj4string(dat)
    ## }
## 
    ## coords <- sp::coordinates(dat)
    ## dat <- data.frame(dat)
    ## dat$lon <- coords[, 1]
    ## dat$lat <- coords[, 2]
## 
    ## fields$lon <- "lon"
    ## fields$lat <- "lat"
  ## }
## 
  ## if (!is(dat, "data.frame")) {
    ## stop("rhrMapFields: dat should be a data.frame")
  ## }
## 
  ## ## Make sure we have all fields
  ## if (!"lat" %in% names(fields)) {
    ## stop("rhrMapFields: latitude is required")
  ## }
## 
  ## if (!"lon" %in% names(fields)) {
    ## stop("rhrMapFields: longitude is required")
  ## }
## 
  ## if (!"id" %in% names(fields) || is.na(fields$id)) {
    ## fields$id <- NULL
  ## } else if (is.na(fields$id)) {
    ## fields$id <- NULL
  ## }
 ##  
  ## if (!"date" %in% names(fields)) {
    ## fields$date <- NULL
  ## } else if (is.na(fields$date)) {
    ## fields$date <- NULL
  ## }
## 
  ## if (!"time" %in% names(fields)) {
    ## fields$time <- NULL
  ## } else if (is.na(fields$time)) {
    ## fields$time <- NULL
  ## }
## 
  ## outdat <- data.frame(id=rep(NA, nrow(dat)), 
                       ## lon=rep(NA, nrow(dat)),
                       ## lat=rep(NA, nrow(dat)), 
                       ## timestamp=rep(NA, nrow(dat)))
## 
  ## ## Now fill in the values
  ## if (is.null(fields$lon)) {
    ## stop("rhrMapFields: lon required")
  ## } else {
    ## outdat$lon <- dat[, fields$lon]
  ## }
## 
  ## if (is.null(fields$lat)) {
    ## stop("rhrMapFields: lat required")
  ## } else {
    ## outdat$lat <- dat[, fields$lat]
  ## }
## 
  ## if (is.null(fields$id)) {
    ## outdat$id <- defaultId
  ## } else {
    ## outdat$id <- dat[, fields$id]
  ## }
## 
  ## ## 4 possibilities
  ## ## date provided but not time
  ## ## time provided but not date -> error
  ## ## date and time provided
  ## ## date as ymd_hm(s)
## 
  ## if (!is.null(fields$date) && !is.null(fields$time)) {
    ## if (dateFormat %in% c("dmy", "ymd", "mdy") && timeFormat %in% c("hms", "hm")) {
## 
      ## dateParsed <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
      ## timeParsed <- lubridate::parse_date_time(dat[, fields$time], timeFormat)
      ## outdat$timestamp <- dateParsed + (lubridate::hour(timeParsed) * 3600 +
                                          ## lubridate::minute(timeParsed) * 60 +
                                          ## lubridate::second(timeParsed))
    ## } else {
      ## warning("rhrMapFields: date and/or time format couldn't be parsed")
      ## outdat$timestamp <- NA
    ## }
  ## } else if (is.null(fields$date) && !is.null(fields$time)) {
    ## warning("rhrMapFields: no date provided")
    ## outdat$timestamp <- NA
  ## } else if (!is.null(fields$date) && is.null(fields$time)) {
    ## if (dateFormat %in% c("dmy", "ymd", "mdy")) {
      ## outdat$timestamp <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
    ## } else if (dateFormat %in% c("ymd_h", "ymd_hm", "ymd_hms", "dmy_h", "dmy_hm", "dmy_hms", "mdy_h", "mdy_hm", "mdy_hms")) {
      ## outdat$timestamp <- lubridate::parse_date_time(dat[, fields$date], dateFormat)
    ## } else {
      ## warning("rhrMapFields: date format couldn't be parsed")
      ## outdat$timestamp <- NA
    ## }
  ## }
## 
 ##  
## 
  ## ## ------------------------------------------------------------------------------ ##  
  ## ## Deal with duplicates
  ## hasTS <- if (all(is.na(outdat$timestamp))) FALSE else TRUE
## 
  ## if (hasTS) {
    ## outdat <- outdat[order(outdat$timestamp), ]
  ## }
## 
  ## projString <- if (is(projString, "CRS")) projString else sp::CRS(as.character(NA))
  ## datSP <- sp::SpatialPoints(outdat[, c("lon", "lat")], proj4string = projString)
## 
  ## ## ------------------------------------------------------------------------------ ##  
  ## ## Deal with missing
## 
  ## invisible(
    ## structure(
      ## list(subset = list(),
           ## duplicates = list(),
           ## outlier = list(), 
           ## interval = list(),
           ## regularize = list(),
           ## split = list(),
           ## burst = list(), 
           ## dat = if (hasTS) {
             ## trajectories::Track(
               ## spacetime::STIDF(datSP, outdat$timestamp, outdat[, "id", drop = FALSE]))
           ## } else {
             ## sp::SpatialPointsDataFrame(datSP, data = outdat[, "id", drop = FALSE])
           ## }), 
      ## class=c(if (hasTS) "RhrMappedDataST" else "RhrMappedDataS",
        ## "RhrMappedData", "list")))
## }
