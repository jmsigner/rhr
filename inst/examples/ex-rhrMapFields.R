data(datSH)
head(datSH)

# no CRS
dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"))
sp::proj4string(dat$dat)

# We can even create a track
class(rhrTrack(dat))


## add CRS
dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"),
                    projString=sp::CRS("+init=epsg:31467"))
sp::proj4string(dat$dat)
head(dat$dat)

## add default Id
dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"),
                    projString=sp::CRS("+init=epsg:31467"),
                    defaultId = "abc1")
head(dat$dat)


## add default time stamp
head(datSH)
dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467",
                                       date="day", time="time"),
                    projString=sp::CRS("+init=epsg:31467"),
                    defaultId = "abc1",
                    dateFormat="ymd", 
                    timeFormat="hms")
head(dat$dat)
class(rhrTrack(dat))

## Transform the CRS of the data
head(datSH)
dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467",
                                       date="day", time="time"),
                    projString=sp::CRS("+init=epsg:31467"),
                    projStringOut=sp::CRS("+init=epsg:4226"),
                    defaultId = "abc1",
                    dateFormat="ymd", 
                    timeFormat="hms")
proj4string(dat$dat)
head(dat$dat)
head(as.data.frame(dat$dat))

class(rhrTrack(dat))

# Lets say we want to split the track by month
class(rhrTracks(dat, id = lubridate::round_date(dat$dat$timestamp, "month")))

