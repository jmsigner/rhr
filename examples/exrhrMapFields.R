## data(datSH)
## head(datSH)
## 
## ## no CRS
## dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"))
## sp::proj4string(dat$dat)
## 
## ## add CRS
## dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"),
                    ## projString=sp::CRS("+init=epsg:31467"))
## sp::proj4string(dat$dat)
## head(dat$dat)
## 
## ## add default Id
## dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"),
                    ## projString=sp::CRS("+init=epsg:31467"),
                    ## defaultId = "abc1")
## head(dat$dat)
## 
## 
## ## add default time stamp
## head(datSH)
## dat <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467",
                                       ## date="day", time="time"),
                    ## projString=sp::CRS("+init=epsg:31467"),
                    ## defaultId = "abc1",
                    ## dateFormat="ymd", 
                    ## timeFormat="hms")
## head(dat$dat)

