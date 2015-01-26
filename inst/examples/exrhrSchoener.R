library(rhr)
data(datSH)
dat <- data.frame(x = datSH[, 2],
                  y = datSH[, 3],
                  t = lubridate::ymd(datSH[, 4]) +
                    lubridate::hms(datSH[, 5]))

rhrSchoener(dat, interval = 60 * 60 * 10)
