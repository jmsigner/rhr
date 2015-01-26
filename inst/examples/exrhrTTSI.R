library(rhr)
data(datSH)

timestamp <- lubridate::ymd(datSH[, 4]) + lubridate::hms(datSH[, 5])

ttsi <- rhrTTSI(datSH[, 2:3], timestamp, interval = 60 * 60 * 10)
plot(ttsi)
