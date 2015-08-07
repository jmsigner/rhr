library(rhr)
data(trackST)

ttsi <- rhrTTSI(trackST, interval = 60 * 60 * 10)
plot(ttsi)
