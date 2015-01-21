library(rhr)
data(datSH)

locoh1 <- rhrLoCoH(datSH[, 2:3], type="k", n=10)
locoh2 <- rhrLoCoH(datSH[, 2:3], type="a", n=1000)
locoh3 <- rhrLoCoH(datSH[, 2:3], type="r", n=100)

plot(rhrIsopleths(locoh1))
plot(rhrIsopleths(locoh2), border = "red", add = TRUE)
plot(rhrIsopleths(locoh3), border = "blue", add = TRUE)

locoh4 <- rhrLoCoH(datSH[, 2:3], type = "k", autoN = TRUE)
rhrTuningParameter(locoh4)
rhrTuningParameter(locoh4, msg = TRUE)

locoh5 <- rhrLoCoH(datSH[, 2:3], type="a", autoN = TRUE)
rhrTuningParameter(locoh5)

