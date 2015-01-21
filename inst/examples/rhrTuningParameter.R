library(rhr)
data(datSH)

kde <- rhrKDE(datSH[, 2:3])
locoh <- rhrLoCoH(datSH[, 2:3])

rhrTuningParameter(kde)
rhrTuningParameter(kde, msg = TRUE)
rhrTuningParameter(kde, msg = TRUE, digits = 1)
rhrTuningParameter(kde, msg = TRUE, digits = 10)

rhrTuningParameter(locoh)
rhrTuningParameter(locoh, msg = TRUE)
