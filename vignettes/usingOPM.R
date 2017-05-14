## ------------------------------------------------------------------------
library(runOPM)
runflow("SPE1.DATA")
rslts <- eclsum("./SPE1")
plotflow(rslts)

