## ----run_simple, cache=FALSE, out.width='30%', out.height='30%'----------
library(runOPM)
# get the SPE1 deck form the package extdata directory
deck <- system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
runflow(deck, basedir="tmp", wait=TRUE)
rslts <- eclsum(casename="SPE1_CASE1", basedir="tmp")
ploteach(rslts)

