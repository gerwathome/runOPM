## ----run_simple, cache=FALSE, out.width='30%', out.height='30%'----------
library(runOPM)
# get the SPE1 deck form the package extdata directory
deck <- system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
RunFlow(deck, basedir = "tmp", wait = TRUE)
rslts <- EclSum(casename = "SPE1_CASE1", basedir = "tmp")
PlotEach(rslts, wgnames = "FIELD")

## ----cleanup_simple, echo=FALSE, warning=FALSE, message=FALSE, eval=TRUE----
unlink("tmp", recursive = TRUE, force = TRUE)

